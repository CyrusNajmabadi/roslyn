// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#pragma warning disable VSTHRD200 // Use "Async" suffix for async methods

using System;
using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.UseExpressionBody;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Shared.Collections;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.MakeMethodAsync;

[ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CSharpMakeMethodAsyncCodeRefactoringProvider)), Shared]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
[method: ImportingConstructor]
internal sealed class CSharpMakeMethodAsyncCodeRefactoringProvider()
    : SyntaxEditorBasedCodeRefactoringProvider
{
    protected override ImmutableArray<RefactorAllScope> SupportedRefactorAllScopes => DefaultRefactorAllScopes;

    private static bool IsPotentialAsyncContainer(SyntaxNode node)
        => node is MethodDeclarationSyntax or LocalFunctionStatementSyntax or AnonymousFunctionExpressionSyntax;

    private static bool HasAsyncModifier(SyntaxNode node)
        => GetModifiers(node).Any(SyntaxKind.AsyncKeyword);

    private static SyntaxTokenList GetModifiers(SyntaxNode node)
        => node switch
        {
            MethodDeclarationSyntax methodDeclaration => methodDeclaration.Modifiers,
            LocalFunctionStatementSyntax localFunction => localFunction.Modifiers,
            AnonymousFunctionExpressionSyntax anonymousFunction => anonymousFunction.Modifiers,
            _ => throw ExceptionUtilities.Unreachable(),
        };

    private static SyntaxNode? GetBody(SyntaxNode node)
        => node switch
        {
            MethodDeclarationSyntax methodDeclaration => methodDeclaration.Body ?? (SyntaxNode?)methodDeclaration.ExpressionBody?.Expression,
            LocalFunctionStatementSyntax localFunction => localFunction.Body ?? (SyntaxNode?)localFunction.ExpressionBody?.Expression,
            AnonymousFunctionExpressionSyntax anonymousFunction => anonymousFunction.Body ?? anonymousFunction.ExpressionBody,
            _ => throw ExceptionUtilities.Unreachable(),
        };

    private static IMethodSymbol? GetMethodSymbol(SemanticModel semanticModel, SyntaxNode node, CancellationToken cancellationToken)
    {
        return node switch
        {
            MethodDeclarationSyntax methodDeclaration => semanticModel.GetDeclaredSymbol(methodDeclaration, cancellationToken),
            LocalFunctionStatementSyntax localFunction => semanticModel.GetDeclaredSymbol(localFunction, cancellationToken),
            AnonymousFunctionExpressionSyntax anonymousFunction => (IMethodSymbol?)semanticModel.GetSymbolInfo(anonymousFunction, cancellationToken).Symbol,
            _ => null,
        };
    }

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var (document, span, cancellationToken) = context;
        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        var taskTypes = new KnownTaskTypes(semanticModel.Compilation);

        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
        var token = root.FindToken(span.Start);

        var container = token.GetRequiredParent().FirstAncestorOrSelf<SyntaxNode>(
            n => n is MethodDeclarationSyntax or LocalFunctionStatementSyntax or AnonymousFunctionExpressionSyntax);
        if (container is null)
            return;

        if (!ShouldMakeAsync(semanticModel, taskTypes, container, cancellationToken))
            return;

        context.RegisterRefactoring(CodeAction.Create(
            "New make async",
            cancellationToken => RefactorAllAsync(document, new([container.Span]), null, cancellationToken)));
    }

    private static bool ShouldMakeAsync(
        SemanticModel semanticModel, KnownTaskTypes taskTypes, SyntaxNode container, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (HasAsyncModifier(container))
            return false;

        var body = GetBody(container);
        if (body is null)
            return false;

        if (container is MethodDeclarationSyntax methodDeclaration)
        {
            var returnType = methodDeclaration.ReturnType;
            var rightMostName = returnType.GetRightmostName();
            if (rightMostName is null)
                return false;

            var identifier = rightMostName.Identifier;
            if (identifier.ValueText is not nameof(Task) and not nameof(ValueTask))
                return false;
        }
        else if (container is LocalFunctionStatementSyntax localFunction)
        {
            var returnType = localFunction.ReturnType;
            var rightMostName = returnType.GetRightmostName();
            if (rightMostName is null)
                return false;

            var identifier = rightMostName.Identifier;
            if (identifier.ValueText is not nameof(Task) and not nameof(ValueTask))
                return false;
        }

        if (body is ExpressionSyntax expression)
        {
            if (expression is ThrowExpressionSyntax)
                return false;

            // Only want to fixup `=> Task.FromResult(...)` and `ValueTask GooAsync() => new(...)` and the like to
            // `async => ...`.  We want to leave alone `Task GooAsync() => BarAsync()`
            if (!IsTaskFromExpressionWrapper(expression, out _) &&
                !TryRewriteSpecializedTask(expression, out _))
            {
                return false;
            }
        }

        var method = GetMethodSymbol(semanticModel, container, cancellationToken);
        if (method is null)
            return false;

        if (!taskTypes.IsTaskLike(method.ReturnType))
            return false;

        return true;
    }

    private static bool TryRewriteSpecializedTask(ExpressionSyntax expression, [NotNullWhen(true)] out ExpressionSyntax? result)
    {
        result = RewriteSpecializedTaskWorker();
        if (result is null)
            return false;

        result = result.WithTriviaFrom(expression);
        return true;

        ExpressionSyntax? RewriteSpecializedTaskWorker()
        {
            if (expression is InvocationExpressionSyntax { ArgumentList.Arguments: [] } invocationExpression)
                expression = invocationExpression.Expression;

            if (expression is MemberAccessExpressionSyntax { Expression: IdentifierNameSyntax { Identifier.ValueText: "SpecializedTasks" } } memberAccess)
            {
                var name = memberAccess.Name;
                return name.Identifier.ValueText switch
                {
                    "True" => SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression),
                    "False" => SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression),
                    "Null" => SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression),
                    "EmptyReadOnlyList" or "EmptyList" or "EmptyImmutableArray" or "EmptyEnumerable" => SyntaxFactory.CollectionExpression(),
                    "Default" when name is GenericNameSyntax { TypeArgumentList.Arguments: [var typeArg] } => SyntaxFactory.DefaultExpression(typeArg),
                    _ => null,
                };
            }

            return null;
        }
    }

    private static bool IsTaskFromExpressionWrapper(ExpressionSyntax expression, [NotNullWhen(true)] out ExpressionSyntax? result)
    {
        return IsTaskConstruction(expression, out result) ||
               IsFromResultInvocation(expression, out result);
    }

    private static bool IsTaskConstruction(ExpressionSyntax expression, [NotNullWhen(true)] out ExpressionSyntax? result)
    {
        if (expression is BaseObjectCreationExpressionSyntax { ArgumentList.Arguments: [var argument] })
        {
            result = argument.Expression;
            return true;
        }

        result = null;
        return false;
    }

    private static bool IsFromResultInvocation(ExpressionSyntax expression, [NotNullWhen(true)] out ExpressionSyntax? result)
    {
        if (expression is InvocationExpressionSyntax { ArgumentList.Arguments: [var argument] } invocation &&
           invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
           memberAccess.Name.Identifier.ValueText == "FromResult")
        {
            result = argument.Expression;
            return true;
        }

        result = null;
        return false;
    }

    protected override async Task RefactorAllAsync(
        Document document,
        ImmutableArray<TextSpan> refactorAllSpans,
        SyntaxEditor editor,
        string? equivalenceKey,
        CancellationToken cancellationToken)
    {
        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

        var intervalTree = new TextSpanMutableIntervalTree(refactorAllSpans);
        var taskTypes = new KnownTaskTypes(semanticModel.Compilation);

        foreach (var node in root.DescendantNodes().Where(IsPotentialAsyncContainer).OrderByDescending(n => n.SpanStart))
        {
            if (!intervalTree.HasIntervalThatOverlapsWith(node.Span))
                continue;

            if (!ShouldMakeAsync(semanticModel, taskTypes, node, cancellationToken))
                continue;

            var methodSymbol = GetMethodSymbol(semanticModel, node, cancellationToken);
            if (methodSymbol is null)
                continue;

            editor.ReplaceNode(
                node,
                (currentNode, generator) => MakeAsync(generator, methodSymbol, currentNode));
        }
    }

    private static SyntaxNode MakeAsync(
        SyntaxGenerator generator, IMethodSymbol methodSymbol, SyntaxNode currentNode)
    {
        var returnsValue = methodSymbol.ReturnType.GetTypeArguments().Length > 0;

        var body = GetBody(currentNode);
        if (body != null)
        {
            var rewritten = RewriteBody(generator, body, returnsValue);
            if (rewritten != null)
            {
                currentNode = currentNode.ReplaceNode(body, rewritten);
                currentNode = generator.WithModifiers(currentNode, generator.GetModifiers(currentNode).WithAsync(true));
            }
        }

        return currentNode;
    }

    private static SyntaxNode? RewriteBody(SyntaxGenerator generator, SyntaxNode? body, bool returnsValue)
    {
        if (body is ExpressionSyntax expression)
        {
            if (IsTaskFromExpressionWrapper(expression, out var unwrapped) ||
                TryRewriteSpecializedTask(expression, out unwrapped))
            {
                return unwrapped.WithTriviaFrom(expression);
            }
        }
        else if (body is BlockSyntax)
        {
            return RewriteBlock(generator, body, returnsValue);
        }

        return body;
    }

    private static bool TryUnwrap(bool returnsValue, SyntaxNode body, ReturnStatementSyntax returnStatement, out ReturnStatementSyntax? result)
    {
        if (returnStatement.Expression != null)
        {
            var returnExpression = returnStatement.Expression;

            if (IsTaskFromExpressionWrapper(returnExpression, out var unwrapped) ||
                TryRewriteSpecializedTask(returnExpression, out unwrapped))
            {
                result = returnStatement.ReplaceNode(returnExpression, unwrapped.WithTriviaFrom(returnExpression));
                return true;
            }

            if (returnExpression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "CompletedTask" })
            {
                if (body is BlockSyntax block &&
                    returnStatement == block.Statements.Last())
                {
                    result = null;
                }
                else
                {
                    result = SyntaxFactory.ReturnStatement().WithTriviaFrom(returnStatement);
                }

                return true;
            }

            if (returnExpression is LiteralExpressionSyntax(kind: SyntaxKind.DefaultLiteralExpression))
            {
                if (!returnsValue)
                {
                    if (body is BlockSyntax block &&
                        returnStatement == block.Statements.Last())
                    {
                        result = null;
                    }
                    else
                    {
                        result = SyntaxFactory.ReturnStatement().WithTriviaFrom(returnStatement);
                    }
                }
                else
                {
                    result = returnStatement;
                }

                return true;
            }
        }

        result = null;
        return false;
    }

    private static SyntaxNode? RewriteBlock(SyntaxGenerator generator, SyntaxNode body, bool returnsValue)
    {
        var bodyEditor = new SyntaxEditor(body, generator);
        var unwrappedCode = false;
        foreach (var child in body.DescendantNodesAndSelf(n => !IsPotentialAsyncContainer(n)).OrderByDescending(n => n.SpanStart))
        {
            if (child is not ReturnStatementSyntax { Expression: { } returnExpression } returnStatement)
                continue;

            if (TryUnwrap(returnsValue, body, returnStatement, out var unwrapped))
            {
                if (unwrapped is null)
                {
                    bodyEditor.RemoveNode(returnStatement);
                }
                else
                {
                    bodyEditor.ReplaceNode(returnStatement, unwrapped);
                }

                unwrappedCode = true;
                continue;
            }

            var awaited = generator.AwaitExpression(
                generator.InvocationExpression(
                    generator.MemberAccessExpression(
                        returnExpression.WithoutTrivia(),
                        "ConfigureAwait"),
                    generator.Argument(generator.FalseLiteralExpression()))).WithTriviaFrom(returnExpression);

            // return FooAsync();
            if (returnsValue)
            {
                bodyEditor.ReplaceNode(returnExpression, awaited);
            }
            else
            {
                bodyEditor.ReplaceNode(
                    returnStatement,
                    SyntaxFactory.ExpressionStatement(
                        (ExpressionSyntax)awaited, returnStatement.SemicolonToken).WithLeadingTrivia(returnStatement.GetLeadingTrivia()));
            }
        }

        return unwrappedCode ? bodyEditor.GetChangedRoot() : null;
    }
}
