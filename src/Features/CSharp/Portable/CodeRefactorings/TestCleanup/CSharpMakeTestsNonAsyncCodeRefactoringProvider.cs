// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.CodeGeneration;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.CodeRefactorings.TestCleanup;

using static CSharpSyntaxTokens;
using static SyntaxFactory;

[ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CSharpMakeTestsNonAsyncCodeRefactoringProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpMakeTestsNonAsyncCodeRefactoringProvider() : SyntaxEditorBasedCodeRefactoringProvider
{
    protected override ImmutableArray<FixAllScope> SupportedFixAllScopes => AllFixAllScopes;

    protected override CodeActionCleanup Cleanup => CodeActionCleanup.SyntaxOnly;

    private static bool ShouldMakeSynchronous(
        [NotNullWhen(true)] MethodDeclarationSyntax? methodDeclaration,
        [NotNullWhen(true)] out StatementSyntax? bodyStatement,
        [NotNullWhen(true)] out ExpressionSyntax? bodyExpression,
        [NotNullWhen(true)] out SyntaxToken semicolonToken)
    {
        bodyStatement = null;
        bodyExpression = null;
        semicolonToken = default;

        if (methodDeclaration?.Body is not BlockSyntax { Statements: [var statement] })
            return false;

        if (methodDeclaration.AttributeLists.Count == 0)
            return false;

        if (statement.GetLeadingTrivia().Any(d => d.Kind() is not (SyntaxKind.WhitespaceTrivia or SyntaxKind.EndOfLineTrivia)))
            return false;

        if (methodDeclaration.Modifiers.Any(SyntaxKind.AsyncKeyword))
        {
            if (statement is not ExpressionStatementSyntax { Expression: AwaitExpressionSyntax awaitExpression } exprStatement)
                return false;

            if (awaitExpression.Expression is InvocationExpressionSyntax invocation &&
                invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
                memberAccess.Name.Identifier.ValueText == "ConfigureAwait")
            {
                return false;
            }

            bodyStatement = exprStatement;
            bodyExpression = awaitExpression.Expression;
            semicolonToken = exprStatement.SemicolonToken;
            return true;
        }
        else
        {
            if (statement is ExpressionStatementSyntax exprStatement)
            {
                bodyStatement = exprStatement;
                bodyExpression = exprStatement.Expression;
                semicolonToken = exprStatement.SemicolonToken;
                return true;
            }
            else if (statement is ReturnStatementSyntax { Expression: { } expression } returnStatement)
            {
                bodyStatement = returnStatement;
                bodyExpression = expression;
                semicolonToken = returnStatement.SemicolonToken;
                return true;
            }
        }

        return false;
    }

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var (document, span, cancellationToken) = context;

        var methodDeclaration = await context.TryGetRelevantNodeAsync<MethodDeclarationSyntax>().ConfigureAwait(false);
        if (!ShouldMakeSynchronous(methodDeclaration, out var statement, out _, out _))
            return;

        context.RegisterRefactoring(CodeAction.Create(
            "Use expression body for test",
            cancellationToken => FixAsync(document, span, equivalenceKey: null, cancellationToken)));
    }

    protected override async Task FixAllAsync(
        Document document,
        ImmutableArray<TextSpan> fixAllSpans,
        SyntaxEditor editor,
        string? equivalenceKey,
        CancellationToken cancellationToken)
    {
        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
        var methodDeclarations = root
            .DescendantNodesAndSelf()
            .OfType<MethodDeclarationSyntax>()
            .Where(l => fixAllSpans.Any(static (s, l) => l.Span.IntersectsWith(s), l))
            .OrderByDescending(d => d.SpanStart);
        foreach (var methodDeclaration in methodDeclarations)
        {
            if (ShouldMakeSynchronous(methodDeclaration, out _, out var expression, out var semicolonToken))
            {
                editor.ReplaceNode(
                    methodDeclaration,
                    UpdateMethodDeclaration(methodDeclaration, expression, semicolonToken));
            }
        }
    }

    private static MethodDeclarationSyntax UpdateMethodDeclaration(
        MethodDeclarationSyntax methodDeclaration,
        ExpressionSyntax expression,
        SyntaxToken semicolonToken)
    {
        var generator = CSharpSyntaxGenerator.Instance;
        methodDeclaration = generator.WithModifiers(
            methodDeclaration,
            generator.GetModifiers(methodDeclaration).WithAsync(isAsync: false));

        var block = methodDeclaration.Body!;

        var arrow = ArrowExpressionClause(
            EqualsGreaterThanToken.WithTrailingTrivia(Space),
            expression.WithoutLeadingTrivia()).WithLeadingTrivia(block.GetLeadingTrivia().Add(Whitespace("    ")));
        methodDeclaration = methodDeclaration
            .WithBody(null)
            .WithSemicolonToken(semicolonToken)
            .WithExpressionBody(arrow);

        return methodDeclaration;
    }
}
