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
        [NotNullWhen(true)] out ExpressionStatementSyntax? expressionStatement,
        [NotNullWhen(true)] out ExpressionSyntax? bodyExpression)
    {
        bodyExpression = null;
        expressionStatement = null;

        if (methodDeclaration?.Body is not BlockSyntax { Statements: [var statement] })
            return false;

        if (!methodDeclaration.Modifiers.Any(SyntaxKind.AsyncKeyword))
            return false;

        if (statement is not ExpressionStatementSyntax { Expression: AwaitExpressionSyntax awaitExpression } exprStatement)
            return false;

        if (awaitExpression.Expression is InvocationExpressionSyntax invocation &&
            invocation.Expression is MemberAccessExpressionSyntax memberAccess &&
            memberAccess.Name.Identifier.ValueText == "ConfigureAwait")
        {
            return false;
        }

        expressionStatement = exprStatement;
        bodyExpression = awaitExpression.Expression;
        return true;
    }

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var (document, span, cancellationToken) = context;

        var methodDeclaration = await context.TryGetRelevantNodeAsync<MethodDeclarationSyntax>().ConfigureAwait(false);
        if (!ShouldMakeSynchronous(methodDeclaration, out _, out _))
            return;

        context.RegisterRefactoring(CodeAction.Create(
            "Make method non-async",
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
            if (ShouldMakeSynchronous(methodDeclaration, out var expressionStatement, out var expression))
            {
                editor.ReplaceNode(
                    methodDeclaration,
                    UpdateMethodDeclaration(methodDeclaration, expressionStatement, expression));
            }
        }
    }

    private static MethodDeclarationSyntax UpdateMethodDeclaration(
        MethodDeclarationSyntax methodDeclaration,
        ExpressionStatementSyntax expressionStatement,
        ExpressionSyntax expression)
    {
        var generator = CSharpSyntaxGenerator.Instance;
        methodDeclaration = generator.WithModifiers(
            methodDeclaration,
            generator.GetModifiers(methodDeclaration).WithAsync(isAsync: false));

        var block = methodDeclaration.Body!;

        var arrow = ArrowExpressionClause(
            EqualsGreaterThanToken.WithTrailingTrivia(Space),
            expression).WithLeadingTrivia(block.GetLeadingTrivia().Add(Whitespace("    ")));
        methodDeclaration = methodDeclaration
            .WithBody(null)
            .WithSemicolonToken(expressionStatement.SemicolonToken)
            .WithExpressionBody(arrow);

        return methodDeclaration;
    }
}
