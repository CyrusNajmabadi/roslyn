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
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.CodeRefactorings.TestCleanup;

using static SyntaxFactory;

[ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CSharpWhereOrSelectThenToImmutableArrayCodeRefactoringProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpWhereOrSelectThenToImmutableArrayCodeRefactoringProvider() : SyntaxEditorBasedCodeRefactoringProvider
{
    protected override ImmutableArray<FixAllScope> SupportedFixAllScopes => DefaultFixAllScopes;

    protected override CodeActionCleanup Cleanup => CodeActionCleanup.SyntaxOnly;

    private static bool CanSimplify(
        [NotNullWhen(true)] InvocationExpressionSyntax? invocationExpression)
    {
        return invocationExpression is
        {
            ArgumentList.Arguments.Count: 1,
            Expression: MemberAccessExpressionSyntax { Name: IdentifierNameSyntax { Identifier.ValueText: "Select" or "Where" } },
            Parent: MemberAccessExpressionSyntax
            {
                Name: IdentifierNameSyntax { Identifier.ValueText: "ToImmutableArray" },
                Parent: InvocationExpressionSyntax { ArgumentList.Arguments.Count: 0 },
            },
        };
    }

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var invocationExpression = await context.TryGetRelevantNodeAsync<InvocationExpressionSyntax>().ConfigureAwait(false);
        if (!CanSimplify(invocationExpression))
            return;

        context.RegisterRefactoring(CodeAction.Create(
            "Use Where/SelectAsArray",
            cancellationToken => this.FixAsync(context.Document, invocationExpression.Span, equivalenceKey: null, cancellationToken),
            equivalenceKey: null));
    }

    protected override async Task FixAllAsync(
        Document document,
        ImmutableArray<TextSpan> fixAllSpans,
        SyntaxEditor editor,
        string? equivalenceKey,
        CancellationToken cancellationToken)
    {
        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
        var invocationExpressions = root
            .DescendantNodes(n => fixAllSpans.Any(span => n.Span.IntersectsWith(span)))
            .OfType<InvocationExpressionSyntax>()
            .Where(i => fixAllSpans.Any(span => i.Span.IntersectsWith(span)))
            .OrderByDescending(i => i.SpanStart);

        foreach (var invocationExpression in invocationExpressions)
        {
            if (!CanSimplify(invocationExpression))
                continue;

            var parentInvocation = (InvocationExpressionSyntax)invocationExpression.GetRequiredParent().GetRequiredParent();
            var selectName = ((MemberAccessExpressionSyntax)invocationExpression.Expression).Name;

            editor.ReplaceNode(
                selectName,
                IdentifierName(selectName.Identifier.ValueText + "AsArray").WithTriviaFrom(selectName));
            editor.ReplaceNode(
                parentInvocation,
                (current, _) => ((MemberAccessExpressionSyntax)((InvocationExpressionSyntax)current).Expression).Expression.WithTrailingTrivia(current.GetTrailingTrivia()));
        }
    }
}
