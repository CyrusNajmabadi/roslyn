// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.CodeRefactorings.TestCleanup;

[ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CSharpInlineTestCodeCodeRefactoringProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpFixTestArgumentIndentationCodeRefactoringProvider() : SyntaxEditorBasedCodeRefactoringProvider
{
    protected override ImmutableArray<FixAllScope> SupportedFixAllScopes => AllFixAllScopes;

    protected override CodeActionCleanup Cleanup => CodeActionCleanup.SyntaxOnly;

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var (document, span, cancellationToken) = context;
        var invocation = await context.TryGetRelevantNodeAsync<InvocationExpressionSyntax>().ConfigureAwait(false);
        if (invocation is null)
            return;

        var text = await context.Document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        var (firstIndentedStringArgument, _, _) = GetFirstIndentedStringArgument(text, invocation);
        if (firstIndentedStringArgument is null)
            return;

        foreach (var argument in invocation.ArgumentList.Arguments)
        {
            var line = text.Lines.GetLineFromPosition(argument.SpanStart);
            if (line.Span.Start != argument.SpanStart)
                continue;

            context.RegisterRefactoring(CodeAction.Create(
                "Fix argument indentation",
                cancellationToken => FixAsync(document, span, equivalenceKey: null, cancellationToken)));
            return;
        }
    }

    private static (ArgumentSyntax? argument, int offset, SyntaxTrivia whitespace) GetFirstIndentedStringArgument(SourceText text, InvocationExpressionSyntax invocation)
    {
        foreach (var argument in invocation.ArgumentList.Arguments)
        {
            if (argument.NameColon is not null)
                continue;

            if (argument.Expression is not InterpolatedStringExpressionSyntax and not LiteralExpressionSyntax(SyntaxKind.StringLiteralExpression))
                continue;

            var line = text.Lines.GetLineFromPosition(argument.SpanStart);
            var offset = line.GetFirstNonWhitespaceOffset();

            if (offset is not > 0)
                continue;

            if (argument.GetLeadingTrivia() is not [.., SyntaxTrivia(SyntaxKind.WhitespaceTrivia) whitespaceTrivia])
                continue;

            return (argument, offset.Value, whitespaceTrivia);
        }

        return default;
    }

    protected override async Task FixAllAsync(
        Document document,
        ImmutableArray<TextSpan> fixAllSpans,
        SyntaxEditor editor,
        string? equivalenceKey,
        CancellationToken cancellationToken)
    {
        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
        var text = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);

        var invocations = root
            .DescendantNodesAndSelf()
            .OfType<InvocationExpressionSyntax>()
            .Where(l => fixAllSpans.Any(static (s, l) => l.Span.IntersectsWith(s), l));

        using var _ = PooledHashSet<InvocationExpressionSyntax>.GetInstance(out var invocationSet);

        foreach (var invocation in invocations)
        {
            // Don't process inner invocations if we processed an outer one.
            if (invocation.Ancestors().OfType<InvocationExpressionSyntax>().Any(static (i, invocationSet) => invocationSet.Contains(i), invocationSet))
                continue;

            var (firstIndentedStringArgument, stringArgumentOffset, stringArgumentWhitespace) = GetFirstIndentedStringArgument(text, invocation);
            if (firstIndentedStringArgument is null)
                continue;

            var madeChanges = false;
            foreach (var argument in invocation.ArgumentList.Arguments)
            {
                if (argument == firstIndentedStringArgument)
                    continue;

                if (!text.AreOnSameLine(argument.SpanStart, argument.Span.End))
                    continue;

                var argumentLine = text.Lines.GetLineFromPosition(argument.SpanStart);
                if (argumentLine.GetFirstNonWhitespaceOffset() == stringArgumentOffset)
                    continue;

                var leadingTrivia = argument.GetLeadingTrivia();
                if (leadingTrivia is not [SyntaxTrivia(SyntaxKind.WhitespaceTrivia)])
                    continue;

                // Reasonable argument to indent.
                editor.ReplaceNode(
                    argument,
                    argument.WithLeadingTrivia(stringArgumentWhitespace));
                madeChanges = true;
            }

            if (!madeChanges)
                continue;

            // We did update this invocation.  Keep track so that we ignore future inner invocations.
            invocationSet.Add(invocation);
        }
    }
}
