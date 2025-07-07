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
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Shared.Extensions;
using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Extensions;

namespace Microsoft.CodeAnalysis.CSharp.CodeRefactorings.TestCleanup;

[ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CSharpInlineTestCodeCodeRefactoringProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpInlineTestCodeCodeRefactoringProvider() : SyntaxEditorBasedCodeRefactoringProvider
{
    protected override ImmutableArray<FixAllScope> SupportedFixAllScopes => AllFixAllScopes;

    protected override CodeActionCleanup Cleanup => CodeActionCleanup.SyntaxOnly;

    private static bool IsInlinable(
        [NotNullWhen(true)] LocalDeclarationStatementSyntax? localDeclaration,
        [NotNullWhen(true)] out ExpressionSyntax? stringExpression,
        [NotNullWhen(true)] out IdentifierNameSyntax? reference)
    {
        stringExpression = null;
        reference = null;

        if (localDeclaration is not { Declaration.Variables: [var variable] })
            return false;

        var initializer = variable.Initializer?.Value;
        if (initializer is null)
            return false;

        if (initializer.Kind() is not (SyntaxKind.StringLiteralExpression or SyntaxKind.InterpolatedStringExpression))
            return false;

        if (localDeclaration.Parent is not BlockSyntax block)
            return false;

        var matches = block
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(id => id.Identifier.ValueText == variable.Identifier.ValueText)
            .ToImmutableArray();

        if (matches.Length != 1)
            return false;

        var identifierReference = matches[0];
        if (identifierReference.Parent is not AssignmentExpressionSyntax assignment ||
            assignment.Parent is not InitializerExpressionSyntax ||
            assignment.Right != identifierReference)
        {
            return false;
        }

        stringExpression = initializer;
        reference = identifierReference;
        return true;
    }

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var (document, span, cancellationToken) = context;

        var localDeclaration = await context.TryGetRelevantNodeAsync<LocalDeclarationStatementSyntax>().ConfigureAwait(false);
        if (!IsInlinable(localDeclaration, out _, out _))
            return;

        context.RegisterRefactoring(CodeAction.Create(
            "Inline test code",
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
        var localDeclarations = root
            .DescendantNodesAndSelf()
            .OfType<LocalDeclarationStatementSyntax>()
            .Where(l => fixAllSpans.Any(static (s, l) => l.Span.IntersectsWith(s), l))
            .OrderByDescending(d => d.SpanStart);
        foreach (var localDeclaration in localDeclarations)
        {
            if (IsInlinable(localDeclaration, out var stringExpression, out var reference))
            {
                editor.ReplaceNode(reference, stringExpression.WithTriviaFrom(reference));

                var removeOptions = SyntaxGenerator.DefaultRemoveOptions;
                if (localDeclaration.GetLeadingTrivia().Any(t => t.IsSingleOrMultiLineComment()))
                    removeOptions |= SyntaxRemoveOptions.KeepLeadingTrivia;

                editor.RemoveNode(localDeclaration, removeOptions);
            }
        }
    }
}
