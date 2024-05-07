// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

// Ignore Spelling: loc kvp

using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeFixesAndRefactorings;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.UsePrimaryConstructor;

internal partial class CSharpUsePrimaryConstructorCodeFixProvider
{
#if !CODE_STYLE // Currently depends on helpers only available in workspace layer.

    /// <summary>
    /// Specialized fix-all provider.  Needed because the fix ends up needing to invoke 'find references' to update
    /// references to removed members to point at parameters.  This allows us to do all that work across all documents
    /// and diagnostics using a single <see cref="SolutionEditor"/> that aggregates the results efficiently.
    /// </summary>
    private sealed class CSharpUsePrimaryConstructorFixAllProvider : FixAllProvider
    {
        public override Task<CodeAction?> GetFixAsync(FixAllContext fixAllContext)
        {
            return DefaultFixAllProviderHelpers.GetFixAsync(
                fixAllContext.GetDefaultFixAllTitle(), fixAllContext, FixAllContextsHelperAsync);
        }

        private static async Task<Solution?> FixAllContextsHelperAsync(FixAllContext originalContext, ImmutableArray<FixAllContext> contexts)
        {
            var cancellationToken = originalContext.CancellationToken;
            var removeMembers = originalContext.CodeActionEquivalenceKey == nameof(CSharpCodeFixesResources.Use_primary_constructor_and_remove_members);

            var solutionEditor = new SolutionEditor(originalContext.Solution);

            foreach (var currentContext in contexts)
            {
                await ProducerConsumer<(Document document, ImmutableArray<Diagnostic> diagnostics)>.RunAsync(
                    ProducerConsumerOptions.MultipleReaderMultipleWriter,
                    produceItems: static (callback, args, cancellationToken) => FixAllContextHelper.GetDocumentDiagnosticsToFixAsync(args.currentContext, callback),
                    consumeItems: static async (stream, args, cancellationToken) =>
                    {
                        await foreach (var (document, diagnostics) in stream)
                        {
                            foreach (var diagnostic in diagnostics.OrderByDescending(d => d.Location.SourceSpan.Start))
                            {
                                if (diagnostic.Location.FindNode(cancellationToken) is not ConstructorDeclarationSyntax constructorDeclaration)
                                    continue;

                                await UsePrimaryConstructorAsync(
                                    args.solutionEditor, document, constructorDeclaration, diagnostic.Properties, args.removeMembers, cancellationToken).ConfigureAwait(false);
                            }
                        }
                    },
                    args: (solutionEditor, removeMembers, currentContext),
                    cancellationToken).ConfigureAwait(false);
            }

            return solutionEditor.GetChangedSolution();
        }
    }
#endif
}
