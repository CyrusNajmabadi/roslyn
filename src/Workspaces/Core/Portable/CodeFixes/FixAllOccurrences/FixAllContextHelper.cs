// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixesAndRefactorings;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeFixes;

internal static partial class FixAllContextHelper
{

    public static async Task<ImmutableDictionary<Document, ImmutableArray<Diagnostic>>> GetDocumentDiagnosticsToFixAsync(
        FixAllContext fixAllContext)
    {
        var builder = ImmutableDictionary.CreateBuilder<Document, ImmutableArray<Diagnostic>>();
        await GetDocumentDiagnosticsToFixAsync(
            fixAllContext, tuple => builder.Add(tuple.document, tuple.diagnostics)).ConfigureAwait(false);
        return builder.ToImmutable();
    }

    public static async Task GetDocumentDiagnosticsToFixAsync(
        FixAllContext fixAllContext, Action<(Document document, ImmutableArray<Diagnostic> diagnostics)> callback)
    {
        var cancellationToken = fixAllContext.CancellationToken;

        var document = fixAllContext.Document;
        var project = fixAllContext.Project;

        switch (fixAllContext.Scope)
        {
            case FixAllScope.Document:
                // Note: We avoid fixing diagnostics in generated code.
                if (document != null && !await document.IsGeneratedCodeAsync(cancellationToken).ConfigureAwait(false))
                {
                    var documentDiagnostics = await fixAllContext.GetDocumentDiagnosticsAsync(document).ConfigureAwait(false);
                    callback((document, documentDiagnostics));
                }

                return;

            case FixAllScope.ContainingMember or FixAllScope.ContainingType:
                // Note: We avoid fixing diagnostics in generated code.
                if (document != null && !await document.IsGeneratedCodeAsync(cancellationToken).ConfigureAwait(false))
                {
                    var diagnosticSpan = fixAllContext.State.DiagnosticSpan;
                    if (diagnosticSpan.HasValue &&
                        document.GetLanguageService<IFixAllSpanMappingService>() is { } spanMappingService)
                    {
                        var documentsAndSpans = await spanMappingService.GetFixAllSpansAsync(document,
                            diagnosticSpan.Value, fixAllContext.Scope, cancellationToken).ConfigureAwait(false);
                        await GetSpanDiagnosticsAsync(fixAllContext, documentsAndSpans, callback).ConfigureAwait(false);
                    }
                }

                return;

            case FixAllScope.Project:
                {
                    var allDiagnostics = await fixAllContext.GetAllDiagnosticsAsync(project).ConfigureAwait(false);
                    await GetDocumentDiagnosticsToFixAsync(
                        fixAllContext.Solution, allDiagnostics, callback, cancellationToken).ConfigureAwait(false);
                }

                return;

            case FixAllScope.Solution:
                {
                    var projectsToFix = project.Solution.Projects
                        .Where(p => p.Language == project.Language)
                        .ToImmutableArray();

                    // Update the progress dialog with the count of projects to actually fix. We'll update the progress
                    // bar as we get all the documents in AddDocumentDiagnosticsAsync.

                    fixAllContext.Progress.AddItems(projectsToFix.Length);

                    var allDiagnostics = await ProducerConsumer<ImmutableArray<Diagnostic>>.RunParallelAsync(
                        source: projectsToFix,
                        produceItems: static async (projectToFix, callback, fixAllContext, cancellationToken) =>
                        {
                            using var _ = fixAllContext.Progress.ItemCompletedScope();
                            callback(await fixAllContext.GetAllDiagnosticsAsync(projectToFix).ConfigureAwait(false));
                        },
                        consumeItems: static async (results, args, cancellationToken) =>
                        {
                            using var _ = ArrayBuilder<Diagnostic>.GetInstance(out var builder);

                            await foreach (var diagnostics in results)
                                builder.AddRange(diagnostics);

                            return builder.ToImmutableAndClear();
                        },
                        args: fixAllContext,
                        cancellationToken).ConfigureAwait(false);

                    await GetDocumentDiagnosticsToFixAsync(
                        fixAllContext.Solution, allDiagnostics, callback, cancellationToken).ConfigureAwait(false);
                }

                return;
        }

        static async Task GetSpanDiagnosticsAsync(
            FixAllContext fixAllContext,
            IEnumerable<KeyValuePair<Document, ImmutableArray<TextSpan>>> documentsAndSpans,
            Action<(Document document, ImmutableArray<Diagnostic> diagnostics)> callback)
        {
            using var _ = ArrayBuilder<Diagnostic>.GetInstance(out var diagnostics);

            foreach (var (document, spans) in documentsAndSpans)
            {
                foreach (var span in spans)
                {
                    var documentDiagnostics = await fixAllContext.GetDocumentSpanDiagnosticsAsync(document, span).ConfigureAwait(false);
                    diagnostics.AddRange(documentDiagnostics);
                }

                callback((document, diagnostics.ToImmutableAndClear()));
            }
        }
    }

    private static async Task GetDocumentDiagnosticsToFixAsync(
        Solution solution,
        ImmutableArray<Diagnostic> diagnostics,
        Action<(Document document, ImmutableArray<Diagnostic> diagnostics)> callback,
        CancellationToken cancellationToken)
    {
        // NOTE: We use 'GetTextDocumentForLocation' extension to ensure we also handle external location diagnostics in non-C#/VB languages.
        foreach (var (textDocument, diagnosticsForDocument) in diagnostics.GroupBy(d => solution.GetTextDocumentForLocation(d.Location)))
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (textDocument is not Document document)
                continue;

            if (await document.IsGeneratedCodeAsync(cancellationToken).ConfigureAwait(false))
                continue;

            callback((document, diagnosticsForDocument.ToImmutableArray()));
        }
    }
}
