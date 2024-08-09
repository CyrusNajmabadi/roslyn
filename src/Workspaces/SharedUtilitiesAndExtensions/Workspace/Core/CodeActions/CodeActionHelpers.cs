// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CaseCorrection;
using Microsoft.CodeAnalysis.CodeCleanup;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Remote;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Microsoft.CodeAnalysis.Simplification;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeActions;

internal static class CodeActionHelpers
{
    public static ImmutableArray<DocumentId> GetAllChangedOrAddedDocumentIds(
        Solution originalSolution,
        Solution changedSolution)
    {
        var solutionChanges = changedSolution.GetChanges(originalSolution);
        var documentIds = solutionChanges
            .GetProjectChanges()
            .SelectMany(p => p.GetChangedDocuments(onlyGetDocumentsWithTextChanges: true).Concat(p.GetAddedDocuments()))
            .Concat(solutionChanges.GetAddedProjects().SelectMany(p => p.DocumentIds))
            .ToImmutableArray();
        return documentIds;
    }

    internal static async Task<Solution> CleanSyntaxAndSemanticsAsync(
        Solution originalSolution,
        Solution changedSolution,
        IProgress<CodeAnalysisProgress> progress,
        CancellationToken cancellationToken)
    {
        var documentIds = CodeActionHelpers.GetAllChangedOrAddedDocumentIds(originalSolution, changedSolution);
        var documentIdsAndOptionsToClean = await GetDocumentIdsAndOptionsToCleanAsync().ConfigureAwait(false);

        // Then do a pass where we cleanup semantics.
        var cleanedSolution = await RunAllCleanupPassesInOrderAsync(
            changedSolution, documentIdsAndOptionsToClean, progress, cancellationToken).ConfigureAwait(false);

        return cleanedSolution;

        async Task<ImmutableArray<(DocumentId documentId, CodeCleanupOptions codeCleanupOptions)>> GetDocumentIdsAndOptionsToCleanAsync()
        {
            using var _ = ArrayBuilder<(DocumentId documentId, CodeCleanupOptions options)>.GetInstance(documentIds.Length, out var documentIdsAndOptions);
            foreach (var documentId in documentIds)
            {
                var document = changedSolution.GetRequiredDocument(documentId);

                // Only care about documents that support syntax.  Non-C#/VB files can't be cleaned.
                if (document.SupportsSyntaxTree)
                {
                    var codeActionOptions = await document.GetCodeCleanupOptionsAsync(cancellationToken).ConfigureAwait(false);
                    documentIdsAndOptions.Add((documentId, codeActionOptions));
                }
            }

            return documentIdsAndOptions.ToImmutableAndClear();
        }
    }
}
