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

public abstract partial class CodeAction
{
    internal static async ValueTask<Document> CleanupDocumentAsync(Document document, CodeCleanupOptions options, CancellationToken cancellationToken)
    {
        if (!document.SupportsSyntaxTree)
            return document;

        var cleanedSolution = await CodeActionHelpers.RunAllCleanupPassesInOrderAsync(
            document.Project.Solution,
            [(document.Id, options)],
            CodeAnalysisProgress.None,
            cancellationToken).ConfigureAwait(false);

        return cleanedSolution.GetRequiredDocument(document.Id);
    }
}
