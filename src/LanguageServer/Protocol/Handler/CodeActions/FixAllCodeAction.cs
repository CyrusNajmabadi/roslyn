// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Threading;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeFixesAndRefactorings;

namespace Microsoft.CodeAnalysis.LanguageServer.Handler.CodeActions;

internal sealed class FixAllCodeAction
    : AbstractFixAllCodeAction<FixAllContext, FixAllContextWitness>
{
    private readonly string _title;

    public FixAllCodeAction(string title, IFixAllState<FixAllContext> fixAllState, bool showPreviewChangesDialog) : base(fixAllState, showPreviewChangesDialog)
    {
        _title = title;
    }

    public override string Title
        => _title;

    protected override FixAllContext CreateFixAllContext(IFixAllState<FixAllContext> fixAllState, IProgress<CodeAnalysisProgress> progressTracker, CancellationToken cancellationToken)
        => new((FixAllState)fixAllState, progressTracker, cancellationToken);

    protected override bool IsInternalProvider(IFixAllState<FixAllContext> fixAllState)
        => true; // FixAll support is internal for the language server.
}
