// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Roslyn.Utilities;
using FixAllScope = Microsoft.CodeAnalysis.CodeFixes.FixAllScope;

namespace Microsoft.CodeAnalysis.CodeFixesAndRefactorings;

/// <summary>
/// Fix all code action for a code action registered by
/// a <see cref="CodeFixes.CodeFixProvider"/> or a <see cref="CodeRefactorings.CodeRefactoringProvider"/>.
/// </summary>
internal abstract class AbstractFixAllCodeAction<
    TFixAllContext,
    TFixAllContextWitness> : CodeAction
    where TFixAllContextWitness : struct, IFixAllContextWitness<TFixAllContext>
{
    private bool _showPreviewChangesDialog;

    public IFixAllState<TFixAllContext> FixAllState { get; }

    protected AbstractFixAllCodeAction(
        IFixAllState<TFixAllContext> fixAllState, bool showPreviewChangesDialog)
    {
        FixAllState = fixAllState;
        _showPreviewChangesDialog = showPreviewChangesDialog;
    }

    /// <summary>
    /// Determine if the <see cref="IFixAllState.Provider"/> is an internal first-party provider or not.
    /// </summary>
    protected abstract bool IsInternalProvider(IFixAllState<TFixAllContext> fixAllState);

    /// <summary>
    /// Creates a new <see cref="IFixAllContext"/> with the given parameters.
    /// </summary>
    protected abstract TFixAllContext CreateFixAllContext(IFixAllState<TFixAllContext> fixAllState, IProgress<CodeAnalysisProgress> progressTracker, CancellationToken cancellationToken);

    public override string Title
        => this.FixAllState.Scope switch
        {
            FixAllScope.Document => FeaturesResources.Document,
            FixAllScope.Project => FeaturesResources.Project,
            FixAllScope.Solution => FeaturesResources.Solution,
            FixAllScope.ContainingMember => FeaturesResources.Containing_Member,
            FixAllScope.ContainingType => FeaturesResources.Containing_Type,
            _ => throw ExceptionUtilities.UnexpectedValue(this.FixAllState.Scope),
        };

    internal override string Message => FeaturesResources.Computing_fix_all_occurrences_code_fix;

    protected sealed override Task<ImmutableArray<CodeActionOperation>> ComputeOperationsAsync(
        IProgress<CodeAnalysisProgress> progressTracker, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();
        FixAllLogger.LogState(FixAllState, IsInternalProvider(FixAllState));

        var service = FixAllState.Project.Solution.Services.GetRequiredService<IFixAllGetFixesService>();

        var fixAllContext = CreateFixAllContext(FixAllState, progressTracker, cancellationToken);
        var witness = default(TFixAllContextWitness);
        progressTracker.Report(CodeAnalysisProgress.Description(witness.GetDefaultFixAllTitle(fixAllContext)));

        return service.GetFixAllOperationsAsync(fixAllContext, witness, _showPreviewChangesDialog);
    }

    protected sealed override Task<Solution?> GetChangedSolutionAsync(
        IProgress<CodeAnalysisProgress> progressTracker, CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();
        FixAllLogger.LogState(FixAllState, IsInternalProvider(FixAllState));

        var service = FixAllState.Project.Solution.Services.GetRequiredService<IFixAllGetFixesService>();

        var fixAllContext = CreateFixAllContext(FixAllState, progressTracker, cancellationToken);
        var witness = default(TFixAllContextWitness);
        progressTracker.Report(CodeAnalysisProgress.Description(witness.GetDefaultFixAllTitle(fixAllContext)));

        return service.GetFixAllChangedSolutionAsync(fixAllContext, witness);
    }

    // internal for testing purposes.
    internal TestAccessor GetTestAccessor()
        => new(this);

    // internal for testing purposes.
    internal readonly struct TestAccessor
    {
        private readonly AbstractFixAllCodeAction<TFixAllContext, TFixAllContextWitness> _fixAllCodeAction;

        internal TestAccessor(AbstractFixAllCodeAction<TFixAllContext, TFixAllContextWitness> fixAllCodeAction)
            => _fixAllCodeAction = fixAllCodeAction;

        /// <summary>
        /// Gets a reference to <see cref="_showPreviewChangesDialog"/>, which can be read or written by test code.
        /// </summary>
        public ref bool ShowPreviewChangesDialog
            => ref _fixAllCodeAction._showPreviewChangesDialog;
    }
}
