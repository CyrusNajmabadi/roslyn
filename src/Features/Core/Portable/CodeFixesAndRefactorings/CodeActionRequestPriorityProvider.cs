// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeActions;

internal interface ICodeActionRequestPriorityProvider
{
    /// <summary>
    /// <see langword="null"/> represents no specified priority.  i.e. any priority should match this.
    /// </summary>
    CodeActionRequestPriority? Priority { get; }

    /// <summary>
    /// Tracks the given <paramref name="analyzerTypeName"/> as a de-prioritized analyzer that should be moved to
    /// <see cref="CodeActionRequestPriority.Low"/> bucket.
    /// </summary>
    ValueTask AddDeprioritizedAnalyzerWithLowPriorityAsync(
        string analyzerTypeName, ImmutableArray<string> supportedDiagnosticIds, CancellationToken cancellationToken);

    ValueTask<bool> IsDeprioritizedAnalyzerWithLowPriorityAsync(string analyzerTypeName, CancellationToken cancellationToken);

    /// <summary>
    /// Indicates whether any deprioritized analyzer supports one of the passed in diagnostic ids.
    /// </summary>
    ValueTask<bool> HasDeprioritizedAnalyzerSupportingDiagnosticIdAsync(
        ImmutableArray<string> diagnosticIds, CancellationToken cancellationToken);
}

internal static class ICodeActionRequestPriorityProviderExtensions
{
    /// <summary>
    /// Returns true if the given <paramref name="analyzer"/> can report diagnostics that can have fixes from a code
    /// fix provider with <see cref="CodeFixProvider.RequestPriority"/> matching <see
    /// cref="ICodeActionRequestPriorityProvider.Priority"/>. This method is useful for performing a performance
    /// optimization for lightbulb diagnostic computation, wherein we can reduce the set of analyzers to be executed
    /// when computing fixes for a specific <see cref="ICodeActionRequestPriorityProvider.Priority"/>.
    /// </summary>
    public static async ValueTask<bool> MatchesPriorityAsync(
        this ICodeActionRequestPriorityProvider provider,
        DiagnosticAnalyzer analyzer,
        CancellationToken cancellationToken)
    {
        var priority = provider.Priority;

        // If caller isn't asking for prioritized result, then run all analyzers.
        if (priority is null)
            return true;

        // 'CodeActionRequestPriority.Lowest' is used for suppression/configuration fixes,
        // which requires all analyzer diagnostics.
        if (priority == CodeActionRequestPriority.Lowest)
            return true;

        // The compiler analyzer always counts for any priority.  It's diagnostics may be fixed
        // by high pri or normal pri fixers.
        if (analyzer.IsCompilerAnalyzer())
            return true;

        // Check if we are computing diagnostics for 'CodeActionRequestPriority.Low' and
        // this analyzer was de-prioritized to low priority bucket.
        if (priority == CodeActionRequestPriority.Low &&
            await provider.IsDeprioritizedAnalyzerWithLowPriorityAsync(
                analyzer.GetType().FullName!, cancellationToken).ConfigureAwait(false))
        {
            return true;
        }

        // Now compute this analyzer's priority and compare it with the provider's request 'Priority'.
        // Our internal 'IBuiltInAnalyzer' can specify custom request priority, while all
        // the third-party analyzers are assigned 'Medium' priority.
        var analyzerPriority = analyzer is IBuiltInAnalyzer { IsHighPriority: true }
            ? CodeActionRequestPriority.High
            : CodeActionRequestPriority.Default;

        return priority == analyzerPriority;
    }

    /// <summary>
    /// Returns true if the given <paramref name="codeFixProvider"/> should be considered a candidate when computing
    /// fixes for the given <see cref="ICodeActionRequestPriorityProvider.Priority"/>.
    /// </summary>
    public static async ValueTask<bool> MatchesPriorityAsync(
        this ICodeActionRequestPriorityProvider provider,
        CodeFixProvider codeFixProvider,
        CancellationToken cancellationToken)
    {
        if (provider.Priority == null)
        {
            // We are computing fixes for all priorities
            return true;
        }

        if (provider.Priority == codeFixProvider.RequestPriority)
        {
            return true;
        }

        if (provider.Priority == CodeActionRequestPriority.Low
            && await provider.HasDeprioritizedAnalyzerSupportingDiagnosticIdAsync(codeFixProvider.FixableDiagnosticIds, cancellationToken).ConfigureAwait(false)
            && codeFixProvider.RequestPriority > CodeActionRequestPriority.Low)
        {
            // 'Low' priority can be used for two types of code fixers:
            //  1. Those which explicitly set their 'RequestPriority' to 'Low' and
            //  2. Those which can fix diagnostics for expensive analyzers which were de-prioritized
            //     to 'Low' priority bucket to improve lightbulb population performance.
            // The first case is handled by the earlier check against matching priorities. For the second
            // case, we accept fixers with any RequestPriority, as long as they can fix a diagnostic from
            // an analyzer that was executed in the 'Low' bucket.
            return true;
        }

        return false;
    }
}

internal sealed class DefaultCodeActionRequestPriorityProvider(CodeActionRequestPriority? priority = null)
    : ICodeActionRequestPriorityProvider
{
    private readonly SemaphoreSlim _gate = new(initialCount: 1);
    private HashSet<string>? _lowPriorityAnalyzers;
    private HashSet<string>? _lowPriorityAnalyzerSupportedDiagnosticIds;

    public CodeActionRequestPriority? Priority { get; } = priority;

    public async ValueTask AddDeprioritizedAnalyzerWithLowPriorityAsync(
        string analyzerTypeName, ImmutableArray<string> supportedDiagnosticIds, CancellationToken cancellationToken)
    {
        using (await _gate.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
        {
            _lowPriorityAnalyzers ??= [];
            _lowPriorityAnalyzerSupportedDiagnosticIds ??= [];

            _lowPriorityAnalyzers.Add(analyzerTypeName);
            _lowPriorityAnalyzerSupportedDiagnosticIds.AddRange(supportedDiagnosticIds);
        }
    }

    public async ValueTask<bool> HasDeprioritizedAnalyzerSupportingDiagnosticIdAsync(
        ImmutableArray<string> diagnosticIds, CancellationToken cancellationToken)
    {
        using (await _gate.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
        {
            if (_lowPriorityAnalyzerSupportedDiagnosticIds == null)
                return false;

            foreach (var diagnosticId in diagnosticIds)
            {
                if (_lowPriorityAnalyzerSupportedDiagnosticIds.Contains(diagnosticId))
                    return true;
            }

            return false;
        }
    }

    public async ValueTask<bool> IsDeprioritizedAnalyzerWithLowPriorityAsync(
        string analyzerTypeName, CancellationToken cancellationToken)
    {
        using (await _gate.DisposableWaitAsync(cancellationToken).ConfigureAwait(false))
        {
            return _lowPriorityAnalyzers != null && _lowPriorityAnalyzers.Contains(analyzerTypeName);
        }
    }
}
