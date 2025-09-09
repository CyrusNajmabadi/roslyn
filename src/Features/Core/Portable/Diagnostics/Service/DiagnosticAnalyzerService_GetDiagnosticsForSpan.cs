// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.ErrorReporting;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Remote;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Telemetry;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Threading;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Diagnostics;

internal sealed partial class DiagnosticAnalyzerService
{
    private static async Task<ImmutableDictionary<DiagnosticAnalyzer, ImmutableArray<DiagnosticData>>> ComputeDocumentDiagnosticsCoreInProcessAsync(
        DocumentAnalysisExecutor executor,
        CancellationToken cancellationToken)
    {
        using var _ = PooledDictionary<DiagnosticAnalyzer, ImmutableArray<DiagnosticData>>.GetInstance(out var builder);
        foreach (var analyzer in executor.AnalysisScope.ProjectAnalyzers.ConcatFast(executor.AnalysisScope.HostAnalyzers))
        {
            var diagnostics = await executor.ComputeDiagnosticsInProcessAsync(analyzer, cancellationToken).ConfigureAwait(false);
            builder.Add(analyzer, diagnostics);
        }

        return builder.ToImmutableDictionary();
    }

    public async Task<ImmutableArray<DiagnosticData>> GetDiagnosticsForSpanAsync(
        TextDocument document,
        TextSpan? range,
        Func<string, ValueTask<bool>>? shouldIncludeDiagnostic,
        ICodeActionRequestPriorityProvider? priorityProvider,
        DiagnosticKind diagnosticKind,
        CancellationToken cancellationToken)
    {
        // Note: due to the in-memory work that priorityProvider and shouldIncludeDiagnostic need to do,
        // much of this function runs locally (in process) to determine which analyzers to run, before
        // finally making a call out to OOP to actually do the work.

        // always make sure that analyzer is called on background thread.
        await Task.Yield().ConfigureAwait(false);
        priorityProvider ??= new DefaultCodeActionRequestPriorityProvider();

        var text = await document.GetValueTextAsync(cancellationToken).ConfigureAwait(false);

        var project = document.Project;
        var unfilteredAnalyzers = GetProjectAnalyzers(project);
        var analyzers = unfilteredAnalyzers
            .WhereAsArray(a => DocumentAnalysisExecutor.IsAnalyzerEnabledForProject(a, project, _globalOptions));

        // Note that some callers, such as diagnostic tagger, might pass in a range equal to the entire document span.
        // We clear out range for such cases as we are computing full document diagnostics.
        if (range == new TextSpan(0, text.Length))
            range = null;

        // We log performance info when we are computing diagnostics for a span
        var logPerformanceInfo = range.HasValue;

        // If we are computing full document diagnostics, we will attempt to perform incremental
        // member edit analysis. This analysis is currently only enabled with LSP pull diagnostics.
        var incrementalAnalysis = range is null && document is Document { SupportsSyntaxTree: true };

        using var _1 = PooledHashSet<DiagnosticAnalyzer>.GetInstance(out var deprioritizationCandidates);

        deprioritizationCandidates.AddRange(await this.GetDeprioritizationCandidatesAsync(
            project, analyzers, cancellationToken).ConfigureAwait(false));

        var (syntaxAnalyzers, semanticSpanAnalyzers, semanticDocumentAnalyzers) = await GetAllAnalyzersAsync().ConfigureAwait(false);
        syntaxAnalyzers = await FilterAnalyzersAsync(syntaxAnalyzers, AnalysisKind.Syntax, range, deprioritizationCandidates).ConfigureAwait(false);
        semanticSpanAnalyzers = await FilterAnalyzersAsync(semanticSpanAnalyzers, AnalysisKind.Semantic, range, deprioritizationCandidates).ConfigureAwait(false);
        semanticDocumentAnalyzers = await FilterAnalyzersAsync(semanticDocumentAnalyzers, AnalysisKind.Semantic, span: null, deprioritizationCandidates).ConfigureAwait(false);

        var allDiagnostics = await this.ComputeDiagnosticsAsync(
            document, range, analyzers, syntaxAnalyzers, semanticSpanAnalyzers, semanticDocumentAnalyzers,
            incrementalAnalysis, logPerformanceInfo,
            cancellationToken).ConfigureAwait(false);
        return allDiagnostics.WhereAsArray(ShouldInclude);

        async ValueTask<(
            ImmutableArray<DiagnosticAnalyzer> syntaxAnalyzers,
            ImmutableArray<DiagnosticAnalyzer> semanticSpanAnalyzers,
            ImmutableArray<DiagnosticAnalyzer> semanticDocumentAnalyzers)> GetAllAnalyzersAsync()
        {
            try
            {
                using var _1 = ArrayBuilder<DiagnosticAnalyzer>.GetInstance(out var syntaxAnalyzers);

                // If we are performing incremental member edit analysis to compute diagnostics incrementally,
                // we divide the analyzers into those that support span-based incremental analysis and
                // those that do not support incremental analysis and must be executed for the entire document.
                // Otherwise, if we are not performing incremental analysis, all semantic analyzers are added
                // to the span-based analyzer set as we want to compute diagnostics only for the given span.
                using var _2 = ArrayBuilder<DiagnosticAnalyzer>.GetInstance(out var semanticSpanBasedAnalyzers);
                using var _3 = ArrayBuilder<DiagnosticAnalyzer>.GetInstance(out var semanticDocumentBasedAnalyzers);

                using var _4 = TelemetryLogging.LogBlockTimeAggregatedHistogram(FunctionId.RequestDiagnostics_Summary, $"Pri{priorityProvider.Priority.GetPriorityInt()}");

                foreach (var analyzer in analyzers)
                {
                    if (!await ShouldIncludeAnalyzerAsync(
                            analyzer, shouldIncludeDiagnostic, priorityProvider, this, cancellationToken).ConfigureAwait(false))
                    {
                        continue;
                    }

                    bool includeSyntax = true, includeSemantic = true;
                    if (diagnosticKind != DiagnosticKind.All)
                    {
                        var isCompilerAnalyzer = analyzer.IsCompilerAnalyzer();
                        includeSyntax = isCompilerAnalyzer
                            ? diagnosticKind == DiagnosticKind.CompilerSyntax
                            : diagnosticKind == DiagnosticKind.AnalyzerSyntax;
                        includeSemantic = isCompilerAnalyzer
                            ? diagnosticKind == DiagnosticKind.CompilerSemantic
                            : diagnosticKind == DiagnosticKind.AnalyzerSemantic;
                    }

                    includeSyntax = includeSyntax && analyzer.SupportAnalysisKind(AnalysisKind.Syntax);
                    includeSemantic = includeSemantic && analyzer.SupportAnalysisKind(AnalysisKind.Semantic) && document is Document;

                    if (includeSyntax || includeSemantic)
                    {
                        if (includeSyntax)
                        {
                            syntaxAnalyzers.Add(analyzer);
                        }

                        if (includeSemantic)
                        {
                            if (!incrementalAnalysis)
                            {
                                // For non-incremental analysis, we always attempt to compute all
                                // analyzer diagnostics for the requested span.
                                semanticSpanBasedAnalyzers.Add(analyzer);
                            }
                            else if (analyzer.SupportsSpanBasedSemanticDiagnosticAnalysis())
                            {
                                // We can perform incremental analysis only for analyzers that support
                                // span-based semantic diagnostic analysis.
                                semanticSpanBasedAnalyzers.Add(analyzer);
                            }
                            else
                            {
                                semanticDocumentBasedAnalyzers.Add(analyzer);
                            }
                        }
                    }
                }

                return (
                    syntaxAnalyzers.ToImmutableAndClear(),
                    semanticSpanBasedAnalyzers.ToImmutableAndClear(),
                    semanticDocumentBasedAnalyzers.ToImmutableAndClear());
            }
            catch (Exception e) when (FatalError.ReportAndPropagateUnlessCanceled(e, cancellationToken))
            {
                throw ExceptionUtilities.Unreachable();
            }
        }

        // Local functions
        static async ValueTask<bool> ShouldIncludeAnalyzerAsync(
            DiagnosticAnalyzer analyzer,
            Func<string, ValueTask<bool>>? shouldIncludeDiagnostic,
            ICodeActionRequestPriorityProvider priorityProvider,
            DiagnosticAnalyzerService owner,
            CancellationToken cancellationToken)
        {
            // Skip executing analyzer if its priority does not match the request priority.
            if (!await priorityProvider.MatchesPriorityAsync(analyzer, cancellationToken).ConfigureAwait(false))
                return false;

            // Special case DocumentDiagnosticAnalyzer to never skip these document analyzers based on
            // 'shouldIncludeDiagnostic' predicate. More specifically, TS has special document analyzer which report
            // 0 supported diagnostics, but we always want to execute it.  This also applies to our special built in
            // analyzers 'FileContentLoadAnalyzer' and 'GeneratorDiagnosticsPlaceholderAnalyzer'.
            if (analyzer is DocumentDiagnosticAnalyzer)
                return true;

            // Skip analyzer if none of its reported diagnostics should be included.
            if (shouldIncludeDiagnostic != null &&
                !owner._analyzerInfoCache.GetDiagnosticDescriptors(analyzer).Any(static (a, shouldIncludeDiagnostic) => shouldIncludeDiagnostic(a.Id), shouldIncludeDiagnostic))
            {
                return false;
            }

            return true;
        }

        async ValueTask<ImmutableArray<DiagnosticAnalyzer>> FilterAnalyzersAsync(
            ImmutableArray<DiagnosticAnalyzer> analyzers,
            AnalysisKind kind,
            TextSpan? span,
            HashSet<DiagnosticAnalyzer> deprioritizationCandidates)
        {
            using var _1 = ArrayBuilder<DiagnosticAnalyzer>.GetInstance(analyzers.Length, out var filteredAnalyzers);

            foreach (var analyzer in analyzers)
            {
                Debug.Assert(await priorityProvider.MatchesPriorityAsync(analyzer, cancellationToken).ConfigureAwait(false));

                // Check if this is an expensive analyzer that needs to be de-prioritized to a lower priority bucket.
                // If so, we skip this analyzer from execution in the current priority bucket.
                // We will subsequently execute this analyzer in the lower priority bucket.
                if (await TryDeprioritizeAnalyzerAsync(
                        analyzer, kind, span, deprioritizationCandidates).ConfigureAwait(false))
                {
                    continue;
                }

                filteredAnalyzers.Add(analyzer);
            }

            return filteredAnalyzers.ToImmutableAndClear();
        }

        async ValueTask<bool> TryDeprioritizeAnalyzerAsync(
            DiagnosticAnalyzer analyzer, AnalysisKind kind, TextSpan? span,
            HashSet<DiagnosticAnalyzer> deprioritizationCandidates)
        {
            // PERF: In order to improve lightbulb performance, we perform de-prioritization optimization for certain analyzers
            // that moves the analyzer to a lower priority bucket. However, to ensure that de-prioritization happens for very rare cases,
            // we only perform this optimizations when following conditions are met:
            //  1. We are performing semantic span-based analysis.
            //  2. We are processing 'CodeActionRequestPriority.Normal' priority request.
            //  3. Analyzer registers certain actions that are known to lead to high performance impact due to its broad analysis scope,
            //     such as SymbolStart/End actions and SemanticModel actions.
            //  4. Analyzer did not report a diagnostic on the same line in prior document snapshot.

            // Conditions 1. and 2.
            if (kind != AnalysisKind.Semantic ||
                !span.HasValue ||
                priorityProvider.Priority != CodeActionRequestPriority.Default)
            {
                return false;
            }

            Debug.Assert(span.Value.Length < text.Length);

            // Condition 3.
            // Check if this is a candidate analyzer that can be de-prioritized into a lower priority bucket based on registered actions.
            if (!deprioritizationCandidates.Contains(analyzer))
                return false;

            // 'LightbulbSkipExecutingDeprioritizedAnalyzers' option determines if we want to execute this analyzer
            // in low priority bucket or skip it completely. If the option is not set, track the de-prioritized
            // analyzer to be executed in low priority bucket.
            // Note that 'AddDeprioritizedAnalyzerWithLowPriority' call below mutates the state in the provider to
            // track this analyzer. This ensures that when the owner of this provider calls us back to execute
            // the low priority bucket, we can still get back to this analyzer and execute it that time.
            if (!this._globalOptions.GetOption(DiagnosticOptionsStorage.LightbulbSkipExecutingDeprioritizedAnalyzers))
            {
                await priorityProvider.AddDeprioritizedAnalyzerWithLowPriorityAsync(
                    analyzer.GetType().FullName!,
                    analyzer.SupportedDiagnostics.SelectAsArray(d => d.Id),
                    cancellationToken).ConfigureAwait(false);
            }

            return true;
        }

        async ValueTask<bool> ShouldIncludeAsync(DiagnosticData diagnostic)
        {
            if (diagnostic.DocumentId != document.Id)
                return false;

            if (range != null && !range.Value.IntersectsWith(diagnostic.DataLocation.UnmappedFileSpan.GetClampedTextSpan(text)))
                return false;

            if (shouldIncludeDiagnostic != null &&
                !await shouldIncludeDiagnostic(diagnostic.Id).ConfigureAwait(false))
            {
                return false;
            }

            return true;
        }
    }

    public async Task<ImmutableArray<DiagnosticData>> ComputeDiagnosticsInProcessAsync(
        TextDocument document,
        TextSpan? range,
        ImmutableArray<DiagnosticAnalyzer> allAnalyzers,
        ImmutableArray<DiagnosticAnalyzer> syntaxAnalyzers,
        ImmutableArray<DiagnosticAnalyzer> semanticSpanAnalyzers,
        ImmutableArray<DiagnosticAnalyzer> semanticDocumentAnalyzers,
        bool incrementalAnalysis,
        bool logPerformanceInfo,
        CancellationToken cancellationToken)
    {
        // We log performance info when we are computing diagnostics for a span
        var project = document.Project;

        var hostAnalyzerInfo = GetOrCreateHostAnalyzerInfo(project);
        var compilationWithAnalyzers = await GetOrCreateCompilationWithAnalyzersAsync(
            document.Project, allAnalyzers, hostAnalyzerInfo, this.CrashOnAnalyzerException, cancellationToken).ConfigureAwait(false);

        using var _ = ArrayBuilder<DiagnosticData>.GetInstance(out var list);

        await ComputeDocumentDiagnosticsAsync(syntaxAnalyzers, AnalysisKind.Syntax, range, incrementalAnalysis: false).ConfigureAwait(false);
        await ComputeDocumentDiagnosticsAsync(semanticSpanAnalyzers, AnalysisKind.Semantic, range, incrementalAnalysis).ConfigureAwait(false);
        await ComputeDocumentDiagnosticsAsync(semanticDocumentAnalyzers, AnalysisKind.Semantic, span: null, incrementalAnalysis: false).ConfigureAwait(false);

        return list.ToImmutableAndClear();

        async Task ComputeDocumentDiagnosticsAsync(
            ImmutableArray<DiagnosticAnalyzer> analyzers,
            AnalysisKind kind,
            TextSpan? span,
            bool incrementalAnalysis)
        {
            if (analyzers.Length == 0)
                return;

            Debug.Assert(!incrementalAnalysis || kind == AnalysisKind.Semantic);
            Debug.Assert(!incrementalAnalysis || analyzers.All(analyzer => analyzer.SupportsSpanBasedSemanticDiagnosticAnalysis()));

            var projectAnalyzers = analyzers.WhereAsArray(static (a, info) => !info.IsHostAnalyzer(a), hostAnalyzerInfo);
            var hostAnalyzers = analyzers.WhereAsArray(static (a, info) => info.IsHostAnalyzer(a), hostAnalyzerInfo);
            var analysisScope = new DocumentAnalysisScope(document, span, projectAnalyzers, hostAnalyzers, kind);
            var executor = new DocumentAnalysisExecutor(this, analysisScope, compilationWithAnalyzers, logPerformanceInfo);
            var version = await GetDiagnosticVersionAsync(document.Project, cancellationToken).ConfigureAwait(false);

            var computeTask = incrementalAnalysis
                ? _incrementalMemberEditAnalyzer.ComputeDiagnosticsInProcessAsync(executor, analyzers, version, cancellationToken)
                : ComputeDocumentDiagnosticsCoreInProcessAsync(executor, cancellationToken);
            var diagnosticsMap = await computeTask.ConfigureAwait(false);

            if (incrementalAnalysis)
                _incrementalMemberEditAnalyzer.UpdateDocumentWithCachedDiagnostics((Document)document);

            list.AddRange(diagnosticsMap.SelectMany(kvp => kvp.Value));
        }
    }
}
