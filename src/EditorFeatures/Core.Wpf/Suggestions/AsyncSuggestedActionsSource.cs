// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.Editor.Shared;
using Microsoft.CodeAnalysis.Editor.Shared.Utilities;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Remote;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.UnifiedSuggestions;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Telemetry;
using Microsoft.VisualStudio.Telemetry.Metrics;
using Microsoft.VisualStudio.Telemetry.Metrics.Events;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Telemetry
{
    internal sealed class TimeBasedHistogramFactory : ITimeBasedHistogramFactory
    {
        /// <summary>
        /// Defaults that go from 50ms to about 10minutes.  
        /// </summary>
        private static readonly double[] s_defaultHistogramBuckets =
        {
            50 * Math.Pow(1.5, 0),  // 50ms
            50 * Math.Pow(1.5, 1),  // 70ms
            50 * Math.Pow(1.5, 2),  // 113ms
            50 * Math.Pow(1.5, 3),  // 169ms
            50 * Math.Pow(1.5, 4),  // 253ms
            50 * Math.Pow(1.5, 5),  // 380ms
            50 * Math.Pow(1.5, 6),  // 570ms
            50 * Math.Pow(1.5, 7),  // 854ms
            50 * Math.Pow(1.5, 8),  // 1.3s
            50 * Math.Pow(1.5, 9),  // 1.9s
            50 * Math.Pow(1.5, 10), // 2.8s
            50 * Math.Pow(1.5, 11), // 4.3s
            50 * Math.Pow(1.5, 12), // 6.5s
            50 * Math.Pow(1.5, 13), // 9.7s
            50 * Math.Pow(1.5, 14), // 14.5s
            50 * Math.Pow(1.5, 15), // 21.9s
            50 * Math.Pow(1.5, 16), // 32.8s
            50 * Math.Pow(1.5, 17), // 49.2s
            50 * Math.Pow(1.5, 18), // 1.2m
            50 * Math.Pow(1.5, 19), // 1.8m
            50 * Math.Pow(1.5, 20), // 2.8m
            50 * Math.Pow(1.5, 21), // 4.1m
            50 * Math.Pow(1.5, 22), // 6.2s
            50 * Math.Pow(1.5, 23), // 9.3s
        };

        private static readonly HistogramConfiguration s_histogramConfiguration = new(s_defaultHistogramBuckets, recordMinMax: true);

        private readonly VSTelemetryMeterProvider _meterProvider = new();
        private readonly TelemetrySession _session;
        private readonly IMeter _meter;
        private readonly TelemetryEvent _event;

        private readonly ConcurrentDictionary<(string name, string description), TimeBasedHistogram> _histogramMap = new();

        private readonly AsyncBatchingWorkQueue<TimeBasedHistogram> _postDataQueue;

        private TimeBasedHistogramFactory(
            TelemetrySession session,
            IThreadingContext threadingContext,
            IAsynchronousOperationListener asyncListener)
        {
            _session = session;
            _meter = _meterProvider.CreateMeter("Microsoft.VisualStudio.Roslyn");
            _event = new TelemetryEvent("Microsoft.VisualStudio.Roslyn");

            _postDataQueue = new AsyncBatchingWorkQueue<TimeBasedHistogram>(
                TimeSpan.FromMinutes(10),
                PostHistogramsAsync,
                EqualityComparer<TimeBasedHistogram>.Default,
                asyncListener,
                threadingContext.DisposalToken);
        }

        public static async Task<TimeBasedHistogramFactory> CreateAsync(
            IThreadingContext threadingContext,
            IAsynchronousOperationListener asyncListener)
        {
            // Create services that are bound to the UI thread
            await threadingContext.JoinableTaskFactory.SwitchToMainThreadAsync(threadingContext.DisposalToken);

            // Fetch the session synchronously on the UI thread; if this doesn't happen before we try using this on
            // the background thread then we will experience hangs like we see in this bug:
            // https://devdiv.visualstudio.com/DefaultCollection/DevDiv/_workitems?_a=edit&id=190808 or
            // https://devdiv.visualstudio.com/DevDiv/_workitems?id=296981&_a=edit
            var session = TelemetryService.DefaultSession;
            return new TimeBasedHistogramFactory(session, threadingContext, asyncListener);
        }

        public ITimeBasedHistogram GetHistogram(string name, string description)
        {
            var histogram = _histogramMap.GetOrAdd((name, description), static (tuple, @this) =>
            {
                var (name, description) = tuple;

                // histogram can live longer than the meter used to create it.
                var underlyingHistogram = @this._meter.CreateHistogram<double>(
                    name, s_histogramConfiguration, unit: "ms", description);
                return new TimeBasedHistogram(underlyingHistogram, @this._postDataQueue);
            }, this);

            return histogram;
        }

        private ValueTask PostHistogramsAsync(ImmutableSegmentedList<TimeBasedHistogram> list, CancellationToken cancellationToken)
        {
            foreach (var histogram in list)
            {
                cancellationToken.ThrowIfCancellationRequested();
                _session.PostMetricEvent(new TelemetryHistogramEvent<double>(_event, histogram.UnderlyingHistogram));
            }

            return ValueTaskFactory.CompletedTask;
        }

        private sealed class TimeBasedHistogram : ITimeBasedHistogram
        {
            public readonly IHistogram<double> UnderlyingHistogram;
            private readonly AsyncBatchingWorkQueue<TimeBasedHistogram> _workQueue;

            public TimeBasedHistogram(
                IHistogram<double> underlyingHistogram,
                AsyncBatchingWorkQueue<TimeBasedHistogram> workQueue)
            {
                UnderlyingHistogram = underlyingHistogram;
                _workQueue = workQueue;
            }

            public void Record(TimeSpan value)
                => RecordAndAddWork(value, static (histogram, value) => histogram.Record(value.TotalMilliseconds));

            public void Record(TimeSpan value, KeyValuePair<string, object?> tag)
                => RecordAndAddWork((value, tag), static (histogram, tuple) => histogram.Record(tuple.value.TotalMilliseconds, tuple.tag));

            public void Record(TimeSpan value, KeyValuePair<string, object?> tag1, KeyValuePair<string, object?> tag2)
                => RecordAndAddWork((value, tag1, tag2), static (histogram, tuple) => histogram.Record(tuple.value.TotalMilliseconds, tuple.tag1, tuple.tag2));

            public void Record(TimeSpan value, KeyValuePair<string, object?> tag1, KeyValuePair<string, object?> tag2, KeyValuePair<string, object?> tag3)
                => RecordAndAddWork((value, tag1, tag2, tag3), static (histogram, tuple) => histogram.Record(tuple.value.TotalMilliseconds, tuple.tag1, tuple.tag2, tuple.tag3));

            public void Record(TimeSpan value, params KeyValuePair<string, object>[] tags)
                => RecordAndAddWork((value, tags), static (histogram, tuple) => histogram.Record(tuple.value.TotalMilliseconds, tuple.tags));

            public void Record(TimeSpan value, ReadOnlySpan<KeyValuePair<string, object?>> tags)
            {
                lock (this)
                {
                    UnderlyingHistogram.Record(value.TotalMilliseconds, tags);
                    _workQueue.AddWork(this);
                }
            }

            private void RecordAndAddWork<TData>(TData data, Action<IHistogram<double>, TData> action)
            {
                lock (this)
                {
                    action(UnderlyingHistogram, data);
                    _workQueue.AddWork(this);
                }
            }
        }
    }
}

namespace Microsoft.CodeAnalysis.Editor.Implementation.Suggestions
{
    internal partial class SuggestedActionsSourceProvider
    {
        private partial class AsyncSuggestedActionsSource : SuggestedActionsSource, IAsyncSuggestedActionsSource
        {
            private readonly IAsynchronousOperationListener _listener;

            public AsyncSuggestedActionsSource(
                IThreadingContext threadingContext,
                IGlobalOptionService globalOptions,
                SuggestedActionsSourceProvider owner,
                ITextView textView,
                ITextBuffer textBuffer,
                ISuggestedActionCategoryRegistryService suggestedActionCategoryRegistry,
                IAsynchronousOperationListener listener)
                : base(threadingContext, globalOptions, owner, textView, textBuffer, suggestedActionCategoryRegistry)
            {
                _listener = listener;
            }

            public async Task GetSuggestedActionsAsync(
                ISuggestedActionCategorySet requestedActionCategories,
                SnapshotSpan range,
                ImmutableArray<ISuggestedActionSetCollector> collectors,
                CancellationToken cancellationToken)
            {
                using (var meter = _meterProvider.CreateMeter("Microsoft.VisualStudio.Roslyn"))
                {
                    AssertIsForeground();
                    using var _ = ArrayBuilder<ISuggestedActionSetCollector>.GetInstance(out var completedCollectors);
                    try
                    {
                        await GetSuggestedActionsWorkerAsync(
                            requestedActionCategories, range, collectors, completedCollectors, cancellationToken).ConfigureAwait(false);
                    }
                    finally
                    {
                        // Always ensure that all the collectors are marked as complete so we don't hang the UI.
                        foreach (var collector in collectors)
                        {
                            if (!completedCollectors.Contains(collector))
                                collector.Complete();
                        }
                    }
                }
            }

            private async Task GetSuggestedActionsWorkerAsync(
                ISuggestedActionCategorySet requestedActionCategories,
                SnapshotSpan range,
                ImmutableArray<ISuggestedActionSetCollector> collectors,
                ArrayBuilder<ISuggestedActionSetCollector> completedCollectors,
                CancellationToken cancellationToken)
            {
                AssertIsForeground();
                using var state = SourceState.TryAddReference();
                if (state is null)
                    return;

                var workspace = state.Target.Workspace;
                if (workspace is null)
                    return;

                var selection = TryGetCodeRefactoringSelection(state, range);
                await workspace.Services.GetRequiredService<IWorkspaceStatusService>().WaitUntilFullyLoadedAsync(cancellationToken).ConfigureAwait(false);

                using (Logger.LogBlock(FunctionId.SuggestedActions_GetSuggestedActionsAsync, cancellationToken))
                {
                    var document = range.Snapshot.GetOpenTextDocumentInCurrentContextWithChanges();
                    if (document is null)
                        return;

                    // Create a single keep-alive session as we process each lightbulb priority group.  We want to
                    // ensure that all calls to OOP will reuse the same solution-snapshot on the oop side (including
                    // reusing all the same computed compilations that may have been computed on that side.  This is
                    // especially important as we are sending disparate requests for diagnostics, and we do not want the
                    // individual diagnostic requests to redo all the work to run source generators, create skeletons,
                    // etc.
                    using var _1 = RemoteKeepAliveSession.Create(document.Project.Solution, _listener);

                    // Keep track of how many actions we've put in the lightbulb at each priority level.  We do
                    // this as each priority level will both sort and inline actions.  However, we don't want to
                    // inline actions at each priority if it's going to make the total number of actions too high.
                    // This does mean we might inline actions from a higher priority group, and then disable 
                    // inlining for lower pri groups.  However, intuitively, that is what we want.  More important
                    // items should be pushed higher up, and less important items shouldn't take up that much space.
                    var currentActionCount = 0;

                    using var _2 = ArrayBuilder<SuggestedActionSet>.GetInstance(out var lowPrioritySets);

                    // Collectors are in priority order.  So just walk them from highest to lowest.
                    foreach (var collector in collectors)
                    {
                        var priority = TryGetPriority(collector.Priority);

                        if (priority != null)
                        {
                            var allSets = GetCodeFixesAndRefactoringsAsync(
                                state, requestedActionCategories, document,
                                range, selection,
                                addOperationScope: _ => null,
                                priority.Value,
                                currentActionCount, cancellationToken).WithCancellation(cancellationToken).ConfigureAwait(false);

                            await foreach (var set in allSets)
                            {
                                if (priority == CodeActionRequestPriority.High && set.Priority == SuggestedActionSetPriority.Low)
                                {
                                    // if we're processing the high pri bucket, but we get action sets for lower pri
                                    // groups, then keep track of them and add them in later when we get to that group.
                                    lowPrioritySets.Add(set);
                                }
                                else
                                {
                                    currentActionCount += set.Actions.Count();
                                    collector.Add(set);
                                }
                            }

                            if (priority == CodeActionRequestPriority.Normal)
                            {
                                // now, add any low pri items we've been waiting on to the final group.
                                foreach (var set in lowPrioritySets)
                                {
                                    currentActionCount += set.Actions.Count();
                                    collector.Add(set);
                                }
                            }
                        }

                        // Ensure we always complete the collector even if we didn't add any items to it.
                        // This ensures that we unblock the UI from displaying all the results for that
                        // priority class.
                        collector.Complete();
                        completedCollectors.Add(collector);
                    }
                }
            }

            private async IAsyncEnumerable<SuggestedActionSet> GetCodeFixesAndRefactoringsAsync(
                ReferenceCountedDisposable<State> state,
                ISuggestedActionCategorySet requestedActionCategories,
                TextDocument document,
                SnapshotSpan range,
                TextSpan? selection,
                Func<string, IDisposable?> addOperationScope,
                CodeActionRequestPriority priority,
                int currentActionCount,
                [EnumeratorCancellation] CancellationToken cancellationToken)
            {
                var workspace = document.Project.Solution.Workspace;
                var supportsFeatureService = workspace.Services.GetRequiredService<ITextBufferSupportsFeatureService>();

                var options = GlobalOptions.GetCodeActionOptionsProvider();

                var fixesTask = GetCodeFixesAsync(
                    state, supportsFeatureService, requestedActionCategories, workspace, document, range,
                    addOperationScope, priority, options, isBlocking: false, cancellationToken);
                var refactoringsTask = GetRefactoringsAsync(
                    state, supportsFeatureService, requestedActionCategories, GlobalOptions, workspace, document, selection,
                    addOperationScope, priority, options, isBlocking: false, cancellationToken);

                await Task.WhenAll(fixesTask, refactoringsTask).ConfigureAwait(false);

                var fixes = await fixesTask.ConfigureAwait(false);
                var refactorings = await refactoringsTask.ConfigureAwait(false);
                foreach (var set in ConvertToSuggestedActionSets(state, selection, fixes, refactorings, currentActionCount))
                    yield return set;
            }
        }
    }
}
