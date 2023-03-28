// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Collections;
using Microsoft.CodeAnalysis.Editor.Shared.Utilities;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.VisualStudio.Telemetry;
using Microsoft.VisualStudio.Telemetry.Metrics;
using Microsoft.VisualStudio.Telemetry.Metrics.Events;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Telemetry;

internal sealed partial class TimeBasedHistogramFactory : ITimeBasedHistogramFactory
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

    private readonly ConcurrentDictionary<(string name, string description), ITimeBasedHistogram> _histogramMap = new();

    private readonly AsyncBatchingWorkQueue<TimeBasedHistogram> _postDataQueue;

    private TimeBasedHistogramFactory(
        TelemetrySession session,
        IThreadingContext threadingContext,
        IAsynchronousOperationListener asyncListener)
    {
        _session = session;
        _meter = _meterProvider.CreateMeter("Microsoft.VisualStudio.Roslyn");
        _event = new TelemetryEvent("Microsoft.VisualStudio.Roslyn");

        // A cadence of 10 minutes means we'll only report a few dozen events a day, while still aggregating a fair
        // amount of information into a single report.  It also means that if we don't end up reporting an item, at most
        // we lose is the last 10 minutes of information.
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
        => _histogramMap.GetOrAdd((name, description), static (tuple, @this) =>
        {
            var (name, description) = tuple;

            // histogram can live longer than the meter used to create it.
            var underlyingHistogram = @this._meter.CreateHistogram<double>(
                name, s_histogramConfiguration, unit: "ms", description);
            return new TimeBasedHistogram(underlyingHistogram, @this._postDataQueue);
        }, this);

    private ValueTask PostHistogramsAsync(ImmutableSegmentedList<TimeBasedHistogram> list, CancellationToken cancellationToken)
    {
        foreach (var histogram in list)
        {
            cancellationToken.ThrowIfCancellationRequested();
            _session.PostMetricEvent(new TelemetryHistogramEvent<double>(_event, histogram.UnderlyingHistogram));
        }

        return ValueTaskFactory.CompletedTask;
    }
}
