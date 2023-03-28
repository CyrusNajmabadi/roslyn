// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Threading;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Telemetry;

/// <summary>
/// Factory used to create <see cref="ITimeBasedHistogram"/>s.  These histograms should be used to measure a
/// user-visible scenario, breaking it up into reasonable time slices indicating where time is spent.  Note, minimum
/// time granularity is 50ms.  So this should not be used to gather extremely fine grained details as to where time
/// is being spent. Instead, this is useful for a rough wall-clock breakdown of the cost of a feature.
/// <para/>
/// Reporting of the histogram data is already handled automatically.  There is no need to do anything special to
/// have that happen.  The histogram will collect enough data locally, then make reports (on a long cadence) so as
/// to both collect enough information to get good breakdowns of timings, while also not reporting data too quickly.
/// </summary>
internal interface ITimeBasedHistogramFactory
{
    /// <summary>
    /// Gets a histogram with a given name and description. The same name/description will result in the same
    /// histogram instance being returned.
    /// </summary>
    ITimeBasedHistogram GetHistogram(string name);
}

internal static class ITimeBasedHistogramFactoryExtensions
{
    public static ScopedTimedBasedHistogram GetScopedHistogram(this ITimeBasedHistogramFactory factory, string name, CancellationToken cancellationToken)
        => GetScopedHistogram(factory, name, tag: null, cancellationToken);

    public static ScopedTimedBasedHistogram GetScopedHistogram(this ITimeBasedHistogramFactory factory, string name, KeyValuePair<string, object?>? tag, CancellationToken cancellationToken)
        => new(factory.GetHistogram(name), tag, cancellationToken);

    public readonly struct ScopedTimedBasedHistogram : IDisposable
    {
        private readonly ITimeBasedHistogram _histogram;
        private readonly KeyValuePair<string, object?>? _tag;
        private readonly CancellationToken _cancellationToken;
        private readonly SharedStopwatch _stopwatch = SharedStopwatch.StartNew();

        public ScopedTimedBasedHistogram(
            ITimeBasedHistogram histogram,
            KeyValuePair<string, object?>? tag,
            CancellationToken cancellationToken)
        {
            _histogram = histogram;
            _tag = tag;
            _cancellationToken = cancellationToken;
        }

        public void Dispose()
        {
            var cancelledKVP = KeyValuePairUtil.Create(nameof(_cancellationToken.IsCancellationRequested), (object?)_cancellationToken.IsCancellationRequested);

            if (_tag == null)
                _histogram.Record(_stopwatch.Elapsed, cancelledKVP);
            else
                _histogram.Record(_stopwatch.Elapsed, cancelledKVP, _tag.Value);
        }
    }
}
