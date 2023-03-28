// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Telemetry.Metrics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Telemetry;

internal sealed partial class TimeBasedHistogramFactory
{
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
            // can't defer to helper here as we can't pass a ref-struct to a lambda.
            lock (this)
            {
                UnderlyingHistogram.Record(value.TotalMilliseconds, tags);
            }

            _workQueue.AddWork(this);
        }

        private void RecordAndAddWork<TData>(TData data, Action<IHistogram<double>, TData> action)
        {
            // Underlying histogram object is not threadsafe, so make sure we only ever have one mutation happening at a time.
            lock (this)
            {
                action(UnderlyingHistogram, data);
            }

            // Ensure this histogram is queued up so we will report it the next time the queue processes items.
            _workQueue.AddWork(this);
        }
    }
}
