// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.Telemetry
{
    /// <summary>
    /// Threadsafe.  Used to record time-spans that operations have taken.  Good for granularities useful for measuring
    /// how long a user-facing feature took.  As such it records buckets from about 50ms up to 10m.
    /// <para/>
    /// Reporting of the histogram data is already handled automatically.  There is no need to do anything special to
    /// have that happen.  The histogram will collect enough data locally, then make reports (on a long cadence) so as
    /// to both collect enough information to get good breakdowns of timings, while also not reporting data too quickly.
    /// </summary>
    internal interface ITimeBasedHistogram
    {
        void Record(TimeSpan value);
        void Record(TimeSpan value, KeyValuePair<string, object?> tag);
        void Record(TimeSpan value, KeyValuePair<string, object?> tag1, KeyValuePair<string, object?> tag2);
        void Record(TimeSpan value, KeyValuePair<string, object?> tag1, KeyValuePair<string, object?> tag2, KeyValuePair<string, object?> tag3);
        void Record(TimeSpan value, params KeyValuePair<string, object>[] tags);
        void Record(TimeSpan value, ReadOnlySpan<KeyValuePair<string, object?>> tags);
    }
}
