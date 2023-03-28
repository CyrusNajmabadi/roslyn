// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

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
    ITimeBasedHistogram GetHistogram(string name, string description);
}
