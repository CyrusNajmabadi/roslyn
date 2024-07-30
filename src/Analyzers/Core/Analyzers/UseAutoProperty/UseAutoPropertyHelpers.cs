// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;

namespace Microsoft.CodeAnalysis.UseAutoProperty;

internal static class UseAutoPropertyHelpers
{
    public const string SemiAutoProperty = nameof(SemiAutoProperty);

    public static readonly ImmutableDictionary<string, string?> SemiAutoProperties =
        ImmutableDictionary<string, string?>.Empty.Add(SemiAutoProperty, SemiAutoProperty);
}
