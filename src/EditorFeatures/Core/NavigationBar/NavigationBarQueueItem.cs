// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Threading;

namespace Microsoft.CodeAnalysis.Editor.Implementation.NavigationBar;

/// <param name="FrozenSemantics">Indicates if we should compute with frozen semantics or not.</param>
/// <param name="NonFrozenComputationToken">If <paramref name="FrozenSemantics"/> is false, then this is a cancellation
/// token that can cancel the expensive work being done if new frozen work is requested.</param>
internal readonly record struct NavigationBarQueueItem(
    bool FrozenSemantics,
    CancellationToken? NonFrozenComputationToken);
