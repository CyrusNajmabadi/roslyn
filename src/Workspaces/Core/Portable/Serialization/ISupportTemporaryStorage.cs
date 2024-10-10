// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Host;

namespace Microsoft.CodeAnalysis.Serialization;

/// <summary>
/// Interface for services that support dumping their contents to memory-mapped-files (generally speaking, our assembly
/// reference objects).  This allows those objects to expose the memory-mapped-file info needed to read that data back
/// in in any process.
/// </summary>
internal interface ISupportTemporaryStorage
{
    /// <summary>
    /// Give a chance for the system to load this object from disk in an async, cancellable fashion.  Used as certain
    /// compiler apis (like <see cref="PortableExecutableReference.GetMetadata()"/>) are synchronous and can cause
    /// undesired blocking.
    /// </summary>
    /// <param name="cancellationToken"></param>
    /// <returns></returns>
    ValueTask PreloadAsync(CancellationToken cancellationToken);

    ValueTask<IReadOnlyList<ITemporaryStorageStreamHandle>?> GetStorageHandlesAsync(CancellationToken cancellationToken);
}
