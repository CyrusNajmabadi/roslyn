// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Host;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Serialization;

internal interface ISerializerService : IWorkspaceService
{
    ValueTask SerializeAsync(object value, ObjectWriter writer, CancellationToken cancellationToken);
    object Deserialize(WellKnownSynchronizationKind kind, ObjectReader reader, CancellationToken cancellationToken);

    ValueTask<Checksum> CreateChecksumAsync(object value, CancellationToken cancellationToken);
    Checksum CreateParseOptionsChecksum(ParseOptions value);
}
