// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis.Host;

namespace Microsoft.CodeAnalysis
{
    internal class MetadataOnlyImage
    {
        /// <summary>
        /// A map to ensure that the streams from the temporary storage service that back the metadata we create stay alive as long
        /// as the metadata is alive.
        /// </summary>
        private static readonly ConditionalWeakTable<AssemblyMetadata, ISupportDirectMemoryAccess> s_lifetime = new();

        public static readonly MetadataOnlyImage Empty = new(storage: null, assemblyName: string.Empty);

        private readonly ITemporaryStreamStorage _storage;
        public readonly string AssemblyName;

        public MetadataOnlyImage(ITemporaryStreamStorage storage, string assemblyName)
        {
            _storage = storage;
            AssemblyName = assemblyName;
        }

        public bool IsEmpty => _storage == null;

        public MetadataReference CreateReference(ImmutableArray<string> aliases, bool embedInteropTypes, DocumentationProvider documentationProvider)
        {
            if (this.IsEmpty)
            {
                return null;
            }

            // first see whether we can use native memory directly.
            var stream = _storage.ReadStream();
            AssemblyMetadata metadata;

            if (stream is ISupportDirectMemoryAccess supportNativeMemory)
            {
                // this is unfortunate that if we give stream, compiler will just re-copy whole content to 
                // native memory again. this is a way to get around the issue by we getting native memory ourselves and then
                // give them pointer to the native memory. also we need to handle lifetime ourselves.
                metadata = AssemblyMetadata.Create(ModuleMetadata.CreateFromImage(supportNativeMemory.GetPointer(), (int)stream.Length));

                // Tie lifetime of stream to metadata we created. It is important to tie this to the Metadata and not the
                // metadata reference, as PE symbols hold onto just the Metadata. We can use Add here since we created
                // a brand new object in AssemblyMetadata.Create above.
                s_lifetime.Add(metadata, supportNativeMemory);
            }
            else
            {
                // Otherwise, we just let it use stream. Unfortunately, if we give stream, compiler will
                // internally copy it to native memory again. since compiler owns lifetime of stream,
                // it would be great if compiler can be little bit smarter on how it deals with stream.

                // We don't deterministically release the resulting metadata since we don't know 
                // when we should. So we leave it up to the GC to collect it and release all the associated resources.
                metadata = AssemblyMetadata.CreateFromStream(stream);
            }

            return metadata.GetReference(
                documentation: documentationProvider,
                aliases: aliases,
                embedInteropTypes: embedInteropTypes,
                display: AssemblyName);
        }

        public void Cleanup()
        {
            if (_storage != null)
            {
                _storage.Dispose();
            }
        }
    }
}
