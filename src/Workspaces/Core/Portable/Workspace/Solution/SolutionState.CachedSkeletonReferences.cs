// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.Storage;
using Roslyn.Utilities;
using Microsoft.CodeAnalysis.ErrorReporting;
using System.IO;

namespace Microsoft.CodeAnalysis;

internal partial class SolutionState
{
    /// <summary>
    /// Caches the skeleton references produced for a given project/compilation under the varying
    /// <see cref="MetadataReferenceProperties"/> it might be referenced by.  Skeletons are used in the compilation
    /// tracker to allow cross-language project references with live semantic updating between VB/C# and vice versa.
    /// Specifically, in a cross language case we will build a skeleton ref for the referenced project and have the
    /// referrer use that to understand its semantics.
    /// <para/>
    /// This approach works, but has the caveat that live cross-language semantics are only possible when the 
    /// skeleton assembly can be built.  This should always be the case for correct code, but it may not be the
    /// case for code with errors depending on if the respective language compiler is resilient to those errors or not.
    /// In that case though where the skeleton cannot be built, this type provides mechanisms to fallback to the last
    /// successfully built skeleton so that a somewhat reasonable experience can be maintained.  If we failed to do this
    /// and instead returned nothing, a user would find that practically all semantic experiences that depended on
    /// that particular project would fail or be seriously degraded (e.g. diagnostics).  To that end, it's better to
    /// limp along with stale date, then barrel on ahead with no data.
    /// <para/>
    /// The implementation works by keeping metadata references around associated with a specific <see cref="VersionStamp"/>
    /// for a project. As long as the <see cref="Project.GetDependentSemanticVersionAsync"/> for that project
    /// is the same, then all the references of it can be reused.  When an <see cref="SolutionState.ICompilationTracker"/> forks
    /// itself, it  will also <see cref="Clone"/> this, allowing previously computed references to be used by later forks.
    /// However, this means that later forks (esp. ones that fail to produce a skeleton, or which produce a skeleton for 
    /// different semantics) will not leak backward to a prior <see cref="ProjectState"/>, causing it to see a view of the world
    /// inapplicable to its current snapshot.
    /// </summary>
    private class CachedSkeletonReferences
    {
        /// <summary>
        /// Mapping from compilation instance to metadata-references for it.  This allows us to associate the same
        /// <see cref="SkeletonReferenceSet"/> to different compilations that may not be the same as the original
        /// compilation we generated the set from.  This allows us to use compilations as keys as long as they're
        /// alive, but also associate the set with new compilations that are generated in the future if the older
        /// compilations were thrown away.
        /// </summary>
        private static readonly ConditionalWeakTable<Compilation, SkeletonReferenceSet> s_compilationToReferenceMap = new();

        private const string s_metadataPersistenceStreamKey = "<MetadataOnlyImageStream>";
        private const string s_metadataPersistenceAssemblyInfoKey = "<MetadataOnlyImageAssemblyNameAndIsEmpty>";
        private const string s_metadataPersistenceVersion = "1";
        private static readonly EmitOptions s_metadataOnlyEmitOptions = new(metadataOnly: true);

        private readonly object _gate = new();

        /// <summary>
        /// The <see cref="Project.GetDependentSemanticVersionAsync"/> version of the project that the
        /// <see cref="_skeletonReferenceSet"/> corresponds to.
        /// </summary>
        private VersionStamp? _version;

        /// <summary>
        /// Mapping from metadata-reference-properties to the actual metadata reference for them.
        /// </summary>
        private SkeletonReferenceSet? _skeletonReferenceSet;

        public CachedSkeletonReferences()
            : this(version: null, skeletonReferenceSet: null)
        {
        }

        private CachedSkeletonReferences(
            VersionStamp? version,
            SkeletonReferenceSet? skeletonReferenceSet)
        {
            _version = version;
            _skeletonReferenceSet = skeletonReferenceSet;
        }

        /// <summary>
        /// Produces a copy of the <see cref="CachedSkeletonReferences"/>, allowing forks of <see cref="ProjectState"/> to
        /// reuse <see cref="MetadataReference"/>s when their dependent semantic version matches ours.  In the case where
        /// the version is different, then the clone will attempt to make a new skeleton reference for that version.  If it
        /// succeeds, it will use that.  If it fails however, it can still use our skeletons.
        /// </summary>
        public CachedSkeletonReferences Clone()
        {
            lock (_gate)
            {
                // pass along the best version/reference-set we computed for ourselves.  That way future ProjectStates
                // can use this data if either the version changed, or they weren't able to build a skeleton for themselves.
                // By passing along a copy we ensure that if they have a different version, they'll end up producing a new
                // SkeletonReferenceSet where they'll store their own data in which will not affect prior ProjectStates.
                return new CachedSkeletonReferences(_version, _skeletonReferenceSet);
            }
        }

        /// <summary>
        /// Return a metadata reference if we already have a reference-set computed for this particular <paramref name="version"/>.
        /// If a reference already exists for the provided <paramref name="properties"/>, the same instance will be returned.  Otherwise,
        /// a fresh instance will be returned.
        /// </summary>
        public MetadataReference? TryGetReference(
            ICompilationTracker compilationTracker,
            SolutionState solutionState,
            VersionStamp version,
            MetadataReferenceProperties properties)
        {
            SkeletonReferenceSet? skeletonReferenceSet = null;
            lock (_gate)
            {
                if (version == _version)
                    skeletonReferenceSet = _skeletonReferenceSet;
            }

            TryGetReferenceSet(version)?.GetMetadataReference(properties);
        }

        public async Task<MetadataReference> GetOrBuildReferenceAsync(
            ICompilationTracker compilationTracker,
            SolutionState solutionState,
            MetadataReferenceProperties properties,
            Compilation finalCompilation,
            VersionStamp version,
            CancellationToken cancellationToken)
        {
            // first, check if we have a direct mapping from this compilation to a reference set. If so, use it.  This
            // ensures the same compilations will get same metadata reference.
            if (s_compilationToReferenceMap.TryGetValue(finalCompilation, out var referenceSet))
                return referenceSet.GetMetadataReference(properties);

            // Didn't have a direct mapping to a reference set.  Compute one for ourselves.
            referenceSet = await GetOrBuildReferenceSetAsync(
                compilationTracker, solutionState, version, finalCompilation, cancellationToken).ConfigureAwait(false);

            // another thread may have come in and beaten us to computing this.  So attempt to actually cache this
            // in the global map.  if it succeeds, use our computed version.  If it fails, use the one the other
            // thread succeeded in storing.
            referenceSet = s_compilationToReferenceMap.GetValue(finalCompilation, _ => referenceSet);

            lock (_gate)
            {
                // whoever won, still store this reference set against us with the provided version.
                _version = version;
                _skeletonReferenceSet = referenceSet;
            }

            return referenceSet.GetMetadataReference(properties);
        }

        private async Task<SkeletonReferenceSet> GetOrBuildReferenceSetAsync(
            ICompilationTracker compilationTracker,
            SolutionState solutionState,
            VersionStamp version,
            Compilation finalCompilation,
            CancellationToken cancellationToken)
        {
            var workspace = compilationTracker.ProjectState.LanguageServices.WorkspaceServices.Workspace;

            // First see if we already have a reference set for this version.  if so, we're done and can return that.
            var referenceSet = TryGetReferenceSet(version);
            if (referenceSet != null)
            {
                workspace.LogTestMessage($"Succeeded at finding reference set corresponding to requested version.");
                return referenceSet;
            }

            // okay, we don't have one. so create one now.

            var image = await GetOrCreateImageAsync(
                compilationTracker, solutionState, finalCompilation, cancellationToken).ConfigureAwait(false);

            if (image.IsEmpty)
            {
                // unfortunately, we couldn't create one. see if we have one from previous compilation., it might be
                // out-of-date big time, but better than nothing.
                referenceSet = TryGetReferenceSet(version: null);
                if (referenceSet != null)
                {
                    workspace.LogTestMessage($"We failed to create metadata so we're using the one we just found from an earlier version.");
                    return referenceSet;
                }
            }

            return new SkeletonReferenceSet(image, new DeferredDocumentationProvider(finalCompilation));
        }

        private static async Task<MetadataOnlyImage> GetOrCreateImageAsync(
            ICompilationTracker compilationTracker,
            SolutionState solutionState,
            Compilation finalCompilation,
            CancellationToken cancellationToken)
        {
            var services = compilationTracker.ProjectState.LanguageServices.WorkspaceServices;
            var workspace = services.Workspace;
            var database = workspace.Options.GetOption(StorageOptions.Database);

            // First, compute the checksum for the project we're creating a metadata reference for.  We'll attempt to
            // persist this so we can avoid expensive regeneration in future host sessions if possible.
            var dependentChecksum = await compilationTracker.GetDependentChecksumAsync(solutionState, cancellationToken).ConfigureAwait(false);
            var totalChecksum = Checksum.Create(Checksum.Create(s_metadataPersistenceVersion), dependentChecksum);

            var temporaryStorageService = workspace.Services.GetRequiredService<ITemporaryStorageService>();
            var persistentStorageService = services.GetPersistentStorageService(database);

            var persistentStorage = await persistentStorageService.GetStorageAsync(SolutionKey.ToSolutionKey(solutionState), cancellationToken).ConfigureAwait(false);
            await using var _ = persistentStorage.ConfigureAwait(false);

            var projectKey = ProjectKey.ToProjectKey(solutionState, compilationTracker.ProjectState);
            var image = await TryReadImageAsync(
                projectKey, temporaryStorageService, persistentStorage, totalChecksum, cancellationToken).ConfigureAwait(false);
            if (image != null)
                return image;

            // Weren't able to successfully read this from the persistence service.  Compute the image from the compilation
            // and store there for future sessions.
            using var imageStream = SerializableBytes.CreateWritableStream();
            image = await GetOrCreateImageAsync(
                compilationTracker, solutionState, finalCompilation, cancellationToken).ConfigureAwait(false);
            Contract.ThrowIfNull(image);

            await WriteImageAsync(
                projectKey, persistentStorage, totalChecksum, imageStream, image, cancellationToken).ConfigureAwait(false);

            return image;
        }

        private static async Task<MetadataOnlyImage?> TryReadImageAsync(
            ProjectKey projectKey,
            ITemporaryStorageService temporaryStorageService,
            IChecksummedPersistentStorage persistentStorage,
            Checksum totalChecksum,
            CancellationToken cancellationToken)
        {
            try
            {
                using var assemblyInfoStream = await persistentStorage.ReadStreamAsync(
                    projectKey,
                    s_metadataPersistenceAssemblyInfoKey,
                    totalChecksum,
                    cancellationToken).ConfigureAwait(false);

                if (assemblyInfoStream != null)
                {
                    using var assemblyInfoReader = ObjectReader.TryGetReader(assemblyInfoStream, leaveOpen: false, cancellationToken);
                    if (assemblyInfoReader != null)
                    {
                        var assemblyName = assemblyInfoReader.ReadString();
                        var isEmpty = assemblyInfoReader.ReadBoolean();

                        if (isEmpty)
                            return MetadataOnlyImage.Empty;

                        using var imageStream = await persistentStorage.ReadStreamAsync(
                            projectKey,
                            s_metadataPersistenceStreamKey,
                            totalChecksum,
                            cancellationToken).ConfigureAwait(false);
                        if (imageStream != null)
                        {
                            var temporaryStorage = temporaryStorageService.CreateTemporaryStreamStorage(cancellationToken);
                            imageStream.Position = 0;
                            temporaryStorage.WriteStream(imageStream, cancellationToken);

                            return new MetadataOnlyImage(temporaryStorage, assemblyName);
                        }
                    }
                }
            }
            catch (Exception ex) when (FatalError.ReportAndCatchUnlessCanceled(ex, cancellationToken))
            {
            }

            return null;
        }

        private static async Task WriteImageAsync(
            ProjectKey projectKey,
            IChecksummedPersistentStorage persistentStorage,
            Checksum totalChecksum,
            Stream imageStream,
            MetadataOnlyImage image,
            CancellationToken cancellationToken)
        {
            try
            {
                using var infoStream = SerializableBytes.CreateWritableStream();
                using (var assemblyInfoWriter = new ObjectWriter(infoStream, leaveOpen: true, cancellationToken))
                {
                    assemblyInfoWriter.WriteString(image.AssemblyName);
                    assemblyInfoWriter.WriteBoolean(image.IsEmpty);
                }

                infoStream.Position = 0;
                await persistentStorage.WriteStreamAsync(
                    projectKey,
                    s_metadataPersistenceAssemblyInfoKey,
                    infoStream,
                    totalChecksum,
                    cancellationToken).ConfigureAwait(false);

                if (image.IsEmpty)
                    return;

                imageStream.Position = 0;
                await persistentStorage.WriteStreamAsync(
                    projectKey,
                    s_metadataPersistenceStreamKey,
                    imageStream,
                    totalChecksum,
                    cancellationToken).ConfigureAwait(false);
            }
            catch (Exception ex) when (FatalError.ReportAndCatchUnlessCanceled(ex, cancellationToken))
            {
            }
        }

        public static MetadataOnlyImage CreateMetadataOnlyImage(
            Workspace workspace,
            ITemporaryStorageService service,
            Compilation compilation,
            Stream stream,
            CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            try
            {
                workspace.LogTestMessage($"Beginning to create a skeleton assembly for {compilation.AssemblyName}...");

                using (Logger.LogBlock(FunctionId.Workspace_SkeletonAssembly_EmitMetadataOnlyImage, cancellationToken))
                {
                    // note: cloning compilation so we don't retain all the generated symbols after its emitted.
                    // * REVIEW * is cloning clone p2p reference compilation as well?
                    var emitResult = compilation.Clone().Emit(stream, options: s_metadataOnlyEmitOptions, cancellationToken: cancellationToken);

                    if (emitResult.Success)
                    {
                        workspace.LogTestMessage($"Successfully emitted a skeleton assembly for {compilation.AssemblyName}");
                        var storage = service.CreateTemporaryStreamStorage(cancellationToken);

                        stream.Position = 0;
                        storage.WriteStream(stream, cancellationToken);

                        return new MetadataOnlyImage(storage, compilation.AssemblyName);
                    }
                    else
                    {
                        workspace.LogTestMessage($"Failed to create a skeleton assembly for {compilation.AssemblyName}:");

                        foreach (var diagnostic in emitResult.Diagnostics)
                        {
                            workspace.LogTestMessage("  " + diagnostic.GetMessage());
                        }

                        // log emit failures so that we can improve most common cases
                        Logger.Log(FunctionId.MetadataOnlyImage_EmitFailure, KeyValueLogMessage.Create(m =>
                        {
                            // log errors in the format of
                            // CS0001:1;CS002:10;...
                            var groups = emitResult.Diagnostics.GroupBy(d => d.Id).Select(g => $"{g.Key}:{g.Count()}");
                            m["Errors"] = string.Join(";", groups);
                        }));

                        return MetadataOnlyImage.Empty;
                    }
                }
            }
            finally
            {
                workspace.LogTestMessage($"Done trying to create a skeleton assembly for {compilation.AssemblyName}");
            }
        }

        /// <summary>
        /// Tries to get the <see cref="SkeletonReferenceSet"/> for this project matching <paramref name="version"/>.
        /// if <paramref name="version"/> is <see langword="null"/>, any cached <see cref="SkeletonReferenceSet"/> 
        /// can be returned, even if it doesn't correspond to that version.  This is useful in error tolerance cases
        /// as building a skeleton assembly may easily fail. In that case it's better to use the last successfully 
        /// built skeleton than just have no semantic information for that project at all.
        /// </summary>
        private SkeletonReferenceSet? TryGetReferenceSet(VersionStamp? version)
        {
            // Otherwise, we don't have a direct mapping stored.  Try to see if the cached reference we have is
            // applicable to this project semantic version.
            lock (_gate)
            {
                // if the caller is requiring a particular semantic version, it much match what we have cached.
                if (version == null || version == _version)
                    return _skeletonReferenceSet;
            }

            return null;
        }

        private sealed class SkeletonReferenceSet
        {
            private readonly object _gate = new();

            // use WeakReference so we don't keep MetadataReference's alive if they are not being consumed
            private readonly Dictionary<MetadataReferenceProperties, WeakReference<MetadataReference>> _metadataReferences = new();

            private readonly MetadataOnlyImage _image;

            /// <summary>
            /// The documentation provider used to lookup xml docs for any metadata reference we pass out.  See
            /// docs on <see cref="DeferredDocumentationProvider"/> for why this is safe to hold onto despite it
            /// rooting a compilation internally.
            /// </summary>
            private readonly DeferredDocumentationProvider _documentationProvider;

            public SkeletonReferenceSet(
                MetadataOnlyImage image,
                DeferredDocumentationProvider documentationProvider)
            {
                _image = image;
                _documentationProvider = documentationProvider;
            }

            public MetadataReference GetMetadataReference(MetadataReferenceProperties properties)
            {
                // lookup first and eagerly return cached value if we have it.
                lock (_gate)
                {
                    if (TryGetExisting(out var metadataReference))
                        return metadataReference;
                }

                // otherwise, create the metadata outside of the lock, and then try to assign it if no one else beat us
                {
                    var metadataReference = _image.CreateReference(properties.Aliases, properties.EmbedInteropTypes, _documentationProvider);
                    var weakMetadata = new WeakReference<MetadataReference>(metadataReference);

                    lock (_gate)
                    {
                        // see if someone beat us to writing this.
                        if (TryGetExisting(out var existingMetadataReference))
                            return existingMetadataReference;

                        _metadataReferences[properties] = weakMetadata;
                    }

                    return metadataReference;
                }

                bool TryGetExisting([NotNullWhen(true)] out MetadataReference? metadataReference)
                {
                    metadataReference = null;
                    return _metadataReferences.TryGetValue(properties, out var weakMetadata) &&
                        weakMetadata.TryGetTarget(out metadataReference);
                }
            }
        }
    }
}
