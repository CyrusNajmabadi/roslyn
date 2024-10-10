// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Diagnostics.CodeAnalysis;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.ErrorReporting;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.Serialization;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis;

internal sealed partial class ProjectState
{
    public bool TryGetStateChecksums([NotNullWhen(true)] out ProjectStateChecksums? stateChecksums)
        => _lazyChecksums.TryGetValue(out stateChecksums);

    public Task<ProjectStateChecksums> GetStateChecksumsAsync(CancellationToken cancellationToken)
        => _lazyChecksums.GetValueAsync(cancellationToken);

    public Task<Checksum> GetChecksumAsync(CancellationToken cancellationToken)
    {
        return SpecializedTasks.TransformWithoutIntermediateCancellationExceptionAsync(
            static (lazyChecksums, cancellationToken) => new ValueTask<ProjectStateChecksums>(lazyChecksums.GetValueAsync(cancellationToken)),
            static (projectStateChecksums, _) => projectStateChecksums.Checksum,
            _lazyChecksums,
            cancellationToken).AsTask();
    }

    public Checksum GetParseOptionsChecksum()
        => GetParseOptionsChecksum(LanguageServices.SolutionServices.GetRequiredService<ISerializerService>());

    private Checksum GetParseOptionsChecksum(ISerializerService serializer)
        => this.SupportsCompilation
            ? ChecksumCache.GetOrCreate(this.ParseOptions!, static (options, serializer) => serializer.CreateParseOptionsChecksum(options), serializer)
            : Checksum.Null;

    private async Task<ProjectStateChecksums> ComputeChecksumsAsync(CancellationToken cancellationToken)
    {
        try
        {
            using (Logger.LogBlock(FunctionId.ProjectState_ComputeChecksumsAsync, FilePath, cancellationToken))
            {
                var documentChecksumsTask = DocumentStates.GetDocumentChecksumsAndIdsAsync(cancellationToken);
                var additionalDocumentChecksumsTask = AdditionalDocumentStates.GetDocumentChecksumsAndIdsAsync(cancellationToken);
                var analyzerConfigDocumentChecksumsTask = AnalyzerConfigDocumentStates.GetDocumentChecksumsAndIdsAsync(cancellationToken);

                var serializer = LanguageServices.SolutionServices.GetRequiredService<ISerializerService>();

                var infoChecksum = this.ProjectInfo.Attributes.Checksum;

                // these compiler objects doesn't have good place to cache checksum. but rarely ever get changed.
                var compilationOptionsChecksum = !SupportsCompilation
                    ? Checksum.Null
                    : await ChecksumCache.GetOrCreateAsync(
                        CompilationOptions!,
                        async static (options, tuple) =>
                            await tuple.serializer.CreateChecksumAsync(options, tuple.cancellationToken).ConfigureAwait(false),
                        (serializer, cancellationToken)).ConfigureAwait(false);
                cancellationToken.ThrowIfCancellationRequested();
                var parseOptionsChecksum = GetParseOptionsChecksum(serializer);

                var projectReferenceChecksums = await ChecksumCache.GetOrCreateChecksumCollectionAsync(
                    ProjectReferences, serializer, cancellationToken).ConfigureAwait(false);
                var metadataReferenceChecksums = await ChecksumCache.GetOrCreateChecksumCollectionAsync(
                    MetadataReferences, serializer, cancellationToken).ConfigureAwait(false);
                var analyzerReferenceChecksums = await ChecksumCache.GetOrCreateChecksumCollectionAsync(
                    AnalyzerReferences, serializer, cancellationToken).ConfigureAwait(false);

                return new ProjectStateChecksums(
                    this.Id,
                    infoChecksum,
                    compilationOptionsChecksum,
                    parseOptionsChecksum,
                    projectReferenceChecksums,
                    metadataReferenceChecksums,
                    analyzerReferenceChecksums,
                    documentChecksums: await documentChecksumsTask.ConfigureAwait(false),
                    await additionalDocumentChecksumsTask.ConfigureAwait(false),
                    await analyzerConfigDocumentChecksumsTask.ConfigureAwait(false));
            }
        }
        catch (Exception e) when (FatalError.ReportAndPropagateUnlessCanceled(e, cancellationToken, ErrorSeverity.Critical))
        {
            throw ExceptionUtilities.Unreachable();
        }
    }
}
