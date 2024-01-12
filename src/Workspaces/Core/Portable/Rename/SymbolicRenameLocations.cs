// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Rename
{
    /// <summary>
    /// Holds the Locations of a symbol that should be renamed, along with the symbol and Solution for the set. It is
    /// considered 'heavy weight' because it holds onto large entities (like Symbols) and thus should not be marshaled
    /// to/from a host to OOP.
    /// </summary>
    internal sealed partial class SymbolicRenameLocations
    {
        public readonly Solution Solution;
        public readonly ISymbol Symbol;
        public readonly SymbolRenameOptions Options;

        public readonly ImmutableArray<RenameLocation> Locations;
        public readonly ImmutableArray<ReferenceLocation> ImplicitLocations;
        public readonly ImmutableArray<ISymbol> ReferencedSymbols;

        public SymbolicRenameLocations(
            ISymbol symbol,
            Solution solution,
            SymbolRenameOptions options,
            ImmutableArray<RenameLocation> locations,
            ImmutableArray<ReferenceLocation> implicitLocations,
            ImmutableArray<ISymbol> referencedSymbols)
        {
            Debug.Assert(locations.Distinct().Count() == locations.Length, "Locations should be unique");
            Contract.ThrowIfTrue(locations.IsDefault);
            Contract.ThrowIfTrue(implicitLocations.IsDefault);
            Contract.ThrowIfTrue(referencedSymbols.IsDefault);

            Solution = solution;
            Symbol = symbol;
            Options = options;
            Locations = locations;
            ReferencedSymbols = referencedSymbols;
            ImplicitLocations = implicitLocations;
        }

        /// <summary>
        /// Attempts to find all the locations to rename.  Will not cross any process boundaries to do this.
        /// </summary>
        public static async Task<SymbolicRenameLocations> FindLocationsInCurrentProcessAsync(
            ISymbol symbol, Solution solution, SymbolRenameOptions options, CancellationToken cancellationToken)
        {
            var locations = await FindLocationsInCurrentProcessWorkerAsync(symbol, solution, options, cancellationToken).ConfigureAwait(false);
            if (options.IgnoreSpans == default && options.IncludeSpans == default)
                return locations;

            return new SymbolicRenameLocations(
                locations.Symbol,
                locations.Solution,
                locations.Options,
                FilterLocations(locations.Locations, static loc => (loc.DocumentId, loc.Location)),
                FilterLocations(locations.ImplicitLocations, static loc => (loc.Document.Id, loc.Location)),
                locations.ReferencedSymbols);

            ImmutableArray<TLocation> FilterLocations<TLocation>(
                ImmutableArray<TLocation> locations,
                Func<TLocation, (DocumentId, Location)> getDocumentIdAndLocation)
            {
                using var _ = ArrayBuilder<TLocation>.GetInstance(locations.Length, out var result);

                foreach (var location in locations)
                {
                    var (documentId, docLocation) = getDocumentIdAndLocation(location);

                    if (options.IgnoreSpans != default && Intersects(documentId, docLocation, options.IgnoreSpans))
                        continue;

                    if (options.IgnoreSpans != default && !Intersects(documentId, docLocation, options.IncludeSpans))
                        continue;

                    result.Add(location);
                }

                return result.ToImmutableAndClear();
            }

            static bool Intersects(DocumentId documentId, Location location, ImmutableArray<DocumentSpan> spans)
            {
                foreach (var span in spans)
                {
                    if (documentId == span.Document.Id &&
                        location.SourceSpan.IntersectsWith(span.SourceSpan))
                    {
                        return true;
                    }
                }

                return false;
            }
        }

        private static async Task<SymbolicRenameLocations> FindLocationsInCurrentProcessWorkerAsync(
            ISymbol symbol, Solution solution, SymbolRenameOptions options, CancellationToken cancellationToken)
        {
            Contract.ThrowIfNull(symbol);
            using (Logger.LogBlock(FunctionId.Rename_AllRenameLocations, cancellationToken))
            {
                symbol = await RenameUtilities.FindDefinitionSymbolAsync(symbol, solution, cancellationToken).ConfigureAwait(false);

                // First, find the direct references just to the symbol being renamed.
                var originalSymbolResult = await AddLocationsReferenceSymbolsAsync(symbol, solution, cancellationToken).ConfigureAwait(false);

                // Next, find references to overloads, if the user has asked to rename those as well.
                var overloadsResult = options.RenameOverloads ? await GetOverloadsAsync(symbol, solution, cancellationToken).ConfigureAwait(false) :
                    ImmutableArray<SearchResult>.Empty;

                // Finally, include strings/comments if that's what the user wants.
                var (strings, comments) = await ReferenceProcessing.GetRenamableLocationsInStringsAndCommentsAsync(
                    symbol,
                    solution,
                    originalSymbolResult.Locations,
                    options.RenameInStrings,
                    options.RenameInComments,
                    cancellationToken).ConfigureAwait(false);

                using var _0 = ArrayBuilder<RenameLocation>.GetInstance(out var mergedLocations);
                using var _1 = ArrayBuilder<ISymbol>.GetInstance(out var mergedReferencedSymbols);
                using var _2 = ArrayBuilder<ReferenceLocation>.GetInstance(out var mergedImplicitLocations);

                var renameMethodGroupReferences = options.RenameOverloads || !RenameUtilities.GetOverloadedSymbols(symbol).Any();
                foreach (var result in overloadsResult.Concat(originalSymbolResult))
                {
                    mergedLocations.AddRange(renameMethodGroupReferences
                        ? result.Locations
                        : result.Locations.Where(x => x.CandidateReason != CandidateReason.MemberGroup));

                    mergedImplicitLocations.AddRange(result.ImplicitLocations);
                    mergedReferencedSymbols.AddRange(result.ReferencedSymbols);
                }

                // Add string and comment locations to the merged hashset 
                // after adding in reference symbols. This allows any references
                // in comments to be resolved as proper references rather than
                // comment resolutions. See https://github.com/dotnet/roslyn/issues/54294
                mergedLocations.AddRange(strings.NullToEmpty());
                mergedLocations.AddRange(comments.NullToEmpty());

                mergedLocations.RemoveDuplicates();

                return new SymbolicRenameLocations(
                    symbol, solution, options,
                    mergedLocations.ToImmutable(),
                    mergedImplicitLocations.ToImmutable(),
                    mergedReferencedSymbols.ToImmutable());
            }
        }

        private static async Task<ImmutableArray<SearchResult>> GetOverloadsAsync(
            ISymbol symbol, Solution solution, CancellationToken cancellationToken)
        {
            using var _ = ArrayBuilder<SearchResult>.GetInstance(out var overloadsResult);

            foreach (var overloadedSymbol in RenameUtilities.GetOverloadedSymbols(symbol))
                overloadsResult.Add(await AddLocationsReferenceSymbolsAsync(overloadedSymbol, solution, cancellationToken).ConfigureAwait(false));

            return overloadsResult.ToImmutable();
        }

        private static async Task<SearchResult> AddLocationsReferenceSymbolsAsync(
            ISymbol symbol,
            Solution solution,
            CancellationToken cancellationToken)
        {
            var locations = ImmutableHashSet.CreateBuilder<RenameLocation>();
            var referenceSymbols = await SymbolFinder.FindRenamableReferencesAsync(
                ImmutableArray.Create(symbol), solution, cancellationToken).ConfigureAwait(false);

            foreach (var referencedSymbol in referenceSymbols)
            {
                locations.AddAll(
                    await ReferenceProcessing.GetRenamableDefinitionLocationsAsync(referencedSymbol.Definition, symbol, solution, cancellationToken).ConfigureAwait(false));

                locations.AddAll(
                    await referencedSymbol.Locations.SelectManyInParallelAsync(
                        (l, c) => ReferenceProcessing.GetRenamableReferenceLocationsAsync(referencedSymbol.Definition, symbol, l, solution, c),
                        cancellationToken).ConfigureAwait(false));
            }

            var implicitLocations = referenceSymbols.SelectMany(refSym => refSym.Locations).Where(loc => loc.IsImplicit).ToImmutableArray();
            var referencedSymbols = referenceSymbols.Select(r => r.Definition).Where(r => !r.Equals(symbol)).ToImmutableArray();

            return new SearchResult(locations.ToImmutable(), implicitLocations, referencedSymbols);
        }
    }
}
