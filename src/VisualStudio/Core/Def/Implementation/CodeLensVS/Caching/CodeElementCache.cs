// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Caching
{
    internal class CodeElementCache : ICodeElementCache
    {
        /// <summary>
        /// Used to parse source code in a language agnostic way.
        /// </summary>
        private readonly IEnumerable<Lazy<IParsingService, IContentTypeMetadata>> parsingServices;

        /// <summary>
        /// Used to create syntax tree snapshots in a language agnostic way.
        /// </summary>
        private readonly IDynamicSyntaxTreeProvider dynamicSyntaxTreeProvider;

        /// <summary>
        /// The parsing service used to walk the current syntax tree and populate the cache.
        /// </summary>
        private IParsingService currentParsingService;

        /// <summary>
        /// The syntax tree which is updated from the source code before rebuilding the cache.
        /// </summary>
        private IDynamicSyntaxTree currentSyntaxTree;

        /// <summary>
        /// A dictionary lookup of line numbers to cache data.
        /// </summary>
        private volatile Dictionary<int, ICacheEntry> cacheData;

        /// <summary>
        /// Creates the cache.
        /// </summary>
        /// <param name="parsingServices">The IParsingServices for the different supported languages.</param>
        /// <param name="dynamicSyntaxTreeProvider">The IDynamicSyntaxTreeProvider to create IDynamicSyntaxTrees.</param>
        public CodeElementCache(
            IEnumerable<Lazy<IParsingService, IContentTypeMetadata>> parsingServices,
            IDynamicSyntaxTreeProvider dynamicSyntaxTreeProvider)
        {
            this.parsingServices = parsingServices;
            this.dynamicSyntaxTreeProvider = dynamicSyntaxTreeProvider;
        }

        /// <summary>
        /// Gets the line numbers that currently have entries in the cache.
        /// </summary>
        public IEnumerable<int> LineNumbers
        {
            get
            {
                if (this.cacheData == null)
                {
                    return Enumerable.Empty<int>();
                }

                return this.cacheData.Keys;
            }
        }

        /// <summary>
        /// Tries to get a cache entry at a specific line number.
        /// </summary>
        /// <param name="lineNumber">The line number to lookup</param>
        /// <param name="cacheEntry">The cache entry at this line number</param>
        /// <returns>True if there is an entry at this line, otherwise, false.</returns>
        public bool TryGetAt(int lineNumber, out ICacheEntry cacheEntry)
        {
            cacheEntry = null;
            if (this.cacheData != null && this.cacheData.TryGetValue(lineNumber, out cacheEntry))
            {
                return true;
            }

            return false;
        }

        /// <summary>
        /// Rebuilds the cache based on a syntax tree snapshot.
        /// </summary>
        /// <param name="snapshot">The specific snapshot to rebuild from</param>
        /// <param name="parseOptionsOpt">The parse options to use. Can be null to use language default options.</param>
        /// <param name="clean">If true, the cache is completely rebuilt.</param>        
        /// <param name="cancellationToken">The cancellation token to cancel the rebuilding.</param>
        public async Task RebuildAsync(ITextSnapshot snapshot, bool clean, CancellationToken cancellationToken)
        {
            ArgumentValidation.NotNull(snapshot, "snapshot");

            ITextBuffer textBuffer = snapshot.TextBuffer;

            if (this.currentParsingService == null || clean)
            {
                var matchingService = this.parsingServices.SingleOrDefault(
                    parsingService => parsingService.Metadata.ContentTypes.Any(ct => textBuffer.ContentType.IsOfType(ct)));

                if (matchingService != null)
                {
                    this.currentParsingService = matchingService.Value;
                }
            }

            if (this.currentParsingService == null)
            {
                // if there is no parsing service, bail out. 
                // there is nothing we can do after this point
                return;
            }

            if (clean || this.currentSyntaxTree == null)
            {
                this.currentSyntaxTree = this.dynamicSyntaxTreeProvider.CreateDynamicSyntaxTree(this.currentParsingService);
            }

            await this.currentSyntaxTree.UpdateAsync(snapshot, cancellationToken);

            cancellationToken.ThrowIfCancellationRequested();

            var cache = new Dictionary<int, ICacheEntry>();
            this.currentParsingService.VisitSyntaxTree(this.currentSyntaxTree.CurrentSyntaxTree, nodeInfo =>
            {
                TextLine line = currentSyntaxTree.CurrentSyntaxTree.GetText().Lines.GetLineFromPosition(nodeInfo.Start);
                if (!cache.ContainsKey(line.LineNumber))
                {
                    cache[line.LineNumber] = new CacheEntry(nodeInfo.CreateSyntaxNodeInfo(snapshot), nodeInfo.Start - line.Start);
                }
            }, cancellationToken);

            this.cacheData = cache;
        }
    }
}
