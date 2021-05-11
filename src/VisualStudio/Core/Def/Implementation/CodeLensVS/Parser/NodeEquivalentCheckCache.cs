// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor;
using Roslyn.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    /// <summary>
    /// This class is used to help speed up SyntaxFactory.AreEquivelant 
    /// checks so that we don't compare the same nodes over and over again.
    /// </summary>
    internal abstract class NodeEquivalentCheckCache : IReusableDescriptorComparisonCache
    {
        /// <summary>
        /// This dictionary caches nodes to results of the comparison against those nodes.
        /// </summary>
        private readonly Dictionary<SyntaxNode, bool> cachedResultsLookup;

        /// <summary>
        /// Creates the cache.
        /// </summary>
        public NodeEquivalentCheckCache()
        {
            this.cachedResultsLookup = new Dictionary<SyntaxNode, bool>();
        }

        protected abstract bool CheckEquivalenceWorker(SyntaxNode oldNode, SyntaxNode newNode);

        /// <summary>
        /// Returns true if two nodes are equivalent.
        /// </summary>
        /// <param name="oldNode">The old node</param>
        /// <param name="newNode">The new node</param>
        /// <returns>True if the nodes are equivalent</returns>
        public bool CheckEquivalence(SyntaxNode? oldNode, SyntaxNode? newNode)
        {
            Contract.ThrowIfNull(oldNode);
            Contract.ThrowIfNull(newNode);

            // Two compilation units will always be different, no matter what, so
            // we never want to compare them. If we're at the root of each tree, 
            // just say they're equal
            if (oldNode.Parent == null && newNode.Parent == null)
            {
                return true;
            }

            // This indicates that one tree has a larger depth than the other. They 
            // cannot be equivalent!
            if (oldNode.Parent == null ^ newNode.Parent == null)
            {
                return false;
            }

            if (!this.cachedResultsLookup.TryGetValue(oldNode, out var result))
            {
                result = CheckEquivalenceWorker(oldNode, newNode);
                this.cachedResultsLookup[oldNode] = result;
            }

            return result;
        }

        //[MethodImpl(MethodImplOptions.NoInlining)]
        //private bool CheckVisualBasicEquivalence(SyntaxNode oldNode, SyntaxNode newNode, bool result)
        //{
        //    var vbasicNode1 = (VB.VisualBasicSyntaxNode)oldNode;
        //    var vbasicNode2 = (VB.VisualBasicSyntaxNode)newNode;

        //    return this.CheckEquivalence(vbasicNode1.Parent, vbasicNode2.Parent)
        //            && VB.SyntaxFactory.AreEquivalent(vbasicNode1, vbasicNode2, true);
        //}
    }
}
