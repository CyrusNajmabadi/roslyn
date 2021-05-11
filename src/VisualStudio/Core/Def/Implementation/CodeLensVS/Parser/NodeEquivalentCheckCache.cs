// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    /// <summary>
    /// This class is used to help speed up SyntaxFactory.AreEquivelant 
    /// checks so that we don't compare the same nodes over and over again.
    /// </summary>
    internal class NodeEquivalentCheckCache : IReusableDescriptorComparisonCache
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

        /// <summary>
        /// Returns true if two nodes are equivalent.
        /// </summary>
        /// <param name="oldNode">The old node</param>
        /// <param name="newNode">The new node</param>
        /// <returns>True if the nodes are equivalent</returns>
        public bool CheckEquivalence(SyntaxNode oldNode, SyntaxNode newNode)
        {
            ArgumentValidation.NotNull(oldNode, "oldNode");
            ArgumentValidation.NotNull(newNode, "newNode");

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

            bool result = false;
            if (!this.cachedResultsLookup.TryGetValue(oldNode, out result))
            {
                if (string.Equals(oldNode.Language, LanguageNames.CSharp) && string.Equals(newNode.Language, LanguageNames.CSharp))
                {
                    result = this.CheckCSharpEquivalence(oldNode, newNode, result);
                }

                if (string.Equals(oldNode.Language, LanguageNames.VisualBasic) && string.Equals(newNode.Language, LanguageNames.VisualBasic))
                {
                    result = this.CheckVisualBasicEquivalence(oldNode, newNode, result);
                }

                this.cachedResultsLookup[oldNode] = result;
            }

            return result;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private bool CheckVisualBasicEquivalence(SyntaxNode oldNode, SyntaxNode newNode, bool result)
        {
            var vbasicNode1 = (VB.VisualBasicSyntaxNode)oldNode;
            var vbasicNode2 = (VB.VisualBasicSyntaxNode)newNode;

            return this.CheckEquivalence(vbasicNode1.Parent, vbasicNode2.Parent)
                    && VB.SyntaxFactory.AreEquivalent(vbasicNode1, vbasicNode2, true);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private bool CheckCSharpEquivalence(SyntaxNode oldNode, SyntaxNode newNode, bool result)
        {
            var csharpNode1 = oldNode;
            var csharpNode2 = newNode;

            return this.CheckEquivalence(csharpNode1.Parent, csharpNode2.Parent)
                    && CS.SyntaxFactory.AreEquivalent(csharpNode1, csharpNode2, true);
        }
    }
}
