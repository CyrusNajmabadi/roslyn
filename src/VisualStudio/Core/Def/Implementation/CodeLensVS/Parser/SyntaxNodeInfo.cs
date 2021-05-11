// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    internal enum SyntaxNodeKind
    {
        /// <summary>
        /// A method
        /// </summary>
        Method,

        /// <summary>
        /// A type (class, interface, struct, enum)
        /// </summary>
        Type,

        /// <summary>
        /// A property
        /// </summary>
        Property,
    }

    internal abstract class SyntaxNodeInfo
    {
        private SyntaxNodeTracker syntaxNodeTracker;
        private SyntaxNode node;

        /// <summary>
        /// This event is fired whenever the node changes.
        /// </summary>
        public event EventHandler? NodeChanged;

        /// <summary>
        /// This event is fired whenever the node contents changes.
        /// </summary>
        public event EventHandler? NodeContentsChanged;

        public SyntaxNodeKind Kind { get; private set; }

        public SyntaxNodeTracker Tracker => this.syntaxNodeTracker;

        public SyntaxNode Node => this.node;

        internal SyntaxNodeInfo(SyntaxNode node, SyntaxNodeKind syntaxNodeKind, SyntaxNodeTracker syntaxNodeTracker)
        {
            this.node = node;
            this.Kind = syntaxNodeKind;
            this.syntaxNodeTracker = syntaxNodeTracker;
        }

        protected abstract bool AreEquivalent(SyntaxNode oldNode, SyntaxNode newNode);
        public abstract string? GetIdentifierName();
        public abstract SyntaxNodeInfo? GetNextParent();
        public abstract IReusableDescriptorComparisonCache CreateCache();

        /// <summary>
        /// Returns true if two syntaxnodeinfo objects are equivalent (but not necessarily
        /// referentially equal).
        /// </summary>
        /// <param name="other">The other syntax node info object</param>
        /// <returns>True if the objects are equal</returns>
        internal bool IsEquivalentTo(SyntaxNodeInfo other)
        {
            if (this.Tracker == null && other.Tracker == null)
            {
                return true;
            }

            if (this.Tracker == null || other.Tracker == null)
            {
                return false;
            }

            if (this.node == null && other.node == null)
            {
                return true;
            }

            if (this.node == null || other.node == null)
            {
                return false;
            }

            return this.node.GetType() == other.node.GetType()
                && this.Tracker.IsEquivalentTo(other.Tracker);
        }

        /// <summary>
        /// Checks to see whether we should fire the NodeChanged event if the other syntax node is not equal
        /// to the node we're describing. If a datapoint is listening to this event, that means they should invalidate
        /// themselves because the underlying data has changed, but the location of the kind in the document 
        /// has not.
        /// </summary>
        internal bool CompareToAndUpdateIfNeccessary(SyntaxNodeInfo nodeInfo, NodeEquivalentCheckCache cache)
        {
            if (this.node == null)
            {
                return false;
            }

            var oldNode = this.node;
            this.node = nodeInfo.node;
            this.syntaxNodeTracker = nodeInfo.syntaxNodeTracker;

            this.FireNodeContentsChangedAsNeeded(oldNode, this.node);

            if (!cache.CheckEquivalence(oldNode, this.node))
            {
                this.FireNodeChanged();
                return true;
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// Fires the node changed event if somebody is listening.
        /// </summary>
        private void FireNodeChanged()
        {
            var nodeChanged = this.NodeChanged;
            if (nodeChanged != null)
            {
                nodeChanged(this, EventArgs.Empty);
            }
        }

        private void FireNodeContentsChangedAsNeeded(SyntaxNode oldNode, SyntaxNode newNode)
        {
            var nodeContentsChanged = this.NodeContentsChanged;
            if (nodeContentsChanged != null)
            {
                if (!this.AreEquivalent(oldNode, newNode))
                    nodeContentsChanged(this, EventArgs.Empty);
            }
        }
    }
}
