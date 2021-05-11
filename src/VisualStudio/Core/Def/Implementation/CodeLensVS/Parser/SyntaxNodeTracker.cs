// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    internal sealed class SyntaxNodeTracker : IEquatable<SyntaxNodeTracker>
    {
        private readonly ITextSnapshot snapshot;

        private readonly ITrackingPoint trackingPoint;

        /// <summary>
        /// This is the position of the node in the path.
        /// </summary>
        private readonly int position;

        /// <summary>
        /// This is the version of the text editor when the locator was created.
        /// </summary>
        private readonly int initialVersion;

        /// <summary>
        /// Creates the syntax path out of a specific node and text container.
        /// </summary>
        /// <param name="node">The node to copy a path for</param>
        /// <param name="snapshot">The text snapshot that we can use to track the point</param>
        public SyntaxNodeTracker(SyntaxNode node, ITextSnapshot snapshot)
            : this(node,
                   snapshot,
                   ArgumentValidation.NotNullPassThrough(node, "node", (SyntaxNode n) => n.Span.Start))
        {
        }

        /// <summary>
        /// Creates the syntax path out of a specific node and text container.
        /// </summary>
        /// <param name="node">The node to copy a path for</param>
        /// <param name="snapshot">The text snapshot that we can use to track the point</param>
        /// <param name="position">Tag position within the node</param>
        public SyntaxNodeTracker(SyntaxNode node, ITextSnapshot snapshot, int position)
        {
            ArgumentValidation.NotNull(node, "node");
            this.snapshot = snapshot;
            this.position = position;

            // The user can create a locator that doesn't track it's location in the document if they want to,
            // though this won't have much use outside of unit testing.
            if (this.snapshot != null)
            {
                // Note: positive tracking will handle cases where insertions at the start
                // of a tracking point cause the point to move. This is what we want. For instance,
                // if the user has "void f() { }" and then inserts "public " before "void",
                // the tracking point that used to be at "void" needs to move. That only happens
                // if we're using positive tracking.
                this.trackingPoint = this.snapshot.CreateTrackingPoint(this.position, PointTrackingMode.Positive);
                this.initialVersion = this.snapshot.Version.VersionNumber;
            }
        }

        /// <summary>
        /// Returns the current position, translated from the text buffer to the latest snapshot
        /// if this locator has a TextBuffer.
        /// </summary>
        public int CurrentPosition
        {
            get
            {
                int currentPosition = this.position;
                if (this.snapshot != null && this.trackingPoint != null)
                {
                    currentPosition = this.trackingPoint.GetPosition(this.snapshot.TextBuffer.CurrentSnapshot);
                }

                return currentPosition;
            }
        }

        /// <summary>
        /// Returns true if two locators point to the same point in the document.
        /// </summary>
        /// <param name="otherTracker">The other locator</param>
        /// <returns>True if the two locators point to the same place in the document</returns>
        public bool IsEquivalentTo(SyntaxNodeTracker otherTracker)
        {
            if (otherTracker != null)
            {
                return this.CurrentPosition == otherTracker.CurrentPosition;
            }

            return false;
        }

        /// <summary>
        /// Override equals so we can re-use existing tags.
        /// </summary>
        /// <param name="obj">The object to compare us to</param>
        /// <returns>True if the objects are equal</returns>
        public override bool Equals(object obj)
        {
            var other = obj as SyntaxNodeTracker;
            if (other != null)
            {
                return this.position == other.position
                    && this.initialVersion == other.initialVersion;
            }

            return false;
        }

        bool IEquatable<SyntaxNodeTracker>.Equals(SyntaxNodeTracker other)
        {
            return this.Equals(other);
        }

        public override int GetHashCode()
        {
            return this.position ^ this.initialVersion;
        }
    }
}
