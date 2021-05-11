// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser;
using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Queries
{
    internal partial class CodeElementTag
    {
        /// <summary>
        /// A descriptor for a Roslyn element in CodeSense
        /// </summary>
        private class CodeElementDescriptor
            : DocumentDescriptor
            , IReusableDescriptor
            , ICodeElementDescriptor
        {
            private readonly SyntaxNodeInfo syntaxNodeInfo;

            private string elementDescription = null;

            /// <summary>
            /// Instantiates a new method descriptor
            /// </summary>
            /// <param name="syntaxNodeInfo">The syntax node info for the method declaration</param>
            /// <param name="filePath">Path to file</param>
            /// <param name="documentId">The <see cref="DocumentId"/> of the file.</param>
            /// <param name="projectGuid">The GUID of the project, as found from the IVsHierarchy.</param>
            public CodeElementDescriptor(SyntaxNodeInfo syntaxNodeInfo, string filePath, DocumentId documentId, Workspace workspace, Guid projectGuid)
                : base(filePath, projectGuid)
            {
                ArgumentValidation.NotNull(syntaxNodeInfo, "syntaxNodeInfo");

                this.syntaxNodeInfo = syntaxNodeInfo;
                this.DocumentId = documentId;
                this.Workspace = workspace;
            }

            public event EventHandler SyntaxNodeChanged
            {
                add
                {
                    this.syntaxNodeInfo.NodeChanged += value;
                }

                remove
                {
                    this.syntaxNodeInfo.NodeChanged -= value;
                }
            }

            public event EventHandler SyntaxNodeContentsChanged
            {
                add
                {
                    this.syntaxNodeInfo.NodeContentsChanged += value;
                }

                remove
                {
                    this.syntaxNodeInfo.NodeContentsChanged -= value;
                }
            }

            /// <summary>
            /// A short description of the code element.  The description is the name of the identifier prepended by its parent name if available. 
            /// </summary>
            public override string ElementDescription
            {
                get
                {
                    if (this.elementDescription == null)
                    {
                        // elementDescription was never set, set it here
                        if (this.SyntaxNode == null)
                        {
                            this.elementDescription = string.Empty;
                        }
                        else
                        {
                            this.elementDescription = this.SyntaxNode.GetIdentifierName();
                            SyntaxNode parentNode = this.SyntaxNode.GetNextParent();
                            if (parentNode != null)
                            {
                                string parentName = parentNode.GetIdentifierName();

                                if (!string.IsNullOrEmpty(this.elementDescription) && !string.IsNullOrEmpty(parentName))
                                {
                                    // prepend the parents name 
                                    this.elementDescription = string.Format(CultureInfo.InvariantCulture, "{0}.{1}", parentName, this.elementDescription);
                                }
                            }
                        }
                    }

                    return this.elementDescription;
                }
            }

            public SyntaxNode SyntaxNode => this.syntaxNodeInfo.Node;

            /// <summary>
            /// This sets <see cref="Microsoft.VisualStudio.Language.CodeLens.ICodeLensDescriptor.ApplicableSpan"/>
            /// which some CodeLens providers depend on.
            /// </summary>
            public override Span? ApplicableSpan => Span.FromBounds(SyntaxNode.Span.Start, SyntaxNode.Span.End);

            public override CodeElementKinds Kind
            {
                get
                {
                    switch (this.syntaxNodeInfo.Kind)
                    {
                        case SyntaxNodeKind.Method:
                            return CodeElementKinds.Method;
                        case SyntaxNodeKind.Property:
                            return CodeElementKinds.Property;
                        case SyntaxNodeKind.Type:
                            return CodeElementKinds.Type;
                        default:
                            return CodeElementKinds.Unspecified;
                    }
                }
            }

            public DocumentId DocumentId { get; }

            public Workspace Workspace { get; }

            /// <summary>
            /// Compares two descriptors and returns true if they are equivelant.
            /// </summary>
            /// <param name="other">The other descriptor</param>
            /// <returns>True if the two descriptors are equivelant</returns>
            bool IReusableDescriptor.IsEquivalentTo(IReusableDescriptor other)
            {
                CodeElementDescriptor descriptor = other as CodeElementDescriptor;
                return descriptor != null
                    ? this.syntaxNodeInfo.IsEquivalentTo(descriptor.syntaxNodeInfo)
                    : false;
            }

            /// <summary>
            /// Creates and returns a new NodeEquivalentCheckCache object.
            /// </summary>
            /// <returns>A new NodeEquivalentCheckCache</returns>
            IReusableDescriptorComparisonCache IReusableDescriptor.CreateCache()
            {
                return new NodeEquivalentCheckCache();
            }

            /// <summary>
            /// A method that compares on descriptor to another and updates itself if applicable.
            /// </summary>
            /// <param name="other">The other descriptor to compare to</param>
            /// <param name="cache">The comparison cache to use</param>
            /// <returns>True if the descriptor updates</returns>
            bool IReusableDescriptor.CompareToAndUpdate(IReusableDescriptor other, IReusableDescriptorComparisonCache cache)
            {
                CodeElementDescriptor otherDescriptor = ((object)other) as CodeElementDescriptor;
                NodeEquivalentCheckCache equivalenceCache = cache as NodeEquivalentCheckCache;
                if (otherDescriptor != null && equivalenceCache != null)
                {
                    bool updated = this.syntaxNodeInfo.CompareToAndUpdateIfNeccessary(otherDescriptor.syntaxNodeInfo, equivalenceCache);

                    // if the node updated itself, clear any saved description
                    if (updated)
                    {
                        this.elementDescription = null;
                    }

                    return updated;
                }

                return false;
            }

            public override string ToString()
            {
                // This data is used for automation to identify this descriptor.
                if (!string.IsNullOrEmpty(this.ElementDescription))
                {
                    return this.ElementDescription;
                }

                return base.ToString();
            }
        }
    }
}
