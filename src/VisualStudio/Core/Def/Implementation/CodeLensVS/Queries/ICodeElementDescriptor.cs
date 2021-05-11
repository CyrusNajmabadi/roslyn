// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using Microsoft.CodeAnalysis;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Queries
{
    internal interface ICodeElementDescriptor : IDocumentDescriptor
    {
        /// <summary>
        /// Event raised when syntax node associated with the code element descriptor changes
        /// </summary>
        event EventHandler SyntaxNodeChanged;

        /// <summary>
        /// Event raised when contents of the syntax node associated with the code element descriptor changes
        /// </summary>
        event EventHandler SyntaxNodeContentsChanged;

        /// <summary>
        /// Common Syntax Node for location in code
        /// </summary>
        SyntaxNode SyntaxNode { get; }

        /// <summary>
        ///  DocumentId of the document
        /// </summary>
        DocumentId DocumentId { get; }

        /// <summary>
        /// Workspace of the document.
        /// </summary>
        Workspace Workspace { get; }
    }
}
