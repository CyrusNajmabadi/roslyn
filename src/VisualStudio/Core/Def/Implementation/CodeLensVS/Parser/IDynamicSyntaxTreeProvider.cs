// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    /// <summary>
    /// Provider of dynamic syntax trees.
    /// </summary>
    internal interface IDynamicSyntaxTreeProvider
    {
        /// <summary>
        /// Gets a dynamic syntax tree for a textView/buffer that reacts to changes.
        /// </summary>
        /// <param name="parsingService">The parsing service that knows how to parse the specified text buffer.</param>
        IDynamicSyntaxTree CreateDynamicSyntaxTree(IParsingService parsingService);
    }

    internal interface IDynamicSyntaxTree
    {
        SyntaxTree? CurrentSyntaxTree { get; }

        /// <summary>
        /// Updates the syntax tree to the state of the text buffer.
        /// </summary>
        Task UpdateAsync(ITextSnapshot snapshot, CancellationToken cancellationToken);
    }
}
