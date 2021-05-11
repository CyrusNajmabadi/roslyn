// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    internal interface IParsingService
    {
        /// <summary>
        /// Parse the provided text into a syntax tree.
        /// </summary>
        /// <param name="text">Text to parse.</param>
        /// <returns>A syntax tree from parsing text.</returns>
        SyntaxTree Parse(SourceText text);

        /// <summary>
        /// Called to visit a syntax tree.
        /// </summary>
        /// <param name="tree">The syntax tree</param>
        /// <param name="onNodeFound">A callback action</param>
        /// <param name="cancellationToken">A cancellation token that can be used to cancel the syntax tree visit</param>
        void VisitSyntaxTree(SyntaxTree tree, Action<INodeInfo> onNodeFound, CancellationToken cancellationToken);

        /// <summary>
        /// Called to determine if current ParsingService understands the current SyntaxTree. If it doesn't, we reparse.
        /// </summary>
        /// <param name="tree">The syntax tree</param>
        bool IsValidSyntaxTree(SyntaxTree? tree);
    }
}
