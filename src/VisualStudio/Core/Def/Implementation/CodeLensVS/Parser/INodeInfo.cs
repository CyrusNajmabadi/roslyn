// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    /// <summary>
    /// Defines an object that has information on a node that was found from a syntax node visitor.
    /// </summary>
    internal interface INodeInfo
    {
        /// <summary>
        /// Returns the starting position of the node.
        /// </summary>
        int Start
        {
            get;
        }

        /// <summary>
        /// Returns a syntax node info object for this found node.
        /// </summary>
        /// <param name="textSnapshot">A text snapshot that the locater uses to translate paths between snapshots</param>
        SyntaxNodeInfo CreateSyntaxNodeInfo(ITextSnapshot textSnapshot);
    }
}
