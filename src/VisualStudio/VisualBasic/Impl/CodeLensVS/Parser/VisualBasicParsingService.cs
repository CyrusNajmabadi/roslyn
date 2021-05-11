// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    /// <summary>
    /// Parsing service implementation for Visual Basic.
    /// </summary>
    [ContentType("Basic")]
    [Export(typeof(IParsingService))]
    internal sealed partial class VisualBasicParsingService : IParsingService
    {
        public SyntaxTree Parse(SourceText text)
        {
            ArgumentValidation.NotNull(text, "text");

            return VisualBasicSyntaxTree.ParseText(text);
        }

        /// <summary>
        /// Called to visit a syntax tree.
        /// </summary>
        /// <param name="tree">The syntax tree</param>
        /// <param name="onNodeFound">A callback action</param>
        /// <param name="cancellationToken">A cancellation token that can stop the walk before it finishes</param>
        public void VisitSyntaxTree(SyntaxTree tree, Action<INodeInfo> onNodeFound, CancellationToken cancellationToken)
        {
            ArgumentValidation.NotNull(tree, "tree");

            VisualBasicSyntaxNodeVisitor visitor = new VisualBasicSyntaxNodeVisitor(onNodeFound, cancellationToken);
            visitor.Visit(tree.GetRoot());
        }

        public bool IsValidSyntaxTree(SyntaxTree tree)
        {
            return tree is VisualBasicSyntaxTree;
        }
    }
}
