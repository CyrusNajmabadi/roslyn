// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.ComponentModel.Composition;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    /// <summary>
    /// Parsing service implementation for C# language
    /// </summary>
    [ContentType("CSharp")]
    [Export(typeof(IParsingService))]
    internal sealed class CSharpParsingService : IParsingService
    {
        public SyntaxTree Parse(SourceText text)
        {
            ArgumentValidation.NotNull(text, "text");

            return CSharpSyntaxTree.ParseText(text);
        }

        /// <summary>
        /// Visits the syntax tree and fires onNodeFound whenever we find a node that we should tag.
        /// </summary>
        /// <param name="tree">The tree to parse</param>
        /// <param name="onNodeFound">An action to call when we find a node to tag</param>
        /// <param name="cancellationToken">A cancellation token that can stop the walk before it finishes</param>
        public void VisitSyntaxTree(SyntaxTree tree, Action<INodeInfo> onNodeFound, CancellationToken cancellationToken)
        {
            ArgumentValidation.NotNull(tree, "tree");

            CSharpSyntaxNodeVisitor visitor = new CSharpSyntaxNodeVisitor(onNodeFound, cancellationToken);
            visitor.Visit(tree.GetRoot());
        }

        public bool IsValidSyntaxTree(SyntaxTree tree)
        {
            return tree is CSharpSyntaxTree;
        }
    }
}
