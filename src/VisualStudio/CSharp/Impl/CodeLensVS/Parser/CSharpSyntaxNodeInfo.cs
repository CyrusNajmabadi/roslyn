// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser;

namespace Microsoft.VisualStudio.LanguageServices.CSharp.CodeLensVS.Parser
{
    internal class CSharpSyntaxNodeInfo : SyntaxNodeInfo
    {
        public CSharpSyntaxNodeInfo(SyntaxNode node, SyntaxNodeKind syntaxNodeKind, SyntaxNodeTracker syntaxNodeTracker) : base(node, syntaxNodeKind, syntaxNodeTracker)
        {
        }

        protected override bool AreEquivalent(SyntaxNode oldNode, SyntaxNode newNode)
            => SyntaxFactory.AreEquivalent(oldNode, newNode, topLevel: false);
    }
}
