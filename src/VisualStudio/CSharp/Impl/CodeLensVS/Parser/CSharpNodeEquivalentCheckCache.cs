// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser;

namespace Microsoft.VisualStudio.LanguageServices.CSharp.CodeLensVS.Parser
{
    internal class CSharpNodeEquivalentCheckCache : NodeEquivalentCheckCache
    {
        protected override bool CheckEquivalenceWorker(SyntaxNode oldNode, SyntaxNode newNode)
        {
            return this.CheckEquivalence(oldNode.Parent, newNode.Parent)
                && SyntaxFactory.AreEquivalent(oldNode, newNode, topLevel: true);
        }
    }
}
