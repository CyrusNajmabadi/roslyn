// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Common;
using Microsoft.CodeAnalysis.EmbeddedLanguages.VirtualChars;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml
{
    internal sealed class XmlTree : EmbeddedSyntaxTree<XmlKind, XmlNode, XmlCompilationUnit>
    {
        public XmlTree(
            ImmutableArray<VirtualChar> text,
            XmlCompilationUnit root,
            ImmutableArray<EmbeddedDiagnostic> diagnostics)
            : base(text, root, diagnostics)
        {
        }
    }
}
