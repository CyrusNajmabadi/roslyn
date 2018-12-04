// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Common;
using Microsoft.CodeAnalysis.EmbeddedLanguages.VirtualChars;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Host;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml
{
    using System.Xml.Linq;
    using static EmbeddedSyntaxHelpers;

    using XmlNodeOrToken = EmbeddedSyntaxNodeOrToken<XmlKind, XmlNode>;
    using XmlToken = EmbeddedSyntaxToken<XmlKind>;
    using XmlTrivia = EmbeddedSyntaxTrivia<XmlKind>;

    internal interface IXmlParser : ILanguageService
    {
        /// <summary>
        /// Given an input text, and set of options, parses out a fully representative syntax tree 
        /// and list of diagnostics.  Parsing should always succeed, except in the case of the stack 
        /// overflowing.
        /// </summary>
        XmlTree TryParse(ImmutableArray<VirtualChar> text, LoadOptions options);
    }
}
