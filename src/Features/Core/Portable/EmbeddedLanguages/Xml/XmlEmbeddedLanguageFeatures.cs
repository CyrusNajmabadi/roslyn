// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.DocumentHighlighting;
using Microsoft.CodeAnalysis.EmbeddedLanguages.LanguageServices;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Xml.LanguageServices;

namespace Microsoft.CodeAnalysis.Features.EmbeddedLanguages.Xml
{
    internal class XmlEmbeddedLanguageFeatures : XmlEmbeddedLanguage, IEmbeddedLanguageFeatures
    {
        public IDocumentHighlightsService DocumentHighlightsService { get; }
        public AbstractBuiltInCodeStyleDiagnosticAnalyzer DiagnosticAnalyzer { get; }

        public XmlEmbeddedLanguageFeatures(EmbeddedLanguageInfo info) : base(info)
        {
            // DocumentHighlightsService = new XmlDocumentHighlightsService(this);
            DiagnosticAnalyzer = new XmlDiagnosticAnalyzer(info);
        }
    }
}
