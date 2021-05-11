// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Caching;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    [Export(typeof(IViewTaggerProvider))]
    [TagType(typeof(Microsoft.VisualStudio.Language.CodeLens.ICodeLensTag))]
    [ContentType("CSharp")]
    [ContentType("Basic")]
    public sealed class CodeElementTaggerProvider : TaggerProvider<CodeElementTag>, IDisposable
    {
        private readonly ICodeElementCacheProvider codeElementCacheProvider;
        private readonly MeasurementBlock measurementBlock;
        private readonly string createTaggerMeasurementBlockName = "CODE_ELEMENT_TAGGER_PROVIDER_CREATE_TAGGER";

        [ImportingConstructor]
        public CodeElementTaggerProvider(
            ICodeElementCacheProvider codeElementCacheProvider)
        {
            ArgumentValidation.NotNull(codeElementCacheProvider, "codeElementCacheProvider");

            this.codeElementCacheProvider = codeElementCacheProvider;
            this.measurementBlock = new MeasurementBlock();
        }

        public void Dispose()
        {
            if (this.measurementBlock != null)
            {
                this.measurementBlock.Dispose();
            }
        }

        protected override Tagger<CodeElementTag> CreateTagger(ITextView textView)
        {
            ArgumentValidation.NotNull(textView, "textView");
            Contract.Assert(!textView.IsClosed, "CreateTagger called for a closed textview!");

            this.measurementBlock.Begin(1, this.createTaggerMeasurementBlockName);
            var documentFileName = textView.TextBuffer.GetDocumentMoniker();
            if (string.IsNullOrEmpty(documentFileName))
            {
                return null;
            }

            var codeElementTagger = new CodeElementTagger(
                textView,
                this.codeElementCacheProvider.CreateCache());

            this.measurementBlock.End();
            this.measurementBlock.Reset();

            return codeElementTagger;
        }
    }
}
