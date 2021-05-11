// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Caching;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Extensions;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Roslyn.Utilities;
using Microsoft.VisualStudio.Language.CodeLens;
using Microsoft.CodeAnalysis.Host.Mef;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    [Export(typeof(IViewTaggerProvider))]
    [TagType(typeof(ICodeLensTag))]
    [ContentType("CSharp")]
    [ContentType("Basic")]
    internal sealed class CodeElementTaggerProvider : TaggerProvider<CodeElementTag>, IDisposable
    {
        private readonly ICodeElementCacheProvider codeElementCacheProvider;

        [ImportingConstructor]
        [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
        public CodeElementTaggerProvider(
            ICodeElementCacheProvider codeElementCacheProvider)
        {
            Contract.ThrowIfNull(codeElementCacheProvider);

            this.codeElementCacheProvider = codeElementCacheProvider;
        }

        public void Dispose()
        {
        }

        protected override Tagger<CodeElementTag>? CreateTagger(ITextView textView)
        {
            Contract.ThrowIfNull(textView);
            Contract.ThrowIfTrue(textView.IsClosed, "CreateTagger called for a closed textview!");

            var documentFileName = textView.TextBuffer.GetDocumentMoniker();
            if (string.IsNullOrEmpty(documentFileName))
            {
                return null;
            }

            var codeElementTagger = new CodeElementTagger(
                textView,
                this.codeElementCacheProvider.CreateCache());

            return codeElementTagger;
        }
    }
}
