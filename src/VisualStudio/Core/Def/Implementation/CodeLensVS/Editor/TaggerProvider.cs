// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    internal abstract class TaggerProvider<TTag>
        : IViewTaggerProvider where TTag : Microsoft.VisualStudio.Language.CodeLens.ICodeLensTag
    {
        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            ArgumentValidation.NotNull(textView, "textView");
            ArgumentValidation.NotNull(buffer, "buffer");

            // We only care about cases where the TextBuffer on the TextView matches the TextBuffer passed in
            if (textView.TextBuffer == buffer)
            {
                Tagger<TTag> tagger = this.CreateTagger(textView);
                if (tagger != null)
                {
                    tagger.UpdateSnapshotAsync(true).FireAndForget();

                    return (ITagger<T>)tagger;
                }
            }

            return null;
        }

        protected abstract Tagger<TTag> CreateTagger(ITextView textView);
    }
}
