// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Roslyn.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    internal abstract class TaggerProvider<TTag>
        : IViewTaggerProvider where TTag : Microsoft.VisualStudio.Language.CodeLens.ICodeLensTag
    {
        public ITagger<T>? CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            Contract.ThrowIfNull(textView);
            Contract.ThrowIfNull(buffer);

            // We only care about cases where the TextBuffer on the TextView matches the TextBuffer passed in
            if (textView.TextBuffer == buffer)
            {
                var tagger = this.CreateTagger(textView);
                if (tagger != null)
                {
                    // fire and forget
                    _ = tagger.UpdateSnapshotAsync(clean: true);

                    return (ITagger<T>)tagger;
                }
            }

            return null;
        }

        protected abstract Tagger<TTag>? CreateTagger(ITextView textView);
    }
}
