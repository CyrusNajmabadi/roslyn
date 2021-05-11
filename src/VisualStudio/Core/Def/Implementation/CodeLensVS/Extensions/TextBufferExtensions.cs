// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Extensions
{
    internal static class TextBufferExtensions
    {
        /// <summary>
        /// Gets the moniker for the text buffer if it exists, null otherwise
        /// </summary>
        /// <param name="textBuffer">Text buffer</param>
        /// <returns>Document moniker</returns>
        public static string? GetDocumentMoniker(this ITextBuffer textBuffer)
        {
            if (textBuffer == null || !textBuffer.Properties.TryGetProperty(typeof(ITextDocument), out ITextDocument document))
                return null;

            return document?.FilePath;
        }
    }
}
