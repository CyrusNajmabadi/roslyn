// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.EmbeddedLanguages.VirtualChars;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Common;

internal static class EmbeddedSyntaxHelpers
{
    public static TextSpan GetSpan<TSyntaxKind>(EmbeddedSyntaxToken<TSyntaxKind> token1, EmbeddedSyntaxToken<TSyntaxKind> token2) where TSyntaxKind : struct
        => GetSpan(token1.VirtualChars[0], token2.VirtualChars[^1]);

    public static TextSpan GetSpan<TVirtualCharSequence, TVirtualCharSequenceIntrospector>(TVirtualCharSequence virtualChars)
        where TVirtualCharSequenceIntrospector : struct, IVirtualCharSequenceIntrospector<TVirtualCharSequence>
    {
        var introspector = default(TVirtualCharSequenceIntrospector);
        return GetSpan(introspector.GetAt(virtualChars, 0), introspector.GetAt(virtualChars, introspector.GetLength(virtualChars) - 1));
    }

    public static TextSpan GetSpan(VirtualChar firstChar, VirtualChar lastChar)
        => TextSpan.FromBounds(firstChar.Span.Start, lastChar.Span.End);
}
