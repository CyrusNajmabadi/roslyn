// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml
{
    internal enum XmlKind
    {
        None,

        CompilationUnit,
        EndOfFile,
        Sequence,

        UnknownNode,
        UnknownToken,
        UnknownTrivia,

        CDataSection,
        Comment,
        Element,
        ElementEndTag,
        ElementStartTag,
        EmptyElement,
        Name,
        NameAttribute,
        Prefix,
        ProcessingInstruction,
        Text,
        TextAttribute,

        CDataEndToken,
        CDataStartToken,
        ColonToken,
        CommentStartToken,
        DoubleQuoteToken,
        EntityLiteralToken,
        EqualsToken,
        GreaterThanToken,
        IdentifierToken,
        LessThanSlashToken,
        LessThanToken,
        MinusMinusToken,
        ProcessingInstructionEndToken,
        ProcessingInstructionStartToken,
        SingleQuoteToken,
        SlashGreaterThanToken,
        TextLiteralNewLineToken,
        TextLiteralToken,
    }
}
