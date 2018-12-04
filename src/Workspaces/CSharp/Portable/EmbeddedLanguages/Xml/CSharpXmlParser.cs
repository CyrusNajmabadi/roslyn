// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics;
using System.Xml.Linq;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Common;
using Microsoft.CodeAnalysis.EmbeddedLanguages.VirtualChars;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Xml;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.EmbeddedLanguages.Xml
{
    using XmlNodeOrToken = EmbeddedSyntaxNodeOrToken<XmlKind, XmlNode>;
    using XmlToken = EmbeddedSyntaxToken<XmlKind>;
    using XmlTrivia = EmbeddedSyntaxTrivia<XmlKind>;

    [ExportLanguageService(typeof(IXmlParser), LanguageNames.CSharp), Shared]
    internal class CSharpXmlParser : IXmlParser
    {
        private static ImmutableArray<VirtualChar> GetChars(ImmutableArray<VirtualChar> text, TextSpan fullSpan)
            => ImmutableArray.Create(text, fullSpan.Start, fullSpan.Length);

        public XmlTree TryParse(ImmutableArray<VirtualChar> text, LoadOptions options)
        {
            if (text.Length == 0)
            {
                return null;
            }

            var textWithSlashes = AddSlashes(text);

            var str = textWithSlashes.CreateString();
            var trivia = SyntaxFactory.ParseDocumentationCommentTrivia(str, CSharpParseOptions.Default);
            return new XmlTree(
                text, Convert(textWithSlashes, trivia),
                ConvertDiagnostics(trivia.GetDiagnostics(), textWithSlashes));
        }

        private ImmutableArray<VirtualChar> AddSlashes(ImmutableArray<VirtualChar> text)
        {
            var result = ArrayBuilder<VirtualChar>.GetInstance();
            var index = 0;

            while (index < text.Length)
            { 
                AddTripleSlash(result, text[index]);
                AddThroughNewLine(result, text, ref index);
            }

            return result.ToImmutableAndFree();
        }

        private void AddThroughNewLine(
            ArrayBuilder<VirtualChar> result, ImmutableArray<VirtualChar> text, ref int index)
        {
            while (index < text.Length)
            {
                var ch = text[index];
                result.Add(ch);
                index++;

                if (ch == '\r' && index < text.Length && text[index] == '\n')
                {
                    continue;
                }

                if (SyntaxFacts.IsNewLine(ch))
                {
                    return;
                }
            }
        }

        private static void AddTripleSlash(ArrayBuilder<VirtualChar> result, VirtualChar ch)
        {
            AddSlash(result, ch);
            AddSlash(result, ch);
            AddSlash(result, ch);
        }

        private static void AddSlash(ArrayBuilder<VirtualChar> result, VirtualChar ch)
            => result.Add(new VirtualChar('/', new TextSpan(ch.Span.Start, 0), allowEmpty: true));

        private ImmutableArray<EmbeddedDiagnostic> ConvertDiagnostics(
            IEnumerable<Diagnostic> diagnostics, ImmutableArray<VirtualChar> text)
        {
            var result = ArrayBuilder<EmbeddedDiagnostic>.GetInstance();

            foreach (var diagnostic in diagnostics)
            {
                result.Add(ConvertDiagnostic(diagnostic, text));
            }

            return result.ToImmutableAndFree();
        }

        private EmbeddedDiagnostic ConvertDiagnostic(
            Diagnostic diagnostic, ImmutableArray<VirtualChar> text)
        {
            var span = diagnostic.Location.SourceSpan;
            var firstChar = text[span.Start];
            var lastChar = text[Math.Max(span.Start, span.End - 1)];
            return new EmbeddedDiagnostic(
                diagnostic.GetMessage(),
                EmbeddedSyntaxHelpers.GetSpan(firstChar, lastChar));
        }

        private static XmlCompilationUnit Convert(ImmutableArray<VirtualChar> text, DocumentationCommentTriviaSyntax trivia)
        {
            return new XmlCompilationUnit(
                ConvertSequence(text, trivia.Content),
                ConvertToken(text, trivia.EndOfComment));
        }

        private static XmlGenericNode ConvertSequence(ImmutableArray<VirtualChar> text, SyntaxList<XmlNodeSyntax> content)
        {
            var result = ArrayBuilder<XmlNodeOrToken>.GetInstance();
            foreach (var node in content)
            {
                result.Add(ConvertNode(text, node));
            }

            return new XmlGenericNode(XmlKind.Sequence, result.ToImmutableAndFree());
        }

        private static XmlToken ConvertToken(ImmutableArray<VirtualChar> text, SyntaxToken token)
        {
            return new XmlToken(
                ConvertTokenKind(token.Kind()),
                ConvertTriviaList(text, token.LeadingTrivia),
                GetChars(text, token.Span),
                ConvertTriviaList(text, token.TrailingTrivia),
                diagnostics: ImmutableArray<EmbeddedDiagnostic>.Empty,
                value: null);
        }

        private static XmlKind ConvertTokenKind(SyntaxKind kind)
        {
            switch (kind)
            {
                case SyntaxKind.XmlCDataEndToken: return XmlKind.CDataEndToken;
                case SyntaxKind.XmlCDataStartToken: return XmlKind.CDataStartToken;
                case SyntaxKind.ColonToken: return XmlKind.ColonToken;
                case SyntaxKind.XmlCommentStartToken: return XmlKind.CommentStartToken;
                case SyntaxKind.DoubleQuoteToken: return XmlKind.DoubleQuoteToken;
                case SyntaxKind.XmlEntityLiteralToken: return XmlKind.EntityLiteralToken;
                case SyntaxKind.EqualsToken: return XmlKind.EqualsToken;
                case SyntaxKind.GreaterThanToken: return XmlKind.GreaterThanToken;
                case SyntaxKind.IdentifierToken: return XmlKind.IdentifierToken;
                case SyntaxKind.LessThanSlashToken: return XmlKind.LessThanSlashToken;
                case SyntaxKind.LessThanToken: return XmlKind.LessThanToken;
                case SyntaxKind.MinusMinusToken: return XmlKind.MinusMinusToken;
                case SyntaxKind.XmlProcessingInstructionEndToken: return XmlKind.ProcessingInstructionEndToken;
                case SyntaxKind.XmlProcessingInstructionStartToken: return XmlKind.ProcessingInstructionStartToken;
                case SyntaxKind.SingleQuoteToken: return XmlKind.SingleQuoteToken;
                case SyntaxKind.SlashGreaterThanToken: return XmlKind.SlashGreaterThanToken;
                case SyntaxKind.XmlTextLiteralNewLineToken: return XmlKind.TextLiteralNewLineToken;
                case SyntaxKind.XmlTextLiteralToken: return XmlKind.TextLiteralToken;
                default:
                    Debug.Fail("Unknown token kind");
                    return XmlKind.UnknownToken;
            }
        }

        private static XmlKind ConvertTriviaKind(SyntaxKind kind)
        {
            switch (kind)
            {

                default:
                    Debug.Fail("Unknown trivia kind");
                    return XmlKind.UnknownTrivia;
            }
        }

        private static XmlKind ConvertNodeKind(SyntaxKind kind)
        {
            switch (kind)
            {
                case SyntaxKind.XmlElement: return XmlKind.Element;
                default:
                    Debug.Fail("Unknown node type.");
                    return XmlKind.UnknownNode;
            }
        }

        private static ImmutableArray<XmlTrivia> ConvertTriviaList(ImmutableArray<VirtualChar> text, SyntaxTriviaList triviaList)
        {
            var result = ArrayBuilder<XmlTrivia>.GetInstance();

            foreach (var trivia in triviaList)
            {
                result.Add(ConvertTrivia(text, trivia));
            }

            return result.ToImmutableAndFree();
        }

        private static XmlTrivia ConvertTrivia(ImmutableArray<VirtualChar> text, SyntaxTrivia trivia)
        {
            return new XmlTrivia(
                ConvertTriviaKind(trivia.Kind()),
                GetChars(text, trivia.FullSpan),
                ImmutableArray<EmbeddedDiagnostic>.Empty);
        }

        private static XmlNode ConvertNode(ImmutableArray<VirtualChar> text, SyntaxNode node)
        {
            return new XmlGenericNode(
                ConvertNodeKind(node.Kind()),
                ConvertChildNodeAndTokens(text, node));
        }

        private static ImmutableArray<XmlNodeOrToken> ConvertChildNodeAndTokens(ImmutableArray<VirtualChar> text, SyntaxNode node)
        {
            var children = ArrayBuilder<XmlNodeOrToken>.GetInstance();
            foreach (var child in node.ChildNodesAndTokens())
            {
                if (child.IsToken)
                {
                    children.Add(ConvertToken(text, child.AsToken()));
                }
                else
                {
                    children.Add(ConvertNode(text, child.AsNode()));
                }
            }

            return children.ToImmutableAndFree();
        }

        //private static XmlElementNode ConvertXmlElement(ImmutableArray<VirtualChar> text, XmlElementSyntax node)
        //{
        //    return new XmlElementNode(
        //        ConvertNode(text, node.StartTag),
        //        ConvertSequence(text, node.Content),
        //        ConvertNode(text, node.EndTag));
        //}

        //private static XmlUnknownNode ConvertUnknownNode(
        //    ImmutableArray<VirtualChar> text, SyntaxNode node)
        //{
        //    var children = ArrayBuilder<XmlNodeOrToken>.GetInstance();
        //    foreach (var child in node.ChildNodesAndTokens())
        //    {
        //        if (child.IsToken)
        //        {
        //            children.Add(ConvertToken(text, child.AsToken()));
        //        }
        //        else
        //        {
        //            children.Add(ConvertNode(text, child.AsNode()));
        //        }
        //    }

        //    return new XmlUnknownNode(children.ToImmutableAndFree());
        //}
    }
}
