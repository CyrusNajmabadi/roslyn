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

            var str = text.CreateString();
            var trivia = SyntaxFactory.ParseDocumentationCommentTrivia(str, CSharpParseOptions.Default);
            return new XmlTree(
                text, Convert(text, trivia),
                ConvertDiagnostics(trivia.GetDiagnostics(), text));
        }

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

        private static XmlSequenceNode ConvertSequence(ImmutableArray<VirtualChar> text, SyntaxList<XmlNodeSyntax> content)
        {
            var result = ArrayBuilder<XmlNode>.GetInstance();
            foreach (var node in content)
            {
                result.Add(ConvertNode(text, node));
            }

            return new XmlSequenceNode(result.ToImmutableAndFree());
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

        private static XmlNode ConvertNode(
            ImmutableArray<VirtualChar> text, SyntaxNode node)
        {
            switch (node.Kind())
            {
                default:
                    Debug.Fail("Unknown node kind");
                    return ConvertUnknownNode(text, node);
            }
        }

        private static XmlUnknownNode ConvertUnknownNode(
            ImmutableArray<VirtualChar> text, SyntaxNode node)
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

            return new XmlUnknownNode(children.ToImmutableAndFree());
        }
    }
}
