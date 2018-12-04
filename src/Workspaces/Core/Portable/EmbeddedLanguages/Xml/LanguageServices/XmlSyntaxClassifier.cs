// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Classification;
using Microsoft.CodeAnalysis.Classification.Classifiers;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Common;
using Microsoft.CodeAnalysis.EmbeddedLanguages.LanguageServices;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml.LanguageServices
{
    using static EmbeddedSyntaxHelpers;

    using XmlToken = EmbeddedSyntaxToken<XmlKind>;
    using XmlTrivia = EmbeddedSyntaxTrivia<XmlKind>;

    /// <summary>
    /// Classifier impl for embedded regex strings.
    /// </summary>
    internal sealed class XmlSyntaxClassifier : AbstractSyntaxClassifier
    {
        private static ObjectPool<Visitor> _visitorPool = new ObjectPool<Visitor>(() => new Visitor());

        private readonly EmbeddedLanguageInfo _info;

        public override ImmutableArray<int> SyntaxTokenKinds { get; }

        public XmlSyntaxClassifier(EmbeddedLanguageInfo info)
        {
            _info = info;
            SyntaxTokenKinds = ImmutableArray.Create(info.StringLiteralTokenKind);
        }

        public override void AddClassifications(
            Workspace workspace, SyntaxToken token, SemanticModel semanticModel,
            ArrayBuilder<ClassifiedSpan> result, CancellationToken cancellationToken)
        {
            if (_info.StringLiteralTokenKind != token.RawKind)
            {
                return;
            }

            if (!workspace.Options.GetOption(XmlFeatureOptions.ColorizeXmlPatterns, semanticModel.Language))
            {
                return;
            }

            // Do some quick syntactic checks before doing any complex work.
            if (XmlPatternDetector.IsDefinitelyNotPattern(token, _info.SyntaxFacts))
            {
                return;
            }

            var detector = XmlPatternDetector.TryGetOrCreate(workspace, semanticModel, _info);
            var tree = detector?.TryParseXmlPattern(token, cancellationToken);
            if (tree == null)
            {
                return;
            }

            var visitor = _visitorPool.Allocate();
            try
            {
                visitor.Result = result;
                AddClassifications(tree.Root, visitor, result);
            }
            finally
            {
                visitor.Result = null;
                _visitorPool.Free(visitor);
            }
        }

        private static void AddClassifications(XmlNode node, Visitor visitor, ArrayBuilder<ClassifiedSpan> result)
        {
            node.Accept(visitor);

            foreach (var child in node)
            {
                if (child.IsNode)
                {
                    AddClassifications(child.Node, visitor, result);
                }
                else
                {
                    AddTriviaClassifications(child.Token, result);
                }
            }
        }

        private static void AddTriviaClassifications(XmlToken token, ArrayBuilder<ClassifiedSpan> result)
        {
            foreach (var trivia in token.LeadingTrivia)
            {
                AddTriviaClassifications(trivia, result);
            }
        }

        private static void AddTriviaClassifications(XmlTrivia trivia, ArrayBuilder<ClassifiedSpan> result)
        {
            //if (trivia.Kind == XmlKind.CommentTrivia &&
            //    trivia.VirtualChars.Length > 0)
            //{
            //    result.Add(new ClassifiedSpan(
            //        ClassificationTypeNames.XmlComment, GetSpan(trivia.VirtualChars)));
            //}
        }

        private class Visitor : IXmlNodeVisitor
        {
            public ArrayBuilder<ClassifiedSpan> Result;

            private void AddClassification(XmlToken token, string typeName)
            {
                if (!token.IsMissing)
                {
                    Result.Add(new ClassifiedSpan(typeName, token.GetSpan()));
                }
            }

            private void ClassifyWholeNode(XmlNode node, string typeName)
            {
                foreach (var child in node)
                {
                    if (child.IsNode)
                    {
                        ClassifyWholeNode(child.Node, typeName);
                    }
                    else
                    {
                        AddClassification(child.Token, typeName);
                    }
                }
            }

            public void Visit(XmlCompilationUnit node)
            {
                // Nothing to highlight.
            }

            public void Visit(XmlSequenceNode node)
            {
                // Nothing to highlight.   
            }

            public void Visit(XmlUnknownNode node)
            {
                throw new System.NotImplementedException();
            }

            // #region Character classes

            //public void Visit(XmlWildcardNode node)
            //    => AddClassification(node.DotToken, ClassificationTypeNames.XmlCharacterClass);

            //public void Visit(XmlCharacterClassNode node)
            //{
            //    AddClassification(node.OpenBracketToken, ClassificationTypeNames.XmlCharacterClass);
            //    AddClassification(node.CloseBracketToken, ClassificationTypeNames.XmlCharacterClass);
            //}

            //public void Visit(XmlNegatedCharacterClassNode node)
            //{
            //    AddClassification(node.OpenBracketToken, ClassificationTypeNames.XmlCharacterClass);
            //    AddClassification(node.CaretToken, ClassificationTypeNames.XmlCharacterClass);
            //    AddClassification(node.CloseBracketToken, ClassificationTypeNames.XmlCharacterClass);
            //}

            //public void Visit(XmlCharacterClassRangeNode node)
            //    => AddClassification(node.MinusToken, ClassificationTypeNames.XmlCharacterClass);

            //public void Visit(XmlCharacterClassSubtractionNode node)
            //    => AddClassification(node.MinusToken, ClassificationTypeNames.XmlCharacterClass);

            //public void Visit(XmlCharacterClassEscapeNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlCharacterClass);

            //public void Visit(XmlCategoryEscapeNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlCharacterClass);

            //#endregion

            //#region Quantifiers

            //public void Visit(XmlZeroOrMoreQuantifierNode node)
            //    => AddClassification(node.AsteriskToken, ClassificationTypeNames.XmlQuantifier);

            //public void Visit(XmlOneOrMoreQuantifierNode node)
            //    => AddClassification(node.PlusToken, ClassificationTypeNames.XmlQuantifier);

            //public void Visit(XmlZeroOrOneQuantifierNode node)
            //    => AddClassification(node.QuestionToken, ClassificationTypeNames.XmlQuantifier);

            //public void Visit(XmlLazyQuantifierNode node)
            //    => AddClassification(node.QuestionToken, ClassificationTypeNames.XmlQuantifier);

            //public void Visit(XmlExactNumericQuantifierNode node)
            //{
            //    AddClassification(node.OpenBraceToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.FirstNumberToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.CloseBraceToken, ClassificationTypeNames.XmlQuantifier);
            //}

            //public void Visit(XmlOpenNumericRangeQuantifierNode node)
            //{
            //    AddClassification(node.OpenBraceToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.FirstNumberToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.CommaToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.CloseBraceToken, ClassificationTypeNames.XmlQuantifier);
            //}

            //public void Visit(XmlClosedNumericRangeQuantifierNode node)
            //{
            //    AddClassification(node.OpenBraceToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.FirstNumberToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.CommaToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.SecondNumberToken, ClassificationTypeNames.XmlQuantifier);
            //    AddClassification(node.CloseBraceToken, ClassificationTypeNames.XmlQuantifier);
            //}

            //#endregion

            //#region Groupings

            //public void Visit(XmlSimpleGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlSimpleOptionsGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlNestedOptionsGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlNonCapturingGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlPositiveLookaheadGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlNegativeLookaheadGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlPositiveLookbehindGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlNegativeLookbehindGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlNonBacktrackingGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlCaptureGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlBalancingGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlConditionalCaptureGroupingNode node)
            //    => ClassifyGrouping(node);

            //public void Visit(XmlConditionalExpressionGroupingNode node)
            //    => ClassifyGrouping(node);

            //// Captures and backreferences refer to groups.  So we classify them the same way as groups.
            //public void Visit(XmlCaptureEscapeNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlGrouping);

            //public void Visit(XmlKCaptureEscapeNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlGrouping);

            //public void Visit(XmlBackreferenceEscapeNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlGrouping);

            //private void ClassifyGrouping(XmlGroupingNode node)
            //{
            //    foreach (var child in node)
            //    {
            //        if (!child.IsNode)
            //        {
            //            AddClassification(child.Token, ClassificationTypeNames.XmlGrouping);
            //        }
            //    }
            //}

            //#endregion

            //#region Other Escapes

            //public void Visit(XmlControlEscapeNode node)
            //    => ClassifyOtherEscape(node);

            //public void Visit(XmlHexEscapeNode node)
            //    => ClassifyOtherEscape(node);

            //public void Visit(XmlUnicodeEscapeNode node)
            //    => ClassifyOtherEscape(node);

            //public void Visit(XmlOctalEscapeNode node)
            //    => ClassifyOtherEscape(node);

            //public void ClassifyOtherEscape(XmlNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlOtherEscape);

            //#endregion 

            //#region Anchors

            //public void Visit(XmlAnchorNode node)
            //    => AddClassification(node.AnchorToken, ClassificationTypeNames.XmlAnchor);

            //public void Visit(XmlAnchorEscapeNode node)
            //    => ClassifyWholeNode(node, ClassificationTypeNames.XmlAnchor);

            //#endregion

            //public void Visit(XmlTextNode node)
            //    => AddClassification(node.TextToken, ClassificationTypeNames.XmlText);

            //public void Visit(XmlPosixPropertyNode node)
            //{
            //    // The .net parser just interprets the [ of the node, and skips the rest. So
            //    // classify the end part as a comment.
            //    Result.Add(new ClassifiedSpan(node.TextToken.VirtualChars[0].Span, ClassificationTypeNames.XmlText));
            //    Result.Add(new ClassifiedSpan(
            //        GetSpan(node.TextToken.VirtualChars[1], node.TextToken.VirtualChars.Last()),
            //        ClassificationTypeNames.XmlComment));
            //}

            //public void Visit(XmlAlternationNode node)
            //    => AddClassification(node.BarToken, ClassificationTypeNames.XmlAlternation);

            //public void Visit(XmlSimpleEscapeNode node)
            //    => ClassifyWholeNode(node, node.IsSelfEscape()
            //        ? ClassificationTypeNames.XmlSelfEscapedCharacter
            //        : ClassificationTypeNames.XmlOtherEscape);
        }
    }
}
