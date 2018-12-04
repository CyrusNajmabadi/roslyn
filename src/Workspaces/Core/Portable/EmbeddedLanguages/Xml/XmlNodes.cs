// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.EmbeddedLanguages.Common;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml
{
    using XmlNodeOrToken = EmbeddedSyntaxNodeOrToken<XmlKind, XmlNode>;
    using XmlToken = EmbeddedSyntaxToken<XmlKind>;

    internal sealed class XmlCompilationUnit : XmlNode
    {
        public XmlCompilationUnit(XmlSequenceNode sequence, XmlToken endOfFileToken)
            : base(XmlKind.CompilationUnit)
        {
            Debug.Assert(sequence != null);
            Debug.Assert(endOfFileToken.Kind == XmlKind.EndOfFile);
            Sequence = sequence;
            EndOfFileToken = endOfFileToken;
        }

        public XmlSequenceNode Sequence { get; }
        public XmlToken EndOfFileToken { get; }

        internal override int ChildCount => 2;

        internal override XmlNodeOrToken ChildAt(int index)
        {
            switch (index)
            {
                case 0: return Sequence;
                case 1: return EndOfFileToken;
            }

            throw new InvalidOperationException();
        }

        public override void Accept(IXmlNodeVisitor visitor)
            => visitor.Visit(this);
    }

    /// <summary>
    /// Represents a possibly-empty sequence of xml nodes.
    /// 
    /// This does not deviate from Roslyn principles.  While nodes for empty text are rare, they
    /// are allowed (for example, OmittedTypeArgument in C#).
    /// </summary>
    internal sealed class XmlSequenceNode : XmlNode
    {
        public ImmutableArray<XmlNode> Children { get; }

        internal override int ChildCount => Children.Length;

        public XmlSequenceNode(ImmutableArray<XmlNode> children)
            : base(XmlKind.Sequence)
        {
            this.Children = children;
        }

        internal override XmlNodeOrToken ChildAt(int index)
            => Children[index];

        public override void Accept(IXmlNodeVisitor visitor)
            => visitor.Visit(this);
    }

    internal sealed class XmlUnknownNode : XmlNode
    {
        public ImmutableArray<XmlNodeOrToken> Children { get; }

        internal override int ChildCount => Children.Length;

        public XmlUnknownNode(ImmutableArray<XmlNodeOrToken> children)
            : base(XmlKind.Sequence)
        {
            this.Children = children;
        }

        internal override XmlNodeOrToken ChildAt(int index)
            => Children[index];

        public override void Accept(IXmlNodeVisitor visitor)
            => visitor.Visit(this);
    }

    //    /// <summary>
    //    /// Represents a chunk of text (usually just a single char) from the original pattern.
    //    /// </summary>
    //    internal sealed class XmlTextNode : XmlPrimaryExpressionNode
    //    {
    //        public XmlTextNode(XmlToken textToken)
    //            : base(XmlKind.Text)
    //        {
    //            Debug.Assert(textToken.Kind == XmlKind.TextToken);
    //            TextToken = textToken;
    //        }

    //        public XmlToken TextToken { get; }

    //        internal override int ChildCount => 1;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return TextToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Base type for [...] and [^...] character classes.
    //    /// </summary>
    //    internal abstract class XmlBaseCharacterClassNode : XmlPrimaryExpressionNode
    //    {
    //        protected XmlBaseCharacterClassNode(
    //            XmlKind kind, XmlToken openBracketToken, XmlSequenceNode components, XmlToken closeBracketToken)
    //            : base(kind)
    //        {
    //            Debug.Assert(openBracketToken.Kind == XmlKind.OpenBracketToken);
    //            Debug.Assert(components != null);
    //            Debug.Assert(closeBracketToken.Kind == XmlKind.CloseBracketToken);
    //            OpenBracketToken = openBracketToken;
    //            Components = components;
    //            CloseBracketToken = closeBracketToken;
    //        }

    //        public XmlToken OpenBracketToken { get; }
    //        public XmlSequenceNode Components { get; }
    //        public XmlToken CloseBracketToken { get; }
    //    }

    //    /// <summary>
    //    /// [...] node.
    //    /// </summary>
    //    internal sealed class XmlCharacterClassNode : XmlBaseCharacterClassNode
    //    {
    //        public XmlCharacterClassNode(
    //            XmlToken openBracketToken, XmlSequenceNode components, XmlToken closeBracketToken)
    //            : base(XmlKind.CharacterClass, openBracketToken, components, closeBracketToken)
    //        {
    //        }

    //        internal override int ChildCount => 3;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenBracketToken;
    //                case 1: return Components;
    //                case 2: return CloseBracketToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// [^...] node
    //    /// </summary>
    //    internal sealed class XmlNegatedCharacterClassNode : XmlBaseCharacterClassNode
    //    {
    //        public XmlNegatedCharacterClassNode(
    //            XmlToken openBracketToken, XmlToken caretToken, XmlSequenceNode components, XmlToken closeBracketToken)
    //            : base(XmlKind.NegatedCharacterClass, openBracketToken, components, closeBracketToken)
    //        {
    //            Debug.Assert(caretToken.Kind == XmlKind.CaretToken);
    //            CaretToken = caretToken;
    //        }

    //        public XmlToken CaretToken { get; }

    //        internal override int ChildCount => 4;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenBracketToken;
    //                case 1: return CaretToken;
    //                case 2: return Components;
    //                case 3: return CloseBracketToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```a-z``` node in a character class.
    //    /// </summary>
    //    internal sealed class XmlCharacterClassRangeNode : XmlPrimaryExpressionNode
    //    {
    //        public XmlCharacterClassRangeNode(
    //            XmlNode left, XmlToken minusToken, XmlNode right)
    //            : base(XmlKind.CharacterClassRange)
    //        {
    //            Debug.Assert(left != null);
    //            Debug.Assert(minusToken.Kind == XmlKind.MinusToken);
    //            Debug.Assert(right != null);
    //            Left = left;
    //            MinusToken = minusToken;
    //            Right = right;
    //        }

    //        public XmlNode Left { get; }
    //        public XmlToken MinusToken { get; }
    //        public XmlNode Right { get; }

    //        internal override int ChildCount => 3;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return Left;
    //                case 1: return MinusToken;
    //                case 2: return Right;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```-[f-m]``` in a pattern like ```[a-z-[f-m]]```.  A subtraction must come last in a 
    //    /// character class, and removes some range of chars from the claracter class built up
    //    /// so far.
    //    /// </summary>
    //    internal sealed class XmlCharacterClassSubtractionNode : XmlPrimaryExpressionNode
    //    {
    //        public XmlCharacterClassSubtractionNode(
    //            XmlToken minusToken, XmlBaseCharacterClassNode characterClass)
    //            : base(XmlKind.CharacterClassSubtraction)
    //        {
    //            Debug.Assert(minusToken.Kind == XmlKind.MinusToken);
    //            Debug.Assert(characterClass != null);
    //            MinusToken = minusToken;
    //            CharacterClass = characterClass;
    //        }

    //        public XmlToken MinusToken { get; }
    //        public XmlBaseCharacterClassNode CharacterClass { get; }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return MinusToken;
    //                case 1: return CharacterClass;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Represents a ```[:...:]``` node in a character class.  Note: the .net regex parser
    //    /// simply treats this as the character ```[``` and ignores the rest of the ```:...:]```.
    //    /// They latter part has no impact on the actual match engine that is produced.
    //    /// </summary>
    //    internal sealed class XmlPosixPropertyNode : XmlPrimaryExpressionNode
    //    {
    //        public XmlPosixPropertyNode(XmlToken textToken)
    //            : base(XmlKind.PosixProperty)
    //        {
    //            Debug.Assert(textToken.Kind == XmlKind.TextToken);
    //            TextToken = textToken;
    //        }

    //        public XmlToken TextToken { get; }

    //        internal override int ChildCount => 1;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return TextToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Root of all expression nodes.
    //    /// </summary>
    //    internal abstract class XmlNode : XmlNode
    //    {
    //        protected XmlNode(XmlKind kind)
    //            : base(kind)
    //        {
    //        }
    //    }

    //    /// <summary>
    //    /// Root of all the primary nodes (similar to unary nodes in C#).
    //    /// </summary>
    //    internal abstract class XmlPrimaryExpressionNode : XmlNode
    //    {
    //        protected XmlPrimaryExpressionNode(XmlKind kind)
    //            : base(kind)
    //        {
    //        }
    //    }

    //    /// <summary>
    //    /// A ```.``` expression.
    //    /// </summary>
    //    internal sealed class XmlWildcardNode : XmlPrimaryExpressionNode
    //    {
    //        public XmlWildcardNode(XmlToken dotToken)
    //            : base(XmlKind.Wildcard)
    //        {
    //            Debug.Assert(dotToken.Kind == XmlKind.DotToken);
    //            DotToken = dotToken;
    //        }

    //        public XmlToken DotToken { get; }

    //        internal override int ChildCount => 1;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return DotToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Root of all quantifier nodes: ```?```, ```*``` etc.
    //    /// </summary>
    //    internal abstract class XmlQuantifierNode : XmlNode
    //    {
    //        protected XmlQuantifierNode(XmlKind kind)
    //            : base(kind)
    //        {
    //        }
    //    }

    //    /// <summary>
    //    /// ```expr*```
    //    /// </summary>
    //    internal sealed class XmlZeroOrMoreQuantifierNode : XmlQuantifierNode
    //    {
    //        public XmlZeroOrMoreQuantifierNode(
    //            XmlNode expression, XmlToken asteriskToken)
    //            : base(XmlKind.ZeroOrMoreQuantifier)
    //        {
    //            Debug.Assert(expression != null);
    //            Debug.Assert(asteriskToken.Kind == XmlKind.AsteriskToken);
    //            Expression = expression;
    //            AsteriskToken = asteriskToken;
    //        }

    //        public XmlNode Expression { get; }
    //        public XmlToken AsteriskToken { get; }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return this.Expression;
    //                case 1: return this.AsteriskToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```expr+```
    //    /// </summary>
    //    internal sealed class XmlOneOrMoreQuantifierNode : XmlQuantifierNode
    //    {
    //        public XmlOneOrMoreQuantifierNode(
    //            XmlNode expression, XmlToken plusToken)
    //            : base(XmlKind.OneOrMoreQuantifier)
    //        {
    //            Debug.Assert(expression != null);
    //            Debug.Assert(plusToken.Kind == XmlKind.PlusToken);
    //            Expression = expression;
    //            PlusToken = plusToken;
    //        }

    //        public XmlNode Expression { get; }
    //        public XmlToken PlusToken { get; }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return this.Expression;
    //                case 1: return this.PlusToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```expr?```
    //    /// </summary>
    //    internal sealed class XmlZeroOrOneQuantifierNode : XmlQuantifierNode
    //    {
    //        public XmlZeroOrOneQuantifierNode(
    //            XmlNode expression, XmlToken questionToken)
    //            : base(XmlKind.ZeroOrOneQuantifier)
    //        {
    //            Debug.Assert(expression != null);
    //            Debug.Assert(questionToken.Kind == XmlKind.QuestionToken);
    //            Expression = expression;
    //            QuestionToken = questionToken;
    //        }

    //        public XmlNode Expression { get; }
    //        public XmlToken QuestionToken { get; }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return this.Expression;
    //                case 1: return this.QuestionToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Quantifiers can be optionally followed by a ? to make them lazy.  i.e. ```a*?``` or ```a+?```.
    //    /// You can even have ```a??```  (zero or one 'a', lazy).  However, only one lazy modifier is allowed
    //    /// ```a*??``` or ```a???``` is not allowed.
    //    /// </summary>
    //    internal sealed class XmlLazyQuantifierNode : XmlNode
    //    {
    //        public XmlLazyQuantifierNode(
    //            XmlQuantifierNode quantifier, XmlToken questionToken)
    //            : base(XmlKind.LazyQuantifier)
    //        {
    //            Debug.Assert(quantifier != null);
    //            Debug.Assert(quantifier.Kind != XmlKind.LazyQuantifier);
    //            Debug.Assert(questionToken.Kind == XmlKind.QuestionToken);
    //            Quantifier = quantifier;
    //            QuestionToken = questionToken;
    //        }

    //        public XmlQuantifierNode Quantifier { get; }
    //        public XmlToken QuestionToken { get; }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return this.Quantifier;
    //                case 1: return this.QuestionToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Base type of all regex numeric quantifier nodes.  i.e.  
    //    /// ```a{5}```,  ```a{5,}``` and ```a{5,10}```
    //    /// </summary>
    //    internal abstract class XmlNumericQuantifierNode : XmlQuantifierNode
    //    {
    //        protected XmlNumericQuantifierNode(
    //            XmlKind kind, XmlPrimaryExpressionNode expression, XmlToken openBraceToken, XmlToken firstNumberToken, XmlToken closeBraceToken)
    //            : base(kind)
    //        {
    //            Debug.Assert(expression != null);
    //            Debug.Assert(openBraceToken.Kind == XmlKind.OpenBraceToken);
    //            Debug.Assert(firstNumberToken.Kind == XmlKind.NumberToken);
    //            Debug.Assert(closeBraceToken.Kind == XmlKind.CloseBraceToken);
    //            Expression = expression;
    //            OpenBraceToken = openBraceToken;
    //            FirstNumberToken = firstNumberToken;
    //            CloseBraceToken = closeBraceToken;
    //        }

    //        public XmlNode Expression { get; }
    //        public XmlToken OpenBraceToken { get; }
    //        public XmlToken FirstNumberToken { get; }
    //        public XmlToken CloseBraceToken { get; }
    //    }

    //    /// <summary>
    //    /// ```a{5}```
    //    /// </summary>
    //    internal sealed class XmlExactNumericQuantifierNode : XmlNumericQuantifierNode
    //    {
    //        public XmlExactNumericQuantifierNode(
    //            XmlPrimaryExpressionNode expression, XmlToken openBraceToken, XmlToken numberToken, XmlToken closeBraceToken)
    //            : base(XmlKind.ExactNumericQuantifier, expression, openBraceToken, numberToken, closeBraceToken)
    //        {
    //        }

    //        internal override int ChildCount => 4;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return Expression;
    //                case 1: return OpenBraceToken;
    //                case 2: return FirstNumberToken;
    //                case 3: return CloseBraceToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```a{5,}```
    //    /// </summary>
    //    internal sealed class XmlOpenNumericRangeQuantifierNode : XmlNumericQuantifierNode
    //    {
    //        public XmlOpenNumericRangeQuantifierNode(
    //            XmlPrimaryExpressionNode expression,
    //            XmlToken openBraceToken, XmlToken firstNumberToken,
    //            XmlToken commaToken, XmlToken closeBraceToken)
    //            : base(XmlKind.OpenRangeNumericQuantifier, expression, openBraceToken, firstNumberToken, closeBraceToken)
    //        {
    //            Debug.Assert(commaToken.Kind == XmlKind.CommaToken);
    //            CommaToken = commaToken;
    //        }

    //        public XmlToken CommaToken { get; }

    //        internal override int ChildCount => 5;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return Expression;
    //                case 1: return OpenBraceToken;
    //                case 2: return FirstNumberToken;
    //                case 3: return CommaToken;
    //                case 4: return CloseBraceToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```a{5,10}```
    //    /// </summary>
    //    internal sealed class XmlClosedNumericRangeQuantifierNode : XmlNumericQuantifierNode
    //    {
    //        public XmlClosedNumericRangeQuantifierNode(
    //            XmlPrimaryExpressionNode expression,
    //            XmlToken openBraceToken, XmlToken firstNumberToken,
    //            XmlToken commaToken, XmlToken secondNumberToken, XmlToken closeBraceToken)
    //            : base(XmlKind.ClosedRangeNumericQuantifier, expression, openBraceToken, firstNumberToken, closeBraceToken)
    //        {
    //            Debug.Assert(commaToken.Kind == XmlKind.CommaToken);
    //            Debug.Assert(secondNumberToken.Kind == XmlKind.NumberToken);
    //            CommaToken = commaToken;
    //            SecondNumberToken = secondNumberToken;
    //        }

    //        public XmlToken CommaToken { get; }
    //        public XmlToken SecondNumberToken { get; }

    //        internal override int ChildCount => 6;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return Expression;
    //                case 1: return OpenBraceToken;
    //                case 2: return FirstNumberToken;
    //                case 3: return CommaToken;
    //                case 4: return SecondNumberToken;
    //                case 5: return CloseBraceToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```$``` or ```^```.
    //    /// </summary>
    //    internal sealed class XmlAnchorNode : XmlPrimaryExpressionNode
    //    {
    //        public XmlAnchorNode(XmlKind kind, XmlToken anchorToken)
    //            : base(kind)
    //        {
    //            Debug.Assert(anchorToken.Kind == XmlKind.DollarToken || anchorToken.Kind == XmlKind.CaretToken);
    //            AnchorToken = anchorToken;
    //        }

    //        public XmlToken AnchorToken { get; }

    //        internal override int ChildCount => 1;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return AnchorToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```expr1|expr2``` node.
    //    /// </summary>
    //    internal sealed class XmlAlternationNode : XmlNode
    //    {
    //        public XmlAlternationNode(
    //            XmlNode left, XmlToken barToken, XmlSequenceNode right)
    //            : base(XmlKind.Alternation)
    //        {
    //            Debug.Assert(left != null);
    //            Debug.Assert(barToken.Kind == XmlKind.BarToken);
    //            Debug.Assert(right != null);
    //            Left = left;
    //            BarToken = barToken;
    //            Right = right;
    //        }

    //        public XmlNode Left { get; }
    //        public XmlToken BarToken { get; }
    //        public XmlSequenceNode Right { get; }

    //        internal override int ChildCount => 3;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return Left;
    //                case 1: return BarToken;
    //                case 2: return Right;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Base type of all non-trivia ```(...)``` nodes
    //    /// </summary>
    //    internal abstract class XmlGroupingNode : XmlPrimaryExpressionNode
    //    {
    //        protected XmlGroupingNode(XmlKind kind, XmlToken openParenToken, XmlToken closeParenToken)
    //            : base(kind)
    //        {
    //            Debug.Assert(openParenToken.Kind == XmlKind.OpenParenToken);
    //            Debug.Assert(closeParenToken.Kind == XmlKind.CloseParenToken);
    //            OpenParenToken = openParenToken;
    //            CloseParenToken = closeParenToken;
    //        }

    //        public XmlToken OpenParenToken { get; }
    //        public XmlToken CloseParenToken { get; }
    //    }

    //    /// <summary>
    //    /// The ```(...)``` node you get when the group does not start with ```(?```
    //    /// </summary>
    //    internal sealed class XmlSimpleGroupingNode : XmlGroupingNode
    //    {
    //        public XmlSimpleGroupingNode(XmlToken openParenToken, XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.SimpleGrouping, openParenToken, closeParenToken)
    //        {
    //            Debug.Assert(expression != null);
    //            Expression = expression;
    //        }

    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 3;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return Expression;
    //                case 2: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Base type of all ```(?...)``` groupings.
    //    /// </summary>
    //    internal abstract class XmlQuestionGroupingNode : XmlGroupingNode
    //    {
    //        protected XmlQuestionGroupingNode(XmlKind kind, XmlToken openParenToken, XmlToken questionToken, XmlToken closeParenToken)
    //            : base(kind, openParenToken, closeParenToken)
    //        {
    //            Debug.Assert(questionToken.Kind == XmlKind.QuestionToken);
    //            QuestionToken = questionToken;
    //        }

    //        public XmlToken QuestionToken { get; }
    //    }

    //    /// <summary>
    //    /// Base type of ```(?inmsx)``` or ```(?inmsx:...)``` nodes.
    //    /// </summary>
    //    internal abstract class XmlOptionsGroupingNode : XmlQuestionGroupingNode
    //    {
    //        protected XmlOptionsGroupingNode(XmlKind kind, XmlToken openParenToken, XmlToken questionToken, XmlToken optionsToken, XmlToken closeParenToken)
    //            : base(kind, openParenToken, questionToken, closeParenToken)
    //        {
    //            OptionsToken = optionsToken;
    //        }

    //        public XmlToken OptionsToken { get; }
    //    }

    //    /// <summary>
    //    /// ```(?inmsx)``` node.  Changes options in a sequence for all subsequence nodes.
    //    /// </summary>
    //    internal sealed class XmlSimpleOptionsGroupingNode : XmlOptionsGroupingNode
    //    {
    //        public XmlSimpleOptionsGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken optionsToken, XmlToken closeParenToken)
    //            : base(XmlKind.SimpleOptionsGrouping, openParenToken, questionToken, optionsToken, closeParenToken)
    //        {
    //        }

    //        internal override int ChildCount => 4;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return OptionsToken;
    //                case 3: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?inmsx:expr)``` node.  Changes options for the parsing of 'expr'.
    //    /// </summary>
    //    internal sealed class XmlNestedOptionsGroupingNode : XmlOptionsGroupingNode
    //    {
    //        public XmlNestedOptionsGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken optionsToken,
    //            XmlToken colonToken, XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.NestedOptionsGrouping, openParenToken, questionToken, optionsToken, closeParenToken)
    //        {
    //            Debug.Assert(colonToken.Kind == XmlKind.ColonToken);
    //            Debug.Assert(expression != null);
    //            ColonToken = colonToken;
    //            Expression = expression;
    //        }

    //        public XmlToken ColonToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 6;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return OptionsToken;
    //                case 3: return ColonToken;
    //                case 4: return Expression;
    //                case 5: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?:expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlNonCapturingGroupingNode : XmlQuestionGroupingNode
    //    {
    //        public XmlNonCapturingGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken colonToken,
    //            XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.NonCapturingGrouping, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(colonToken.Kind == XmlKind.ColonToken);
    //            Debug.Assert(expression != null);
    //            ColonToken = colonToken;
    //            Expression = expression;
    //        }

    //        public XmlToken ColonToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 5;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return ColonToken;
    //                case 3: return Expression;
    //                case 4: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?=expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlPositiveLookaheadGroupingNode : XmlQuestionGroupingNode
    //    {
    //        public XmlPositiveLookaheadGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken equalsToken,
    //            XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.PositiveLookaheadGrouping, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(equalsToken.Kind == XmlKind.EqualsToken);
    //            Debug.Assert(expression != null);
    //            EqualsToken = equalsToken;
    //            Expression = expression;
    //        }

    //        public XmlToken EqualsToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 5;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return EqualsToken;
    //                case 3: return Expression;
    //                case 4: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?!expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlNegativeLookaheadGroupingNode : XmlQuestionGroupingNode
    //    {
    //        public XmlNegativeLookaheadGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken exclamationToken,
    //            XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.NegativeLookaheadGrouping, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(exclamationToken.Kind == XmlKind.ExclamationToken);
    //            Debug.Assert(expression != null);
    //            ExclamationToken = exclamationToken;
    //            Expression = expression;
    //        }

    //        public XmlToken ExclamationToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 5;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return ExclamationToken;
    //                case 3: return Expression;
    //                case 4: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    internal abstract class XmlLookbehindGroupingNode : XmlQuestionGroupingNode
    //    {
    //        protected XmlLookbehindGroupingNode(
    //            XmlKind kind, XmlToken openParenToken, XmlToken questionToken,
    //            XmlToken lessThanToken, XmlToken closeParenToken)
    //            : base(kind, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(lessThanToken.Kind == XmlKind.LessThanToken);
    //            LessThanToken = lessThanToken;
    //        }

    //        public XmlToken LessThanToken { get; }
    //    }

    //    /// <summary>
    //    /// ```(?&lt;=expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlPositiveLookbehindGroupingNode : XmlLookbehindGroupingNode
    //    {
    //        public XmlPositiveLookbehindGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken lessThanToken,
    //            XmlToken equalsToken, XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.PositiveLookbehindGrouping, openParenToken, questionToken, lessThanToken, closeParenToken)
    //        {
    //            Debug.Assert(equalsToken.Kind == XmlKind.EqualsToken);
    //            Debug.Assert(expression != null);
    //            EqualsToken = equalsToken;
    //            Expression = expression;
    //        }

    //        public XmlToken EqualsToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 6;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return LessThanToken;
    //                case 3: return EqualsToken;
    //                case 4: return Expression;
    //                case 5: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?&lt;!expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlNegativeLookbehindGroupingNode : XmlLookbehindGroupingNode
    //    {
    //        public XmlNegativeLookbehindGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken lessThanToken,
    //            XmlToken exclamationToken, XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.NegativeLookbehindGrouping, openParenToken, questionToken, lessThanToken, closeParenToken)
    //        {
    //            Debug.Assert(exclamationToken.Kind == XmlKind.ExclamationToken);
    //            Debug.Assert(expression != null);
    //            ExclamationToken = exclamationToken;
    //            Expression = expression;
    //        }

    //        public XmlToken ExclamationToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 6;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return LessThanToken;
    //                case 3: return ExclamationToken;
    //                case 4: return Expression;
    //                case 5: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?&gt;expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlNonBacktrackingGroupingNode : XmlQuestionGroupingNode
    //    {
    //        public XmlNonBacktrackingGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken greaterThanToken,
    //            XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.NonBacktrackingGrouping, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(greaterThanToken.Kind == XmlKind.GreaterThanToken);
    //            Debug.Assert(expression != null);
    //            GreaterThanToken = greaterThanToken;
    //            Expression = expression;
    //        }

    //        public XmlToken GreaterThanToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 5;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return GreaterThanToken;
    //                case 3: return Expression;
    //                case 4: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?'name'expr)``` or ```(?&lt;name&gt;expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlCaptureGroupingNode : XmlQuestionGroupingNode
    //    {
    //        public XmlCaptureGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken openToken,
    //            XmlToken captureToken, XmlToken closeToken,
    //            XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.CaptureGrouping, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(expression != null);
    //            OpenToken = openToken;
    //            CaptureToken = captureToken;
    //            CloseToken = closeToken;
    //            Expression = expression;
    //        }

    //        public XmlToken OpenToken { get; }
    //        public XmlToken CaptureToken { get; }
    //        public XmlToken CloseToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 7;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return OpenToken;
    //                case 3: return CaptureToken;
    //                case 4: return CloseToken;
    //                case 5: return Expression;
    //                case 6: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?'name1-name2'expr)``` or ```(?&lt;name1-name2&gt;expr)``` node.
    //    /// </summary>
    //    internal sealed class XmlBalancingGroupingNode : XmlQuestionGroupingNode
    //    {
    //        public XmlBalancingGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken, XmlToken openToken,
    //            XmlToken firstCaptureToken, XmlToken minusToken, XmlToken secondCaptureToken,
    //            XmlToken closeToken, XmlNode expression, XmlToken closeParenToken)
    //            : base(XmlKind.BalancingGrouping, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(minusToken.Kind == XmlKind.MinusToken);
    //            Debug.Assert(expression != null);
    //            OpenToken = openToken;
    //            FirstCaptureToken = firstCaptureToken;
    //            MinusToken = minusToken;
    //            SecondCaptureToken = secondCaptureToken;
    //            CloseToken = closeToken;
    //            Expression = expression;
    //        }

    //        public XmlToken OpenToken { get; }
    //        public XmlToken FirstCaptureToken { get; }
    //        public XmlToken MinusToken { get; }
    //        public XmlToken SecondCaptureToken { get; }
    //        public XmlToken CloseToken { get; }
    //        public XmlNode Expression { get; }

    //        internal override int ChildCount => 9;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return OpenToken;
    //                case 3: return FirstCaptureToken;
    //                case 4: return MinusToken;
    //                case 5: return SecondCaptureToken;
    //                case 6: return CloseToken;
    //                case 7: return Expression;
    //                case 8: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    internal abstract class XmlConditionalGroupingNode : XmlQuestionGroupingNode
    //    {
    //        protected XmlConditionalGroupingNode(
    //            XmlKind kind, XmlToken openParenToken, XmlToken questionToken,
    //            XmlNode result, XmlToken closeParenToken)
    //            : base(kind, openParenToken, questionToken, closeParenToken)
    //        {
    //            Debug.Assert(result != null);
    //            Result = result;
    //        }

    //        public XmlNode Result { get; }
    //    }

    //    /// <summary>
    //    /// ```(?(capture_name)result)```
    //    /// </summary>
    //    internal sealed class XmlConditionalCaptureGroupingNode : XmlConditionalGroupingNode
    //    {
    //        public XmlConditionalCaptureGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken,
    //            XmlToken innerOpenParenToken, XmlToken captureToken, XmlToken innerCloseParenToken,
    //            XmlNode result, XmlToken closeParenToken)
    //            : base(XmlKind.ConditionalCaptureGrouping, openParenToken, questionToken, result, closeParenToken)
    //        {
    //            Debug.Assert(innerOpenParenToken.Kind == XmlKind.OpenParenToken);
    //            Debug.Assert(innerCloseParenToken.Kind == XmlKind.CloseParenToken);
    //            InnerOpenParenToken = innerOpenParenToken;
    //            CaptureToken = captureToken;
    //            InnerCloseParenToken = innerCloseParenToken;
    //        }

    //        public XmlToken InnerOpenParenToken { get; }
    //        public XmlToken CaptureToken { get; }
    //        public XmlToken InnerCloseParenToken { get; }

    //        internal override int ChildCount => 7;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return InnerOpenParenToken;
    //                case 3: return CaptureToken;
    //                case 4: return InnerCloseParenToken;
    //                case 5: return Result;
    //                case 6: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```(?(group)result)```
    //    /// </summary>
    //    internal sealed class XmlConditionalExpressionGroupingNode : XmlConditionalGroupingNode
    //    {
    //        public XmlConditionalExpressionGroupingNode(
    //            XmlToken openParenToken, XmlToken questionToken,
    //            XmlGroupingNode grouping,
    //            XmlNode result, XmlToken closeParenToken)
    //            : base(XmlKind.ConditionalExpressionGrouping, openParenToken, questionToken, result, closeParenToken)
    //        {
    //            Debug.Assert(grouping != null);
    //            Grouping = grouping;
    //        }

    //        internal override int ChildCount => 5;

    //        public XmlGroupingNode Grouping { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return OpenParenToken;
    //                case 1: return QuestionToken;
    //                case 2: return Grouping;
    //                case 3: return Result;
    //                case 4: return CloseParenToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// Base type of all regex primitives that start with \
    //    /// </summary>
    //    internal abstract class XmlEscapeNode : XmlPrimaryExpressionNode
    //    {
    //        protected XmlEscapeNode(XmlKind kind, XmlToken backslashToken) : base(kind)
    //        {
    //            Debug.Assert(backslashToken.Kind == XmlKind.BackslashToken);
    //            BackslashToken = backslashToken;
    //        }

    //        public XmlToken BackslashToken { get; }
    //    }

    //    /// <summary>
    //    /// Base type of all regex escapes that start with \ and some informative character (like \v \t \c etc.).
    //    /// </summary>
    //    internal abstract class XmlTypeEscapeNode : XmlEscapeNode
    //    {
    //        protected XmlTypeEscapeNode(XmlKind kind, XmlToken backslashToken, XmlToken typeToken)
    //            : base(kind, backslashToken)
    //        {
    //            TypeToken = typeToken;
    //        }

    //        public XmlToken TypeToken { get; }
    //    }

    //    /// <summary>
    //    /// A basic escape that just has \ and one additional character and needs no further information.
    //    /// </summary>
    //    internal sealed class XmlSimpleEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlSimpleEscapeNode(XmlToken backslashToken, XmlToken typeToken)
    //            : base(XmlKind.SimpleEscape, backslashToken, typeToken)
    //        {
    //            Debug.Assert(typeToken.Kind == XmlKind.TextToken);
    //        }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// One of \b \B \A \G \z \Z
    //    /// </summary>
    //    internal sealed class XmlAnchorEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlAnchorEscapeNode(XmlToken backslashToken, XmlToken typeToken)
    //            : base(XmlKind.AnchorEscape, backslashToken, typeToken)
    //        {
    //        }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// One of \s \S \d \D \w \W
    //    /// </summary>
    //    internal sealed class XmlCharacterClassEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlCharacterClassEscapeNode(XmlToken backslashToken, XmlToken typeToken)
    //            : base(XmlKind.CharacterClassEscape, backslashToken, typeToken)
    //        {
    //        }

    //        internal override int ChildCount => 2;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\cX``` escape
    //    /// </summary>
    //    internal sealed class XmlControlEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlControlEscapeNode(XmlToken backslashToken, XmlToken typeToken, XmlToken controlToken)
    //            : base(XmlKind.ControlEscape, backslashToken, typeToken)
    //        {
    //            ControlToken = controlToken;
    //        }

    //        internal override int ChildCount => 3;

    //        public XmlToken ControlToken { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //                case 2: return ControlToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\xFF``` escape.
    //    /// </summary>
    //    internal sealed class XmlHexEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlHexEscapeNode(XmlToken backslashToken, XmlToken typeToken, XmlToken hexText)
    //            : base(XmlKind.HexEscape, backslashToken, typeToken)
    //        {
    //            HexText = hexText;
    //        }

    //        internal override int ChildCount => 3;

    //        public XmlToken HexText { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //                case 2: return HexText;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\uFFFF``` escape.
    //    /// </summary>
    //    internal sealed class XmlUnicodeEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlUnicodeEscapeNode(XmlToken backslashToken, XmlToken typeToken, XmlToken hexText)
    //            : base(XmlKind.UnicodeEscape, backslashToken, typeToken)
    //        {
    //            HexText = hexText;
    //        }

    //        internal override int ChildCount => 3;

    //        public XmlToken HexText { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //                case 2: return HexText;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\'name'``` or ```\&lt;name&gt;``` escape.
    //    /// </summary>
    //    internal sealed class XmlCaptureEscapeNode : XmlEscapeNode
    //    {
    //        public XmlCaptureEscapeNode(
    //            XmlToken backslashToken, XmlToken openToken, XmlToken captureToken, XmlToken closeToken)
    //            : base(XmlKind.CaptureEscape, backslashToken)
    //        {
    //            OpenToken = openToken;
    //            CaptureToken = captureToken;
    //            CloseToken = closeToken;
    //        }

    //        internal override int ChildCount => 4;

    //        public XmlToken OpenToken { get; }
    //        public XmlToken CaptureToken { get; }
    //        public XmlToken CloseToken { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return OpenToken;
    //                case 2: return CaptureToken;
    //                case 3: return CloseToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\k'name'``` or ```\k&lt;name&gt;``` escape.
    //    /// </summary>
    //    internal sealed class XmlKCaptureEscapeNode : XmlTypeEscapeNode
    //    {
    //        public XmlKCaptureEscapeNode(
    //            XmlToken backslashToken, XmlToken typeToken,
    //            XmlToken openToken, XmlToken captureToken, XmlToken closeToken)
    //            : base(XmlKind.KCaptureEscape, backslashToken, typeToken)
    //        {
    //            OpenToken = openToken;
    //            CaptureToken = captureToken;
    //            CloseToken = closeToken;
    //        }

    //        internal override int ChildCount => 5;

    //        public XmlToken OpenToken { get; }
    //        public XmlToken CaptureToken { get; }
    //        public XmlToken CloseToken { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //                case 2: return OpenToken;
    //                case 3: return CaptureToken;
    //                case 4: return CloseToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\1``` escape. In contexts where back-references are not allowed.
    //    /// </summary>
    //    internal sealed class XmlOctalEscapeNode : XmlEscapeNode
    //    {
    //        public XmlOctalEscapeNode(XmlToken backslashToken, XmlToken octalText)
    //            : base(XmlKind.OctalEscape, backslashToken)
    //        {
    //            OctalText = octalText;
    //        }

    //        internal override int ChildCount => 2;

    //        public XmlToken OctalText { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return OctalText;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\1```
    //    /// </summary>
    //    internal sealed class XmlBackreferenceEscapeNode : XmlEscapeNode
    //    {
    //        public XmlBackreferenceEscapeNode(XmlToken backslashToken, XmlToken numberToken)
    //            : base(XmlKind.BackreferenceEscape, backslashToken)
    //        {
    //            NumberToken = numberToken;
    //        }

    //        internal override int ChildCount => 2;

    //        public XmlToken NumberToken { get; }

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return NumberToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }

    //    /// <summary>
    //    /// ```\p{...}```
    //    /// </summary>
    //    internal sealed class XmlCategoryEscapeNode : XmlEscapeNode
    //    {
    //        public XmlCategoryEscapeNode(
    //            XmlToken backslashToken, XmlToken typeToken, XmlToken openBraceToken, XmlToken categoryToken, XmlToken closeBraceToken)
    //            : base(XmlKind.CategoryEscape, backslashToken)
    //        {
    //            Debug.Assert(openBraceToken.Kind == XmlKind.OpenBraceToken);
    //            Debug.Assert(closeBraceToken.Kind == XmlKind.CloseBraceToken);
    //            TypeToken = typeToken;
    //            OpenBraceToken = openBraceToken;
    //            CategoryToken = categoryToken;
    //            CloseBraceToken = closeBraceToken;
    //        }

    //        public XmlToken TypeToken { get; }
    //        public XmlToken OpenBraceToken { get; }
    //        public XmlToken CategoryToken { get; }
    //        public XmlToken CloseBraceToken { get; }

    //        internal override int ChildCount => 5;

    //        internal override XmlNodeOrToken ChildAt(int index)
    //        {
    //            switch (index)
    //            {
    //                case 0: return BackslashToken;
    //                case 1: return TypeToken;
    //                case 2: return OpenBraceToken;
    //                case 3: return CategoryToken;
    //                case 4: return CloseBraceToken;
    //            }

    //            throw new InvalidOperationException();
    //        }

    //        public override void Accept(IXmlNodeVisitor visitor)
    //            => visitor.Visit(this);
    //    }
}
