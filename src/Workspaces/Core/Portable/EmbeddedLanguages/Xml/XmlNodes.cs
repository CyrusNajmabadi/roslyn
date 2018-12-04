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

    internal sealed class XmlElementNode : XmlNode
    {
        public XmlElementNode(XmlElementStartTagNode startTag, XmlSequenceNode content, XmlElementEndTagNode endTag)
            : base(XmlKind.CompilationUnit)
        {
            StartTag = startTag;
            Content = content;
            EndTag = endTag;
        }

        internal override int ChildCount => 2;

        public XmlElementStartTagNode StartTag { get; }
        public XmlSequenceNode Content { get; }
        public XmlElementEndTagNode EndTag { get; }

        internal override XmlNodeOrToken ChildAt(int index)
        {
            switch (index)
            {
                case 0: return StartTag;
                case 1: return Content;
                case 2: return EndTag;
            }

            throw new InvalidOperationException();
        }

        public override void Accept(IXmlNodeVisitor visitor)
            => visitor.Visit(this);
    }

    internal sealed class XmlElementStartTagNode : XmlNode
    {
        public XmlElementNode(XmlStartStartNode startTag, XmlSequenceNode content, XmlEndTagNode endTag)
            : base(XmlKind.CompilationUnit)
        {
            StartTag = startTag;
            Content = content;
            EndTag = endTag;
        }

        internal override int ChildCount => 2;

        public XmlStartStartNode StartTag { get; }
        public XmlSequenceNode Content { get; }
        public XmlEndTagNode EndTag { get; }

        internal override XmlNodeOrToken ChildAt(int index)
        {
            switch (index)
            {
                case 0: return StartTag;
                case 1: return Content;
                case 2: return EndTag;
            }

            throw new InvalidOperationException();
        }

        public override void Accept(IXmlNodeVisitor visitor)
            => visitor.Visit(this);
    }
}
