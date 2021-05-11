// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    internal class CSharpSyntaxNodeVisitor : CSharpSyntaxVisitor
    {
        /// <summary>
        /// Callback action.
        /// </summary>
        private readonly Action<INodeInfo> onNodeFound;

        /// <summary>
        /// Token that can be used to cancel the visitor as we're walking the tree.
        /// </summary>
        private readonly CancellationToken cancellationToken;

        /// <summary>
        /// Creates the visitor.
        /// </summary>
        /// <param name="onNodeFound">Callback for when we find a node</param>
        /// <param name="cancellationToken">A cancellation token that can stop the walk before it finishes</param>
        public CSharpSyntaxNodeVisitor(Action<INodeInfo> onNodeFound, CancellationToken cancellationToken)
        {
            ArgumentValidation.NotNull(onNodeFound, "onNodeFound");

            this.onNodeFound = onNodeFound;
            this.cancellationToken = cancellationToken;
        }

        /// <summary>
        /// Visits a compilation node.
        /// </summary>
        /// <param name="node">The complication (root) node</param>
        public override void VisitCompilationUnit(CompilationUnitSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        /// <summary>
        /// Visits a namespace declaration.
        /// </summary>
        /// <param name="node">The namespace declaration node</param>
        public override void VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        private void VisitTypeDeclaration(TypeDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node, SyntaxNodeKind.Type, node.Identifier);
            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            => VisitTypeDeclaration(node);

        public override void VisitRecordDeclaration(RecordDeclarationSyntax node)
            => VisitTypeDeclaration(node);

        public override void VisitStructDeclaration(StructDeclarationSyntax node)
            => VisitTypeDeclaration(node);

        public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
            => VisitTypeDeclaration(node);

        public override void VisitEnumDeclaration(EnumDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Type, node.Identifier);
        }

        /// <summary>
        /// Visits a method declaration.
        /// </summary>
        /// <param name="node">The method declaration</param>
        public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.Identifier);
        }

        /// <summary>
        /// The constructor declaration.
        /// </summary>
        /// <param name="node">The constructor declaration</param>
        public override void VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.Identifier);
        }

        /// <summary>
        /// Visits a destructor declaration.
        /// </summary>
        /// <param name="node">The descructor declaration</param>
        public override void VisitDestructorDeclaration(DestructorDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.TildeToken);
        }

        /// <summary>
        /// Visits an operator declaration.
        /// </summary>
        /// <param name="node">The operator declaration</param>
        public override void VisitOperatorDeclaration(OperatorDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.OperatorKeyword);
        }

        /// <summary>
        /// Visits a property declaration.
        /// </summary>
        /// <param name="node">The property declaration</param>
        public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Property, node.Identifier);
        }

        /// <summary>
        /// Visits a indexer
        /// </summary>
        public override void VisitIndexerDeclaration(IndexerDeclarationSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");
            this.FireOnNodeFound(node, SyntaxNodeKind.Property, node.ThisKeyword);
        }

        /// <summary>
        /// Override the default visit method to throw if we're cancelled.
        /// </summary>
        /// <param name="node">The syntax node</param>
        public override void Visit(SyntaxNode node)
        {
            this.cancellationToken.ThrowIfCancellationRequested();
            base.Visit(node);
        }

        /// <summary>
        /// Validates node state prior to firing OnNodeFound
        /// </summary>
        /// <param name="node">Syntax Node</param>
        /// <param name="kind">Node Kind</param>
        /// <param name="identifier">Identifier token of the node</param>
        private void FireOnNodeFound(SyntaxNode node, SyntaxNodeKind kind, SyntaxToken identifier)
        {
            if (identifier.Span.Length > 0)
            {
                this.onNodeFound(new CSharpNodeInfo(node, kind, identifier));
            }
        }

        /// <summary>
        /// This class is returned to callers when we find a new node.
        /// </summary>
        private class CSharpNodeInfo : INodeInfo
        {
            private readonly SyntaxNode node;
            private readonly SyntaxNodeKind kind;
            private int startPosition = 0;

            /// <summary>
            /// Creates the CSharp node found info object.
            /// </summary>
            /// <param name="node">The node that was found</param>
            /// <param name="kind">The kind of node</param>
            /// <param name="identifier">The token in the node on which the tag should be created</param>
            public CSharpNodeInfo(SyntaxNode node, SyntaxNodeKind kind, SyntaxToken identifier)
            {
                this.node = node;
                this.kind = kind;

                if (identifier == default(SyntaxToken))
                {
                    this.startPosition = node.Span.Start;
                }
                else
                {
                    var lines = identifier.SyntaxTree.GetText().Lines;
                    var line = lines.GetLineFromPosition(identifier.Span.Start);

                    // check if start of line is within current node
                    this.startPosition = node.FullSpan.Contains(line.Start) ? node.FindToken(line.Start).Span.Start : node.Span.Start;
                }
            }

            /// <summary>
            /// Returns the starting position of the node.
            /// </summary>
            public int Start
            {
                get
                {
                    return this.startPosition;
                }
            }

            /// <summary>
            /// Returns a new syntax node info object for this found node.
            /// </summary>
            /// <param name="snapshot">The text snapshot that the locator uses to translate positions between versions</param>
            public SyntaxNodeInfo CreateSyntaxNodeInfo(ITextSnapshot snapshot)
            {
                return new SyntaxNodeInfo(this.node, this.kind, new SyntaxNodeTracker(this.node, snapshot, this.startPosition));
            }
        }
    }
}
