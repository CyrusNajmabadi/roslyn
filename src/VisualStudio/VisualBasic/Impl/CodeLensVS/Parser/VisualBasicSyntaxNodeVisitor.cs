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
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    internal class VisualBasicSyntaxNodeVisitor : VisualBasicSyntaxVisitor
    {
        /// <summary>
        /// Callback action.
        /// </summary>
        private readonly Action<INodeInfo> onNodeFound;

        /// <summary>
        /// A cancellation token that we use to stop the walk if we're not finished.
        /// </summary>
        private readonly CancellationToken cancellationToken;

        /// <summary>
        /// Creates the visitor.
        /// </summary>
        /// <param name="onNodeFound">Callback for when we find a node</param>
        /// <param name="cancellationToken">A cancellation token that can stop the walk before it finishes</param>
        public VisualBasicSyntaxNodeVisitor(Action<INodeInfo> onNodeFound, CancellationToken cancellationToken)
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
        /// Visits a namespace block.
        /// </summary>
        /// <param name="node">The namespace block node</param>
        public override void VisitNamespaceBlock(NamespaceBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        /// <summary>
        /// Visits a class block.
        /// </summary>
        /// <param name="node">The class block</param>
        public override void VisitClassBlock(ClassBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node.ClassStatement, SyntaxNodeKind.Type, node.ClassStatement.Identifier);
            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        /// <summary>
        /// Visits a structure block.
        /// </summary>
        /// <param name="node">The struct node</param>
        public override void VisitStructureBlock(StructureBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node.StructureStatement, SyntaxNodeKind.Type, node.StructureStatement.Identifier);
            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        /// <summary>
        /// Visits an interface block.
        /// </summary>
        /// <param name="node">The interface block</param>
        public override void VisitInterfaceBlock(InterfaceBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node.InterfaceStatement, SyntaxNodeKind.Type, node.InterfaceStatement.Identifier);
            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        /// <summary>
        /// Visits a module block.
        /// </summary>
        /// <param name="node">The module block</param>
        public override void VisitModuleBlock(ModuleBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node.ModuleStatement, SyntaxNodeKind.Type, node.ModuleStatement.Identifier);
            foreach (var child in node.Members)
            {
                this.Visit(child);
            }
        }

        /// <summary>
        /// Visits an enumeration block.
        /// </summary>
        /// <param name="node">The enumeration block</param>
        public override void VisitEnumBlock(EnumBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node.EnumStatement, SyntaxNodeKind.Type, node.EnumStatement.Identifier);
        }

        /// <summary>
        /// Visits a method block.
        /// </summary>
        /// <param name="node">The method block</param>
        public override void VisitMethodBlock(MethodBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            // This code was written at a time when node.Begin was a base class property that did not define an Identifier property.
            // node.SubOrFunctionStatement does and this code should be updated accordingly.
            this.Visit(node.SubOrFunctionStatement);
        }

        /// <summary>
        /// Visits a constructor block. 
        /// </summary>
        /// <param name="node">The constructor block</param>
        public override void VisitConstructorBlock(ConstructorBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            // As with other kinds of method blocks, we're interested in the "Sub New" statement,
            // not the block itself.
            this.Visit(node.SubNewStatement);
        }

        /// <summary>
        /// Visits a constructor statement. E.g., "Sub New(...)".
        /// </summary>
        /// <param name="node">The constructor statement syntax</param>
        public override void VisitSubNewStatement(SubNewStatementSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.NewKeyword);
        }

        /// <summary>
        /// Visits an operator statement.
        /// </summary>
        /// <param name="node">The operator statement syntax</param>
        public override void VisitOperatorStatement(OperatorStatementSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.OperatorToken);
        }

        /// <summary>
        /// Visits a method statement, example: interface declaration
        /// </summary>
        /// <param name="node">The method statement node</param>
        public override void VisitMethodStatement(MethodStatementSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node, SyntaxNodeKind.Method, node.Identifier);
        }

        /// <summary>
        /// Visits a property declaration.
        /// </summary>
        /// <param name="node">The property declaration</param>
        public override void VisitPropertyBlock(PropertyBlockSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node.PropertyStatement, SyntaxNodeKind.Property, node.PropertyStatement.Identifier);
        }

        /// <summary>
        /// Visits a property statements. We can get here in cases where there are anonymous properties, because
        /// there is no block statement.
        /// </summary>
        /// <param name="node">The syntax node</param>
        public override void VisitPropertyStatement(PropertyStatementSyntax node)
        {
            ArgumentValidation.NotNull(node, "node");

            this.FireOnNodeFound(node, SyntaxNodeKind.Property, node.Identifier);
        }

        /// <summary>
        /// Override the default visit to throw if the process has been canceled.
        /// </summary>
        /// <param name="node">The node that was visited</param>
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
                this.onNodeFound(new VisualBasicNodeInfo(node, kind, identifier));
            }
        }

        /// <summary>
        /// This class is returned to callers when we find a new node.
        /// </summary>
        private class VisualBasicNodeInfo : INodeInfo
        {
            private readonly SyntaxNode node;
            private readonly SyntaxNodeKind kind;
            private readonly int startPosition;

            /// <summary>
            /// Creates the CSharp node found info object.
            /// </summary>
            /// <param name="node">The node that was found</param>
            /// <param name="kind">The kind of node</param>
            public VisualBasicNodeInfo(SyntaxNode node, SyntaxNodeKind kind, SyntaxToken identifier)
            {
                this.node = node;
                this.kind = kind;
                this.startPosition = node.Span.Start;

                var lines = identifier.SyntaxTree.GetText().Lines;
                var taggedLine = lines.GetLineFromPosition(identifier.Span.Start);

                // If the tagged line start is within node.FullSpan, start looking for tokens from start of tagged line
                // If tagged line start is not part of the current node, there maybe another node that precedes this node at
                // start of this line. In that scenario, return node.Span.Start for the tag position
                if (node.FullSpan.Contains(taggedLine.Start))
                {
                    var nextToken = node.FindToken(taggedLine.Start);

                    // Find first token on the tagged line
                    while (nextToken.Span.End <= node.Span.End)
                    {
                        // Token position is on the tagged line, use as startPosition
                        if (nextToken.Span.Start >= taggedLine.Start)
                        {
                            this.startPosition = nextToken.Span.Start;
                            break;
                        }

                        nextToken = nextToken.GetNextToken();
                    }
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
