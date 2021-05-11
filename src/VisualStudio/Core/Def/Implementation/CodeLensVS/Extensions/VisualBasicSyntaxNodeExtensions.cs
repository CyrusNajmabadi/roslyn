// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Extensions
{
    internal static class VisualBasicSyntaxNodeExtensions
    {
        /// <summary>
        /// Get the parent node of a given node.  If the node is a StatementSyntax, get closest parent node that is not the corresponding BlockNode.
        /// </summary>
        public static SyntaxNode GetParentNode(SyntaxNode node)
        {
            if (node is NamespaceStatementSyntax)
            {
                return node.Parent is NamespaceBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is ModuleStatementSyntax)
            {
                return node.Parent is ModuleBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is ClassStatementSyntax)
            {
                return node.Parent is ClassBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is EnumStatementSyntax)
            {
                return node.Parent is EnumBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is InterfaceStatementSyntax)
            {
                return node.Parent is InterfaceBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is StructureStatementSyntax)
            {
                return node.Parent is StructureBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is DeclareStatementSyntax || node is MethodStatementSyntax || node is OperatorStatementSyntax)
            {
                return node.Parent is MethodBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is PropertyStatementSyntax)
            {
                return node.Parent is PropertyBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }
            else if (node is SubNewStatementSyntax)
            {
                return node.Parent is ConstructorBlockSyntax ? GetParentNode(node.Parent) : node.Parent;
            }

            return node.Parent;
        }

        /// <summary>
        /// Get the identifier name of a node.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily", Justification = "Too much memory to create all types of objects")]
        public static string GetIdentifierName(SyntaxNode node)
        {
            // Make sure we have a StatementNode.  If it is a BlockNode, get its corresponding StatementNode
            node = GetStatementFromBlock(node);

            if (node is NamespaceStatementSyntax)
            {
                return RemoveTrivia(((NamespaceStatementSyntax)node).Name).ToString();
            }
            else if (node is ModuleStatementSyntax)
            {
                return ((ModuleStatementSyntax)node).Identifier.ToString();
            }
            else if (node is ClassStatementSyntax)
            {
                return ((ClassStatementSyntax)node).Identifier.ToString();
            }
            else if (node is EnumStatementSyntax)
            {
                return ((EnumStatementSyntax)node).Identifier.ToString();
            }
            else if (node is InterfaceStatementSyntax)
            {
                return ((InterfaceStatementSyntax)node).Identifier.ToString();
            }
            else if (node is StructureStatementSyntax)
            {
                return ((StructureStatementSyntax)node).Identifier.ToString();
            }
            else if (node is DeclareStatementSyntax)
            {
                return ((DeclareStatementSyntax)node).Identifier.ToString();
            }
            else if (node is MethodStatementSyntax)
            {
                return ((MethodStatementSyntax)node).Identifier.ToString();
            }
            else if (node is OperatorStatementSyntax)
            {
                OperatorStatementSyntax operatorNode = (OperatorStatementSyntax)node;
                return string.Join(" ", operatorNode.OperatorKeyword.ToString(), operatorNode.OperatorToken.ToString());
            }
            else if (node is SubNewStatementSyntax)
            {
                return ((SubNewStatementSyntax)node).NewKeyword.ToString();
            }
            else if (node is PropertyStatementSyntax)
            {
                return ((PropertyStatementSyntax)node).Identifier.ToString();
            }
            else if (node is CompilationUnitSyntax)
            {
                return string.Empty;
            }
            else
            {
                Debug.Assert(false, "Unhandled node type");
                return string.Empty;
            }
        }

        /// <summary>
        /// Get the StatementNode that begins the BlockNode
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily", Justification = "Too much memory to create all types of objects")]
        private static SyntaxNode GetStatementFromBlock(SyntaxNode node)
        {
            if (node is NamespaceBlockSyntax)
            {
                node = ((NamespaceBlockSyntax)node).NamespaceStatement;
            }
            else if (node is ModuleBlockSyntax)
            {
                node = ((ModuleBlockSyntax)node).ModuleStatement;
            }
            else if (node is ClassBlockSyntax)
            {
                node = ((ClassBlockSyntax)node).ClassStatement;
            }
            else if (node is EnumBlockSyntax)
            {
                node = ((EnumBlockSyntax)node).EnumStatement;
            }
            else if (node is InterfaceBlockSyntax)
            {
                node = ((InterfaceBlockSyntax)node).InterfaceStatement;
            }
            else if (node is StructureBlockSyntax)
            {
                node = ((StructureBlockSyntax)node).StructureStatement;
            }
            else if (node is PropertyBlockSyntax)
            {
                node = ((PropertyBlockSyntax)node).PropertyStatement;
            }
            else if (node is MethodBlockSyntax)
            {
                node = ((MethodBlockSyntax)node).SubOrFunctionStatement;
            }

            return node;
        }

        private static TSyntaxNode RemoveTrivia<TSyntaxNode>(TSyntaxNode node) where TSyntaxNode : SyntaxNode
        {
            return node.ReplaceTrivia(node.DescendantTrivia(), (originalTrivia, replacementTrivia) => default(SyntaxTrivia));
        }
    }
}
