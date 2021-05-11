// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Diagnostics;
using System.Globalization;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Extensions
{
    internal static class CSharpSyntaxNodeExtensions
    {
        /// <summary>
        /// Get the name of the identifier for the node.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily", Justification = "Too much memory to create all types of objects")]
        public static string GetIdentifierName(SyntaxNode node)
        {
            if (node is NamespaceDeclarationSyntax)
            {
                return RemoveTrivia(((NamespaceDeclarationSyntax)node).Name).ToString();
            }
            else if (node is ClassDeclarationSyntax)
            {
                return ((ClassDeclarationSyntax)node).Identifier.ToString();
            }
            else if (node is StructDeclarationSyntax)
            {
                return ((StructDeclarationSyntax)node).Identifier.ToString();
            }
            else if (node is InterfaceDeclarationSyntax)
            {
                return ((InterfaceDeclarationSyntax)node).Identifier.ToString();
            }
            else if (node is EnumDeclarationSyntax)
            {
                return ((EnumDeclarationSyntax)node).Identifier.ToString();
            }
            else if (node is PropertyDeclarationSyntax)
            {
                PropertyDeclarationSyntax propNode = node as PropertyDeclarationSyntax;
                string id = propNode.Identifier.ToString();
                return propNode.ExplicitInterfaceSpecifier != null
                    ? string.Format(CultureInfo.InvariantCulture, "({0}.{1})", RemoveTrivia(propNode.ExplicitInterfaceSpecifier.Name).ToString(), id)
                    : id;
            }
            else if (node is MethodDeclarationSyntax)
            {
                return ((MethodDeclarationSyntax)node).Identifier.ToString();
            }
            else if (node is OperatorDeclarationSyntax)
            {
                OperatorDeclarationSyntax operatorNode = (OperatorDeclarationSyntax)node;

                return string.Join(" ", operatorNode.OperatorKeyword.ToString(), operatorNode.OperatorToken.ToString());
            }
            else if (node is IndexerDeclarationSyntax)
            {
                IndexerDeclarationSyntax indexerNode = node as IndexerDeclarationSyntax;
                string id = indexerNode.ThisKeyword.ToString();
                return indexerNode.ExplicitInterfaceSpecifier != null
                    ? string.Format(CultureInfo.InvariantCulture, "({0}.{1})", RemoveTrivia(indexerNode.ExplicitInterfaceSpecifier.Name).ToString(), id)
                    : id;
            }
            else if (node is ConstructorDeclarationSyntax)
            {
                return ((ConstructorDeclarationSyntax)node).Identifier.ToString();
            }
            else if (node is DestructorDeclarationSyntax)
            {
                return string.Format(CultureInfo.InvariantCulture, "~{0}", ((DestructorDeclarationSyntax)node).Identifier.ToString());
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

        private static TSyntaxNode RemoveTrivia<TSyntaxNode>(TSyntaxNode node) where TSyntaxNode : SyntaxNode
        {
            return node.ReplaceTrivia(node.DescendantTrivia(), (originalTrivia, replacementTrivia) => default(SyntaxTrivia));
        }
    }
}
