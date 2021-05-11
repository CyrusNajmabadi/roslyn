' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Text
Imports System.Threading.Tasks
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Extensions
{
    Friend Module VisualBasicSyntaxNodeExtensions
        ''' <summary>
        ''' Get the parent node of a given node.  If the node is a StatementSyntax, get closest parent node that is not the corresponding BlockNode.
        ''' </summary>
        Public Shared Function GetParentNode(node As SyntaxNode) As SyntaxNode
            If (TypeOf node Is NamespaceStatementSyntax) Then
                Return If(TypeOf node.Parent Is NamespaceBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is ModuleStatementSyntax) Then
                Return If(TypeOf node.Parent Is ModuleBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is ClassStatementSyntax) Then
                Return If(TypeOf node.Parent Is ClassBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is EnumStatementSyntax) Then
                Return If(TypeOf node.Parent Is EnumBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is InterfaceStatementSyntax) Then
                Return If(TypeOf node.Parent Is InterfaceBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is StructureStatementSyntax) Then
                Return If(TypeOf node.Parent Is StructureBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is DeclareStatementSyntax Or TypeOf node Is MethodStatementSyntax Or TypeOf node Is OperatorStatementSyntax) Then
                Return If(TypeOf node.Parent Is MethodBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is PropertyStatementSyntax) Then
                Return If(TypeOf node.Parent Is PropertyBlockSyntax, GetParentNode(node.Parent), node.Parent)
            ElseIf (TypeOf node Is SubNewStatementSyntax) Then
                Return If(TypeOf node.Parent Is ConstructorBlockSyntax, GetParentNode(node.Parent), node.Parent)
            End If

            Return node.Parent
        End Function

        ''' <summary>
        ''' Get the identifier name of a node.
        ''' </summary>
        Public Shared Function GetIdentifierName(node As SyntaxNode) As String
            ' Make sure we have a StatementNode.  If it is a BlockNode, get its corresponding StatementNode
            node = GetStatementFromBlock(node)

            If (TypeOf node Is NamespaceStatementSyntax) Then
                                {
                Return RemoveTrivia(((NamespaceStatementSyntax)node).Name).ToString()
            }
            ElseIf (TypeOf node Is ModuleStatementSyntax) Then
                                {
                Return ((ModuleStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is ClassStatementSyntax) Then
                                {
                Return ((ClassStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is EnumStatementSyntax) Then
                                {
                Return ((EnumStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is InterfaceStatementSyntax) Then
                                {
                Return ((InterfaceStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is StructureStatementSyntax) Then
                                {
                Return ((StructureStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is DeclareStatementSyntax) Then
                                {
                Return ((DeclareStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is MethodStatementSyntax) Then
                                {
                Return ((MethodStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is OperatorStatementSyntax) Then
                                {
                OperatorStatementSyntax operatorNode = (OperatorStatementSyntax)node
                Return String.Join(" ", operatorNode.OperatorKeyword.ToString(), operatorNode.OperatorToken.ToString())
            }
            ElseIf (TypeOf node Is SubNewStatementSyntax) Then
                                {
                Return ((SubNewStatementSyntax)node).NewKeyword.ToString()
            }
            ElseIf (TypeOf node Is PropertyStatementSyntax) Then
                                {
                Return ((PropertyStatementSyntax)node).Identifier.ToString()
            }
            ElseIf (TypeOf node Is CompilationUnitSyntax) Then
                                {
                Return String.Empty
            }
            Else
            {
                Debug.Assert(False, "Unhandled node type")
                    Return String.Empty
            }
        }

        ''' <summary>
        ''' Get the StatementNode that begins the BlockNode
        ''' </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily", Justification = "Too much memory to create all types of objects")]
        Private Static SyntaxNode GetStatementFromBlock(SyntaxNode node)
        {
            If (node Is NamespaceBlockSyntax) Then
                                    {
                node = ((NamespaceBlockSyntax)node).NamespaceStatement
            }
            ElseIf (TypeOf node Is ModuleBlockSyntax) Then
                                    {
                node = ((ModuleBlockSyntax)node).ModuleStatement
            }
            ElseIf (TypeOf node Is ClassBlockSyntax) Then
                                    {
                node = ((ClassBlockSyntax)node).ClassStatement
            }
            ElseIf (TypeOf node Is EnumBlockSyntax) Then
                                    {
                node = ((EnumBlockSyntax)node).EnumStatement
            }
            ElseIf (TypeOf node Is InterfaceBlockSyntax) Then
                                    {
                node = ((InterfaceBlockSyntax)node).InterfaceStatement
            }
            ElseIf (TypeOf node Is StructureBlockSyntax) Then
                                    {
                node = ((StructureBlockSyntax)node).StructureStatement
            }
            ElseIf (TypeOf node Is PropertyBlockSyntax) Then
                                    {
                node = ((PropertyBlockSyntax)node).PropertyStatement
            }
            ElseIf (TypeOf node Is MethodBlockSyntax) Then
            {
                node = ((MethodBlockSyntax)node).SubOrFunctionStatement
            }

            Return node
        }

        Private Static TSyntaxNode RemoveTrivia<TSyntaxNode>(TSyntaxNode node) where TSyntaxNode : SyntaxNode
        {
            Return node.ReplaceTrivia(node.DescendantTrivia(), (originalTrivia, replacementTrivia) >= default(SyntaxTrivia))
        }
    }
}
