﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic.EditAndContinue
    ''' <summary>
    ''' A field that's part of declaration with multiple identifiers and As New clause:
    '''   Dim [|a|], [|b|] As New C(expr)
    ''' </summary>
    Friend NotInheritable Class FieldWithMultipleAsNewClauseDeclarationBody
        Inherits FieldOrPropertyDeclarationBody

        Private ReadOnly _modifedIdentifier As ModifiedIdentifierSyntax

        Public Sub New(modifiedIdentifier As ModifiedIdentifierSyntax)
            _modifedIdentifier = modifiedIdentifier
        End Sub

        Private ReadOnly Property NewExpression As NewExpressionSyntax
            Get
                Return DirectCast(DirectCast(_modifedIdentifier.Parent, VariableDeclaratorSyntax).AsClause, AsNewClauseSyntax).NewExpression
            End Get
        End Property

        Public Overrides ReadOnly Property InitializerActiveStatement As SyntaxNode
            Get
                Return _modifedIdentifier
            End Get
        End Property

        Public Overrides ReadOnly Property OtherActiveStatementContainer As SyntaxNode
            Get
                Return NewExpression
            End Get
        End Property

        Public Overrides ReadOnly Property Envelope As TextSpan
            Get
                Return TextSpan.FromBounds(_modifedIdentifier.Span.Start, NewExpression.Span.End)
            End Get
        End Property

        Public Overrides Function IsExcludedActiveStatementSpanWithinEnvelope(span As TextSpan) As Boolean
            ' exclude spans in between the identifier and AsNew clause
            Return TextSpan.FromBounds(_modifedIdentifier.Span.End, NewExpression.Span.Start).Contains(span)
        End Function

        Public Overrides ReadOnly Property EncompassingAncestor As SyntaxNode
            Get
                Return _modifedIdentifier.Parent
            End Get
        End Property

        Public Overrides Function GetActiveTokens(getDescendantTokens As Func(Of SyntaxNode, IEnumerable(Of SyntaxToken))) As IEnumerable(Of SyntaxToken)
            Return getDescendantTokens(InitializerActiveStatement).Concat(getDescendantTokens(NewExpression))
        End Function

        Public Overrides Function GetUserCodeTokens(getDescendantTokens As Func(Of SyntaxNode, IEnumerable(Of SyntaxToken))) As IEnumerable(Of SyntaxToken)
            Return getDescendantTokens(NewExpression)
        End Function
    End Class
End Namespace
