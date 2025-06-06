﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis.VisualBasic.Symbols
Imports Microsoft.CodeAnalysis.VisualBasic.UnitTests.Symbols
Imports Roslyn.Test.Utilities

Namespace Microsoft.CodeAnalysis.VisualBasic.UnitTests

    Public Class ImplicitClassTests
        Inherits BasicTestBase

        <Fact, WorkItem(6040, "https://github.com/dotnet/roslyn/issues/6040")>
        Public Sub ImplicitClassSymbol()
            Dim c = CompilationUtils.CreateCompilationWithMscorlib40(
<compilation name="C">
    <file>
Namespace N
    Sub Goo
    End Sub
End Namespace
    </file>
</compilation>)

            Dim n = DirectCast(c.Assembly.GlobalNamespace.GetMembers("N").Single(), NamespaceSymbol)
            Dim implicitClass = DirectCast(n.GetMembers().Single(), NamedTypeSymbol)

            Assert.Equal(0, implicitClass.GetAttributes().Length)
            Assert.Equal(0, implicitClass.Interfaces.Length)
            Assert.Equal(c.ObjectType, implicitClass.BaseType)
            Assert.Equal(0, implicitClass.Arity)
            Assert.True(implicitClass.IsImplicitlyDeclared)
            Assert.Equal(SyntaxKind.NamespaceStatement, implicitClass.DeclaringSyntaxReferences.Single().GetSyntax().Kind)
            Assert.False(implicitClass.IsSubmissionClass)
            Assert.False(implicitClass.IsScriptClass)

            Dim c2 = CreateCompilationWithMscorlib461(source:=Nothing, {c.ToMetadataReference()})

            n = DirectCast(c2.GlobalNamespace.GetMembers("N").Single(), NamespaceSymbol)
            implicitClass = DirectCast(n.GetMembers().Single(), NamedTypeSymbol)
            Assert.IsType(Of Retargeting.RetargetingNamedTypeSymbol)(implicitClass)
            Assert.Equal(0, implicitClass.Interfaces.Length)
            Assert.Equal(c2.ObjectType, implicitClass.BaseType)
        End Sub

        <ConditionalFact(GetType(NoUsedAssembliesValidation))> ' https://github.com/dotnet/roslyn/issues/40682: The test hook is blocked by this issue.
        <WorkItem(40682, "https://github.com/dotnet/roslyn/issues/40682")>
        Public Sub ScriptClassSymbol()
            Dim c = CompilationUtils.CreateCompilationWithMscorlib40(
<compilation name="C">
    <file>
Sub Goo
End Sub
    </file>
</compilation>, parseOptions:=TestOptions.Script)

            Dim scriptClass = DirectCast(c.Assembly.GlobalNamespace.GetMembers().Single(), NamedTypeSymbol)

            Assert.Equal(0, scriptClass.GetAttributes().Length)
            Assert.Equal(0, scriptClass.Interfaces.Length)
            Assert.Equal(c.ObjectType, scriptClass.BaseType)
            Assert.Equal(0, scriptClass.Arity)
            Assert.True(scriptClass.IsImplicitlyDeclared)
            Assert.Equal(SyntaxKind.CompilationUnit, scriptClass.DeclaringSyntaxReferences.Single().GetSyntax().Kind)
            Assert.False(scriptClass.IsSubmissionClass)
            Assert.True(scriptClass.IsScriptClass)
        End Sub
    End Class
End Namespace

