﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Namespace Microsoft.CodeAnalysis.Editor.VisualBasic.UnitTests.ImplementAbstractClass
    Partial Public Class ImplementAbstractClassTests

        <Fact>
        <Trait(Traits.Feature, Traits.Features.CodeActionsImplementAbstractClass)>
        <Trait(Traits.Feature, Traits.Features.CodeActionsFixAllOccurrences)>
        Public Async Function TestFixAllInDocument() As Task
            Dim input = <Workspace>
                            <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class {|FixAllInDocument:B1|}
    Inherits A1
    Implements I1

    Private Class C1
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                                <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                            <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                <ProjectReference>Assembly1</ProjectReference>
                                <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                        </Workspace>.ToString()

            Dim expected = <Workspace>
                               <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                   <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B1
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C1
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                                   <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                               </Project>
                               <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                   <ProjectReference>Assembly1</ProjectReference>
                                   <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                               </Project>
                           </Workspace>.ToString()

            Await TestInRegularAndScriptAsync(input, expected)
        End Function

        <Fact>
        <Trait(Traits.Feature, Traits.Features.CodeActionsImplementAbstractClass)>
        <Trait(Traits.Feature, Traits.Features.CodeActionsFixAllOccurrences)>
        Public Async Function TestFixAllInProject() As Task
            Dim input = <Workspace>
                            <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class {|FixAllInProject:B1|}
    Inherits A1
    Implements I1

    Private Class C1
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                                <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                            <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                <ProjectReference>Assembly1</ProjectReference>
                                <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                        </Workspace>.ToString()

            Dim expected = <Workspace>
                               <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                   <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B1
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C1
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                                   <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C2
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                               </Project>
                               <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                   <ProjectReference>Assembly1</ProjectReference>
                                   <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                               </Project>
                           </Workspace>.ToString()

            Await TestInRegularAndScriptAsync(input, expected)
        End Function

        <Fact>
        <Trait(Traits.Feature, Traits.Features.CodeActionsImplementAbstractClass)>
        <Trait(Traits.Feature, Traits.Features.CodeActionsFixAllOccurrences)>
        Public Async Function TestFixAllInSolution() As Task
            Dim input = <Workspace>
                            <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class {|FixAllInSolution:B1|}
    Inherits A1
    Implements I1

    Private Class C1
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                                <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                            <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                <ProjectReference>Assembly1</ProjectReference>
                                <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                        </Workspace>.ToString()

            Dim expected = <Workspace>
                               <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                   <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B1
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C1
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                                   <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C2
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                               </Project>
                               <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                   <ProjectReference>Assembly1</ProjectReference>
                                   <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C3
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                               </Project>
                           </Workspace>.ToString()

            Await TestInRegularAndScriptAsync(input, expected)
        End Function

        <Fact>
        <Trait(Traits.Feature, Traits.Features.CodeActionsImplementAbstractClass)>
        <Trait(Traits.Feature, Traits.Features.CodeActionsFixAllOccurrences)>
        Public Async Function TestFixAllInSolution_DifferentAssemblyWithSameTypeName() As Task
            Dim input = <Workspace>
                            <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class {|FixAllInSolution:B1|}
    Inherits A1
    Implements I1

    Private Class C1
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                                <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                            <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                        </Workspace>.ToString()

            Dim expected = <Workspace>
                               <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                   <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B1
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C1
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                                   <Document><![CDATA[
Class B2
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C2
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class]]>
                                   </Document>
                               </Project>
                               <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                   <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B3
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                               </Project>
                           </Workspace>.ToString()

            Await TestInRegularAndScriptAsync(input, expected)
        End Function

        <Fact>
        <Trait(Traits.Feature, Traits.Features.CodeActionsImplementAbstractClass)>
        <Trait(Traits.Feature, Traits.Features.CodeActionsFixAllOccurrences)>
        Public Async Function TestFixAllInContainingMember_NotApplicable() As Task
            Dim input = <Workspace>
                            <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class {|FixAllInContainingMember:B1|}
    Inherits A1
    Implements I1

    Private Class C1
        Inherits A1
        Implements I1
    End Class
End Class

Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                                <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                            <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                <ProjectReference>Assembly1</ProjectReference>
                                <Document><![CDATA[
Class B4
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                        </Workspace>.ToString()

            Await TestMissingInRegularAndScriptAsync(input)
        End Function

        <Fact>
        <Trait(Traits.Feature, Traits.Features.CodeActionsImplementAbstractClass)>
        <Trait(Traits.Feature, Traits.Features.CodeActionsFixAllOccurrences)>
        Public Async Function TestFixAllInContainingType() As Task
            Dim input = <Workspace>
                            <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class {|FixAllInContainingType:B1|}
    Inherits A1
    Implements I1

    Private Class C1
        Inherits A1
        Implements I1
    End Class
End Class

Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                                <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                            <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                <ProjectReference>Assembly1</ProjectReference>
                                <Document><![CDATA[
Class B4
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                </Document>
                            </Project>
                        </Workspace>.ToString()

            Dim expected = <Workspace>
                               <Project Language="Visual Basic" AssemblyName="Assembly1" CommonReferences="true">
                                   <Document><![CDATA[
Public MustInherit Class A1
    Public MustOverride Sub F1()
End Class

Public Interface I1
    Sub F2()
End Interface

Class B1
    Inherits A1
    Implements I1

    Public Overrides Sub F1()
        Throw New NotImplementedException()
    End Sub

    Private Class C1
        Inherits A1
        Implements I1

        Public Overrides Sub F1()
            Throw New NotImplementedException()
        End Sub
    End Class
End Class

Class B2
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                                   <Document><![CDATA[
Class B3
    Inherits A1
    Implements I1

    Private Class C2
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                               </Project>
                               <Project Language="Visual Basic" AssemblyName="Assembly2" CommonReferences="true">
                                   <ProjectReference>Assembly1</ProjectReference>
                                   <Document><![CDATA[
Class B4
    Inherits A1
    Implements I1

    Private Class C3
        Inherits A1
        Implements I1
    End Class
End Class]]>
                                   </Document>
                               </Project>
                           </Workspace>.ToString()

            Await TestInRegularAndScriptAsync(input, expected)
        End Function
    End Class
End Namespace
