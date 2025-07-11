﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Test.Utilities
Imports Microsoft.VisualStudio.LanguageServices.Implementation.CodeModel
Imports Roslyn.Test.Utilities

Namespace Microsoft.VisualStudio.LanguageServices.UnitTests.CodeModel.CSharp
    <[UseExportProvider]>
    Public Class SyntaxNodeKeyTests

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestClass() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        class C$$
        {
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "C", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestPartialClass1() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        partial class C$$
        {
        }

        partial class C
        {
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "C", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestPartialClass2() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        partial class C
        {
        }

        partial class C$$
        {
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "C", 2)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestClassInNamespace() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        namespace N1
        {
            class C$$
            {
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "N1.C", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestMethod() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        namespace N1
        {
            class C
            {
                void $$M()
                {
                }
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "N1.C.M()", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestMethodWithParameters() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        namespace N1
        {
            class C
            {
                void $$M(ref int, string)
                {
                }
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "N1.C.M(ref int,string)", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestGenericMethodWithParametersInGenericClass() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        namespace N1
        {
            class C&lt;T, U&gt;
            {
                void $$M&lt;V, W, X&gt;(ref int, string)
                {
                }
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "N1.C`2.M`3(ref int,string)", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestEscapedNames() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        namespace @int
        {
            namespace @class
            {
                class $$@void
                {
                }
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "int.class.void", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestConversionOperator1() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        class C
        {
            public static explicit $$operator string(C c)
            {
                return null;
            }

            public static implicit operator int(C c)
            {
                return 0;
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "C.#op_Explicit_string(C)", 1)
        End Function

        <WpfFact, Trait(Traits.Feature, Traits.Features.CodeModel)>
        Public Async Function TestConversionOperator2() As Task
            Dim input =
<Workspace>
    <Project Language="C#" CommonReferences="true">
        <Document>
        class C
        {
            public static explicit operator string(C c)
            {
                return null;
            }

            public static implicit $$operator int(C c)
            {
                return 0;
            }
        }
        </Document>
    </Project>
</Workspace>

            Await TestAsync(input, "C.#op_Implicit_int(C)", 1)
        End Function

        Private Shared Async Function TestAsync(definition As XElement, expectedName As String, expectedOrdinal As Integer) As Task
            Using workspace = EditorTestWorkspace.Create(definition, composition:=VisualStudioTestCompositions.LanguageServices)
                Dim project = workspace.CurrentSolution.Projects.First()
                Dim codeModelService = project.Services.GetService(Of ICodeModelService)()
                Assert.NotNull(codeModelService)

                Dim cursorDocument = workspace.Documents.First(Function(d) d.CursorPosition.HasValue)
                Dim cursorPosition = cursorDocument.CursorPosition.Value

                Dim document = workspace.CurrentSolution.GetDocument(cursorDocument.Id)

                Dim tree = Await document.GetSyntaxTreeAsync()
                Dim node = tree.GetRoot().FindToken(cursorPosition).Parent
                Dim nodeKey = codeModelService.GetNodeKey(node)

                Assert.Equal(expectedName, nodeKey.Name)
                Assert.Equal(expectedOrdinal, nodeKey.Ordinal)
            End Using
        End Function

    End Class
End Namespace
