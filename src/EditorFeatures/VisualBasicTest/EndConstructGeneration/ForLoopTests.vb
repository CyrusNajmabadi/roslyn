﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis.Collections
Namespace Microsoft.CodeAnalysis.Editor.VisualBasic.UnitTests.EndConstructGeneration
    <[UseExportProvider]>
    <Trait(Traits.Feature, Traits.Features.EndConstructGeneration)>
    Public Class ForLoopTests
        <WpfFact>
        Public Async Function TestVerifyForWithIndex() As Task
            Await VerifyStatementEndConstructAppliedAsync(
                before:="Class c1
  Sub goo()
    For i = 1 To 10
  End Sub
End Class",
                beforeCaret:={2, -1},
                after:="Class c1
  Sub goo()
    For i = 1 To 10

    Next
  End Sub
End Class",
                afterCaret:={3, -1})
        End Function

        <WpfFact>
        Public Async Function TestVerifyForEach() As Task
            Await VerifyStatementEndConstructAppliedAsync(
                before:="Class c1
  Sub goo()
    For Each i In collection
  End Sub
End Class",
                beforeCaret:={2, -1},
                after:="Class c1
  Sub goo()
    For Each i In collection

    Next
  End Sub
End Class",
                afterCaret:={3, -1})
        End Function

        <WpfFact, WorkItem("http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/527481")>
        Public Async Function VerifyIndexMatchedInner1() As Task
            Await VerifyStatementEndConstructNotAppliedAsync(
                text:="Class c1
  Sub goo()
    For i = 1 To 10
      For j = 1 To 10
      Next j
  End Sub
End Class",
                 caret:={3, -1})
        End Function

        <WpfFact, WorkItem("http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/527481")>
        Public Async Function TestVerifyIndexMatchedInner2() As Task
            Await VerifyStatementEndConstructAppliedAsync(
                before:="Class c1
  Sub goo()
    For i = 1 To 10
      For j = 1 To 10
      Next j
  End Sub
End Class",
                beforeCaret:={2, -1},
                after:="Class c1
  Sub goo()
    For i = 1 To 10

    Next
      For j = 1 To 10
      Next j
  End Sub
End Class",
                afterCaret:={3, -1})
        End Function

        <WpfFact, WorkItem("http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/527481")>
        Public Async Function VerifyIndexSharedNext() As Task
            Await VerifyStatementEndConstructNotAppliedAsync(
                text:="Class c1
  Sub goo()
    For i = 1 To 10
      For j = 1 To 10
    Next j, i
  End Sub
End Class",
                 caret:={3, -1})
        End Function

        <WpfFact>
        Public Async Function TestVerifyNestedFor() As Task
            Await VerifyStatementEndConstructAppliedAsync(
                before:="' NestedFor
Class C
    Sub s
        For i = 1 to 10
            For i = 1 to 10
        Next
    End sub
End Class",
                beforeCaret:={4, -1},
                 after:="' NestedFor
Class C
    Sub s
        For i = 1 to 10
            For i = 1 to 10

            Next
        Next
    End sub
End Class",
                afterCaret:={5, -1})
        End Function

        <WpfFact>
        Public Async Function TestVerifyNestedForEach() As Task
            Await VerifyStatementEndConstructAppliedAsync(
                before:="Class C
    function f(byval x as Integer,
               byref y as string) as string
        for each k in {1,2,3}
            For each i in c
        Next
        return y
    End Function
End Class",
                beforeCaret:={4, -1},
                 after:="Class C
    function f(byval x as Integer,
               byref y as string) as string
        for each k in {1,2,3}
            For each i in c

            Next
        Next
        return y
    End Function
End Class",
                afterCaret:={5, -1})
        End Function

        <WpfFact>
        Public Async Function VerifyReCommitForEach() As Task
            Await VerifyStatementEndConstructNotAppliedAsync(
                text:="Class C
    Public Property p(byval x as Integer) as Integer
        for each i in {1,2,3}
        Next
    End Property
End Class",
                caret:={2, -1})
        End Function

        <WpfFact>
        Public Async Function VerifyForAtIncorrectLocation() As Task
            Await VerifyStatementEndConstructNotAppliedAsync(
                text:="Class C
    For i = 1 to 10",
                caret:={1, -1})
        End Function

        <WpfFact>
        Public Async Function VerifyInvalidForSyntax() As Task
            Await VerifyStatementEndConstructNotAppliedAsync(
                text:="Class C
    Sub s
        for For
    End Sub
End Class",
                caret:={2, -1})
        End Function
    End Class
End Namespace
