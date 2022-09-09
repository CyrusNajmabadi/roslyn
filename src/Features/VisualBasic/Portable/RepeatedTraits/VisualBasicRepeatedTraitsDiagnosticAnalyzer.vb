' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Editing
Imports Microsoft.CodeAnalysis.LanguageService
Imports Microsoft.CodeAnalysis.TestAttributes
Imports Microsoft.CodeAnalysis.VisualBasic.CodeGeneration
Imports Microsoft.CodeAnalysis.VisualBasic.LanguageService
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Microsoft.CodeAnalysis.VisualBasic.RepeatedTraits
    <DiagnosticAnalyzer(LanguageNames.VisualBasic)>
    Friend Class VisualBasicRepeatedTraitsDiagnosticAnalyzer
        Inherits AbstractRepeatedTraitsDiagnosticAnalyzer(Of SyntaxKind, ClassBlockSyntax)

        Protected Overrides ReadOnly Property SyntaxFacts As ISyntaxFacts = VisualBasicSyntaxFacts.Instance
        Protected Overrides ReadOnly Property SyntaxGenerator As SyntaxGenerator = VisualBasicSyntaxGenerator.Instance
    End Class
End Namespace
