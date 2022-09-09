// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.CSharp.CodeGeneration;
using Microsoft.CodeAnalysis.CSharp.LanguageService;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.TestAttributes;

namespace Microsoft.CodeAnalysis.CSharp.RepeatedTraits
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal sealed class CSharpRepeatedTraitsDiagnosticAnalyzer
        : AbstractRepeatedTraitsDiagnosticAnalyzer<SyntaxKind, ClassDeclarationSyntax>
    {
        protected override ISyntaxFacts SyntaxFacts => CSharpSyntaxFacts.Instance;

        protected override SyntaxGenerator SyntaxGenerator => CSharpSyntaxGenerator.Instance;
    }
}
