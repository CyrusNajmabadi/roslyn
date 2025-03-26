// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Microsoft.CodeAnalysis.MakeClassSealed;

[DiagnosticAnalyzer(LanguageNames.CSharp, LanguageNames.VisualBasic)]
internal sealed class MakeClassSealedDiagnosticAnalyzer()
    : AbstractBuiltInCodeStyleDiagnosticAnalyzer(
        IDEDiagnosticIds.MakeClassSealedDiagnosticId,
        EnforceOnBuildValues.MakeClassSealed,
        option: null,
        "Make type sealed")
{
    public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
        => DiagnosticAnalyzerCategory.SemanticDocumentAnalysis;

    protected override void InitializeWorker(AnalysisContext context)
    {
        context.RegisterSymbolAction(context =>
        {
            var namedType = (INamedTypeSymbol)context.Symbol;
            if (namedType.TypeKind != TypeKind.Class)
                return;

            if (namedType.IsAbstract)
                return;

            if (namedType.IsSealed)
                return;

            if (namedType.IsStatic)
                return;

            if (IsPublic(namedType))
                return;

            foreach (var reference in namedType.DeclaringSyntaxReferences)
            {
                var syntax = reference.GetSyntax(context.CancellationToken);

                context.ReportDiagnostic(Diagnostic.Create(
                    this.Descriptor,
                    syntax.GetLocation()));
            }
        }, SymbolKind.NamedType);
    }

    private static bool IsPublic(INamedTypeSymbol namedType)
    {
        for (var current = namedType; current != null; current = current.ContainingType)
        {
            if (current.DeclaredAccessibility != Accessibility.Public)
                return false;
        }

        return true;
    }
}
