' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports Analyzer.Utilities.Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Operations
Imports Roslyn.Diagnostics.Analyzers

Namespace Roslyn.Diagnostics.VisualBasic.Analyzers
    <DiagnosticAnalyzer(LanguageNames.VisualBasic)>
    Public NotInheritable Class BasicDoNotImplicitlyConvertCharArrayToStringAnalyzer
        Inherits DiagnosticAnalyzer

        Private Shared ReadOnly DoNotImplicitlyConvertCharArrayToStringRuleId As New DiagnosticDescriptor(
            RoslynDiagnosticIds.DoNotImplicitlyConvertCharArrayToStringRuleId,
            RoslynDiagnosticsAnalyzersResources.DoNotImplicitlyConvertCharArrayToString,
            RoslynDiagnosticsAnalyzersResources.DoNotImplicitlyConvertCharArrayToString,
            DiagnosticCategory.RoslynDiagnosticsPerformance,
            DiagnosticSeverity.Warning,
            isEnabledByDefault:=True,
            customTags:=WellKnownDiagnosticTagsExtensions.Telemetry)

        Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor) =
            ImmutableArray.Create(DoNotImplicitlyConvertCharArrayToStringRuleId)

        Public Overrides Sub Initialize(context As AnalysisContext)
            context.EnableConcurrentExecution()
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None)
            context.RegisterOperationAction(AddressOf AnalyzeConversion, OperationKind.Conversion)
        End Sub

        Private Sub AnalyzeConversion(context As OperationAnalysisContext)
            Dim operation = DirectCast(context.Operation, IConversionOperation)
            Dim conversion = operation.GetConversion()

            If conversion.IsWidening AndAlso conversion.IsString Then
                Dim operandType = TryCast(operation.Operand.Type, IArrayTypeSymbol)
                If operandType?.ElementType.SpecialType = SpecialType.System_Char Then
                    context.ReportDiagnostic(operation.CreateDiagnostic(DoNotImplicitlyConvertCharArrayToStringRuleId))
                End If
            End If
        End Sub
    End Class
End Namespace
