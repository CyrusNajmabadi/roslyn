// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.CSharp.MakeMethodAsync;

internal sealed class CSharpMakeMethodAsyncDiagnosticAnalyzer()
    : AbstractBuiltInCodeStyleDiagnosticAnalyzer(
        "MA0001", EnforceOnBuild.WhenExplicitlyEnabled, (IOption2?)null, "New make async")
{
    public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
        => DiagnosticAnalyzerCategory.SemanticDocumentAnalysis;

    protected override void InitializeWorker(AnalysisContext context)
    {
        context.RegisterCompilationStartAction(context =>
        {
            var taskTypes = new KnownTaskTypes(context.Compilation);

            context.RegisterSyntaxNodeAction(AnalyzeMethodDeclaration, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(context => AnalyzeAnonymousFunction(context, taskTypes), SyntaxKind.AnonymousMethodExpression, SyntaxKind.SimpleLambdaExpression, SyntaxKind.ParenthesizedLambdaExpression);

        });
    }

    private void AnalyzeMethodDeclaration(SyntaxNodeAnalysisContext context)
    {
        var methodDeclaration = (MethodDeclarationSyntax)context.Node;
        var returnType = methodDeclaration.ReturnType;
        var rightMostName = returnType.GetRightmostName();
        if (rightMostName is null)
            return;

        var identifier = rightMostName.Identifier;
        if (identifier.ValueText is not nameof(Task) and not nameof(ValueTask))
            return;

        if (methodDeclaration.Modifiers.Any(SyntaxKind.AsyncKeyword))
            return;

        context.ReportDiagnostic(Diagnostic.Create(this.Descriptor, methodDeclaration.GetLocation()));
    }

    private void AnalyzeAnonymousFunction(SyntaxNodeAnalysisContext context, KnownTaskTypes taskTypes)
    {
        var cancellationToken = context.CancellationToken;
        var node = (AnonymousFunctionExpressionSyntax)context.Node;
        if (node.Modifiers.Any(SyntaxKind.AsyncKeyword))
            return;

        if (context.SemanticModel.GetSymbolInfo(node, cancellationToken).Symbol is not IMethodSymbol lambdaMethod)
            return;

        if (!taskTypes.IsTaskLike(lambdaMethod.ReturnType))
            return;

        context.ReportDiagnostic(Diagnostic.Create(this.Descriptor, node.GetLocation()));
    }
}
