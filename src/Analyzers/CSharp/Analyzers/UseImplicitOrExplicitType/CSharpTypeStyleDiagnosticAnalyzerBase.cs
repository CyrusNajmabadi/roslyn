// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Utilities;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Diagnostics.TypeStyle;

internal abstract partial class CSharpTypeStyleDiagnosticAnalyzerBase(
    string diagnosticId,
    EnforceOnBuild enforceOnBuild,
    LocalizableString title,
    LocalizableString message)
    : AbstractBuiltInCodeStyleDiagnosticAnalyzer(diagnosticId,
        enforceOnBuild,
        [CSharpCodeStyleOptions.VarForBuiltInTypes, CSharpCodeStyleOptions.VarWhenTypeIsApparent, CSharpCodeStyleOptions.VarElsewhere],
        title, message)
{
    public const string IsInApparentTypeContext = nameof(IsInApparentTypeContext);
    public const string IsInIntrinsicTypeContext = nameof(IsInIntrinsicTypeContext);
    public const string ElsewhereContext = nameof(ElsewhereContext);
    public const string EquivalenceKey = nameof(EquivalenceKey);

    private static readonly ImmutableDictionary<string, string?> IsInApparentTypeContextProperties
        = ImmutableDictionary<string, string?>.Empty.Add(EquivalenceKey, IsInApparentTypeContext);

    private static readonly ImmutableDictionary<string, string?> IsInIntrinsicTypeContextProperties
        = ImmutableDictionary<string, string?>.Empty.Add(EquivalenceKey, IsInIntrinsicTypeContext);

    private static readonly ImmutableDictionary<string, string?> ElsewhereContextProperties
        = ImmutableDictionary<string, string?>.Empty.Add(EquivalenceKey, ElsewhereContext);

    protected abstract CSharpTypeStyleHelper Helper { get; }

    public override DiagnosticAnalyzerCategory GetAnalyzerCategory() => DiagnosticAnalyzerCategory.SemanticSpanAnalysis;

    protected override void InitializeWorker(AnalysisContext context)
        => context.RegisterSyntaxNodeAction(
            HandleVariableDeclaration, SyntaxKind.VariableDeclaration, SyntaxKind.ForEachStatement, SyntaxKind.DeclarationExpression);

    private void HandleVariableDeclaration(SyntaxNodeAnalysisContext context)
    {
        var declarationStatement = context.Node;
        var cancellationToken = context.CancellationToken;

        var semanticModel = context.SemanticModel;
        var declaredType = Helper.FindAnalyzableType(declarationStatement, semanticModel, cancellationToken);
        if (declaredType == null)
            return;

        var simplifierOptions = context.GetCSharpAnalyzerOptions().GetSimplifierOptions();

        var typeStyle = Helper.AnalyzeTypeName(
            declaredType, semanticModel, simplifierOptions, cancellationToken);
        if (!typeStyle.IsStylePreferred
            || ShouldSkipAnalysis(context, typeStyle.Notification)
            || !typeStyle.CanConvert(cancellationToken))
        {
            return;
        }

        // The severity preference is not Hidden, as indicated by IsStylePreferred.
        var descriptor = Descriptor;
        var properties =
            typeStyle.IsInApparentTypeContext ? IsInApparentTypeContextProperties :
            typeStyle.IsInIntrinsicTypeContext ? IsInIntrinsicTypeContextProperties :
            ElsewhereContextProperties;

        context.ReportDiagnostic(DiagnosticHelper.Create(
            descriptor,
            declarationStatement.SyntaxTree.GetLocation(declaredType.StripRefIfNeeded().Span),
            typeStyle.Notification,
            context.Options,
            additionalLocations: null,
            properties));
    }
}
