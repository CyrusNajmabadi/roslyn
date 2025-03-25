// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.CSharp.Analyzers.RemoveUnnecessaryNullableDirective;

#pragma warning disable RS1030 // The point of this analyzer is to check how forking without #nullable disable works.

[DiagnosticAnalyzer(LanguageNames.CSharp)]
internal sealed class CSharpRemoveUnnecessaryNullableDisableDirectiveDiagnosticAnalyzer()
    : AbstractBuiltInUnnecessaryCodeStyleDiagnosticAnalyzer(
        IDEDiagnosticIds.RemoveUnnecessaryNullableDisableDirectiveDiagnosticId,
        EnforceOnBuildValues.RemoveUnnecessaryNullableDisableDirective,
        option: null,
        fadingOption: null,
        new LocalizableResourceString(nameof(CSharpAnalyzersResources.Remove_unnecessary_nullable_directive), CSharpAnalyzersResources.ResourceManager, typeof(CSharpAnalyzersResources)),
        new LocalizableResourceString(nameof(CSharpAnalyzersResources.Nullable_directive_is_unnecessary), CSharpAnalyzersResources.ResourceManager, typeof(CSharpAnalyzersResources)))
{
    public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
        => DiagnosticAnalyzerCategory.SemanticDocumentAnalysis;

    protected override void InitializeWorker(AnalysisContext context)
        => context.RegisterCompilationStartAction(context =>
        {
            var compilation = (CSharpCompilation)context.Compilation;
            if (compilation.LanguageVersion < LanguageVersion.CSharp8)
            {
                // Compilation does not support nullable directives
                return;
            }

            context.RegisterSemanticModelAction(context => ProcessSemanticModel(context));
        });

    private void ProcessSemanticModel(SemanticModelAnalysisContext context)
    {
        var cancellationToken = context.CancellationToken;

        var semanticModel = context.SemanticModel;
        var compilation = semanticModel.Compilation;
        var syntaxTree = semanticModel.SyntaxTree;

        var root = syntaxTree.GetRoot(cancellationToken);
        if (!root.ContainsDirectives)
            return;

        using var _1 = ArrayBuilder<NullableDirectiveTriviaSyntax>.GetInstance(out var nullableDirectives);

        using var _2 = ArrayBuilder<SyntaxNodeOrToken>.GetInstance(out var stack);
        stack.Push(root);

        while (stack.TryPop(out var current))
        {
            if (!current.ContainsDirectives)
                continue;

            if (current.AsNode(out var currentNode))
            {
                foreach (var child in currentNode.ChildNodesAndTokens().Reverse())
                {
                    if (child.ContainsDirectives)
                        stack.Push(child);
                }
            }
            else
            {
                foreach (var trivia in current.AsToken().LeadingTrivia)
                {
                    if (trivia.GetStructure() is NullableDirectiveTriviaSyntax nullableDirective)
                        nullableDirectives.Add(nullableDirective);
                }
            }
        }

        if (nullableDirectives.Count == 0)
            return;

        var text = syntaxTree.GetText(cancellationToken);
        for (int i = 0, n = nullableDirectives.Count; i < n; i++)
        {
            var directive = nullableDirectives[i];
            if (directive.SettingToken.Kind() != SyntaxKind.DisableKeyword)
                continue;

            // Fork the document such that the directive is now a comment.  e.g. `#nullable disable` becomes
            // `//#nullable disable`. We do things this way so that the directive now has no impact.  Note: this will
            // make it so that positions change.
            var directiveText = text.ToString(directive.Span);
            var forkedText = text.WithChanges(
                new TextChange(new TextSpan(directive.SpanStart, length: 0), "//"));

            // Analyze from the end of this nullable directive to the start of the next one (or the end of the file if
            // there are no more).
            var spanToAnalyzeInInitialCoordinates = TextSpan.FromBounds(
                directive.FullSpan.End,
                i + 1 < n ? nullableDirectives[i + 1].FullSpan.Start : root.FullSpan.End);

            // Ignore any regions that are already reporting errors or warnings.
            var initialDiagnostics = semanticModel.GetDiagnostics(spanToAnalyzeInInitialCoordinates, cancellationToken);
            if (initialDiagnostics.Any(d => d.Severity is DiagnosticSeverity.Warning or DiagnosticSeverity.Error))
                continue;

            var spanToAnalyzerInForkedCoordinates = new TextSpan(
                spanToAnalyzeInInitialCoordinates.Start + "//".Length, spanToAnalyzeInInitialCoordinates.Length);

            var forkedSyntaxTree = syntaxTree.WithChangedText(forkedText);
            var forkedCompilation = compilation.ReplaceSyntaxTree(syntaxTree, forkedSyntaxTree);
            var forkedSemanticModel = forkedCompilation.GetSemanticModel(forkedSyntaxTree);

            var forkedDiagnostics = forkedSemanticModel.GetDiagnostics(spanToAnalyzerInForkedCoordinates, cancellationToken);
            if (forkedDiagnostics.Any(d => d.Severity is DiagnosticSeverity.Warning or DiagnosticSeverity.Error))
                continue;

            context.ReportDiagnostic(Diagnostic.Create(
                this.Descriptor,
                directive.SettingToken.GetLocation()));
        }
    }
}
