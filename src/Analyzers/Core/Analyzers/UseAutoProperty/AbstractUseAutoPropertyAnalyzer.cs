// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.UseAutoProperty;

internal abstract partial class AbstractUseAutoPropertyAnalyzer<
    TAnalyzer,
    TSyntaxKind,
    TPropertyDeclaration,
    TConstructorDeclaration,
    TFieldDeclaration,
    TVariableDeclarator,
    TExpression,
    TIdentifierName> : AbstractBuiltInCodeStyleDiagnosticAnalyzer
    where TAnalyzer : AbstractUseAutoPropertyAnalyzer<
        TAnalyzer,
        TSyntaxKind,
        TPropertyDeclaration,
        TConstructorDeclaration,
        TFieldDeclaration,
        TVariableDeclarator,
        TExpression,
        TIdentifierName>
    where TSyntaxKind : struct, Enum
    where TPropertyDeclaration : SyntaxNode
    where TConstructorDeclaration : SyntaxNode
    where TFieldDeclaration : SyntaxNode
    where TVariableDeclarator : SyntaxNode
    where TExpression : SyntaxNode
    where TIdentifierName : TExpression
{
    /// <summary>
    /// ConcurrentStack as that's the only concurrent collection that supports 'Clear' in netstandard2.
    /// </summary>
    private static readonly ObjectPool<ConcurrentStack<AnalysisResult>> s_analysisResultPool = new(() => new());
    private static readonly ObjectPool<ConcurrentSet<IFieldSymbol>> s_fieldSetPool = new(() => []);
    private static readonly ObjectPool<ConcurrentSet<SyntaxNode>> s_nodeSetPool = new(() => []);
    private static readonly ObjectPool<ConcurrentDictionary<IFieldSymbol, ConcurrentSet<SyntaxNode>>> s_fieldWriteLocationPool = new(() => []);

    private static readonly Func<IFieldSymbol, ConcurrentSet<SyntaxNode>> s_createFieldWriteNodeSet = _ => s_nodeSetPool.Allocate();

    /// <summary>
    /// Not static as this has different semantics around case sensitivity for C# and VB.
    /// </summary>
    private readonly ObjectPool<HashSet<string>> _fieldNamesPool;

    protected AbstractUseAutoPropertyAnalyzer()
        : base(IDEDiagnosticIds.UseAutoPropertyDiagnosticId,
               EnforceOnBuildValues.UseAutoProperty,
               CodeStyleOptions2.PreferAutoProperties,
               new LocalizableResourceString(nameof(AnalyzersResources.Use_auto_property), AnalyzersResources.ResourceManager, typeof(AnalyzersResources)),
               new LocalizableResourceString(nameof(AnalyzersResources.Use_auto_property), AnalyzersResources.ResourceManager, typeof(AnalyzersResources)))
    {
        _fieldNamesPool = new(() => new(this.SyntaxFacts.StringComparer));
    }

    protected static void AddFieldWrite(ConcurrentDictionary<IFieldSymbol, ConcurrentSet<SyntaxNode>> fieldWrites, IFieldSymbol field, SyntaxNode node)
        => fieldWrites.GetOrAdd(field, s_createFieldWriteNodeSet).Add(node);

    /// <summary>
    /// A method body edit anywhere in a type will force us to reanalyze the whole type.
    /// </summary>
    /// <returns></returns>
    public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
        => DiagnosticAnalyzerCategory.SemanticDocumentAnalysis;

    protected abstract ISemanticFacts SemanticFacts { get; }
    protected ISyntaxFacts SyntaxFacts => this.SemanticFacts.SyntaxFacts;

    protected abstract TSyntaxKind PropertyDeclarationKind { get; }
    protected abstract bool SupportsReadOnlyProperties(Compilation compilation);
    protected abstract bool SupportsPropertyInitializer(Compilation compilation);
    protected abstract bool CanExplicitInterfaceImplementationsBeFixed();
    protected abstract TExpression? GetFieldInitializer(TVariableDeclarator variable, CancellationToken cancellationToken);
    protected abstract TExpression? GetGetterExpression(IMethodSymbol getMethod, CancellationToken cancellationToken);
    protected abstract TExpression? GetSetterExpression(IMethodSymbol setMethod, SemanticModel semanticModel, CancellationToken cancellationToken);
    protected abstract SyntaxNode GetFieldNode(TFieldDeclaration fieldDeclaration, TVariableDeclarator variableDeclarator);

    protected abstract void RegisterIneligibleFieldsAction(
        HashSet<string> fieldNames, ConcurrentSet<IFieldSymbol> ineligibleFields, SemanticModel semanticModel, SyntaxNode codeBlock, CancellationToken cancellationToken);

    private readonly struct SemiAutoPropertyAnalyzer : IDisposable
    {
        private readonly TAnalyzer _analyzer;

        private readonly INamedTypeSymbol _containingType;

        private readonly HashSet<string> _fieldNames;
        private readonly ConcurrentSet<IFieldSymbol> _fieldsOfInterest;

        public SemiAutoPropertyAnalyzer(
            TAnalyzer analyzer,
            SymbolStartAnalysisContext context)
        {
            _analyzer = analyzer;

            _containingType = (INamedTypeSymbol)context.Symbol;
            _fieldNames = _analyzer._fieldNamesPool.Allocate();
            _fieldsOfInterest = s_fieldSetPool.Allocate();

            if (_analyzer.SupportsSemiAutoProperties)
            {
                foreach (var member in _containingType.GetMembers())
                {
                    if (member is not IFieldSymbol
                        {
                            DeclaredAccessibility: Accessibility.Private,
                            CanBeReferencedByName: true,
                        } field)
                    {
                        continue;
                    }

                    _fieldsOfInterest.Add(field);
                    _fieldNames.Add(field.Name);
                }

                context.RegisterCodeBlockStartAction<TSyntaxKind>(AnalyzeCodeBlock);
            }
        }

        public void Dispose()
        {
            _analyzer._fieldNamesPool.ClearAndFree(_fieldNames);

            // s_analysisResultPool.ClearAndFree(AnalysisResults);
            s_fieldSetPool.ClearAndFree(_fieldsOfInterest);
        }
    }

    protected sealed override void InitializeWorker(AnalysisContext context)
        => context.RegisterSymbolStartAction(context =>
        {
            var namedType = (INamedTypeSymbol)context.Symbol;
            if (!ShouldAnalyze(context, namedType))
                return;

            var simpleAutoPropertyAnalyzer = new SimpleAutoPropertyAnalyzer(
                (TAnalyzer)this, context);

            var semiAutoPropertyAnalyzer = new SemiAutoPropertyAnalyzer(
                (TAnalyzer)this, context);

            context.RegisterSymbolEndAction(context =>
            {
                using (simpleAutoPropertyAnalyzer)
                using (semiAutoPropertyAnalyzer)
                {
                    Process(
                        simpleAutoPropertyAnalyzer.AnalysisResults,
                        simpleAutoPropertyAnalyzer.IneligibleFields,
                        simpleAutoPropertyAnalyzer.NonConstructorFieldWrites,
                        context);
                }
            });

            bool ShouldAnalyze(SymbolStartAnalysisContext context, INamedTypeSymbol namedType)
            {
                if (namedType.TypeKind is not TypeKind.Class and not TypeKind.Struct and not TypeKind.Module)
                    return false;

                // Don't bother running on this type unless at least one of its parts has the 'prefer auto props' option
                // on, and the diagnostic is not suppressed.
                if (!namedType.DeclaringSyntaxReferences.Select(d => d.SyntaxTree).Distinct().Any(tree =>
                {
                    var preferAutoProps = context.Options.GetAnalyzerOptions(tree).PreferAutoProperties;
                    return preferAutoProps.Value && !ShouldSkipAnalysis(tree, context.Options, context.Compilation.Options, preferAutoProps.Notification, context.CancellationToken);
                }))
                {
                    return false;
                }

                // If we are analyzing a sub-span (lightbulb case), then check if the filter span
                // has a field/property declaration where a diagnostic could be reported.
                if (context.FilterSpan.HasValue)
                {
                    Contract.ThrowIfNull(context.FilterTree);
                    var shouldAnalyze = false;
                    var analysisRoot = context.GetAnalysisRoot(findInTrivia: false);
                    foreach (var node in analysisRoot.DescendantNodes())
                    {
                        if (node is TPropertyDeclaration or TFieldDeclaration && context.ShouldAnalyzeSpan(node.Span, node.SyntaxTree))
                        {
                            shouldAnalyze = true;
                            break;
                        }
                    }

                    if (!shouldAnalyze && analysisRoot.FirstAncestorOrSelf<SyntaxNode>(node => node is TPropertyDeclaration or TFieldDeclaration) == null)
                        return false;
                }

                return true;
            }
        }, SymbolKind.NamedType);

    protected virtual bool CanConvert(IPropertySymbol property)
        => true;

    private IFieldSymbol? GetSetterField(SemanticModel semanticModel, IMethodSymbol setMethod, CancellationToken cancellationToken)
        => CheckFieldAccessExpression(semanticModel, GetSetterExpression(setMethod, semanticModel, cancellationToken), cancellationToken);

    private IFieldSymbol? GetGetterField(SemanticModel semanticModel, IMethodSymbol getMethod, CancellationToken cancellationToken)
        => CheckFieldAccessExpression(semanticModel, GetGetterExpression(getMethod, cancellationToken), cancellationToken);

    private static IFieldSymbol? CheckFieldAccessExpression(SemanticModel semanticModel, TExpression? expression, CancellationToken cancellationToken)
    {
        if (expression == null)
            return null;

        var symbolInfo = semanticModel.GetSymbolInfo(expression, cancellationToken);
        return symbolInfo.Symbol is IFieldSymbol { DeclaringSyntaxReferences.Length: 1 } field
            ? field
            : null;
    }

    private void Process(
        ConcurrentStack<AnalysisResult> analysisResults,
        ConcurrentSet<IFieldSymbol> ineligibleFields,
        ConcurrentDictionary<IFieldSymbol, ConcurrentSet<SyntaxNode>> nonConstructorFieldWrites,
        SymbolAnalysisContext context)
    {
        foreach (var result in analysisResults)
        {
            // C# specific check.
            if (ineligibleFields.Contains(result.Field))
                continue;

            // VB specific check.
            //
            // if the property doesn't have a setter currently.check all the types the field is declared in.  If the
            // field is written to outside of a constructor, then this field Is Not eligible for replacement with an
            // auto prop.  We'd have to make the autoprop read/write, And that could be opening up the property
            // widely (in accessibility terms) in a way the user would not want.
            if (result.Property.Language == LanguageNames.VisualBasic)
            {
                if (result.Property.DeclaredAccessibility != Accessibility.Private &&
                    result.Property.SetMethod is null &&
                    nonConstructorFieldWrites.TryGetValue(result.Field, out var writeLocations1) &&
                    writeLocations1.Any(loc => !loc.Ancestors().Contains(result.PropertyDeclaration)))
                {
                    continue;
                }
            }

            // If this was an `init` property, and there was a write to the field, then we can't support this.
            // That's because we can't still keep this `init` as that write will not be allowed, and we can't make
            // it a `setter` as that would allow arbitrary writing outside the type, despite the original `init`
            // semantics.
            if (result.Property.SetMethod is { IsInitOnly: true } &&
                nonConstructorFieldWrites.TryGetValue(result.Field, out var writeLocations2) &&
                writeLocations2.Any(loc => !loc.Ancestors().Contains(result.PropertyDeclaration)))
            {
                continue;
            }

            Process(result, context);
        }
    }

    private void Process(AnalysisResult result, SymbolAnalysisContext context)
    {
        var propertyDeclaration = result.PropertyDeclaration;
        var variableDeclarator = result.VariableDeclarator;
        var fieldNode = GetFieldNode(result.FieldDeclaration, variableDeclarator);

        // Now add diagnostics to both the field and the property saying we can convert it to 
        // an auto property.  For each diagnostic store both location so we can easily retrieve
        // them when performing the code fix.
        var additionalLocations = ImmutableArray.Create(
            propertyDeclaration.GetLocation(),
            variableDeclarator.GetLocation());

        // Place the appropriate marker on the field depending on the user option.
        var diagnostic1 = DiagnosticHelper.Create(
            Descriptor,
            fieldNode.GetLocation(),
            result.Notification,
            context.Options,
            additionalLocations: additionalLocations,
            properties: null);

        // Also, place a hidden marker on the property.  If they bring up a lightbulb
        // there, they'll be able to see that they can convert it to an auto-prop.
        var diagnostic2 = Diagnostic.Create(
            Descriptor, propertyDeclaration.GetLocation(),
            additionalLocations: additionalLocations);

        context.ReportDiagnostic(diagnostic1);
        context.ReportDiagnostic(diagnostic2);
    }

    private sealed record AnalysisResult(
        IPropertySymbol Property,
        IFieldSymbol Field,
        TPropertyDeclaration PropertyDeclaration,
        TFieldDeclaration FieldDeclaration,
        TVariableDeclarator VariableDeclarator,
        NotificationOption2 Notification);
}
