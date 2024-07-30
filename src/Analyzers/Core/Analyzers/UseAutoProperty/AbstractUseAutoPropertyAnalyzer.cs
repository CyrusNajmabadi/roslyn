// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
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
    private static readonly Func<IFieldSymbol, ConcurrentSet<SyntaxNode>> s_createFieldWriteNodeSet = _ => ConcurrentSetPool<SyntaxNode>.Allocate();

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
    protected abstract bool SupportsSemiAutoProperties(Compilation compilation);

    protected abstract bool CanExplicitInterfaceImplementationsBeFixed();
    protected abstract TExpression? GetFieldInitializer(TVariableDeclarator variable, CancellationToken cancellationToken);
    protected abstract TExpression? GetGetterExpression(IMethodSymbol getMethod, CancellationToken cancellationToken);
    protected abstract TExpression? GetSetterExpression(IMethodSymbol setMethod, SemanticModel semanticModel, CancellationToken cancellationToken);
    protected abstract SyntaxNode GetFieldNode(TFieldDeclaration fieldDeclaration, TVariableDeclarator variableDeclarator);

    protected abstract void RegisterIneligibleFieldsAction(
        HashSet<string> fieldNames, ConcurrentSet<IFieldSymbol> ineligibleFields, SemanticModel semanticModel, SyntaxNode codeBlock, CancellationToken cancellationToken);

    private static bool CanConvert(
        IFieldSymbol field,
        INamedTypeSymbol? suppressMessageAttributeType,
        [NotNullWhen(true)] out TFieldDeclaration? fieldDeclaration,
        [NotNullWhen(true)] out TVariableDeclarator? variableDeclarator,
        CancellationToken cancellationToken)
    {
        fieldDeclaration = null;
        variableDeclarator = null;

        if (!field.CanBeReferencedByName)
            return false;

        // Don't want to remove constants and volatile fields.
        if (field.IsConst || field.IsVolatile)
            return false;

        // Only support this for private fields.  It limits the scope of hte program
        // we have to analyze to make sure this is safe to do.
        if (field.DeclaredAccessibility != Accessibility.Private)
            return false;

        // Can't remove the field if it has attributes on it.
        var attributes = field.GetAttributes();
        foreach (var attribute in attributes)
        {
            if (!Equals(attribute.AttributeClass, suppressMessageAttributeType))
                return false;
        }

        var fieldReference = field.DeclaringSyntaxReferences[0];
        if (fieldReference.GetSyntax(cancellationToken) is not TVariableDeclarator { Parent.Parent: TFieldDeclaration fieldDecl } variableDecl)
            return false;

        fieldDeclaration = fieldDecl;
        variableDeclarator = variableDecl;
        return true;
    }

    private static bool CanConvert(
        IFieldSymbol field,
        IPropertySymbol property)
    {
        if (property.IsIndexer)
            return false;

        // Field and property have to be in the same type.
        if (!field.ContainingType.Equals(property.ContainingType))
            return false;

        // Property and field have to agree on type.
        if (!property.Type.Equals(field.Type))
            return false;

        // Field and property should match in static-ness
        if (field.IsStatic != property.IsStatic)
            return false;

        return true;
    }

    protected sealed override void InitializeWorker(AnalysisContext context)
        => context.RegisterSymbolStartAction(context =>
        {
            var namedType = (INamedTypeSymbol)context.Symbol;
            if (!ShouldAnalyze(context, namedType))
                return;

            // Serializable types can depend on fields (and their order).  Don't report these
            // properties in that case.
            if (namedType.IsSerializable)
                return;

            // Create two inner analyzers to analyze this type.  The first looks for fields/properties we can convert to
            // a full auto prop (like `int X { get; private set; }`).  The second looks for fields/properties we can
            // convert to a semi-auto-prop (like `int X { get => field.Trim(); }`) if the first doesn't succeed.
            var simpleAutoPropertyAnalyzer = new FullAutoPropertyAnalyzer((TAnalyzer)this, context);
            var semiAutoPropertyAnalyzer = new SemiAutoPropertyAnalyzer((TAnalyzer)this, context);

            context.RegisterSymbolEndAction(context =>
            {
                // Ensure we cleanup the analyzers once done with this named type.
                using (simpleAutoPropertyAnalyzer)
                using (semiAutoPropertyAnalyzer)
                {
                    // Keep track of the fields/properties we converted to auto-props.  We won't offer to convert those
                    // to semi-auto-properties.
                    var convertedToAutoProperty = ConcurrentDictionaryPool<IFieldSymbol, IPropertySymbol>.Allocate();

                    simpleAutoPropertyAnalyzer.OnSymbolEnd(convertedToAutoProperty, context);
                    semiAutoPropertyAnalyzer.OnSymbolEnd(convertedToAutoProperty, context);

                    ConcurrentDictionaryPool<IFieldSymbol, IPropertySymbol>.Free(convertedToAutoProperty);
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

    private void ReportDiagnostics(
        TPropertyDeclaration propertyDeclaration,
        TFieldDeclaration fieldDeclaration,
        TVariableDeclarator variableDeclarator,
        NotificationOption2 notification,
        ImmutableDictionary<string, string?>? properties,
        SymbolAnalysisContext context)
    {
        var fieldNode = this.GetFieldNode(fieldDeclaration, variableDeclarator);

        // Now add diagnostics to both the field and the property saying we can convert it to 
        // an auto property.  For each diagnostic store both location so we can easily retrieve
        // them when performing the code fix.
        var additionalLocations = ImmutableArray.Create(
            propertyDeclaration.GetLocation(),
            variableDeclarator.GetLocation());

        // Place the appropriate marker on the field depending on the user option.
        var diagnostic1 = DiagnosticHelper.Create(
            this.Descriptor,
            fieldNode.GetLocation(),
            notification,
            context.Options,
            additionalLocations: additionalLocations,
            properties: properties);

        // Also, place a hidden marker on the property.  If they bring up a lightbulb
        // there, they'll be able to see that they can convert it to an auto-prop.
        var diagnostic2 = Diagnostic.Create(
            this.Descriptor,
            propertyDeclaration.GetLocation(),
            additionalLocations: additionalLocations,
            properties: properties);

        context.ReportDiagnostic(diagnostic1);
        context.ReportDiagnostic(diagnostic2);
    }
}
