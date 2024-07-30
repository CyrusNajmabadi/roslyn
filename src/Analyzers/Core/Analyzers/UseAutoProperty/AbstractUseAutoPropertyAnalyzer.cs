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
    /// <summary>
    /// ConcurrentStack as that's the only concurrent collection that supports 'Clear' in netstandard2.
    /// </summary>
    private static readonly ObjectPool<ConcurrentStack<AnalysisResult>> s_analysisResultPool = new(() => new());
    private static readonly ObjectPool<ConcurrentSet<IFieldSymbol>> s_fieldSetPool = new(() => []);
    private static readonly ObjectPool<ConcurrentSet<SyntaxNode>> s_nodeSetPool = new(() => []);
    private static readonly ObjectPool<ConcurrentDictionary<IFieldSymbol, ConcurrentSet<SyntaxNode>>> s_fieldWriteLocationPool = new(() => []);
    private static readonly ObjectPool<ConcurrentDictionary<IFieldSymbol, IPropertySymbol>> s_fieldToPropertyPool = new(() => []);

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
    protected abstract bool SupportsSemiAutoProperties(Compilation compilation);

    protected abstract bool CanExplicitInterfaceImplementationsBeFixed();
    protected abstract TExpression? GetFieldInitializer(TVariableDeclarator variable, CancellationToken cancellationToken);
    protected abstract TExpression? GetGetterExpression(IMethodSymbol getMethod, CancellationToken cancellationToken);
    protected abstract TExpression? GetSetterExpression(IMethodSymbol setMethod, SemanticModel semanticModel, CancellationToken cancellationToken);
    protected abstract SyntaxNode GetFieldNode(TFieldDeclaration fieldDeclaration, TVariableDeclarator variableDeclarator);

    protected abstract void RegisterIneligibleFieldsAction(
        HashSet<string> fieldNames, ConcurrentSet<IFieldSymbol> ineligibleFields, SemanticModel semanticModel, SyntaxNode codeBlock, CancellationToken cancellationToken);

    /// <summary>
    /// Analyzer that looks at all the fields in a type, finds those only referenced in a single property and suggests
    /// converting those to semi-auto-prop (using `field`).
    /// </summary>
    private readonly struct SemiAutoPropertyAnalyzer : IDisposable
    {
        private readonly TAnalyzer _analyzer;

        private readonly INamedTypeSymbol _containingType;

        private readonly HashSet<string> _fieldNames;
        private readonly ConcurrentSet<IFieldSymbol> _fieldsOfInterest;

        /// <summary>
        /// Fields we know cannot be converted.  A field cannot be converted if it is referenced *anywhere* outside of
        /// property.
        /// </summary>
        private readonly ConcurrentSet<IFieldSymbol> _ineligibleFields;

        /// <summary>
        /// The locations we see a particular field accessed from.  If a field is only referenced from a single property
        /// </summary>
        private readonly ConcurrentDictionary<IFieldSymbol, IPropertySymbol> _fieldToPropertyReference;

        public SemiAutoPropertyAnalyzer(
            TAnalyzer analyzer,
            SymbolStartAnalysisContext context)
        {
            _analyzer = analyzer;

            _containingType = (INamedTypeSymbol)context.Symbol;

            _fieldNames = _analyzer._fieldNamesPool.Allocate();
            _fieldsOfInterest = s_fieldSetPool.Allocate();
            _ineligibleFields = s_fieldSetPool.Allocate();
            _fieldToPropertyReference = s_fieldToPropertyPool.Allocate();
            var compilation = context.Compilation;

            if (_analyzer.SupportsSemiAutoProperties(compilation))
            {
                var cancellationToken = context.CancellationToken;
                var suppressMessageAttributeType = compilation.SuppressMessageAttributeType();
                foreach (var member in _containingType.GetMembers())
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    if (member is not IFieldSymbol field ||
                        !CanConvert(field, suppressMessageAttributeType, out _, out _, cancellationToken))
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
            s_fieldSetPool.ClearAndFree(_ineligibleFields);

            //foreach (var (_, nodeSet) in _propertiesAFieldIsReferencedFrom)
            //    s_nodeSetPool.ClearAndFree(nodeSet);

            s_fieldToPropertyPool.ClearAndFree(_fieldToPropertyReference);
        }

        private void AnalyzeCodeBlock(
            CodeBlockStartAnalysisContext<TSyntaxKind> context)
        {
            var cancellationToken = context.CancellationToken;
            var semanticModel = context.SemanticModel;
            var syntaxFacts = _analyzer.SyntaxFacts;
            var suppressMessageAttributeType = semanticModel.Compilation.SuppressMessageAttributeType();

            foreach (var identifierName in context.CodeBlock.DescendantNodesAndSelf().OfType<TIdentifierName>())
            {
                cancellationToken.ThrowIfCancellationRequested();

                // Quick textual check to avoid looking at anything that couldn't bind to a field.
                if (!_fieldNames.Contains(syntaxFacts.GetIdentifierOfIdentifierName(identifierName).ValueText))
                    continue;

                // ok, we're seeing an identifier that could be referencing a field we care about.
                var symbol = semanticModel.GetSymbolInfo(identifierName, cancellationToken);
                if (symbol.Symbol is not IFieldSymbol field)
                    continue;

                // Ignore a field reference to a field in some other type.
                var originalField = field.OriginalDefinition;
                if (!_fieldsOfInterest.Contains(originalField))
                    continue;

                if (!TryAnalyzeFieldReference(
                        field, semanticModel, identifierName, suppressMessageAttributeType, cancellationToken))
                {
                    // Was a field we care about, but was used in a way that prevents conversion.  Add it to the
                    // ineligible list
                    _ineligibleFields.Add(originalField);
                    continue;
                }
            }
        }

        private bool TryAnalyzeFieldReference(
            IFieldSymbol field,
            SemanticModel semanticModel,
            TIdentifierName identifierName,
            INamedTypeSymbol? suppressMessageAttributeType,
            CancellationToken cancellationToken)
        {
            // If the field is referenced through a generic instantiation, then we can't make this an auto prop.
            if (!field.Equals(field.OriginalDefinition))
                return false;

            // if this field is referenced outside of a property then we can't convert this.
            var propertyDeclaration = identifierName.GetAncestor<TPropertyDeclaration>();
            if (propertyDeclaration is null)
                return false;

            var propertySymbol = (IPropertySymbol)semanticModel.GetRequiredDeclaredSymbol(propertyDeclaration, cancellationToken);

            // if this field is referenced in multiple properties then we can't convert it.
            var existingPropertyReference = _fieldToPropertyReference.GetOrAdd(field, propertySymbol);
            if (existingPropertyReference != null && !existingPropertyReference.Equals(propertySymbol))
                return false;

            // if the field and property are not complimentary, then we can't convert this.
            if (!CanConvert(field, suppressMessageAttributeType, out _, out _, cancellationToken))
                return false;

            return true;
        }
    }

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
        IPropertySymbol property,
        INamedTypeSymbol? suppressMessageAttributeType,
        [NotNullWhen(true)] out TFieldDeclaration? fieldDeclaration,
        [NotNullWhen(true)] out TVariableDeclarator? variableDeclarator,
        CancellationToken cancellationToken)
    {
        if (!CanConvert(field, suppressMessageAttributeType, out fieldDeclaration, out variableDeclarator, cancellationToken))
            return false;

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
                using (simpleAutoPropertyAnalyzer)
                using (semiAutoPropertyAnalyzer)
                {
                    // Keep track of the fields/properties we converted to auto-props.  We won't offer to convert those
                    // to semi-auto-properties.
                    using var _ = s_fieldToPropertyPool.GetPooledObject(out var convertedToAutoProperty);

                    simpleAutoPropertyAnalyzer.OnSymbolEnd(convertedToAutoProperty, context);
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

    private sealed record AnalysisResult(
        IPropertySymbol Property,
        IFieldSymbol Field,
        TPropertyDeclaration PropertyDeclaration,
        TFieldDeclaration FieldDeclaration,
        TVariableDeclarator VariableDeclarator,
        NotificationOption2 Notification);
}
