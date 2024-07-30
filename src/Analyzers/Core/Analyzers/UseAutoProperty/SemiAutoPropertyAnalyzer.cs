// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.UseAutoProperty;

using static UseAutoPropertyHelpers;

internal abstract partial class AbstractUseAutoPropertyAnalyzer<
    TAnalyzer,
    TSyntaxKind,
    TPropertyDeclaration,
    TConstructorDeclaration,
    TFieldDeclaration,
    TVariableDeclarator,
    TExpression,
    TIdentifierName>
{
    /// <summary>
    /// Analyzer that looks at all the fields in a type, finds those only referenced in a single property and suggests
    /// converting those to semi-auto-prop (using `field`).
    /// </summary>
    private readonly struct SemiAutoPropertyAnalyzer : IDisposable
    {
        private readonly TAnalyzer _analyzer;

        private readonly INamedTypeSymbol _containingType;

        private readonly HashSet<string> _fieldNames;

        private readonly ConcurrentSet<IFieldSymbol> _fieldsOfInterest = ConcurrentSetPool<IFieldSymbol>.Allocate();

        /// <summary>
        /// Fields we know cannot be converted.  A field cannot be converted if it is referenced *anywhere* outside of
        /// property.
        /// </summary>
        private readonly ConcurrentSet<IFieldSymbol> _ineligibleFields = ConcurrentSetPool<IFieldSymbol>.Allocate();

        private readonly ConcurrentSet<IFieldSymbol> _constructorWrites = ConcurrentSetPool<IFieldSymbol>.Allocate();
        private readonly ConcurrentSet<IFieldSymbol> _nonConstructorWrites = ConcurrentSetPool<IFieldSymbol>.Allocate();

        /// <summary>
        /// The locations we see a particular field accessed from.  If a field is only referenced from a single property
        /// </summary>
        private readonly ConcurrentDictionary<IFieldSymbol, (TPropertyDeclaration propertyDeclaration, IPropertySymbol property)> _fieldToPropertyReference;

        public SemiAutoPropertyAnalyzer(
            TAnalyzer analyzer,
            SymbolStartAnalysisContext context)
        {
            _analyzer = analyzer;

            _containingType = (INamedTypeSymbol)context.Symbol;

            _fieldNames = _analyzer._fieldNamesPool.Allocate();
            _fieldToPropertyReference = ConcurrentDictionaryPool<IFieldSymbol, (TPropertyDeclaration propertyDeclaration, IPropertySymbol property)>.Allocate();
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
            ConcurrentSetPool<IFieldSymbol>.Free(_fieldsOfInterest);
            ConcurrentSetPool<IFieldSymbol>.Free(_ineligibleFields);
            ConcurrentSetPool<IFieldSymbol>.Free(_constructorWrites);
            ConcurrentSetPool<IFieldSymbol>.Free(_nonConstructorWrites);
            ConcurrentDictionaryPool<IFieldSymbol, (TPropertyDeclaration propertyDeclaration, IPropertySymbol property)>.Free(_fieldToPropertyReference);
        }

        private void AnalyzeCodeBlock(
            CodeBlockStartAnalysisContext<TSyntaxKind> context)
        {
            var self = this;
            var cancellationToken = context.CancellationToken;
            var semanticModel = context.SemanticModel;

            var syntaxFacts = _analyzer.SyntaxFacts;
            var semanticFacts = _analyzer.SemanticFacts;

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

                if (!TryAnalyzeFieldReference(field, identifierName))
                {
                    // Was a field we care about, but was used in a way that prevents conversion.  Add it to the
                    // ineligible list
                    _ineligibleFields.Add(originalField);
                    continue;
                }
            }

            return;

            bool TryAnalyzeFieldReference(
                IFieldSymbol field,
                TIdentifierName identifierName)
            {
                // `field` can't be used inside of a nameof() expression.
                if (semanticFacts.IsInsideNameOfExpression(semanticModel, identifierName, cancellationToken))
                    return false;

                // If the field is referenced through a generic instantiation, then we can't make this an auto prop.
                if (!field.Equals(field.OriginalDefinition))
                    return false;

                // Check for common blockers.
                if (!CanConvert(field, suppressMessageAttributeType, out _, out _, cancellationToken))
                    return false;

                // if this field is referenced outside of a property then we can't convert this.
                var propertyDeclaration = identifierName.GetAncestor<TPropertyDeclaration>();
                if (propertyDeclaration != null)
                {
                    var property = (IPropertySymbol)semanticModel.GetRequiredDeclaredSymbol(propertyDeclaration, cancellationToken);

                    // if this field is referenced in multiple properties then we can't convert it.
                    var (existingPropertyDeclaration, existingProperty) = self._fieldToPropertyReference.GetOrAdd(field, (propertyDeclaration, property));
                    if (existingProperty != null && !existingProperty.Equals(property))
                        return false;

                    // if the field and property are not complimentary, then we can't convert this.

                    if (!CanConvert(field, property))
                        return false;

                    if (existingProperty is null)
                    {
                        // first time seeing this property.  ensure the property is one we can convert.
                        var preferAutoProps = context.GetAnalyzerOptions().PreferAutoProperties;
                        if (!preferAutoProps.Value)
                            return false;

                        // Avoid reporting diagnostics when the feature is disabled. This primarily avoids reporting the
                        // hidden helper diagnostic which is not otherwise influenced by the severity settings.
                        var notification = preferAutoProps.Notification;
                        if (notification.Severity == ReportDiagnostic.Suppress)
                            return false;
                    }

                    return true;
                }

                // Access outside of a property constructor.  This may be ok if it's a *write* only. A write will be ok
                // if we're going to give the property a basic `set;` method when converting it.
                if (syntaxFacts.IsLeftSideOfAssignment(identifierName))
                {
                    var constructorDeclaration = identifierName.GetAncestor<TConstructorDeclaration>();
                    var writesSet = constructorDeclaration != null
                        ? self._constructorWrites
                        : self._nonConstructorWrites;

                    writesSet.Add(field);
                }

                return false;
            }
        }

        public void OnSymbolEnd(
            ConcurrentDictionary<IFieldSymbol, IPropertySymbol> convertedToAutoProperty,
            SymbolAnalysisContext context)
        {
            var cancellationToken = context.CancellationToken;
            var compilation = context.Compilation;
            var suppressMessageAttribute = compilation.SuppressMessageAttributeType();

            foreach (var field in _fieldsOfInterest)
            {
                // Ignore fields we know we can't convert.
                if (_ineligibleFields.Contains(field))
                    continue;

                // Ignore fields we already offered to convert to a straight auto prop.
                if (convertedToAutoProperty.ContainsKey(field))
                    continue;

                if (!_fieldToPropertyReference.TryGetValue(field, out var propertyInfo))
                    continue;

                // We checked this originally when we added to the set of fields to look at.
                Contract.ThrowIfFalse(CanConvert(
                    field, suppressMessageAttribute, out var fieldDeclaration, out var variableDeclarator, cancellationToken));

                var propertyDeclaration = propertyInfo.propertyDeclaration;
                var notification = context.Options.GetAnalyzerOptions(
                    propertyDeclaration.SyntaxTree).PreferAutoProperties.Notification;

                _analyzer.ReportDiagnostics(
                    propertyDeclaration,
                    fieldDeclaration,
                    variableDeclarator,
                    notification,
                    SemiAutoProperties.Add(FieldName, field.Name),
                    context);
            }
        }
    }
}
