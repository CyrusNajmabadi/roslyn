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
    TIdentifierName>
{
    private readonly struct FullAutoPropertyAnalyzer : IDisposable
    {
        private readonly record struct AnalysisResult(
            IPropertySymbol Property,
            IFieldSymbol Field,
            TPropertyDeclaration PropertyDeclaration,
            TFieldDeclaration FieldDeclaration,
            TVariableDeclarator VariableDeclarator,
            NotificationOption2 Notification);

        /// <summary>
        /// ConcurrentStack as that's the only concurrent collection that supports 'Clear' in netstandard2.
        /// </summary>
        private static readonly ObjectPool<ConcurrentStack<AnalysisResult>> s_analysisResultPool = new(() => new());

        private readonly TAnalyzer _analyzer;

        /// <summary>
        /// The current type we're analyzing.
        /// </summary>
        private readonly INamedTypeSymbol _containingType;

        /// <summary>
        /// The set of names of fields in this type.  Used to help us bind only identifiers that could bind to a field,
        /// allowing us to avoid the majority of binding.
        /// </summary>
        private readonly HashSet<string> _fieldNames;

        /// <summary>
        /// Fields we've determined cannot be removed.
        /// </summary>
        private readonly ConcurrentSet<IFieldSymbol> _ineligibleFields;

        /// <summary>
        /// Locations (outside of a constructor) where a field has been written to.
        /// </summary>
        private readonly ConcurrentDictionary<IFieldSymbol, ConcurrentSet<SyntaxNode>> _nonConstructorFieldWrites;

        /// <summary>
        /// Final set of candidates we believe we can convert to an auto-prop.
        /// </summary>
        private readonly ConcurrentStack<AnalysisResult> _analysisResults;

        public FullAutoPropertyAnalyzer(
            TAnalyzer analyzer,
            SymbolStartAnalysisContext context)
        {
            _analyzer = analyzer;

            _containingType = (INamedTypeSymbol)context.Symbol;

            // Record the names of all the fields in this type.  We can use this to greatly reduce the amount of
            // binding we need to perform when looking for restrictions in the type.
            _fieldNames = _analyzer._fieldNamesPool.Allocate();
            foreach (var member in _containingType.GetMembers())
            {
                if (member is IFieldSymbol field)
                    _fieldNames.Add(field.Name);
            }

            _analysisResults = s_analysisResultPool.Allocate();
            _ineligibleFields = ConcurrentSetPool<IFieldSymbol>.Allocate();
            _nonConstructorFieldWrites = ConcurrentDictionaryPool<IFieldSymbol, SyntaxNode>.AllocateMulti();

            var self = this;
            context.RegisterSyntaxNodeAction(
                self.AnalyzePropertyDeclaration, _analyzer.PropertyDeclarationKind);
            context.RegisterCodeBlockStartAction<TSyntaxKind>(context =>
            {
                self._analyzer.RegisterIneligibleFieldsAction(self._fieldNames, self._ineligibleFields, context.SemanticModel, context.CodeBlock, context.CancellationToken);
                self.RegisterNonConstructorFieldWrites(context.SemanticModel, context.CodeBlock, context.CancellationToken);
            });
        }

        public void Dispose()
        {
            // Cleanup after doing all our work.
            _analyzer._fieldNamesPool.ClearAndFree(_fieldNames);

            s_analysisResultPool.ClearAndFree(_analysisResults);

            ConcurrentSetPool<IFieldSymbol>.Free(_ineligibleFields);
            ConcurrentDictionaryPool<IFieldSymbol, SyntaxNode>.Free(_nonConstructorFieldWrites);
        }

        private void AnalyzePropertyDeclaration(SyntaxNodeAnalysisContext context)
        {
            var cancellationToken = context.CancellationToken;
            var semanticModel = context.SemanticModel;
            var compilation = semanticModel.Compilation;

            var propertyDeclaration = (TPropertyDeclaration)context.Node;
            if (semanticModel.GetDeclaredSymbol(propertyDeclaration, cancellationToken) is not IPropertySymbol property)
                return;

            if (!_containingType.Equals(property.ContainingType))
                return;

            // The property can't be virtual.  We don't know if it is overridden somewhere.  If it 
            // is, then calls to it may not actually assign to the field.
            if (property.IsVirtual || property.IsOverride || property.IsSealed)
                return;

            if (property.IsWithEvents)
                return;

            if (property.Parameters.Length > 0)
                return;

            // Need at least a getter.
            if (property.GetMethod == null)
                return;

            if (!_analyzer.CanExplicitInterfaceImplementationsBeFixed() && property.ExplicitInterfaceImplementations.Length != 0)
                return;

            var preferAutoProps = context.GetAnalyzerOptions().PreferAutoProperties;
            if (!preferAutoProps.Value)
                return;

            // Avoid reporting diagnostics when the feature is disabled. This primarily avoids reporting the hidden
            // helper diagnostic which is not otherwise influenced by the severity settings.
            var notification = preferAutoProps.Notification;
            if (notification.Severity == ReportDiagnostic.Suppress)
                return;

            var getterField = _analyzer.GetGetterField(semanticModel, property.GetMethod, cancellationToken);
            if (getterField == null)
                return;

            // If the user made the field readonly, we only want to convert it to a property if we
            // can keep it readonly.
            if (getterField.IsReadOnly && !_analyzer.SupportsReadOnlyProperties(compilation))
                return;

            // Check for common things blocking conversion
            if (!CanConvert(
                    getterField, compilation.SuppressMessageAttributeType(),
                    out var fieldDeclaration, out var variableDeclarator, cancellationToken))
            {
                return;
            }

            if (!CanConvert(getterField, property))
                return;

            // Mutable value type fields are mutable unless they are marked read-only
            if (!getterField.IsReadOnly && getterField.Type.IsMutableValueType() != false)
                return;

            // A setter is optional though.
            var setMethod = property.SetMethod;
            if (setMethod != null)
            {
                var setterField = _analyzer.GetSetterField(semanticModel, setMethod, cancellationToken);
                // If there is a getter and a setter, they both need to agree on which field they are 
                // writing to.
                if (setterField != getterField)
                    return;
            }

            var initializer = _analyzer.GetFieldInitializer(variableDeclarator, cancellationToken);
            if (initializer != null && !_analyzer.SupportsPropertyInitializer(compilation))
                return;

            if (!_analyzer.CanConvert(property))
                return;

            // Looks like a viable property/field to convert into an auto property.
            _analysisResults.Push(new AnalysisResult(property, getterField, propertyDeclaration, fieldDeclaration, variableDeclarator, notification));
        }

        private void RegisterNonConstructorFieldWrites(
            SemanticModel semanticModel,
            SyntaxNode codeBlock,
            CancellationToken cancellationToken)
        {
            if (codeBlock.FirstAncestorOrSelf<TConstructorDeclaration>() != null)
                return;

            var semanticFacts = _analyzer.SemanticFacts;
            var syntaxFacts = _analyzer.SyntaxFacts;
            foreach (var identifierName in codeBlock.DescendantNodesAndSelf().OfType<TIdentifierName>())
            {
                var identifier = syntaxFacts.GetIdentifierOfIdentifierName(identifierName);
                if (!_fieldNames.Contains(identifier.ValueText))
                    continue;

                if (semanticModel.GetSymbolInfo(identifierName, cancellationToken).Symbol is not IFieldSymbol field)
                    continue;

                if (!semanticFacts.IsWrittenTo(semanticModel, identifierName, cancellationToken))
                    continue;

                AddFieldWrite(_nonConstructorFieldWrites, field, identifierName);
            }
        }

        public void OnSymbolEnd(
            ConcurrentDictionary<IFieldSymbol, IPropertySymbol> convertedToAutoProperty,
            SymbolAnalysisContext context)
        {
            foreach (var result in _analysisResults)
            {
                // C# specific check.
                if (_ineligibleFields.Contains(result.Field))
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
                        _nonConstructorFieldWrites.TryGetValue(result.Field, out var writeLocations1) &&
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
                    _nonConstructorFieldWrites.TryGetValue(result.Field, out var writeLocations2) &&
                    writeLocations2.Any(loc => !loc.Ancestors().Contains(result.PropertyDeclaration)))
                {
                    continue;
                }

                convertedToAutoProperty[result.Field] = result.Property;

                _analyzer.ReportDiagnostics(
                    result.PropertyDeclaration,
                    result.FieldDeclaration,
                    result.VariableDeclarator,
                    result.Notification,
                    properties: null,
                    context);
            }
        }
    }
}
