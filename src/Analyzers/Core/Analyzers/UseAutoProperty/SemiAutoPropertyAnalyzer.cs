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

internal abstract partial class AbstractUseAutoPropertyAnalyzer<
    TAnalyzer,
    TSyntaxKind,
    TPropertyDeclaration,
    TConstructorDeclaration,
    TFieldDeclaration,
    TVariableDeclarator,
    TExpression,
    TIdentifierName> where TAnalyzer : AbstractUseAutoPropertyAnalyzer<
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

        public void OnSymbolEnd(ConcurrentDictionary<IFieldSymbol, IPropertySymbol> convertedToAutoProperty, SymbolAnalysisContext context)
        {
            foreach (var field in _fieldsOfInterest)
            {
                // Ignore fields we know we can't convert.
                if (_ineligibleFields.Contains(field))
                    continue;

                // Ignore fields we already offered to convert to a straight auto prop.
                if (convertedToAutoProperty.ContainsKey(field))
                    continue;


            }
        }
    }
}
