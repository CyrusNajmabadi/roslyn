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
    private readonly struct SimpleAutoPropertyAnalyzer : IDisposable
    {
        private readonly TAnalyzer _analyzer;

        private readonly INamedTypeSymbol _containingType;

        private readonly HashSet<string> _fieldNames;
        public readonly ConcurrentStack<AnalysisResult> AnalysisResults;
        public readonly ConcurrentSet<IFieldSymbol> IneligibleFields;
        public readonly ConcurrentDictionary<IFieldSymbol, ConcurrentSet<SyntaxNode>> NonConstructorFieldWrites;

        public SimpleAutoPropertyAnalyzer(
            TAnalyzer analyzer,
            SymbolStartAnalysisContext context,
            HashSet<string> fieldNames)
        {
            _analyzer = analyzer;

            _containingType = (INamedTypeSymbol)context.Symbol;
            _fieldNames = fieldNames;

            AnalysisResults = s_analysisResultPool.Allocate();
            IneligibleFields = s_fieldSetPool.Allocate();
            NonConstructorFieldWrites = s_fieldWriteLocationPool.Allocate();

            var self = this;
            context.RegisterSyntaxNodeAction(
                context => self.AnalyzePropertyDeclaration(context),
                _analyzer.PropertyDeclarationKind);
            context.RegisterCodeBlockStartAction<TSyntaxKind>(context =>
            {
                self._analyzer.RegisterIneligibleFieldsAction(self._fieldNames, self.IneligibleFields, context.SemanticModel, context.CodeBlock, context.CancellationToken);
                self.RegisterNonConstructorFieldWrites(context.SemanticModel, context.CodeBlock, context.CancellationToken);
            });
        }

        public void Dispose()
        {
            // Cleanup after doing all our work.
            _analyzer._fieldNamesPool.ClearAndFree(_fieldNames);

            s_analysisResultPool.ClearAndFree(AnalysisResults);
            s_fieldSetPool.ClearAndFree(IneligibleFields);

            foreach (var (_, nodeSet) in NonConstructorFieldWrites)
                s_nodeSetPool.ClearAndFree(nodeSet);

            s_fieldWriteLocationPool.ClearAndFree(NonConstructorFieldWrites);
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

            if (property.IsIndexer)
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

            // Serializable types can depend on fields (and their order).  Don't report these
            // properties in that case.
            if (_containingType.IsSerializable)
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

            // Only support this for private fields.  It limits the scope of hte program
            // we have to analyze to make sure this is safe to do.
            if (getterField.DeclaredAccessibility != Accessibility.Private)
                return;

            // If the user made the field readonly, we only want to convert it to a property if we
            // can keep it readonly.
            if (getterField.IsReadOnly && !_analyzer.SupportsReadOnlyProperties(compilation))
                return;

            // Field and property have to be in the same type.
            if (!_containingType.Equals(getterField.ContainingType))
                return;

            // Property and field have to agree on type.
            if (!property.Type.Equals(getterField.Type))
                return;

            // Mutable value type fields are mutable unless they are marked read-only
            if (!getterField.IsReadOnly && getterField.Type.IsMutableValueType() != false)
                return;

            // Don't want to remove constants and volatile fields.
            if (getterField.IsConst || getterField.IsVolatile)
                return;

            // Field and property should match in static-ness
            if (getterField.IsStatic != property.IsStatic)
                return;

            var fieldReference = getterField.DeclaringSyntaxReferences[0];
            if (fieldReference.GetSyntax(cancellationToken) is not TVariableDeclarator { Parent.Parent: TFieldDeclaration fieldDeclaration } variableDeclarator)
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

            // Can't remove the field if it has attributes on it.
            var attributes = getterField.GetAttributes();
            var suppressMessageAttributeType = compilation.SuppressMessageAttributeType();
            foreach (var attribute in attributes)
            {
                if (attribute.AttributeClass != suppressMessageAttributeType)
                    return;
            }

            if (!_analyzer.CanConvert(property))
                return;

            // Looks like a viable property/field to convert into an auto property.
            AnalysisResults.Push(new AnalysisResult(property, getterField, propertyDeclaration, fieldDeclaration, variableDeclarator, notification));
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

                AddFieldWrite(NonConstructorFieldWrites, field, identifierName);
            }
        }
    }
}
