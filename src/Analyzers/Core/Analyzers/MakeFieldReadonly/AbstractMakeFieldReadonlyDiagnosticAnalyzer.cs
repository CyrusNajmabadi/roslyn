﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.MakeFieldReadonly;

internal abstract class AbstractMakeFieldReadonlyDiagnosticAnalyzer<TSyntaxKind, TThisExpression>()
    : AbstractBuiltInCodeStyleDiagnosticAnalyzer(
        IDEDiagnosticIds.MakeFieldReadonlyDiagnosticId,
        EnforceOnBuildValues.MakeFieldReadonly,
        CodeStyleOptions2.PreferReadonly,
        new LocalizableResourceString(nameof(AnalyzersResources.Add_readonly_modifier), AnalyzersResources.ResourceManager, typeof(AnalyzersResources)),
        new LocalizableResourceString(nameof(AnalyzersResources.Make_field_readonly), AnalyzersResources.ResourceManager, typeof(AnalyzersResources)))
    where TSyntaxKind : struct, Enum
    where TThisExpression : SyntaxNode
{
    protected abstract ISyntaxKinds SyntaxKinds { get; }
    protected abstract ISemanticFacts SemanticFacts { get; }

    public sealed override DiagnosticAnalyzerCategory GetAnalyzerCategory() => DiagnosticAnalyzerCategory.SemanticDocumentAnalysis;

    // We need to analyze generated code to get callbacks for read/writes to non-generated members in generated code.
    protected sealed override GeneratedCodeAnalysisFlags GeneratedCodeAnalysisFlags => GeneratedCodeAnalysisFlags.Analyze;

    protected sealed override void InitializeWorker(AnalysisContext context)
    {
        context.RegisterCompilationStartAction(context =>
        {
            // State map for fields:
            //  'isCandidate' : Indicates whether the field is a candidate to be made readonly based on it's options.
            //  'written'     : Indicates if there are any writes to the field outside the constructor and field initializer.
            var fieldStateMap = new ConcurrentDictionary<IFieldSymbol, (bool isCandidate, bool written)>();

            var compilation = context.Compilation;
            var threadStaticAttribute = compilation.ThreadStaticAttributeType();
            var dataContractAttribute = compilation.DataContractAttribute();
            var dataMemberAttribute = compilation.DataMemberAttribute();
            var inlineArrayAttribute = compilation.InlineArrayAttributeType();

            // We register following actions in the compilation:
            // 1. A symbol action for field symbols to ensure the field state is initialized for every field in
            //    the compilation.
            // 2. An operation action for field references to detect if a candidate field is written outside
            //    constructor and field initializer, and update field state accordingly.
            // 3. A symbol start/end action for named types to report diagnostics for candidate fields that were
            //    not written outside constructor and field initializer.

            context.RegisterSymbolAction(AnalyzeFieldSymbol, SymbolKind.Field);

            context.RegisterSymbolStartAction(context =>
            {
                if (!ShouldAnalyze(context, (INamedTypeSymbol)context.Symbol))
                    return;

                context.RegisterOperationAction(AnalyzeOperation, OperationKind.FieldReference);

                // Can't allow changing the fields to readonly if the struct overwrites itself.  e.g. `this = default;`
                var writesToThis = false;
                context.RegisterSyntaxNodeAction(context =>
                {
                    writesToThis = writesToThis || this.SemanticFacts.IsWrittenTo(context.SemanticModel, context.Node, context.CancellationToken);
                }, SyntaxKinds.Convert<TSyntaxKind>(SyntaxKinds.ThisExpression));

                context.RegisterSymbolEndAction(context =>
                {
                    if (writesToThis)
                        return;

                    OnSymbolEnd(context);
                });
            }, SymbolKind.NamedType);

            return;

            // Local functions.
            void AnalyzeFieldSymbol(SymbolAnalysisContext symbolContext)
            {
                var field = (IFieldSymbol)symbolContext.Symbol;
                if (!symbolContext.ShouldAnalyzeLocation(GetDiagnosticLocation(field)))
                    return;

                _ = TryGetOrInitializeFieldState(field, symbolContext.Options, symbolContext.CancellationToken);
            }

            void AnalyzeOperation(OperationAnalysisContext operationContext)
            {
                var fieldReference = (IFieldReferenceOperation)operationContext.Operation;
                var (isCandidate, written) = TryGetOrInitializeFieldState(fieldReference.Field, operationContext.Options, operationContext.CancellationToken);

                // Ignore fields that are not candidates or have already been written outside the constructor/field initializer.
                if (!isCandidate || written)
                    return;

                // Check if this is a field write outside constructor and field initializer, and update field state accordingly.
                if (!IsFieldWrite(fieldReference, operationContext.ContainingSymbol))
                    return;

                var field = fieldReference.Field;
                Debug.Assert(fieldStateMap.ContainsKey(field.OriginalDefinition));

                fieldStateMap[field.OriginalDefinition] = (isCandidate: true, written: true);
            }

            void OnSymbolEnd(SymbolAnalysisContext symbolEndContext)
            {
                // Report diagnostics for candidate fields that are not written outside constructor and field initializer.
                var members = ((INamedTypeSymbol)symbolEndContext.Symbol).GetMembers();
                foreach (var member in members)
                {
                    if (member is IFieldSymbol field && fieldStateMap.TryRemove(field.OriginalDefinition, out var value))
                    {
                        var (isCandidate, written) = value;
                        if (isCandidate && !written)
                        {
                            var option = GetCodeStyleOption(field, symbolEndContext.Options, out var location);
                            var diagnostic = DiagnosticHelper.Create(
                                Descriptor,
                                location,
                                option.Notification,
                                context.Options,
                                additionalLocations: null,
                                properties: null);
                            symbolEndContext.ReportDiagnostic(diagnostic);
                        }
                    }
                }
            }

            bool ShouldAnalyze(SymbolStartAnalysisContext context, INamedTypeSymbol namedType)
            {
                // Check if we have at least one candidate field in analysis scope.
                foreach (var member in namedType.GetMembers())
                {
                    if (member is IFieldSymbol field &&
                        IsCandidateField(field) &&
                        GetDiagnosticLocation(field) is { } location &&
                        context.ShouldAnalyzeLocation(location))
                    {
                        return true;
                    }
                }

                // We have to analyze nested types if containing type contains a candidate field in analysis scope.
                if (namedType.ContainingType is { } containingType)
                    return ShouldAnalyze(context, containingType);

                return false;
            }

            bool IsCandidateField(IFieldSymbol symbol)
            {
                if (symbol is not
                    {
                        DeclaredAccessibility: Accessibility.Private,
                        IsReadOnly: false,
                        IsConst: false,
                        IsImplicitlyDeclared: false,
                        Locations.Length: 1,
                        IsFixedSizeBuffer: false,
                    })
                {
                    return false;
                }

                if (symbol.Type.IsMutableValueType() != false)
                    return false;

                if (symbol.HasAttribute(threadStaticAttribute))
                    return false;

                if (IsDataContractSerializable(symbol, dataContractAttribute, dataMemberAttribute))
                    return false;

                // The private instance field inside an inline-array is not allowed to be readonly.
                if (!symbol.IsStatic && symbol.ContainingType.HasAttribute(inlineArrayAttribute))
                    return false;

                return true;
            }

            static bool IsDataContractSerializable(IFieldSymbol symbol, INamedTypeSymbol? dataContractAttribute, INamedTypeSymbol? dataMemberAttribute)
            {
                if (dataContractAttribute is null || dataMemberAttribute is null)
                    return false;

                return symbol.HasAttribute(dataMemberAttribute)
                    && symbol.ContainingType.HasAttribute(dataContractAttribute);
            }

            // Method to get or initialize the field state.
            (bool isCandidate, bool written) TryGetOrInitializeFieldState(IFieldSymbol fieldSymbol, AnalyzerOptions options, CancellationToken cancellationToken)
            {
                if (!IsCandidateField(fieldSymbol))
                    return default;

                if (fieldStateMap.TryGetValue(fieldSymbol.OriginalDefinition, out var result))
                    return result;

                result = ComputeInitialFieldState(fieldSymbol, options, cancellationToken);
                return fieldStateMap.GetOrAdd(fieldSymbol.OriginalDefinition, result);
            }

            // Method to compute the initial field state.
            (bool isCandidate, bool written) ComputeInitialFieldState(
                IFieldSymbol field,
                AnalyzerOptions options,
                CancellationToken cancellationToken)
            {
                Debug.Assert(IsCandidateField(field));

                var option = GetCodeStyleOption(field, options, out var location);
                if (option == null
                    || !option.Value
                    || ShouldSkipAnalysis(location.SourceTree!, options, compilation.Options, option.Notification, cancellationToken))
                {
                    return default;
                }

                return (isCandidate: true, written: false);
            }
        });
    }

    private static Location GetDiagnosticLocation(IFieldSymbol field)
        => field.Locations[0];

    private static bool IsWrittenTo(IOperation? operation, ISymbol owningSymbol)
    {
        if (operation is null)
            return false;

        var result = operation.GetValueUsageInfo(owningSymbol);
        if (result.IsWrittenTo())
            return true;

        // Accessing an indexer/property off of a value type will read/write the value type depending on how the
        // indexer/property itself is used.
        return operation is { Type.IsValueType: true, Parent: IPropertyReferenceOperation }
            ? IsWrittenTo(operation.Parent, owningSymbol)
            : false;
    }

    private static bool IsFieldWrite(IFieldReferenceOperation fieldReference, ISymbol owningSymbol)
    {
        // Check if the underlying member is being written or a writable reference to the member is taken.
        if (!IsWrittenTo(fieldReference, owningSymbol))
            return false;

        // Writes to fields inside constructor are ignored, except for the below cases:
        //  1. Instance reference of an instance field being written is not the instance being initialized by the constructor.
        //  2. Field is being written inside a lambda or local function.

        // Check if we are in the constructor of the containing type of the written field.
        var isInInstanceConstructor = owningSymbol.IsConstructor();
        var isInStaticConstructor = owningSymbol.IsStaticConstructor();

        // If we're not in a constructor, this is definitely a write to the field that would prevent it from
        // becoming readonly.
        if (!isInInstanceConstructor && !isInStaticConstructor)
            return true;

        var field = fieldReference.Field;

        // Follow 'strict' C#/VB behavior. Has to be opted into by user with feature-flag "strict", but is
        // actually what the languages specify as correct.  Specifically the actual types of the containing
        // type and field-reference-containing type must be the same (not just their OriginalDefinition
        // types).
        //
        // https://github.com/dotnet/roslyn/blob/8770fb62a36157ed4ca38a16a0283d27321a01a7/src/Compilers/CSharp/Portable/Binder/Binder.ValueChecks.cs#L1201-L1203
        // https//github.com/dotnet/roslyn/blob/93d3aa1a2cf1790b1a0fe2d120f00987d50445c0/src/Compilers/VisualBasic/Portable/Binding/Binder_Expressions.vb#L1868-L1871
        if (!field.ContainingType.Equals(owningSymbol.ContainingType))
            return true;

        if (isInStaticConstructor)
        {
            // For static fields, ensure that we are in the static constructor.
            if (!field.IsStatic)
                return true;
        }
        else
        {
            // For instance fields, ensure that the instance reference is being initialized by the constructor.
            Debug.Assert(isInInstanceConstructor);
            if (fieldReference.Instance?.Kind != OperationKind.InstanceReference ||
                fieldReference.IsTargetOfObjectMemberInitializer())
            {
                return true;
            }
        }

        // Finally, ensure that the write is not inside a lambda or local function.
        if (fieldReference.TryGetContainingAnonymousFunctionOrLocalFunction() is not null)
            return true;

        return false;
    }

    private static CodeStyleOption2<bool> GetCodeStyleOption(IFieldSymbol field, AnalyzerOptions options, out Location diagnosticLocation)
    {
        diagnosticLocation = GetDiagnosticLocation(field);
        return options.GetAnalyzerOptions(diagnosticLocation.SourceTree!).PreferReadonly;
    }
}
