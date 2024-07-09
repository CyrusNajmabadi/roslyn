// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Diagnostics.Analyzers.NamingStyles;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;

namespace Microsoft.CodeAnalysis.ImplementInterface;

using static ImplementHelpers;

internal abstract partial class AbstractImplementInterfaceService : IImplementInterfaceService
{
    // C#: `Dispose(bool disposed)`.  VB: `Dispose(disposed As Boolean)`
    private static readonly SymbolDisplayFormat s_format = new(
        memberOptions: SymbolDisplayMemberOptions.IncludeParameters,
        parameterOptions: SymbolDisplayParameterOptions.IncludeName | SymbolDisplayParameterOptions.IncludeType,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes);

    private const string DisposingName = "disposing";

    public async Task<Document> ImplementIDisposableInterfaceAsync(
        Document document,
        ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> unimplementedMembers,
        bool explicitly,
        INamedTypeSymbol classType,
        SyntaxNode classDecl,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        var compilation = await document.Project.GetRequiredCompilationAsync(cancellationToken).ConfigureAwait(false);

        var disposedValueField = await CreateDisposedValueFieldAsync(
            document, classType, options, cancellationToken).ConfigureAwait(false);

        var disposeMethod = TryGetIDisposableDispose(compilation)!;
        var (disposableMethods, finalizer) = CreateDisposableMethods(
            compilation, document, explicitly, classType, disposeMethod, disposedValueField);

        // First, implement all the interfaces (except for IDisposable).
        var docWithCoreMembers = await GetUpdatedDocumentAsync(
            document,
            unimplementedMembers.WhereAsArray(m => !m.type.Equals(disposeMethod.ContainingType)),
            classType,
            classDecl,
            extraMembers: [disposedValueField],
            cancellationToken).ConfigureAwait(false);

        // Next, add the Dispose pattern methods at the end of the type (we want to keep all
        // the members together).
        var rootWithCoreMembers = await docWithCoreMembers.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

        var firstGeneratedMember = rootWithCoreMembers.GetAnnotatedNodes(CodeGenerator.Annotation).First();
        var typeDeclarationWithCoreMembers = firstGeneratedMember.Parent!;

        var context = new CodeGenerationContext(
            addImports: false,
            sortMembers: false,
            autoInsertionLocation: false);

        var info = await document.GetCodeGenerationInfoAsync(context, options.FallbackOptions, cancellationToken).ConfigureAwait(false);

        var typeDeclarationWithAllMembers = info.Service.AddMembers(
            typeDeclarationWithCoreMembers,
            disposableMethods,
            info,
            cancellationToken);

        var docWithAllMembers = docWithCoreMembers.WithSyntaxRoot(
            rootWithCoreMembers.ReplaceNode(
                typeDeclarationWithCoreMembers, typeDeclarationWithAllMembers));

        // Finally, add a commented out finalizer with the Dispose methods. We have to do
        // this ourselves as our code-gen helpers can create real methods, but not commented
        // out ones.
        return await AddFinalizerCommentAsync(docWithAllMembers, finalizer, cancellationToken).ConfigureAwait(false);
    }

    private (ImmutableArray<ISymbol>, SyntaxNode) CreateDisposableMethods(
        Compilation compilation,
        Document document,
        bool explicitly,
        INamedTypeSymbol classType,
        IMethodSymbol disposeMethod,
        IFieldSymbol disposedValueField)
    {
        var disposeImplMethod = CreateDisposeImplementationMethod(compilation, document, classType, disposeMethod, disposedValueField);

        var disposeMethodDisplayString = this.ToDisplayString(disposeImplMethod, s_format);

        var disposeInterfaceMethod = CreateDisposeInterfaceMethod(
            compilation, document, explicitly, classType, disposeMethod,
            disposedValueField, disposeMethodDisplayString);

        var g = document.GetRequiredLanguageService<SyntaxGenerator>();
        var finalizer = this.CreateFinalizer(g, classType, disposeMethodDisplayString);

        return (ImmutableArray.Create<ISymbol>(disposeImplMethod, disposeInterfaceMethod), finalizer);
    }

    private IMethodSymbol CreateDisposeImplementationMethod(
        Compilation compilation,
        Document document,
        INamedTypeSymbol classType,
        IMethodSymbol disposeMethod,
        IFieldSymbol disposedValueField)
    {
        var accessibility = classType.IsSealed
            ? Accessibility.Private
            : Accessibility.Protected;

        var modifiers = classType.IsSealed
            ? DeclarationModifiers.None
            : DeclarationModifiers.Virtual;

        var g = document.GetRequiredLanguageService<SyntaxGenerator>();

        // if (disposing)
        // {
        //     // TODO: dispose managed state...
        // }
        var ifDisposingStatement = g.IfStatement(g.IdentifierName(DisposingName), []);
        ifDisposingStatement = this.AddCommentInsideIfStatement(
            ifDisposingStatement,
            CreateCommentTrivia(g, FeaturesResources.TODO_colon_dispose_managed_state_managed_objects))
                .WithoutTrivia().WithTrailingTrivia(g.CarriageReturnLineFeed, g.CarriageReturnLineFeed);

        // TODO: free unmanaged ...
        // TODO: set large fields...
        // disposedValue = true
        var disposedValueEqualsTrueStatement = AddComments(g,
            FeaturesResources.TODO_colon_free_unmanaged_resources_unmanaged_objects_and_override_finalizer,
            FeaturesResources.TODO_colon_set_large_fields_to_null,
            g.AssignmentStatement(
                g.IdentifierName(disposedValueField.Name), g.TrueLiteralExpression()));

        var ifStatement = g.IfStatement(
            g.LogicalNotExpression(g.IdentifierName(disposedValueField.Name)),
            [ifDisposingStatement, disposedValueEqualsTrueStatement]);

        return CodeGenerationSymbolFactory.CreateMethodSymbol(
            disposeMethod,
            containingType: classType,
            accessibility: accessibility,
            modifiers: modifiers,
            name: disposeMethod.Name,
            parameters: ImmutableArray.Create(
                CodeGenerationSymbolFactory.CreateParameterSymbol(
                    compilation.GetSpecialType(SpecialType.System_Boolean),
                    DisposingName)),
            statements: [ifStatement]);
    }

    private async Task<IFieldSymbol> CreateDisposedValueFieldAsync(
        Document document,
        INamedTypeSymbol containingType,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        var rule = await document.GetApplicableNamingRuleAsync(
            SymbolKind.Field, Accessibility.Private, options.FallbackOptions, cancellationToken).ConfigureAwait(false);

        var requireAccessiblity = await GetAccessibilityModifiersRequiredAsync(document, options, cancellationToken).ConfigureAwait(false);

        var compilation = await document.Project.GetRequiredCompilationAsync(cancellationToken).ConfigureAwait(false);
        var boolType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var accessibilityLevel = requireAccessiblity is AccessibilityModifiersRequired.Never or AccessibilityModifiersRequired.OmitIfDefault
            ? Accessibility.NotApplicable
            : Accessibility.Private;

        var uniqueName = GenerateUniqueNameForDisposedValueField(containingType, rule);

        return CodeGenerationSymbolFactory.CreateFieldSymbol(
            default,
            accessibilityLevel,
            DeclarationModifiers.None,
            boolType, uniqueName);
    }

    private static string GenerateUniqueNameForDisposedValueField(INamedTypeSymbol containingType, NamingRule rule)
    {
        // Determine an appropriate name to call the new field.
        var baseName = rule.NamingStyle.CreateName(s_disposedValueNameParts);

        // Ensure that the name is unique in the containing type so we
        // don't stomp on an existing member.
        var uniqueName = NameGenerator.GenerateUniqueName(
            baseName, n => containingType.GetMembers(n).IsEmpty);
        return uniqueName;
    }

    /// <summary>
    /// This helper is implementing access to the editorconfig option. This would usually be done via <see cref="CodeFixOptionsProvider"/> but
    /// we do not have access to <see cref="CodeActionOptionsProvider"/> here since the code action implementation is also used to implement <see cref="IImplementInterfaceService "/>.
    /// TODO: remove - see https://github.com/dotnet/roslyn/issues/60990.
    /// </summary>
    private async ValueTask<AccessibilityModifiersRequired> GetAccessibilityModifiersRequiredAsync(
        Document document, ImplementTypeGenerationOptions options, CancellationToken cancellationToken)
    {
        var syntaxTree = await document.GetRequiredSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
        var configOptions = document.Project.AnalyzerOptions.AnalyzerConfigOptionsProvider.GetOptions(syntaxTree);

        if (configOptions.TryGetEditorConfigOption<CodeStyleOption2<AccessibilityModifiersRequired>>(CodeStyleOptions2.AccessibilityModifiersRequired, out var value))
        {
            return value.Value;
        }

        var fallbackFormattingOptions = await ((OptionsProvider<SyntaxFormattingOptions>)options.FallbackOptions).GetOptionsAsync(document.Project.Services, cancellationToken).ConfigureAwait(false);

        return fallbackFormattingOptions.AccessibilityModifiersRequired;
    }

    private static async Task<Document> AddFinalizerCommentAsync(
        Document document, SyntaxNode finalizer, CancellationToken cancellationToken)
    {
        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

        var lastGeneratedMember = root.GetAnnotatedNodes(CodeGenerator.Annotation)
                                      .OrderByDescending(n => n.SpanStart)
                                      .First();

        finalizer = finalizer.NormalizeWhitespace();
        var finalizerLines = finalizer.ToFullString().Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);

        var generator = document.GetRequiredLanguageService<SyntaxGenerator>();
        var finalizerComments = CreateCommentTrivia(generator, finalizerLines);

        var lastMemberWithComments = lastGeneratedMember.WithPrependedLeadingTrivia(
            finalizerComments.Insert(0, generator.CarriageReturnLineFeed)
                             .Add(generator.CarriageReturnLineFeed));

        var finalRoot = root.ReplaceNode(lastGeneratedMember, lastMemberWithComments);
        return document.WithSyntaxRoot(finalRoot);
    }

    private IMethodSymbol CreateDisposeInterfaceMethod(
        Compilation compilation,
        Document document,
        bool explicitly,
        INamedTypeSymbol classType,
        IMethodSymbol disposeMethod,
        IFieldSymbol disposedValueField,
        string disposeMethodDisplayString)
    {
        using var _ = ArrayBuilder<SyntaxNode>.GetInstance(out var statements);

        var g = document.GetRequiredLanguageService<SyntaxGenerator>();
        var syntaxFacts = document.GetRequiredLanguageService<ISyntaxFactsService>();

        // // Do not change...
        // Dispose(true);
        statements.Add(AddComment(g,
            string.Format(FeaturesResources.Do_not_change_this_code_Put_cleanup_code_in_0_method, disposeMethodDisplayString),
            g.ExpressionStatement(
                g.InvocationExpression(
                    g.IdentifierName(nameof(IDisposable.Dispose)),
                    g.Argument(DisposingName, RefKind.None, g.TrueLiteralExpression())))));

        // GC.SuppressFinalize(this);
        var gcType = compilation.GetTypeByMetadataName(typeof(GC).FullName!);
        if (gcType != null)
        {
            statements.Add(g.ExpressionStatement(
                g.InvocationExpression(
                    g.MemberAccessExpression(
                        g.TypeExpression(gcType),
                        nameof(GC.SuppressFinalize)),
                    g.ThisExpression())));
        }

        var modifiers = DeclarationModifiers.From(disposeMethod);
        modifiers = modifiers.WithIsAbstract(false);

        var explicitInterfaceImplementations = explicitly || !this.CanImplementImplicitly
            ? ImmutableArray.Create(disposeMethod) : default;

        var result = CodeGenerationSymbolFactory.CreateMethodSymbol(
            disposeMethod,
            accessibility: explicitly ? Accessibility.NotApplicable : Accessibility.Public,
            modifiers: modifiers,
            explicitInterfaceImplementations: explicitInterfaceImplementations,
            statements: statements.ToImmutable());

        return result;
    }

    protected static TNode AddComment<TNode>(SyntaxGenerator g, string comment, TNode node) where TNode : SyntaxNode
        => AddComments(g, [comment], node);

    protected static TNode AddComments<TNode>(SyntaxGenerator g, string comment1, string comment2, TNode node) where TNode : SyntaxNode
        => AddComments(g, [comment1, comment2,], node);

    protected static TNode AddComments<TNode>(SyntaxGenerator g, string[] comments, TNode node) where TNode : SyntaxNode
        => node.WithPrependedLeadingTrivia(CreateCommentTrivia(g, comments));

    protected static SyntaxTriviaList CreateCommentTrivia(SyntaxGenerator generator, params string[] comments)
    {
        using var _ = ArrayBuilder<SyntaxTrivia>.GetInstance(out var trivia);

        foreach (var comment in comments)
        {
            trivia.Add(generator.SingleLineComment(" " + comment));
            trivia.Add(generator.ElasticCarriageReturnLineFeed);
        }

        return new SyntaxTriviaList(trivia);
    }
}
