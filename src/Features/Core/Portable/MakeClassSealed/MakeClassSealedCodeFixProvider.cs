// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Composition;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.MakeClassSealed;

[ExportCodeRefactoringProvider(LanguageNames.CSharp, LanguageNames.VisualBasic, Name = nameof(MakeClassSealedCodeRefactoringProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class MakeClassSealedCodeRefactoringProvider() : SyntaxEditorBasedCodeRefactoringProvider
{
    private static async Task<bool> HasDerivedClassesAsync(
        Solution solution, INamedTypeSymbol namedType, CancellationToken cancellationToken)
    {
        var derivedClasses = await SymbolFinder.FindDerivedClassesAsync(namedType, solution, transitive: false, cancellationToken: cancellationToken).ConfigureAwait(false);
        return derivedClasses.Any();
    }

    protected override ImmutableArray<FixAllScope> SupportedFixAllScopes => DefaultFixAllScopes;

    protected override async Task FixAllAsync(
        Document document,
        ImmutableArray<TextSpan> fixAllSpans,
        SyntaxEditor editor,
        string? equivalenceKey,
        CancellationToken cancellationToken)
    {
        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);

        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
        var syntaxFacts = document.GetRequiredLanguageService<ISyntaxFactsService>();

        using var _ = ArrayBuilder<SyntaxNode>.GetInstance(out var stack);
        stack.Push(root);

        while (stack.TryPop(out var current))
        {
            foreach (var child in current.ChildNodesAndTokens())
            {
                if (child.AsNode(out var childNode))
                {
                    if (syntaxFacts.IsBaseNamespaceDeclaration(childNode) ||
                        syntaxFacts.IsTypeDeclaration(childNode))
                    {
                        stack.Push(childNode);
                    }
                }
            }

            if (await CanBecomeSealedAsync(
                    document, semanticModel, current, cancellationToken).ConfigureAwait(false))
            {
                editor.ReplaceNode(
                    current,
                    static (currentTypeDeclaration, generator) => generator.WithModifiers(currentTypeDeclaration, DeclarationModifiers.Sealed));
            }
        }
    }

    private static async ValueTask<bool> CanBecomeSealedAsync(
        Document document,
        SemanticModel semanticModel,
        SyntaxNode typeDeclaration,
        CancellationToken cancellationToken)
    {
        var syntaxFacts = document.GetRequiredLanguageService<ISyntaxFactsService>();
        if (!syntaxFacts.IsTypeDeclaration(typeDeclaration))
            return false;

        if (semanticModel.GetDeclaredSymbol(typeDeclaration, cancellationToken) is not INamedTypeSymbol namedType)
            return false;

        if (namedType.TypeKind != TypeKind.Class)
            return false;

        if (namedType.IsAbstract)
            return false;

        if (namedType.IsSealed)
            return false;

        if (namedType.IsStatic)
            return false;

        if (IsPublic(namedType) && !namedType.Name.Contains("Test"))
            return false;

        if (await HasDerivedClassesAsync(document.Project.Solution, namedType, cancellationToken).ConfigureAwait(false))
            return false;

        return true;
    }

    public override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
    {
        var (document, span, cancellationToken) = context;
        if (span.Length > 0)
            return;

        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
        var token = root.FindToken(span.Start);
        var typeDeclaration = token.GetRequiredParent();

        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        if (!await CanBecomeSealedAsync(document, semanticModel, typeDeclaration, cancellationToken).ConfigureAwait(false))
            return;

        context.RegisterRefactoring(CodeAction.Create(
            "Make class sealed",
            cancellationToken =>
            {
                var root = semanticModel.SyntaxTree.GetRoot(cancellationToken);
                var generator = SyntaxGenerator.GetGenerator(document);

                var sealedTypeDeclaration = generator.WithModifiers(typeDeclaration, DeclarationModifiers.Sealed);
                var newRoot = root.ReplaceNode(typeDeclaration, sealedTypeDeclaration);
                return Task.FromResult(document.WithSyntaxRoot(newRoot));
            },
            "Make class sealed"));
    }

    private static bool IsPublic(INamedTypeSymbol namedType)
    {
        for (var current = namedType; current != null; current = current.ContainingType)
        {
            if (current.DeclaredAccessibility != Accessibility.Public)
                return false;
        }

        return true;
    }
}
