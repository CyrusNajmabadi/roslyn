// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Composition;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Shared.Extensions;
using System;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.CodeActions;

namespace Microsoft.CodeAnalysis.MakeClassSealed;

[ExportCodeFixProvider(LanguageNames.CSharp, LanguageNames.VisualBasic, Name = nameof(MakeClassSealedCodeFixProvider)), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class MakeClassSealedCodeFixProvider() : SyntaxEditorBasedCodeFixProvider
{
    public override ImmutableArray<string> FixableDiagnosticIds => [IDEDiagnosticIds.MakeClassSealedDiagnosticId];

    protected override async Task FixAllAsync(Document document, ImmutableArray<Diagnostic> diagnostics, SyntaxEditor editor, CancellationToken cancellationToken)
    {
        var solution = document.Project.Solution;
        var generator = editor.Generator;

        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);

        foreach (var diagnostic in diagnostics)
        {
            var typeDeclaration = GetTypeDeclaration(diagnostic, cancellationToken);
            if (await HasDerivedClassesAsync(solution, semanticModel, typeDeclaration, cancellationToken).ConfigureAwait(false))
                continue;

            var sealedTypeDeclaration = generator.WithModifiers(typeDeclaration, DeclarationModifiers.Sealed);
            editor.ReplaceNode(typeDeclaration, sealedTypeDeclaration);
        }
    }

    private static SyntaxNode GetTypeDeclaration(Diagnostic diagnostic, CancellationToken cancellationToken)
        => diagnostic.Location.FindNode(getInnermostNodeForTie: true, cancellationToken);

    private static async Task<bool> HasDerivedClassesAsync(
        Solution solution, SemanticModel semanticModel, SyntaxNode typeDeclaration, CancellationToken cancellationToken)
    {
        var namedType = (INamedTypeSymbol)semanticModel.GetRequiredDeclaredSymbol(typeDeclaration, cancellationToken);
        var derivedClasses = await SymbolFinder.FindDerivedClassesAsync(namedType, solution, cancellationToken: cancellationToken).ConfigureAwait(false);
        return derivedClasses.Any();
    }

    public override async Task RegisterCodeFixesAsync(CodeFixContext context)
    {
        var cancellationToken = context.CancellationToken;
        var document = context.Document;
        var diagnostic = context.Diagnostics.First();

        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        var typeDeclaration = GetTypeDeclaration(diagnostic, cancellationToken);
        if (await HasDerivedClassesAsync(document.Project.Solution, semanticModel, typeDeclaration, cancellationToken).ConfigureAwait(false))
            return;

        context.RegisterCodeFix(CodeAction.Create(
            "Make class sealed",
            cancellationToken =>
            {
                var root = semanticModel.SyntaxTree.GetRoot(cancellationToken);
                var generator = SyntaxGenerator.GetGenerator(document);

                var sealedTypeDeclaration = generator.WithModifiers(typeDeclaration, DeclarationModifiers.Sealed);
                var newRoot = root.ReplaceNode(typeDeclaration, sealedTypeDeclaration);
                return Task.FromResult(document.WithSyntaxRoot(newRoot));
            },
            "Make class sealed"),
            diagnostic);
    }
}
