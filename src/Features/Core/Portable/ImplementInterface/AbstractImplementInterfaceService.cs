// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.ImplementInterface;

internal abstract partial class AbstractImplementInterfaceService() : IImplementInterfaceService
{
    protected abstract string ToDisplayString(IMethodSymbol disposeImplMethod, SymbolDisplayFormat format);

    public abstract bool CanImplementImplicitly { get; }
    public abstract bool HasHiddenExplicitImplementation { get; }

    public abstract bool AllowDelegateAndEnumConstraints(ParseOptions options);
    public abstract SyntaxNode AddCommentInsideIfStatement(SyntaxNode ifDisposingStatement, SyntaxTriviaList trivia);
    public abstract SyntaxNode CreateFinalizer(SyntaxGenerator generator, INamedTypeSymbol classType, string disposeMethodDisplayString);

    protected abstract bool TryInitializeState(
        Document document, SemanticModel model, SyntaxNode interfaceNode, CancellationToken cancellationToken, out SyntaxNode classOrStructDecl, out INamedTypeSymbol classOrStructType, out ImmutableArray<INamedTypeSymbol> interfaceTypes);

    public async Task<Document> ImplementInterfaceAsync(
        Document document, ImplementTypeGenerationOptions options, SyntaxNode node, CancellationToken cancellationToken)
    {
        using (Logger.LogBlock(FunctionId.Refactoring_ImplementInterface, cancellationToken))
        {
            var model = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            var state = State.Generate(this, document, model, node, cancellationToken);
            if (state == null)
            {
                return document;
            }

            // TODO: https://github.com/dotnet/roslyn/issues/60990
            // While implementing just one default action, like in the case of pressing enter after interface name in VB,
            // choose to implement with the dispose pattern as that's the Dev12 behavior.
            var action = ShouldImplementDisposePattern(state, explicitly: false)
                ? ImplementInterfaceWithDisposePatternCodeAction.CreateImplementWithDisposePatternCodeAction(this, document, options, state)
                : ImplementInterfaceCodeAction.CreateImplementCodeAction(this, document, options, state);

            return await action.GetUpdatedDocumentAsync(cancellationToken).ConfigureAwait(false);
        }
    }

    public async Task<IImplementInterfaceInfo> ComputeInfoAsync(Document document, SyntaxNode node, CancellationToken cancellationToken)
    {
        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        var state = State.Generate(this, document, semanticModel, node, cancellationToken);
        return state;
    }
}
