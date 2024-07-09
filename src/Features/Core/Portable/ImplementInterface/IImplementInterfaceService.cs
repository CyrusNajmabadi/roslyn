// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.ImplementType;

namespace Microsoft.CodeAnalysis.ImplementInterface;

internal interface IImplementInterfaceInfo
{

}

internal readonly record struct ImplementInterfaceOptions(
    bool Explicitly,
    bool Abstractly,
    bool OnlyRemaining,
    ISymbol? ThroughMember)
{
}

internal interface IImplementInterfaceService : ILanguageService
{
    Task<Document> ImplementInterfaceAsync(Document document, ImplementTypeGenerationOptions options, SyntaxNode node, CancellationToken cancellationToken);

    Task<IImplementInterfaceInfo> GetInfoAsync(Document document, SyntaxNode node, CancellationToken cancellationToken);
    Task<Document> ImplementInterfaceAsync(
        Document document,
        IImplementInterfaceInfo info,
        ImplementTypeGenerationOptions typeGenerationOptions,
        ImplementInterfaceOptions implementInterfaceOptions,
        CancellationToken cancellationToken);

    // ImmutableArray<CodeAction> GetCodeActions(Document document, ImplementTypeGenerationOptions options, SemanticModel model, SyntaxNode node, CancellationToken cancellationToken);
}
