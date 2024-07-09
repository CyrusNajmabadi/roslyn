// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.ImplementInterface;

internal abstract class AbstractImplementInterfaceCodeFixProvider<TTypeSyntax> : CodeFixProvider
    where TTypeSyntax : SyntaxNode
{
    protected abstract bool IsTypeInBaseTypeList(TTypeSyntax typeSyntax);

    public sealed override FixAllProvider GetFixAllProvider()
        => WellKnownFixAllProviders.BatchFixer;

    public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
    {
        var document = context.Document;
        var span = context.Span;
        var cancellationToken = context.CancellationToken;

        var root = await document.GetRequiredSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

        var token = root.FindToken(span.Start);
        if (!token.Span.IntersectsWith(span))
            return;

        var service = document.GetRequiredLanguageService<IImplementInterfaceService>();

        var options = context.Options.GetImplementTypeGenerationOptions(document.Project.Services);
        foreach (var typeSyntax in token.Parent.GetAncestorsOrThis<TTypeSyntax>())
        {
            if (IsTypeInBaseTypeList(typeSyntax))
            {
                var info = await service.ComputeInfoAsync(document, typeSyntax, cancellationToken).ConfigureAwait(false);
                var actions = await ComputeActionsAsync(info, options, cancellationToken).ConfigureAwait(false);
                if (!actions.IsEmpty)
                {
                    context.RegisterFixes(actions, context.Diagnostics);
                    return;
                }
            }
        }
    }

    private async Task<ImmutableArray<CodeAction>> ComputeActionsAsync(
        IImplementInterfaceInfo info,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        throw new NotImplementedException();
    }
}
