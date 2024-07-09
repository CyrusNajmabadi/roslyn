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
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.ImplementInterface;

using static ImplementHelpers;

internal abstract partial class AbstractImplementInterfaceCodeFixProvider<TTypeSyntax> : CodeFixProvider
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
                var actions = await ComputeActionsAsync(document, info, options, cancellationToken).ConfigureAwait(false);
                if (!actions.IsEmpty)
                {
                    context.RegisterFixes(actions, context.Diagnostics);
                    return;
                }
            }
        }
    }

    private async Task<ImmutableArray<CodeAction>> ComputeActionsAsync(
        Document document,
        IImplementInterfaceInfo info,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        return ComputeActionsEnumerable(document, info, options, cancellationToken).ToImmutableArray();
    }

    private IEnumerable<CodeAction> ComputeActionsEnumerable(
        Document document,
        IImplementInterfaceInfo state,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        if (state == null)
        {
            yield break;
        }

        var service = document.GetRequiredLanguageService<IImplementInterfaceService>();

        if (state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented.Length > 0)
        {
            var totalMemberCount = 0;
            var inaccessibleMemberCount = 0;

            foreach (var (_, members) in state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented)
            {
                foreach (var member in members)
                {
                    totalMemberCount++;

                    if (IsLessAccessibleThan(member, state.ClassOrStructType))
                    {
                        inaccessibleMemberCount++;
                    }
                }
            }

            // If all members to implement are inaccessible, then "Implement interface" codeaction
            // will be the same as "Implement interface explicitly", so there is no point in having both of them
            if (totalMemberCount != inaccessibleMemberCount)
            {
                yield return ImplementInterfaceCodeAction.CreateImplementCodeAction(service, document, options, state);
            }

            if (ShouldImplementDisposePattern(state, explicitly: false))
            {
                yield return ImplementInterfaceWithDisposePatternCodeAction.CreateImplementWithDisposePatternCodeAction(document, options, state);
            }

            var delegatableMembers = GetDelegatableMembers(state, cancellationToken);
            foreach (var member in delegatableMembers)
            {
                yield return ImplementInterfaceCodeAction.CreateImplementThroughMemberCodeAction(this, document, options, state, member);
            }

            if (state.ClassOrStructType.IsAbstract)
            {
                yield return ImplementInterfaceCodeAction.CreateImplementAbstractlyCodeAction(this, document, options, state);
            }
        }

        if (state.MembersWithoutExplicitImplementation.Length > 0)
        {
            yield return ImplementInterfaceCodeAction.CreateImplementExplicitlyCodeAction(this, document, options, state);

            if (ShouldImplementDisposePattern(state, explicitly: true))
            {
                yield return ImplementInterfaceWithDisposePatternCodeAction.CreateImplementExplicitlyWithDisposePatternCodeAction(this, document, options, state);
            }
        }

        if (AnyImplementedImplicitly(state))
        {
            yield return ImplementInterfaceCodeAction.CreateImplementRemainingExplicitlyCodeAction(this, document, options, state);
        }
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
