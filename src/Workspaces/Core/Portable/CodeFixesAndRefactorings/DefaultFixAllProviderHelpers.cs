// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Remote;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeFixesAndRefactorings;

/// <summary>
/// Default implementation of a <see cref="FixAllProvider"/> that efficiently handles the dispatch logic for fixing
/// entire solutions.  Used by <see cref="BatchFixAllProvider"/> and <see cref="DocumentBasedFixAllProvider"/>.
/// </summary>
internal static class DefaultFixAllProviderHelpers
{
    public static async Task<CodeAction?> GetFixAsync<TFixAllContext, TFixAllContextWitness>(
        string title,
        TFixAllContext fixAllContext,
        TFixAllContextWitness witness,
        Func<TFixAllContext, ImmutableArray<TFixAllContext>, Task<Solution?>> fixAllContextsAsync)
        where TFixAllContextWitness : struct, IFixAllContextWitness<TFixAllContext>
    {

        // We're about to do a lot of computation to compute all the diagnostics needed and to perform the
        // changes.  Keep this solution alive on the OOP side so that we never drop it and then resync it
        // (which would cause us to drop/recreate compilations, skeletons and sg docs.
        using var _ = await RemoteKeepAliveSession.CreateAsync(
            witness.GetSolution(fixAllContext), witness.GetCancellationToken(fixAllContext)).ConfigureAwait(false);

        var solution = witness.GetScope(fixAllContext) switch
        {
            FixAllScope.Document or FixAllScope.ContainingMember or FixAllScope.ContainingType
                => await GetDocumentFixesAsync(fixAllContext, fixAllContextsAsync).ConfigureAwait(false),
            FixAllScope.Project => await GetProjectFixesAsync(fixAllContext, witness, fixAllContextsAsync).ConfigureAwait(false),
            FixAllScope.Solution => await GetSolutionFixesAsync(fixAllContext, witness, fixAllContextsAsync).ConfigureAwait(false),
            var scope => throw ExceptionUtilities.UnexpectedValue(scope),
        };

        if (solution == null)
            return null;

        return CodeAction.Create(
            title, _ => Task.FromResult(solution));
    }

    private static Task<Solution?> GetDocumentFixesAsync<TFixAllContext>(
        TFixAllContext fixAllContext,
        Func<TFixAllContext, ImmutableArray<TFixAllContext>, Task<Solution?>> fixAllContextsAsync)
        => fixAllContextsAsync(fixAllContext, [fixAllContext]);

    private static Task<Solution?> GetProjectFixesAsync<TFixAllContext, TFixAllContextWitness>(
        TFixAllContext fixAllContext,
        TFixAllContextWitness witness,
        Func<TFixAllContext, ImmutableArray<TFixAllContext>, Task<Solution?>> fixAllContextsAsync)
        where TFixAllContextWitness : struct, IFixAllContextWitness<TFixAllContext>
        => fixAllContextsAsync(fixAllContext, [witness.With(fixAllContext, (document: null, witness.GetProject(fixAllContext)))]);

    private static Task<Solution?> GetSolutionFixesAsync<TFixAllContext, TFixAllContextWitness>(
        TFixAllContext fixAllContext,
        TFixAllContextWitness witness,
        Func<TFixAllContext, ImmutableArray<TFixAllContext>, Task<Solution?>> fixAllContextsAsync)
        where TFixAllContextWitness : struct, IFixAllContextWitness<TFixAllContext>
    {
        var solution = witness.GetSolution(fixAllContext);
        var dependencyGraph = solution.GetProjectDependencyGraph();

        // Walk through each project in topological order, determining and applying the diagnostics for each
        // project.  We do this in topological order so that the compilations for successive projects are readily
        // available as we just computed them for dependent projects.  If we were to do it out of order, we might
        // start with a project that has a ton of dependencies, and we'd spend an inordinate amount of time just
        // building the compilations for it before we could proceed.
        //
        // By processing one project at a time, we can also let go of a project once done with it, allowing us to
        // reclaim lots of the memory so we don't overload the system while processing a large solution.
        //
        // Note: we have to filter down to projects of the same language as the FixAllContext points at a
        // CodeFixProvider, and we can't call into providers of different languages with diagnostics from a
        // different language.
        var sortedProjects = dependencyGraph.GetTopologicallySortedProjects()
                                            .Select(solution.GetRequiredProject)
                                            .Where(p => p.Language == witness.GetProject(fixAllContext).Language);
        return fixAllContextsAsync(
            fixAllContext,
            sortedProjects.SelectAsArray(p => witness.With(fixAllContext, (document: null, project: p), scope: FixAllScope.Project)));
    }
}
