//// Licensed to the .NET Foundation under one or more agreements.
//// The .NET Foundation licenses this file to you under the MIT license.
//// See the LICENSE file in the project root for more information.

//using System;
//using System.Threading;
//using Microsoft.CodeAnalysis.CodeFixes;

//namespace Microsoft.CodeAnalysis.CodeFixesAndRefactorings;

///// <summary>
///// Represents a FixAllContext for code fixes or refactorings. 
///// </summary>
//internal interface IFixAllContext
//{
//    IFixAllState State { get; }
//    IFixAllProvider FixAllProvider { get; }
//    Solution Solution { get; }
//    Project Project { get; }
//    Document? Document { get; }
//    object Provider { get; }
//    FixAllScope Scope { get; }
//    string? CodeActionEquivalenceKey { get; }
//    CancellationToken CancellationToken { get; }
//    IProgress<CodeAnalysisProgress> Progress { get; }

//    string GetDefaultFixAllTitle();
//    IFixAllContext With(
//        Optional<(Document? document, Project project)> documentAndProject = default,
//        Optional<FixAllScope> scope = default,
//        Optional<string?> codeActionEquivalenceKey = default,
//        Optional<CancellationToken> cancellationToken = default);
//}

using System;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeFixesAndRefactorings;

internal interface IFixAllContextWitness<TFixAllContext>
{
    CancellationToken GetCancellationToken(TFixAllContext fixAllContext);
    string GetDefaultFixAllTitle(TFixAllContext fixAllContext);
    IProgress<CodeAnalysisProgress> GetProgress(TFixAllContext fixAllContext);
    Project GetProject(TFixAllContext fixAllContext);
    FixAllScope GetScope(TFixAllContext fixAllContext);
    Solution GetSolution(TFixAllContext fixAllContext);
    IFixAllState<TFixAllContext> GetState(TFixAllContext fixAllContext);

    TFixAllContext With(
        TFixAllContext fixAllContext,
        Optional<(Document? document, Project project)> documentAndProject = default,
        Optional<FixAllScope> scope = default,
        Optional<string?> codeActionEquivalenceKey = default,
        Optional<CancellationToken> cancellationToken = default);
}
