// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Structure;
using Microsoft.CodeAnalysis.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.Editor.UnitTests.Structure;

[UseExportProvider]
[Trait(Traits.Feature, Traits.Features.Outlining)]
public sealed class BlockStructureServiceTests
{
    [Fact]
    public Task TestSimpleLambda()
        => TestAsync(
            """
            using System.Linq;
            class C
            {
                static void Goo()
                {
                    var q = Enumerable.Range(1, 100).Where(x =>
                    {
                        return x % 2 == 0;
                    });
                }
            }
            """,
            expectedSpanCount: 4);

    [Fact]
    public Task TestParenthesizedLambda()
        => TestAsync(
            """
            using System.Linq;
            class C
            {
                static void Goo()
                {
                    var q = Enumerable.Range(1, 100).Where((x) =>
                    {
                        return x % 2 == 0;
                    });
                }
            }
            """,
            expectedSpanCount: 4);

    [Fact]
    public Task TestAnonymousDelegate()
        => TestAsync(
            """
            using System.Linq;
            class C
            {
                static void Goo()
                {
                    var q = Enumerable.Range(1, 100).Where(delegate (int x)
                    {
                        return x % 2 == 0;
                    });
                }
            }
            """,
            expectedSpanCount: 4);

    [Fact]
    public Task TestTwoInvocationExpressionsThreeLines()
        => TestAsync(
            """
            var x = MyMethod1(MyMethod2(
                "",
                "");
            """,
            expectedSpanCount: 1,
            expectedSpanStart: 27);

    private static async Task<ImmutableArray<BlockSpan>> GetSpansFromWorkspaceAsync(
        TestWorkspace workspace)
    {
        var hostDocument = workspace.Documents.First();
        var document = workspace.CurrentSolution.GetDocument(hostDocument.Id);
        var outliningService = document.GetLanguageService<BlockStructureService>();
        var options = BlockStructureOptions.Default;

        var structure = await outliningService.GetBlockStructureAsync(document, options, CancellationToken.None);
        return structure.Spans;
    }

    private static async Task TestAsync(
        string code,
        int expectedSpanCount,
        int? expectedSpanStart = null)
    {
        using var workspace = TestWorkspace.CreateCSharp(code);
        var spans = await GetSpansFromWorkspaceAsync(workspace);

        Assert.Equal(expectedSpanCount, spans.Length);
        if (expectedSpanStart.HasValue)
        {
            Assert.Equal(expectedSpanStart.Value, spans[0].TextSpan.Start);
        }
    }
}
