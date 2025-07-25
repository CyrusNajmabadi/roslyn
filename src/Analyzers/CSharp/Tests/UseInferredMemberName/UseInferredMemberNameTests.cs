﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.UseInferredMemberName;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editor.CSharp.UnitTests.Diagnostics;
using Microsoft.CodeAnalysis.Test.Utilities;
using Roslyn.Test.Utilities;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.CodeAnalysis.Editor.CSharp.UnitTests.InferredMemberName;

[Trait(Traits.Feature, Traits.Features.CodeActionsUseInferredMemberName)]
public sealed class UseInferredMemberNameTests : AbstractCSharpDiagnosticProviderBasedUserDiagnosticTest_NoEditor
{
    public UseInferredMemberNameTests(ITestOutputHelper logger)
      : base(logger)
    {
    }

    internal override (DiagnosticAnalyzer, CodeFixProvider) CreateDiagnosticProviderAndFixer(Workspace workspace)
        => (new CSharpUseInferredMemberNameDiagnosticAnalyzer(), new CSharpUseInferredMemberNameCodeFixProvider());

    private static readonly CSharpParseOptions s_parseOptions =
        CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.Latest);

    [Fact]
    public Task TestInferredTupleName()
        => TestAsync(
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    var t = ([||]a: a, 2);
                }
            }
            """,
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    var t = (a, 2);
                }
            }
            """, new(parseOptions: s_parseOptions));

    [Fact, WorkItem("https://github.com/dotnet/roslyn/issues/24480")]
    public Task TestInferredTupleName_WithAmbiguity()
        => TestMissingAsync(
            """
            class C
            {
                void M()
                {
                    int alice = 1;
                    (int, int, string) t = ([||]alice: alice, alice, null);
                }
            }
            """, parameters: new TestParameters(parseOptions: s_parseOptions));

    [Fact]
    public Task TestInferredTupleNameAfterCommaWithCSharp6()
        => TestActionCountAsync(
            """
            class C
            {
                void M()
                {
                    int a = 2;
                    var t = (1, [||]a: a);
                }
            }
            """, count: 0, parameters: new TestParameters(CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.CSharp6)));

    [Fact]
    public Task TestInferredTupleNameAfterCommaWithCSharp7()
        => TestActionCountAsync(
            """
            class C
            {
                void M()
                {
                    int a = 2;
                    var t = (1, [||]a: a);
                }
            }
            """, count: 0, parameters: new TestParameters(CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.CSharp7)));

    [Fact]
    public Task TestFixAllInferredTupleNameWithTrivia()
        => TestAsync(
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    int b = 2;
                    var t = ( /*before*/ {|FixAllInDocument:a:|} /*middle*/ a /*after*/, /*before*/ b: /*middle*/ b /*after*/);
                }
            }
            """,
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    int b = 2;
                    var t = ( /*before*/  /*middle*/ a /*after*/, /*before*/  /*middle*/ b /*after*/);
                }
            }
            """, new(parseOptions: s_parseOptions));

    [Fact]
    public Task TestInferredAnonymousTypeMemberName()
        => TestAsync(
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    var t = new { [||]a= a, 2 };
                }
            }
            """,
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    var t = new { a, 2 };
                }
            }
            """, new(parseOptions: s_parseOptions));

    [Fact, WorkItem("https://github.com/dotnet/roslyn/issues/24480")]
    public Task TestInferredAnonymousTypeMemberName_WithAmbiguity()
        => TestMissingAsync(
            """
            class C
            {
                void M()
                {
                    int alice = 1;
                    var t = new { [||]alice=alice, alice };
                }
            }
            """, parameters: new TestParameters(parseOptions: s_parseOptions));

    [Fact]
    public Task TestFixAllInferredAnonymousTypeMemberNameWithTrivia()
        => TestAsync(
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    int b = 2;
                    var t = new { /*before*/ {|FixAllInDocument:a =|} /*middle*/ a /*after*/, /*before*/ b = /*middle*/ b /*after*/ };
                }
            }
            """,
            """
            class C
            {
                void M()
                {
                    int a = 1;
                    int b = 2;
                    var t = new { /*before*/  /*middle*/ a /*after*/, /*before*/  /*middle*/ b /*after*/ };
                }
            }
            """, new(parseOptions: s_parseOptions));
}
