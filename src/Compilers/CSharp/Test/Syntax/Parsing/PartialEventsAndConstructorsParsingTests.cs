// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Xunit;
using Xunit.Abstractions;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests;

public sealed class PartialEventsAndConstructorsParsingTests(ITestOutputHelper output) : ParsingTests(output)
{
    private sealed class CSharp14_Preview()
        : CombinatorialValuesAttribute(LanguageVersion.CSharp14, LanguageVersion.Preview);

    private sealed class CSharp13_CSharp14_Preview()
        : CombinatorialValuesAttribute(LanguageVersion.CSharp13, LanguageVersion.CSharp14, LanguageVersion.Preview);

    [Theory, CombinatorialData]
    public void Event_Tree([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            partial class C
            {
                partial event Action E;
                partial event Action E { add { } remove { } }
            }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.EventFieldDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.EventKeyword);
                    N(SyntaxKind.VariableDeclaration);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "Action");
                        }
                        N(SyntaxKind.VariableDeclarator);
                        {
                            N(SyntaxKind.IdentifierToken, "E");
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.EventDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.EventKeyword);
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "Action");
                    }
                    N(SyntaxKind.IdentifierToken, "E");
                    N(SyntaxKind.AccessorList);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.AddAccessorDeclaration);
                        {
                            N(SyntaxKind.AddKeyword);
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.RemoveAccessorDeclaration);
                        {
                            N(SyntaxKind.RemoveKeyword);
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_Multiple([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E, F;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
                N(SyntaxKind.CommaToken);
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "F");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_Initializer([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E = null;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                    N(SyntaxKind.EqualsValueClause);
                    {
                        N(SyntaxKind.EqualsToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_Multiple_Initializer([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E, F = null;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
                N(SyntaxKind.CommaToken);
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.EqualsValueClause);
                    {
                        N(SyntaxKind.EqualsToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_Multiple_Initializers([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E = null, F = null;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                    N(SyntaxKind.EqualsValueClause);
                    {
                        N(SyntaxKind.EqualsToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                }
                N(SyntaxKind.CommaToken);
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.EqualsValueClause);
                    {
                        N(SyntaxKind.EqualsToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_PartialAfterEvent([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            event partial Action E;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,22): error CS1003: Syntax error, ',' expected
            // event partial Action E;
            Diagnostic(ErrorCode.ERR_SyntaxError, "E").WithArguments(",").WithLocation(1, 22));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "partial");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_PartialAfterType([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            event Action partial E;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,22): error CS1003: Syntax error, ',' expected
            // event Action partial E;
            Diagnostic(ErrorCode.ERR_SyntaxError, "E").WithArguments(",").WithLocation(1, 22));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "partial");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_PartialAfterPublic([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            public partial event Action E;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PublicKeyword);
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_PartialBeforePublic([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial public event Action E;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.PublicKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Definition_DoublePartial([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial partial event Action E;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();

        var compilation = CreateCompilation(
            "partial class C { partial partial event System.Action E; }",
            parseOptions: TestOptions.Regular.WithLanguageVersion(langVersion));

        if (langVersion == LanguageVersion.CSharp13)
        {
            compilation.VerifyDiagnostics(
                // (1,27): error CS1004: Duplicate 'partial' modifier
                // partial class C { partial partial event System.Action E; }
                Diagnostic(ErrorCode.ERR_DuplicateModifier, "partial").WithArguments("partial").WithLocation(1, 27),
                // (1,55): error CS9275: Partial member 'C.E' must have an implementation part.
                // partial class C { partial partial event System.Action E; }
                Diagnostic(ErrorCode.ERR_PartialMemberMissingImplementation, "E").WithArguments("C.E").WithLocation(1, 55),
                // (1,55): error CS8703: The modifier 'partial' is not valid for this item in C# 13.0. Please use language version '14.0' or greater.
                // partial class C { partial partial event System.Action E; }
                Diagnostic(ErrorCode.ERR_InvalidModifierForLanguageVersion, "E").WithArguments("partial", "13.0", "14.0").WithLocation(1, 55));
        }
        else
        {
            compilation.VerifyDiagnostics(
                // (1,27): error CS1004: Duplicate 'partial' modifier
                // partial class C { partial partial event System.Action E; }
                Diagnostic(ErrorCode.ERR_DuplicateModifier, "partial").WithArguments("partial").WithLocation(1, 27),
                // (1,55): error CS9275: Partial member 'C.E' must have an implementation part.
                // partial class C { partial partial event System.Action E; }
                Diagnostic(ErrorCode.ERR_PartialMemberMissingImplementation, "E").WithArguments("C.E").WithLocation(1, 55));
        }
    }

    [Theory, CombinatorialData]
    public void Event_Definition_MissingRest([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,14): error CS1031: Type expected
            // partial event
            Diagnostic(ErrorCode.ERR_TypeExpected, "").WithLocation(1, 14),
            // (1,14): error CS1514: { expected
            // partial event
            Diagnostic(ErrorCode.ERR_LbraceExpected, "").WithLocation(1, 14),
            // (1,14): error CS1513: } expected
            // partial event
            Diagnostic(ErrorCode.ERR_RbraceExpected, "").WithLocation(1, 14));

        N(SyntaxKind.EventDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            M(SyntaxKind.IdentifierName);
            {
                M(SyntaxKind.IdentifierToken);
            }
            M(SyntaxKind.IdentifierToken);
            M(SyntaxKind.AccessorList);
            {
                M(SyntaxKind.OpenBraceToken);
                M(SyntaxKind.CloseBraceToken);
            }
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Implementation([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E { add { } remove { } }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "Action");
            }
            N(SyntaxKind.IdentifierToken, "E");
            N(SyntaxKind.AccessorList);
            {
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.AddAccessorDeclaration);
                {
                    N(SyntaxKind.AddKeyword);
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.RemoveAccessorDeclaration);
                {
                    N(SyntaxKind.RemoveKeyword);
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Implementation_Multiple([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E, F { add { } remove { } }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,27): error CS1003: Syntax error, ',' expected
            // partial event Action E, F { add { } remove { } }
            Diagnostic(ErrorCode.ERR_SyntaxError, "{").WithArguments(",").WithLocation(1, 27),
            // (1,49): error CS1002: ; expected
            // partial event Action E, F { add { } remove { } }
            Diagnostic(ErrorCode.ERR_SemicolonExpected, "").WithLocation(1, 49));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "E");
                }
                N(SyntaxKind.CommaToken);
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "F");
                }
            }
            M(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Implementation_PartialAfterEvent([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            event partial Action E { add { } remove { } }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,22): error CS1003: Syntax error, ',' expected
            // event partial Action E { add { } remove { } }
            Diagnostic(ErrorCode.ERR_SyntaxError, "E").WithArguments(",").WithLocation(1, 22),
            // (1,46): error CS1002: ; expected
            // event partial Action E { add { } remove { } }
            Diagnostic(ErrorCode.ERR_SemicolonExpected, "").WithLocation(1, 46));

        N(SyntaxKind.EventFieldDeclaration);
        {
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "partial");
                }
                N(SyntaxKind.VariableDeclarator);
                {
                    N(SyntaxKind.IdentifierToken, "Action");
                }
            }
            M(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Implementation_SemicolonAccessors([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial event Action E { add; remove; }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.EventDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "Action");
            }
            N(SyntaxKind.IdentifierToken, "E");
            N(SyntaxKind.AccessorList);
            {
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.AddAccessorDeclaration);
                {
                    N(SyntaxKind.AddKeyword);
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.RemoveAccessorDeclaration);
                {
                    N(SyntaxKind.RemoveKeyword);
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.CloseBraceToken);
            }
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_Implementation_PartialAccessors([CSharp14_Preview] LanguageVersion langVersion)
    {
        const string source = "partial event Action E { partial add; partial remove; }";
        var parseOptions = TestOptions.Regular.WithLanguageVersion(langVersion);

        UsingDeclaration(source, parseOptions);
        CreateCompilation($$"""
            using System;

            partial class C
            {
                partial event Action E;
                {{source}}
            }
            """, parseOptions: parseOptions).VerifyDiagnostics(
            // (6,30): error CS1609: Modifiers cannot be placed on event accessor declarations
            //     partial event Action E { partial add; partial remove; }
            Diagnostic(ErrorCode.ERR_NoModifiersOnAccessor, "partial").WithLocation(6, 30),
            // (6,41): error CS0073: An add or remove accessor must have a body
            //     partial event Action E { partial add; partial remove; }
            Diagnostic(ErrorCode.ERR_AddRemoveMustHaveBody, ";").WithLocation(6, 41),
            // (6,43): error CS1609: Modifiers cannot be placed on event accessor declarations
            //     partial event Action E { partial add; partial remove; }
            Diagnostic(ErrorCode.ERR_NoModifiersOnAccessor, "partial").WithLocation(6, 43),
            // (6,57): error CS0073: An add or remove accessor must have a body
            //     partial event Action E { partial add; partial remove; }
            Diagnostic(ErrorCode.ERR_AddRemoveMustHaveBody, ";").WithLocation(6, 57));

        N(SyntaxKind.EventDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.EventKeyword);
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "Action");
            }
            N(SyntaxKind.IdentifierToken, "E");
            N(SyntaxKind.AccessorList);
            {
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.AddAccessorDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.AddKeyword);
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.RemoveAccessorDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.RemoveKeyword);
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.CloseBraceToken);
            }
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Event_InPlaceOfIdentifier([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            partial class C
            {
                [Attr(
                partial event Action E;
            }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (4,13): error CS1026: ) expected
            //     partial event Action E;
            Diagnostic(ErrorCode.ERR_CloseParenExpected, "event").WithLocation(4, 13),
            // (4,13): error CS1003: Syntax error, ']' expected
            //     partial event Action E;
            Diagnostic(ErrorCode.ERR_SyntaxError, "event").WithArguments("]").WithLocation(4, 13));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.EventFieldDeclaration);
                {
                    N(SyntaxKind.AttributeList);
                    {
                        N(SyntaxKind.OpenBracketToken);
                        N(SyntaxKind.Attribute);
                        {
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "Attr");
                            }
                            N(SyntaxKind.AttributeArgumentList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.AttributeArgument);
                                {
                                    N(SyntaxKind.IdentifierName);
                                    {
                                        N(SyntaxKind.IdentifierToken, "partial");
                                    }
                                }
                                M(SyntaxKind.CloseParenToken);
                            }
                        }
                        M(SyntaxKind.CloseBracketToken);
                    }
                    N(SyntaxKind.EventKeyword);
                    N(SyntaxKind.VariableDeclaration);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "Action");
                        }
                        N(SyntaxKind.VariableDeclarator);
                        {
                            N(SyntaxKind.IdentifierToken, "E");
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_Tree([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            partial class C
            {
                partial C();
                partial C() { }
            }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.ConstructorDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.IdentifierToken, "C");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.ConstructorDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.IdentifierToken, "C");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_Declaration([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial C() { }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.Block);
            {
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.CloseBraceToken);
            }
        }
        EOF();
    }

    [Fact]
    public void Constructor_Declaration_CSharp13()
    {
        UsingDeclaration("""
            partial C() { }
            """,
            TestOptions.Regular13);

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.Block);
            {
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.CloseBraceToken);
            }
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_ArrowBody([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial C() => throw null;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.ArrowExpressionClause);
            {
                N(SyntaxKind.EqualsGreaterThanToken);
                N(SyntaxKind.ThrowExpression);
                {
                    N(SyntaxKind.ThrowKeyword);
                    N(SyntaxKind.NullLiteralExpression);
                    {
                        N(SyntaxKind.NullKeyword);
                    }
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_NoParens([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial C;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,1): error CS1073: Unexpected token ';'
            // partial C;
            Diagnostic(ErrorCode.ERR_UnexpectedToken, "partial C").WithArguments(";").WithLocation(1, 1),
            // (1,10): error CS1519: Invalid token ';' in a member declaration
            // partial C;
            Diagnostic(ErrorCode.ERR_InvalidMemberDecl, ";").WithArguments(";").WithLocation(1, 10));

        N(SyntaxKind.IncompleteMember);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "C");
            }
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_NoName([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial ();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.IdentifierToken, "partial");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_PartialAsName([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial partial();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierToken, "partial");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Fact]
    public void Constructor_PartialAsName_CSharp13()
    {
        UsingDeclaration("""
            partial partial();
            """,
            TestOptions.Regular13);

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierToken, "partial");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_PartialAfterName([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            C partial();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.MethodDeclaration);
        {
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "C");
            }
            N(SyntaxKind.IdentifierToken, "partial");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_PartialAfterPublic([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            public partial C();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PublicKeyword);
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_PartialBeforePublic([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial public C();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.ConstructorDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.PublicKeyword);
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_TypeTwice([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial C C();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.MethodDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "C");
            }
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_PartialEscaped([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            @partial C();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.MethodDeclaration);
        {
            N(SyntaxKind.IdentifierName);
            {
                N(SyntaxKind.IdentifierToken, "@partial");
            }
            N(SyntaxKind.IdentifierToken, "C");
            N(SyntaxKind.ParameterList);
            {
                N(SyntaxKind.OpenParenToken);
                N(SyntaxKind.CloseParenToken);
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_KeywordName([CSharp13_CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingDeclaration("""
            partial const();
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,15): error CS8124: Tuple must contain at least two elements.
            // partial const();
            Diagnostic(ErrorCode.ERR_TupleTooFewElements, ")").WithLocation(1, 15),
            // (1,16): error CS1001: Identifier expected
            // partial const();
            Diagnostic(ErrorCode.ERR_IdentifierExpected, ";").WithLocation(1, 16),
            // (1,16): error CS0145: A const field requires a value to be provided
            // partial const();
            Diagnostic(ErrorCode.ERR_ConstValueRequired, ";").WithLocation(1, 16));

        N(SyntaxKind.FieldDeclaration);
        {
            N(SyntaxKind.PartialKeyword);
            N(SyntaxKind.ConstKeyword);
            N(SyntaxKind.VariableDeclaration);
            {
                N(SyntaxKind.TupleType);
                {
                    N(SyntaxKind.OpenParenToken);
                    M(SyntaxKind.TupleElement);
                    {
                        M(SyntaxKind.IdentifierName);
                        {
                            M(SyntaxKind.IdentifierToken);
                        }
                    }
                    M(SyntaxKind.CommaToken);
                    M(SyntaxKind.TupleElement);
                    {
                        M(SyntaxKind.IdentifierName);
                        {
                            M(SyntaxKind.IdentifierToken);
                        }
                    }
                    N(SyntaxKind.CloseParenToken);
                }
                M(SyntaxKind.VariableDeclarator);
                {
                    M(SyntaxKind.IdentifierToken);
                }
            }
            N(SyntaxKind.SemicolonToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void Constructor_InPlaceOfIdentifier([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            partial class C
            {
                [Attr(
                partial C();
            }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (4,13): error CS1003: Syntax error, ',' expected
            //     partial C();
            Diagnostic(ErrorCode.ERR_SyntaxError, "C").WithArguments(",").WithLocation(4, 13),
            // (4,16): error CS1003: Syntax error, ',' expected
            //     partial C();
            Diagnostic(ErrorCode.ERR_SyntaxError, ";").WithArguments(",").WithLocation(4, 16),
            // (4,17): error CS1026: ) expected
            //     partial C();
            Diagnostic(ErrorCode.ERR_CloseParenExpected, "").WithLocation(4, 17),
            // (4,17): error CS1003: Syntax error, ']' expected
            //     partial C();
            Diagnostic(ErrorCode.ERR_SyntaxError, "").WithArguments("]").WithLocation(4, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.IncompleteMember);
                {
                    N(SyntaxKind.AttributeList);
                    {
                        N(SyntaxKind.OpenBracketToken);
                        N(SyntaxKind.Attribute);
                        {
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "Attr");
                            }
                            N(SyntaxKind.AttributeArgumentList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.AttributeArgument);
                                {
                                    N(SyntaxKind.IdentifierName);
                                    {
                                        N(SyntaxKind.IdentifierToken, "partial");
                                    }
                                }
                                M(SyntaxKind.CommaToken);
                                N(SyntaxKind.AttributeArgument);
                                {
                                    N(SyntaxKind.InvocationExpression);
                                    {
                                        N(SyntaxKind.IdentifierName);
                                        {
                                            N(SyntaxKind.IdentifierToken, "C");
                                        }
                                        N(SyntaxKind.ArgumentList);
                                        {
                                            N(SyntaxKind.OpenParenToken);
                                            N(SyntaxKind.CloseParenToken);
                                        }
                                    }
                                }
                                M(SyntaxKind.CloseParenToken);
                            }
                        }
                        M(SyntaxKind.CloseBracketToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void ReturningPartialType_LocalFunction_InMethod([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial F() => null;
                }
            }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial F() => null;
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS1520: Method must have a return type
            //         partial F() => null;
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.ArrowExpressionClause);
                            {
                                N(SyntaxKind.EqualsGreaterThanToken);
                                N(SyntaxKind.NullLiteralExpression);
                                {
                                    N(SyntaxKind.NullKeyword);
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void ReturningPartialType_LocalFunction_InMethod_CSharp13()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial F() => null;
                }
            }
            """,
            TestOptions.Regular13,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial F() => null;
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS1520: Method must have a return type
            //         partial F() => null;
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.ArrowExpressionClause);
                            {
                                N(SyntaxKind.EqualsGreaterThanToken);
                                N(SyntaxKind.NullLiteralExpression);
                                {
                                    N(SyntaxKind.NullKeyword);
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void ReturningPartialType_LocalFunction_TopLevel([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            partial F() => null;
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion),
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial F() => null;
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS1520: Method must have a return type
            // partial F() => null;
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void ReturningPartialType_LocalFunction_TopLevel_CSharp13()
    {
        UsingTree("""
            partial F() => null;
            """,
            TestOptions.Regular13,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial F() => null;
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS1520: Method must have a return type
            // partial F() => null;
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS1520: Method must have a return type
            //         partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            partial F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS1520: Method must have a return type
            // partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialLocalFunction_Semicolon_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial F();
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS1520: Method must have a return type
            //         partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialLocalFunction_Semicolon_TopLevel()
    {
        UsingTree("""
            partial F();
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS1520: Method must have a return type
            // partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialLocalFunctionName_ExpressionBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial() => null;
                }
            }
            """,
            // (4,6): error CS1520: Method must have a return type
            //     {
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "").WithLocation(4, 6));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "partial");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.ArrowExpressionClause);
                            {
                                N(SyntaxKind.EqualsGreaterThanToken);
                                N(SyntaxKind.NullLiteralExpression);
                                {
                                    N(SyntaxKind.NullKeyword);
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialLocalFunctionName_ExpressionBody_TopLevel()
    {
        UsingTree("""
            partial() => null;
            """,
            // (1,1): error CS1520: Method must have a return type
            // partial() => null;
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "partial").WithLocation(1, 1));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "partial");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialGenericLocalFunctionName_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial<T>() { }
                }
            }
            """,
            // (4,6): error CS1520: Method must have a return type
            //     {
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "").WithLocation(4, 6));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "partial");
                            N(SyntaxKind.TypeParameterList);
                            {
                                N(SyntaxKind.LessThanToken);
                                N(SyntaxKind.TypeParameter);
                                {
                                    N(SyntaxKind.IdentifierToken, "T");
                                }
                                N(SyntaxKind.GreaterThanToken);
                            }
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialGenericLocalFunctionName_BlockBody_TopLevel()
    {
        UsingTree("""
            partial<T>() { }
            """,
            // (1,1): error CS1520: Method must have a return type
            // partial<T>() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "partial").WithLocation(1, 1));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "partial");
                    N(SyntaxKind.TypeParameterList);
                    {
                        N(SyntaxKind.LessThanToken);
                        N(SyntaxKind.TypeParameter);
                        {
                            N(SyntaxKind.IdentifierToken, "T");
                        }
                        N(SyntaxKind.GreaterThanToken);
                    }
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void StaticPartialLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    static partial F() { }
                }
            }
            """,
            // (5,16): error CS0106: The modifier 'partial' is not valid for this item
            //         static partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 16),
            // (5,24): error CS1520: Method must have a return type
            //         static partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 24));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.StaticKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void StaticPartialLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            static partial F() { }
            """,
            // (1,8): error CS0106: The modifier 'partial' is not valid for this item
            // static partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 8),
            // (1,16): error CS1520: Method must have a return type
            // static partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 16));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.StaticKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialStaticLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial static F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial static F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,24): error CS1520: Method must have a return type
            //         partial static F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 24));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.StaticKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialStaticLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            partial static F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial static F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,16): error CS1520: Method must have a return type
            // partial static F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 16));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.StaticKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void AsyncPartialLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    async partial F() { }
                }
            }
            """,
            // (5,15): error CS0106: The modifier 'partial' is not valid for this item
            //         async partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 15),
            // (5,23): error CS1520: Method must have a return type
            //         async partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 23));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.AsyncKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void AsyncPartialLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            async partial F() { }
            """,
            // (1,7): error CS0106: The modifier 'partial' is not valid for this item
            // async partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 7),
            // (1,15): error CS1520: Method must have a return type
            // async partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 15));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.AsyncKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void RepeatedPartialLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial partial F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 17),
            // (5,25): error CS1520: Method must have a return type
            //         partial partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 25));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void RepeatedPartialLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            partial partial F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 9),
            // (1,17): error CS1520: Method must have a return type
            // partial partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void StaticPartialLocalFunction_Semicolon_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    static partial F();
                }
            }
            """,
            // (5,16): error CS0106: The modifier 'partial' is not valid for this item
            //         static partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 16),
            // (5,24): error CS1520: Method must have a return type
            //         static partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 24));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.StaticKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void StaticPartialLocalFunction_Semicolon_TopLevel()
    {
        UsingTree("""
            static partial F();
            """,
            // (1,8): error CS0106: The modifier 'partial' is not valid for this item
            // static partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 8),
            // (1,16): error CS1520: Method must have a return type
            // static partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 16));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.StaticKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialStaticLocalFunction_Semicolon_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial static F();
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial static F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,24): error CS1520: Method must have a return type
            //         partial static F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 24));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.StaticKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialStaticLocalFunction_Semicolon_TopLevel()
    {
        UsingTree("""
            partial static F();
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial static F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,16): error CS1520: Method must have a return type
            // partial static F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 16));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.StaticKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void AsyncPartialLocalFunction_Semicolon_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    async partial F();
                }
            }
            """,
            // (5,15): error CS0106: The modifier 'partial' is not valid for this item
            //         async partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 15),
            // (5,23): error CS1520: Method must have a return type
            //         async partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 23));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.AsyncKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void AsyncPartialLocalFunction_Semicolon_TopLevel()
    {
        UsingTree("""
            async partial F();
            """,
            // (1,7): error CS0106: The modifier 'partial' is not valid for this item
            // async partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 7),
            // (1,15): error CS1520: Method must have a return type
            // async partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 15));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.AsyncKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void ExternPartialLocalFunction_Semicolon_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    extern partial F();
                }
            }
            """,
            // (5,16): error CS0106: The modifier 'partial' is not valid for this item
            //         extern partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 16),
            // (5,24): error CS1520: Method must have a return type
            //         extern partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 24));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.ExternKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void ExternPartialLocalFunction_Semicolon_TopLevel()
    {
        UsingTree("""
            extern partial F();
            """,
            // (1,8): error CS0106: The modifier 'partial' is not valid for this item
            // extern partial F();
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 8),
            // (1,16): error CS1520: Method must have a return type
            // extern partial F();
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 16));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.ExternKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void StaticLocalFunctionReturningPartial_MissingName_Semicolon_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    static partial();
                }
            }
            """,
            // (5,23): error CS1001: Identifier expected
            //         static partial();
            Diagnostic(ErrorCode.ERR_IdentifierExpected, "(").WithLocation(5, 23));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.StaticKeyword);
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "partial");
                            }
                            M(SyntaxKind.IdentifierToken);
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void StaticLocalFunctionReturningPartial_MissingName_Semicolon_TopLevel()
    {
        UsingTree("""
            static partial();
            """,
            // (1,15): error CS1001: Identifier expected
            // static partial();
            Diagnostic(ErrorCode.ERR_IdentifierExpected, "(").WithLocation(1, 15));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.StaticKeyword);
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "partial");
                    }
                    M(SyntaxKind.IdentifierToken);
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void TriplePartialLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial partial partial F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 17),
            // (5,25): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 25),
            // (5,33): error CS1520: Method must have a return type
            //         partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 33));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void TriplePartialLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            partial partial partial F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 9),
            // (1,17): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 17),
            // (1,25): error CS1520: Method must have a return type
            // partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 25));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialModifier_GenericReturnType_LocalFunction_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial G<T> F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial G<T> F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.GenericName);
                            {
                                N(SyntaxKind.IdentifierToken, "G");
                                N(SyntaxKind.TypeArgumentList);
                                {
                                    N(SyntaxKind.LessThanToken);
                                    N(SyntaxKind.IdentifierName);
                                    {
                                        N(SyntaxKind.IdentifierToken, "T");
                                    }
                                    N(SyntaxKind.GreaterThanToken);
                                }
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialModifier_GenericReturnType_LocalFunction_TopLevel()
    {
        UsingTree("""
            partial G<T> F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial G<T> F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.GenericName);
                    {
                        N(SyntaxKind.IdentifierToken, "G");
                        N(SyntaxKind.TypeArgumentList);
                        {
                            N(SyntaxKind.LessThanToken);
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "T");
                            }
                            N(SyntaxKind.GreaterThanToken);
                        }
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialNullableReturnType_LocalFunction_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial? F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial? F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,16): error CS1031: Type expected
            //         partial? F() { }
            Diagnostic(ErrorCode.ERR_TypeExpected, "?").WithLocation(5, 16));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.NullableType);
                            {
                                M(SyntaxKind.IdentifierName);
                                {
                                    M(SyntaxKind.IdentifierToken);
                                }
                                N(SyntaxKind.QuestionToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialNullableReturnType_LocalFunction_TopLevel()
    {
        UsingTree("""
            partial? F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial? F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,8): error CS1031: Type expected
            // partial? F() { }
            Diagnostic(ErrorCode.ERR_TypeExpected, "?").WithLocation(1, 8));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.NullableType);
                    {
                        M(SyntaxKind.IdentifierName);
                        {
                            M(SyntaxKind.IdentifierToken);
                        }
                        N(SyntaxKind.QuestionToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialStaticTupleReturnType_LocalFunction_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial static (int, int) F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial static (int, int) F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.StaticKeyword);
                            N(SyntaxKind.TupleType);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.TupleElement);
                                {
                                    N(SyntaxKind.PredefinedType);
                                    {
                                        N(SyntaxKind.IntKeyword);
                                    }
                                }
                                N(SyntaxKind.CommaToken);
                                N(SyntaxKind.TupleElement);
                                {
                                    N(SyntaxKind.PredefinedType);
                                    {
                                        N(SyntaxKind.IntKeyword);
                                    }
                                }
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialStaticTupleReturnType_LocalFunction_TopLevel()
    {
        UsingTree("""
            partial static (int, int) F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial static (int, int) F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.StaticKeyword);
                    N(SyntaxKind.TupleType);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.TupleElement);
                        {
                            N(SyntaxKind.PredefinedType);
                            {
                                N(SyntaxKind.IntKeyword);
                            }
                        }
                        N(SyntaxKind.CommaToken);
                        N(SyntaxKind.TupleElement);
                        {
                            N(SyntaxKind.PredefinedType);
                            {
                                N(SyntaxKind.IntKeyword);
                            }
                        }
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialModifier_PartialLocalFunctionName_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial partial() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS1520: Method must have a return type
            //         partial partial() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "partial").WithLocation(5, 17));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "partial");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialModifier_PartialLocalFunctionName_BlockBody_TopLevel()
    {
        UsingTree("""
            partial partial() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS1520: Method must have a return type
            // partial partial() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "partial").WithLocation(1, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "partial");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void FourPartialLocalFunction_BlockBody_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial partial partial partial F() { }
                }
            }
            """,
            // (5,9): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 9),
            // (5,17): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 17),
            // (5,25): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 25),
            // (5,33): error CS0106: The modifier 'partial' is not valid for this item
            //         partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(5, 33),
            // (5,41): error CS1520: Method must have a return type
            //         partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(5, 41));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.PartialKeyword);
                            N(SyntaxKind.PartialKeyword);
                            M(SyntaxKind.IdentifierName);
                            {
                                M(SyntaxKind.IdentifierToken);
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void FourPartialLocalFunction_BlockBody_TopLevel()
    {
        UsingTree("""
            partial partial partial partial F() { }
            """,
            // (1,1): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 1),
            // (1,9): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 9),
            // (1,17): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 17),
            // (1,25): error CS0106: The modifier 'partial' is not valid for this item
            // partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_BadMemberFlag, "partial").WithArguments("partial").WithLocation(1, 25),
            // (1,33): error CS1520: Method must have a return type
            // partial partial partial partial F() { }
            Diagnostic(ErrorCode.ERR_MemberNeedsType, "F").WithLocation(1, 33));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.PartialKeyword);
                    M(SyntaxKind.IdentifierName);
                    {
                        M(SyntaxKind.IdentifierToken);
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialInvocation_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial();
                }
            }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.ExpressionStatement);
                        {
                            N(SyntaxKind.InvocationExpression);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "partial");
                                }
                                N(SyntaxKind.ArgumentList);
                                {
                                    N(SyntaxKind.OpenParenToken);
                                    N(SyntaxKind.CloseParenToken);
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialInvocation_TopLevel()
    {
        UsingTree("""
            partial();
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.ExpressionStatement);
                {
                    N(SyntaxKind.InvocationExpression);
                    {
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "partial");
                        }
                        N(SyntaxKind.ArgumentList);
                        {
                            N(SyntaxKind.OpenParenToken);
                            N(SyntaxKind.CloseParenToken);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialGenericInvocation_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial<T>();
                }
            }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.ExpressionStatement);
                        {
                            N(SyntaxKind.InvocationExpression);
                            {
                                N(SyntaxKind.GenericName);
                                {
                                    N(SyntaxKind.IdentifierToken, "partial");
                                    N(SyntaxKind.TypeArgumentList);
                                    {
                                        N(SyntaxKind.LessThanToken);
                                        N(SyntaxKind.IdentifierName);
                                        {
                                            N(SyntaxKind.IdentifierToken, "T");
                                        }
                                        N(SyntaxKind.GreaterThanToken);
                                    }
                                }
                                N(SyntaxKind.ArgumentList);
                                {
                                    N(SyntaxKind.OpenParenToken);
                                    N(SyntaxKind.CloseParenToken);
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialGenericInvocation_TopLevel()
    {
        UsingTree("""
            partial<T>();
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.ExpressionStatement);
                {
                    N(SyntaxKind.InvocationExpression);
                    {
                        N(SyntaxKind.GenericName);
                        {
                            N(SyntaxKind.IdentifierToken, "partial");
                            N(SyntaxKind.TypeArgumentList);
                            {
                                N(SyntaxKind.LessThanToken);
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "T");
                                }
                                N(SyntaxKind.GreaterThanToken);
                            }
                        }
                        N(SyntaxKind.ArgumentList);
                        {
                            N(SyntaxKind.OpenParenToken);
                            N(SyntaxKind.CloseParenToken);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void RefPartialReturnType_LocalFunction_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    ref partial F() { }
                }
            }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.RefType);
                            {
                                N(SyntaxKind.RefKeyword);
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "partial");
                                }
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void RefPartialReturnType_LocalFunction_TopLevel()
    {
        UsingTree("""
            ref partial F() { }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.RefType);
                    {
                        N(SyntaxKind.RefKeyword);
                        N(SyntaxKind.IdentifierName);
                        {
                            N(SyntaxKind.IdentifierToken, "partial");
                        }
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void EscapedPartialReturnType_LocalFunction_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    @partial F() { }
                }
            }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalFunctionStatement);
                        {
                            N(SyntaxKind.IdentifierName);
                            {
                                N(SyntaxKind.IdentifierToken, "@partial");
                            }
                            N(SyntaxKind.IdentifierToken, "F");
                            N(SyntaxKind.ParameterList);
                            {
                                N(SyntaxKind.OpenParenToken);
                                N(SyntaxKind.CloseParenToken);
                            }
                            N(SyntaxKind.Block);
                            {
                                N(SyntaxKind.OpenBraceToken);
                                N(SyntaxKind.CloseBraceToken);
                            }
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void EscapedPartialReturnType_LocalFunction_TopLevel()
    {
        UsingTree("""
            @partial F() { }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.LocalFunctionStatement);
                {
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "@partial");
                    }
                    N(SyntaxKind.IdentifierToken, "F");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialType_LocalVariable_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial p;
                }
            }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalDeclarationStatement);
                        {
                            N(SyntaxKind.VariableDeclaration);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "partial");
                                }
                                N(SyntaxKind.VariableDeclarator);
                                {
                                    N(SyntaxKind.IdentifierToken, "p");
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialType_LocalVariable_TopLevel()
    {
        UsingTree("""
            partial p;
            """,
            // (1,9): error CS0116: A namespace cannot directly contain members such as fields, methods or statements
            // partial p;
            Diagnostic(ErrorCode.ERR_NamespaceUnexpected, "p").WithLocation(1, 9));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.IncompleteMember);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "p");
                }
            }
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.EmptyStatement);
                {
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialType_InitializedLocalVariable_InMethod()
    {
        UsingTree("""
            class C
            {
                void M()
                {
                    partial p = default;
                }
            }
            """);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.PredefinedType);
                    {
                        N(SyntaxKind.VoidKeyword);
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.Block);
                    {
                        N(SyntaxKind.OpenBraceToken);
                        N(SyntaxKind.LocalDeclarationStatement);
                        {
                            N(SyntaxKind.VariableDeclaration);
                            {
                                N(SyntaxKind.IdentifierName);
                                {
                                    N(SyntaxKind.IdentifierToken, "partial");
                                }
                                N(SyntaxKind.VariableDeclarator);
                                {
                                    N(SyntaxKind.IdentifierToken, "p");
                                    N(SyntaxKind.EqualsValueClause);
                                    {
                                        N(SyntaxKind.EqualsToken);
                                        N(SyntaxKind.DefaultLiteralExpression);
                                        {
                                            N(SyntaxKind.DefaultKeyword);
                                        }
                                    }
                                }
                            }
                            N(SyntaxKind.SemicolonToken);
                        }
                        N(SyntaxKind.CloseBraceToken);
                    }
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void PartialType_InitializedLocalVariable_TopLevel()
    {
        UsingTree("""
            partial p = default;
            """,
            // (1,9): error CS0116: A namespace cannot directly contain members such as fields, methods or statements
            // partial p = default;
            Diagnostic(ErrorCode.ERR_NamespaceUnexpected, "p").WithLocation(1, 9),
            // (1,11): error CS1525: Invalid expression term '='
            // partial p = default;
            Diagnostic(ErrorCode.ERR_InvalidExprTerm, "=").WithArguments("=").WithLocation(1, 11));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.IncompleteMember);
            {
                N(SyntaxKind.PartialKeyword);
                N(SyntaxKind.IdentifierName);
                {
                    N(SyntaxKind.IdentifierToken, "p");
                }
            }
            N(SyntaxKind.GlobalStatement);
            {
                N(SyntaxKind.ExpressionStatement);
                {
                    N(SyntaxKind.SimpleAssignmentExpression);
                    {
                        M(SyntaxKind.IdentifierName);
                        {
                            M(SyntaxKind.IdentifierToken);
                        }
                        N(SyntaxKind.EqualsToken);
                        N(SyntaxKind.DefaultLiteralExpression);
                        {
                            N(SyntaxKind.DefaultKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Theory, CombinatorialData]
    public void ReturningPartialType_Method([CSharp14_Preview] LanguageVersion langVersion)
    {
        UsingTree("""
            class C
            {
                partial M() => null;
                @partial M() => null;
            }
            """,
            TestOptions.Regular.WithLanguageVersion(langVersion));

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.ConstructorDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "@partial");
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }

    [Fact]
    public void ReturningPartialType_Method_CSharp13()
    {
        UsingTree("""
            class C
            {
                partial M() => null;
                @partial M() => null;
            }
            """,
            TestOptions.Regular13);

        N(SyntaxKind.CompilationUnit);
        {
            N(SyntaxKind.ClassDeclaration);
            {
                N(SyntaxKind.ClassKeyword);
                N(SyntaxKind.IdentifierToken, "C");
                N(SyntaxKind.OpenBraceToken);
                N(SyntaxKind.ConstructorDeclaration);
                {
                    N(SyntaxKind.PartialKeyword);
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.MethodDeclaration);
                {
                    N(SyntaxKind.IdentifierName);
                    {
                        N(SyntaxKind.IdentifierToken, "@partial");
                    }
                    N(SyntaxKind.IdentifierToken, "M");
                    N(SyntaxKind.ParameterList);
                    {
                        N(SyntaxKind.OpenParenToken);
                        N(SyntaxKind.CloseParenToken);
                    }
                    N(SyntaxKind.ArrowExpressionClause);
                    {
                        N(SyntaxKind.EqualsGreaterThanToken);
                        N(SyntaxKind.NullLiteralExpression);
                        {
                            N(SyntaxKind.NullKeyword);
                        }
                    }
                    N(SyntaxKind.SemicolonToken);
                }
                N(SyntaxKind.CloseBraceToken);
            }
            N(SyntaxKind.EndOfFileToken);
        }
        EOF();
    }
}
