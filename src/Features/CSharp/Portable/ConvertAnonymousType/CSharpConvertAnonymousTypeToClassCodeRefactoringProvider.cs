﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Composition;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.ConvertAnonymousType;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.CSharp.ConvertAnonymousType;

using static CSharpSyntaxTokens;
using static SyntaxFactory;

[ExtensionOrder(Before = PredefinedCodeRefactoringProviderNames.IntroduceVariable)]
[ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = PredefinedCodeRefactoringProviderNames.ConvertAnonymousTypeToClass), Shared]
[method: ImportingConstructor]
[method: SuppressMessage("RoslynDiagnosticsReliability", "RS0033:Importing constructor should be [Obsolete]", Justification = "Used in test code: https://github.com/dotnet/roslyn/issues/42814")]
internal class CSharpConvertAnonymousTypeToClassCodeRefactoringProvider()
    : AbstractConvertAnonymousTypeToClassCodeRefactoringProvider<
        ExpressionSyntax,
        NameSyntax,
        IdentifierNameSyntax,
        ObjectCreationExpressionSyntax,
        AnonymousObjectCreationExpressionSyntax,
        BaseNamespaceDeclarationSyntax>
{
    protected override ObjectCreationExpressionSyntax CreateObjectCreationExpression(NameSyntax nameNode, AnonymousObjectCreationExpressionSyntax anonymousObject)
        => ObjectCreationExpression(nameNode, CreateArgumentList(anonymousObject), initializer: null);

    private ArgumentListSyntax CreateArgumentList(AnonymousObjectCreationExpressionSyntax anonymousObject)
        => ArgumentList(
            OpenParenToken.WithTriviaFrom(anonymousObject.OpenBraceToken),
            CreateArguments(anonymousObject.Initializers),
            CloseParenToken.WithTriviaFrom(anonymousObject.CloseBraceToken));

    private SeparatedSyntaxList<ArgumentSyntax> CreateArguments(SeparatedSyntaxList<AnonymousObjectMemberDeclaratorSyntax> initializers)
        => SeparatedList<ArgumentSyntax>(CreateArguments(OmitTrailingComma(initializers.GetWithSeparators())));

    private static SyntaxNodeOrTokenList OmitTrailingComma(SyntaxNodeOrTokenList list)
    {
        // Trailing comma is allowed in initializer list, but disallowed in method calls.
        if (list.Count == 0 || list.Count % 2 == 1)
        {
            // The list is either empty, or does not end with a trailing comma.
            return list;
        }

        return list
            .Replace(
                list[^2],
                list[^2].AsNode()!
                    .WithAppendedTrailingTrivia(list[^1].GetLeadingTrivia())
                    .WithAppendedTrailingTrivia(list[^1].GetTrailingTrivia()))
            .RemoveAt(list.Count - 1);
    }

    private SyntaxNodeOrTokenList CreateArguments(SyntaxNodeOrTokenList list)
        => [.. list.Select(CreateArgumentOrComma)];

    private SyntaxNodeOrToken CreateArgumentOrComma(SyntaxNodeOrToken declOrComma)
        => declOrComma.IsToken
            ? declOrComma
            : CreateArgument((AnonymousObjectMemberDeclaratorSyntax)declOrComma.AsNode()!);

    private static ArgumentSyntax CreateArgument(AnonymousObjectMemberDeclaratorSyntax decl)
        => Argument(decl.Expression);
}
