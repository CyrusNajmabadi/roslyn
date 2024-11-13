// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Simplification;

namespace Microsoft.CodeAnalysis.CSharp.MakeMethodAsynchronous;

using static SyntaxFactory;

internal static class MakeMethodAsynchronousHelpers
{
    public static TypeSyntax MakeTaskLike(
        bool keepVoid,
        IMethodSymbol methodSymbol,
        TypeSyntax returnTypeSyntax,
        KnownTaskTypes knownTypes,
        CancellationToken cancellationToken)
    {
        var newReturnType = returnTypeSyntax.WithAdditionalAnnotations(Formatter.Annotation);

        if (methodSymbol.ReturnsVoid)
        {
            if (!keepVoid)
            {
                newReturnType = knownTypes.TaskType!.GenerateTypeSyntax();
            }
        }
        else
        {
            var returnType = methodSymbol.ReturnType;
            if (IsIEnumerable(returnType, knownTypes) && IsIterator(methodSymbol, cancellationToken))
            {
                newReturnType = knownTypes.IAsyncEnumerableOfTType is null
                    ? MakeGenericType(nameof(IAsyncEnumerable<int>), methodSymbol.ReturnType)
                    : knownTypes.IAsyncEnumerableOfTType.Construct(methodSymbol.ReturnType.GetTypeArguments()[0]).GenerateTypeSyntax();
            }
            else if (IsIEnumerator(returnType, knownTypes) && IsIterator(methodSymbol, cancellationToken))
            {
                newReturnType = knownTypes.IAsyncEnumeratorOfTType is null
                    ? MakeGenericType(nameof(IAsyncEnumerator<int>), methodSymbol.ReturnType)
                    : knownTypes.IAsyncEnumeratorOfTType.Construct(methodSymbol.ReturnType.GetTypeArguments()[0]).GenerateTypeSyntax();
            }
            else if (IsIAsyncEnumerableOrEnumerator(returnType, knownTypes))
            {
                // Leave the return type alone
            }
            else if (!knownTypes.IsTaskLike(returnType))
            {
                // If it's not already Task-like, then wrap the existing return type
                // in Task<>.
                newReturnType = knownTypes.TaskOfTType!.Construct(methodSymbol.ReturnType).GenerateTypeSyntax();
            }
        }

        return newReturnType.WithTriviaFrom(returnTypeSyntax).WithAdditionalAnnotations(Simplifier.AddImportsAnnotation);

        static TypeSyntax MakeGenericType(string type, ITypeSymbol typeArgumentFrom)
        {
            var result = GenericName(
                Identifier(type),
                TypeArgumentList([typeArgumentFrom.GetTypeArguments()[0].GenerateTypeSyntax()]));

            return result.WithAdditionalAnnotations(Simplifier.Annotation);
        }
    }

    public static bool IsIAsyncEnumerableOrEnumerator(ITypeSymbol returnType, KnownTaskTypes knownTypes)
        => returnType.OriginalDefinition.Equals(knownTypes.IAsyncEnumerableOfTType) ||
           returnType.OriginalDefinition.Equals(knownTypes.IAsyncEnumeratorOfTType);

    private static bool IsIterator(IMethodSymbol method, CancellationToken cancellationToken)
        => method.Locations.Any(static (loc, cancellationToken) => loc.FindNode(cancellationToken).ContainsYield(), cancellationToken);

    private static bool IsIEnumerable(ITypeSymbol returnType, KnownTaskTypes knownTypes)
        => returnType.OriginalDefinition.Equals(knownTypes.IEnumerableOfTType);

    private static bool IsIEnumerator(ITypeSymbol returnType, KnownTaskTypes knownTypes)
        => returnType.OriginalDefinition.Equals(knownTypes.IEnumeratorOfTType);
}
