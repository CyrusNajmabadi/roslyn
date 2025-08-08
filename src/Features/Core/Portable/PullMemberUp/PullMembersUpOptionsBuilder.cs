// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.  

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.PullMemberUp;

namespace Microsoft.CodeAnalysis.CodeRefactorings.PullMemberUp;

internal sealed class PullMembersUpOptionsBuilder
{
    public static PullMembersUpOptions BuildPullMembersUpOptions(
        INamedTypeSymbol destination,
        ImmutableArray<(ISymbol member, bool makeAbstract)> members)
    {
        var membersAnalysisResult = members.SelectAsArray(tuple =>
        {
            var (member, makeAbstract) = tuple;
            if (destination.TypeKind == TypeKind.Interface)
            {
                return new MemberAnalysisResult(
                    member,
                    Accessibility.Public,
                    member.IsStatic,
                    makeMemberDeclarationAbstract: false,
                    changeDestinationTypeToAbstract: false);
            }
            else
            {
                var changeDestinationToAbstract = !destination.IsAbstract && (makeAbstract || member.IsAbstract);
                return new MemberAnalysisResult(
                    member,
                    member.DeclaredAccessibility,
                    changeOriginalToNonStatic: false,
                    makeAbstract,
                    changeDestinationTypeToAbstract: changeDestinationToAbstract);
            }
        });

        return new PullMembersUpOptions(destination, membersAnalysisResult);
    }
}
