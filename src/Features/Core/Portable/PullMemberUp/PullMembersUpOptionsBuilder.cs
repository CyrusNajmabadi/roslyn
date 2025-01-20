// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.  

#nullable disable

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
                var changeOriginalToPublic = member.DeclaredAccessibility != Accessibility.Public;
                var changeOriginalToNonStatic = member.IsStatic;
                return new MemberAnalysisResult(
                    member,
                    Accessibility.Public,
                    changeOriginalToNonStatic,
                    makeMemberDeclarationAbstract: false,
                    changeDestinationTypeToAbstract: false);
            }
            else
            {
                var changeDestinationToAbstract = !destination.IsAbstract && (makeAbstract || member.IsAbstract);

                // Convert to protected if we're moving a private member into the base class, so we can still refer to
                // the member from the derived class.
                var desiredAccessibility = member.DeclaredAccessibility == Accessibility.Private
                    ? Accessibility.Protected
                    : member.DeclaredAccessibility;
                return new MemberAnalysisResult(member,
                    accessibility: member.DeclaredAccessibility,
                    changeOriginalToNonStatic: false,
                    makeAbstract,
                    changeDestinationTypeToAbstract: changeDestinationToAbstract);
            }
        });

        return new PullMembersUpOptions(destination, membersAnalysisResult);
    }
}
