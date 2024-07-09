// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;

namespace Microsoft.CodeAnalysis.ImplementInterface;

internal interface IImplementInterfaceInfo
{
    /// <summary>
    /// Class or struct implementing hte unbound interface.
    /// </summary>
    INamedTypeSymbol ClassOrStructType { get; }
    SyntaxNode ClassOrStructDecl { get; }

    SyntaxNode InterfaceNode { get; }

    ImmutableArray<INamedTypeSymbol> InterfaceTypes { get; }

    ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented { get; }
    ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> MembersWithoutExplicitOrImplicitImplementation { get; }

    // The members that have no explicit implementation.
    ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> MembersWithoutExplicitImplementation { get; }

}
