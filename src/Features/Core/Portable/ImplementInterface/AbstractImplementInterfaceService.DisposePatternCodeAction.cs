// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Diagnostics.Analyzers.NamingStyles;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.ImplementInterface;

using static ImplementHelpers;

internal abstract partial class AbstractImplementInterfaceService
{
    // C#: `Dispose(bool disposed)`.  VB: `Dispose(disposed As Boolean)`
    private static readonly SymbolDisplayFormat s_format = new(
        memberOptions: SymbolDisplayMemberOptions.IncludeParameters,
        parameterOptions: SymbolDisplayParameterOptions.IncludeName | SymbolDisplayParameterOptions.IncludeType,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.UseSpecialTypes);

    private static bool ShouldImplementDisposePattern(State state, bool explicitly)
    {
        // Dispose pattern should be implemented only if -
        // 1. An interface named 'System.IDisposable' is unimplemented.
        // 2. This interface has one and only one member - a non-generic method named 'Dispose' that takes no arguments and returns 'void'.
        // 3. The implementing type is a class that does not already declare any conflicting members named 'disposedValue' or 'Dispose'
        //    (because we will be generating a 'disposedValue' field and a couple of methods named 'Dispose' as part of implementing 
        //    the dispose pattern).
        if (state.ClassOrStructType.TypeKind != TypeKind.Class)
            return false;

        var disposeMethod = TryGetIDisposableDispose(state.Model.Compilation);
        if (disposeMethod == null)
            return false;

        var idisposableType = disposeMethod.ContainingType;
        var unimplementedMembers = explicitly
            ? state.MembersWithoutExplicitImplementation
            : state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented;
        if (!unimplementedMembers.Any(static (m, idisposableType) => m.type.Equals(idisposableType), idisposableType))
            return false;

        // The dispose pattern is only applicable if the implementing type does
        // not already have an implementation of IDisposableDispose.
        return state.ClassOrStructType.FindImplementationForInterfaceMember(disposeMethod) == null;
    }
}
