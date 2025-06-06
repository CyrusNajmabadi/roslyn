﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable
#pragma warning disable RS1024 // Use 'SymbolEqualityComparer' when comparing symbols (https://github.com/dotnet/roslyn/issues/78583)

using System;
using System.Linq;

namespace Microsoft.CodeAnalysis.Shared.Extensions;

internal static partial class ITypeSymbolExtensions
{
    private sealed class AnonymousTypeRemover(Compilation compilation) : SymbolVisitor<ITypeSymbol>
    {
        public override ITypeSymbol DefaultVisit(ISymbol node)
            => throw new NotImplementedException();

        public override ITypeSymbol VisitDynamicType(IDynamicTypeSymbol symbol)
            => symbol;

        public override ITypeSymbol VisitArrayType(IArrayTypeSymbol symbol)
        {
            var elementType = symbol.ElementType.Accept(this);
            if (elementType != null && elementType.Equals(symbol.ElementType))
            {
                return symbol;
            }

            return compilation.CreateArrayTypeSymbol(elementType, symbol.Rank);
        }

        public override ITypeSymbol VisitFunctionPointerType(IFunctionPointerTypeSymbol symbol)
        {
            // TODO(https://github.com/dotnet/roslyn/issues/43890): function pointers could theoretically
            // have a parameter of an anonymous type if you have a generic function that returns function
            // pointers, and that was called with an anonymous type.
            return symbol;
        }

        public override ITypeSymbol VisitNamedType(INamedTypeSymbol symbol)
        {
            if (symbol.IsAnonymousType())
                return compilation.ObjectType;

            var arguments = symbol.TypeArguments.Select(t => t.Accept(this)).ToArray();
            if (arguments.SequenceEqual(symbol.TypeArguments))
                return symbol;

            return symbol.ConstructedFrom.Construct([.. arguments]);
        }

        public override ITypeSymbol VisitPointerType(IPointerTypeSymbol symbol)
        {
            var elementType = symbol.PointedAtType.Accept(this);
            if (elementType != null && elementType.Equals(symbol.PointedAtType))
            {
                return symbol;
            }

            return compilation.CreatePointerTypeSymbol(elementType);
        }

        public override ITypeSymbol VisitTypeParameter(ITypeParameterSymbol symbol)
            => symbol;
    }
}
