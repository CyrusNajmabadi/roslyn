// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable enable

global using CSharpBindingDiagnosticBag = Microsoft.CodeAnalysis.BindingDiagnosticBag<Microsoft.CodeAnalysis.CSharp.Symbols.AssemblySymbol>;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.PooledObjects;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal static class BindingDiagnosticBag
    {
        public static readonly CSharpBindingDiagnosticBag Discarded = BindingDiagnosticBag.CreateNewInstance(null, null);

        private static Func<DiagnosticInfo, DiagnosticBag, Location, bool> _reportUseSiteDiagnostic = Symbol.ReportUseSiteDiagnostic;

        public static CSharpBindingDiagnosticBag CreateNewInstance()
            => CreateNewInstance(usePool: false);

        private static CSharpBindingDiagnosticBag CreateNewInstance(bool usePool)
            => new CSharpBindingDiagnosticBag(usePool, _reportUseSiteDiagnostic);

        public static CSharpBindingDiagnosticBag CreateNewInstance(DiagnosticBag? diagnosticBag)
            => new CSharpBindingDiagnosticBag(diagnosticBag, dependenciesBag: null, _reportUseSiteDiagnostic);

        public static CSharpBindingDiagnosticBag CreateNewInstance(DiagnosticBag? diagnosticBag, ICollection<AssemblySymbol>? dependenciesBag)
            => new CSharpBindingDiagnosticBag(diagnosticBag, dependenciesBag, _reportUseSiteDiagnostic);

        internal static CSharpBindingDiagnosticBag GetInstance()
        {
            return CreateNewInstance(usePool: true);
        }

        internal static CSharpBindingDiagnosticBag GetInstance(bool withDiagnostics, bool withDependencies)
        {
            if (withDiagnostics)
            {
                if (withDependencies)
                {
                    return GetInstance();
                }

                return CreateNewInstance(DiagnosticBag.GetInstance());
            }
            else if (withDependencies)
            {
                return CreateNewInstance(diagnosticBag: null, PooledHashSet<AssemblySymbol>.GetInstance());
            }
            else
            {
                return Discarded;
            }
        }

        internal static CSharpBindingDiagnosticBag GetInstance(CSharpBindingDiagnosticBag template)
        {
            return GetInstance(template.AccumulatesDiagnostics, template.AccumulatesDependencies);
        }

        internal static CSharpBindingDiagnosticBag Create(CSharpBindingDiagnosticBag template)
        {
            if (template.AccumulatesDiagnostics)
            {
                if (template.AccumulatesDependencies)
                {
                    return CreateNewInstance();
                }

                return CreateNewInstance(new DiagnosticBag());
            }
            else if (template.AccumulatesDependencies)
            {
                return CreateNewInstance(diagnosticBag: null, new HashSet<AssemblySymbol>());
            }
            else
            {
                return Discarded;
            }
        }

    }

    internal static class CSharpBindingDiagnosticBagExtensions
    {
        internal static void AddDependencies(this CSharpBindingDiagnosticBag diagnosticBag, Symbol? symbol)
        {
            if (symbol is object && diagnosticBag.DependenciesBag is object)
            {
                diagnosticBag.AddDependencies(symbol.GetUseSiteInfo());
            }
        }

        internal static bool ReportUseSite(this CSharpBindingDiagnosticBag diagnosticBag, Symbol? symbol, SyntaxNode node)
        {
            return ReportUseSite(diagnosticBag, symbol, static node => node.Location, node);
        }

        internal static bool ReportUseSite(this CSharpBindingDiagnosticBag diagnosticBag, Symbol? symbol, SyntaxToken token)
        {
            return ReportUseSite(diagnosticBag, symbol, static token => token.GetLocation(), token);
        }

        internal static bool ReportUseSite(this CSharpBindingDiagnosticBag diagnosticBag, Symbol? symbol, Location location)
            => ReportUseSite(diagnosticBag, symbol, static location => location, location);

        internal static bool ReportUseSite<TData>(this CSharpBindingDiagnosticBag diagnosticBag, Symbol? symbol, Func<TData, Location> getLocation, TData data)
        {
            if (symbol is object)
            {
                return diagnosticBag.Add(symbol.GetUseSiteInfo(), getLocation, data);
            }

            return false;
        }

        internal static void AddAssembliesUsedByNamespaceReference(this CSharpBindingDiagnosticBag diagnosticBag, NamespaceSymbol ns)
        {
            if (diagnosticBag.DependenciesBag is null)
            {
                return;
            }

            addAssembliesUsedByNamespaceReferenceImpl(ns);

            void addAssembliesUsedByNamespaceReferenceImpl(NamespaceSymbol ns)
            {
                // Treat all assemblies contributing to this namespace symbol as used
                if (ns.Extent.Kind == NamespaceKind.Compilation)
                {
                    foreach (var constituent in ns.ConstituentNamespaces)
                    {
                        addAssembliesUsedByNamespaceReferenceImpl(constituent);
                    }
                }
                else
                {
                    AssemblySymbol? containingAssembly = ns.ContainingAssembly;

                    if (containingAssembly?.IsMissing == false)
                    {
                        diagnosticBag.DependenciesBag!.Add(containingAssembly);
                    }
                }
            }
        }

        internal static CSDiagnosticInfo Add(this CSharpBindingDiagnosticBag diagnosticBag, ErrorCode code, Location location)
        {
            var info = new CSDiagnosticInfo(code);
            Add(diagnosticBag, info, location);
            return info;
        }

        internal static CSDiagnosticInfo Add(this CSharpBindingDiagnosticBag diagnosticBag, ErrorCode code, SyntaxNode syntax, params object[] args)
            => Add(diagnosticBag, code, syntax.Location, args);

        internal static CSDiagnosticInfo Add(this CSharpBindingDiagnosticBag diagnosticBag, ErrorCode code, SyntaxToken syntax, params object[] args)
            => Add(diagnosticBag, code, syntax.GetLocation()!, args);

        internal static CSDiagnosticInfo Add(this CSharpBindingDiagnosticBag diagnosticBag, ErrorCode code, Location location, params object[] args)
        {
            var info = new CSDiagnosticInfo(code, args);
            Add(diagnosticBag, info, location);
            return info;
        }

        internal static CSDiagnosticInfo Add(this CSharpBindingDiagnosticBag diagnosticBag, ErrorCode code, Location location, ImmutableArray<Symbol> symbols, params object[] args)
        {
            var info = new CSDiagnosticInfo(code, args, symbols, ImmutableArray<Location>.Empty);
            Add(diagnosticBag, info, location);
            return info;
        }

        internal static void Add(this CSharpBindingDiagnosticBag diagnosticBag, DiagnosticInfo? info, Location location)
        {
            if (info is object)
            {
                diagnosticBag.DiagnosticBag?.Add(info, location);
            }
        }
    }
}
