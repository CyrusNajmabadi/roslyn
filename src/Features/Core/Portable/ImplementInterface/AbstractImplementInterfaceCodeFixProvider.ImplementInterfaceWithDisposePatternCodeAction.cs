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

internal abstract partial class AbstractImplementInterfaceCodeFixProvider<TTypeSyntax>
{

    private sealed class ImplementInterfaceWithDisposePatternCodeAction(
        Document document,
        ImplementTypeGenerationOptions options,
        IImplementInterfaceInfo state,
        bool explicitly,
        bool abstractly,
        ISymbol? throughMember) : ImplementInterfaceCodeAction(document, options, state, explicitly, abstractly, onlyRemaining: !explicitly, throughMember)
    {
        public static ImplementInterfaceWithDisposePatternCodeAction CreateImplementWithDisposePatternCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state)
        {
            return new ImplementInterfaceWithDisposePatternCodeAction(document, options, state, explicitly: false, abstractly: false, throughMember: null);
        }

        public static ImplementInterfaceWithDisposePatternCodeAction CreateImplementExplicitlyWithDisposePatternCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state)
        {
            return new ImplementInterfaceWithDisposePatternCodeAction(document, options, state, explicitly: true, abstractly: false, throughMember: null);
        }

        public override string Title
            => Explicitly
                ? FeaturesResources.Implement_interface_explicitly_with_Dispose_pattern
                : FeaturesResources.Implement_interface_with_Dispose_pattern;

        public override Task<Document> GetUpdatedDocumentAsync(
            Document document,
            ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> unimplementedMembers,
            INamedTypeSymbol classType,
            SyntaxNode classDecl,
            CancellationToken cancellationToken)
        {
            return this.Service.ImplementIDisposableInterfaceAsync(
                document, unimplementedMembers, classType, classDecl, cancellationToken);
        }
    }
}
