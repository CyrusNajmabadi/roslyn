// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.Editing;
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
    internal partial class ImplementInterfaceCodeAction : CodeAction
    {
        protected readonly bool Explicitly;
        protected readonly bool Abstractly;
        private readonly bool _onlyRemaining;
        protected readonly ISymbol? ThroughMember;
        protected readonly Document Document;
        protected readonly ImplementTypeGenerationOptions Options;
        protected readonly IImplementInterfaceInfo State;
        protected readonly IImplementInterfaceService Service;
        private readonly string _equivalenceKey;

        internal ImplementInterfaceCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state,
            bool explicitly,
            bool abstractly,
            bool onlyRemaining,
            ISymbol? throughMember)
        {
            Service = document.GetRequiredLanguageService<IImplementInterfaceService>();
            Document = document;
            State = state;
            Options = options;
            Abstractly = abstractly;
            _onlyRemaining = onlyRemaining;
            Explicitly = explicitly;
            ThroughMember = throughMember;
            _equivalenceKey = ComputeEquivalenceKey(state, explicitly, abstractly, onlyRemaining, throughMember, GetType().FullName!);
        }

        public static ImplementInterfaceCodeAction CreateImplementAbstractlyCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state)
        {
            return new ImplementInterfaceCodeAction(document, options, state, explicitly: false, abstractly: true, onlyRemaining: true, throughMember: null);
        }

        public static ImplementInterfaceCodeAction CreateImplementCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state)
        {
            return new ImplementInterfaceCodeAction(document, options, state, explicitly: false, abstractly: false, onlyRemaining: true, throughMember: null);
        }

        public static ImplementInterfaceCodeAction CreateImplementExplicitlyCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state)
        {
            return new ImplementInterfaceCodeAction(document, options, state, explicitly: true, abstractly: false, onlyRemaining: false, throughMember: null);
        }

        public static ImplementInterfaceCodeAction CreateImplementThroughMemberCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state,
            ISymbol throughMember)
        {
            return new ImplementInterfaceCodeAction(document, options, state, explicitly: false, abstractly: false, onlyRemaining: false, throughMember: throughMember);
        }

        public static ImplementInterfaceCodeAction CreateImplementRemainingExplicitlyCodeAction(
            Document document,
            ImplementTypeGenerationOptions options,
            IImplementInterfaceInfo state)
        {
            return new ImplementInterfaceCodeAction(document, options, state, explicitly: true, abstractly: false, onlyRemaining: true, throughMember: null);
        }

        public override string Title
        {
            get
            {
                if (Explicitly)
                {
                    if (_onlyRemaining)
                    {
                        return FeaturesResources.Implement_remaining_members_explicitly;
                    }
                    else
                    {
                        return FeaturesResources.Implement_all_members_explicitly;
                    }
                }
                else if (Abstractly)
                {
                    return FeaturesResources.Implement_interface_abstractly;
                }
                else if (ThroughMember != null)
                {
                    return string.Format(FeaturesResources.Implement_interface_through_0, ThroughMember.Name);
                }
                else
                {
                    return FeaturesResources.Implement_interface;
                }
            }
        }

        private static string ComputeEquivalenceKey(
            IImplementInterfaceInfo state,
            bool explicitly,
            bool abstractly,
            bool onlyRemaining,
            ISymbol? throughMember,
            string codeActionTypeName)
        {
            var interfaceType = state.InterfaceTypes.First();
            var typeName = interfaceType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var assemblyName = interfaceType.ContainingAssembly.Name;

            // Consider code actions equivalent if they correspond to the same interface being implemented elsewhere
            // in the same manner.  Note: 'implement through member' means implementing the same interface through
            // an applicable member with the same name in the destination.
            return explicitly.ToString() + ";" +
               abstractly.ToString() + ";" +
               onlyRemaining.ToString() + ":" +
               typeName + ";" +
               assemblyName + ";" +
               codeActionTypeName + ";" +
               throughMember?.Name;
        }

        public override string EquivalenceKey => _equivalenceKey;

        protected override Task<Document> GetChangedDocumentAsync(CancellationToken cancellationToken)
            => GetUpdatedDocumentAsync(cancellationToken);

        public Task<Document> GetUpdatedDocumentAsync(CancellationToken cancellationToken)
        {
            var unimplementedMembers = Explicitly
                ? _onlyRemaining
                    ? State.MembersWithoutExplicitOrImplicitImplementation
                    : State.MembersWithoutExplicitImplementation
                : State.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented;
            return this.Service.ImplementInterfaceAsync(
                Document, unimplementedMembers, this.Explicitly, State.ClassOrStructType, State.ClassOrStructDecl, this.Options, cancellationToken);
        }
        private bool IsReservedName(string name)
        {
            return
                IdentifiersMatch(this.State.ClassOrStructType.Name, name) ||
                this.State.ClassOrStructType.TypeParameters.Any(static (t, arg) => arg.self.IdentifiersMatch(t.Name, arg.name), (self: this, name));
        }

        private string DetermineMemberName(ISymbol member, ArrayBuilder<ISymbol> implementedVisibleMembers)
        {
            if (HasConflictingMember(member, implementedVisibleMembers))
            {
                var memberNames = State.ClassOrStructType.GetAccessibleMembersInThisAndBaseTypes<ISymbol>(State.ClassOrStructType).Select(m => m.Name);

                return NameGenerator.GenerateUniqueName(
                    string.Format("{0}_{1}", member.ContainingType.Name, member.Name),
                    n => !memberNames.Contains(n) &&
                        !implementedVisibleMembers.Any(m => IdentifiersMatch(m.Name, n)) &&
                        !IsReservedName(n));
            }

            return member.Name;
        }

        private IEnumerable<ISymbol?> GenerateMembers(
            Compilation compilation,
            ISymbol member,
            string memberName,
            bool generateInvisibly,
            bool generateAbstractly,
            bool addNew,
            bool addUnsafe,
            ImplementTypePropertyGenerationBehavior propertyGenerationBehavior)
        {
            var factory = Document.GetRequiredLanguageService<SyntaxGenerator>();
            var modifiers = new DeclarationModifiers(isStatic: member.IsStatic, isAbstract: generateAbstractly, isNew: addNew, isUnsafe: addUnsafe);

            var useExplicitInterfaceSymbol = generateInvisibly || !Service.CanImplementImplicitly;
            var accessibility = member.Name == memberName || generateAbstractly
                ? Accessibility.Public
                : Accessibility.Private;

            if (member is IMethodSymbol method)
            {
                yield return GenerateMethod(compilation, method, accessibility, modifiers, generateAbstractly, useExplicitInterfaceSymbol, memberName);
            }
            else if (member is IPropertySymbol property)
            {
                foreach (var generated in GeneratePropertyMembers(compilation, property, accessibility, modifiers, generateAbstractly, useExplicitInterfaceSymbol, memberName, propertyGenerationBehavior))
                    yield return generated;
            }
            else if (member is IEventSymbol @event)
            {
                yield return GenerateEvent(compilation, memberName, generateInvisibly, factory, modifiers, useExplicitInterfaceSymbol, accessibility, @event);
            }
        }

        private ISymbol GenerateEvent(Compilation compilation, string memberName, bool generateInvisibly, SyntaxGenerator factory, DeclarationModifiers modifiers, bool useExplicitInterfaceSymbol, Accessibility accessibility, IEventSymbol @event)
        {
            var accessor = CodeGenerationSymbolFactory.CreateAccessorSymbol(
                attributes: default,
                accessibility: Accessibility.NotApplicable,
                statements: factory.CreateThrowNotImplementedStatementBlock(compilation));

            return CodeGenerationSymbolFactory.CreateEventSymbol(
                @event,
                accessibility: accessibility,
                modifiers: modifiers,
                explicitInterfaceImplementations: useExplicitInterfaceSymbol ? [@event] : default,
                name: memberName,
                addMethod: GetAddOrRemoveMethod(@event, generateInvisibly, accessor, memberName, factory.AddEventHandler),
                removeMethod: GetAddOrRemoveMethod(@event, generateInvisibly, accessor, memberName, factory.RemoveEventHandler));
        }

        private IMethodSymbol? GetAddOrRemoveMethod(
            IEventSymbol @event, bool generateInvisibly, IMethodSymbol accessor, string memberName,
            Func<SyntaxNode, SyntaxNode, SyntaxNode> createAddOrRemoveHandler)
        {
            if (ThroughMember != null)
            {
                var generator = Document.GetRequiredLanguageService<SyntaxGenerator>();
                var throughExpression = generator.CreateDelegateThroughExpression(@event, ThroughMember);
                var statement = generator.ExpressionStatement(createAddOrRemoveHandler(
                    generator.MemberAccessExpression(throughExpression, memberName), generator.IdentifierName("value")));

                return CodeGenerationSymbolFactory.CreateAccessorSymbol(
                       attributes: default,
                       accessibility: Accessibility.NotApplicable,
                       statements: [statement]);
            }

            return generateInvisibly ? accessor : null;
        }

        private bool HasNameConflict(
            ISymbol member,
            string memberName,
            IEnumerable<INamedTypeSymbol> baseTypes)
        {
            // There's a naming conflict if any member in the base types chain is accessible to
            // us, has our name.  Note: a simple name won't conflict with a generic name (and
            // vice versa).  A method only conflicts with another method if they have the same
            // parameter signature (return type is irrelevant). 
            return
                baseTypes.Any(ts => ts.GetMembers(memberName)
                                      .Where(m => m.IsAccessibleWithin(State.ClassOrStructType))
                                      .Any(m => HasNameConflict(member, m)));
        }

        private static bool HasNameConflict(ISymbol member, ISymbol baseMember)
        {
            if (member is IMethodSymbol method1 && baseMember is IMethodSymbol method2)
            {
                // A method only conflicts with another method if they have the same parameter
                // signature (return type is irrelevant). 
                return method1.MethodKind == MethodKind.Ordinary &&
                       method2.MethodKind == MethodKind.Ordinary &&
                       method1.TypeParameters.Length == method2.TypeParameters.Length &&
                       method1.Parameters.SequenceEqual(method2.Parameters, SymbolEquivalenceComparer.Instance.ParameterEquivalenceComparer);
            }

            // Any non method members with the same name simple name conflict.
            return true;
        }

        private bool IdentifiersMatch(string identifier1, string identifier2)
            => IsCaseSensitive
                ? identifier1 == identifier2
                : StringComparer.OrdinalIgnoreCase.Equals(identifier1, identifier2);

        private bool IsCaseSensitive => Document.GetRequiredLanguageService<ISyntaxFactsService>().IsCaseSensitive;
    }
}
