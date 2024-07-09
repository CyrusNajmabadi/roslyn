// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.ImplementType;
using Microsoft.CodeAnalysis.Internal.Log;
using Microsoft.CodeAnalysis.PooledObjects;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Shared.Utilities;

namespace Microsoft.CodeAnalysis.ImplementInterface;

using static ImplementHelpers;

internal abstract partial class AbstractImplementInterfaceService() : IImplementInterfaceService
{
    // Parts of the name `disposedValue`.  Used so we can generate a field correctly with 
    // the naming style that the user has specified.
    private static readonly ImmutableArray<string> s_disposedValueNameParts =
        ["disposed", "value"];

    public abstract bool CanImplementImplicitly { get; }
    public abstract bool HasHiddenExplicitImplementation { get; }

    public abstract bool AllowDelegateAndEnumConstraints(ParseOptions options);
    public abstract SyntaxNode AddCommentInsideIfStatement(SyntaxNode ifDisposingStatement, SyntaxTriviaList trivia);
    public abstract SyntaxNode CreateFinalizer(SyntaxGenerator generator, INamedTypeSymbol classType, string disposeMethodDisplayString);

    public abstract string ToDisplayString(IMethodSymbol disposeImplMethod, SymbolDisplayFormat format);

    protected abstract bool TryInitializeState(
        Document document, SemanticModel model, SyntaxNode interfaceNode, CancellationToken cancellationToken, out SyntaxNode classOrStructDecl, out INamedTypeSymbol classOrStructType, out ImmutableArray<INamedTypeSymbol> interfaceTypes);

    public async Task<Document> ImplementInterfaceAsync(
        Document document, ImplementTypeGenerationOptions options, SyntaxNode node, CancellationToken cancellationToken)
    {
        using (Logger.LogBlock(FunctionId.Refactoring_ImplementInterface, cancellationToken))
        {
            var model = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            var state = State.Generate(this, document, model, node, cancellationToken);
            if (state == null)
                return document;

            // TODO: https://github.com/dotnet/roslyn/issues/60990
            // While implementing just one default action, like in the case of pressing enter after interface name in VB,
            // choose to implement with the dispose pattern as that's the Dev12 behavior.
            if (ShouldImplementDisposePattern(model.Compilation, state, explicitly: false))
            {
                return await this.ImplementIDisposableInterfaceAsync(
                    document, state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented,
                    explicitly: false, state.ClassOrStructType, state.ClassOrStructDecl, options, cancellationToken).ConfigureAwait(false);
            }

            return await this.ImplementInterfaceAsync(
                document, state.MembersWithoutExplicitOrImplicitImplementationWhichCanBeImplicitlyImplemented,
                explicitly: false, state.ClassOrStructType, state.ClassOrStructDecl, options, cancellationToken).ConfigureAwait(false);
        }
    }

    public async Task<IImplementInterfaceInfo> ComputeInfoAsync(Document document, SyntaxNode node, CancellationToken cancellationToken)
    {
        var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
        var state = State.Generate(this, document, semanticModel, node, cancellationToken);
        return state;
    }

    public virtual Task<Document> ImplementInterfaceAsync(
        Document document,
        ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> unimplementedMembers,
        bool explicitly,
        INamedTypeSymbol classOrStructType,
        SyntaxNode classOrStructDecl,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        return GetUpdatedDocumentAsync(
            document, unimplementedMembers, classOrStructType, classOrStructDecl,
            [], options, cancellationToken);
    }

    private async Task<Document> GetUpdatedDocumentAsync(
        Document document,
        ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> unimplementedMembers,
        INamedTypeSymbol classOrStructType,
        SyntaxNode classOrStructDecl,
        ImmutableArray<ISymbol> extraMembers,
        ImplementTypeGenerationOptions options,
        CancellationToken cancellationToken)
    {
        var tree = await document.GetRequiredSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
        var compilation = await document.Project.GetRequiredCompilationAsync(cancellationToken).ConfigureAwait(false);

        var isComImport = unimplementedMembers.Any(static t => t.type.IsComImport);

        var memberDefinitions = GenerateMembers(
            compilation, tree.Options, unimplementedMembers, options.ImplementTypeOptions.PropertyGenerationBehavior);

        // Only group the members in the destination if the user wants that *and* 
        // it's not a ComImport interface.  Member ordering in ComImport interfaces 
        // matters, so we don't want to much with them.
        var groupMembers = !isComImport &&
            options.ImplementTypeOptions.InsertionBehavior == ImplementTypeInsertionBehavior.WithOtherMembersOfTheSameKind;

        return await CodeGenerator.AddMemberDeclarationsAsync(
            new CodeGenerationSolutionContext(
                document.Project.Solution,
                new CodeGenerationContext(
                    contextLocation: classOrStructDecl.GetLocation(),
                    autoInsertionLocation: groupMembers,
                    sortMembers: groupMembers),
                options.FallbackOptions),
            classOrStructType,
            memberDefinitions.Concat(extraMembers),
            cancellationToken).ConfigureAwait(false);
    }

    private ImmutableArray<ISymbol> GenerateMembers(
        Compilation compilation,
        ParseOptions options,
        ImmutableArray<(INamedTypeSymbol type, ImmutableArray<ISymbol> members)> unimplementedMembers,
        ImplementTypePropertyGenerationBehavior propertyGenerationBehavior)
    {
        // As we go along generating members we may end up with conflicts.  For example, say
        // you have "interface IGoo { string Bar { get; } }" and "interface IQuux { int Bar
        // { get; } }" and we need to implement both 'Bar' methods.  The second will have to
        // be explicitly implemented as it will conflict with the first.  So we need to keep
        // track of what we've actually implemented so that we can check further interface
        // members against both the actual type and that list.
        //
        // Similarly, if you have two interfaces with the same member, then we don't want to
        // implement that member twice.  
        //
        // Note: if we implement a method explicitly then we do *not* add it to this list.
        // That's because later members won't conflict with it even if they have the same
        // signature otherwise.  i.e. if we chose to implement IGoo.Bar explicitly, then we
        // could implement IQuux.Bar implicitly (and vice versa).
        using var _1 = ArrayBuilder<ISymbol>.GetInstance(out var implementedVisibleMembers);
        using var _2 = ArrayBuilder<ISymbol>.GetInstance(out var implementedMembers);

        foreach (var (_, unimplementedInterfaceMembers) in unimplementedMembers)
        {
            foreach (var unimplementedInterfaceMember in unimplementedInterfaceMembers)
            {
                var members = GenerateMembers(
                    compilation, options,
                    unimplementedInterfaceMember, implementedVisibleMembers,
                    propertyGenerationBehavior);
                foreach (var member in members)
                {
                    if (member is null)
                        continue;

                    implementedMembers.Add(member);

                    if (!(member.ExplicitInterfaceImplementations().Any() && this.HasHiddenExplicitImplementation))
                        implementedVisibleMembers.Add(member);
                }
            }
        }

        return implementedMembers.ToImmutableAndClear();
    }

    private IEnumerable<ISymbol?> GenerateMembers(
        Document document,
        Compilation compilation,
        ParseOptions options,
        ISymbol member,
        ArrayBuilder<ISymbol> implementedVisibleMembers,
        ImplementTypePropertyGenerationBehavior propertyGenerationBehavior)
    {
        // First check if we already generate a member that matches the member we want to
        // generate.  This can happen in C# when you have interfaces that have the same
        // method, and you are implementing implicitly.  For example:
        //
        // interface IGoo { void Goo(); }
        //
        // interface IBar : IGoo { new void Goo(); }
        //
        // class C : IBar
        //
        // In this case we only want to generate 'Goo' once.
        if (HasMatchingMember(implementedVisibleMembers, member))
            return [];

        var memberName = DetermineMemberName(member, implementedVisibleMembers);

        // See if we need to generate an invisible member.  If we do, then reset the name
        // back to what then member wants it to be.
        var generateInvisibleMember = ShouldGenerateInvisibleMember(options, member, memberName);
        memberName = generateInvisibleMember ? member.Name : memberName;

        // The language doesn't allow static abstract implementations of interface methods. i.e,
        // Only interface member is declared abstract static, but implementation should be only static.
        var generateAbstractly = !member.IsStatic && !generateInvisibleMember && Abstractly;

        // Check if we need to add 'new' to the signature we're adding.  We only need to do this
        // if we're not generating something explicit and we have a naming conflict with
        // something in our base class hierarchy.
        var addNew = !generateInvisibleMember && HasNameConflict(member, memberName, State.ClassOrStructType.GetBaseTypes());

        // Check if we need to add 'unsafe' to the signature we're generating.
        var syntaxFacts = document.GetRequiredLanguageService<ISyntaxFactsService>();
        var addUnsafe = member.RequiresUnsafeModifier() && !syntaxFacts.IsUnsafeContext(State.InterfaceNode);

        return GenerateMembers(
            compilation, member, memberName, generateInvisibleMember, generateAbstractly,
            addNew, addUnsafe, propertyGenerationBehavior);
    }

    private bool HasMatchingMember(ArrayBuilder<ISymbol> implementedVisibleMembers, ISymbol member)
    {
        // If this is a language that doesn't support implicit implementation then no
        // implemented members will ever match.  For example, if you have:
        //
        // Interface IGoo : sub Goo() : End Interface
        //
        // Interface IBar : Inherits IGoo : Shadows Sub Goo() : End Interface
        //
        // Class C : Implements IBar
        //
        // We'll first end up generating:
        //
        // Public Sub Goo() Implements IGoo.Goo
        //
        // However, that same method won't be viable for IBar.Goo (unlike C#) because it
        // explicitly specifies its interface).
        if (!this.CanImplementImplicitly)
        {
            return false;
        }

        return implementedVisibleMembers.Any(m => MembersMatch(m, member));
    }

    private bool MembersMatch(ISymbol member1, ISymbol member2)
    {
        if (member1.Kind != member2.Kind)
            return false;

        if (member1.DeclaredAccessibility != member2.DeclaredAccessibility ||
            member1.IsStatic != member2.IsStatic)
        {
            return false;
        }

        if (member1.ExplicitInterfaceImplementations().Any() || member2.ExplicitInterfaceImplementations().Any())
            return false;

        return SignatureComparer.Instance.HaveSameSignatureAndConstraintsAndReturnTypeAndAccessors(
            member1, member2, this.IsCaseSensitive);
    }

    private bool ShouldGenerateInvisibleMember(IImplementInterfaceInfo state, bool explicitly, ParseOptions options, ISymbol member, string memberName)
    {
        if (this.HasHiddenExplicitImplementation)
        {
            // User asked for an explicit (i.e. invisible) member.
            if (explicitly)
                return true;

            // Have to create an invisible member if we have constraints we can't express
            // with a visible member.
            if (this.HasUnexpressibleConstraint(options, member))
                return true;

            // If we had a conflict with a member of the same name, then we have to generate
            // as an invisible member.
            if (member.Name != memberName)
                return true;

            // If the member is less accessible than type, for which we are implementing it,
            // then only explicit implementation is valid.
            if (IsLessAccessibleThan(member, state.ClassOrStructType))
                return true;
        }

        // Can't generate an invisible member if the language doesn't support it.
        return false;
    }

    private bool HasUnexpressibleConstraint(ParseOptions options, ISymbol member)
    {
        // interface IGoo<T> { void Bar<U>() where U : T; }
        //
        // class A : IGoo<int> { }
        //
        // In this case we cannot generate an implement method for Bar.  That's because we'd
        // need to say "where U : int" and that's disallowed by the language.  So we must
        // generate something explicit here.
        if (member is not IMethodSymbol method)
            return false;

        var allowDelegateAndEnumConstraints = this.AllowDelegateAndEnumConstraints(options);
        return method.TypeParameters.Any(t => IsUnexpressibleTypeParameter(t, allowDelegateAndEnumConstraints));
    }

    private static bool IsUnexpressibleTypeParameter(
        ITypeParameterSymbol typeParameter,
        bool allowDelegateAndEnumConstraints)
    {
        var condition1 = typeParameter.ConstraintTypes.Count(t => t.TypeKind == TypeKind.Class) >= 2;
        var condition2 = typeParameter.ConstraintTypes.Any(static (ts, allowDelegateAndEnumConstraints) => ts.IsUnexpressibleTypeParameterConstraint(allowDelegateAndEnumConstraints), allowDelegateAndEnumConstraints);
        var condition3 = typeParameter.HasReferenceTypeConstraint && typeParameter.ConstraintTypes.Any(static ts => ts.IsReferenceType && ts.SpecialType != SpecialType.System_Object);

        return condition1 || condition2 || condition3;
    }
}
