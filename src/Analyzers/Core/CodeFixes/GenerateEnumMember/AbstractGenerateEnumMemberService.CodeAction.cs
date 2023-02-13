// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeGeneration;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.Shared.Utilities;

namespace Microsoft.CodeAnalysis.GenerateEnumMember
{
    internal abstract partial class AbstractGenerateEnumMemberService<TService, TSimpleNameSyntax, TExpressionSyntax>
    {
        private partial class GenerateEnumMemberCodeAction : CodeAction
        {
            private readonly Document _document;
            private readonly State _state;
            private readonly CodeAndImportGenerationOptionsProvider _fallbackOptions;

            public GenerateEnumMemberCodeAction(
                Document document,
                State state,
                CodeAndImportGenerationOptionsProvider fallbackOptions)
            {
                _document = document;
                _state = state;
                _fallbackOptions = fallbackOptions;
            }

            public override string Title
                => string.Format(CodeFixesResources.Generate_enum_member_0, _state.IdentifierToken.ValueText);

            protected override async Task<Document> GetChangedDocumentAsync(CancellationToken cancellationToken)
            {
                var languageServices = _document.Project.Solution.Services.GetLanguageServices(_state.TypeToGenerateIn.Language);
                var codeGenerator = languageServices.GetRequiredService<ICodeGenerationService>();
                var semanticFacts = languageServices.GetRequiredService<ISemanticFactsService>();

                var value = semanticFacts.LastEnumValueHasInitializer(_state.TypeToGenerateIn)
                    ? EnumValueUtilities.GetNextEnumValue(_state.TypeToGenerateIn)
                    : null;

                var result = await codeGenerator.AddFieldAsync(
                    new CodeGenerationSolutionContext(
                        _document.Project.Solution,
                        new CodeGenerationContext(
                            contextLocation: _state.IdentifierToken.GetLocation()),
                        _fallbackOptions),
                    _state.TypeToGenerateIn,
                    CodeGenerationSymbolFactory.CreateFieldSymbol(
                        attributes: default,
                        accessibility: Accessibility.Public,
                        modifiers: default,
                        type: _state.TypeToGenerateIn,
                        name: _state.IdentifierToken.ValueText,
                        hasConstantValue: value != null,
                        constantValue: value),
                    cancellationToken).ConfigureAwait(false);

                return result;
            }
        }
    }
}
