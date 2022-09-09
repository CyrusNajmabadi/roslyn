// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.LanguageService;
using Microsoft.CodeAnalysis.Shared.Extensions;

namespace Microsoft.CodeAnalysis.TestAttributes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, LanguageNames.VisualBasic), Shared]
    internal sealed class RepeatedTraitsCodeFixProvider : SyntaxEditorBasedCodeFixProvider
    {
        [ImportingConstructor]
        [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
        public RepeatedTraitsCodeFixProvider()
        {
        }

        public override ImmutableArray<string> FixableDiagnosticIds { get; } = ImmutableArray.Create(IDEDiagnosticIds.RepeatedTraitsDiagnosticId);

        public override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            RegisterCodeFix(context, "RemoveRepeatedTraits", "RemoveRepeatedTraits");
            return Task.CompletedTask;
        }

        protected override Task FixAllAsync(
            Document document,
            ImmutableArray<Diagnostic> diagnostics,
            SyntaxEditor editor,
            CodeActionOptionsProvider fallbackOptions,
            CancellationToken cancellationToken)
        {
            var syntaxFacts = document.GetRequiredLanguageService<ISyntaxFactsService>();

            foreach (var diagnostic in diagnostics.OrderByDescending(d => d.Location.SourceSpan.Start))
            {
                var classDeclaration = diagnostic.AdditionalLocations.Single().FindNode(getInnermostNodeForTie: true, cancellationToken);

                SyntaxNode? firstTraitAttribute = null;
                foreach (var member in syntaxFacts.GetMembersOfTypeDeclaration(classDeclaration))
                {
                    var attributeLists = syntaxFacts.GetAttributeLists(member);
                    foreach (var attributeList in attributeLists)
                    {
                        foreach (var attribute in syntaxFacts.GetAttributesOfAttributeList(attributeList))
                        {
                            if (RepeatedTraitsHelpers.IsTraitAttribute(attribute, editor.Generator, out _, out _))
                            {
                                firstTraitAttribute ??= attribute;
                                editor.RemoveNode(attribute);
                                goto outer;
                            }
                        }
                    }

outer:
                    ;
                }

                if (firstTraitAttribute != null)
                    editor.AddAttribute(classDeclaration, firstTraitAttribute);
            }

            return Task.CompletedTask;
        }
    }
}
