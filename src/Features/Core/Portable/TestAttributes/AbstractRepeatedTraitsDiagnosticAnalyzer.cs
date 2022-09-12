// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Text;
using Microsoft.CodeAnalysis.CodeStyle;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.LanguageService;

namespace Microsoft.CodeAnalysis.TestAttributes
{
    internal static class RepeatedTraitsHelpers
    {
        public static bool IsTraitAttribute(
            SyntaxNode attribute,
            SyntaxGenerator generator,
            [NotNullWhen(true)] out string? traitName,
            [NotNullWhen(true)] out string? traitValue)
        {
            traitName = null;
            traitValue = null;

            var syntaxFacts = generator.SyntaxFacts;
            var attributeName = syntaxFacts.GetNameOfAttribute(attribute);
            attributeName = syntaxFacts.IsQualifiedName(attributeName) ? syntaxFacts.GetRightSideOfDot(attributeName) : attributeName;
            if (!syntaxFacts.IsSimpleName(attributeName))
                return false;

            var arguments = generator.GetAttributeArguments(attribute);
            if (arguments.Count != 2)
                return false;

            traitName = GetArgumentName(arguments[0]);
            traitValue = GetArgumentName(arguments[1]);

            return traitName != null && traitValue != null;
            
            string? GetArgumentName(SyntaxNode syntaxNode)
            {
                var expression =
                    syntaxFacts.IsArgument(syntaxNode) ? syntaxFacts.GetExpressionOfArgument(syntaxNode) :
                    syntaxFacts.IsAttributeArgument(syntaxNode) ? syntaxFacts.GetExpressionOfAttributeArgument(syntaxNode) : null;

                if (syntaxFacts.IsMemberAccessExpression(expression))
                {
                    var right = syntaxFacts.GetNameOfMemberAccessExpression(expression);
                    return syntaxFacts.GetIdentifierOfSimpleName(right).ValueText;
                }

                return null;
            }
        }
    }

    internal abstract class AbstractRepeatedTraitsDiagnosticAnalyzer<TSyntaxKind, TClassDeclarationSyntax>
        : AbstractBuiltInCodeStyleDiagnosticAnalyzer
        where TSyntaxKind : struct
        where TClassDeclarationSyntax : SyntaxNode
    {
        protected AbstractRepeatedTraitsDiagnosticAnalyzer()
            : base(IDEDiagnosticIds.RepeatedTraitsDiagnosticId,
                   EnforceOnBuildValues.RepeatedTraits,
                   CodeStyleOptions2.RepeatedTraits,
                   "RepeatedTraits",
                   "RepeatedTraits")
        {
        }

        protected abstract ISyntaxFacts SyntaxFacts { get; }
        protected abstract SyntaxGenerator SyntaxGenerator { get; }

        private ISyntaxKinds SyntaxKinds => SyntaxFacts.SyntaxKinds;

        public override DiagnosticAnalyzerCategory GetAnalyzerCategory()
            => DiagnosticAnalyzerCategory.SemanticDocumentAnalysis;

        protected override void InitializeWorker(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeClassDeclaration, this.SyntaxKinds.Convert<TSyntaxKind>(this.SyntaxKinds.ClassDeclaration));
        }

        private void AnalyzeClassDeclaration(SyntaxNodeAnalysisContext context)
        {
            var classDeclaration = (TClassDeclarationSyntax)context.Node;
            if (classDeclaration.Language != LanguageNames.VisualBasic)
                return;

            var members = SyntaxFacts.GetMembersOfTypeDeclaration(classDeclaration);

            string? traitName = null, traitValue = null;
            var count = 0;

            foreach (var member in members)
            {
                var attributeLists = this.SyntaxFacts.GetAttributeLists(member);
                foreach (var attributeList in attributeLists)
                {
                    foreach (var attribute in this.SyntaxFacts.GetAttributesOfAttributeList(attributeList))
                    {
                        if (RepeatedTraitsHelpers.IsTraitAttribute(attribute, this.SyntaxGenerator, out var currentTraitName, out var currentTraitValue))
                        {
                            traitName ??= currentTraitName;
                            traitValue ??= currentTraitValue;

                            if (traitName != currentTraitName || traitValue != currentTraitValue)
                                return;

                            count++;
                        }
                    }
                }
            }

            if (count < 2)
                return;

            if (traitName is null || traitValue is null)
                return;

            context.ReportDiagnostic(DiagnosticHelper.Create(
                Descriptor,
                classDeclaration.GetFirstToken().GetLocation(),
                ReportDiagnostic.Warn,
                additionalLocations: ImmutableArray.Create(classDeclaration.GetLocation()),
                properties: null));
        }
    }
}
