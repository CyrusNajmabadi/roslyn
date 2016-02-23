using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp.InvokeDelegateWithConditionalAccess
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal class RemovePrivateSetterAnalyzer : DiagnosticAnalyzer
    {
        public static readonly DiagnosticDescriptor Descriptor = new DiagnosticDescriptor(
            nameof(RemovePrivateSetterAnalyzer),
            nameof(RemovePrivateSetterAnalyzer),
            nameof(RemovePrivateSetterAnalyzer),
            DiagnosticCategory.Style,
            DiagnosticSeverity.Hidden,
            isEnabledByDefault: true,
            customTags: DiagnosticCustomTags.Unnecessary);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(Descriptor);


        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(ProcessNode, SyntaxKind.SetAccessorDeclaration);
        }

        private void ProcessNode(SyntaxNodeAnalysisContext context)
        {
            var accessor = (AccessorDeclarationSyntax)context.Node;
            if (accessor.Modifiers.Any(SyntaxKind.PrivateKeyword) &&
                accessor.Body == null)
            {
                context.ReportDiagnostic(Diagnostic.Create(
                    Descriptor, accessor.GetLocation()));
            }
        }
    }
}
