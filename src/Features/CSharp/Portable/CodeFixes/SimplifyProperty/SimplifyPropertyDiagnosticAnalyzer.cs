using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp.CodeFixes.SimplifyProperty
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal class SimplifyPropertyDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        private static readonly DiagnosticDescriptor s_descriptor = new DiagnosticDescriptor(
            IDEDiagnosticIds.SimplifyPropertyId,
            "SimplifyProperty",
            "SimplifyProperty",
            DiagnosticCategory.Style,
            DiagnosticSeverity.Hidden,
            isEnabledByDefault: true,
            customTags: DiagnosticCustomTags.Unnecessary);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
            ImmutableArray.Create(s_descriptor);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(ProcessNode, SyntaxKind.PropertyDeclaration);
        }

        private void ProcessNode(SyntaxNodeAnalysisContext context)
        {
            var propertyDeclaration = (PropertyDeclarationSyntax)context.Node;
            if (propertyDeclaration.AccessorList.Accessors.Count != 1 ||
                !propertyDeclaration.AccessorList.Accessors[0].IsKind(SyntaxKind.GetAccessorDeclaration))
            {
                return;
            }

            var accessor = propertyDeclaration.AccessorList.Accessors[0];
            if (!accessor.Body.IsKind(SyntaxKind.Block))
            {
                return;
            }

            if (accessor.AttributeLists.Count > 0)
            {
                return;
            }

            var block = (BlockSyntax)accessor.Body;
            if (block.Statements.Count != 1)
            {
                return;
            }

            var statement = block.Statements[0];
            if (!statement.IsKind(SyntaxKind.ReturnStatement))
            {
                return;
            }

            var returnStatement = (ReturnStatementSyntax)statement;
            if (returnStatement.Expression == null)
            {
                return;
            }

            if (!IsSimpleExpression(returnStatement.Expression))
            {
                return;
            }

            if (HasAnyNonWhitespaceTrivia(accessor))
            {
                return;
            }

            context.ReportDiagnostic(Diagnostic.Create(
                s_descriptor, propertyDeclaration.GetLocation()));
        }

        private bool IsSimpleExpression(ExpressionSyntax expression)
        {
            if (expression.IsKind(SyntaxKind.IdentifierName))
            {
                return true;
            }
            else if (expression.IsKind(SyntaxKind.SimpleMemberAccessExpression))
            {
                var accessExpression = (MemberAccessExpressionSyntax)expression;
                return IsSimpleExpression(accessExpression.Expression);
            }
            else if (expression.IsKind(SyntaxKind.ThisExpression) || 
                expression.IsKind(SyntaxKind.BaseExpression))
            {
                return true;
            }

            return false;
        }

        private bool HasAnyNonWhitespaceTrivia(AccessorDeclarationSyntax accessor)
        {
            return accessor.DescendantTrivia().Any(t =>
            {
                if (!t.IsKind(SyntaxKind.WhitespaceTrivia) &&
                    !t.IsKind(SyntaxKind.EndOfLineTrivia))
                {
                    // Allow trivia on the semicolon in the return statement.
                    // We're going to move that to our final property, so 
                    // we'll still preserve that trivia.
                    if (t.Token.Kind() != SyntaxKind.SemicolonToken)
                    {
                        return true;
                    }
                }

                return false;
            });
        }
    }
}
