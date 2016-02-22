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
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.CodeFixes.SimplifyProperty
{
    [ExportCodeFixProvider(LanguageNames.CSharp), Shared]
    internal class SimplePropertyDiagnosticCodeFixProvider : CodeFixProvider
    {
        public override ImmutableArray<string> FixableDiagnosticIds { get; } =
            ImmutableArray.Create(IDEDiagnosticIds.SimplifyPropertyId);

        public override FixAllProvider GetFixAllProvider() => BatchFixAllProvider.Instance;

        public override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            context.RegisterCodeFix(new MyCodeAction("SimplifyProperty", 
                c => UpdateSolutionAsync(context, c)), context.Diagnostics);
            return SpecializedTasks.EmptyTask;
        }

        private async Task<Solution> UpdateSolutionAsync(CodeFixContext context, CancellationToken cancellationToken)
        {
            var root = await context.Document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var propertyDeclaration = (PropertyDeclarationSyntax)root.FindNode(context.Span);

            var returnStatement = (ReturnStatementSyntax)propertyDeclaration.AccessorList.Accessors[0].Body.Statements[0];
            var semicolonToken = returnStatement.SemicolonToken.WithTrailingTrivia(
                returnStatement.SemicolonToken.TrailingTrivia.Where(t => t.Kind() != SyntaxKind.EndOfLineTrivia));
            var newPropertyDeclaration = propertyDeclaration
                .WithAccessorList(null)
                .WithExpressionBody(SyntaxFactory.ArrowExpressionClause(returnStatement.Expression))
                .WithSemicolonToken(semicolonToken)
                .WithAppendedTrailingTrivia(propertyDeclaration.AccessorList.GetTrailingTrivia());

            var newDocument = context.Document.WithSyntaxRoot(root.ReplaceNode(
                propertyDeclaration, newPropertyDeclaration));

            return newDocument.Project.Solution;
        }

        private class MyCodeAction : CodeAction.SolutionChangeAction
        {
            public MyCodeAction(
                string title,
                Func<CancellationToken, Task<Solution>> createChangedSolution) 
                : base(title, createChangedSolution, title)
            {
            }
        }
    }
}
