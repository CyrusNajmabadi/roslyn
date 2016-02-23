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
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp.InvokeDelegateWithConditionalAccess
{
    [ExportCodeFixProvider(LanguageNames.CSharp), Shared]
    internal class RemovePrivateSetterCodeFixProvider : CodeFixProvider
    {
        public override ImmutableArray<string> FixableDiagnosticIds { get; } =
            ImmutableArray.Create(RemovePrivateSetterAnalyzer.Descriptor.Id);

        public override FixAllProvider GetFixAllProvider() => BatchFixAllProvider.Instance;

        public override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            context.RegisterCodeFix(new MyCodeAction(
                nameof(RemovePrivateSetterAnalyzer),
                c => UpdateDocumentAsync(context, c)), context.Diagnostics);
            return SpecializedTasks.EmptyTask;
        }

        private async Task<Solution> UpdateDocumentAsync(CodeFixContext context, CancellationToken cancellationToken)
        {
            var root = await context.Document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var node = (AccessorDeclarationSyntax)root.FindNode(context.Span);
            var newRoot = root.RemoveNode(node, SyntaxRemoveOptions.AddElasticMarker);
            return context.Document.WithSyntaxRoot(newRoot).Project.Solution;
        }

        private class MyCodeAction : CodeAction.SolutionChangeAction
        {
            public MyCodeAction(string title, Func<CancellationToken, Task<Solution>> createChangedSolution) 
                : base(title, createChangedSolution, title)
            {
            }
        }
    }
}
