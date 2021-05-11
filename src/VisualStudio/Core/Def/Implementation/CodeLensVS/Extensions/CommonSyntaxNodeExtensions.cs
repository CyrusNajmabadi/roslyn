// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;
using Roslyn.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Extensions
{
    internal static class SyntaxNodeExtensions
    {
        private static readonly SymbolDisplayFormat MethodDisplayFormat =
            new SymbolDisplayFormat(
                typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
                memberOptions: SymbolDisplayMemberOptions.IncludeContainingType);

        public static async Task<string> GetFullyQualifiedNameAsync(
            this SyntaxNode syntaxNode,
            Document document,
            CancellationToken cancellationToken = default)
        {
            var semanticModel = await document.GetRequiredSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            var symbol = semanticModel.GetDeclaredSymbol(syntaxNode, cancellationToken);

            if (symbol == null)
            {
                return string.Empty;
            }

            var parts = symbol.ToDisplayParts(MethodDisplayFormat);

            var previousWasType = false;
            var builder = new StringBuilder();
            for (var index = 0; index < parts.Length; index++)
            {
                var part = parts[index];
                if (previousWasType &&
                    part.Kind == SymbolDisplayPartKind.Punctuation &&
                    index < parts.Length - 1)
                {
                    switch (parts[index + 1].Kind)
                    {
                        case SymbolDisplayPartKind.ClassName:
                        case SymbolDisplayPartKind.DelegateName:
                        case SymbolDisplayPartKind.EnumName:
                        case SymbolDisplayPartKind.ErrorTypeName:
                        case SymbolDisplayPartKind.InterfaceName:
                        case SymbolDisplayPartKind.StructName:
                            builder.Append('+');
                            break;

                        default:
                            builder.Append(part);
                            break;
                    }
                }
                else
                {
                    builder.Append(part);
                }

                previousWasType = part.Kind == SymbolDisplayPartKind.ClassName ||
                                  part.Kind == SymbolDisplayPartKind.InterfaceName ||
                                  part.Kind == SymbolDisplayPartKind.StructName;
            }

            return builder.ToString();
        }

        /// <summary>
        /// Get the identifier name of the node
        /// </summary>
        public static string GetIdentifierName(this SyntaxNode node)
        {
            if (node != null)
            {
                if (node.Language.Equals(LanguageNames.VisualBasic))
                {
                    return GetVisualBasicIdentifierName(node);
                }
                else if (node.Language.Equals(LanguageNames.CSharp))
                {
                    return GetCSharpIdentifierName(node);
                }

                Debug.Assert(false, "Language should be either C# or VB");
            }

            return string.Empty;
        }

        /// <summary>
        /// Get the next parent that is not associated with this node
        /// </summary>
        public static SyntaxNode? GetNextParent(this SyntaxNode node)
        {
            if (node != null)
            {
                if (node.Language.Equals(LanguageNames.CSharp))
                {
                    return node.Parent;
                }
                else if (node.Language.Equals(LanguageNames.VisualBasic))
                {
                    return GetVisualBasicParent(node);
                }

                Debug.Assert(false, "Language should be either C# or VB");
            }

            return null;
        }

        // another place where we need to split methods per language and make sure
        // it doesn't get inlined. otherwise, dlls for both language will be brought 
        // into process regardless which language is actually used in the solution.
        [MethodImpl(MethodImplOptions.NoInlining)]
        private static SyntaxNode GetVisualBasicParent(SyntaxNode node)
        {
            return VisualBasicSyntaxNodeExtensions.GetParentNode(node);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static string GetCSharpIdentifierName(SyntaxNode node)
        {
            return CSharpSyntaxNodeExtensions.GetIdentifierName(node);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static string GetVisualBasicIdentifierName(SyntaxNode node)
        {
            return VisualBasicSyntaxNodeExtensions.GetIdentifierName(node);
        }

        /// <summary>
        /// Returns the current SyntaxNode for a particular syntax node.
        /// </summary>
        /// <param name="syntaxNode">The syntax node</param>
        /// <returns>The current SyntaxNode in the current syntax tree.</returns>
        public static async Task<SyntaxNode?> GetCurrentSyntaxNodeAsync(this SyntaxNode syntaxNode, Document document, CancellationToken cancellationToken)
        {
            Contract.ThrowIfNull(syntaxNode);
            Contract.ThrowIfNull(document);

            var position = syntaxNode.LocateAnchorPosition();

            if (syntaxNode.SyntaxTree.TryGetText(out var sourceText))
            {
                var sourceTextSnapshot = sourceText.FindCorrespondingEditorTextSnapshot();
                if (sourceTextSnapshot != null)
                {
                    var sourceSnapshotPoint = new SnapshotPoint(sourceTextSnapshot, position);
                    position = sourceSnapshotPoint.TranslateTo(sourceTextSnapshot.TextBuffer.CurrentSnapshot, PointTrackingMode.Positive);
                }
                else
                {
                    Debug.Fail("Couldn't get the ITextSnapshot from the SourceText");
                }
            }
            else
            {
                Debug.Fail("Couldn't get SourceText from the SyntaxNode");
            }

            var kind = syntaxNode.RawKind;
            var tree = await document.GetRequiredSyntaxTreeAsync(cancellationToken).ConfigureAwait(true);
            var root = tree.GetRoot(cancellationToken);

            if (root != null && position < tree.Length)
            {
                /* When we tag a node in the document, we re-use that tag which means 
                 * that we could be serializing the position of an old node in an old 
                 * tree to Roslyn, which is always working of the latest tree. To handle
                 * this, we translate the position of the identifier in our old tree 
                 * to where it is in the latest snapshot. This handles cases where the 
                 * user formats the document, adds newlines, tabs, stuff above or below,
                 * etc. 
                 *
                 * In the refactor case, what happens is the following. The refactor 
                 * procedure inserts new text at the position that we tagged, and 
                 * then removes the old text. The result of this operation is that the 
                 * tracking span now translates our tagged position forward by a number 
                 * equal to the length of the new identifier string. 
                 *
                 * For instance – say we tagged “Foo” at position 10 in the document. 
                 * Note that 10 refers  to the location of the ‘F’ character. Then the 
                 * user refactors the name “Foo” and types “Foo2”. The tracking span 
                 * now says position 10 is at position 14. We are still able to find the 
                 * token we are looking for on the remote side. However, comparing the 
                 * start position of that node to the position we serialized across the 
                 * wire fails. So we get ‘- references’ because the node couldn’t be 
                 * found.
                 *
                 * This is why we do Span.IntersectsWith check below. It prevents 
                 * us from finding the wrong node, and it handles cases where refactor
                 * procedures or other auto-formatting events cause the position to move
                 * out from the position we originally tagged.
                 */
                Contract.ThrowIfNull(root);
                var token = root.FindToken(position);
                if (token.Span.IntersectsWith(position))
                {
                    var node = token.Parent;

                    // Roslyn will sometimes give us a syntax token that's not directly parented to
                    // the node we tagged. One example of this are attributes: the syntax node
                    // that we care about (the method declaration) is actually two node's above the 
                    // token that we find at this position.
                    // The easiest way to handle this is to walk up the tree until we find the first 
                    // node that matches the kind we initially tagged at this line.
                    while (node != null && node.RawKind != kind)
                    {
                        node = node.Parent;
                    }

                    if (node != null)
                    {
                        return node;
                    }
                }
            }

            return null;
        }
    }
}
