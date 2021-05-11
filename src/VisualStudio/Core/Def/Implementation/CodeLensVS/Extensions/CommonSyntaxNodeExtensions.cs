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
    }
}
