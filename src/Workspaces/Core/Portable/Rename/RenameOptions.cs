// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Runtime.Serialization;
using Microsoft.CodeAnalysis.ChangeNamespace;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Simplification;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Rename
{
    [Obsolete("Use SymbolRenameOptions or DocumentRenameOptions instead")]
    public static class RenameOptions
    {
#pragma warning disable RS0030 // Do not used banned APIs: Option<T>
        public static Option<bool> RenameOverloads { get; } = new Option<bool>(nameof(RenameOptions), nameof(RenameOverloads), defaultValue: false);

        public static Option<bool> RenameInStrings { get; } = new Option<bool>(nameof(RenameOptions), nameof(RenameInStrings), defaultValue: false);

        public static Option<bool> RenameInComments { get; } = new Option<bool>(nameof(RenameOptions), nameof(RenameInComments), defaultValue: false);

        public static Option<bool> PreviewChanges { get; } = new Option<bool>(nameof(RenameOptions), nameof(PreviewChanges), defaultValue: false);
#pragma warning restore
    }

    /// <summary>
    /// Options for renaming a symbol.
    /// </summary>
    /// <param name="RenameOverloads">If the symbol is a method rename its overloads as well.</param>
    /// <param name="RenameInStrings">Rename identifiers in string literals that match the name of the symbol.</param>
    /// <param name="RenameInComments">Rename identifiers in comments that match the name of the symbol.</param>
    /// <param name="RenameFile">If the symbol is a type renames the file containing the type declaration as well.</param>
    /// <param name="IncludeSpans">If not equal to <see langword="default"/> then rename will only update locations
    /// within the provided document spans.</param>
    /// <param name="IgnoreSpans">If not equal to <see langword="default"/> then rename will not update locations
    /// within the provided document spans.</param>
    public readonly record struct SymbolRenameOptions(
        bool RenameOverloads = false,
        bool RenameInStrings = false,
        bool RenameInComments = false,
        bool RenameFile = false,
        ImmutableArray<DocumentSpan> IncludeSpans = default,
        ImmutableArray<DocumentSpan> IgnoreSpans = default)
    {
        // Binary compat constructor
        [EditorBrowsable(EditorBrowsableState.Never)]
        public SymbolRenameOptions(
            bool RenameOverloads,
            bool RenameInStrings,
            bool RenameInComments,
            bool RenameFile) : this(RenameOverloads, RenameInStrings, RenameInComments, RenameFile, default, default)
        {
        }
    }

    [DataContract]
    internal readonly record struct SerializableSymbolRenameOptions(
        [property: DataMember(Order = 0)] bool RenameOverloads,
        [property: DataMember(Order = 1)] bool RenameInStrings,
        [property: DataMember(Order = 2)] bool RenameInComments,
        [property: DataMember(Order = 3)] bool RenameFile,
        [property: DataMember(Order = 3)] ImmutableArray<(DocumentId, TextSpan)> IncludeSpans,
        [property: DataMember(Order = 3)] ImmutableArray<(DocumentId, TextSpan)> IgnoreSpans)
    {
        public static SerializableSymbolRenameOptions Dehydrate(SymbolRenameOptions options)
            => new(options.RenameOverloads, options.RenameInStrings, options.RenameInComments, options.RenameFile,
                options.IncludeSpans == default ? default : options.IncludeSpans.SelectAsArray(ds => (ds.Document.Id, ds.SourceSpan)),
                options.IgnoreSpans == default ? default : options.IgnoreSpans.SelectAsArray(ds => (ds.Document.Id, ds.SourceSpan)));

        public SymbolRenameOptions Dehydrate(Solution solution)
            => new(RenameOverloads, RenameInStrings, RenameInComments, RenameFile,
                IncludeSpans == default ? default : IncludeSpans.SelectAsArray(ds => new DocumentSpan(solution.GetRequiredDocument(ds.Item1), ds.Item2)),
                IgnoreSpans == default ? default : IgnoreSpans.SelectAsArray(ds => new DocumentSpan(solution.GetRequiredDocument(ds.Item1), ds.Item2)));
    }

    /// <summary>
    /// Options for renaming a document.
    /// </summary>
    /// <param name="RenameMatchingTypeInStrings">If the document contains a type declaration with matching name rename identifiers in strings that match the name as well.</param>
    /// <param name="RenameMatchingTypeInComments">If the document contains a type declaration with matching name rename identifiers in comments that match the name as well.</param>
    [DataContract]
    public readonly record struct DocumentRenameOptions(
        [property: DataMember(Order = 0)] bool RenameMatchingTypeInStrings = false,
        [property: DataMember(Order = 1)] bool RenameMatchingTypeInComments = false);
}
