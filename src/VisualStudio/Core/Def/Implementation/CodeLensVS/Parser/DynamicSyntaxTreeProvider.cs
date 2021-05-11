// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser
{
    [Export(typeof(IDynamicSyntaxTreeProvider))]
    internal sealed class DynamicSyntaxTreeProvider : IDynamicSyntaxTreeProvider
    {
        #region Public Methods
        public IDynamicSyntaxTree CreateDynamicSyntaxTree(IParsingService parsingService)
        {
            ArgumentValidation.NotNull(parsingService, "parsingService");

            return new DynamicSyntaxTree(parsingService);
        }
        #endregion

        #region Private Classes
        // NOTE: This DymamicSyntaxTree implementation is *not* thread-safe
        private sealed class DynamicSyntaxTree : IDynamicSyntaxTree
        {
            private const string OnTextChangedMeasurementBlockName = "DYNAMIC_SYNTAX_TREE_ONTEXTCHANGED";

            private readonly IParsingService parsingService;
            private SyntaxTree currentSyntaxTree;

            public DynamicSyntaxTree(IParsingService parsingService)
            {
                ArgumentValidation.NotNull(parsingService, "parsingService");

                this.parsingService = parsingService;
            }

            public SyntaxTree CurrentSyntaxTree
            {
                get
                {
                    return this.currentSyntaxTree;
                }
            }

            public async Task UpdateAsync(ITextSnapshot textSnapshot, CancellationToken cancellationToken = default(CancellationToken))
            {
                // If possible, we want to get the syntax tree through the Roslyn Document for this snapshot.
                // That way the syntax tree will take into account the current compiler options (such as the
                // preprocessor symbols), and we're not reparsing the text if some other component has already
                // obtained the syntax tree.
                // It is also possible that the snapshot has no related Document. In that case, we still need
                // to do the parsing ourselves.
                var document = textSnapshot.GetOpenDocumentInCurrentContextWithChanges();
                if (document != null)
                {
                    this.currentSyntaxTree = await document.GetSyntaxTreeAsync().ConfigureAwait(false);

                    // verify that the tree and parsing service are still in sync, if not, reparse.
                    // this check is required because if a content type change occured
                    // codelens switches the parsing service and GetRelatedDocumentsWithChanges()
                    // could return an out of date document if workspace didn't observe that change yet.
                    if (!this.parsingService.IsValidSyntaxTree(this.currentSyntaxTree))
                    {
                        this.currentSyntaxTree = this.parsingService.Parse(textSnapshot.AsText());
                    }
                }
                else
                {
                    var sourceText = textSnapshot.AsText();

                    if (this.currentSyntaxTree == null)
                    {
                        this.currentSyntaxTree = this.parsingService.Parse(sourceText);
                    }
                    else
                    {
                        this.currentSyntaxTree = this.currentSyntaxTree.WithChangedText(sourceText);
                    }
                }
            }
        }
        #endregion
    }
}
