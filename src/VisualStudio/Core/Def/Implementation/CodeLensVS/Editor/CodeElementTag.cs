// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.Language.CodeLens;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser;
using Microsoft.VisualStudio.Text;
using Roslyn.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    internal partial class CodeElementTag : ICodeLensTag2, ICodeLensDescriptorContextProvider
    {
        private bool isDisconnected;
        private readonly CodeElementDescriptor descriptor;

        public CodeElementTag(SyntaxNodeInfo syntaxNodeInfo, string filePath, DocumentId documentId, Workspace workspace, Guid projectGuid)
        {
            Contract.ThrowIfNull(syntaxNodeInfo);
            Contract.ThrowIfNull(filePath);

            this.descriptor = new CodeElementDescriptor(syntaxNodeInfo, filePath, documentId, workspace, projectGuid);
        }

        public event EventHandler? Disconnected;

        public ICodeLensDescriptor Descriptor => this.descriptor;

        public ICodeLensDescriptorContextProvider DescriptorContextProvider => this;

        public void Disconnect()
        {
            if (!this.isDisconnected)
            {
                this.isDisconnected = true;
                this.Disconnected?.Invoke(this, EventArgs.Empty);
            }
        }

        #region ICodeLensDescriptorContextProvider

        public async Task<CodeLensDescriptorContext?> GetCurrentContextAsync()
        {
            if (this.descriptor.SyntaxNode != null)
            {
                var solution = this.descriptor.Workspace.CurrentSolution;
                var document = solution.GetDocument(this.descriptor.DocumentId) ??
                               await solution.GetSourceGeneratedDocumentAsync(this.descriptor.DocumentId, CancellationToken.None).ConfigureAwait(false);

                if (document != null)
                {
                    var currentSyntaxNode = await this.descriptor.SyntaxNode.GetCurrentSyntaxNodeAsync(document).ConfigureAwait(false);
                    if (currentSyntaxNode != null)
                    {
                        var fullyQualifiedName = await currentSyntaxNode.GetFullyQualifiedNameAsync(document).ConfigureAwait(false);
                        var lineSpan = currentSyntaxNode.GetLocation().GetLineSpan();

                        return new CodeLensDescriptorContext(
                            applicableSpan: Span.FromBounds(currentSyntaxNode.Span.Start, currentSyntaxNode.Span.End),
                            properties: new Dictionary<object, object?>()
                            {
                                { "VisualStudioProcessId", VisualStudioProcessId },
                                { "OutputFilePath", document.Project.OutputFilePath },
                                { "FullyQualifiedName", fullyQualifiedName },
                                { "StartLine", lineSpan.StartLinePosition.Line },
                                { "StartColumn", lineSpan.StartLinePosition.Character },
                                { "RoslynDocumentIdGuid", document.Id.Id.ToString() },
                                { "RoslynProjectIdGuid", document.Id.ProjectId.Id.ToString() },
                            });
                    }
                }
            }

            return null;
        }

        private static readonly int VisualStudioProcessId = Process.GetCurrentProcess().Id;

        #endregion
    }
}
