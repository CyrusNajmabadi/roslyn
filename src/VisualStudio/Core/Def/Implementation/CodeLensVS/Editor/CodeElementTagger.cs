// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Caching;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    internal sealed class CodeElementTagger : Tagger<CodeElementTag>
    {
        #region Fields

        private readonly ITextBuffer textBuffer;
        private readonly SourceTextContainer sourceTextContainer;
        private readonly ICodeElementCache cache;
        private readonly WorkspaceRegistration workspaceRegistration;

        private VisualStudioWorkspace? workspace;
        private string? filePath;

        /// <summary>
        /// The current <see cref="DocumentId" />. Could be null if we don't have a document.
        /// </summary>
        private DocumentId? documentId;

        /// <summary>
        /// The current project GUID. Could be Guid.Empty if we don't have a project.
        /// </summary>
        private Guid projectGuid;

        #endregion

        #region Constructors

        public CodeElementTagger(ITextView textView,
                                 ICodeElementCache cache)
            : base(textView)
        {
            ArgumentValidation.NotNull(textView, "textView");
            ArgumentValidation.NotNull(cache, "cache");

            this.textBuffer = textView.TextBuffer;
            this.sourceTextContainer = textView.TextBuffer.AsTextContainer();
            this.cache = cache;

            this.workspaceRegistration = Workspace.GetWorkspaceRegistration(this.sourceTextContainer);
            this.workspaceRegistration.WorkspaceChanged += WorkspaceRegistration_WorkspaceChanged;

            ConnectToWorkspace();
            SetFilePathAndDocumentId();
        }

        private void ConnectToWorkspace()
        {
            // The code that previously was digging through Roslyn's internal project model implicitly restricted code lens to only
            // work on regular projects and not files in Miscellaneous Files projects nor any custom workspaces. This maintains that.
            this.workspace = this.workspaceRegistration.Workspace as VisualStudioWorkspace;

            if (this.workspace != null)
            {
                this.workspace.DocumentActiveContextChanged += Workspace_DocumentActiveContextChanged;
            }
        }

        private void DisconnectFromWorkspace()
        {
            if (this.workspace != null)
            {
                this.workspace.DocumentActiveContextChanged -= Workspace_DocumentActiveContextChanged;
            }

            this.workspace = null;
        }

        private void WorkspaceRegistration_WorkspaceChanged(object sender, EventArgs e)
        {
            DisconnectFromWorkspace();
            ConnectToWorkspace();
            SetFilePathAndDocumentId();
        }

        private void Workspace_DocumentActiveContextChanged(object sender, DocumentActiveContextChangedEventArgs e)
        {
            if (e.SourceTextContainer == this.sourceTextContainer)
            {
                SetFilePathAndDocumentId();
            }
        }

        private void SetFilePathAndDocumentId()
        {
            var newDocument = this.textBuffer.CurrentSnapshot.GetOpenDocumentInCurrentContextWithChanges();

            // We filter to certain workspace types in ConnectToWorkspace; if this is coming from a different
            // workspace we should filter it out.
            if (newDocument?.Project.Solution.Workspace != this.workspace)
            {
                newDocument = null;
            }

            string? newFilePath = newDocument?.FilePath;
            Guid newProjectGuid = Guid.Empty;

            if (newDocument != null)
            {
                newProjectGuid = this.workspace!.GetHierarchy(newDocument.Id.ProjectId)?.GetProjectGuid() ?? Guid.Empty;
            }

            if (this.documentId != newDocument?.Id ||
                this.filePath != newFilePath ||
                this.projectGuid != newProjectGuid)
            {
                this.documentId = newDocument?.Id;
                this.filePath = newFilePath;
                this.projectGuid = newProjectGuid;

                this.UpdateSnapshotAsync(clean: true);
            }
        }

        #endregion

        #region Protected Methods
        protected override Tuple<CodeElementTag, int>? GetTag(int lineNumber)
        {
            if (this.workspace == null)
            {
                return null;
            }

            ICacheEntry cacheEntry;
            if (this.cache.TryGetAt(lineNumber, out cacheEntry))
            {
                return Tuple.Create(new CodeElementTag(cacheEntry.SyntaxNodeInfo, this.filePath, this.documentId, this.workspace, this.projectGuid), cacheEntry.LineOffset);
            }

            return null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                DisconnectFromWorkspace();
                this.workspaceRegistration.WorkspaceChanged -= WorkspaceRegistration_WorkspaceChanged;
            }

            base.Dispose(disposing);
        }

        protected override async Task<IEnumerable<int>> UpdateSnapshotAsync(ITextSnapshot snapshot, bool clean, CancellationToken cancellationToken)
        {
            await this.cache.RebuildAsync(snapshot, clean, cancellationToken);

            return this.cache.LineNumbers;
        }

        protected override void CleanupTag(CodeElementTag tag)
        {
            if (tag != null)
            {
                tag.Disconnect();
            }
        }
        #endregion
    }
}
