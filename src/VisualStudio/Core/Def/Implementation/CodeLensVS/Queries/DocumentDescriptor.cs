// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Diagnostics;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Queries
{
    [DebuggerStepThrough]
    internal class DocumentDescriptor : IDocumentDescriptor
    {
        public DocumentDescriptor(string filePath, Guid projectGuid)
        {
            if (string.IsNullOrEmpty(filePath))
            {
                throw new ArgumentNullException("filePath");
            }

            this.FilePath = filePath;
            this.ProjectGuid = projectGuid;
        }

        public string FilePath { get; }

        public Guid ProjectGuid { get; }

        /// <summary>
        /// Description of the element, returns the file path.
        /// </summary>
        public virtual string ElementDescription => this.FilePath;

        public virtual Span? ApplicableSpan { get; }

        public virtual CodeElementKinds Kind { get; }
    }
}
