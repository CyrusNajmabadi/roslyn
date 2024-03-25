// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Composition;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Operations;

namespace Microsoft.CodeAnalysis.Editor.Implementation.Workspaces;

[ExportWorkspaceServiceFactory(typeof(ITextUndoHistoryWorkspaceService), ServiceLayer.Default), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal class TextUndoHistoryWorkspaceService(ITextUndoHistoryRegistry textUndoHistoryRegistry) : ITextUndoHistoryWorkspaceService
{
    public bool TryGetTextUndoHistory(Workspace editorWorkspace, ITextBuffer textBuffer, out ITextUndoHistory undoHistory)
        => textUndoHistoryRegistry.TryGetHistory(textBuffer, out undoHistory);
}
