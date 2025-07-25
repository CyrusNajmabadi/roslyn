﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;

namespace Microsoft.CodeAnalysis.Editor.Shared.Utilities;

internal sealed class CaretPreservingEditTransaction : IDisposable
{
    private readonly IEditorOperations _editorOperations;
    private readonly ITextUndoHistory? _undoHistory;
    private ITextUndoTransaction? _transaction;
    private bool _active;

    public CaretPreservingEditTransaction(
        string description,
        ITextView textView,
        ITextUndoHistoryRegistry undoHistoryRegistry,
        IEditorOperationsFactoryService editorOperationsFactoryService)
        : this(description, undoHistoryRegistry.GetHistory(textView.TextBuffer), editorOperationsFactoryService.GetEditorOperations(textView))
    {
    }

    public CaretPreservingEditTransaction(string description, ITextUndoHistory? undoHistory, IEditorOperations editorOperations)
    {
        _editorOperations = editorOperations;
        _undoHistory = undoHistory;
        _active = true;

        if (_undoHistory != null)
        {
            _transaction = new HACK_TextUndoTransactionThatRollsBackProperly(_undoHistory.CreateTransaction(description));
            _editorOperations.AddBeforeTextBufferChangePrimitive();
        }
    }

    public static CaretPreservingEditTransaction? TryCreate(string description,
        ITextView textView,
        ITextUndoHistoryRegistry undoHistoryRegistry,
        IEditorOperationsFactoryService editorOperationsFactoryService)
    {
        if (undoHistoryRegistry.TryGetHistory(textView.TextBuffer, out _))
        {
            return new CaretPreservingEditTransaction(description, textView, undoHistoryRegistry, editorOperationsFactoryService);
        }

        return null;
    }

    public void Complete()
    {
        if (!_active)
        {
            throw new InvalidOperationException(EditorFeaturesResources.The_transaction_is_already_complete);
        }

        _editorOperations.AddAfterTextBufferChangePrimitive();
        _transaction?.Complete();

        EndTransaction();
    }

    public void Cancel()
    {
        if (!_active)
        {
            throw new InvalidOperationException(EditorFeaturesResources.The_transaction_is_already_complete);
        }

        _transaction?.Cancel();

        EndTransaction();
    }

    public void Dispose()
    {
        if (_transaction != null)
        {
            // If the transaction is still pending, we'll cancel it
            Cancel();
        }
    }

    public IMergeTextUndoTransactionPolicy? MergePolicy
    {
        get
        {
            return _transaction?.MergePolicy;
        }

        set
        {
            _transaction?.MergePolicy = value;
        }
    }

    private void EndTransaction()
    {
        if (_transaction != null)
        {
            _transaction.Dispose();
            _transaction = null;
        }

        _active = false;
    }
}
