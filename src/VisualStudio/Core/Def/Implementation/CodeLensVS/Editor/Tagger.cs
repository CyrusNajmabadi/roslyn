// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Roslyn.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    internal abstract class Tagger<TTag>
        : ITagger<TTag>
        , IDisposable
        where TTag : Microsoft.VisualStudio.Language.CodeLens.ICodeLensTag
    {
        #region Fields

        // Using the same delay as the squiggle provider:
        protected const int DefaultUpdateDelayInMS = 1500;
        private readonly ITextView textView;
        private readonly Dictionary<int, Tuple<TTag, int>?> tagCache;
        private ITextSnapshot? previousSnapshot;

        /// <summary>
        /// Note - this is a task of int to help make sure the compiler catches errors. If this is a regular task,
        /// then the compiler is OK with:
        /// 
        /// this.previousUpdate = this.previousUpdate.ContinueWith(async t => { ... });
        /// 
        /// But in this case, the ContinueWith returns a Task of Task, which breaks the serialization of tasks. Unfortunately,
        /// it is really easy to miss this, so in order to help make sure we do the right thing, this is a task of int. This will
        /// ensure that we use the Unwrap() extension method to assign the right task to this field.
        /// </summary>
        private Task<int> previousUpdate;
        private CancellationTokenSource previousUpdateCancellationToken;
        private bool disposed;
        private bool isEnabled;
        private bool hasBeenAskedForTags = false;
        private readonly object updateTaskSyncRoot = new object();
        #endregion

        #region Constructors
        protected Tagger(ITextView textView)
        {
            Contract.ThrowIfNull(textView);

            this.textView = textView;
            this.tagCache = new Dictionary<int, Tuple<TTag, int>?>();
            this.previousUpdate = Task.FromResult(0);
            this.previousUpdateCancellationToken = new CancellationTokenSource();

            this.isEnabled = textView.Options.GetOptionValue(CodeLensOptions.IsCodeLensEnabledOptionKey);
            if (this.isEnabled)
            {
                this.AttachToTextView(update: false);
            }

            textView.Options.OptionChanged += this.OnEditorOptionsChanged;
        }
        #endregion

        #region Events
        public event EventHandler<SnapshotSpanEventArgs> TagsChanged = delegate { };
        #endregion

        #region Protected Fields
        protected Dictionary<int, Tuple<TTag, int>?> TagCache
        {
            get
            {
                return this.tagCache;
            }
        }
        #endregion

        #region Private Properties
        private bool IsEnabled
        {
            get
            {
                return this.isEnabled;
            }

            set
            {
                if (this.isEnabled != value)
                {
                    this.isEnabled = value;
                    this.OnIsEnabledChanged();
                }
            }
        }
        #endregion

        #region Public Methods
        public IEnumerable<ITagSpan<TTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            this.hasBeenAskedForTags = true;
            if (this.IsEnabled)
            {
                ////return this.GetTagsCore(spans).ToArray(); // Use this for profiling
                return this.GetTagsCore(spans);
            }
            else
            {
                return SpecializedCollections.EmptyEnumerable<ITagSpan<TTag>>();
            }
        }

        public void Dispose()
        {
            this.Dispose(true);
            GC.SuppressFinalize(this);
        }
        #endregion

        #region Internal Methods

        protected internal Task UpdateSnapshotAsync(bool clean)
        {
            return this.UpdateSnapshotAsync(clean, DefaultUpdateDelayInMS);
        }

        // This is protected for testabilty
        protected internal Task UpdateSnapshotAsync(bool clean, int delayInMS)
        {
            // This method is called from both UI and background threads so we need to synchronize 
            // update task scheduling.
            lock (updateTaskSyncRoot)
            {
                if (this.IsEnabled)
                {
                    // Get the current snapshot
                    var currentSnapshot = this.textView.TextBuffer.CurrentSnapshot;

                    this.previousUpdateCancellationToken.Cancel();
                    this.previousUpdateCancellationToken = new CancellationTokenSource();
                    var cancellationToken = this.previousUpdateCancellationToken.Token;

                    this.previousUpdate = this.previousUpdate.ContinueWith(async task =>
                    {
                        try
                        {
                            // If this is an incremental rebuild, delay the update for a period of time.
                            // This way, if the user is still typing we will wait until they are finished 
                            // typing before we try and update the cache.
                            if (!clean)
                            {
                                await Task.Delay(delayInMS, cancellationToken).ConfigureAwait(true);
                            }

                            cancellationToken.ThrowIfCancellationRequested();
                            var invalidatedLines = new HashSet<int>();

                            // Inform the tagger that it should update the snapshot it's working against
                            var newlyTaggedLines = await this.UpdateSnapshotAsync(currentSnapshot, clean, cancellationToken).ConfigureAwait(true);
                            invalidatedLines.UnionWith(newlyTaggedLines);

                            IReusableDescriptorComparisonCache? cache = null;

                            // See which lines in our cache need to be updated:                    
                            lock (this.tagCache)
                            {
                                // this.previousSnapshot is null on the very first update, which is equivalent
                                // to an empty cache
                                if (this.previousSnapshot != null)
                                {
                                    var keyPairs = this.tagCache.ToArray();
                                    this.tagCache.Clear();

                                    foreach (var keyPair in keyPairs)
                                    {
                                        var oldLineNumber = keyPair.Key;
                                        var existingTag = keyPair.Value;

                                        if (existingTag != null)
                                        {
                                            // Find the line number in the new snapshot:
                                            if (oldLineNumber >= this.previousSnapshot.LineCount)
                                            {
                                                // Something went very wrong and we cannot find the position of
                                                // existing tag on the previous snapshot.
                                                // Instead of crashing (see https://devdiv.visualstudio.com/DevDiv/_workitems/edit/1080056)
                                                // we are going to log a fault and skip this tag
                                                TelemetryService.DefaultSession.PostFault("vs/platform/codelens/tagger/updatesnapshoterror/incorrectlinenumber",
                                                    $"oldLineNumber={oldLineNumber},  this.previousSnapshot.LineCount={this.previousSnapshot.LineCount}");
                                                continue;
                                            }

                                            var previousSnapshotLine = this.previousSnapshot.GetLineFromLineNumber(oldLineNumber);
                                            var existingTagOffset = existingTag.Item2;
                                            var newSnapshotPoint = previousSnapshotLine.Start;

                                            if (existingTagOffset > previousSnapshotLine.Length)
                                            {
                                                // This is not expected, but won't crash immediately, log it
                                                TelemetryService.DefaultSession.PostFault("vs/platform/codelens/tagger/updatesnapshoterror/offsetoutoflinebounds",
                                                    $"previousSnapshotLine.Length={previousSnapshotLine.Length}, existingTagOffset={existingTagOffset}");
                                            }

                                            if (newSnapshotPoint.Position + existingTagOffset < newSnapshotPoint.Snapshot.Length)
                                            {
                                                newSnapshotPoint += existingTagOffset;
                                            }
                                            else
                                            {
                                                // Something went very wrong and we cannot find the position of
                                                // existing tag on the previous snapshot.
                                                // Instead of crashing (see https://devdiv.visualstudio.com/DevDiv/_workitems/edit/1080056)
                                                // we are going to log a fault and proceed even if CodeLens tag will be
                                                // incorrectly positioned.
                                                TelemetryService.DefaultSession.PostFault("vs/platform/codelens/tagger/updatesnapshoterror/offsetoutofsnapshotbounds",
                                                    $"newSnapshotPoint={newSnapshotPoint.Position}, newSnapshotLength={newSnapshotPoint.Snapshot.Length}, existingTagOffset={existingTagOffset}");
                                            }

                                            newSnapshotPoint = newSnapshotPoint.TranslateTo(currentSnapshot, PointTrackingMode.Positive);
                                            var newSnapshotLine = newSnapshotPoint.GetContainingLine();
                                            var newLineNumber = newSnapshotLine.LineNumber;

                                            // Do we still have a tag at this line? If so, compare it to the one we currently have 
                                            // and see if it's changed:
                                            var newTag = this.GetTag(newLineNumber);

                                            // Note: if this is a clean build, we can't re-use old tags. The reason why is because 
                                            // the tag is created out of a certain file path and in a rename scenario, all of the indicators
                                            // would need to update their file path / document ids. They can do this but it can become tricky,
                                            // so the easiest solution is just to blow away the old tags and add new ones. Note that we still 
                                            // need to invalidate the old line because the old tags might need to be invalidated as well.
                                            if (newTag != null && !clean)
                                            {
                                                var updateableDescriptor1 = existingTag.Item1.Descriptor as IReusableDescriptor;
                                                var updateableDescriptor2 = newTag.Item1.Descriptor as IReusableDescriptor;

                                                if (updateableDescriptor1 != null && updateableDescriptor2 != null && updateableDescriptor1.IsEquivalentTo(updateableDescriptor2))
                                                {
                                                    // If the cache hasn't been created yet, make it now:
                                                    if (cache == null)
                                                    {
                                                        cache = updateableDescriptor1.CreateCache();
                                                    }

                                                    // Re-use existing tags. Important! We need to make sure that we translate 
                                                    // the offset while we are rebuilding the cache if we are reusing the same tag.
                                                    var newOffset = newSnapshotPoint - newSnapshotLine.Start;
                                                    this.tagCache[newLineNumber] = new Tuple<TTag, int>(existingTag.Item1, newOffset);
                                                    updateableDescriptor1.CompareToAndUpdate(updateableDescriptor2, cache);
                                                }
                                                else
                                                {
                                                    // The tag is not updatable, so this means we have no 
                                                    // choice but to invalidate it.
                                                    invalidatedLines.Add(newLineNumber);
                                                    this.CleanupTag(existingTag.Item1);
                                                }
                                            }
                                            else
                                            {
                                                // We don't have a tag on this line any more. Let's make sure it gets invalidated so that it's removed
                                                // from the editor:
                                                invalidatedLines.Add(newLineNumber);
                                                this.CleanupTag(existingTag.Item1);
                                            }
                                        }
                                    }
                                }

                                // Now that our cache is rebuilt, let's swap the old snapshot with the new one:
                                this.previousSnapshot = currentSnapshot;
                            }

                            if (this.hasBeenAskedForTags)
                            {
                                // Raise tags changed for each invalidated lines. Line numbers are 0-indexed.
                                foreach (var lineNumber in invalidatedLines)
                                {
                                    var line = currentSnapshot.GetLineFromLineNumber(lineNumber);
                                    this.TagsChanged(this, new SnapshotSpanEventArgs(line.Extent));
                                }
                            }
                        }
                        catch (OperationCanceledException)
                        {
                            // Ignore this, it's expected. But catch it so we do't get unhandled exceptions in the debugger.
                        }

                        return 0;
                    },
                    cancellationToken,
                    TaskContinuationOptions.LazyCancellation,
                    TaskScheduler.Default).Unwrap();
                }

                return this.previousUpdate;
            }
        }
        #endregion

        #region Protected Methods
        protected virtual void Dispose(bool disposing)
        {
            if (!this.disposed)
            {
                if (disposing)
                {
                    this.DetachFromTextView();
                    this.textView.Options.OptionChanged -= this.OnEditorOptionsChanged;
                    this.previousUpdateCancellationToken.Cancel();
                }

                this.disposed = true;
            }
        }

        protected virtual void OnIsEnabledChanged()
        {
            if (this.isEnabled)
            {
                this.AttachToTextView(update: true);
            }
            else
            {
                this.DetachFromTextView();
            }
        }

        /// <summary>
        /// Gets an ITagSpan for the specified line number.
        /// </summary>
        /// <param name="lineNumber">The line number of the ITextSnapshot that was most recently passed to UpdateSnapshotAsync.</param>
        /// <returns>A tag and a line offset relative to the ITextSnapshot that was most recently passed to UpdateSnapshotAsync.</returns>
        protected abstract Tuple<TTag, int>? GetTag(int lineNumber);

        /// <summary>
        /// Updates to the specified ITextSnapshot. All calls to GetTag should be evaluated against this ITextSnapshot.
        /// </summary>
        /// <param name="snapshot">The ITextSnapshot that should be used for any calls to GetTag.</param>
        /// <param name="clean">If true, assume the ITextSnapshot has completely changed (not incrementally changed).</param>
        /// <param name="cancellationToken">A cancellation token that can stop the update before it finishes.</param>
        /// <returns>The line numbers that have Tags.</returns>
        protected abstract Task<IEnumerable<int>> UpdateSnapshotAsync(ITextSnapshot snapshot, bool clean, CancellationToken cancellationToken);

        /// <summary>
        /// Called when a tag is no longer used by the cache.  Such tags can have associated resources released.
        /// </summary>
        /// <param name="tag">The tag which is no longer a member of the cache.</param>
        protected virtual void CleanupTag(TTag tag)
        {
        }
        #endregion

        #region Private Methods
        private IEnumerable<ITagSpan<TTag>> GetTagsCore(NormalizedSnapshotSpanCollection spans)
        {
            // Each normalized span collection will only ever contain spans for a single displayed line, although the contents 
            // of each span might encompase multiple logical lines (due to regions and hidden text, for example).
            // Therefore, we only ever return a tag for the first span we find so that we can handle cases where 
            // word wrap or hidden text causes multiple tags to appear on the same line.
            // int startLine = 0;
            foreach (var span in spans)
            {
                // Get the line number (only one tag per line is supported)
                ITextSnapshotLine line;
                Tuple<TTag, int>? tagAndOffset;

                lock (this.tagCache)
                {
                    if (this.previousSnapshot == null)
                    {
                        yield break;
                    }

                    // Translate the span to the previous snapshot
                    var previousSpan = span.TranslateTo(this.previousSnapshot, SpanTrackingMode.EdgeInclusive);
                    line = previousSpan.Start.GetContainingLine();

                    if (!this.tagCache.TryGetValue(line.LineNumber, out tagAndOffset))
                    {
                        // Get the tag and its line offset (against the previous snapshot)
                        tagAndOffset = this.GetTag(line.LineNumber);

                        // Cache the tag info so we don't have to ask the subclass again
                        // NOTE: We intentionally store an entry even when the result of GetTag
                        // is null so that we don't have to ask the subclass again for line that
                        // *don't* have a tag. However, we may need to rethink this if the
                        // cache grows to big for large files.
                        this.tagCache.Add(line.LineNumber, tagAndOffset);
                    }
                }

                // The addition checks for the lengths of the line snapshot are to prevent an ArgumentOutOfRangeException
                // that happens when adding line.Start + TagAndOffset.Item2.  It is believed that this can happen if the
                // new snapshot contains a deletion that would put the tag after the end of the document when starting from the line
                // it's currently on and adding its original offset.
                if (tagAndOffset != null && line.Start.Position + tagAndOffset.Item2 < line.Snapshot.Length)
                {
                    // The tagger will translate this for us to the latest snapshot:
                    var tagPosition = line.Start + tagAndOffset.Item2;

                    // Return the current tag span
                    yield return new TagSpan<TTag>(new SnapshotSpan(tagPosition, 0), tagAndOffset.Item1);
                    break;
                }
            }
        }

        private void OnTextBufferChangedOnBackground(object sender, TextContentChangedEventArgs e)
        {
            // This event is raised on a background thread, but synchronized and in order edits were applied
            var delay = DefaultUpdateDelayInMS;

            // If we have a text changed event that spans one or more lines, then
            // we should update the snapshot immediately - no delay.
            if (e.Changes.Any(change => change.OldEnd > e.Before.GetLineFromPosition(change.OldPosition).End || change.NewEnd > e.After.GetLineFromPosition(change.NewPosition).End))
            {
                delay = 0;
            }

            // fire and forget
            _ = this.UpdateSnapshotAsync(false, delay);
        }

        private void OnContentTypeChanged(object sender, ContentTypeChangedEventArgs e)
        {
            // fire and forget
            _ = this.UpdateSnapshotAsync(true);
        }

        private void OnEditorOptionsChanged(object sender, EditorOptionChangedEventArgs e)
        {
            if (e.OptionId == CodeLensOptions.IsCodeLensEnabledOptionId)
            {
                this.IsEnabled = this.textView.Options.GetOptionValue(CodeLensOptions.IsCodeLensEnabledOptionKey);
            }
        }

        private void AttachToTextView(bool update)
        {
            this.SubscribeTextViewEvents();
            if (update)
            {
                // fire and forget
                _ = this.UpdateSnapshotAsync(clean: true);
            }
        }

        private void DetachFromTextView()
        {
            this.UnsubscribeTextViewEvents();

            // Cancel the previous update, then post a continuation that will clear
            // the tag cache data:
            this.previousUpdateCancellationToken.Cancel();
            this.previousUpdate = this.previousUpdate.ContinueWith(task =>
            {
                Tuple<TTag, int>?[] tagsToCleanup;
                lock (this.tagCache)
                {
                    tagsToCleanup = this.tagCache.Values.ToArray();
                    this.tagCache.Clear();
                }

                foreach (var tagPair in tagsToCleanup)
                {
                    if (tagPair != null)
                    {
                        this.CleanupTag(tagPair.Item1);
                    }
                }

                this.previousSnapshot = null;
                return 0;
            }, TaskScheduler.Current);
        }

        private void SubscribeTextViewEvents()
        {
            ((ITextBuffer2)this.textView.TextBuffer).ChangedOnBackground += this.OnTextBufferChangedOnBackground;
            this.textView.TextBuffer.ContentTypeChanged += this.OnContentTypeChanged;
        }

        private void UnsubscribeTextViewEvents()
        {
            ((ITextBuffer2)this.textView.TextBuffer).ChangedOnBackground -= this.OnTextBufferChangedOnBackground;
            this.textView.TextBuffer.ContentTypeChanged -= this.OnContentTypeChanged;
        }
        #endregion
    }
}
