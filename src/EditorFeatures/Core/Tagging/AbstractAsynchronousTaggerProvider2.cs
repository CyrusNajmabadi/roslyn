// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
#if DEBUG
using System.Diagnostics;
#endif
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.Editor.Shared.Extensions;
using Microsoft.CodeAnalysis.Editor.Shared.Options;
using Microsoft.CodeAnalysis.Editor.Shared.Tagging;
using Microsoft.CodeAnalysis.Editor.Shared.Utilities;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.CodeAnalysis.Text.Shared.Extensions;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Editor.Tagging
{
    /// <summary>
    /// Base type of all asynchronous tagger providers (<see cref="ITaggerProvider"/> and <see cref="IViewTaggerProvider"/>). 
    /// </summary>
    internal abstract partial class AbstractAsynchronousTaggerProvider2<TTag> : ForegroundThreadAffinitizedObject where TTag : ITag
    {
        private readonly object _uniqueKey = new();

        protected readonly IAsynchronousOperationListener AsyncListener;

        /// <summary>
        /// The behavior the tagger engine will have when text changes happen to the subject buffer
        /// it is attached to.  Most taggers can simply use <see cref="TaggerTextChangeBehavior.None"/>.
        /// However, advanced taggers that want to perform specialized behavior depending on what has
        /// actually changed in the file can specify <see cref="TaggerTextChangeBehavior.TrackTextChanges"/>.
        /// 
        /// If this is specified the tagger engine will track text changes and pass them along as
        /// <see cref="TaggerContext{TTag}.TextChangeRange"/> when calling 
        /// <see cref="ProduceTagsAsync(TaggerContext{TTag})"/>.
        /// </summary>
        protected virtual TaggerTextChangeBehavior TextChangeBehavior => TaggerTextChangeBehavior.None;

        /// <summary>
        /// The behavior the tagger will have when changes happen to the caret.
        /// </summary>
        protected virtual TaggerCaretChangeBehavior CaretChangeBehavior => TaggerCaretChangeBehavior.None;

        /// <summary>
        /// The behavior of tags that are created by the async tagger.  This will matter for tags
        /// created for a previous version of a document that are mapped forward by the async
        /// tagging architecture.  This value cannot be <see cref="SpanTrackingMode.Custom"/>.
        /// </summary>
        protected virtual SpanTrackingMode SpanTrackingMode => SpanTrackingMode.EdgeExclusive;

        /// <summary>
        /// Options controlling this tagger.  The tagger infrastructure will check this option
        /// against the buffer it is associated with to see if it should tag or not.
        /// 
        /// An empty enumerable, or null, can be returned to indicate that this tagger should 
        /// run unconditionally.
        /// </summary>
        protected virtual IEnumerable<Option2<bool>> Options => SpecializedCollections.EmptyEnumerable<Option2<bool>>();
        protected virtual IEnumerable<PerLanguageOption2<bool>> PerLanguageOptions => SpecializedCollections.EmptyEnumerable<PerLanguageOption2<bool>>();

        ///// <summary>
        ///// This controls what delay tagger will use to let editor know about newly inserted tags
        ///// </summary>
        //protected virtual TaggerDelay AddedTagNotificationDelay => TaggerDelay.NearImmediate;

#if DEBUG
        public readonly string StackTrace;
#endif

        protected AbstractAsynchronousTaggerProvider2(
            IThreadingContext threadingContext,
            IAsynchronousOperationListener asyncListener)
            : base(threadingContext)
        {
            AsyncListener = asyncListener;

#if DEBUG
            StackTrace = new StackTrace().ToString();
#endif
        }

        protected ITagger<T>? CreateTaggerWorker<T>(ITextView textViewOpt, ITextBuffer subjectBuffer) where T : ITag
        {
            this.AssertIsForeground();

            if (!subjectBuffer.GetFeatureOnOffOption(EditorComponentOnOffOptions.Tagger))
                return null;

            if (subjectBuffer is ITextBuffer2 subjectBuffer2)
                throw new ArgumentException($"{nameof(subjectBuffer)} was not an {nameof(ITextBuffer2}");

            var tagSource = GetOrCreateTagSource(textViewOpt, subjectBuffer2);
            var tagger = new Tagger(ThreadingContext, AsyncListener, tagSource, subjectBuffer2) as ITagger<T>;
            if (tagger == null)
            {
                // If we couldn't actually create the tagger then dispose the tag source so its
                // refcount is correct.
                tagSource.OnTaggerDisposed();
                return null;
            }

            return tagger;
        }

        private TagSource GetOrCreateTagSource(ITextView textViewOpt, ITextBuffer2 subjectBuffer)
        {
            this.AssertIsForeground();
            if (!this.TryRetrieveTagSource(textViewOpt, subjectBuffer, out var tagSource))
            {
                tagSource = new TagSource(this, textViewOpt, subjectBuffer);

                this.StoreTagSource(textViewOpt, subjectBuffer, tagSource);
                tagSource.Disposed += (s, e) => this.RemoveTagSource(textViewOpt, subjectBuffer);
            }

            return tagSource;
        }

        private bool TryRetrieveTagSource(ITextView textViewOpt, ITextBuffer subjectBuffer, [NotNullWhen(true)] out TagSource? tagSource)
        {
            this.AssertIsForeground();
            return textViewOpt != null
                ? textViewOpt.TryGetPerSubjectBufferProperty(subjectBuffer, _uniqueKey, out tagSource)
                : subjectBuffer.Properties.TryGetProperty(_uniqueKey, out tagSource);
        }

        private void RemoveTagSource(ITextView textViewOpt, ITextBuffer subjectBuffer)
        {
            this.AssertIsForeground();
            if (textViewOpt != null)
            {
                textViewOpt.RemovePerSubjectBufferProperty<TagSource, ITextView>(subjectBuffer, _uniqueKey);
            }
            else
            {
                subjectBuffer.Properties.RemoveProperty(_uniqueKey);
            }
        }

        private void StoreTagSource(ITextView textViewOpt, ITextBuffer subjectBuffer, TagSource tagSource)
        {
            this.AssertIsForeground();
            if (textViewOpt != null)
            {
                textViewOpt.AddPerSubjectBufferProperty(subjectBuffer, _uniqueKey, tagSource);
            }
            else
            {
                subjectBuffer.Properties.AddProperty(_uniqueKey, tagSource);
            }
        }

        /// <summary>
        /// Called by the <see cref="AbstractAsynchronousTaggerProvider{TTag}"/> infrastructure to 
        /// determine the caret position.  This value will be passed in as the value to 
        /// <see cref="TaggerContext{TTag}.CaretPosition"/> in the call to
        /// <see cref="ProduceTagsAsync(TaggerContext{TTag})"/>.
        /// </summary>
        protected virtual SnapshotPoint? GetCaretPoint(ITextView textViewOpt, ITextBuffer subjectBuffer)
            => textViewOpt?.GetCaretPoint(subjectBuffer);

        /// <summary>
        /// Called by the <see cref="AbstractAsynchronousTaggerProvider{TTag}"/> infrastructure to determine
        /// the set of spans that it should asynchronously tag.  This will be called in response to
        /// notifications from the <see cref="ITaggerEventSource"/> that something has changed, and
        /// will only be called from the UI thread.  The tagger infrastructure will then determine
        /// the <see cref="DocumentSnapshotSpan"/>s associated with these <see cref="SnapshotSpan"/>s
        /// and will asynchronously call into <see cref="ProduceTagsAsync(TaggerContext{TTag})"/> at some point in
        /// the future to produce tags for these spans.
        /// </summary>
        protected virtual IEnumerable<SnapshotSpan> GetSpansToTag(ITextView textViewOpt, ITextBuffer subjectBuffer)
        {
            // For a standard tagger, the spans to tag is the span of the entire snapshot.
            return SpecializedCollections.SingletonEnumerable(subjectBuffer.CurrentSnapshot.GetFullSpan());
        }

        /// <summary>
        /// Creates the <see cref="ITaggerEventSource"/> that notifies the <see cref="AbstractAsynchronousTaggerProvider{TTag}"/>
        /// that it should recompute tags for the text buffer after an appropriate <see cref="TaggerDelay"/>.
        /// </summary>
        protected abstract ITaggerEventSource CreateEventSource(ITextView textViewOpt, ITextBuffer subjectBuffer);

        /// <summary>
        /// Produce tags for the given context.
        /// Keep in sync with <see cref="ProduceTagsSynchronously(TaggerContext{TTag})"/>
        /// </summary>
        protected virtual async Task ProduceTagsAsync(TaggerContext<TTag> context)
        {
            foreach (var spanToTag in context.SpansToTag)
            {
                context.CancellationToken.ThrowIfCancellationRequested();
                await ProduceTagsAsync(
                    context, spanToTag,
                    GetCaretPosition(context.CaretPosition, spanToTag.SnapshotSpan)).ConfigureAwait(false);
            }
        }

        /// <summary>
        /// Produce tags for the given context.
        /// Keep in sync with <see cref="ProduceTagsAsync(TaggerContext{TTag})"/>
        /// </summary>
        protected void ProduceTagsSynchronously(TaggerContext<TTag> context)
        {
            foreach (var spanToTag in context.SpansToTag)
            {
                context.CancellationToken.ThrowIfCancellationRequested();
                ProduceTagsSynchronously(
                    context, spanToTag,
                    GetCaretPosition(context.CaretPosition, spanToTag.SnapshotSpan));
            }
        }

        private static int? GetCaretPosition(SnapshotPoint? caretPosition, SnapshotSpan snapshotSpan)
        {
            return caretPosition.HasValue && caretPosition.Value.Snapshot == snapshotSpan.Snapshot
                ? caretPosition.Value.Position : (int?)null;
        }

        protected virtual Task ProduceTagsAsync(TaggerContext<TTag> context, DocumentSnapshotSpan spanToTag, int? caretPosition)
            => Task.CompletedTask;

        protected virtual void ProduceTagsSynchronously(TaggerContext<TTag> context, DocumentSnapshotSpan spanToTag, int? caretPosition)
        {
            // By default we implement the sync version of this by blocking on the async version.
            //
            // The benefit of this is that all taggers can implicitly be used as IAccurateTaggers
            // without any code changes.
            // 
            // However, the drawback is that it means the UI thread might be blocked waiting for 
            // tasks to be scheduled and run on the threadpool. 
            //
            // Taggers that need to be called accurately should override this method to produce
            // results quickly if possible.
            ProduceTagsAsync(context, spanToTag, caretPosition).Wait(context.CancellationToken);
        }

        internal TestAccessor GetTestAccessor()
            => new(this);

        private struct DiffResult
        {
            public NormalizedSnapshotSpanCollection Added { get; }
            public NormalizedSnapshotSpanCollection Removed { get; }

            public DiffResult(List<SnapshotSpan> added, List<SnapshotSpan> removed)
                : this(added?.Count == 0 ? null : (IEnumerable<SnapshotSpan>?)added, removed?.Count == 0 ? null : (IEnumerable<SnapshotSpan>?)removed)
            {
            }

            public DiffResult(IEnumerable<SnapshotSpan>? added, IEnumerable<SnapshotSpan>? removed)
            {
                Added = added != null ? new NormalizedSnapshotSpanCollection(added) : NormalizedSnapshotSpanCollection.Empty;
                Removed = removed != null ? new NormalizedSnapshotSpanCollection(removed) : NormalizedSnapshotSpanCollection.Empty;
            }

            public int Count => Added.Count + Removed.Count;
        }

        internal readonly struct TestAccessor
        {
            private readonly AbstractAsynchronousTaggerProvider2<TTag> _provider;

            public TestAccessor(AbstractAsynchronousTaggerProvider2<TTag> provider)
                => _provider = provider;

            internal Task ProduceTagsAsync(TaggerContext<TTag> context)
                => _provider.ProduceTagsAsync(context);
        }

        private class TagSource
        {
            private readonly AbstractAsynchronousTaggerProvider2<TTag> _provider;
            private readonly ITextView _textViewOpt;
            private readonly ITextBuffer2 _subjectBuffer;

            private readonly ITaggerEventSource _eventSource;

            private readonly AsyncBatchingWorkQueue<ITextSnapshot> _changeQueue;
            private readonly AsyncBatchingWorkQueue<NormalizedSnapshotSpanCollection> _tagsRemovedNotificationQueue;
            private readonly AsyncBatchingWorkQueue<NormalizedSnapshotSpanCollection> _tagsAddedNotificationQueue;

            private readonly CancellationTokenSource _backgroundWorkCancellationSource = new();

            public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

            public TagSource(AbstractAsynchronousTaggerProvider2<TTag> provider, ITextView textViewOpt, ITextBuffer2 subjectBuffer)
            {
                provider.AssertIsForeground();
                _provider = provider;
                _textViewOpt = textViewOpt;
                _subjectBuffer = subjectBuffer;

                this.CachedTagTrees = ImmutableDictionary.Create<ITextBuffer, TagSpanIntervalTree<TTag>>();

                _eventSource = this.CreateEventSource();

                _changeQueue = new AsyncBatchingWorkQueue<ITextSnapshot>(
                    TaggerDelay.Short.ComputeTimeDelay(),
                    ProcessChangesAsync,
                    EqualityComparer<ITextSnapshot>.Default,
                    provider.AsyncListener,
                    _backgroundWorkCancellationSource.Token);

                // Notify the editor of removed tags immediately.  We always want those to dissapear
                // as soon as possible.
                _tagsRemovedNotificationQueue = new AsyncBatchingWorkQueue<NormalizedSnapshotSpanCollection>(
                    TaggerDelay.NearImmediate.ComputeTimeDelay(),
                    ProcessNotificationsAsync,
                    equalityComparer: null,
                    provider.AsyncListener,
                    _backgroundWorkCancellationSource.Token);

                // For added tags, update the editor at whatever pace this feature wants.
                _tagsAddedNotificationQueue = new AsyncBatchingWorkQueue<NormalizedSnapshotSpanCollection>(
                    provider.TagsTagsAddedNotificationDelay.ComputeTimeDelay(),
                    ProcessNotificationsAsync,
                    equalityComparer: null,
                    provider.AsyncListener,
                    _backgroundWorkCancellationSource.Token);

                Connect();

                // Start computing the initial set of tags immediately.  We want to get the UI
                // to a complete state as soon as possible.
                _changeQueue.AddWork(_subjectBuffer.CurrentSnapshot);
            }

            private ITaggerEventSource CreateEventSource()
            {
                var eventSource = _provider.CreateEventSource(_textViewOpt, _subjectBuffer);

                // If there are any options specified for this tagger, then also hook up event
                // notifications for when those options change.
                var optionChangedEventSources =
                    _provider.Options.Concat<IOption>(_provider.PerLanguageOptions)
                        .SelectAsArray(o => TaggerEventSources.OnOptionChanged(_subjectBuffer, o, TaggerDelay.NearImmediate));

                // If no options specified for this tagger, ust keep the event source as is.
                if (optionChangedEventSources.IsEmpty)
                    return eventSource;

                optionChangedEventSources.Add(eventSource);
                return TaggerEventSources.Compose(optionChangedEventSources);
            }

            private Task ProcessNotificationsAsync(
                ImmutableArray<NormalizedSnapshotSpanCollection> array, CancellationToken cancellationToken)
            {
                foreach (var collection in array)
                {
                    foreach (var span in collection)
                        this.TagsChanged?.Invoke(this, new SnapshotSpanEventArgs(span));
                }

                return Task.CompletedTask;
            }

            private void Connect()
            {
                _provider.AssertIsForeground();

                _eventSource.Changed += OnEventSourceChanged;
                _subjectBuffer.ChangedOnBackground += OnSubjectBufferChangedOnBackground;

                //if (_dataSource.CaretChangeBehavior.HasFlag(TaggerCaretChangeBehavior.RemoveAllTagsOnCaretMoveOutsideOfTag))
                //{
                //    if (_textViewOpt == null)
                //    {
                //        throw new ArgumentException(
                //            nameof(_dataSource.CaretChangeBehavior) + " can only be specified for an " + nameof(IViewTaggerProvider));
                //    }

                //    _textViewOpt.Caret.PositionChanged += OnCaretPositionChanged;
                //}

                // Tell the interaction object to start issuing events.
                _eventSource.Connect();
            }

            private void Disconnect()
            {
                _provider.AssertIsForeground();
                _backgroundWorkCancellationSource.Cancel();

                // Tell the interaction object to stop issuing events.
                _eventSource.Disconnect();

                //if (_dataSource.CaretChangeBehavior.HasFlag(TaggerCaretChangeBehavior.RemoveAllTagsOnCaretMoveOutsideOfTag))
                //{
                //    _textViewOpt.Caret.PositionChanged -= OnCaretPositionChanged;
                //}

                _subjectBuffer.ChangedOnBackground -= OnSubjectBufferChangedOnBackground;
                _eventSource.Changed -= OnEventSourceChanged;
            }

            private void OnEventSourceChanged(object? sender, TaggerEventArgs e)
                => _changeQueue.AddWork(_subjectBuffer.CurrentSnapshot);

            private void OnSubjectBufferChangedOnBackground(object? sender, TextContentChangedEventArgs e)
                => _changeQueue.AddWork(_subjectBuffer.CurrentSnapshot);
        }
    }
}
