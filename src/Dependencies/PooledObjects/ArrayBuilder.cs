// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

#if COMPILERCORE
using Roslyn.Utilities;
#endif

namespace Microsoft.CodeAnalysis.PooledObjects
{
    /// <summary>
    /// A writable array accessor that can be converted into an <see cref="ImmutableArray{T}"/>
    /// instance without allocating memory.
    /// </summary>
    [DebuggerDisplay("Count = {Count,nq}")]
    [DebuggerTypeProxy(typeof(ArrayBuilder<>.DebuggerProxy))]
    internal sealed partial class ArrayBuilder<T> : IReadOnlyCollection<T>, IReadOnlyList<T>, ICollection<T>
    {
        /// <summary>
        /// See <see cref="Free()"/> for an explanation of this constant value.
        /// </summary>
        public const int PooledArrayLengthLimitExclusive = 128;

        #region DebuggerProxy

        private sealed class DebuggerProxy
        {
            private readonly ArrayBuilder<T> _builder;

            public DebuggerProxy(ArrayBuilder<T> builder)
            {
                _builder = builder;
            }

            [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
            public T[] A
            {
                get
                {
                    var result = new T[_builder.Count];
                    for (var i = 0; i < result.Length; i++)
                    {
                        result[i] = _builder[i];
                    }

                    return result;
                }
            }
        }

        #endregion

        /// <summary>
        /// The backing array for the builder.
        /// </summary>
        private T[] _elements;

        /// <summary>
        /// The number of initialized elements in the array.
        /// </summary>
        private int _count;

        private readonly ObjectPool<ArrayBuilder<T>>? _pool;

        public ArrayBuilder(int size)
        {
            _elements = new T[size];
        }

        public ArrayBuilder()
            : this(8)
        { }

        private ArrayBuilder(ObjectPool<ArrayBuilder<T>> pool)
            : this()
        {
            _pool = pool;
        }

        /// <summary>
        /// Realizes the array.
        /// </summary>
        public ImmutableArray<T> ToImmutable()
        {
            return ImmutableCollectionsMarshal.AsImmutableArray(this.ToArray());
        }

        /// <summary>
        /// Realizes the array and clears the collection.
        /// </summary>
        public ImmutableArray<T> ToImmutableAndClear()
        {
            ImmutableArray<T> result;
            if (Count == 0)
            {
                result = ImmutableArray<T>.Empty;
            }
            else if (_elements.Length == Count)
            {
                result = ImmutableCollectionsMarshal.AsImmutableArray(_elements);
                _elements = Array.Empty<T>();
                _count = 0;
            }
            else
            {
                result = ToImmutable();
                Clear();
            }

            return result;
        }

        /// <summary>
        /// Gets or sets the length of the builder.
        /// </summary>
        /// <remarks>
        /// If the value is decreased, the array contents are truncated. If the value is increased, the added elements
        /// are initialized to the default value of type <typeparamref name="T"/>.
        /// </remarks>
        public int Count
        {
            get
            {
                return _count;
            }
            set
            {
                Debug.Assert(value >= 0, nameof(value));
                if (value < _count)
                {
                    // truncation mode
                    // Clear the elements of the elements that are effectively removed.

                    // PERF: Array.Clear works well for big arrays,
                    //       but may have too much overhead with small ones (which is the common case here)
                    if (_count - value > 64)
                    {
                        Array.Clear(_elements, value, _count - value);
                    }
                    else
                    {
                        for (int i = value; i < this.Count; i++)
                        {
                            _elements[i] = default(T)!;
                        }
                    }
                }
                else if (value > _count)
                {
                    // expansion
                    this.EnsureCapacity(value);
                }

                _count = value;
            }
        }

        /// <summary>
        /// Get and sets the length of the internal array.  When set the internal array is
        /// reallocated to the given capacity if it is not already the specified length.
        /// </summary>
        public int Capacity
        {
            get
            {
                return _elements.Length;
            }

            set
            {
                if (value < _count)
                {
                    throw new ArgumentException("Capacity must be greater or equal to Count", paramName: nameof(value));
                }

                if (value != _elements.Length)
                {
                    if (value > 0)
                    {
                        var temp = new T[value];
                        if (_count > 0)
                        {
                            Array.Copy(_elements, temp, _count);
                        }

                        _elements = temp;
                    }
                    else
                    {
                        _elements = Array.Empty<T>();
                    }
                }
            }
        }

        private static void ThrowIndexOutOfRangeException() => throw new IndexOutOfRangeException();

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <param name="index">The index.</param>
        /// <returns></returns>
        /// <exception cref="IndexOutOfRangeException">
        /// </exception>
        public T this[int index]
        {
            get
            {
                if (index >= this.Count)
                {
                    ThrowIndexOutOfRangeException();
                }

                return _elements[index];
            }

            set
            {
                if (index >= this.Count)
                {
                    ThrowIndexOutOfRangeException();
                }

                _elements[index] = value;
            }
        }

        public bool IsReadOnly
            => false;

        public bool IsEmpty
            => Count == 0;

        /// <summary>
        /// Write <paramref name="value"/> to slot <paramref name="index"/>. 
        /// Fills in unallocated slots preceding the <paramref name="index"/>, if any.
        /// </summary>
        public void SetItem(int index, T value)
        {
            while (index > _count)
            {
                this.Add(default!);
            }

            if (index == this.Count)
            {
                this.Add(value);
            }
            else
            {
                _elements[index] = value;
            }
        }

        /// <summary>
        /// Adds an item to the <see cref="ICollection{T}"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="ICollection{T}"/>.</param>
        public void Add(T item)
        {
            int newCount = _count + 1;
            this.EnsureCapacity(newCount);
            _elements[_count] = item;
            _count = newCount;
        }

        public void Insert(int index, T item)
        {
            Debug.Assert(index >= 0 && index <= this.Count, nameof(index));
            this.EnsureCapacity(this.Count + 1);

            if (index < this.Count)
            {
                Array.Copy(_elements, index, _elements, index + 1, this.Count - index);
            }

            _count++;
            _elements[index] = item;
        }

        /// <summary>
        /// Resizes the array to accommodate the specified capacity requirement.
        /// </summary>
        /// <param name="capacity">The required capacity.</param>
        public void EnsureCapacity(int capacity)
        {
            if (_elements.Length < capacity)
            {
                int newCapacity = Math.Max(_elements.Length * 2, capacity);
                Array.Resize(ref _elements, newCapacity);
            }
        }

        /// <summary>
        /// Removes all items from the <see cref="ICollection{T}"/>.
        /// </summary>
        public void Clear()
        {
            this.Count = 0;
        }

        /// <summary>
        /// Determines whether the <see cref="ICollection{T}"/> contains a specific value.
        /// </summary>
        /// <param name="item">The object to locate in the <see cref="ICollection{T}"/>.</param>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="ICollection{T}"/>; otherwise, false.
        /// </returns>
        public bool Contains(T item)
        {
            return this.IndexOf(item) >= 0;
        }

        /// <summary>
        /// Determines the index of a specific item in the <see cref="IList{T}"/>.
        /// </summary>
        /// <param name="item">The object to locate in the <see cref="IList{T}"/>.</param>
        /// <returns>
        /// The index of <paramref name="item"/> if found in the list; otherwise, -1.
        /// </returns>
        public int IndexOf(T item)
        {
            return this.IndexOf(item, 0, _count, EqualityComparer<T>.Default);
        }

        public int IndexOf(T item, IEqualityComparer<T> equalityComparer)
        {
            return this.IndexOf(item, 0, _count, equalityComparer);
        }

        public int IndexOf(T item, int startIndex, int count)
        {
            return this.IndexOf(item, startIndex, count, EqualityComparer<T>.Default);
        }

        /// <summary>
        /// Searches the array for the specified item.
        /// </summary>
        /// <param name="item">The item to search for.</param>
        /// <param name="startIndex">The index at which to begin the search.</param>
        /// <param name="count">The number of elements to search.</param>
        /// <param name="equalityComparer">
        /// The equality comparer to use in the search.
        /// If <c>null</c>, <see cref="EqualityComparer{T}.Default"/> is used.
        /// </param>
        /// <returns>The 0-based index into the array where the item was found; or -1 if it could not be found.</returns>
        public int IndexOf(T item, int startIndex, int count, IEqualityComparer<T>? equalityComparer)
        {
            if (count == 0 && startIndex == 0)
            {
                return -1;
            }

            Debug.Assert(startIndex >= 0 && startIndex < this.Count, nameof(startIndex));
            Debug.Assert(count >= 0 && startIndex + count <= this.Count, nameof(count));

            equalityComparer ??= EqualityComparer<T>.Default;
            if (equalityComparer == EqualityComparer<T>.Default)
            {
                return Array.IndexOf(_elements, item, startIndex, count);
            }
            else
            {
                for (int i = startIndex; i < startIndex + count; i++)
                {
                    if (equalityComparer.Equals(_elements[i], item))
                    {
                        return i;
                    }
                }

                return -1;
            }
        }

        public int FindIndex(Predicate<T> match)
            => FindIndex(0, this.Count, match);

        public int FindIndex(int startIndex, Predicate<T> match)
            => FindIndex(startIndex, this.Count - startIndex, match);

        public int FindIndex(int startIndex, int count, Predicate<T> match)
        {
            var endIndex = startIndex + count;
            for (var i = startIndex; i < endIndex; i++)
            {
                if (match(_elements[i]))
                {
                    return i;
                }
            }

            return -1;
        }

        public int FindIndex<TArg>(Func<T, TArg, bool> match, TArg arg)
            => FindIndex(0, Count, match, arg);

        public int FindIndex<TArg>(int startIndex, Func<T, TArg, bool> match, TArg arg)
            => FindIndex(startIndex, Count - startIndex, match, arg);

        public int FindIndex<TArg>(int startIndex, int count, Func<T, TArg, bool> match, TArg arg)
        {
            var endIndex = startIndex + count;
            for (var i = startIndex; i < endIndex; i++)
            {
                if (match(_elements[i], arg))
                {
                    return i;
                }
            }

            return -1;
        }

        /// <summary>
        /// Removes the first occurrence of the specified element from the builder.
        /// If no match is found, the builder remains unchanged.
        /// </summary>
        /// <param name="element">The element.</param>
        /// <returns>A value indicating whether the specified element was found and removed from the collection.</returns>
        public bool Remove(T element)
        {
            int index = this.IndexOf(element);
            if (index >= 0)
            {
                this.RemoveAt(index);
                return true;
            }

            return false;
        }

        /// <summary>
        /// Removes the <see cref="IList{T}"/> item at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the item to remove.</param>
        public void RemoveAt(int index)
        {
            Debug.Assert(index >= 0 && index < this.Count, nameof(index));

            if (index < this.Count - 1)
            {
                Array.Copy(_elements, index + 1, _elements, index, this.Count - index - 1);
            }

            this.Count--;
        }

        /// <summary>
        /// Removes the specified values from this list.
        /// </summary>
        /// <param name="index">The 0-based index into the array for the element to omit from the returned array.</param>
        /// <param name="length">The number of elements to remove.</param>
        public void RemoveRange(int index, int length)
        {
            Debug.Assert(index >= 0 && index <= _count, nameof(index));
            Debug.Assert(length >= 0 && index <= _count - length, nameof(length));

            if (length == 0)
            {
                return;
            }

            if (index + length < this._count)
            {

#if NET
                if (RuntimeHelpers.IsReferenceOrContainsReferences<T>())
                {
                    Array.Clear(_elements, index, length); // Clear the elements so that the gc can reclaim the references.
                }
#endif
                Array.Copy(_elements, index + length, _elements, index, this.Count - index - length);
            }

            this._count -= length;
        }

        public void RemoveLast()
        {
            this.RemoveAt(this.Count - 1);
        }

        /// <summary>
        /// Removes all the elements that match the conditions defined by the specified
        /// predicate.
        /// </summary>
        /// <param name="match">
        /// The <see cref="Predicate{T}"/> delegate that defines the conditions of the elements
        /// to remove.
        /// </param>
        public void RemoveAll(Predicate<T> match)
        {
            List<int>? removeIndices = null;
            for (int i = 0; i < _count; i++)
            {
                if (match(_elements[i]))
                {
                    removeIndices ??= new List<int>();
                    removeIndices.Add(i);
                }
            }

            if (removeIndices != null)
            {
                RemoveAtRange(removeIndices);
            }
        }

        private void RemoveAtRange(ICollection<int> indicesToRemove)
        {
            Debug.Assert(indicesToRemove != null, nameof(indicesToRemove));

            if (indicesToRemove!.Count == 0)
            {
                return;
            }

            int copied = 0;
            int removed = 0;
            int lastIndexRemoved = -1;
            foreach (int indexToRemove in indicesToRemove)
            {
                Debug.Assert(lastIndexRemoved < indexToRemove);
                int copyLength = lastIndexRemoved == -1 ? indexToRemove : (indexToRemove - lastIndexRemoved - 1);
                Array.Copy(_elements, copied + removed, _elements, copied, copyLength);
                removed++;
                copied += copyLength;
                lastIndexRemoved = indexToRemove;
            }

            Array.Copy(_elements, copied + removed, _elements, copied, _elements.Length - (copied + removed));

            _count -= indicesToRemove.Count;
        }

        public void RemoveAll<TArg>(Func<T, TArg, bool> match, TArg arg)
        {
            var i = 0;
            for (var j = 0; j < this.Count; j++)
            {
                if (!match(_elements[j], arg))
                {
                    if (i != j)
                    {
                        _elements[i] = _elements[j];
                    }

                    i++;
                }
            }

            Clip(i);
        }

        /// <summary>
        /// Reverses the order of elements in the collection.
        /// </summary>
        public void ReverseContents()
        {
#if NET || NETSTANDARD2_1_OR_GREATER
            Array.Reverse<T>(_elements, 0, _count);
#else
            // The non-generic Array.Reverse is not used because it does not perform
            // well for non-primitive value types.
            int i = 0;
            int j = _count - 1;
            T[] array = _elements;
            while (i < j)
            {
                T temp = array[i];
                array[i] = array[j];
                array[j] = temp;
                i++;
                j--;
            }
#endif
        }

        /// <summary>
        /// Sorts the array.
        /// </summary>
        public void Sort()
        {
            if (Count > 1)
            {
                Array.Sort(_elements, 0, this.Count, Comparer<T>.Default);
            }
        }

        /// <summary>
        /// Sorts the array.
        /// </summary>
        /// <param name="comparer">The comparer to use in sorting. If <c>null</c>, the default comparer is used.</param>
        public void Sort(IComparer<T>? comparer)
        {
            if (Count > 1)
            {
                Array.Sort(_elements, 0, _count, comparer);
            }
        }

        /// <summary>
        /// Sorts the array.
        /// </summary>
        /// <param name="index">The index of the first element to consider in the sort.</param>
        /// <param name="count">The number of elements to include in the sort.</param>
        /// <param name="comparer">The comparer to use in sorting. If <c>null</c>, the default comparer is used.</param>
        public void Sort(int index, int count, IComparer<T>? comparer)
        {
            // Don't rely on Array.Sort's argument validation since our internal array may exceed
            // the bounds of the publicly addressable region.
            Debug.Assert(index >= 0, nameof(index));
            Debug.Assert(count >= 0 && index + count <= this.Count, nameof(count));

            if (count > 1)
            {
                Array.Sort(_elements, index, count, comparer);
            }
        }

        /// <summary>
        /// Sorts the elements in the entire array using
        /// the specified <see cref="Comparison{T}"/>.
        /// </summary>
        /// <param name="comparison">
        /// The <see cref="Comparison{T}"/> to use when comparing elements.
        /// </param>
        /// <exception cref="ArgumentNullException"><paramref name="comparison"/> is null.</exception>
        public void Sort(Comparison<T> comparison)
        {
            Debug.Assert(comparison != null, nameof(comparison));

            if (Count > 1)
            {
#if NET
                // MemoryExtensions.Sort is not available in .NET Framework / Standard 2.0.
                // But the overload with a Comparison argument doesn't allocate.
                _elements.AsSpan(0, _count).Sort(comparison);
#else
                // Array.Sort does not have an overload that takes both bounds and a Comparison.
                // We could special case _count == _elements.Length in order to try to avoid
                // the IComparer allocation, but the Array.Sort overload that takes a Comparison
                // allocates such an IComparer internally, anyway.
                Array.Sort(_elements, 0, _count, Comparer<T>.Create(comparison));
#endif
            }
        }

        public void Sort(int startIndex, IComparer<T> comparer)
        {
            this.Sort(startIndex, this.Count - startIndex, comparer);
        }

        /// <summary>
        /// Creates a new array with the current contents of this Builder.
        /// </summary>
        public T[] ToArray()
        {
            if (this.Count == 0)
            {
                return Array.Empty<T>();
            }

            T[] result = new T[this.Count];
            Array.Copy(_elements, result, this.Count);
            return result;
        }

        /// <summary>
        /// Copies the current contents to the specified array.
        /// </summary>
        /// <param name="array">The array to copy to.</param>
        /// <param name="index">The starting index of the target array.</param>
        public void CopyTo(T[] array, int index)
        {
            Debug.Assert(array != null, nameof(array));
            Debug.Assert(index >= 0 && index + this.Count <= array!.Length, nameof(index));
            Array.Copy(_elements, 0, array, index, this.Count);
        }

        public T Last()
            => this[this.Count - 1];

        internal T? LastOrDefault()
            => Count == 0 ? default : Last();

        public T First()
        {
            return this[0];
        }

        public bool Any()
        {
            return this.Count > 0;
        }

        /// <summary>
        /// Realizes the array.
        /// </summary>
        public ImmutableArray<T> ToImmutableOrNull()
        {
            if (Count == 0)
            {
                return default;
            }

            return this.ToImmutable();
        }

        /// <summary>
        /// Realizes the array, downcasting each element to a derived type.
        /// </summary>
        public ImmutableArray<U> ToDowncastedImmutable<U>()
            where U : T
        {
            if (Count == 0)
            {
                return ImmutableArray<U>.Empty;
            }

            var tmp = ArrayBuilder<U>.GetInstance(Count);
            foreach (var i in this)
            {
                tmp.Add((U)i!);
            }

            return tmp.ToImmutableAndFree();
        }

        public ImmutableArray<U> ToDowncastedImmutableAndFree<U>() where U : T
        {
            var result = ToDowncastedImmutable<U>();
            this.Free();
            return result;
        }

        /// <summary>
        /// Realizes the array and disposes the builder in one operation.
        /// </summary>
        public ImmutableArray<T> ToImmutableAndFree()
        {
            // This is mostly the same as 'MoveToImmutable', but avoids delegating to that method since 'Free' contains
            // fast paths to avoid calling 'Clear' in some cases.
            ImmutableArray<T> result;
            if (Count == 0)
            {
                result = ImmutableArray<T>.Empty;
            }
            else if (this.Capacity == Count)
            {
                result = ImmutableCollectionsMarshal.AsImmutableArray(_elements);
                _elements = Array.Empty<T>();
                _count = 0;
            }
            else
            {
                result = ToImmutable();
            }

            this.Free();
            return result;
        }

        public T[] ToArrayAndFree()
        {
            var result = this.ToArray();
            this.Free();
            return result;
        }

        #region Poolable

        // To implement Poolable, you need two things:
        // 1) Expose Freeing primitive. 
        public void Free()
        {
            var pool = _pool;
            if (pool != null)
            {
                // According to the statistics of a C# compiler self-build, the most commonly used builder size is 0.  (808003 uses).
                // The distant second is the Count == 1 (455619), then 2 (106362) ...
                // After about 50 (just 67) we have a long tail of infrequently used builder sizes.
                // However we have builders with size up to 50K   (just one such thing)
                //
                // We do not want to retain (potentially indefinitely) very large builders 
                // while the chance that we will need their size is diminishingly small.
                // It makes sense to constrain the size to some "not too small" number. 
                // Overall perf does not seem to be very sensitive to this number, so I picked 128 as a limit.
                if (this.Capacity < PooledArrayLengthLimitExclusive)
                {
                    if (this.Count != 0)
                    {
                        this.Clear();
                    }

                    pool.Free(this);
                    return;
                }
                else
                {
                    pool.ForgetTrackedObject(this);
                }
            }
        }

        // 2) Expose the pool or the way to create a pool or the way to get an instance.
        //    for now we will expose both and figure which way works better
        private static readonly ObjectPool<ArrayBuilder<T>> s_poolInstance = CreatePool();
        public static ArrayBuilder<T> GetInstance()
        {
            var builder = s_poolInstance.Allocate();
            Debug.Assert(builder.Count == 0);
            return builder;
        }

        public static ArrayBuilder<T> GetInstance(int capacity)
        {
            var builder = GetInstance();
            builder.EnsureCapacity(capacity);
            return builder;
        }

        public static ArrayBuilder<T> GetInstance(int capacity, T fillWithValue)
        {
            var builder = GetInstance();
            builder.EnsureCapacity(capacity);

            for (var i = 0; i < capacity; i++)
            {
                builder.Add(fillWithValue);
            }

            return builder;
        }

        public static ObjectPool<ArrayBuilder<T>> CreatePool()
        {
            return CreatePool(128); // we rarely need more than 10
        }

        public static ObjectPool<ArrayBuilder<T>> CreatePool(int size)
        {
            ObjectPool<ArrayBuilder<T>>? pool = null;
            pool = new ObjectPool<ArrayBuilder<T>>(() => new ArrayBuilder<T>(pool!), size);
            return pool;
        }

        #endregion

        public Enumerator GetEnumerator()
        {
            return new Enumerator(this);
        }

        /// <summary>
        /// Returns an enumerator for the contents of the array.
        /// </summary>
        /// <returns>An enumerator.</returns>
        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            for (int i = 0; i < this.Count; i++)
            {
                yield return this[i];
            }
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<T>)this).GetEnumerator();
        }

        internal Dictionary<K, ImmutableArray<T>> ToDictionary<K>(Func<T, K> keySelector, IEqualityComparer<K>? comparer = null)
            where K : notnull
        {
            if (this.Count == 1)
            {
                var dictionary1 = new Dictionary<K, ImmutableArray<T>>(1, comparer);
                var value = this[0];
                dictionary1.Add(keySelector(value), ImmutableArray.Create(value));
                return dictionary1;
            }

            if (this.Count == 0)
            {
                return new Dictionary<K, ImmutableArray<T>>(comparer);
            }

            // bucketize
            // prevent reallocation. it may not have 'count' entries, but it won't have more. 
            var accumulator = new Dictionary<K, ArrayBuilder<T>>(Count, comparer);
            for (var i = 0; i < Count; i++)
            {
                var item = this[i];
                var key = keySelector(item);
                if (!accumulator.TryGetValue(key, out var bucket))
                {
                    bucket = ArrayBuilder<T>.GetInstance();
                    accumulator.Add(key, bucket);
                }

                bucket.Add(item);
            }

            var dictionary = new Dictionary<K, ImmutableArray<T>>(accumulator.Count, comparer);

            // freeze
            foreach (var pair in accumulator)
            {
                dictionary.Add(pair.Key, pair.Value.ToImmutableAndFree());
            }

            return dictionary;
        }

        public Span<T> AsSpan()
        {
            return new Span<T>(_elements, 0, _count);
        }

        public Span<T> AsSpan(int start, int length)
        {
            Debug.Assert(start + length <= _count);
            return new Span<T>(_elements, start, length);
        }

        public void AddRange(ArrayBuilder<T> items)
        {
            this.AddRange(items.AsSpan());
        }

        public void AddRange<U>(ArrayBuilder<U> items, Func<U, T> selector)
        {
            foreach (var item in items)
            {
                this.Add(selector(item));
            }
        }

        public void AddRange<U>(ArrayBuilder<U> items) where U : T
        {
            this.AddRange(items.AsSpan());
        }

        public void AddRange<U>(ArrayBuilder<U> items, int start, int length) where U : T
        {
            this.AddRange(items.AsSpan(start, length));
        }

        public void AddRange(ImmutableArray<T> items)
        {
            AddRange(items.AsSpan());
        }

        public void AddRange(ImmutableArray<T> items, int length)
        {
            AddRange(items.AsSpan(0, length));
        }

        public void AddRange(ImmutableArray<T> items, int start, int length)
        {
            AddRange(items.AsSpan(start, length));
        }

        public void AddRange<S>(ImmutableArray<S> items) where S : class, T
        {
            AddRange(items.AsSpan());
        }

        public void AddRange(T[] items, int start, int length)
        {
            AddRange(items.AsSpan(start, length));
        }

        public void AddRange(IEnumerable<T> items)
        {
            Debug.Assert(items != null, nameof(items));

#if NET50_OR_GREATER
            if (Enumerable.TryGetNonEnumeratedCount(items, out var count))
            {
                this.EnsureCapacity(this.Count + count);
            }
#endif

            foreach (T item in items!)
            {
                this.Add(item);
            }
        }

        public void AddRange(params T[] items)
        {
            AddRange(items.AsSpan());
        }

        public void AddRange(T[] items, int length)
        {
            AddRange(items.AsSpan(0, length));
        }

        /// <summary>
        /// Adds the specified items to the end of the array.
        /// </summary>
        /// <typeparam name="TDerived">The type that derives from the type of item already in the array.</typeparam>
        /// <param name="items">The items to add at the end of the array.</param>
        public void AddRange<TDerived>(params ReadOnlySpan<TDerived> items) where TDerived : T
        {
            int offset = this.Count;
            this.Count += items.Length;

            var elements = new Span<T>(_elements, offset, items.Length);
            for (int i = 0; i < items.Length; i++)
            {
                elements[i] = items[i];
            }
        }

#if COMPILERCORE
        public void AddRange(OneOrMany<T> items)
        {
            items.AddRangeTo(this);
        }
#endif

        public void Clip(int limit)
        {
            Debug.Assert(limit <= Count);
            this.Count = limit;
        }

        public void ZeroInit(int count)
        {
            this.Clear();
            this.Count = count;
        }

        public void AddMany(T item, int count)
        {
            EnsureCapacity(Count + count);

            for (var i = 0; i < count; i++)
            {
                Add(item);
            }
        }

        public void RemoveDuplicates()
        {
            var set = PooledHashSet<T>.GetInstance();

            var j = 0;
            for (var i = 0; i < Count; i++)
            {
                if (set.Add(this[i]))
                {
                    this[j] = this[i];
                    j++;
                }
            }

            Clip(j);
            set.Free();
        }

        public void SortAndRemoveDuplicates(IComparer<T> comparer)
        {
            if (Count <= 1)
            {
                return;
            }

            Sort(comparer);

            int j = 0;
            for (int i = 1; i < Count; i++)
            {
                if (comparer.Compare(this[j], this[i]) < 0)
                {
                    j++;
                    this[j] = this[i];
                }
            }

            Clip(j + 1);
        }

        public ImmutableArray<S> SelectDistinct<S>(Func<T, S> selector)
        {
            var result = ArrayBuilder<S>.GetInstance(Count);
            var set = PooledHashSet<S>.GetInstance();

            foreach (var item in this)
            {
                var selected = selector(item);
                if (set.Add(selected))
                {
                    result.Add(selected);
                }
            }

            set.Free();
            return result.ToImmutableAndFree();
        }
    }
}
