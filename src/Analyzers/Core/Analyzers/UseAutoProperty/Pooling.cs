// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Concurrent;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.UseAutoProperty;

internal abstract partial class AbstractUseAutoPropertyAnalyzer<
    TAnalyzer,
    TSyntaxKind,
    TPropertyDeclaration,
    TConstructorDeclaration,
    TFieldDeclaration,
    TVariableDeclarator,
    TExpression,
    TIdentifierName>
{
    private static class ConcurrentSetPool<T> where T : notnull
    {
        private static readonly ObjectPool<ConcurrentSet<T>> s_pool = new(() => []);

        public static ConcurrentSet<T> Allocate() => s_pool.Allocate();
        public static void Free(ConcurrentSet<T> set) => s_pool.ClearAndFree(set);
    }

    private static class ConcurrentDictionaryPool<TKey, TValue>
        where TKey : notnull
        where TValue : notnull
    {
        private static readonly ObjectPool<ConcurrentDictionary<TKey, TValue>> s_pool = new(() => []);
        private static readonly ObjectPool<ConcurrentDictionary<TKey, ConcurrentSet<TValue>>> s_multiPool = new(() => []);

        public static ConcurrentDictionary<TKey, TValue> Allocate() => s_pool.Allocate();
        public static void Free(ConcurrentDictionary<TKey, TValue> map) => s_pool.ClearAndFree(map);

        public static ConcurrentDictionary<TKey, ConcurrentSet<TValue>> AllocateMulti() => s_multiPool.Allocate();
        public static void Free(ConcurrentDictionary<TKey, ConcurrentSet<TValue>> map)
        {
            foreach (var (_, set) in map)
                ConcurrentSetPool<TValue>.Free(set);

            s_multiPool.ClearAndFree(map);
        }
    }
}
