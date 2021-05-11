// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Caching
{
    /// <summary>
    /// Defines an entry in the cache structure.
    /// </summary>
    internal class CacheEntry : ICacheEntry
    {
        private readonly SyntaxNodeInfo syntaxNodeInfo;
        private readonly int lineOffset;

        /// <summary>
        /// Creates a new cache entry with the specified kind.
        /// </summary>
        /// <param name="syntaxNodeInfo">The syntax node info.</param>
        /// <param name="lineOffset">The offset from the beginning of hte line where the code element is located.</param>
        public CacheEntry(SyntaxNodeInfo syntaxNodeInfo, int lineOffset)
        {
            this.syntaxNodeInfo = syntaxNodeInfo;
            this.lineOffset = lineOffset;
        }

        public SyntaxNodeInfo SyntaxNodeInfo
        {
            get
            {
                return this.syntaxNodeInfo;
            }
        }

        public int LineOffset
        {
            get
            {
                return this.lineOffset;
            }
        }
    }
}
