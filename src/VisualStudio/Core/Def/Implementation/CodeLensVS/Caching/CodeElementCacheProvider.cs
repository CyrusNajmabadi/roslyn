// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Parser;
using Microsoft.VisualStudio.Utilities;
using Roslyn.Utilities;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Caching
{
    [Export(typeof(ICodeElementCacheProvider))]
    internal class CodeElementCacheProvider : ICodeElementCacheProvider
    {
        #region Fields
        private readonly IEnumerable<Lazy<IParsingService, IContentTypeMetadata>> parsingServices;
        private readonly IDynamicSyntaxTreeProvider dynamicSyntaxTreeProvider;
        #endregion

        #region Constructors
        [ImportingConstructor]
        [Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
        public CodeElementCacheProvider(
            [ImportMany] IEnumerable<Lazy<IParsingService, IContentTypeMetadata>> parsingServices,
            IDynamicSyntaxTreeProvider dynamicSyntaxTreeProvider)
        {
            Contract.ThrowIfNull(parsingServices);

            this.parsingServices = parsingServices.ToArray();
            this.dynamicSyntaxTreeProvider = dynamicSyntaxTreeProvider;
        }
        #endregion

        #region Public Methods
        public ICodeElementCache CreateCache()
        {
            return new CodeElementCache(this.parsingServices, this.dynamicSyntaxTreeProvider);
        }
        #endregion
    }
}
