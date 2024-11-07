// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.InheritanceMargin;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Remote;

internal sealed class RemoteInheritanceMarginService : BrokeredServiceBase, IRemoteInheritanceMarginService
{
    internal sealed class Factory : FactoryBase<IRemoteInheritanceMarginService>
    {
        protected override IRemoteInheritanceMarginService CreateService(in ServiceConstructionArguments arguments)
        {
            return new RemoteInheritanceMarginService(arguments);
        }
    }

    public RemoteInheritanceMarginService(in ServiceConstructionArguments arguments) : base(in arguments)
    {
    }

    public ValueTask<ImmutableArray<InheritanceMarginItem>> GetInheritanceMarginItemsAsync(
        Checksum solutionChecksum,
        DocumentId documentId,
        TextSpan spanToSearch,
        bool includeGlobalImports,
        bool frozenSemantics,
        CancellationToken cancellationToken)
    {
        return RunServiceAsync(solutionChecksum, async solution =>
        {
            // Explicitly disabling frozen on the OOP size.  This flag was passed in, but had no actual
            // effect (since OOP didn't support frozen semantics initially).  When OOP gained real support
            // for frozen, this started breaking inheritance margin.  So, until that is figured out, we just
            // disable this to keep the pre-existing behavior.
            //
            // Tracked by https://github.com/dotnet/roslyn/issues/67065.
            frozenSemantics = false;
            var document = await solution.GetRequiredDocumentAsync(documentId, includeSourceGenerated: true, cancellationToken).ConfigureAwait(false);
            var service = document.GetRequiredLanguageService<IInheritanceMarginService>();
            return await service.GetInheritanceMemberItemsAsync(document, spanToSearch, includeGlobalImports, frozenSemantics, cancellationToken).ConfigureAwait(false);
        }, cancellationToken);
    }
}
