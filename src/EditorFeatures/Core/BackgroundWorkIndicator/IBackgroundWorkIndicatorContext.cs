// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Threading;
using Microsoft.VisualStudio.Utilities;

namespace Microsoft.CodeAnalysis.Editor.BackgroundWorkIndicator;

/// <summary>
/// <see cref="IBackgroundWorkIndicator"/>
/// </summary>
internal interface IBackgroundWorkIndicatorContext : IDisposable //, IUIThreadOperationContext
{
    /// <summary>
    /// Cancellation token that allows user to cancel the operation unless the operation is not cancellable.
    /// </summary>
    CancellationToken UserCancellationToken { get; }

    /// <summary>
    /// Allows clients to temporarily suppress auto cancel behaviors when they want to apply edits or navigate without canceling.
    /// </summary>
    IDisposable SuppressAutoCancel();
}
