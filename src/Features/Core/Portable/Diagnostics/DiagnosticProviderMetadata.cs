﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.Collections.Generic;
using Microsoft.CodeAnalysis.Host.Mef;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Diagnostics;

internal sealed class DiagnosticProviderMetadata(IDictionary<string, object> data) : ILanguageMetadata
{
    public string Name { get; } = (string)data.GetValueOrDefault("Name");
    public string Language { get; } = (string)data.GetValueOrDefault("Language");
}
