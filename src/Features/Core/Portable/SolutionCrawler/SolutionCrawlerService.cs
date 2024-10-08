﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis.Options;

namespace Microsoft.CodeAnalysis.SolutionCrawler;

internal sealed partial class SolutionCrawlerRegistrationService
{
    internal static readonly Option2<bool> EnableSolutionCrawler = new("dotnet_enable_solution_crawler", defaultValue: true);
}
