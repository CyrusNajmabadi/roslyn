﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Composition;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Host.Mef;

namespace Microsoft.CodeAnalysis.CSharp.CodeRefactorings;

[ExportLanguageService(typeof(IRefactoringHelpersService), LanguageNames.CSharp), Shared]
[method: ImportingConstructor]
[method: Obsolete(MefConstruction.ImportingConstructorMessage, error: true)]
internal sealed class CSharpRefactoringHelpersService() : AbstractRefactoringHelpersService<ExpressionSyntax, ArgumentSyntax, ExpressionStatementSyntax>
{
    protected override IRefactoringHelpers RefactoringHelpers { get; } = CSharpRefactoringHelpers.Instance;
}
