﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.Features.RQName.SimpleTree;

namespace Microsoft.CodeAnalysis.Features.RQName.Nodes;

internal sealed class RQMemberParameterIndexFromPartialImplementation(
    RQMember containingMember,
    int parameterIndex) : RQMemberParameterIndex(containingMember, parameterIndex)
{
    protected override void AppendChildren(List<SimpleTreeNode> childList)
    {
        childList.Add(ContainingMember.ToSimpleTree());
        childList.Add(new SimpleLeafNode(ParameterIndex.ToString()));
        childList.Add(new SimpleLeafNode(RQNameStrings.PartialImplementation));
    }
}
