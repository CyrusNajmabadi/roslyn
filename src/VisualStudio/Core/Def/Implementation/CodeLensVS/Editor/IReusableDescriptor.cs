// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

namespace Microsoft.VisualStudio.LanguageServices.Implementation.CodeLensVS.Editor
{
    /// <summary>
    /// Defines a descriptor that can explicitly supports being re-used between tags and has the ability to update
    /// itself accordingly.
    /// </summary>
    internal interface IReusableDescriptor
    {
        /// <summary>
        /// Compares two descriptors and returns true if they are equivalent.
        /// </summary>
        /// <param name="other">The other descriptor</param>
        /// <returns>True if the two descriptors are equivelant</returns>
        bool IsEquivalentTo(IReusableDescriptor other);

        /// <summary>
        /// Creates a cache that can be used during the update process.
        /// </summary>
        /// <returns>An object that implements IReusableDescriptorComparisonCache</returns>
        IReusableDescriptorComparisonCache CreateCache();

        /// <summary>
        /// A method that compares one descriptor to another and updates itself if applicable.
        /// </summary>
        /// <param name="other">The other descriptor to compare to</param>
        /// <param name="cache">The comparison cache to use during the update</param>
        /// <returns>True if the descriptor updates</returns>
        bool CompareToAndUpdate(IReusableDescriptor other, IReusableDescriptorComparisonCache cache);
    }
}
