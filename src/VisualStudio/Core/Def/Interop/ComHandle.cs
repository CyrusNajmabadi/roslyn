﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.LanguageServices.Implementation.Interop;

/// <summary>
/// Holds onto a managed object as well as the CCW for that object if there is one.
/// </summary>
/// <typeparam name="THandle">The COM interface type to keep a reference to</typeparam>
/// <typeparam name="TObject">The managed object type to keep a reference to</typeparam>
internal readonly struct ComHandle<THandle, TObject>
    where THandle : class
    where TObject : class, THandle
{

    /// <summary>
    /// Create an instance from a "ComObject" or from a managed object.
    /// </summary>
    public ComHandle(THandle handleOrManagedObject)
    {
        if (handleOrManagedObject == null)
        {
            Handle = null;
            Object = null;
        }
        else if (Marshal.IsComObject(handleOrManagedObject))
        {
            Handle = handleOrManagedObject;
            Object = ComAggregate.GetManagedObject<TObject>(handleOrManagedObject);
        }
        else
        {
            Handle = (THandle)ComAggregate.TryGetWrapper(handleOrManagedObject);
            Object = (TObject)handleOrManagedObject;
        }
    }

    public ComHandle(THandle handle, TObject managedObject)
    {
        if (handle == null && managedObject == null)
        {
            Handle = null;
            Object = null;
        }
        else
        {
            // NOTE: This might get triggered if you do testing with the "NoWrap"
            // ComAggregatePolicy, since both handle will not be a ComObject in that
            // case.
            if (handle != null && !Marshal.IsComObject(handle))
            {
                throw new ArgumentException("must be null or a Com object", nameof(handle));
            }

            Handle = handle;
            Object = managedObject;
        }
    }

    /// <summary>
    /// Return the IComWrapperFixed object (as T) or the managed object (as T) if the managed object is not wrapped.
    /// </summary>
    public THandle Handle
    {
        get
        {
            Debug.Assert(field == null || Marshal.IsComObject(field), "Invariant broken!");

            if (field == null)
            {
                return Object;
            }
            else
            {
                return field;
            }
        }
    }

    /// <summary>
    /// Return the managed object
    /// </summary>
    public TObject Object { get; }

    public ComHandle<TNewHandle, TNewObject> Cast<TNewHandle, TNewObject>()
        where TNewHandle : class
        where TNewObject : class, TNewHandle
    {
        if (Handle is not TNewHandle newHandle)
        {
            throw new InvalidOperationException("Invalid cast.");
        }

        if (Object is not TNewObject newObject)
        {
            throw new InvalidOperationException("Invalid cast.");
        }

        return new ComHandle<TNewHandle, TNewObject>(newHandle, newObject);
    }
}
