namespace System

open Vesper

/// <summary>The CLI type <see cref="T:System.SystemException"/>, the root every exception
/// below derives from.</summary>
type SystemException =
    inherit exn
    new: message: string -> SystemException

/// <summary>The CLI type <see cref="T:System.InvalidOperationException"/>.</summary>
type InvalidOperationException =
    inherit SystemException
    new: message: string -> InvalidOperationException

/// <summary>The CLI type <see cref="T:System.ArgumentException"/>.</summary>
type ArgumentException =
    inherit SystemException
    new: message: string -> ArgumentException

/// <summary>The CLI type <see cref="T:System.ArgumentNullException"/>.</summary>
type ArgumentNullException =
    inherit ArgumentException
    new: message: string -> ArgumentNullException

/// <summary>The CLI type <see cref="T:System.NotSupportedException"/>.</summary>
type NotSupportedException =
    inherit SystemException
    new: message: string -> NotSupportedException

/// <summary>The CLI type <see cref="T:System.IndexOutOfRangeException"/>.</summary>
type IndexOutOfRangeException =
    inherit SystemException
    new: message: string -> IndexOutOfRangeException

/// <summary>The CLI type <see cref="T:System.FormatException"/>.</summary>
type FormatException =
    inherit SystemException
    new: message: string -> FormatException
