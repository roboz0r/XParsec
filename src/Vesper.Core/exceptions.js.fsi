namespace System

open Vesper

/// <summary>The CLI type <see cref="T:System.InvalidOperationException"/>.</summary>
type InvalidOperationException =
    inherit exn
    new: message: string -> InvalidOperationException

/// <summary>The CLI type <see cref="T:System.ArgumentException"/>.</summary>
type ArgumentException =
    inherit exn
    new: message: string -> ArgumentException

/// <summary>The CLI type <see cref="T:System.ArgumentNullException"/>.</summary>
type ArgumentNullException =
    inherit exn
    new: message: string -> ArgumentNullException

/// <summary>The CLI type <see cref="T:System.NotSupportedException"/>.</summary>
type NotSupportedException =
    inherit exn
    new: message: string -> NotSupportedException

/// <summary>The CLI type <see cref="T:System.IndexOutOfRangeException"/>.</summary>
type IndexOutOfRangeException =
    inherit exn
    new: message: string -> IndexOutOfRangeException

/// <summary>The CLI type <see cref="T:System.FormatException"/>.</summary>
type FormatException =
    inherit exn
    new: message: string -> FormatException
