namespace System

open Vesper

// The common BCL exception roots, as Vesper *contract* types. Each inherits the exception root `exn` (`Vesper.exn`), so a consumer's
// subtype check (`raise : exn -> 'T`, a `:> exn` argument) reconciles through this
// contract `inherit` chain — independent of any host BCL metadata. On JS every one
// erases to the `exn` repr (`Error`, from `prim-types-exn.js.fs`); the CLR target
// reaches the same names through `System.Private.CoreLib` instead (this package is
// not referenced by CLR builds). The `(message: string)` constructor is the leading
// BCL ctor, the only arg with an `Error` slot on JS.

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
