namespace Vesper

// TODO: Add compiler recognised types to this file, so that we can use them in the core library without referencing FSharp.Core

/// <summary>An abbreviation for the CLI type <see cref="T:System.Exception"/>.</summary>
///
/// <category>Basic Types</category>
type exn = extern class with
    inherit obj

    /// <summary>Equatable but NOT comparable, as <c>obj</c> is.</summary>
    interface equatable<exn>

    /// <summary>Creates an exception carrying a message.</summary>
    new: message: string -> exn

    /// <summary>Creates an exception with no message.</summary>
    new: unit -> exn
