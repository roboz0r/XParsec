namespace Vesper

/// <summary>The disposal capability — anchors `use` (and `for … in` finally). On
/// the CLI it is <see cref="T:System.IDisposable"/>.</summary>
///
/// <category>Language Capabilities</category>
type disposable = extern interface with
    abstract member Dispose: unit -> unit

/// <summary>The equality capability — anchors `[<CustomEquality>]` conformance.
/// On the CLI it is <see cref="T:System.IEquatable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
type equatable<'T> = extern interface with
    abstract member Equals: 'T -> bool

/// <summary>The comparison capability — anchors `[<CustomComparison>]`
/// conformance. On the CLI it is <see cref="T:System.IComparable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
type comparable<'T> = extern interface with
    abstract member CompareTo: 'T -> int

namespace Vesper.Collections

// The iteration cluster — `for … in` resolves and lowers through these. An implementor
// writes only the pull protocol declared below; the BCL surface is wider (the non-generic
// bases, `object Current`, `Reset`) and the CLR backend fills those slots in.

/// <summary>The iteration-cursor capability — anchors the enumerator half of `for … in`.
/// On the CLI it is <see cref="T:System.Collections.Generic.IEnumerator`1"/>.</summary>
///
/// <remarks>Inherits <c>disposable</c>: an enumerator may hold a file or stream cursor, so
/// it is disposed at the end of a <c>for … in</c> and is a valid <c>use</c> binding.</remarks>
///
/// <category>Language Capabilities</category>
type enumerator<'T> = extern interface with
    inherit Vesper.disposable

    abstract member MoveNext: unit -> bool
    abstract member Current: 'T

/// <summary>The iteration capability — anchors `for … in`. On the CLI it is
/// <see cref="T:System.Collections.Generic.IEnumerable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
type seq<'T> = extern interface with
    abstract member GetEnumerator: unit -> enumerator<'T>
