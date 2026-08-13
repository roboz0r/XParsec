namespace Vesper.Collections

// `for … in` resolves through these. An implementor writes only `MoveNext`/`Current`;
// the CLR backend fills in the wider BCL surface (non-generic bases, `object Current`,
// `Reset`).

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
