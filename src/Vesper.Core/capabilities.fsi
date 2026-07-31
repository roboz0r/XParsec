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

// The iteration capability CLUSTER — `for … in` resolves and lowers through these.
// Declared in `Vesper.Core` (not `Vesper.List`) so `Vesper.List` — whose `List` union
// implements `seq` — can resolve them from a dependency rather than its own not-yet-built
// contract. `enumerator` is declared before `seq` because `seq.GetEnumerator` returns it.
// Both keep the `Vesper.Collections` namespace, so the resolver lookup and every bare
// `seq<'T>` reference are unchanged from when `seq` was the `IEnumerable<'T>` abbreviation.
//
// These are the FIRST capabilities whose BCL interface is WIDER than the member surface:
// `IEnumerable` / `IEnumerator` drag in the non-generic bases, `object Current`, and
// `Reset`. The author writes only the pull protocol (`GetEnumerator` / `MoveNext` /
// `Current`); the CLR backend synthesizes the BCL co-slots as forwarding shims during
// reconciliation, so a C# consumer of a Vesper assembly iterates the type normally.
// Disposal is NOT part of the cluster — an `enumerator` that ALSO `interface disposable`
// is disposed in the `for … in` `finally` via that separate capability.

/// <summary>The iteration-cursor capability — anchors the enumerator half of `for … in`.
/// On the CLI it is <see cref="T:System.Collections.Generic.IEnumerator`1"/>.</summary>
///
/// <remarks>
///  Inherits <c>disposable</c>: an enumerator may hold a resource (a file/stream cursor),
///  so it is disposed at the end of a `for … in` and is a valid `use` binding — BCL parity
///  (<c>IEnumerator`1 : IDisposable</c>). Disposal is the SEPARATE `disposable` capability
///  composed in, not a `Dispose` member declared here; a purely in-memory enumerator's
///  `Dispose` is a no-op, exactly as in F#.
///</remarks>
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
