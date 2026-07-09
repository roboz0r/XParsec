module Primitives

// Capability interface: `extern interface` (tagged) — an all-abstract external
// surface, distinct from bare `extern` (opaque value capability) and `extern class`
// (heritable reference base). The `interface` tag is admitted only because `with`
// follows it.
type disposable = extern interface with
    abstract member Dispose: unit -> unit

// Generic capability interface.
type equatable<'T> = extern interface with
    abstract member Equals: 'T -> bool

// Disambiguation: a bare `extern with interface <Type>` carries an interface-
// implementation MEMBER (the `interface` has a type name), NOT an `extern interface`
// tag — so it stays an untagged `extern`.
type Iterable<'T> = extern with
    interface System.Collections.Generic.IEnumerable<'T>
    member GetEnumerator: unit -> System.Collections.Generic.IEnumerator<'T>
