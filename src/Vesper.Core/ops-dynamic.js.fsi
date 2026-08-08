namespace Vesper

/// <summary>Unsafe FFI escape hatches. Not <c>[&lt;AutoOpen&gt;]</c>: an explicit
/// <c>open Vesper.Unsafe</c> is the marker that a cast here is unchecked.</summary>
module Unsafe =

    /// <summary>The general erasing reinterpret — emits its operand unchanged and re-types
    /// it <c>^U</c>, with no runtime check.</summary>
    val inline retype: x: ^T -> ^U

[<AutoOpen>]
module DynamicOperators =

    /// <summary>Enter <c>dynamic</c> — the same JS value, retyped. A value never flows
    /// into <c>dynamic</c> silently; you write <c>dynamic x</c>.</summary>
    val inline dynamic: value: ^T -> dynamic

    /// <summary>Dynamic member access — <c>x?foo</c> (F# spec 6.4.5) emits the computed
    /// member read <c>x["foo"]</c>. Target-typed: <c>^TResult</c> defaults to
    /// <c>dynamic</c>, so <c>x?a?b</c> stays dynamic unless the context pins it.</summary>
    val inline (?): target: dynamic -> name: string -> ^TResult when default ^TResult: dynamic

    /// <summary>Dynamic member set on a <c>dynamic</c> value — <c>x?foo &lt;- v</c>.
    /// Emits the computed-member assignment <c>x["foo"] = v</c>.</summary>
    val inline (?<-): target: dynamic -> name: string -> value: ^TValue -> unit
