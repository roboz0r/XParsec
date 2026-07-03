namespace Vesper

// The `dynamic` operators — the disciplined entry/exit surface for the opaque
// `dynamic` type (see `prim-types-dynamic.js.fsi` + `docs/dynamic-typing-design.md`).
// JS-only (`files-js`); the inline bodies live in `ops-dynamic.js.fs`
// (`inline-bodies-js`) as `$0[$1]` computed-member templates.
//
//   retype  — the general erasing reinterpret (`(# "" x : ^U #)`): emits the value
//             unchanged and re-types it. Inherently unsafe (no runtime check) — the
//             escape valve, and the primitive `dynamic` / whole-value exit build on.
//   dynamic — enter `dynamic` from any value (`retype` at a fixed result type). `x`
//             alone never flows in silently; you say `dynamic x`.
//   (?)     — member access on a `dynamic` receiver, target-typed. Unconstrained it
//             defaults `^TResult` to `dynamic` (so `x?a?b` stays dynamic); a pinned
//             context (`let n: int = d?foo`) unifies `^TResult` before the default
//             fires — the principled escape back to static. STRICT `dynamic` receiver.
//   (?<-)   — the setter (`x?foo <- v`).

[<AutoOpen>]
module DynamicOperators =

    /// <summary>The general erasing reinterpret — emits its operand unchanged and
    /// re-types it <c>^U</c> (the <c>: ^U</c> annotation is the reinterpret target).
    /// Inherently unsafe (no runtime check); the escape valve, not the common
    /// path. Whole-value exit from <c>dynamic</c> is <c>retype d : 'T</c>.</summary>
    val inline retype: x: ^T -> ^U

    /// <summary>Enter <c>dynamic</c> — the same JS value, retyped to <c>dynamic</c>.
    /// Shares the type's name (as <c>int</c>/<c>string</c>/<c>box</c> do). A value
    /// never flows into <c>dynamic</c> silently; you write <c>dynamic x</c>.</summary>
    val inline dynamic: value: ^T -> dynamic

    /// <summary>Dynamic member access on a <c>dynamic</c> receiver — <c>x?foo</c>
    /// (F# spec 6.4.5). Emits the computed member read <c>x["foo"]</c>. Target-typed:
    /// <c>^TResult</c> defaults to <c>dynamic</c> when the context does not pin it, so
    /// <c>x?a?b</c> chains stay dynamic; a pinned context unifies it first and the
    /// default never fires — the principled, checked-by-you escape back to
    /// static.</summary>
    val inline (?): target: dynamic -> name: string -> ^TResult when default ^TResult: dynamic

    /// <summary>Dynamic member set on a <c>dynamic</c> receiver — <c>x?foo &lt;- v</c>.
    /// Emits the computed-member assignment <c>x["foo"] = v</c>.</summary>
    val inline (?<-): target: dynamic -> name: string -> value: ^TValue -> unit
