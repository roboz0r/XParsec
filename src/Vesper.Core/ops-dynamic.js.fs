namespace Vesper

#nowarn "42"

// ops-dynamic.js.fs — the JS-target inline bodies for `ops-dynamic.js.fsi`. Selected
// via the manifest `inline-bodies-js` list; each `let inline` body is read across the
// package boundary and spliced at every use site (the `$N`-template idiom documented
// in `ops-platform.js.fs`: `$0`/`$1`/`$2` are the operands in source order).
//
// `retype` is the empty-string identity intrinsic `(# "" x : ^U #)` — it emits the
// operand verbatim and re-types it (the JS backend lowers an empty-template intrinsic
// to its lone operand, no wrapper). `(?)` / `(?<-)` emit the safe COMPUTED-member form
// `$0[$1]` / `$0[$1] = $2` (bracket access works for any member name, including ones
// that are not valid JS identifiers).

[<AutoOpen>]
module DynamicOperators =

    /// General erasing reinterpret — the identity cast (`(# "" x : ^U #)` emits `x`
    /// unchanged, re-typed `^U`). The primitive `dynamic`/whole-value exit build on.
    let inline retype (x: ^T) : ^U = (# "" x : ^U #)

    /// Enter `dynamic` — `retype` at the fixed result type `dynamic` (same JS value).
    let inline dynamic (value: ^T) : dynamic = retype value

    /// `x?foo` → the computed member read `x["foo"]`.
    let inline (?) (target: dynamic) (name: string) : ^TResult = (# "$0[$1]" target name : ^TResult #)

    /// `x?foo <- v` → the computed-member assignment `x["foo"] = v`.
    let inline (?<-) (target: dynamic) (name: string) (value: ^TValue) : unit =
        (# "$0[$1] = $2" target name value : unit #)
