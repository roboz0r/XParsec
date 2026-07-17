namespace Vesper

#nowarn "42"

/// Unsafe FFI escape hatches — NOT auto-opened (see the `.fsi`): `open Vesper.Unsafe`
/// to reach `retype`.
module Unsafe =

    /// General erasing reinterpret — the identity cast (`(# "" x : ^U #)` emits `x`
    /// unchanged, re-typed `^U`). The primitive `dynamic`/whole-value exit build on.
    let inline retype (x: ^T) : ^U = (# "" x : ^U #)

[<AutoOpen>]
module DynamicOperators =

    /// Enter `dynamic` — `retype` at the fixed result type `dynamic` (same JS value).
    let inline dynamic (value: ^T) : dynamic = Unsafe.retype value

    /// `x?foo` → the computed member read `x["foo"]`.
    let inline (?) (target: dynamic) (name: string) : ^TResult = (# "$0[$1]" target name : ^TResult #)

    /// `x?foo <- v` → the computed-member assignment `x["foo"] = v`.
    let inline (?<-) (target: dynamic) (name: string) (value: ^TValue) : unit =
        (# "$0[$1] = $2" target name value : unit #)
