namespace Vesper

#nowarn "42"

/// Not auto-opened: `open Vesper.Unsafe` to reach `retype`.
module Unsafe =

    /// The empty template is the erasing identity cast — emits `x` unchanged, re-typed `^U`.
    let inline retype (x: ^T) : ^U = (# "" x : ^U #)

[<AutoOpen>]
module DynamicOperators =

    let inline dynamic (value: ^T) : dynamic = Unsafe.retype value

    /// `x?foo` → the computed member read `x["foo"]`.
    let inline (?) (target: dynamic) (name: string) : ^TResult = (# "$0[$1]" target name : ^TResult #)

    /// `x?foo <- v` → the computed-member assignment `x["foo"] = v`.
    let inline (?<-) (target: dynamic) (name: string) (value: ^TValue) : unit =
        (# "$0[$1] = $2" target name value : unit #)
