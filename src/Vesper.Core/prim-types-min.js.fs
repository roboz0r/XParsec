namespace Vesper

#nowarn "42"

open Vesper.ArithmeticRuntime

type int =
    (# "number" #)
    with
        // `| 0` truncates back to 32 bits: JS `+` computes in float64.
        static member inline (+)(x: int, y: int) : int = (# "($0 + $1) | 0" x y : int #)
        static member inline (-)(x: int, y: int) : int = (# "($0 - $1) | 0" x y : int #)
        // `Math.imul` is the exact product mod 2^32; a masked `$0 * $1` loses low bits past 2^53.
        static member inline ( * )(x: int, y: int) : int = (# "Math.imul($0, $1)" x y : int #)
        // `| 0` does double duty: JS `/` is true division, and truncating toward zero is
        // F#'s rule. `checkedDivisor` throws on 0, where `Infinity | 0` would answer 0.
        static member inline (/)(x: int, y: int) : int = (# "($0 / $1) | 0" x (checkedDivisor y) : int #)
        static member inline (%)(x: int, y: int) : int = (# "($0 % $1) | 0" x (checkedDivisor y) : int #)
        static member inline (~+)(value: int) : int = value
        // The mask is the whole point here: `-(Int32.MinValue)` overflows to itself.
        static member inline (~-)(n: int) : int = (# "(-$0) | 0" n : int #)
        // JS bitwise operators already coerce to and answer in signed int32 — no mask needed.
        static member inline (&&&)(x: int, y: int) : int = (# "$0 & $1" x y : int #)
        static member inline (|||)(x: int, y: int) : int = (# "$0 | $1" x y : int #)
        static member inline (^^^)(x: int, y: int) : int = (# "$0 ^ $1" x y : int #)
        static member inline (~~~)(value: int) : int = (# "~$0" value : int #)
        static member inline (<<<)(value: int, shift: int) : int = (# "$0 << $1" value shift : int #)
        static member inline (>>>)(value: int, shift: int) : int = (# "$0 >> $1" value shift : int #)
    end
type bool = (# "boolean" #)
type unit = (# "undefined" #)

// JS has no interfaces, so these name nothing at runtime — `!` is illegal in a JS
// identifier, and a sentinel reaching emit is a syntax error rather than a wrong global.
type equatable<'T> = (# "!Vesper.equatable" #)
type comparable<'T> = (# "!Vesper.comparable" #)
type disposable = (# "!Vesper.disposable" #)

type Fun<'A, 'B> =
    abstract member Invoke: arg: 'A -> 'B

type Fun<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C

type Fun<'A, 'B, 'C, 'D> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C -> 'D

type Fun<'A, 'B, 'C, 'D, 'E> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C * d: 'D -> 'E
