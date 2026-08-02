namespace Vesper

#nowarn "42"

type int =
    (# "number" #)
    with
        // `| 0` truncates back to 32 bits: JS `+` computes in float64.
        static member inline (+)(x: int, y: int) : int = (# "($0 + $1) | 0" x y : int #)
        static member inline (-)(x: int, y: int) : int = (# "($0 - $1) | 0" x y : int #)
        // `Math.imul` computes the product mod 2^32 directly. A masked `$0 * $1` cannot:
        // a full 32×32 product reaches ~2^64 and loses its low bits — the ones the mask
        // keeps — past 2^53.
        static member inline ( * )(x: int, y: int) : int = (# "Math.imul($0, $1)" x y : int #)
        // `| 0` is doing double duty: JS `/` is true division, and truncating toward zero
        // is F#'s rule. `checkedDivisor` throws on 0, where JS would answer `Infinity` and
        // `Infinity | 0` would quietly be 0; it returns its argument so the mask still
        // wraps it, and reads the operand once.
        static member inline (/)(x: int, y: int) : int = (# "($0 / $1) | 0" x (checkedDivisor y) : int #)
        static member inline (%)(x: int, y: int) : int = (# "($0 % $1) | 0" x (checkedDivisor y) : int #)
        // The identity, so no template and nothing to mask — the one member here whose
        // body is the same on both targets.
        static member inline (~+)(value: int) : int = value
        // The mask is the whole point here: `-(Int32.MinValue)` overflows to itself.
        static member inline (~-)(n: int) : int = (# "(-$0) | 0" n : int #)
        // The JS bitwise operators already coerce to signed int32 and answer in it, so
        // these need no mask — unlike the arithmetic above.
        static member inline (&&&)(x: int, y: int) : int = (# "$0 & $1" x y : int #)
        static member inline (|||)(x: int, y: int) : int = (# "$0 | $1" x y : int #)
        static member inline (^^^)(x: int, y: int) : int = (# "$0 ^ $1" x y : int #)
        static member inline (~~~)(value: int) : int = (# "~$0" value : int #)
        static member inline (<<<)(value: int, shift: int) : int = (# "$0 << $1" value shift : int #)
        static member inline (>>>)(value: int, shift: int) : int = (# "$0 >> $1" value shift : int #)
    end
type bool = (# "boolean" #)
type unit = (# "undefined" #)
