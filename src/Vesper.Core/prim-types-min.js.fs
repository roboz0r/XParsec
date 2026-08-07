namespace Vesper

#nowarn "42"

type int =
    (# "number" #)
    with
        // `| 0` truncates back to 32 bits: JS `+` computes in float64.
        static member inline (+)(x: int, y: int) : int = (# "($0 + $1) | 0" x y : int #)
        static member inline (-)(x: int, y: int) : int = (# "($0 - $1) | 0" x y : int #)
        // `Math.imul` is the product mod 2^32 directly; a masked `$0 * $1` reaches ~2^64
        // and loses the low bits the mask keeps, past 2^53.
        static member inline ( * )(x: int, y: int) : int = (# "Math.imul($0, $1)" x y : int #)
        // `| 0` does double duty: JS `/` is true division, and truncating toward zero is
        // F#'s rule. `checkedDivisor` throws on 0, where JS would answer `Infinity` and
        // `Infinity | 0` a silent 0.
        static member inline (/)(x: int, y: int) : int = (# "($0 / $1) | 0" x (checkedDivisor y) : int #)
        static member inline (%)(x: int, y: int) : int = (# "($0 % $1) | 0" x (checkedDivisor y) : int #)
        static member inline (~+)(value: int) : int = value
        // The mask is the whole point here: `-(Int32.MinValue)` overflows to itself.
        static member inline (~-)(n: int) : int = (# "(-$0) | 0" n : int #)
        // The JS bitwise operators already coerce to signed int32 and answer in it, so
        // these need no mask.
        static member inline (&&&)(x: int, y: int) : int = (# "$0 & $1" x y : int #)
        static member inline (|||)(x: int, y: int) : int = (# "$0 | $1" x y : int #)
        static member inline (^^^)(x: int, y: int) : int = (# "$0 ^ $1" x y : int #)
        static member inline (~~~)(value: int) : int = (# "~$0" value : int #)
        static member inline (<<<)(value: int, shift: int) : int = (# "$0 << $1" value shift : int #)
        static member inline (>>>)(value: int, shift: int) : int = (# "$0 >> $1" value shift : int #)
    end
type bool = (# "boolean" #)
type unit = (# "undefined" #)
// `!0[]` is the element-type placeholder the CLR uses; the JS backend reads the
// repr as the marker that arrays ARE representable here, not as emitted text.
type 'T ``[]`` = (# "!0[]" #)
type 'T array = 'T[]

// One interface per arity — the contract pairs on typar count and order.
type Fun<'A, 'B> =
    abstract member Invoke: arg: 'A -> 'B

type Fun<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C

type Fun<'A, 'B, 'C, 'D> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C -> 'D

type Fun<'A, 'B, 'C, 'D, 'E> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C * d: 'D -> 'E
