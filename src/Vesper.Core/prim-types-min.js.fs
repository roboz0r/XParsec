namespace Vesper

#nowarn "42"

type int =
    (# "number" #)
    with
        // `| 0` truncates back to 32 bits: JS `+` computes in float64.
        static member (+)(x: int, y: int) : int = (# "($0 + $1) | 0" x y : int #)
        // The JS bitwise operators already coerce to signed int32 and answer in it, so
        // these need no mask — unlike the arithmetic above.
        static member (&&&)(x: int, y: int) : int = (# "$0 & $1" x y : int #)
        static member (|||)(x: int, y: int) : int = (# "$0 | $1" x y : int #)
        static member (^^^)(x: int, y: int) : int = (# "$0 ^ $1" x y : int #)
        static member (~~~)(value: int) : int = (# "~$0" value : int #)
        static member (<<<)(value: int, shift: int) : int = (# "$0 << $1" value shift : int #)
        static member (>>>)(value: int, shift: int) : int = (# "$0 >> $1" value shift : int #)
    end
type bool = (# "boolean" #)
type unit = (# "undefined" #)
