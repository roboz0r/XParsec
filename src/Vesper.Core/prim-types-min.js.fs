namespace Vesper

#nowarn "42"

type int =
    (# "number" #)
    with
        // `| 0` truncates back to 32 bits: JS `+` computes in float64.
        static member (+)(x: int, y: int) : int = (# "($0 + $1) | 0" x y : int #)
    end
type bool = (# "boolean" #)
type unit = (# "undefined" #)
