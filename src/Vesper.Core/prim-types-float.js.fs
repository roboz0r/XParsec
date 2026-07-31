namespace Vesper

#nowarn "42"

// `float` IS the JS number, so its arithmetic is the bare JS operator — no mask, and no
// `checkedDivisor`: `1.0 / 0.0` is `Infinity`, which is IEEE's answer and F#'s.
// `float32` is a narrower width JS does not have, so every result rounds back through
// `Math.fround`.

type float32 =
    (# "number" #)
    with
        static member (+)(x: float32, y: float32) : float32 = (# "Math.fround($0 + $1)" x y : float32 #)
        static member (-)(x: float32, y: float32) : float32 = (# "Math.fround($0 - $1)" x y : float32 #)
        static member ( * )(x: float32, y: float32) : float32 = (# "Math.fround($0 * $1)" x y : float32 #)
        static member (/)(x: float32, y: float32) : float32 = (# "Math.fround($0 / $1)" x y : float32 #)
        static member (%)(x: float32, y: float32) : float32 = (# "Math.fround($0 % $1)" x y : float32 #)
        static member (~-)(n: float32) : float32 = (# "Math.fround(-$0)" n : float32 #)
    end

type float =
    (# "number" #)
    with
        static member (+)(x: float, y: float) : float = (# "$0 + $1" x y : float #)
        static member (-)(x: float, y: float) : float = (# "$0 - $1" x y : float #)
        static member ( * )(x: float, y: float) : float = (# "$0 * $1" x y : float #)
        static member (/)(x: float, y: float) : float = (# "$0 / $1" x y : float #)
        static member (%)(x: float, y: float) : float = (# "$0 % $1" x y : float #)
        static member (~-)(n: float) : float = (# "-$0" n : float #)
    end
