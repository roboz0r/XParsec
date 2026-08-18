namespace Vesper

#nowarn "42"

// `float` IS the JS number, so its arithmetic is the bare operator — no mask, and no
// `checkedDivisor`: `1.0 / 0.0` is `Infinity`, which is IEEE's answer and F#'s. `float32`
// is a narrower width JS does not have, so every result rounds back through `Math.fround`.

type float32 =
    (# "number" #)
    with
        static member inline (+)(x: float32, y: float32) : float32 = (# "Math.fround($0 + $1)" x y : float32 #)
        static member inline (-)(x: float32, y: float32) : float32 = (# "Math.fround($0 - $1)" x y : float32 #)
        static member inline ( * )(x: float32, y: float32) : float32 = (# "Math.fround($0 * $1)" x y : float32 #)
        static member inline (/)(x: float32, y: float32) : float32 = (# "Math.fround($0 / $1)" x y : float32 #)
        static member inline (%)(x: float32, y: float32) : float32 = (# "Math.fround($0 % $1)" x y : float32 #)
        static member inline (~+)(value: float32) : float32 = value
        static member inline (~-)(n: float32) : float32 = (# "Math.fround(-$0)" n : float32 #)
    end

type float =
    (# "number" #)
    with
        static member inline (+)(x: float, y: float) : float = (# "$0 + $1" x y : float #)
        static member inline (-)(x: float, y: float) : float = (# "$0 - $1" x y : float #)
        static member inline ( * )(x: float, y: float) : float = (# "$0 * $1" x y : float #)
        static member inline (/)(x: float, y: float) : float = (# "$0 / $1" x y : float #)
        static member inline (%)(x: float, y: float) : float = (# "$0 % $1" x y : float #)
        static member inline (~+)(value: float) : float = value
        static member inline (~-)(n: float) : float = (# "-$0" n : float #)
    end
type single = float32
type double = float
