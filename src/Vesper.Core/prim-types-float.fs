namespace Vesper

#nowarn "42"

// The float mnemonics are the plain ones at both widths: CIL `add`/`sub`/`mul`/`div`/`rem`
// are IEEE operations when their operands are float32/float64, so neither width needs a
// `conv.*` and neither division needs an unsigned form. `rem` is the IEEE truncated
// remainder, which is F#'s `%`.

type float32 =
    (# "System.Single" #)
    with
        static member (+)(x: float32, y: float32) : float32 = (# "add" x y : float32 #)
        static member (-)(x: float32, y: float32) : float32 = (# "sub" x y : float32 #)
        static member ( * )(x: float32, y: float32) : float32 = (# "mul" x y : float32 #)
        static member (/)(x: float32, y: float32) : float32 = (# "div" x y : float32 #)
        static member (%)(x: float32, y: float32) : float32 = (# "rem" x y : float32 #)
        static member (~-)(n: float32) : float32 = (# "neg" n : float32 #)
    end

type float =
    (# "System.Double" #)
    with
        static member (+)(x: float, y: float) : float = (# "add" x y : float #)
        static member (-)(x: float, y: float) : float = (# "sub" x y : float #)
        static member ( * )(x: float, y: float) : float = (# "mul" x y : float #)
        static member (/)(x: float, y: float) : float = (# "div" x y : float #)
        static member (%)(x: float, y: float) : float = (# "rem" x y : float #)
        static member (~-)(n: float) : float = (# "neg" n : float #)
    end
type single = float32
type double = float
