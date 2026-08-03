namespace Vesper

#nowarn "42"

type float32 =
    (# "System.Single" #)
    with
        static member inline (+)(x: float32, y: float32) : float32 = (# "add" x y : float32 #)
        static member inline (-)(x: float32, y: float32) : float32 = (# "sub" x y : float32 #)
        static member inline ( * )(x: float32, y: float32) : float32 = (# "mul" x y : float32 #)
        static member inline (/)(x: float32, y: float32) : float32 = (# "div" x y : float32 #)
        static member inline (%)(x: float32, y: float32) : float32 = (# "rem" x y : float32 #)
        static member inline (~+)(value: float32) : float32 = value
        static member inline (~-)(n: float32) : float32 = (# "neg" n : float32 #)
    end

type float =
    (# "System.Double" #)
    with
        static member inline (+)(x: float, y: float) : float = (# "add" x y : float #)
        static member inline (-)(x: float, y: float) : float = (# "sub" x y : float #)
        static member inline ( * )(x: float, y: float) : float = (# "mul" x y : float #)
        static member inline (/)(x: float, y: float) : float = (# "div" x y : float #)
        static member inline (%)(x: float, y: float) : float = (# "rem" x y : float #)
        static member inline (~+)(value: float) : float = value
        static member inline (~-)(n: float) : float = (# "neg" n : float #)
    end
type single = float32
type double = float
