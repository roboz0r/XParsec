namespace Vesper

open System.Collections.Generic

[<AutoOpen>]
module ComparisonOperators =

    let inline (<) (x: ^T) (y: ^T) : bool =
        (# "clt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #)
        when ^T: byte = (# "clt" x y : bool #)
        when ^T: char = (# "clt" x y : bool #)
        when ^T: bool = (# "clt" x y : bool #)
        when ^T: float32 = (# "clt" x y : bool #)
        when ^T: float = (# "clt" x y : bool #)
        when ^T: int64 = (# "clt" x y : bool #)
        when ^T: int = (# "clt" x y : bool #)

    let inline (>) (x: ^T) (y: ^T) : bool =
        (# "cgt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #)
        when ^T: byte = (# "cgt" x y : bool #)
        when ^T: char = (# "cgt" x y : bool #)
        when ^T: bool = (# "cgt" x y : bool #)
        when ^T: float32 = (# "cgt" x y : bool #)
        when ^T: float = (# "cgt" x y : bool #)
        when ^T: int64 = (# "cgt" x y : bool #)
        when ^T: int = (# "cgt" x y : bool #)

    let inline (<=) (x: ^T) (y: ^T) : bool =
        (# "ceq" (# "cgt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #) false : bool #)
        when ^T: byte = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: char = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: bool = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: float32 = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: float = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: int64 = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: int = (# "ceq" (# "cgt" x y : bool #) false : bool #)

    let inline (>=) (x: ^T) (y: ^T) : bool =
        (# "ceq" (# "clt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #) false : bool #)
        when ^T: byte = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: char = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: bool = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: float32 = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: float = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: int64 = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: int = (# "ceq" (# "clt" x y : bool #) false : bool #)
