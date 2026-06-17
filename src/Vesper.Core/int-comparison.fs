namespace Vesper

module IntComparison =

    let inline (<) (x: int) (y: int) : bool = (# "clt" x y : bool #)

    let inline (>) (x: int) (y: int) : bool = (# "cgt" x y : bool #)

    let inline (<=) (x: int) (y: int) : bool = (# "ceq" (# "cgt" x y : bool #) false : bool #)

    let inline (>=) (x: int) (y: int) : bool = (# "ceq" (# "clt" x y : bool #) false : bool #)
