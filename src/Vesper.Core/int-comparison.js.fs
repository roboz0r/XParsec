namespace Vesper

module IntComparison =

    let inline (<) (x: int) (y: int) : bool = (# "$0 < $1" x y : bool #)

    let inline (>) (x: int) (y: int) : bool = (# "$0 > $1" x y : bool #)

    let inline (<=) (x: int) (y: int) : bool = (# "$0 <= $1" x y : bool #)

    let inline (>=) (x: int) (y: int) : bool = (# "$0 >= $1" x y : bool #)
