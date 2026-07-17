namespace Vesper

[<AutoOpen>]
module ComparisonOperators =

    let inline (<) (x: ^T) (y: ^T) : bool =
        (# "$0 < 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 < $1" x y : bool #)
        when ^T: char = (# "$0 < $1" x y : bool #)
        when ^T: bool = (# "$0 < $1" x y : bool #)
        when ^T: float32 = (# "$0 < $1" x y : bool #)
        when ^T: float = (# "$0 < $1" x y : bool #)
        when ^T: int64 = (# "$0 < $1" x y : bool #)
        when ^T: int = (# "$0 < $1" x y : bool #)

    let inline (>) (x: ^T) (y: ^T) : bool =
        (# "$0 > 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 > $1" x y : bool #)
        when ^T: char = (# "$0 > $1" x y : bool #)
        when ^T: bool = (# "$0 > $1" x y : bool #)
        when ^T: float32 = (# "$0 > $1" x y : bool #)
        when ^T: float = (# "$0 > $1" x y : bool #)
        when ^T: int64 = (# "$0 > $1" x y : bool #)
        when ^T: int = (# "$0 > $1" x y : bool #)

    let inline (<=) (x: ^T) (y: ^T) : bool =
        (# "$0 <= 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 <= $1" x y : bool #)
        when ^T: char = (# "$0 <= $1" x y : bool #)
        when ^T: bool = (# "$0 <= $1" x y : bool #)
        when ^T: float32 = (# "$0 <= $1" x y : bool #)
        when ^T: float = (# "$0 <= $1" x y : bool #)
        when ^T: int64 = (# "$0 <= $1" x y : bool #)
        when ^T: int = (# "$0 <= $1" x y : bool #)

    let inline (>=) (x: ^T) (y: ^T) : bool =
        (# "$0 >= 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 >= $1" x y : bool #)
        when ^T: char = (# "$0 >= $1" x y : bool #)
        when ^T: bool = (# "$0 >= $1" x y : bool #)
        when ^T: float32 = (# "$0 >= $1" x y : bool #)
        when ^T: float = (# "$0 >= $1" x y : bool #)
        when ^T: int64 = (# "$0 >= $1" x y : bool #)
        when ^T: int = (# "$0 >= $1" x y : bool #)
