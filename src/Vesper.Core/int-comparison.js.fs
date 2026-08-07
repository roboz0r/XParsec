namespace Vesper

// `int` is a JS `number` held in int32 range, so the native relational operators answer
// exactly — no structural-compare detour.
module IntComparison =

    let inline (<) (x: int) (y: int) : bool = (# "$0 < $1" x y : bool #)

    let inline (>) (x: int) (y: int) : bool = (# "$0 > $1" x y : bool #)

    let inline (<=) (x: int) (y: int) : bool = (# "$0 <= $1" x y : bool #)

    let inline (>=) (x: int) (y: int) : bool = (# "$0 >= $1" x y : bool #)
