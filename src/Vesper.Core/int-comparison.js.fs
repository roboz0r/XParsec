namespace Vesper

// The primitive ordering subset. `int` is a JS `number` held in int32 range, so the
// native relational operators answer exactly — no `structuralCompare` detour, which is
// what the polymorphic family in Vesper.Comparison needs for aggregates.
module IntComparison =

    let inline (<) (x: int) (y: int) : bool = (# "$0 < $1" x y : bool #)

    let inline (>) (x: int) (y: int) : bool = (# "$0 > $1" x y : bool #)

    let inline (<=) (x: int) (y: int) : bool = (# "$0 <= $1" x y : bool #)

    let inline (>=) (x: int) (y: int) : bool = (# "$0 >= $1" x y : bool #)
