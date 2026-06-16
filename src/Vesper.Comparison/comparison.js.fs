namespace Vesper

// comparison.js.fs — the JS-target body of `comparison.fsi` (manifest `inline-bodies-js`),
// the ordering analogue of `Vesper.Core/ops-platform.js.fs` and the JS counterpart of the
// CLR `comparison.fs`. Each of the four operators is static-optimization over `$N`-template
// JS: a primitive operand lowers to the JS relational operator inline (`$0 < $1`); an
// aggregate operand delegates to `structuralCompare` (`Vesper.Comparison.mjs`), tested
// against 0. Unlike the CLR side there is no `ceq … false` negation — JS has `<=` / `>=`
// directly. Runtime wiring: see `Vesper.Comparison.mjs`.

[<AutoOpen>]
module ComparisonOperators =

    /// Structural less-than. Primitive clauses are a direct JS `<`; the aggregate base is
    /// `structuralCompare x y < 0`.
    let inline (<) (x: ^T) (y: ^T) : bool =
        (# "$0 < 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 < $1" x y : bool #)
        when ^T: char = (# "$0 < $1" x y : bool #)
        when ^T: bool = (# "$0 < $1" x y : bool #)
        when ^T: float32 = (# "$0 < $1" x y : bool #)
        when ^T: float = (# "$0 < $1" x y : bool #)
        when ^T: int64 = (# "$0 < $1" x y : bool #)
        when ^T: int = (# "$0 < $1" x y : bool #)

    /// Structural greater-than — mirror of `(<)` with `>`.
    let inline (>) (x: ^T) (y: ^T) : bool =
        (# "$0 > 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 > $1" x y : bool #)
        when ^T: char = (# "$0 > $1" x y : bool #)
        when ^T: bool = (# "$0 > $1" x y : bool #)
        when ^T: float32 = (# "$0 > $1" x y : bool #)
        when ^T: float = (# "$0 > $1" x y : bool #)
        when ^T: int64 = (# "$0 > $1" x y : bool #)
        when ^T: int = (# "$0 > $1" x y : bool #)

    /// Structural less-than-or-equal. A direct JS `<=` (no `ceq … false` complement is
    /// needed as on the CLR — JS has the operator); the aggregate base is
    /// `structuralCompare x y <= 0`.
    let inline (<=) (x: ^T) (y: ^T) : bool =
        (# "$0 <= 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 <= $1" x y : bool #)
        when ^T: char = (# "$0 <= $1" x y : bool #)
        when ^T: bool = (# "$0 <= $1" x y : bool #)
        when ^T: float32 = (# "$0 <= $1" x y : bool #)
        when ^T: float = (# "$0 <= $1" x y : bool #)
        when ^T: int64 = (# "$0 <= $1" x y : bool #)
        when ^T: int = (# "$0 <= $1" x y : bool #)

    /// Structural greater-than-or-equal — mirror of `(<=)` with `>=`.
    let inline (>=) (x: ^T) (y: ^T) : bool =
        (# "$0 >= 0" (structuralCompare x y) : bool #)
        when ^T: byte = (# "$0 >= $1" x y : bool #)
        when ^T: char = (# "$0 >= $1" x y : bool #)
        when ^T: bool = (# "$0 >= $1" x y : bool #)
        when ^T: float32 = (# "$0 >= $1" x y : bool #)
        when ^T: float = (# "$0 >= $1" x y : bool #)
        when ^T: int64 = (# "$0 >= $1" x y : bool #)
        when ^T: int = (# "$0 >= $1" x y : bool #)
