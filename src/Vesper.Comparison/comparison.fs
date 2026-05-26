namespace Vesper

open System.Collections.Generic

// comparison.fs — the per-target *implementation* of `comparison.fsi`
// (symbol-resolution-plan §5.2: the `.fsi` is the target-agnostic contract,
// the `.fs` is the binding). The four bare ordering operators (`<` / `>` /
// `<=` / `>=`) live here as F# static-optimization over inline IL, exactly
// the same pattern as `(=)` / `(<>)` in `Vesper.Core/ops-platform.fs`.
//
// Each operator's body has a fallback (the static-opt *base*) and a list of
// per-primitive clauses. The base routes through
// `Comparer<^T>.Default.Compare(x, y)`, the structural comparator the BCL
// dispatches through `IComparable<^T>` when the operand type implements it
// — the same family the generated `CompareTo` pair uses, so a user record /
// union opted in via `[<StructuralComparison>]` compares lexicographically by
// construction. Each primitive clause emits
// the matching CIL opcode directly (`clt` / `cgt`), so primitive comparisons
// stay alloc-free with no comparer dispatch — mirrors the equality family.
//
// `<=` / `>=` are the negated complements of `>` / `<` (no dedicated CIL
// opcode), realised the same way the equality family realises `<>`: an
// outer `ceq (... : bool) false` against the inner compare result.
//
// An **unpinned** generic operand (`let f a b = a < b`, where `^T` is a free
// typar `Comparer<!0>` can't encode) falls back to `Emit.BuiltinOps`'s
// `clt` / `cgt` via the codegen `isGround` guard — same posture as
// equality. That fallback is wrong for non-primitive un-ground operands
// (`clt` on object refs is unverifiable), but matches existing behaviour
// and is no worse than the equality side's `ceq` fallback.

[<AutoOpen>]
module ComparisonOperators =

    /// Structural less-than. The base routes through
    /// `Comparer<^T>.Default.Compare(x, y) < 0` (`clt` against `0`), which
    /// dispatches to `IComparable<^T>::CompareTo` for an opted-in record /
    /// union; each primitive clause short-circuits to CIL `clt`.
    let inline (<) (x: ^T) (y: ^T) : bool =
        (# "clt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #)
        when ^T: byte = (# "clt" x y : bool #)
        when ^T: char = (# "clt" x y : bool #)
        when ^T: bool = (# "clt" x y : bool #)
        when ^T: float32 = (# "clt" x y : bool #)
        when ^T: float = (# "clt" x y : bool #)
        when ^T: int64 = (# "clt" x y : bool #)
        when ^T: int = (# "clt" x y : bool #)

    /// Structural greater-than — mirror of `(<)` with `cgt`.
    let inline (>) (x: ^T) (y: ^T) : bool =
        (# "cgt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #)
        when ^T: byte = (# "cgt" x y : bool #)
        when ^T: char = (# "cgt" x y : bool #)
        when ^T: bool = (# "cgt" x y : bool #)
        when ^T: float32 = (# "cgt" x y : bool #)
        when ^T: float = (# "cgt" x y : bool #)
        when ^T: int64 = (# "cgt" x y : bool #)
        when ^T: int = (# "cgt" x y : bool #)

    /// Structural less-than-or-equal: `not (x > y)`. No dedicated CIL opcode;
    /// realised as `ceq (x > y) false`, matching how the equality family
    /// realises `<>` against a primitive `ceq`.
    let inline (<=) (x: ^T) (y: ^T) : bool =
        (# "ceq" (# "cgt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #) false : bool #)
        when ^T: byte = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: char = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: bool = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: float32 = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: float = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: int64 = (# "ceq" (# "cgt" x y : bool #) false : bool #)
        when ^T: int = (# "ceq" (# "cgt" x y : bool #) false : bool #)

    /// Structural greater-than-or-equal: `not (x < y)`. Mirror of `(<=)` with `clt`.
    let inline (>=) (x: ^T) (y: ^T) : bool =
        (# "ceq" (# "clt" (Comparer< ^T >.Default.Compare(x, y)) 0 : bool #) false : bool #)
        when ^T: byte = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: char = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: bool = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: float32 = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: float = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: int64 = (# "ceq" (# "clt" x y : bool #) false : bool #)
        when ^T: int = (# "ceq" (# "clt" x y : bool #) false : bool #)
