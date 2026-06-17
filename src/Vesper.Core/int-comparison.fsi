namespace Vesper

// The polymorphic ordering family lives in Vesper.Comparison:
// `val inline (<) : 'T -> 'T -> bool when 'T: comparison`, dispatching
// through `Comparer<^T>.Default` for aggregates. This module is the *primitive
// subset*, in Vesper.Core, so a package that depends only on Vesper.Core can
// resolve `<`/`>`/`<=`/`>=` on `int` without referencing Vesper.Comparison.
module IntComparison =
    val inline (<): x: int -> y: int -> bool
    val inline (>): x: int -> y: int -> bool
    val inline (<=): x: int -> y: int -> bool
    val inline (>=): x: int -> y: int -> bool
