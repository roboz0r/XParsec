namespace Vesper

// The primitive subset of the ordering family, so a package depending only on Vesper.Core
// can resolve `<` / `>` / `<=` / `>=` on `int`. The polymorphic
// `'T -> 'T -> bool when 'T: comparison` family lives in Vesper.Comparison.
module IntComparison =
    val inline (<): x: int -> y: int -> bool
    val inline (>): x: int -> y: int -> bool
    val inline (<=): x: int -> y: int -> bool
    val inline (>=): x: int -> y: int -> bool
