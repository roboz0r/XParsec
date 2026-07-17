namespace Vesper

[<AutoOpen>]
module ComparisonRuntime =

    /// Structural three-way comparison of two values — the runtime entry the JS
    /// `< > <= >=` aggregate bases call for a non-primitive operand. Returns a sign
    /// (-1 / 0 / 1) the bases test against 0. JS body: `Vesper.Comparison.mjs`'s curried
    /// `structuralCompare` (a shape-keyed walk; consistent with Vesper.Core's
    /// `structuralEquals` by construction, so equal values compare 0).
    val structuralCompare: x: 'T -> y: 'T -> int when 'T: comparison
