namespace Vesper

[<AutoOpen>]
module ComparisonRuntime =

    /// Structural three-way comparison — the runtime entry the JS `< > <= >=` bases call
    /// for a non-primitive operand. Returns -1 / 0 / 1; `a < b` emits
    /// `structuralCompare(a, b) < 0` against `Vesper.Comparison.mjs`.
    val structuralCompare: x: 'T -> y: 'T -> int when 'T: comparison
