namespace Vesper

// Vesper.Comparison — the ordering family (operators-plan.md O1/O2).
//
// The four bare ordering operators relocated here from Vesper.Core's
// `ops-platform.fsi` so the entire ordering surface lives in one package, while
// equality (`=` / `<>` / `hash`) stays in Vesper.Core. Their `when 'T: comparison`
// constraints are retained and enforced (operators-plan.md O4) exactly as the
// equality family's `when 'T: equality` is.
//
// `compare` / `min` / `max` (and the `Comparer<'T>.Default` dispatch / opt-in
// structural `CompareTo` generation) land in slice C-Cmp1 alongside the
// default-contract-closure wiring (operators-plan.md O3/O6) — not in this pass,
// which is scoped to the Vesper.Core equality slice.

// The non-inline structural-comparison runtime entry the JS aggregate bases call for a
// non-primitive operand — the ordering analogue of Vesper.Core's
// `StructuralRuntime.structuralEquals`. JS-body-only (`Vesper.Comparison.mjs`); the CLR
// target uses its own `Comparer<^T>.Default` base (comparison.fs) and never references
// this — a `val` whose body is supplied per-target, the established Vesper.Core pattern.
[<AutoOpen>]
module ComparisonRuntime =

    /// Structural three-way comparison of two values — the runtime entry the JS
    /// `< > <= >=` aggregate bases call for a non-primitive operand. Returns a sign
    /// (-1 / 0 / 1) the bases test against 0. JS body: `Vesper.Comparison.mjs`'s curried
    /// `structuralCompare` (a shape-keyed walk; consistent with Vesper.Core's
    /// `structuralEquals` by construction, so equal values compare 0).
    val structuralCompare: x: 'T -> y: 'T -> int when 'T: comparison

[<AutoOpen>]
module ComparisonOperators =

    /// <summary>Structural less-than comparison</summary>
    ///
    /// <param name="x">The first parameter.</param>
    /// <param name="y">The second parameter.</param>
    ///
    /// <returns>The result of the comparison.</returns>
    ///
    /// <example id="compare-less-than-example">
    /// <code lang="fsharp">
    /// 1 &lt; 5               // Evaluates to true
    /// 5 &lt; 5               // Evaluates to false
    /// (1, "a") &lt; (1, "z") // Evaluates to true
    /// </code>
    /// </example>
    ///
    val inline (<): x: 'T -> y: 'T -> bool when 'T: comparison

    /// <summary>Structural greater-than</summary>
    ///
    /// <param name="x">The first parameter.</param>
    /// <param name="y">The second parameter.</param>
    ///
    /// <returns>The result of the comparison.</returns>
    ///
    /// <example id="compare-greater-than-example">
    /// <code lang="fsharp">
    ///  5 &gt; 1               // Evaluates to true
    ///  5 &gt; 5               // Evaluates to false
    ///  (1, "a") &gt; (1, "z") // Evaluates to false
    /// </code>
    /// </example>
    ///
    val inline (>): x: 'T -> y: 'T -> bool when 'T: comparison

    /// <summary>Structural greater-than-or-equal</summary>
    ///
    /// <param name="x">The first parameter.</param>
    /// <param name="y">The second parameter.</param>
    ///
    /// <returns>The result of the comparison.</returns>
    ///
    /// <example id="compare-greater-than-or-equal-example">
    /// <code lang="fsharp">
    ///  5 >= 1              // Evaluates to true
    ///  5 >= 5              // Evaluates to true
    ///  [1; 5] >= [1; 6]    // Evaluates to false
    /// </code>
    /// </example>
    ///
    val inline (>=): x: 'T -> y: 'T -> bool when 'T: comparison

    /// <summary>Structural less-than-or-equal comparison</summary>
    ///
    /// <param name="x">The first parameter.</param>
    /// <param name="y">The second parameter.</param>
    ///
    /// <returns>The result of the comparison.</returns>
    ///
    /// <example id="compare-less-than-or-equal-example">
    /// <code lang="fsharp">
    ///  5 &lt;= 1              // Evaluates to false
    ///  5 &lt;= 5              // Evaluates to true
    ///  [1; 5] &lt;= [1; 6]    // Evaluates to true
    /// </code>
    /// </example>
    ///
    val inline (<=): x: 'T -> y: 'T -> bool when 'T: comparison
