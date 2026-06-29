namespace Vesper

// Vesper.Comparison — the ordering family.
//
// The four bare ordering operators relocated here from Vesper.Core's
// `ops-platform.fsi` so the entire ordering surface lives in one package, while
// equality (`=` / `<>` / `hash`) stays in Vesper.Core. Their `when 'T: comparison`
// constraints are retained and enforced exactly as the equality family's
// `when 'T: equality` is.
//
// `compare` / `min` / `max` (and the `Comparer<'T>.Default` dispatch / opt-in
// structural `CompareTo` generation) land in slice C-Cmp1 alongside the
// default-contract-closure wiring — not in this pass, which is scoped to the
// Vesper.Core equality slice.

// The JS-only structural-comparison runtime entry (`structuralCompare`) that once sat
// here moved to `comparison-runtime.js.fsi` (manifest `files-js`): it has no CLR `.fs`
// body (CLR `< > <= >=` use `Comparer<^T>.Default` inline and never reference it), so a
// CLR-visible `val` was an over-declaration. The JS `< > <= >=` base arms still delegate
// to it, resolved from that JS-only contract.

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
