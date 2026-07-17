namespace Vesper

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
