namespace Vesper

[<AutoOpen>]
module ArithmeticOperators =

        /// <summary>Overloaded unary negation.</summary>
        ///
        /// <param name="n">The value to negate.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example-tbd></example-tbd>
        /// 
        val inline (~-): n: ^T -> ^T when ^T: (static member ( ~- ): ^T -> ^T) and default ^T: int
        
        /// <summary>Overloaded prefix-plus operator</summary>
        ///
        /// <param name="value">The input value.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example-tbd></example-tbd>
        /// 
        val inline (~+): value: ^T -> ^T when ^T: (static member (~+): ^T -> ^T) and default ^T: int

        /// <summary>Overloaded addition operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="addition-example-1">
        /// <code lang="fsharp">
        /// 2 + 2 //  Evaluates to 4
        /// "Hello " + "World" // Evaluates to "Hello World"
        /// </code>
        /// </example>
        /// 
        val inline (+): x: ^T1 -> y: ^T2 -> ^T3  when (^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3) and default ^T2: ^T3 and default ^T3: ^T1 and default ^T3: ^T2 and default ^T1: ^T3 and default ^T1: ^T2 and default ^T1: int
        
        /// <summary>Overloaded subtraction operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="subtraction-example-1">
        /// <code lang="fsharp">
        /// 10 - 2 //  Evaluates to 8
        /// </code>
        /// </example>
        val inline (-): x: ^T1 -> y: ^T2 -> ^T3  when (^T1 or ^T2): (static member (-): ^T1 * ^T2 -> ^T3) and default ^T2: ^T3 and default ^T3: ^T1 and default ^T3: ^T2 and default ^T1: ^T3 and default ^T1: ^T2 and default ^T1: int
        
        /// <summary>Overloaded multiplication operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="multiplication-example-1">
        /// <code lang="fsharp">
        /// 8 * 6 //  Evaluates to 48
        /// </code>
        /// </example>
        val inline (*): x: ^T1 -> y: ^T2 -> ^T3 when (^T1 or ^T2): (static member (*): ^T1 * ^T2    -> ^T3) and default ^T2: ^T3 and default ^T3: ^T1 and default ^T3: ^T2 and default ^T1: ^T3 and default ^T1: ^T2 and default ^T1: int
        
        /// <summary>Overloaded division operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="division-example-1">
        /// <code lang="fsharp">
        /// 16 / 2 //  Evaluates to 8
        /// </code>
        /// </example>
        val inline (/): x: ^T1 -> y: ^T2 -> ^T3  when (^T1 or ^T2): (static member (/): ^T1 * ^T2 -> ^T3) and default ^T2: ^T3 and default ^T3: ^T1 and default ^T3: ^T2 and default ^T1: ^T3 and default ^T1: ^T2 and default ^T1: int
        
        /// <summary>Overloaded modulo operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="modulo-example-1">
        /// <code lang="fsharp">
        /// 29 % 5 //  Evaluates to 4
        /// </code>
        /// </example>
        val inline (%): x: ^T1 -> y: ^T2 -> ^T3 when (^T1 or ^T2): (static member (%): ^T1 * ^T2 -> ^T3) and default ^T2: ^T3 and default ^T3: ^T1 and default ^T3: ^T2 and default ^T1: ^T3 and default ^T1: ^T2 and default ^T1: int
        
[<AutoOpen>]
module BitwiseOperators =

        /// <summary>Overloaded bitwise-AND operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="bitwise-and-example-1">
        /// <code lang="fsharp">
        /// let a = 13       // 00000000000000000000000000001101
        /// let b = 11       // 00000000000000000000000000001011
        /// let c = a &amp;&amp;&amp; b  // 00000000000000000000000000001001
        /// </code>
        /// Evaluates to 9
        /// </example>
        val inline (&&&): x: ^T -> y: ^T -> ^T when ^T: (static member (&&&): ^T * ^T -> ^T) and default ^T: int
        
        /// <summary>Overloaded bitwise-OR operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="bitwise-or-example-1">
        /// <code lang="fsharp">
        /// let a = 13       // 00000000000000000000000000001101
        /// let b = 11       // 00000000000000000000000000001011
        /// let c = a ||| b  // 00000000000000000000000000001111
        /// </code>
        /// Evaluates to 15
        /// </example>
        val inline (|||): x: ^T -> y: ^T -> ^T when ^T: (static member (|||): ^T * ^T -> ^T) and default ^T: int
        
        /// <summary>Overloaded bitwise-XOR operator</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="bitwise-xor-example-1">
        /// <code lang="fsharp">
        /// let a = 13       // 00000000000000000000000000001101
        /// let b = 11       // 00000000000000000000000000001011
        /// let c = a ^^^ b  // 00000000000000000000000000000110
        /// </code>
        /// Evaluates to 6
        /// </example>
        val inline (^^^): x: ^T -> y: ^T -> ^T when ^T: (static member (^^^): ^T * ^T -> ^T) and default ^T: int
        
        /// <summary>Overloaded byte-shift left operator by a specified number of bits</summary>
        ///
        /// <param name="value">The input value.</param>
        /// <param name="shift">The amount to shift.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="left-shift-example-1">
        /// <code lang="fsharp">
        /// let a = 13       // 00000000000000000000000000001101
        /// let c = a &lt;&lt;&lt; 4   // 00000000000000000000000011010000
        /// </code>
        /// Evaluates to 208
        /// </example>
        val inline (<<<): value: ^T -> shift: int32 -> ^T when ^T : (static member (<<<) : ^T * int32 -> ^T) and default ^T : int
        
        /// <summary>Overloaded byte-shift right operator by a specified number of bits</summary>
        ///
        /// <param name="value">The input value.</param>
        /// <param name="shift">The amount to shift.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="right-shift-example-1">
        /// <code lang="fsharp">
        /// let a = 206      // 00000000000000000000000011010000
        /// let c1 = a &gt;&gt;&gt; 2  // 00000000000000000000000000110100
        /// // Evaluates to 51
        /// let c2 = a &gt;&gt;&gt; 6  // 00000000000000000000000000000011
        /// Evaluates to 3
        /// </code>
        /// </example>
        val inline (>>>): value: ^T -> shift: int32 -> ^T when ^T: (static member (>>>): ^T * int32 -> ^T) and default ^T: int
        
        /// <summary>Overloaded bitwise-NOT operator</summary>
        ///
        /// <param name="value">The input value.</param>
        ///
        /// <returns>The result of the operation.</returns>
        /// 
        /// <example id="bitwise-not-example-1">
        /// <code lang="fsharp">
        /// let byte1 = 60uy  //  00111100
        /// let byte2 = ~~~b1 //  11000011
        /// </code>
        /// Evaluates to 195
        /// </example>
        /// 
        val inline (~~~): value: ^T -> ^T when ^T: (static member (~~~): ^T -> ^T) and default ^T: int

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
        val inline (<): x: 'T -> y: 'T -> bool when 'T : comparison
        
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
        val inline (>=): x: 'T -> y: 'T -> bool when 'T : comparison
        
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
        
        /// <summary>Structural equality</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the comparison.</returns>
        /// 
        /// <example id="compare-equal-example">
        /// <code lang="fsharp">
        ///  5 = 5              // Evaluates to true
        ///  5 = 6              // Evaluates to false
        ///  [1; 2] = [1; 2]    // Evaluates to true
        ///  (1, 5) = (1, 6)    // Evaluates to false
        /// </code>
        /// </example>
        /// 
        val inline (=): x: 'T -> y: 'T -> bool when 'T: equality
        
        /// <summary>Structural inequality</summary>
        ///
        /// <param name="x">The first parameter.</param>
        /// <param name="y">The second parameter.</param>
        ///
        /// <returns>The result of the comparison.</returns>
        /// 
        /// <example id="compare-not-equal-example">
        /// <code lang="fsharp">
        ///  5 &lt;&gt; 5              // Evaluates to false
        ///  5 &lt;&gt; 6              // Evaluates to true
        ///  [1; 2] &lt;&gt; [1; 2]    // Evaluates to false
        /// </code>
        /// </example>
        /// 
        val inline (<>): x:'T -> y:'T -> bool when 'T : equality
