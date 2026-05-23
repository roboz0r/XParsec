namespace Vesper

[<AutoOpen>]
module LogicalOperators =
    /// <summary>Binary 'and'. When used as a binary operator the right hand value is evaluated only on demand</summary>
    ///
    /// <param name="e1">The first value.</param>
    /// <param name="e2">The second value.</param>
    ///
    /// <returns>The result of the operation.</returns>
    val (&&): e1: bool -> e2: bool -> bool
    
    /// <summary>Binary 'or'. When used as a binary operator the right hand value is evaluated only on demand</summary>
    ///
    /// <param name="e1">The first value.</param>
    /// <param name="e2">The second value.</param>
    ///
    /// <returns>The result of the operation.</returns>
    val (||): e1: bool -> e2: bool -> bool



[<AutoOpen>]
module CompositionOperators =

        /// <summary>Compose two functions, the function on the left being applied first</summary>
        ///
        /// <param name="func1">The first function to apply.</param>
        /// <param name="func2">The second function to apply.</param>
        ///
        /// <returns>The composition of the input functions.</returns>
        /// 
        /// <example id="compose-example">
        /// <code lang="fsharp">
        /// let addOne x = x + 1
        /// let doubleIt x = x * 2
        /// let addThenDouble = addOne >> doubleIt
        /// addThenDouble 3  // Evaluates to 8
        /// </code>
        /// </example>
        /// 
        val inline (>>): func1: ('T1 -> 'T2) -> func2: ('T2 -> 'T3) -> ('T1 -> 'T3) 
        
        /// <summary>Compose two functions, the function on the right being applied first</summary>
        ///
        /// <param name="func2">The second function to apply.</param>
        /// <param name="func1">The first function to apply.</param>
        ///
        /// <returns>The composition of the input functions.</returns>
        /// 
        /// <example id="right-compose-example">
        /// <code lang="fsharp">
        /// let addOne x = x + 1
        /// let doubleIt x = x * 2
        /// let doubleThenAdd = addOne &lt;&lt; doubleIt
        /// doubleThenAdd 3  
        /// </code>
        /// </example>
        /// 
        val inline (<<): func2: ('T2 -> 'T3) -> func1: ('T1 -> 'T2) -> ('T1 -> 'T3) 
        
        /// <summary>Apply a function to a value, the value being on the left, the function on the right</summary>
        ///
        /// <param name="arg">The argument.</param>
        /// <param name="func">The function.</param>
        ///
        /// <returns>The function result.</returns>
        /// 
        /// <example id="pipeline-example">
        /// <code lang="fsharp">
        /// let doubleIt x = x * 2
        /// 3 |> doubleIt  //  Evaluates to 6
        /// </code>
        /// </example>
        /// 
        val inline (|>): arg: 'T1 -> func: ('T1 -> 'U) -> 'U

        /// <summary>Apply a function to two values, the values being a pair on the left, the function on the right</summary>
        ///
        /// <param name="arg1">The first argument.</param>
        /// <param name="arg2">The second argument.</param>
        /// <param name="func">The function.</param>
        ///
        /// <returns>The function result.</returns>
        /// 
        /// <example id="double-pipeline-example">
        /// <code lang="fsharp">
        /// let sum x y = x + y
        /// (3, 4) ||> sum   // Evaluates to 7
        /// </code>
        /// </example>
        /// 
        val inline (||>): arg1: 'T1 * arg2: 'T2 -> func: ('T1 -> 'T2 -> 'U) -> 'U

        /// <summary>Apply a function to three values, the values being a triple on the left, the function on the right</summary>
        ///
        /// <param name="arg1">The first argument.</param>
        /// <param name="arg2">The second argument.</param>
        /// <param name="arg3">The third argument.</param>
        /// <param name="func">The function.</param>
        ///
        /// <returns>The function result.</returns>
        /// 
        /// <example id="triple-pipeline-example">
        /// <code lang="fsharp">
        /// let sum3 x y z = x + y + z
        /// (3, 4, 5) |||> sum3   // Evaluates to 12
        /// </code>
        /// </example>
        /// 
        val inline (|||>): arg1: 'T1 * arg2: 'T2 * arg3: 'T3 -> func: ('T1 -> 'T2 -> 'T3 -> 'U) -> 'U
        
        /// <summary>Apply a function to a value, the value being on the right, the function on the left</summary>
        ///
        /// <param name="func">The function.</param>
        /// <param name="arg1">The argument.</param>
        ///
        /// <returns>The function result.</returns>
        /// 
        /// <example id="left-pipeline-example">
        /// <code lang="fsharp">
        /// let doubleIt x = x * 2
        /// doubleIt &lt;| 3  //  Evaluates to 6
        /// </code>
        /// </example>
        /// 
        val inline (<|): func: ('T -> 'U) -> arg1: 'T -> 'U

        /// <summary>Apply a function to two values, the values being a pair on the right, the function on the left</summary>
        ///
        /// <param name="func">The function.</param>
        /// <param name="arg1">The first argument.</param>
        /// <param name="arg2">The second argument.</param>
        ///
        /// <returns>The function result.</returns>
        /// 
        /// <example id="left-double-pipeline-example">
        /// <code lang="fsharp">
        /// let sum x y = x + y
        /// sum &lt;|| (3, 4)   // Evaluates to 7
        /// </code>
        /// </example>
        /// 
        val inline (<||): func: ('T1 -> 'T2 -> 'U) -> arg1: 'T1 * arg2: 'T2 -> 'U

        /// <summary>Apply a function to three values, the values being a triple on the right, the function on the left</summary>
        ///
        /// <param name="func">The function.</param>
        /// <param name="arg1">The first argument.</param>
        /// <param name="arg2">The second argument.</param>
        /// <param name="arg3">The third argument.</param>
        ///
        /// <returns>The function result.</returns>
        /// 
        /// <example id="left-triple-pipeline-example">
        /// <code lang="fsharp">
        /// let sum3 x y z = x + y + z
        /// sum3 &lt;||| (3, 4, 5)   // Evaluates to 12
        /// </code>
        /// </example>
        /// 
        val inline (<|||): func: ('T1 -> 'T2 -> 'T3 -> 'U) -> arg1: 'T1 * arg2: 'T2 * arg3: 'T3 -> 'U