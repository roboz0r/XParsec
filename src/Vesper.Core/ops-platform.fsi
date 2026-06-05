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

// Equality (`=` / `<>`) and `hash` stay in Vesper.Core. The four ordering
// operators (`<` / `>` / `<=` / `>=`) moved to `Vesper.Comparison`
// (operators-plan.md O2); `compare` / `min` / `max` join them there in C-Cmp1.
[<AutoOpen>]
module EqualityOperators =
        
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

[<AutoOpen>]
module Operators =

        /// <summary>Generate a hash value for the given value. Part of the equality
        /// family (operators-plan.md O6): no runtime member — codegen dispatches it
        /// to a primitive hash or the structural hash of a generated type.</summary>
        ///
        /// <param name="obj">The input value.</param>
        ///
        /// <returns>The computed hash value.</returns>
        ///
        /// <example id="hash-example">
        /// <code lang="fsharp">
        /// hash 1   // Evaluates to a hash code
        /// </code>
        /// </example>
        ///
        val inline hash: obj: 'T -> int when 'T: equality

        /// <summary>Negate a boolean value.</summary>
        ///
        /// <param name="value">The value to negate.</param>
        ///
        /// <returns><c>true</c> if the input is <c>false</c>, otherwise <c>false</c>.</returns>
        ///
        /// <remarks>Inline — lowers to <c>ceq(value, false)</c>, the same
        /// <c>(# "ceq" … false : bool #)</c> shape the <c>(&lt;&gt;)</c> base uses to
        /// negate a comparison. A plain identifier (not operator-named), so it
        /// resolves through the ambient open scope like <c>hash</c> /
        /// <c>failwith</c>; the cross-package inline-body splice
        /// (<c>SymbolProviders.inlineBodies</c>) delivers the body to each use site,
        /// so this pins no Vesper runtime dependency.</remarks>
        ///
        /// <example id="not-example">
        /// <code lang="fsharp">
        /// not true    // Evaluates to false
        /// not false   // Evaluates to true
        /// </code>
        /// </example>
        ///
        val inline not: value: bool -> bool

        /// <summary>Indexed read of a single-dimensional, zero-based array — the
        /// lowering target the front end desugars <c>arr.[i]</c> to (mirroring F#'s
        /// <c>IntrinsicFunctions.GetArray</c>).</summary>
        ///
        /// <param name="array">The array.</param>
        /// <param name="index">The index.</param>
        ///
        /// <returns>The element at the given index.</returns>
        ///
        /// <remarks>Inline; the <c>(# "ldelem.any" … #)</c> body splices at each use
        /// site so the element load is emitted inline (no call). The platform
        /// mnemonic lives in <c>ops-platform.fs</c>, not the target-agnostic
        /// Semantic Analysis layer.</remarks>
        val inline GetArray: array: 'T[] -> index: int -> 'T

        /// <summary>Indexed write of a single-dimensional, zero-based array — the
        /// lowering target the front end desugars <c>arr.[i] &lt;- value</c> to
        /// (mirroring F#'s <c>IntrinsicFunctions.SetArray</c>).</summary>
        ///
        /// <param name="array">The array.</param>
        /// <param name="index">The index.</param>
        /// <param name="value">The value to store.</param>
        ///
        /// <returns>Unit; the store has no result.</returns>
        ///
        /// <remarks>Inline; the <c>(# "stelem.any" … #)</c> body splices at each use
        /// site so the element store is emitted inline (no call) — the write mirror
        /// of <c>GetArray</c>. The platform mnemonic lives in
        /// <c>ops-platform.fs</c>, not the target-agnostic Semantic Analysis
        /// layer.</remarks>
        val inline SetArray: array: 'T[] -> index: int -> value: 'T -> unit

        /// <summary>Length of a single-dimensional, zero-based array — the lowering
        /// target the front end desugars <c>arr.Length</c> to.</summary>
        ///
        /// <param name="array">The array.</param>
        ///
        /// <returns>The number of elements.</returns>
        ///
        /// <remarks>Inline; the <c>(# "ldlen" … #)</c> body splices at each use site
        /// so the length read is emitted inline (codegen narrows the native int with
        /// <c>conv.i4</c>, matching F#'s <c>ldlen; conv.i4</c>). The platform mnemonic
        /// lives in <c>ops-platform.fs</c>, not Semantic Analysis.</remarks>
        val inline GetArrayLength: array: 'T[] -> int

        /// <summary>Raise the given exception.</summary>
        ///
        /// <param name="exn">The exception to raise.</param>
        ///
        /// <returns>Never returns normally; the result type unifies with any context.</returns>
        ///
        /// <remarks>Inline IL — lowers to <c>throw</c>. Same shape as
        /// FSharp.Core's <c>raise</c> (<c>prim-types.fs:547</c>): the parameter
        /// is a typar bounded by <c>:> exn</c>, so only an exception type can be
        /// raised. The contract extractor captures the coercion constraint and
        /// the unifier enforces it via <c>subsumes</c>, which reconciles
        /// <c>exn</c> with the BCL <c>System.Exception</c> through the
        /// intrinsic-repr binding in <c>prim-types-exn.fs</c> and walks external
        /// <c>inherit</c> chains (so <c>InvalidOperationException :&gt; exn</c>
        /// holds). The inline-body splice machinery
        /// (<c>SymbolProviders.inlineBodies</c>) reads the body from
        /// <c>ops-platform.fs</c> and emits it at each use site, so this pins no
        /// Vesper runtime dependency.</remarks>
        ///
        /// <example id="raise-example">
        /// <code lang="fsharp">
        /// raise (System.Exception "boom")   // throws System.Exception "boom"
        /// </code>
        /// </example>
        ///
        val inline raise: exn: 'TException -> 'T when 'TException :> exn

        /// <summary>Throw a <see cref="T:System.Exception"/> with the given message.</summary>
        ///
        /// <param name="message">The exception message.</param>
        ///
        /// <returns>Never returns normally; the result type unifies with any context.</returns>
        ///
        /// <remarks>Inline — desugars to <c>raise (new System.Exception(message))</c>;
        /// the cross-package inline-body splice
        /// (<c>SymbolProviders.inlineBodies</c>) delivers the body to each use
        /// site, where it lowers through the standard
        /// <c>TExpr.New</c> + <c>TExpr.ILIntrinsic "throw"</c> paths. No
        /// dedicated codegen recipe — the previous <c>Emit.isFailwith</c>
        /// name-suffix probe is gone.</remarks>
        ///
        /// <example id="failwith-example">
        /// <code lang="fsharp">
        /// failwith "boom"   // raises System.Exception "boom"
        /// </code>
        /// </example>
        ///
        val inline failwith: message: string -> 'T

        /// <summary>Raise a <see cref="T:System.ArgumentException"/> naming the offending argument.</summary>
        ///
        /// <param name="argumentName">The name of the argument that was invalid.</param>
        /// <param name="message">The exception message.</param>
        ///
        /// <returns>Never returns normally; the result type unifies with any context.</returns>
        ///
        /// <remarks>Inline — desugars to
        /// <c>raise (new System.ArgumentException(message, argumentName))</c>; the
        /// cross-package inline-body splice delivers the body to each use site,
        /// where the two-string BCL ctor is selected by the external-ctor
        /// overload pick in <c>Infer.inferNew</c>. Argument order follows
        /// FSharp.Core: the user-facing argument name comes first, the message
        /// second, but the BCL ctor takes <c>(message, paramName)</c>.</remarks>
        ///
        /// <example id="invalidArg-example">
        /// <code lang="fsharp">
        /// invalidArg "x" "must be positive"   // throws ArgumentException
        /// </code>
        /// </example>
        ///
        val inline invalidArg: argumentName: string -> message: string -> 'T
