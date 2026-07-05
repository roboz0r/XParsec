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

// The JS-only structural runtime entries (`structuralEquals` / `structuralHash`) that
// once sat here moved to `ops-platform-runtime.js.fsi` (manifest `files-js`): they have
// no CLR `.fs` body (CLR's `=` / `<>` / `hash` use `EqualityComparer` inline and never
// reference them), so a CLR-visible `val` was an over-declaration. The JS `=` / `<>` /
// `hash` base arms still delegate to them, resolved from that JS-only contract.

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

        /// <summary>Generate a hash value for the given value.</summary>
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
        /// <example id="not-example">
        /// <code lang="fsharp">
        /// not true    // Evaluates to false
        /// not false   // Evaluates to true
        /// </code>
        /// </example>
        ///
        val inline not: value: bool -> bool

        /// <summary>Ignore the passed value — discard it and yield <c>unit</c>
        /// (mirroring FSharp.Core's <c>ignore</c>).</summary>
        ///
        /// <param name="value">The value to ignore.</param>
        ///
        /// <returns><c>unit</c>.</returns>
        ///
        /// <example id="ignore-example">
        /// <code lang="fsharp">
        /// ignore 55555   //  Evaluates to ()
        /// </code>
        /// </example>
        ///
        val inline ignore: value: 'T -> unit

        /// <summary>Test whether the given reference value is <c>null</c>
        /// (mirroring FSharp.Core's <c>isNull</c>).</summary>
        ///
        /// <param name="value">The value to test.</param>
        ///
        /// <returns><c>true</c> when the value is <c>null</c>, otherwise
        /// <c>false</c>.</returns>
        ///
        /// <example id="isNull-example">
        /// <code lang="fsharp">
        /// isNull null        //  Evaluates to true
        /// isNull "Not null"  //  Evaluates to false
        /// </code>
        /// </example>
        ///
        val inline isNull: value: 'T -> bool when 'T: null

        /// <summary>Box a value to <c>obj</c> (mirroring FSharp.Core's
        /// <c>box</c>).</summary>
        ///
        /// <param name="value">The value to box.</param>
        ///
        /// <returns>The value boxed as <c>obj</c>.</returns>
        ///
        val inline box: value: 'T -> obj

        /// <summary>Convert a value to <c>uint32</c> (mirroring FSharp.Core's
        /// <c>ToUInt32</c>).</summary>
        ///
        /// <param name="value">The input value.</param>
        ///
        /// <returns>The converted <c>uint32</c>.</returns>
        ///
        val inline uint32: value: ^T -> uint32

        /// <summary>Convert a value to <c>uint32</c> — the <c>uint</c> abbreviation
        /// of <c>uint32</c> (mirroring FSharp.Core's <c>ToUInt</c>).</summary>
        ///
        /// <param name="value">The input value.</param>
        ///
        /// <returns>The converted <c>uint32</c>.</returns>
        val inline uint: value: ^T -> uint32

        /// <summary>Convert a value to <c>int32</c> — the signed sibling of
        /// <c>uint32</c> (mirroring FSharp.Core's <c>ToInt32</c>).</summary>
        ///
        /// <param name="value">The input value.</param>
        ///
        /// <returns>The converted <c>int32</c>.</returns>
        ///
        val inline int32: value: ^T -> int32

        /// <summary>Convert a value to <c>int32</c> — the <c>int</c> abbreviation
        /// of <c>int32</c> (mirroring FSharp.Core's <c>ToInt</c>).</summary>
        ///
        /// <param name="value">The input value.</param>
        ///
        /// <returns>The converted <c>int32</c>.</returns>
        val inline int: value: ^T -> int

        /// <summary>Indexed read of a single-dimensional, zero-based array — the
        /// lowering target the front end desugars <c>arr.[i]</c> to (mirroring F#'s
        /// <c>IntrinsicFunctions.GetArray</c>).</summary>
        ///
        /// <param name="array">The array.</param>
        /// <param name="index">The index.</param>
        ///
        /// <returns>The element at the given index.</returns>
        ///
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
        val inline SetArray: array: 'T[] -> index: int -> value: 'T -> unit

        /// <summary>Length of a single-dimensional, zero-based array — the lowering
        /// target the front end desugars <c>arr.Length</c> to.</summary>
        ///
        /// <param name="array">The array.</param>
        ///
        /// <returns>The number of elements.</returns>
        ///
        val inline GetArrayLength: array: 'T[] -> int

        /// <summary>Raise the given exception.</summary>
        ///
        /// <param name="exn">The exception to raise.</param>
        ///
        /// <returns>Never returns normally; the result type unifies with any context.</returns>
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
        /// <remarks>Argument order follows FSharp.Core: name first, message second;
        /// the BCL ctor takes <c>(message, paramName)</c>.</remarks>
        ///
        /// <example id="invalidArg-example">
        /// <code lang="fsharp">
        /// invalidArg "x" "must be positive"   // throws ArgumentException
        /// </code>
        /// </example>
        ///
        val inline invalidArg: argumentName: string -> message: string -> 'T

/// String indexing intrinsics — the desugaring target for `s.[i]` on a
/// `string` receiver, mirroring `Operators.GetArray` for `arr.[i]`. The front
/// end routes here only when the BCL `get_Chars` accessor does NOT resolve (the
/// JS target, whose `string` carries no BCL metadata); the CLR target keeps its
/// metadata `get_Chars` path, so this intrinsic is effectively JS-only.
[<AutoOpen>]
module StringIntrinsics =

    /// <summary>Indexed read of a UTF-16 code unit — the lowering target the front
    /// end desugars <c>s.[i]</c> to on a <c>string</c> receiver (the `string`
    /// analogue of <c>GetArray</c>).</summary>
    val inline GetString: s: string -> index: int -> char

/// Index-signature intrinsics — the desugaring target for `x.[k]` / `x.[k] <- v` on a
/// receiver whose EXTERNAL type carries a TS index signature (`{ [k: K]: V }`),
/// mirroring `GetArray`/`SetArray` for arrays and `GetString` for strings. JS-target
/// concept (a JS object has no `get_Item` method — bracket access is the only form);
/// like `GetString`, the CLR body is contract-only.
[<AutoOpen>]
module IndexIntrinsics =

    /// <summary>Indexed read of an index-signature object — the lowering target the
    /// front end desugars <c>x.[k]</c> to on such a receiver.</summary>
    val inline GetIndex: target: 'T -> key: 'K -> 'V

    /// <summary>Indexed write of an index-signature object — the lowering target the
    /// front end desugars <c>x.[k] &lt;- value</c> to on such a receiver.</summary>
    val inline SetIndex: target: 'T -> key: 'K -> value: 'V -> unit
