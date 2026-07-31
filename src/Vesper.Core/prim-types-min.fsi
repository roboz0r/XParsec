namespace Vesper

/// <summary>An intrinsic 32-bit signed integer provided by the target.</summary>
///
/// <category>Basic Types</category>
type int = extern with

    /// <summary>The witness the overloaded <c>(+)</c> trait constraint dispatches to.
    /// Declared here, on the type, so the unifier resolves it by ordinary member lookup
    /// rather than synthesising a candidate from an operator-name table. The body is the
    /// target's own in the paired <c>.fs</c>: it is spliced at the use site, never
    /// emitted.</summary>
    static member (+): x: int * y: int -> int

/// <summary>An intrinsic boolean provided by the target.</summary>
///
/// <category>Basic Types</category>
type bool = extern

/// <summary>The type 'unit', which has only one value "()".</summary>
///
/// <category>Basic Types</category>
type unit = extern

/// <summary>Single dimensional, zero-based arrays, written <c>int array</c>, <c>string array</c> etc.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[]`` = extern

/// <summary>Single dimensional, zero-based arrays, written <c>int array</c>, <c>string array</c> etc.</summary>
///
/// <category>Basic Types</category>
type 'T array = 'T[]

/// <summary>The function type: a value with a single abstract <c>Invoke</c> method.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B> =
    abstract member Invoke: arg: 'A -> 'B

/// <summary>The flat arity-2 function type: a saturated 2-arg call dispatches in
/// one <c>Invoke(a,b)</c> with no intermediate <c>Fun&lt;'B,'C&gt;</c>. Overloads
/// <c>Fun&lt;'A,'B&gt;</c> by generic arity (CLR <c>Fun`3</c> vs <c>Fun`2</c>); NOT a
/// subtype of <c>Fun&lt;'A, Fun&lt;'B,'C&gt;&gt;</c>, flat&lt;-&gt;curried adaptation
/// goes through <c>curryFun</c> / <c>flatten</c> in core-types.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C

/// <summary>The flat arity-3 function type: a saturated 3-arg call dispatches in
/// one <c>Invoke(a,b,c)</c> with no intermediate <c>Fun&lt;'B,'C&gt;</c> /
/// <c>Fun&lt;'C,'D&gt;</c>. Overloads <c>Fun&lt;'A,'B&gt;</c> by generic arity (CLR
/// <c>Fun`4</c> vs <c>Fun`2</c>); NOT a subtype of the curried nesting,
/// flat&lt;-&gt;curried adaptation goes through <c>curryFun</c> / <c>flatten</c> in
/// core-types.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B, 'C, 'D> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C -> 'D

/// <summary>The flat arity-4 function type: a saturated 4-arg call dispatches in
/// one <c>Invoke(a,b,c,d)</c> with no intermediate <c>Fun&lt;'B,'C&gt;</c> /
/// <c>Fun&lt;'C,'D&gt;</c> / <c>Fun&lt;'D,'E&gt;</c>. Overloads <c>Fun&lt;'A,'B&gt;</c>
/// by generic arity (CLR <c>Fun`5</c> vs <c>Fun`2</c>); NOT a subtype of the curried
/// nesting, flat&lt;-&gt;curried adaptation goes through <c>curryFun</c> /
/// <c>flatten</c> in core-types.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B, 'C, 'D, 'E> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C * d: 'D -> 'E
