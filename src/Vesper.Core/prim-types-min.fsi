namespace Vesper

/// <summary>An intrinsic 32-bit signed integer provided by the target.</summary>
///
/// <category>Basic Types</category>
type int = extern with

    /// <summary>Each operator's body is the target's own, in the paired <c>.fs</c>:
    /// spliced at the use site, never emitted.</summary>
    static member inline (+): x: int * y: int -> int

    static member inline (-): x: int * y: int -> int

    static member inline ( * ): x: int * y: int -> int

    static member inline (/): x: int * y: int -> int

    static member inline (%): x: int * y: int -> int

    /// <summary>Prefix plus — the identity, declared at every numeric width.</summary>
    static member inline (~+): value: int -> int

    /// <summary>Unary negation, declared at the SIGNED widths only: a negated unsigned
    /// value has no answer its own width can hold.</summary>
    static member inline (~-): n: int -> int

    static member inline (&&&): x: int * y: int -> int

    static member inline (|||): x: int * y: int -> int

    static member inline (^^^): x: int * y: int -> int

    static member inline (~~~): value: int -> int

    /// <summary>The shift amount is <c>int32</c> at every width, never the shifted type.</summary>
    static member inline (<<<): value: int * shift: int -> int

    static member inline (>>>): value: int * shift: int -> int

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

/// <summary>The flat arity-2 function type: a saturated 2-arg call dispatches in one
/// <c>Invoke(a, b)</c>. It overloads <c>Fun&lt;'A,'B&gt;</c> by generic arity and is NOT a
/// subtype of <c>Fun&lt;'A, Fun&lt;'B,'C&gt;&gt;</c> — adapting between them is a conversion.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C

/// <summary>The flat arity-3 function type: a saturated 3-arg call dispatches in one
/// <c>Invoke(a, b, c)</c>.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B, 'C, 'D> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C -> 'D

/// <summary>The flat arity-4 function type: a saturated 4-arg call dispatches in one
/// <c>Invoke(a, b, c, d)</c>.</summary>
///
/// <category>Basic Types</category>
type Fun<'A, 'B, 'C, 'D, 'E> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C * d: 'D -> 'E
