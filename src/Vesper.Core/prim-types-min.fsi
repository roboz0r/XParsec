namespace Vesper

/// <summary>An intrinsic 32-bit signed integer provided by the target.</summary>
///
/// <category>Basic Types</category>
type int = extern with

    interface equatable<int>
    interface comparable<int>

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
and bool = extern with

    interface equatable<bool>
    interface comparable<bool>

/// <summary>The type 'unit', which has only one value "()".</summary>
///
/// <remarks>Comparable as well as equatable: a one-element set is totally ordered, so
/// <c>compare () ()</c> is <c>0</c> and <c>Set&lt;unit&gt;</c> is legal if degenerate.</remarks>
///
/// <category>Basic Types</category>
and unit = extern with

    interface equatable<unit>
    interface comparable<unit>

/// <summary>The equality capability — anchors `[<CustomEquality>]` conformance.
/// On the CLI it is <see cref="T:System.IEquatable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
and equatable<'T> = extern interface with
    abstract member Equals: 'T -> bool

/// <summary>The comparison capability — anchors `[<CustomComparison>]`
/// conformance. On the CLI it is <see cref="T:System.IComparable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
and comparable<'T> = extern interface with
    abstract member CompareTo: 'T -> int

/// <summary>The disposal capability — anchors `use` (and `for … in` finally). On
/// the CLI it is <see cref="T:System.IDisposable"/>.</summary>
///
/// <category>Language Capabilities</category>
type disposable = extern interface with
    abstract member Dispose: unit -> unit

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
