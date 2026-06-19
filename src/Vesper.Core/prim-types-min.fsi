namespace Vesper

// Minimal types necessary to represent a turing complete ML.
//
// Contract (`.fsi`) side: each primitive is declared `extern` — "the target
// provides this; there is no Vesper representation here." The set of `extern`
// declarations is this target's primitive capability set; the impl (`.fs`)
// side pairs each with a `(# "..." #)` intrinsic carrying the representation.

/// <summary>An intrinsic 32-bit signed integer provided by the target.</summary>
///
/// <category>Basic Types</category>
type int = extern

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
/// one <c>Invoke(a,b)</c> with no intermediate <c>Fun&lt;'B,'C&gt;</c>. A distinct
/// nominal type, NOT a subtype of <c>Fun&lt;'A, Fun&lt;'B,'C&gt;&gt;</c>;
/// flat&lt;-&gt;curried adaptation goes through <c>curryFun</c> / <c>flatten</c> in
/// core-types.</summary>
///
/// <category>Basic Types</category>
type Fun2<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C
