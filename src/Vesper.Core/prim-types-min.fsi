namespace Vesper

// Minimal types necessary to represent a turing complete ML.

#nowarn "42" // This construct is deprecated: it is only for use in the F# library
open System

/// <summary>An abbreviation for the CLI type <see cref="T:System.Int32"/>.</summary>
///
/// <category>Basic Types</category>
type int = Int32

/// <summary>An abbreviation for the CLI type <see cref="T:System.Boolean"/>.</summary>
///
/// <category>Basic Types</category>
type bool = Boolean

/// <summary>Single dimensional, zero-based arrays, written <c>int array</c>, <c>string array</c> etc.</summary>
///
/// <remarks>Use the values in the <c>Array</c> module to manipulate values
/// of this type, or the notation <c>arr.[x]</c> to get/set array
/// values.</remarks>
///
/// <category>Basic Types</category>
/// <exclude />
type 'T ``[]`` = (# "!0[]" #)

/// <summary>Single dimensional, zero-based arrays, written <c>int array</c>, <c>string array</c> etc.</summary>
///
/// <remarks>Use the values in the <see cref="T:Microsoft.FSharp.Collections.ArrayModule" /> module to manipulate values
/// of this type, or the notation <c>arr.[x]</c> to get/set array
/// values.</remarks>
///
/// <category>Basic Types</category>
type 'T array = 'T[]

type Fun<'A, 'B> =
    abstract member Invoke : arg:'A -> 'B

/// <namespacedoc><summary>
///   Basic definitions of operators, options, functions, results, choices, attributes and plain text formatting.
/// </summary></namespacedoc>
///
/// <summary>The type 'unit', which has only one value "()".</summary>
///
/// <category>Basic Types</category>
/// <exclude />
type Unit = ValueTuple
    
/// <summary>The type 'unit', which has only one value "()".</summary>
///
/// <category index="1">Basic Types</category>
and unit = Unit
