namespace Vesper

// The arity-1 claim of each floating-point carrier; see `prim-types-int-measured.fsi` for
// what a measured carrier is.

// FSharp.Core/prim-types.fsi:1076
/// <summary>The type of double-precision floating point numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Double"/>.</summary>
///
/// <category index="6">Basic Types with Units of Measure</category>
type float<[<Measure>] 'Measure> = float

// FSharp.Core/prim-types.fsi:1086
/// <summary>The type of single-precision floating point numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Single"/>.
/// </summary>
///
/// <category>Basic Types with Units of Measure</category>
type float32<[<Measure>] 'Measure> = float32
