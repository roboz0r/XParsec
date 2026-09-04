namespace Vesper

// The arity-1 claim of the decimal carrier; see `prim-types-int-measured.fsi` for what a
// measured carrier is.

// FSharp.Core/prim-types.fsi:1095
/// <summary>The type of decimal numbers, annotated with a unit of measure. The unit
/// of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Decimal"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type decimal<[<Measure>] 'Measure> = decimal
