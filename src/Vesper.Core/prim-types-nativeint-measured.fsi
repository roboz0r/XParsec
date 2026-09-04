namespace Vesper

// The arity-1 claim of each machine-sized integer carrier; see `prim-types-int-measured.fsi`
// for what a measured carrier is.

// FSharp.Core/prim-types.fsi:1140
/// <summary>The type of machine-sized signed integer numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.IntPtr"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type nativeint<[<Measure>] 'Measure> = nativeint

// FSharp.Core/prim-types.fsi:1185
/// <summary>The type of machine-sized unsigned integer numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.UIntPtr"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type unativeint<[<Measure>] 'Measure> = unativeint
