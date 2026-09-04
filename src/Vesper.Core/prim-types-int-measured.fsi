namespace Vesper

// The measured carriers: each numeric primitive is claimed a SECOND time, at arity 1, with a
// MEASURE-kinded parameter. FSharp.Core marks each one `[<MeasureAnnotatedAbbreviation>]`;
// here the parameter's kind carries that fact.

// FSharp.Core/prim-types.fsi:1104
/// <summary>The type of 32-bit signed integer numbers, annotated with a unit of measure. The unit
/// of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Int32"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type int<[<Measure>] 'Measure> = int

// FSharp.Core/prim-types.fsi:1113
/// <summary>The type of 8-bit signed integer numbers, annotated with a unit of measure. The unit
/// of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.SByte"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type sbyte<[<Measure>] 'Measure> = sbyte

// FSharp.Core/prim-types.fsi:1122
/// <summary>The type of 16-bit signed integer numbers, annotated with a unit of measure. The unit
/// of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Int16"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type int16<[<Measure>] 'Measure> = int16

// FSharp.Core/prim-types.fsi:1131
/// <summary>The type of 64-bit signed integer numbers, annotated with a unit of measure. The unit
/// of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Int64"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type int64<[<Measure>] 'Measure> = int64

// FSharp.Core/prim-types.fsi:1149, where the canonical 32-bit unsigned spelling is `uint`
/// <summary>The type of 32-bit unsigned integer numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.UInt32"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type uint32<[<Measure>] 'Measure> = uint32

// FSharp.Core/prim-types.fsi:1158
/// <summary>The type of 8-bit unsigned integer numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.Byte"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type byte<[<Measure>] 'Measure> = byte

// FSharp.Core/prim-types.fsi:1167
/// <summary>The type of 16-bit unsigned integer numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.UInt16"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type uint16<[<Measure>] 'Measure> = uint16

// FSharp.Core/prim-types.fsi:1176
/// <summary>The type of 64-bit unsigned integer numbers, annotated with a unit of measure.
/// The unit of measure is erased in compiled code and when values of this type
/// are analyzed using reflection. The type is representationally equivalent to
/// <see cref="T:System.UInt64"/>.</summary>
///
/// <category>Basic Types with Units of Measure</category>
type uint64<[<Measure>] 'Measure> = uint64
