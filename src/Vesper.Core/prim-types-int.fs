namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic bindings for the integer primitives. Retarget
// a primitive here in one line; same-underlying aliases mirror the .fsi.

type sbyte = (# "System.SByte" #)
type byte = (# "System.Byte" #)
type int8 = sbyte
type uint8 = byte
type int16 = (# "System.Int16" #)
type uint16 = (# "System.UInt16" #)
type int32 = int
type uint32 = (# "System.UInt32" #)
type int64 = (# "System.Int64" #)
type uint64 = (# "System.UInt64" #)
type uint = uint32
