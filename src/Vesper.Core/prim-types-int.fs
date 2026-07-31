namespace Vesper

#nowarn "42"

// The CIL bitwise mnemonics are width-agnostic on the evaluation stack: `and`/`or`/`xor`
// of two in-range operands stays in range, and `not` on a sub-int32 width keeps the
// meaningful low bits — so none of them needs a `conv.*` back. Right shift is the one
// split: `shr` sign-extends (correct for the signed widths), `shr.un` zero-fills.

type sbyte =
    (# "System.SByte" #)
    with
        static member (&&&)(x: sbyte, y: sbyte) : sbyte = (# "and" x y : sbyte #)
        static member (|||)(x: sbyte, y: sbyte) : sbyte = (# "or" x y : sbyte #)
        static member (^^^)(x: sbyte, y: sbyte) : sbyte = (# "xor" x y : sbyte #)
        static member (~~~)(value: sbyte) : sbyte = (# "not" value : sbyte #)
        static member (<<<)(value: sbyte, shift: int) : sbyte = (# "shl" value shift : sbyte #)
        static member (>>>)(value: sbyte, shift: int) : sbyte = (# "shr" value shift : sbyte #)
    end

type byte =
    (# "System.Byte" #)
    with
        static member (&&&)(x: byte, y: byte) : byte = (# "and" x y : byte #)
        static member (|||)(x: byte, y: byte) : byte = (# "or" x y : byte #)
        static member (^^^)(x: byte, y: byte) : byte = (# "xor" x y : byte #)
        static member (~~~)(value: byte) : byte = (# "not" value : byte #)
        static member (<<<)(value: byte, shift: int) : byte = (# "shl" value shift : byte #)
        static member (>>>)(value: byte, shift: int) : byte = (# "shr.un" value shift : byte #)
    end

type int8 = sbyte
type uint8 = byte

type int16 =
    (# "System.Int16" #)
    with
        static member (&&&)(x: int16, y: int16) : int16 = (# "and" x y : int16 #)
        static member (|||)(x: int16, y: int16) : int16 = (# "or" x y : int16 #)
        static member (^^^)(x: int16, y: int16) : int16 = (# "xor" x y : int16 #)
        static member (~~~)(value: int16) : int16 = (# "not" value : int16 #)
        static member (<<<)(value: int16, shift: int) : int16 = (# "shl" value shift : int16 #)
        static member (>>>)(value: int16, shift: int) : int16 = (# "shr" value shift : int16 #)
    end

type uint16 =
    (# "System.UInt16" #)
    with
        static member (&&&)(x: uint16, y: uint16) : uint16 = (# "and" x y : uint16 #)
        static member (|||)(x: uint16, y: uint16) : uint16 = (# "or" x y : uint16 #)
        static member (^^^)(x: uint16, y: uint16) : uint16 = (# "xor" x y : uint16 #)
        static member (~~~)(value: uint16) : uint16 = (# "not" value : uint16 #)
        static member (<<<)(value: uint16, shift: int) : uint16 = (# "shl" value shift : uint16 #)
        static member (>>>)(value: uint16, shift: int) : uint16 = (# "shr.un" value shift : uint16 #)
    end

type int32 = int

type uint32 =
    (# "System.UInt32" #)
    with
        static member (&&&)(x: uint32, y: uint32) : uint32 = (# "and" x y : uint32 #)
        static member (|||)(x: uint32, y: uint32) : uint32 = (# "or" x y : uint32 #)
        static member (^^^)(x: uint32, y: uint32) : uint32 = (# "xor" x y : uint32 #)
        static member (~~~)(value: uint32) : uint32 = (# "not" value : uint32 #)
        static member (<<<)(value: uint32, shift: int) : uint32 = (# "shl" value shift : uint32 #)
        static member (>>>)(value: uint32, shift: int) : uint32 = (# "shr.un" value shift : uint32 #)
    end

type int64 =
    (# "System.Int64" #)
    with
        static member (&&&)(x: int64, y: int64) : int64 = (# "and" x y : int64 #)
        static member (|||)(x: int64, y: int64) : int64 = (# "or" x y : int64 #)
        static member (^^^)(x: int64, y: int64) : int64 = (# "xor" x y : int64 #)
        static member (~~~)(value: int64) : int64 = (# "not" value : int64 #)
        static member (<<<)(value: int64, shift: int) : int64 = (# "shl" value shift : int64 #)
        static member (>>>)(value: int64, shift: int) : int64 = (# "shr" value shift : int64 #)
    end

type uint64 =
    (# "System.UInt64" #)
    with
        static member (&&&)(x: uint64, y: uint64) : uint64 = (# "and" x y : uint64 #)
        static member (|||)(x: uint64, y: uint64) : uint64 = (# "or" x y : uint64 #)
        static member (^^^)(x: uint64, y: uint64) : uint64 = (# "xor" x y : uint64 #)
        static member (~~~)(value: uint64) : uint64 = (# "not" value : uint64 #)
        static member (<<<)(value: uint64, shift: int) : uint64 = (# "shl" value shift : uint64 #)
        static member (>>>)(value: uint64, shift: int) : uint64 = (# "shr.un" value shift : uint64 #)
    end

type uint = uint32
