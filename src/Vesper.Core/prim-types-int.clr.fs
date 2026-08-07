namespace Vesper

#nowarn "42"

// The arithmetic mnemonics compute on the int32 evaluation stack, so every sub-int32
// width truncates its result back through a `conv.*` — without it `200uy + 100uy` answers
// 300 rather than wrapping to 44uy. The bitwise mnemonics stay in range and need none.

type sbyte =
    (# "System.SByte" #)
    with
        static member inline (+)(x: sbyte, y: sbyte) : sbyte = (# "conv.i1" (# "add" x y : int #) : sbyte #)
        static member inline (-)(x: sbyte, y: sbyte) : sbyte = (# "conv.i1" (# "sub" x y : int #) : sbyte #)
        static member inline ( * )(x: sbyte, y: sbyte) : sbyte = (# "conv.i1" (# "mul" x y : int #) : sbyte #)
        static member inline (/)(x: sbyte, y: sbyte) : sbyte = (# "conv.i1" (# "div" x y : int #) : sbyte #)
        static member inline (%)(x: sbyte, y: sbyte) : sbyte = (# "conv.i1" (# "rem" x y : int #) : sbyte #)
        static member inline (~+)(value: sbyte) : sbyte = value
        static member inline (~-)(n: sbyte) : sbyte = (# "conv.i1" (# "neg" n : int #) : sbyte #)
        static member inline (&&&)(x: sbyte, y: sbyte) : sbyte = (# "and" x y : sbyte #)
        static member inline (|||)(x: sbyte, y: sbyte) : sbyte = (# "or" x y : sbyte #)
        static member inline (^^^)(x: sbyte, y: sbyte) : sbyte = (# "xor" x y : sbyte #)
        static member inline (~~~)(value: sbyte) : sbyte = (# "not" value : sbyte #)
        static member inline (<<<)(value: sbyte, shift: int) : sbyte = (# "shl" value shift : sbyte #)
        static member inline (>>>)(value: sbyte, shift: int) : sbyte = (# "shr" value shift : sbyte #)
    end

type byte =
    (# "System.Byte" #)
    with
        static member inline (+)(x: byte, y: byte) : byte = (# "conv.u1" (# "add" x y : int #) : byte #)
        static member inline (-)(x: byte, y: byte) : byte = (# "conv.u1" (# "sub" x y : int #) : byte #)
        static member inline ( * )(x: byte, y: byte) : byte = (# "conv.u1" (# "mul" x y : int #) : byte #)
        static member inline (/)(x: byte, y: byte) : byte = (# "conv.u1" (# "div.un" x y : int #) : byte #)
        static member inline (%)(x: byte, y: byte) : byte = (# "conv.u1" (# "rem.un" x y : int #) : byte #)
        // No `(~-)` at an unsigned width, but `(~+)` is the identity and so has an answer.
        static member inline (~+)(value: byte) : byte = value
        static member inline (&&&)(x: byte, y: byte) : byte = (# "and" x y : byte #)
        static member inline (|||)(x: byte, y: byte) : byte = (# "or" x y : byte #)
        static member inline (^^^)(x: byte, y: byte) : byte = (# "xor" x y : byte #)
        static member inline (~~~)(value: byte) : byte = (# "not" value : byte #)
        static member inline (<<<)(value: byte, shift: int) : byte = (# "shl" value shift : byte #)
        static member inline (>>>)(value: byte, shift: int) : byte = (# "shr.un" value shift : byte #)
    end

type int8 = sbyte
type uint8 = byte

type int16 =
    (# "System.Int16" #)
    with
        static member inline (+)(x: int16, y: int16) : int16 = (# "conv.i2" (# "add" x y : int #) : int16 #)
        static member inline (-)(x: int16, y: int16) : int16 = (# "conv.i2" (# "sub" x y : int #) : int16 #)
        static member inline ( * )(x: int16, y: int16) : int16 = (# "conv.i2" (# "mul" x y : int #) : int16 #)
        static member inline (/)(x: int16, y: int16) : int16 = (# "conv.i2" (# "div" x y : int #) : int16 #)
        static member inline (%)(x: int16, y: int16) : int16 = (# "conv.i2" (# "rem" x y : int #) : int16 #)
        static member inline (~+)(value: int16) : int16 = value
        static member inline (~-)(n: int16) : int16 = (# "conv.i2" (# "neg" n : int #) : int16 #)
        static member inline (&&&)(x: int16, y: int16) : int16 = (# "and" x y : int16 #)
        static member inline (|||)(x: int16, y: int16) : int16 = (# "or" x y : int16 #)
        static member inline (^^^)(x: int16, y: int16) : int16 = (# "xor" x y : int16 #)
        static member inline (~~~)(value: int16) : int16 = (# "not" value : int16 #)
        static member inline (<<<)(value: int16, shift: int) : int16 = (# "shl" value shift : int16 #)
        static member inline (>>>)(value: int16, shift: int) : int16 = (# "shr" value shift : int16 #)
    end

type uint16 =
    (# "System.UInt16" #)
    with
        static member inline (+)(x: uint16, y: uint16) : uint16 = (# "conv.u2" (# "add" x y : int #) : uint16 #)
        static member inline (-)(x: uint16, y: uint16) : uint16 = (# "conv.u2" (# "sub" x y : int #) : uint16 #)
        static member inline ( * )(x: uint16, y: uint16) : uint16 = (# "conv.u2" (# "mul" x y : int #) : uint16 #)
        static member inline (/)(x: uint16, y: uint16) : uint16 = (# "conv.u2" (# "div.un" x y : int #) : uint16 #)
        static member inline (%)(x: uint16, y: uint16) : uint16 = (# "conv.u2" (# "rem.un" x y : int #) : uint16 #)
        static member inline (~+)(value: uint16) : uint16 = value
        static member inline (&&&)(x: uint16, y: uint16) : uint16 = (# "and" x y : uint16 #)
        static member inline (|||)(x: uint16, y: uint16) : uint16 = (# "or" x y : uint16 #)
        static member inline (^^^)(x: uint16, y: uint16) : uint16 = (# "xor" x y : uint16 #)
        static member inline (~~~)(value: uint16) : uint16 = (# "not" value : uint16 #)
        static member inline (<<<)(value: uint16, shift: int) : uint16 = (# "shl" value shift : uint16 #)
        static member inline (>>>)(value: uint16, shift: int) : uint16 = (# "shr.un" value shift : uint16 #)
    end

type int32 = int

type uint32 =
    (# "System.UInt32" #)
    with
        static member inline (+)(x: uint32, y: uint32) : uint32 = (# "add" x y : uint32 #)
        static member inline (-)(x: uint32, y: uint32) : uint32 = (# "sub" x y : uint32 #)
        static member inline ( * )(x: uint32, y: uint32) : uint32 = (# "mul" x y : uint32 #)
        static member inline (/)(x: uint32, y: uint32) : uint32 = (# "div.un" x y : uint32 #)
        static member inline (%)(x: uint32, y: uint32) : uint32 = (# "rem.un" x y : uint32 #)
        static member inline (~+)(value: uint32) : uint32 = value
        static member inline (&&&)(x: uint32, y: uint32) : uint32 = (# "and" x y : uint32 #)
        static member inline (|||)(x: uint32, y: uint32) : uint32 = (# "or" x y : uint32 #)
        static member inline (^^^)(x: uint32, y: uint32) : uint32 = (# "xor" x y : uint32 #)
        static member inline (~~~)(value: uint32) : uint32 = (# "not" value : uint32 #)
        static member inline (<<<)(value: uint32, shift: int) : uint32 = (# "shl" value shift : uint32 #)
        static member inline (>>>)(value: uint32, shift: int) : uint32 = (# "shr.un" value shift : uint32 #)
    end

type int64 =
    (# "System.Int64" #)
    with
        static member inline (+)(x: int64, y: int64) : int64 = (# "add" x y : int64 #)
        static member inline (-)(x: int64, y: int64) : int64 = (# "sub" x y : int64 #)
        static member inline ( * )(x: int64, y: int64) : int64 = (# "mul" x y : int64 #)
        static member inline (/)(x: int64, y: int64) : int64 = (# "div" x y : int64 #)
        static member inline (%)(x: int64, y: int64) : int64 = (# "rem" x y : int64 #)
        static member inline (~+)(value: int64) : int64 = value
        static member inline (~-)(n: int64) : int64 = (# "neg" n : int64 #)
        static member inline (&&&)(x: int64, y: int64) : int64 = (# "and" x y : int64 #)
        static member inline (|||)(x: int64, y: int64) : int64 = (# "or" x y : int64 #)
        static member inline (^^^)(x: int64, y: int64) : int64 = (# "xor" x y : int64 #)
        static member inline (~~~)(value: int64) : int64 = (# "not" value : int64 #)
        static member inline (<<<)(value: int64, shift: int) : int64 = (# "shl" value shift : int64 #)
        static member inline (>>>)(value: int64, shift: int) : int64 = (# "shr" value shift : int64 #)
    end

type uint64 =
    (# "System.UInt64" #)
    with
        static member inline (+)(x: uint64, y: uint64) : uint64 = (# "add" x y : uint64 #)
        static member inline (-)(x: uint64, y: uint64) : uint64 = (# "sub" x y : uint64 #)
        static member inline ( * )(x: uint64, y: uint64) : uint64 = (# "mul" x y : uint64 #)
        static member inline (/)(x: uint64, y: uint64) : uint64 = (# "div.un" x y : uint64 #)
        static member inline (%)(x: uint64, y: uint64) : uint64 = (# "rem.un" x y : uint64 #)
        static member inline (~+)(value: uint64) : uint64 = value
        static member inline (&&&)(x: uint64, y: uint64) : uint64 = (# "and" x y : uint64 #)
        static member inline (|||)(x: uint64, y: uint64) : uint64 = (# "or" x y : uint64 #)
        static member inline (^^^)(x: uint64, y: uint64) : uint64 = (# "xor" x y : uint64 #)
        static member inline (~~~)(value: uint64) : uint64 = (# "not" value : uint64 #)
        static member inline (<<<)(value: uint64, shift: int) : uint64 = (# "shl" value shift : uint64 #)
        static member inline (>>>)(value: uint64, shift: int) : uint64 = (# "shr.un" value shift : uint64 #)
    end

type uint = uint32
