namespace Vesper

#nowarn "42"

// The bitwise family per width, under the SAME masks the arithmetic family uses
// (`ops-platform.js.fs`): `& 0xFF` byte, `<< 24 >> 24` sbyte, `& 0xFFFF` uint16,
// `<< 16 >> 16` int16, `>>> 0` uint32, `BigInt.asIntN/asUintN(64, …)` the 64-bit pair.
//
// A mask appears only where the operator can leave the width. JS `&`/`|`/`^` coerce to
// int32 and AND/OR/XOR of in-range operands stays in range, so those need none; `~` and
// `<<` DO leave it, and at the unsigned widths so does the sign (`~5uy` is -6 on the JS
// wire and 250 after the mask). Right shift takes the zero-filling `>>>` at the unsigned
// widths and the sign-extending `>>` at the signed ones, mirroring CIL `shr.un`/`shr`.

type sbyte =
    (# "number" #)
    with
        static member (&&&)(x: sbyte, y: sbyte) : sbyte = (# "$0 & $1" x y : sbyte #)
        static member (|||)(x: sbyte, y: sbyte) : sbyte = (# "$0 | $1" x y : sbyte #)
        static member (^^^)(x: sbyte, y: sbyte) : sbyte = (# "$0 ^ $1" x y : sbyte #)
        static member (~~~)(value: sbyte) : sbyte = (# "~$0" value : sbyte #)
        static member (<<<)(value: sbyte, shift: int) : sbyte = (# "($0 << $1) << 24 >> 24" value shift : sbyte #)
        static member (>>>)(value: sbyte, shift: int) : sbyte = (# "$0 >> $1" value shift : sbyte #)
    end

type byte =
    (# "number" #)
    with
        static member (&&&)(x: byte, y: byte) : byte = (# "$0 & $1" x y : byte #)
        static member (|||)(x: byte, y: byte) : byte = (# "$0 | $1" x y : byte #)
        static member (^^^)(x: byte, y: byte) : byte = (# "$0 ^ $1" x y : byte #)
        static member (~~~)(value: byte) : byte = (# "(~$0) & 0xFF" value : byte #)
        static member (<<<)(value: byte, shift: int) : byte = (# "($0 << $1) & 0xFF" value shift : byte #)
        static member (>>>)(value: byte, shift: int) : byte = (# "$0 >>> $1" value shift : byte #)
    end

type int16 =
    (# "number" #)
    with
        static member (&&&)(x: int16, y: int16) : int16 = (# "$0 & $1" x y : int16 #)
        static member (|||)(x: int16, y: int16) : int16 = (# "$0 | $1" x y : int16 #)
        static member (^^^)(x: int16, y: int16) : int16 = (# "$0 ^ $1" x y : int16 #)
        static member (~~~)(value: int16) : int16 = (# "~$0" value : int16 #)
        static member (<<<)(value: int16, shift: int) : int16 = (# "($0 << $1) << 16 >> 16" value shift : int16 #)
        static member (>>>)(value: int16, shift: int) : int16 = (# "$0 >> $1" value shift : int16 #)
    end

type uint16 =
    (# "number" #)
    with
        static member (&&&)(x: uint16, y: uint16) : uint16 = (# "$0 & $1" x y : uint16 #)
        static member (|||)(x: uint16, y: uint16) : uint16 = (# "$0 | $1" x y : uint16 #)
        static member (^^^)(x: uint16, y: uint16) : uint16 = (# "$0 ^ $1" x y : uint16 #)
        static member (~~~)(value: uint16) : uint16 = (# "(~$0) & 0xFFFF" value : uint16 #)
        static member (<<<)(value: uint16, shift: int) : uint16 = (# "($0 << $1) & 0xFFFF" value shift : uint16 #)
        static member (>>>)(value: uint16, shift: int) : uint16 = (# "$0 >>> $1" value shift : uint16 #)
    end

type uint32 =
    (# "number" #)
    with
        // Every one of these reads back through `>>> 0`: JS bitwise answers SIGNED int32,
        // so the top-bit-set results are negative without it.
        static member (&&&)(x: uint32, y: uint32) : uint32 = (# "($0 & $1) >>> 0" x y : uint32 #)
        static member (|||)(x: uint32, y: uint32) : uint32 = (# "($0 | $1) >>> 0" x y : uint32 #)
        static member (^^^)(x: uint32, y: uint32) : uint32 = (# "($0 ^ $1) >>> 0" x y : uint32 #)
        static member (~~~)(value: uint32) : uint32 = (# "(~$0) >>> 0" value : uint32 #)
        static member (<<<)(value: uint32, shift: int) : uint32 = (# "($0 << $1) >>> 0" value shift : uint32 #)
        // `>>>` is already the unsigned read; no second coercion.
        static member (>>>)(value: uint32, shift: int) : uint32 = (# "$0 >>> $1" value shift : uint32 #)
    end

type int64 =
    (# "bigint" #)
    with
        static member (&&&)(x: int64, y: int64) : int64 = (# "$0 & $1" x y : int64 #)
        static member (|||)(x: int64, y: int64) : int64 = (# "$0 | $1" x y : int64 #)
        static member (^^^)(x: int64, y: int64) : int64 = (# "$0 ^ $1" x y : int64 #)
        static member (~~~)(value: int64) : int64 = (# "~$0" value : int64 #)
        // A BigInt shift needs a BigInt shift amount, so the `int` operand converts.
        static member (<<<)(value: int64, shift: int) : int64 =
            (# "BigInt.asIntN(64, $0 << BigInt($1))" value shift : int64 #)

        static member (>>>)(value: int64, shift: int) : int64 =
            (# "BigInt.asIntN(64, $0 >> BigInt($1))" value shift : int64 #)
    end

type uint64 =
    (# "bigint" #)
    with
        static member (&&&)(x: uint64, y: uint64) : uint64 = (# "$0 & $1" x y : uint64 #)
        static member (|||)(x: uint64, y: uint64) : uint64 = (# "$0 | $1" x y : uint64 #)
        static member (^^^)(x: uint64, y: uint64) : uint64 = (# "$0 ^ $1" x y : uint64 #)
        static member (~~~)(value: uint64) : uint64 = (# "BigInt.asUintN(64, ~$0)" value : uint64 #)

        static member (<<<)(value: uint64, shift: int) : uint64 =
            (# "BigInt.asUintN(64, $0 << BigInt($1))" value shift : uint64 #)

        // Non-negative by construction, so `>>` zero-fills and stays in range.
        static member (>>>)(value: uint64, shift: int) : uint64 = (# "$0 >> BigInt($1)" value shift : uint64 #)
    end
