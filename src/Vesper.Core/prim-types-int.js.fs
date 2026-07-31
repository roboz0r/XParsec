namespace Vesper

#nowarn "42"

// One mask per width, applied to whichever JS operator computes the operation: `& 0xFF`
// byte, `<< 24 >> 24` sbyte, `& 0xFFFF` uint16, `<< 16 >> 16` int16, `>>> 0` uint32,
// `BigInt.asIntN/asUintN(64, …)` the 64-bit pair. JS has one number type, so the mask IS
// the width — `10uy - 20uy` is -10 on the wire and 246 after it.
//
// Three things the mask alone does not settle:
//
// MULTIPLICATION at 32 bits takes `Math.imul`, which computes the product mod 2^32
// directly. A masked `$0 * $1` cannot: a full 32×32 product reaches ~2^64 and loses its
// low bits — the ones the mask keeps — past 2^53. The narrow widths need no such care;
// their products are exact as doubles. The 32-bit bit pattern is the same signed or
// unsigned, so uint32 differs only in reading it back through `>>> 0`.
//
// DIVISION is true division in JS (`10uy / 3uy` is 3.333…, and the fraction survives into
// the next operation, where no report-site `int (…)` can launder it). Each integral mask
// is a bitwise coercion, and those truncate toward zero — F#'s rule. A zero divisor would
// give `Infinity`, and `Infinity | 0` is a silent 0, so every integral divisor passes
// through `checkedDivisor`: it throws, returns its argument so the mask still wraps it,
// and reads the operand once.
//
// The BITWISE family below needs a mask only where the operator can leave the width. JS
// `&`/`|`/`^` coerce to int32 and stay in range; `~` and `<<` do leave it, and at the
// unsigned widths so does the sign (`~5uy` is -6 on the wire and 250 after the mask).
// Right shift takes the zero-filling `>>>` at the unsigned widths and the sign-extending
// `>>` at the signed ones, mirroring CIL `shr.un`/`shr`.

type sbyte =
    (# "number" #)
    with
        static member (+)(x: sbyte, y: sbyte) : sbyte = (# "($0 + $1) << 24 >> 24" x y : sbyte #)
        static member (-)(x: sbyte, y: sbyte) : sbyte = (# "($0 - $1) << 24 >> 24" x y : sbyte #)
        static member ( * )(x: sbyte, y: sbyte) : sbyte = (# "($0 * $1) << 24 >> 24" x y : sbyte #)

        static member (/)(x: sbyte, y: sbyte) : sbyte =
            (# "($0 / $1) << 24 >> 24" x (checkedDivisor y) : sbyte #)

        static member (%)(x: sbyte, y: sbyte) : sbyte =
            (# "($0 % $1) << 24 >> 24" x (checkedDivisor y) : sbyte #)

        static member (~-)(n: sbyte) : sbyte = (# "(-$0) << 24 >> 24" n : sbyte #)
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
        static member (+)(x: byte, y: byte) : byte = (# "($0 + $1) & 0xFF" x y : byte #)
        static member (-)(x: byte, y: byte) : byte = (# "($0 - $1) & 0xFF" x y : byte #)
        static member ( * )(x: byte, y: byte) : byte = (# "($0 * $1) & 0xFF" x y : byte #)
        static member (/)(x: byte, y: byte) : byte = (# "($0 / $1) & 0xFF" x (checkedDivisor y) : byte #)
        static member (%)(x: byte, y: byte) : byte = (# "($0 % $1) & 0xFF" x (checkedDivisor y) : byte #)
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
        static member (+)(x: int16, y: int16) : int16 = (# "($0 + $1) << 16 >> 16" x y : int16 #)
        static member (-)(x: int16, y: int16) : int16 = (# "($0 - $1) << 16 >> 16" x y : int16 #)
        static member ( * )(x: int16, y: int16) : int16 = (# "($0 * $1) << 16 >> 16" x y : int16 #)

        static member (/)(x: int16, y: int16) : int16 =
            (# "($0 / $1) << 16 >> 16" x (checkedDivisor y) : int16 #)

        static member (%)(x: int16, y: int16) : int16 =
            (# "($0 % $1) << 16 >> 16" x (checkedDivisor y) : int16 #)

        static member (~-)(n: int16) : int16 = (# "(-$0) << 16 >> 16" n : int16 #)
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
        static member (+)(x: uint16, y: uint16) : uint16 = (# "($0 + $1) & 0xFFFF" x y : uint16 #)
        static member (-)(x: uint16, y: uint16) : uint16 = (# "($0 - $1) & 0xFFFF" x y : uint16 #)
        static member ( * )(x: uint16, y: uint16) : uint16 = (# "($0 * $1) & 0xFFFF" x y : uint16 #)

        static member (/)(x: uint16, y: uint16) : uint16 =
            (# "($0 / $1) & 0xFFFF" x (checkedDivisor y) : uint16 #)

        static member (%)(x: uint16, y: uint16) : uint16 =
            (# "($0 % $1) & 0xFFFF" x (checkedDivisor y) : uint16 #)

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
        static member (+)(x: uint32, y: uint32) : uint32 = (# "($0 + $1) >>> 0" x y : uint32 #)
        static member (-)(x: uint32, y: uint32) : uint32 = (# "($0 - $1) >>> 0" x y : uint32 #)
        static member ( * )(x: uint32, y: uint32) : uint32 = (# "Math.imul($0, $1) >>> 0" x y : uint32 #)
        static member (/)(x: uint32, y: uint32) : uint32 = (# "($0 / $1) >>> 0" x (checkedDivisor y) : uint32 #)
        static member (%)(x: uint32, y: uint32) : uint32 = (# "($0 % $1) >>> 0" x (checkedDivisor y) : uint32 #)
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
        static member (+)(x: int64, y: int64) : int64 = (# "BigInt.asIntN(64, $0 + $1)" x y : int64 #)
        static member (-)(x: int64, y: int64) : int64 = (# "BigInt.asIntN(64, $0 - $1)" x y : int64 #)
        static member ( * )(x: int64, y: int64) : int64 = (# "BigInt.asIntN(64, $0 * $1)" x y : int64 #)

        // BigInt `/` already truncates toward zero; only the 64-bit wrap is left.
        static member (/)(x: int64, y: int64) : int64 =
            (# "BigInt.asIntN(64, $0 / $1)" x (checkedDivisor y) : int64 #)

        static member (%)(x: int64, y: int64) : int64 =
            (# "BigInt.asIntN(64, $0 % $1)" x (checkedDivisor y) : int64 #)

        static member (~-)(n: int64) : int64 = (# "BigInt.asIntN(64, -$0)" n : int64 #)
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
        static member (+)(x: uint64, y: uint64) : uint64 = (# "BigInt.asUintN(64, $0 + $1)" x y : uint64 #)
        static member (-)(x: uint64, y: uint64) : uint64 = (# "BigInt.asUintN(64, $0 - $1)" x y : uint64 #)
        static member ( * )(x: uint64, y: uint64) : uint64 = (# "BigInt.asUintN(64, $0 * $1)" x y : uint64 #)

        static member (/)(x: uint64, y: uint64) : uint64 =
            (# "BigInt.asUintN(64, $0 / $1)" x (checkedDivisor y) : uint64 #)

        static member (%)(x: uint64, y: uint64) : uint64 =
            (# "BigInt.asUintN(64, $0 % $1)" x (checkedDivisor y) : uint64 #)

        static member (&&&)(x: uint64, y: uint64) : uint64 = (# "$0 & $1" x y : uint64 #)
        static member (|||)(x: uint64, y: uint64) : uint64 = (# "$0 | $1" x y : uint64 #)
        static member (^^^)(x: uint64, y: uint64) : uint64 = (# "$0 ^ $1" x y : uint64 #)
        static member (~~~)(value: uint64) : uint64 = (# "BigInt.asUintN(64, ~$0)" value : uint64 #)

        static member (<<<)(value: uint64, shift: int) : uint64 =
            (# "BigInt.asUintN(64, $0 << BigInt($1))" value shift : uint64 #)

        // Non-negative by construction, so `>>` zero-fills and stays in range.
        static member (>>>)(value: uint64, shift: int) : uint64 = (# "$0 >> BigInt($1)" value shift : uint64 #)
    end
