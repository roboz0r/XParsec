namespace Vesper

#nowarn "42"

open Vesper.ArithmeticRuntime

// JS has one number type, so the trailing mask IS the width: `10uy - 20uy` is -10 on the
// wire and 246 after `& 0xFF`. A bitwise member carries a mask only where the operator can
// leave the width — `~5uy` is -6 before `& 0xFF` and 250 after.

type sbyte =
    (# "number" #)
    with
        static member inline (+)(x: sbyte, y: sbyte) : sbyte = (# "($0 + $1) << 24 >> 24" x y : sbyte #)
        static member inline (-)(x: sbyte, y: sbyte) : sbyte = (# "($0 - $1) << 24 >> 24" x y : sbyte #)
        static member inline ( * )(x: sbyte, y: sbyte) : sbyte = (# "($0 * $1) << 24 >> 24" x y : sbyte #)

        // JS `/` is true division (`10y / 3y` is 3.333…); the mask is a bitwise coercion and
        // so truncates toward zero, F#'s rule. `checkedDivisor` throws on 0, where an
        // unguarded `Infinity << 24 >> 24` would answer 0.
        static member inline (/)(x: sbyte, y: sbyte) : sbyte =
            (# "($0 / $1) << 24 >> 24" x (checkedDivisor y) : sbyte #)

        static member inline (%)(x: sbyte, y: sbyte) : sbyte =
            (# "($0 % $1) << 24 >> 24" x (checkedDivisor y) : sbyte #)

        static member inline (~+)(value: sbyte) : sbyte = value
        static member inline (~-)(n: sbyte) : sbyte = (# "(-$0) << 24 >> 24" n : sbyte #)
        static member inline (&&&)(x: sbyte, y: sbyte) : sbyte = (# "$0 & $1" x y : sbyte #)
        static member inline (|||)(x: sbyte, y: sbyte) : sbyte = (# "$0 | $1" x y : sbyte #)
        static member inline (^^^)(x: sbyte, y: sbyte) : sbyte = (# "$0 ^ $1" x y : sbyte #)
        static member inline (~~~)(value: sbyte) : sbyte = (# "~$0" value : sbyte #)
        static member inline (<<<)(value: sbyte, shift: int) : sbyte = (# "($0 << $1) << 24 >> 24" value shift : sbyte #)
        static member inline (>>>)(value: sbyte, shift: int) : sbyte = (# "$0 >> $1" value shift : sbyte #)
    end

type byte =
    (# "number" #)
    with
        static member inline (+)(x: byte, y: byte) : byte = (# "($0 + $1) & 0xFF" x y : byte #)
        static member inline (-)(x: byte, y: byte) : byte = (# "($0 - $1) & 0xFF" x y : byte #)
        static member inline ( * )(x: byte, y: byte) : byte = (# "($0 * $1) & 0xFF" x y : byte #)
        static member inline (/)(x: byte, y: byte) : byte = (# "($0 / $1) & 0xFF" x (checkedDivisor y) : byte #)
        static member inline (%)(x: byte, y: byte) : byte = (# "($0 % $1) & 0xFF" x (checkedDivisor y) : byte #)
        static member inline (~+)(value: byte) : byte = value
        static member inline (&&&)(x: byte, y: byte) : byte = (# "$0 & $1" x y : byte #)
        static member inline (|||)(x: byte, y: byte) : byte = (# "$0 | $1" x y : byte #)
        static member inline (^^^)(x: byte, y: byte) : byte = (# "$0 ^ $1" x y : byte #)
        static member inline (~~~)(value: byte) : byte = (# "(~$0) & 0xFF" value : byte #)
        static member inline (<<<)(value: byte, shift: int) : byte = (# "($0 << $1) & 0xFF" value shift : byte #)
        static member inline (>>>)(value: byte, shift: int) : byte = (# "$0 >>> $1" value shift : byte #)
    end
type int8 = sbyte
type uint8 = byte

type int16 =
    (# "number" #)
    with
        static member inline (+)(x: int16, y: int16) : int16 = (# "($0 + $1) << 16 >> 16" x y : int16 #)
        static member inline (-)(x: int16, y: int16) : int16 = (# "($0 - $1) << 16 >> 16" x y : int16 #)
        static member inline ( * )(x: int16, y: int16) : int16 = (# "($0 * $1) << 16 >> 16" x y : int16 #)

        static member inline (/)(x: int16, y: int16) : int16 =
            (# "($0 / $1) << 16 >> 16" x (checkedDivisor y) : int16 #)

        static member inline (%)(x: int16, y: int16) : int16 =
            (# "($0 % $1) << 16 >> 16" x (checkedDivisor y) : int16 #)

        static member inline (~+)(value: int16) : int16 = value
        static member inline (~-)(n: int16) : int16 = (# "(-$0) << 16 >> 16" n : int16 #)
        static member inline (&&&)(x: int16, y: int16) : int16 = (# "$0 & $1" x y : int16 #)
        static member inline (|||)(x: int16, y: int16) : int16 = (# "$0 | $1" x y : int16 #)
        static member inline (^^^)(x: int16, y: int16) : int16 = (# "$0 ^ $1" x y : int16 #)
        static member inline (~~~)(value: int16) : int16 = (# "~$0" value : int16 #)
        static member inline (<<<)(value: int16, shift: int) : int16 = (# "($0 << $1) << 16 >> 16" value shift : int16 #)
        static member inline (>>>)(value: int16, shift: int) : int16 = (# "$0 >> $1" value shift : int16 #)
    end

type uint16 =
    (# "number" #)
    with
        static member inline (+)(x: uint16, y: uint16) : uint16 = (# "($0 + $1) & 0xFFFF" x y : uint16 #)
        static member inline (-)(x: uint16, y: uint16) : uint16 = (# "($0 - $1) & 0xFFFF" x y : uint16 #)
        static member inline ( * )(x: uint16, y: uint16) : uint16 = (# "($0 * $1) & 0xFFFF" x y : uint16 #)

        static member inline (/)(x: uint16, y: uint16) : uint16 =
            (# "($0 / $1) & 0xFFFF" x (checkedDivisor y) : uint16 #)

        static member inline (%)(x: uint16, y: uint16) : uint16 =
            (# "($0 % $1) & 0xFFFF" x (checkedDivisor y) : uint16 #)

        static member inline (~+)(value: uint16) : uint16 = value
        static member inline (&&&)(x: uint16, y: uint16) : uint16 = (# "$0 & $1" x y : uint16 #)
        static member inline (|||)(x: uint16, y: uint16) : uint16 = (# "$0 | $1" x y : uint16 #)
        static member inline (^^^)(x: uint16, y: uint16) : uint16 = (# "$0 ^ $1" x y : uint16 #)
        static member inline (~~~)(value: uint16) : uint16 = (# "(~$0) & 0xFFFF" value : uint16 #)
        static member inline (<<<)(value: uint16, shift: int) : uint16 = (# "($0 << $1) & 0xFFFF" value shift : uint16 #)
        static member inline (>>>)(value: uint16, shift: int) : uint16 = (# "$0 >>> $1" value shift : uint16 #)
    end
type int32 = int

type uint32 =
    (# "number" #)
    with
        static member inline (+)(x: uint32, y: uint32) : uint32 = (# "($0 + $1) >>> 0" x y : uint32 #)
        static member inline (-)(x: uint32, y: uint32) : uint32 = (# "($0 - $1) >>> 0" x y : uint32 #)
        // `Math.imul` is the exact product mod 2^32; a masked `$0 * $1` loses low bits past 2^53.
        static member inline ( * )(x: uint32, y: uint32) : uint32 = (# "Math.imul($0, $1) >>> 0" x y : uint32 #)
        static member inline (/)(x: uint32, y: uint32) : uint32 = (# "($0 / $1) >>> 0" x (checkedDivisor y) : uint32 #)
        static member inline (%)(x: uint32, y: uint32) : uint32 = (# "($0 % $1) >>> 0" x (checkedDivisor y) : uint32 #)
        static member inline (~+)(value: uint32) : uint32 = value
        // JS bitwise answers SIGNED int32, so top-bit-set results need the `>>> 0` read-back.
        static member inline (&&&)(x: uint32, y: uint32) : uint32 = (# "($0 & $1) >>> 0" x y : uint32 #)
        static member inline (|||)(x: uint32, y: uint32) : uint32 = (# "($0 | $1) >>> 0" x y : uint32 #)
        static member inline (^^^)(x: uint32, y: uint32) : uint32 = (# "($0 ^ $1) >>> 0" x y : uint32 #)
        static member inline (~~~)(value: uint32) : uint32 = (# "(~$0) >>> 0" value : uint32 #)
        static member inline (<<<)(value: uint32, shift: int) : uint32 = (# "($0 << $1) >>> 0" value shift : uint32 #)
        // `>>>` is already the unsigned read; no second coercion.
        static member inline (>>>)(value: uint32, shift: int) : uint32 = (# "$0 >>> $1" value shift : uint32 #)
    end

type int64 =
    (# "bigint" #)
    with
        static member inline (+)(x: int64, y: int64) : int64 = (# "BigInt.asIntN(64, $0 + $1)" x y : int64 #)
        static member inline (-)(x: int64, y: int64) : int64 = (# "BigInt.asIntN(64, $0 - $1)" x y : int64 #)
        static member inline ( * )(x: int64, y: int64) : int64 = (# "BigInt.asIntN(64, $0 * $1)" x y : int64 #)

        // BigInt `/` already truncates toward zero; only the 64-bit wrap is left.
        static member inline (/)(x: int64, y: int64) : int64 =
            (# "BigInt.asIntN(64, $0 / $1)" x (checkedDivisor y) : int64 #)

        static member inline (%)(x: int64, y: int64) : int64 =
            (# "BigInt.asIntN(64, $0 % $1)" x (checkedDivisor y) : int64 #)

        static member inline (~+)(value: int64) : int64 = value
        static member inline (~-)(n: int64) : int64 = (# "BigInt.asIntN(64, -$0)" n : int64 #)
        static member inline (&&&)(x: int64, y: int64) : int64 = (# "$0 & $1" x y : int64 #)
        static member inline (|||)(x: int64, y: int64) : int64 = (# "$0 | $1" x y : int64 #)
        static member inline (^^^)(x: int64, y: int64) : int64 = (# "$0 ^ $1" x y : int64 #)
        static member inline (~~~)(value: int64) : int64 = (# "~$0" value : int64 #)
        // A BigInt shift needs a BigInt shift amount, so the `int` operand converts.
        static member inline (<<<)(value: int64, shift: int) : int64 =
            (# "BigInt.asIntN(64, $0 << BigInt($1))" value shift : int64 #)

        static member inline (>>>)(value: int64, shift: int) : int64 =
            (# "BigInt.asIntN(64, $0 >> BigInt($1))" value shift : int64 #)
    end

type uint64 =
    (# "bigint" #)
    with
        static member inline (+)(x: uint64, y: uint64) : uint64 = (# "BigInt.asUintN(64, $0 + $1)" x y : uint64 #)
        static member inline (-)(x: uint64, y: uint64) : uint64 = (# "BigInt.asUintN(64, $0 - $1)" x y : uint64 #)
        static member inline ( * )(x: uint64, y: uint64) : uint64 = (# "BigInt.asUintN(64, $0 * $1)" x y : uint64 #)

        static member inline (/)(x: uint64, y: uint64) : uint64 =
            (# "BigInt.asUintN(64, $0 / $1)" x (checkedDivisor y) : uint64 #)

        static member inline (%)(x: uint64, y: uint64) : uint64 =
            (# "BigInt.asUintN(64, $0 % $1)" x (checkedDivisor y) : uint64 #)

        static member inline (~+)(value: uint64) : uint64 = value
        static member inline (&&&)(x: uint64, y: uint64) : uint64 = (# "$0 & $1" x y : uint64 #)
        static member inline (|||)(x: uint64, y: uint64) : uint64 = (# "$0 | $1" x y : uint64 #)
        static member inline (^^^)(x: uint64, y: uint64) : uint64 = (# "$0 ^ $1" x y : uint64 #)
        static member inline (~~~)(value: uint64) : uint64 = (# "BigInt.asUintN(64, ~$0)" value : uint64 #)

        static member inline (<<<)(value: uint64, shift: int) : uint64 =
            (# "BigInt.asUintN(64, $0 << BigInt($1))" value shift : uint64 #)

        // Non-negative by construction, so `>>` zero-fills and stays in range.
        static member inline (>>>)(value: uint64, shift: int) : uint64 = (# "$0 >> BigInt($1)" value shift : uint64 #)
    end
type uint = uint32
