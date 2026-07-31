namespace Vesper

// Each width states its own bitwise surface (`&&& ||| ^^^ ~~~ <<< >>>`). The list is
// the contract: an operand type that states no member does not support the operator,
// which is what makes `1.0 &&& 2.0` an error rather than garbage IL. The bodies are the
// target's own, in the paired `.fs` — a member on an intrinsic is spliced at the use
// site, never emitted. The shift amount is `int` at every width.

/// <summary>An abbreviation for the CLI type <see cref="T:System.SByte"/>.</summary>
///
/// <category>Basic Types</category>
type sbyte =
    extern with

    static member (&&&): x: sbyte * y: sbyte -> sbyte

    static member (|||): x: sbyte * y: sbyte -> sbyte

    static member (^^^): x: sbyte * y: sbyte -> sbyte

    static member (~~~): value: sbyte -> sbyte

    static member (<<<): value: sbyte * shift: int -> sbyte

    static member (>>>): value: sbyte * shift: int -> sbyte

/// <summary>An abbreviation for the CLI type <see cref="T:System.Byte"/>.</summary>
///
/// <category>Basic Types</category>
type byte =
    extern with

    static member (&&&): x: byte * y: byte -> byte

    static member (|||): x: byte * y: byte -> byte

    static member (^^^): x: byte * y: byte -> byte

    static member (~~~): value: byte -> byte

    static member (<<<): value: byte * shift: int -> byte

    static member (>>>): value: byte * shift: int -> byte

/// <summary>An abbreviation for the CLI type <see cref="T:System.SByte"/>.</summary>
///
/// <category>Basic Types</category>
type int8 = sbyte

/// <summary>An abbreviation for the CLI type <see cref="T:System.Byte"/>.</summary>
///
/// <category>Basic Types</category>
type uint8 = byte

/// <summary>An abbreviation for the CLI type <see cref="T:System.Int16"/>.</summary>
///
/// <category>Basic Types</category>
type int16 =
    extern with

    static member (&&&): x: int16 * y: int16 -> int16

    static member (|||): x: int16 * y: int16 -> int16

    static member (^^^): x: int16 * y: int16 -> int16

    static member (~~~): value: int16 -> int16

    static member (<<<): value: int16 * shift: int -> int16

    static member (>>>): value: int16 * shift: int -> int16

/// <summary>An abbreviation for the CLI type <see cref="T:System.UInt16"/>.</summary>
///
/// <category>Basic Types</category>
type uint16 =
    extern with

    static member (&&&): x: uint16 * y: uint16 -> uint16

    static member (|||): x: uint16 * y: uint16 -> uint16

    static member (^^^): x: uint16 * y: uint16 -> uint16

    static member (~~~): value: uint16 -> uint16

    static member (<<<): value: uint16 * shift: int -> uint16

    static member (>>>): value: uint16 * shift: int -> uint16

/// <summary>An abbreviation for the CLI type <see cref="T:System.Int32"/>.</summary>
///
/// <category>Basic Types</category>
type int32 = int

/// <summary>An abbreviation for the CLI type <see cref="T:System.UInt32"/>.</summary>
///
/// <category>Basic Types</category>
type uint32 =
    extern with

    static member (&&&): x: uint32 * y: uint32 -> uint32

    static member (|||): x: uint32 * y: uint32 -> uint32

    static member (^^^): x: uint32 * y: uint32 -> uint32

    static member (~~~): value: uint32 -> uint32

    static member (<<<): value: uint32 * shift: int -> uint32

    static member (>>>): value: uint32 * shift: int -> uint32

/// <summary>An abbreviation for the CLI type <see cref="T:System.Int64"/>.</summary>
///
/// <category>Basic Types</category>
type int64 =
    extern with

    static member (&&&): x: int64 * y: int64 -> int64

    static member (|||): x: int64 * y: int64 -> int64

    static member (^^^): x: int64 * y: int64 -> int64

    static member (~~~): value: int64 -> int64

    static member (<<<): value: int64 * shift: int -> int64

    static member (>>>): value: int64 * shift: int -> int64

/// <summary>An abbreviation for the CLI type <see cref="T:System.UInt64"/>.</summary>
///
/// <category>Basic Types</category>
type uint64 =
    extern with

    static member (&&&): x: uint64 * y: uint64 -> uint64

    static member (|||): x: uint64 * y: uint64 -> uint64

    static member (^^^): x: uint64 * y: uint64 -> uint64

    static member (~~~): value: uint64 -> uint64

    static member (<<<): value: uint64 * shift: int -> uint64

    static member (>>>): value: uint64 * shift: int -> uint64

/// <summary>An abbreviation for the CLI type <see cref="T:System.UInt32"/>.</summary>
///
/// <category>Basic Types</category>
type uint = uint32
