namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open XParsec.FSharp

// Post-lexing interpretation of a numeric-literal token: its value, read from the
// `NumericBase` / `NumericKind` flags the lexer has already classified onto it.

/// One of F#'s integral primitive types, by its width and signedness. Shared by the literal
/// reader, the semantic constant model and both backends.
[<RequireQualifiedAccess>]
type IntKind =
    | SByte
    | Byte
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    /// Pointer-width. Has no JS representation, and cannot underlie a CLR enum.
    | NativeInt
    | UNativeInt

/// Per-kind facts, and the operations on a kind's value. A value travels as `bits: int64`,
/// the 64-bit two's-complement encoding: sign-extended out of a signed kind, zero-extended
/// out of an unsigned one. A `uint64` past `Int64.MaxValue` reads back through `render`.
module IntKind =

    let all: IntKind list =
        [
            IntKind.SByte
            IntKind.Byte
            IntKind.Int16
            IntKind.UInt16
            IntKind.Int32
            IntKind.UInt32
            IntKind.Int64
            IntKind.UInt64
            IntKind.NativeInt
            IntKind.UNativeInt
        ]

    /// The F# literal suffix (`10y`, `10UL`). `Int32` is unsuffixed.
    let suffix (k: IntKind) : string =
        match k with
        | IntKind.SByte -> "y"
        | IntKind.Byte -> "uy"
        | IntKind.Int16 -> "s"
        | IntKind.UInt16 -> "us"
        | IntKind.Int32 -> ""
        | IntKind.UInt32 -> "u"
        | IntKind.Int64 -> "L"
        | IntKind.UInt64 -> "UL"
        | IntKind.NativeInt -> "n"
        | IntKind.UNativeInt -> "un"

    let isSigned (k: IntKind) : bool =
        match k with
        | IntKind.SByte
        | IntKind.Int16
        | IntKind.Int32
        | IntKind.Int64
        | IntKind.NativeInt -> true
        | IntKind.Byte
        | IntKind.UInt16
        | IntKind.UInt32
        | IntKind.UInt64
        | IntKind.UNativeInt -> false

    /// Pointer-width: `nativeint` / `unativeint`, a distinct CIL stack type.
    let isNative (k: IntKind) : bool =
        match k with
        | IntKind.NativeInt
        | IntKind.UNativeInt -> true
        | IntKind.SByte
        | IntKind.Byte
        | IntKind.Int16
        | IntKind.UInt16
        | IntKind.Int32
        | IntKind.UInt32
        | IntKind.Int64
        | IntKind.UInt64 -> false

    /// The kind needs 64 bits: the CIL `int64` stack type, or a JS `BigInt`. Everything
    /// narrower shares the `int32` stack type and the JS `number`.
    let isWide (k: IntKind) : bool =
        match k with
        | IntKind.Int64
        | IntKind.UInt64
        | IntKind.NativeInt
        | IntKind.UNativeInt -> true
        | IntKind.SByte
        | IntKind.Byte
        | IntKind.Int16
        | IntKind.UInt16
        | IntKind.Int32
        | IntKind.UInt32 -> false

    /// May a CLR enum be based on this kind? `System.Enum` admits the eight fixed-width kinds
    /// only.
    let isEnumBase (k: IntKind) : bool = not (isNative k)

    /// The integral kind a lexed token's `NumericKind` denotes, `ValueNone` for the
    /// non-integral ones: the floats, `decimal`, and the custom `NumericLiteral` suffixes.
    let ofNumericKind (kind: NumericKind) : IntKind voption =
        match kind with
        | NumericKind.SByte -> ValueSome IntKind.SByte
        | NumericKind.Byte -> ValueSome IntKind.Byte
        | NumericKind.Int16 -> ValueSome IntKind.Int16
        | NumericKind.UInt16 -> ValueSome IntKind.UInt16
        | NumericKind.Int32 -> ValueSome IntKind.Int32
        | NumericKind.UInt32 -> ValueSome IntKind.UInt32
        | NumericKind.Int64 -> ValueSome IntKind.Int64
        | NumericKind.UInt64 -> ValueSome IntKind.UInt64
        | NumericKind.NativeInt -> ValueSome IntKind.NativeInt
        | NumericKind.UNativeInt -> ValueSome IntKind.UNativeInt
        | _ -> ValueNone

    /// Read a magnitude at `k`'s width, range-checked by the `Convert` overload
    /// (`Convert.ToByte("300", 10)` throws). Throws `OverflowException` / `FormatException`
    /// for a magnitude or sign the kind cannot hold. The result is encoded into `bits`.
    let parseBits (k: IntKind) (digits: string) (radix: int) : int64 =
        match k with
        | IntKind.SByte -> int64 (Convert.ToSByte(digits, radix))
        | IntKind.Byte -> int64 (Convert.ToByte(digits, radix))
        | IntKind.Int16 -> int64 (Convert.ToInt16(digits, radix))
        | IntKind.UInt16 -> int64 (Convert.ToUInt16(digits, radix))
        | IntKind.Int32 -> int64 (Convert.ToInt32(digits, radix))
        | IntKind.UInt32 -> int64 (Convert.ToUInt32(digits, radix))
        | IntKind.Int64 -> Convert.ToInt64(digits, radix)
        | IntKind.UInt64 -> int64 (Convert.ToUInt64(digits, radix))
        // Pointer-width: read the magnitude at 64 bits, the width of the targeted runtimes.
        | IntKind.NativeInt -> Convert.ToInt64(digits, radix)
        | IntKind.UNativeInt -> int64 (Convert.ToUInt64(digits, radix))

    /// Truncate a 64-bit pattern onto `k`'s width and re-extend it, giving the normal form of
    /// `bits`. A wrap for the narrow kinds, the identity for the 64-bit ones.
    let private normalize (k: IntKind) (bits: int64) : int64 =
        match k with
        | IntKind.SByte -> int64 (sbyte bits)
        | IntKind.Byte -> int64 (byte bits)
        | IntKind.Int16 -> int64 (int16 bits)
        | IntKind.UInt16 -> int64 (uint16 bits)
        | IntKind.Int32 -> int64 (int32 bits)
        | IntKind.UInt32 -> int64 (uint32 bits)
        | IntKind.Int64
        | IntKind.UInt64
        | IntKind.NativeInt
        | IntKind.UNativeInt -> bits

    /// The value's decimal text at the kind's own signedness, as the .NET primitive's
    /// `ToString()` prints it.
    let render (k: IntKind) (bits: int64) : string =
        if isSigned k then string bits else string (uint64 bits)

    /// The value boxed at its own .NET primitive type, as a CLR enum-case field constant must
    /// carry it: the metadata writer reads the box's runtime type.
    let boxed (k: IntKind) (bits: int64) : obj =
        match k with
        | IntKind.SByte -> box (sbyte bits)
        | IntKind.Byte -> box (byte bits)
        | IntKind.Int16 -> box (int16 bits)
        | IntKind.UInt16 -> box (uint16 bits)
        | IntKind.Int32 -> box (int32 bits)
        | IntKind.UInt32 -> box (uint32 bits)
        | IntKind.Int64 -> box bits
        | IntKind.UInt64 -> box (uint64 bits)
        | IntKind.NativeInt -> box (nativeint bits)
        | IntKind.UNativeInt -> box (unativeint (uint64 bits))

    /// Two's-complement negation at `k`'s width: the value wraps rather than growing, so
    /// `-(-128y)` is `-128y` and `-Int32.MinValue` is itself. Meaningful only on a signed
    /// kind, since F# defines no negation on an unsigned one.
    let negate (k: IntKind) (bits: int64) : int64 = normalize k (-bits)

[<RequireQualifiedAccess>]
type NumericLiteralValue =
    | Integral of kind: IntKind * bits: int64
    | Float of double
    | Float32 of single
    | Decimal of decimal

/// Why a literal token carries no primitive constant.
[<RequireQualifiedAccess>]
type NumericLiteralRejection =
    /// A custom numeric literal, whose suffix F# translates to a call into the
    /// `NumericLiteral<suffix>` module in scope (`52I` = `NumericLiteralI.FromInt32 52`), or
    /// a suffix F# reserves and gives no meaning to. Neither projects to a constant.
    | CustomLiteral
    /// The lexed magnitude or sign does not fit the classified kind: an out-of-range
    /// magnitude (`300uy`), or a negative-signed *unsigned* literal (`-1uy`, formed by the
    /// negative-literal merge).
    | OutOfRange
    /// The token is not a numeric literal, or carries a `NumericKind` outside the declared
    /// set (a 5-bit field, so F# cannot prove the match exhaustive). A producer bug at every
    /// call site, not a user error.
    | NotNumeric

module NumericLiterals =

    /// The .NET radix for a classified base: 2/8/10/16 is the only set the
    /// `Convert.To…(string, int)` overloads accept, decimal included.
    let private baseRadix (numBase: NumericBase) : int =
        match numBase with
        | NumericBase.Hex -> 16
        | NumericBase.Octal -> 8
        | NumericBase.Binary -> 2
        | _ -> 10

    /// The bare digit span of an integer magnitude: drop the trailing width-suffix letters
    /// (`u`/`l`/`y`/`s`/`n`, any case, none of which collide with the hex digits `a`–`f`),
    /// then the two-char radix prefix for a non-decimal base, then the digit-group underscores.
    let private intDigits (numBase: NumericBase) (text: string) : string =
        let mutable hi = text.Length

        while hi > 0
              && (
                  match Char.ToLowerInvariant text.[hi - 1] with
                  | 'u'
                  | 'l'
                  | 'y'
                  | 's'
                  | 'n' -> true
                  | _ -> false
              ) do
            hi <- hi - 1

        let lo = if numBase = NumericBase.Decimal then 0 else 2
        text.Substring(lo, hi - lo).Replace("_", "")

    /// The classified numeric kind of a token, `ValueNone` when it is not a numeric literal.
    /// The public read of the lexer's flag field, whose radix axis is already collapsed:
    /// `10y` and `0x0Ay` are both `NumericKind.SByte`.
    let numericKindOf (token: Token) : NumericKind voption =
        if TokenInfo.isNumeric token then
            ValueSome(TokenInfo.numericKind token)
        else
            ValueNone

    /// Strip a single trailing suffix, case-insensitively: the float (`f`) and decimal (`m`)
    /// markers.
    let private stripSuffix (suffix: string) (text: string) : string =
        if text.EndsWith(suffix, StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    /// Parse a numeric literal's value from its raw source `text`, at the kind and base the
    /// lexer recorded on `token`. A non-decimal magnitude is read as its two's-complement bit
    /// pattern (`0xFFFFFFFF` is `-1`), matching F#'s wrap semantics for radix literals.
    let parseNumericLiteral (token: Token) (text: string) : Result<NumericLiteralValue, NumericLiteralRejection> =
        if not (TokenInfo.isNumeric token) then
            Error NumericLiteralRejection.NotNumeric
        else
            let numBase = TokenInfo.numericBase token
            let radix = baseRadix numBase
            let kind = TokenInfo.numericKind token

            // An integral kind reads its magnitude through `IntKind.parseBits`, whose
            // `Convert` overload is that kind's range check. The kinds `ofNumericKind`
            // declines are the non-integral ones, and each parses its own way.
            let project () =
                match IntKind.ofNumericKind kind with
                | ValueSome k -> Ok(NumericLiteralValue.Integral(k, IntKind.parseBits k (intDigits numBase text) radix))
                | ValueNone ->

                    match kind with
                    | NumericKind.IEEE64 ->
                        Ok(NumericLiteralValue.Float(Double.Parse(text, CultureInfo.InvariantCulture)))
                    | NumericKind.IEEE32 ->
                        Ok(
                            NumericLiteralValue.Float32(
                                Single.Parse(stripSuffix "f" text, CultureInfo.InvariantCulture)
                            )
                        )
                    | NumericKind.Decimal ->
                        Ok(
                            NumericLiteralValue.Decimal(
                                Decimal.Parse(stripSuffix "m" text, NumberStyles.Float, CultureInfo.InvariantCulture)
                            )
                        )
                    | NumericKind.BigIntegerQ
                    | NumericKind.BigIntegerR
                    | NumericKind.BigIntegerZ
                    | NumericKind.BigIntegerI
                    | NumericKind.BigIntegerN
                    | NumericKind.BigIntegerG
                    | NumericKind.ReservedNumericLiteral -> Error NumericLiteralRejection.CustomLiteral
                    // `NumericKind` is a 5-bit field, so a hand-built token can carry a value
                    // outside the declared cases. That is a corrupt token, not a literal.
                    | _ -> Error NumericLiteralRejection.NotNumeric

            try
                project ()
            with
            | :? OverflowException
            | :? FormatException -> Error NumericLiteralRejection.OutOfRange
