namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open XParsec.FSharp

// Post-lexing interpretation of a numeric-literal token: its value, read from the
// `NumericBase` / `NumericKind` flags the lexer has already classified onto it.

/// An integral width. The single width type shared by the literal reader, the semantic
/// constant model and both backends.
[<RequireQualifiedAccess>]
type IntWidth =
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

/// Per-width facts, and the operations on a width's value. A value travels as `bits: int64`,
/// the 64-bit two's-complement encoding: sign-extended out of a signed width, zero-extended
/// out of an unsigned one. A `uint64` past `Int64.MaxValue` reads back through `render`.
module IntWidth =

    let all: IntWidth list =
        [
            IntWidth.SByte
            IntWidth.Byte
            IntWidth.Int16
            IntWidth.UInt16
            IntWidth.Int32
            IntWidth.UInt32
            IntWidth.Int64
            IntWidth.UInt64
            IntWidth.NativeInt
            IntWidth.UNativeInt
        ]

    /// The F# literal suffix (`10y`, `10UL`). `Int32` is unsuffixed.
    let suffix (w: IntWidth) : string =
        match w with
        | IntWidth.SByte -> "y"
        | IntWidth.Byte -> "uy"
        | IntWidth.Int16 -> "s"
        | IntWidth.UInt16 -> "us"
        | IntWidth.Int32 -> ""
        | IntWidth.UInt32 -> "u"
        | IntWidth.Int64 -> "L"
        | IntWidth.UInt64 -> "UL"
        | IntWidth.NativeInt -> "n"
        | IntWidth.UNativeInt -> "un"

    let isSigned (w: IntWidth) : bool =
        match w with
        | IntWidth.SByte
        | IntWidth.Int16
        | IntWidth.Int32
        | IntWidth.Int64
        | IntWidth.NativeInt -> true
        | IntWidth.Byte
        | IntWidth.UInt16
        | IntWidth.UInt32
        | IntWidth.UInt64
        | IntWidth.UNativeInt -> false

    /// Pointer-width: `nativeint` / `unativeint`, a distinct CIL stack type.
    let isNative (w: IntWidth) : bool =
        match w with
        | IntWidth.NativeInt
        | IntWidth.UNativeInt -> true
        | IntWidth.SByte
        | IntWidth.Byte
        | IntWidth.Int16
        | IntWidth.UInt16
        | IntWidth.Int32
        | IntWidth.UInt32
        | IntWidth.Int64
        | IntWidth.UInt64 -> false

    /// The width needs 64 bits: the CIL `int64` stack type, or a JS `BigInt`. Everything
    /// narrower shares the `int32` stack type and the JS `number`.
    let isWide (w: IntWidth) : bool =
        match w with
        | IntWidth.Int64
        | IntWidth.UInt64
        | IntWidth.NativeInt
        | IntWidth.UNativeInt -> true
        | IntWidth.SByte
        | IntWidth.Byte
        | IntWidth.Int16
        | IntWidth.UInt16
        | IntWidth.Int32
        | IntWidth.UInt32 -> false

    /// May a CLR enum be based on this width? `System.Enum` admits the eight fixed widths only.
    let isEnumBase (w: IntWidth) : bool = not (isNative w)

    /// The width a lexed token's `NumericKind` denotes, `ValueNone` for the kinds that are
    /// not integral widths: the floats, `decimal`, and the custom `NumericLiteral` suffixes.
    let ofNumericKind (kind: NumericKind) : IntWidth voption =
        match kind with
        | NumericKind.SByte -> ValueSome IntWidth.SByte
        | NumericKind.Byte -> ValueSome IntWidth.Byte
        | NumericKind.Int16 -> ValueSome IntWidth.Int16
        | NumericKind.UInt16 -> ValueSome IntWidth.UInt16
        | NumericKind.Int32 -> ValueSome IntWidth.Int32
        | NumericKind.UInt32 -> ValueSome IntWidth.UInt32
        | NumericKind.Int64 -> ValueSome IntWidth.Int64
        | NumericKind.UInt64 -> ValueSome IntWidth.UInt64
        | NumericKind.NativeInt -> ValueSome IntWidth.NativeInt
        | NumericKind.UNativeInt -> ValueSome IntWidth.UNativeInt
        | _ -> ValueNone

    /// Read a magnitude at this width, range-checked by the `Convert` overload
    /// (`Convert.ToByte("300", 10)` throws). Throws `OverflowException` / `FormatException`
    /// for a magnitude or sign the width cannot hold. The result is encoded into `bits`.
    let parseBits (w: IntWidth) (digits: string) (radix: int) : int64 =
        match w with
        | IntWidth.SByte -> int64 (Convert.ToSByte(digits, radix))
        | IntWidth.Byte -> int64 (Convert.ToByte(digits, radix))
        | IntWidth.Int16 -> int64 (Convert.ToInt16(digits, radix))
        | IntWidth.UInt16 -> int64 (Convert.ToUInt16(digits, radix))
        | IntWidth.Int32 -> int64 (Convert.ToInt32(digits, radix))
        | IntWidth.UInt32 -> int64 (Convert.ToUInt32(digits, radix))
        | IntWidth.Int64 -> Convert.ToInt64(digits, radix)
        | IntWidth.UInt64 -> int64 (Convert.ToUInt64(digits, radix))
        // Pointer-width: read the magnitude at 64 bits, the width of the targeted runtimes.
        | IntWidth.NativeInt -> Convert.ToInt64(digits, radix)
        | IntWidth.UNativeInt -> int64 (Convert.ToUInt64(digits, radix))

    /// Truncate a 64-bit pattern onto the width and re-extend it, giving the normal form of
    /// `bits`. A wrap for the narrow widths, the identity for the 64-bit ones.
    let private normalize (w: IntWidth) (bits: int64) : int64 =
        match w with
        | IntWidth.SByte -> int64 (sbyte bits)
        | IntWidth.Byte -> int64 (byte bits)
        | IntWidth.Int16 -> int64 (int16 bits)
        | IntWidth.UInt16 -> int64 (uint16 bits)
        | IntWidth.Int32 -> int64 (int32 bits)
        | IntWidth.UInt32 -> int64 (uint32 bits)
        | IntWidth.Int64
        | IntWidth.UInt64
        | IntWidth.NativeInt
        | IntWidth.UNativeInt -> bits

    /// The value's decimal text at the width's own signedness, as the .NET primitive's
    /// `ToString()` prints it.
    let render (w: IntWidth) (bits: int64) : string =
        if isSigned w then string bits else string (uint64 bits)

    /// The value boxed at its own .NET primitive type, as a CLR enum-case field constant must
    /// carry it: the metadata writer reads the box's runtime type.
    let boxed (w: IntWidth) (bits: int64) : obj =
        match w with
        | IntWidth.SByte -> box (sbyte bits)
        | IntWidth.Byte -> box (byte bits)
        | IntWidth.Int16 -> box (int16 bits)
        | IntWidth.UInt16 -> box (uint16 bits)
        | IntWidth.Int32 -> box (int32 bits)
        | IntWidth.UInt32 -> box (uint32 bits)
        | IntWidth.Int64 -> box bits
        | IntWidth.UInt64 -> box (uint64 bits)
        | IntWidth.NativeInt -> box (nativeint bits)
        | IntWidth.UNativeInt -> box (unativeint (uint64 bits))

    /// Two's-complement negation at the width: the value wraps rather than growing, so
    /// `-(-128y)` is `-128y` and `-Int32.MinValue` is itself. Meaningful only on a signed
    /// width, since F# defines no negation on an unsigned one.
    let negate (w: IntWidth) (bits: int64) : int64 = normalize w (-bits)

[<RequireQualifiedAccess>]
type NumericLiteralValue =
    | Integral of width: IntWidth * bits: int64
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
    /// The lexed magnitude or sign does not fit the classified width: an out-of-range
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

    /// Parse a numeric literal's value from its raw source `text`, at the width and base the
    /// lexer recorded on `token`. A non-decimal magnitude is read as its two's-complement bit
    /// pattern (`0xFFFFFFFF` is `-1`), matching F#'s wrap semantics for radix literals.
    let parseNumericLiteral (token: Token) (text: string) : Result<NumericLiteralValue, NumericLiteralRejection> =
        if not (TokenInfo.isNumeric token) then
            Error NumericLiteralRejection.NotNumeric
        else
            let numBase = TokenInfo.numericBase token
            let radix = baseRadix numBase
            let kind = TokenInfo.numericKind token

            // An integral kind reads its magnitude through `IntWidth.parseBits`, whose
            // `Convert` overload is that width's range check. The kinds `ofNumericKind`
            // declines are the non-integral ones, and each parses its own way.
            let project () =
                match IntWidth.ofNumericKind kind with
                | ValueSome w ->
                    Ok(NumericLiteralValue.Integral(w, IntWidth.parseBits w (intDigits numBase text) radix))
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
