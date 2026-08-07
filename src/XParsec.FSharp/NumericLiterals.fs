namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open XParsec.FSharp

// Post-lexing INTERPRETATION of a numeric-literal token: its value. Not lexing —
// the token has already been classified (its `NumericBase` / `NumericKind` flags
// carry the radix and the width), and this reads those flags rather than
// re-sniffing the `0x`/`0o`/`0b` prefix or the `u`/`L`/`uy` suffix grammar. So it
// lives beside `Lexing`, not in it.

/// An integral width, AS A VALUE. Every layer that carries an integral constant keys off
/// this one type — the literal reader (`NumericLiteralValue`), the semantic constant model
/// (`TConstValue`), the elaborator's enum-underlying-type choice, and both backends — so a
/// consumer ASKS a width its properties (`IntWidth.suffix`, `IntWidth.isSigned`, …) instead
/// of re-deriving them by enumerating one constant case per width at every site.
///
/// That is the difference between a width the compiler tracks and one it doesn't. The
/// tables below are exhaustive matches on `IntWidth`, so adding a width is a compile error
/// at every fact that must be stated about it — where an or-pattern over N constant cases
/// with a `| other -> failwith` residual arm would have kept compiling and started failing
/// at run time.
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
    /// Pointer-width, and so not a fixed width at all: it has no JS representation, and no
    /// CLR enum may be based on it. `isNative` is the predicate both facts are asked by.
    | NativeInt
    | UNativeInt

/// The width tables — the single place each per-width fact is stated.
///
/// A width's VALUE travels as `bits: int64`: the two's-complement 64-bit encoding of the
/// value, sign-extended out of a signed width and zero-extended out of an unsigned one. ONE
/// encoding, so no consumer invents its own reinterpretation, and a source literal still
/// reads as itself (`-1` is `-1L`, `255uy` is `255L`). The one place it is not literal is a
/// `uint64` past `Int64.MaxValue`, which necessarily shows as a negative — read it back with
/// `render` / `boxed`, never by looking at `bits`.
///
/// The pair (`width`, `bits`) is the whole value: `render` gives its decimal text at the
/// width's own signedness, `boxed` gives the .NET primitive, and a backend truncates it to
/// its own stack width. All three are correct on that one encoding alone.
module IntWidth =

    /// The F# literal suffix (`10y`, `10UL`). `int` alone is unsuffixed — it is the width an
    /// unannotated literal defaults to.
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

    /// Signedness — which is what decides whether the value's `bits` read back as `int64`
    /// or `uint64`, whether negation is defined on the width at all, and (for the pointer
    /// pair) whether CIL sign- or zero-extends.
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

    /// Pointer-width (`nativeint` / `unativeint`) — a distinct CIL stack type, and a width
    /// with no JS representation at all.
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

    /// `true` when the width's magnitude does not fit 32 bits — the CIL 64-bit stack type
    /// (`ldc.i8`) on one backend, a JS `BigInt` on the other. Everything narrower shares the
    /// int32 stack type / the JS `number`.
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

    /// May a CLR enum be based on this width? Every fixed width; never the pointer pair
    /// (`System.Enum`'s underlying type is one of the eight integral primitives).
    let isEnumBase (w: IntWidth) : bool = not (isNative w)

    /// The width a lexed numeric token's classified `NumericKind` denotes — `ValueNone` for
    /// the kinds that are not integral widths at all (the floats, `decimal`, the custom
    /// `NumericLiteral` suffixes). The bridge from the lexer's flag to the width, asked by
    /// everything that types a numeric literal: the unifier's literal carrier
    /// (`UnificationInferLiterals.literalCarrier`) and the value reader below.
    ///
    /// `NumericKind` is an enum over a 5-bit field, so F# cannot prove this match exhaustive
    /// — the residual arm is the honest answer for a value outside the declared set, not a
    /// width fallback.
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

    /// Read a magnitude at this width, RANGE-CHECKED — and the range check IS the `Convert`
    /// overload (`Convert.ToByte("300", 10)` throws), which is the only reason the widths are
    /// spelled out one by one anywhere. Throws `OverflowException` / `FormatException` for a
    /// magnitude or sign the width cannot hold; `NumericLiterals.parseNumericLiteral` is the
    /// caller that turns that into an `OutOfRange` rejection.
    ///
    /// The result is encoded into `bits`: signed widths sign-extend (`int64 v`), unsigned ones
    /// zero-extend and reinterpret (`int64 (uint64 v)` — the identity below 2^63, and what
    /// carries a `uint64` past `Int64.MaxValue`).
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
        // Pointer-width: read the magnitude at 64 bits, the width of the runtimes we target.
        | IntWidth.NativeInt -> Convert.ToInt64(digits, radix)
        | IntWidth.UNativeInt -> int64 (Convert.ToUInt64(digits, radix))

    /// Truncate a 64-bit pattern onto the width and re-extend it — the normal form `bits` is
    /// always in. A wrap for the narrow widths; the identity for the 64-bit ones (which is
    /// where a `uint64` past `Int64.MaxValue` keeps its all-ones encoding as a negative
    /// `bits`).
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

    /// The value's decimal text, read back at the width's own signedness — what `.NET`'s
    /// `ToString()` prints for that primitive. Signed: `bits` IS the value. Unsigned:
    /// `uint64 bits` is (the narrow unsigned widths are zero-extended, so they read the same
    /// either way; only `uint64` / `unativeint` past `Int64.MaxValue` need the reinterpret).
    let render (w: IntWidth) (bits: int64) : string =
        if isSigned w then string bits else string (uint64 bits)

    /// The value boxed AT ITS OWN .NET primitive type — what a CLR enum-case field constant
    /// must carry (the metadata writer reads the box's runtime type).
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

    /// Two's-complement negation AT THE WIDTH: the value WRAPS rather than growing, so
    /// `-(-128y)` is `-128y` and not `128`, and `-Int32.MinValue` is itself. Only meaningful
    /// on a signed width — negating an unsigned value has no answer the width can hold, and
    /// F# defines none.
    let negate (w: IntWidth) (bits: int64) : int64 = normalize w (-bits)

/// A numeric literal's parsed value. The integral case carries its width as an `IntWidth`
/// witness, so no width can fold onto another and silently truncate — and so the semantic
/// layer's `TConstValue` agrees with this one BY CONSTRUCTION (both key off `IntWidth`)
/// rather than by a hand-maintained case-for-case correspondence.
/// `NumericLiterals.parseNumericLiteral` is the canonical producer.
[<RequireQualifiedAccess>]
type NumericLiteralValue =
    | Integral of width: IntWidth * bits: int64
    | Float of double
    | Float32 of single
    | Decimal of decimal

/// Why a literal token carries no primitive constant. These are genuinely different
/// answers, and a consumer with a diagnostic channel must be able to say WHICH — a single
/// "no value" would have `52I` reported as an out-of-range magnitude.
[<RequireQualifiedAccess>]
type NumericLiteralRejection =
    /// A CUSTOM numeric literal (`52I` / `52Q` / …): F# translates the suffix to a call
    /// into whatever `NumericLiteral<suffix>` module is in scope
    /// (`52I` = `NumericLiteralI.FromInt32 52`), so there is no constant to project, by
    /// construction. `ReservedNumericLiteral` is a suffix F# reserves and gives no meaning
    /// to, and so likewise names no value. See `custom-numeric-literals-plan.md`.
    | CustomLiteral
    /// The lexed magnitude or sign does not fit the width the lexer classified: a
    /// negative-signed *unsigned* literal (`-1uy`, formed by the negative-literal merge)
    /// or an out-of-range magnitude (`300uy`).
    | OutOfRange
    /// The token is not a numeric literal at all, or carries a `NumericKind` outside the
    /// declared set (the field is 5 bits, so F# cannot prove the match exhaustive). A
    /// producer bug at every call site, not a user error.
    | NotNumeric

module NumericLiterals =

    /// `.NET`-radix (2/8/10/16) for the lexer's classified base — the only set
    /// `Convert.To{Int32,UInt32,Int64,Byte,UInt64}(string, int)` accepts, which
    /// is why one `Convert.To…` call covers every base (decimal included).
    let private baseRadix (numBase: NumericBase) : int =
        match numBase with
        | NumericBase.Hex -> 16
        | NumericBase.Octal -> 8
        | NumericBase.Binary -> 2
        | _ -> 10

    /// The bare digit span of an integer magnitude: drop the trailing width-
    /// suffix letters (`u`/`l`/`y`/`s`/`n`, any case — none collide with the
    /// hex digits `a`–`f`, so this is safe for a `0x…` magnitude), then the
    /// fixed two-char radix prefix for a non-decimal base, then the digit-group
    /// underscores. The base is known, so the prefix is never re-sniffed.
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

    /// The classified numeric KIND of a token — `ValueNone` when it is not a numeric literal
    /// at all. The public read of the lexer's internal flag field (`TokenInfo` is internal to
    /// this assembly), so a consumer outside the lexer keys off the classification the lexer
    /// already made instead of re-deriving it from the token's own case. Note the radix axis
    /// is already collapsed: `10y` and `0x0Ay` are both `NumericKind.SByte`.
    let numericKindOf (token: Token) : NumericKind voption =
        if TokenInfo.isNumeric token then
            ValueSome(TokenInfo.numericKind token)
        else
            ValueNone

    /// Strip a single trailing suffix (case-insensitively) — the float (`f`) /
    /// decimal (`m`) markers, which `intDigits`' radix path doesn't touch.
    let private stripSuffix (suffix: string) (text: string) : string =
        if text.EndsWith(suffix, StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    /// Parse a numeric literal's value from its raw source `text`, keyed off the
    /// width + base the lexer already recorded on `token`. A non-decimal magnitude
    /// is read as its two's-complement bit pattern (`0xFFFFFFFF` → `-1`), matching
    /// F#'s wrap semantics for radix literals.
    ///
    /// `Error` carries WHY there is no constant (`NumericLiteralRejection`) — never a
    /// truncation, and never merely "no". The consumer (`ElaborateLiterals.tryParseConst`)
    /// turns the reason into the diagnostic the user reads, so a custom numeric literal
    /// is not reported as an out-of-range magnitude.
    let parseNumericLiteral (token: Token) (text: string) : Result<NumericLiteralValue, NumericLiteralRejection> =
        if not (TokenInfo.isNumeric token) then
            Error NumericLiteralRejection.NotNumeric
        else
            let numBase = TokenInfo.numericBase token
            let radix = baseRadix numBase
            let kind = TokenInfo.numericKind token

            // An INTEGRAL kind reads its magnitude through `IntWidth.parseBits` — whose
            // `Convert` overload is that width's range check, and which encodes the result
            // into `bits`. The kinds `ofNumericKind` declines are exactly the non-integral
            // ones, and each parses its own way.
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
                    // Not a width fallback: `NumericKind` is a 5-bit field, so a hand-built
                    // token can carry a value outside the declared cases (F# never proves an
                    // enum match exhaustive). That is a corrupt token, not a literal.
                    | _ -> Error NumericLiteralRejection.NotNumeric

            try
                project ()
            with
            | :? OverflowException
            | :? FormatException -> Error NumericLiteralRejection.OutOfRange
