namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open XParsec.FSharp

// Post-lexing INTERPRETATION of a numeric-literal token: its value. Not lexing —
// the token has already been classified (its `NumericBase` / `NumericKind` flags
// carry the radix and the width), and this reads those flags rather than
// re-sniffing the `0x`/`0o`/`0b` prefix or the `u`/`L`/`uy` suffix grammar. So it
// lives beside `Lexing`, not in it.

/// A numeric literal's parsed value: one case per INTEGRAL/REAL width F# can
/// write, so no width folds onto another and none can silently truncate. The
/// semantic layer's `TConstValue` mirrors this case-for-case (its case IS the
/// constant's width — the codebase has no separate `IntWidth` type).
/// `NumericLiterals.tryParseNumericLiteral` is the canonical producer.
[<RequireQualifiedAccess>]
type NumericLiteralValue =
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | NativeInt of nativeint
    | UNativeInt of unativeint
    | Float of double
    | Float32 of single
    | Decimal of decimal

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
    /// `ValueNone` is the single "not a primitive constant" answer, and it is never
    /// a truncation:
    ///   - `token` is not a numeric literal at all;
    ///   - the literal is a CUSTOM numeric literal (`52I` / `52Q` / …) — a
    ///     syntactic translation to a `NumericLiteral<suffix>.From*` CALL, so no
    ///     constant can hold it (see `custom-numeric-literals-plan.md`);
    ///   - the lexed magnitude/sign does not fit the width the lexer classified
    ///     (a negative-signed unsigned literal `-1uy`, formed by the
    ///     negative-literal merge, or an out-of-range magnitude) — the underlying
    ///     `Convert`/`Parse` overflow is caught rather than thrown.
    /// The consumer (`FreezeLiterals.tryParseConst`) turns it into a diagnostic.
    let tryParseNumericLiteral (token: Token) (text: string) : NumericLiteralValue voption =
        if not (TokenInfo.isNumeric token) then
            ValueNone
        else
            let numBase = TokenInfo.numericBase token
            let radix = baseRadix numBase
            let inline digits () = intDigits numBase text

            // Exhaustive over the WIDTHS `NumericKind` declares: every one has its own
            // `NumericLiteralValue` case, so a width can never fold onto a narrower one
            // (`10UL` folded onto `Int32` is how a uint64 literal used to lose its high
            // 32 bits). The residual arm is NOT a width fallback — a `NumericKind` is a
            // 5-bit field, so a hand-built token can carry a value outside the declared
            // cases (F# never proves an enum match exhaustive), and that is no literal.
            let project () =
                match TokenInfo.numericKind token with
                | NumericKind.SByte -> ValueSome(NumericLiteralValue.SByte(Convert.ToSByte(digits (), radix)))
                | NumericKind.Byte -> ValueSome(NumericLiteralValue.Byte(Convert.ToByte(digits (), radix)))
                | NumericKind.Int16 -> ValueSome(NumericLiteralValue.Int16(Convert.ToInt16(digits (), radix)))
                | NumericKind.UInt16 -> ValueSome(NumericLiteralValue.UInt16(Convert.ToUInt16(digits (), radix)))
                | NumericKind.Int32 -> ValueSome(NumericLiteralValue.Int32(Convert.ToInt32(digits (), radix)))
                | NumericKind.UInt32 -> ValueSome(NumericLiteralValue.UInt32(Convert.ToUInt32(digits (), radix)))
                | NumericKind.Int64 -> ValueSome(NumericLiteralValue.Int64(Convert.ToInt64(digits (), radix)))
                | NumericKind.UInt64 -> ValueSome(NumericLiteralValue.UInt64(Convert.ToUInt64(digits (), radix)))
                // `nativeint` / `unativeint` are pointer-width: parse the magnitude at 64
                // bits (the width of the runtimes we target) and reinterpret.
                | NumericKind.NativeInt ->
                    ValueSome(NumericLiteralValue.NativeInt(nativeint (Convert.ToInt64(digits (), radix))))
                | NumericKind.UNativeInt ->
                    ValueSome(NumericLiteralValue.UNativeInt(unativeint (Convert.ToUInt64(digits (), radix))))
                | NumericKind.IEEE64 ->
                    ValueSome(NumericLiteralValue.Float(Double.Parse(text, CultureInfo.InvariantCulture)))
                | NumericKind.IEEE32 ->
                    ValueSome(
                        NumericLiteralValue.Float32(Single.Parse(stripSuffix "f" text, CultureInfo.InvariantCulture))
                    )
                | NumericKind.Decimal ->
                    ValueSome(
                        NumericLiteralValue.Decimal(
                            Decimal.Parse(stripSuffix "m" text, NumberStyles.Float, CultureInfo.InvariantCulture)
                        )
                    )
                // The `Q`/`R`/`Z`/`I`/`N`/`G` suffixes are F#'s CUSTOM NUMERIC LITERALS,
                // not primitive constants: `52I` means `NumericLiteralI.FromInt32 52`, a
                // call into whatever `NumericLiteral<suffix>` module is in scope, typed by
                // that module's return type. There is no constant to project, by
                // construction — the desugaring is a separate feature
                // (`custom-numeric-literals-plan.md`). `ReservedNumericLiteral` is a
                // suffix F# reserves and defines no meaning for, so likewise no value.
                | NumericKind.BigIntegerQ
                | NumericKind.BigIntegerR
                | NumericKind.BigIntegerZ
                | NumericKind.BigIntegerI
                | NumericKind.BigIntegerN
                | NumericKind.BigIntegerG
                | NumericKind.ReservedNumericLiteral -> ValueNone
                | _ -> ValueNone

            try
                project ()
            with
            | :? OverflowException
            | :? FormatException -> ValueNone
