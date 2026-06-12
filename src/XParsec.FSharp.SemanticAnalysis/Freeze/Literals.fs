namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Constant / string-literal parsing primitives for the Freeze pass. No dependency
// on the recursive `translateExpr`; shared by the pattern projection
// (`FreezePatterns`) and the expression projection (`FreezeExpr`).

module internal FreezeLiterals =

    /// Only strip the suffixes that map to literal kinds Unification recognises;
    /// the remainder is fed to the corresponding BCL parser.
    let private stripSuffix (suffix: string) (text: string) =
        if text.EndsWith(suffix, System.StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    /// The escape set mirrors the lexer's `pCharChar` (Lexing.fs) exactly — a
    /// char literal that reaches here already lexed clean, so any unexpected
    /// shape is a broken invariant.
    let private parseCharLiteral (text: string) : char =
        let inner = text.Substring(1, text.Length - 2)

        if inner.Length = 1 then
            inner.[0]
        elif inner.Length >= 2 && inner.[0] = '\\' then
            match inner.[1] with
            | '"' -> '"'
            | '\\' -> '\\'
            | '\'' -> '\''
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'b' -> '\b'
            | 'r' -> '\r'
            | 'a' -> '\a'
            | 'f' -> '\f'
            | 'v' -> '\v'
            | 'u' ->
                char (
                    System.UInt16.Parse(
                        inner.Substring(2, 4),
                        System.Globalization.NumberStyles.AllowHexSpecifier,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                )
            | 'x' ->
                char (
                    System.Byte.Parse(
                        inner.Substring(2, 2),
                        System.Globalization.NumberStyles.AllowHexSpecifier,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                )
            | d when System.Char.IsDigit d ->
                // Trigraph `\DDD` (decimal byte).
                char (System.Int32.Parse(inner.Substring(1, 3), System.Globalization.CultureInfo.InvariantCulture))
            | other -> failwithf "Freeze.parseCharLiteral: unsupported char escape '\\%c' in %s" other text
        else
            failwithf "Freeze.parseCharLiteral: unexpected char literal text %s" text

    let parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        let parseLiteral (t: SyntaxToken) : TConstValue =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> TConstValue.Bool true
            | Token.KWFalse -> TConstValue.Bool false
            | Token.NumIEEE64
            | Token.NumIEEE64Hex
            | Token.NumIEEE64Octal
            | Token.NumIEEE64Binary ->
                TConstValue.Float(System.Double.Parse(text, System.Globalization.CultureInfo.InvariantCulture))
            | Token.NumIEEE32
            | Token.NumIEEE32Hex
            | Token.NumIEEE32Octal
            | Token.NumIEEE32Binary ->
                // `f` suffix (case-insensitive via `stripSuffix`) stripped before
                // the invariant-culture single parse — the float32 twin of the
                // `float` arm above.
                TConstValue.Float32(
                    System.Single.Parse(stripSuffix "f" text, System.Globalization.CultureInfo.InvariantCulture)
                )
            | Token.NumInt64
            | Token.NumInt64Hex
            | Token.NumInt64Octal
            | Token.NumInt64Binary -> TConstValue.Int64(System.Int64.Parse(stripSuffix "L" text))
            | Token.NumByte
            | Token.NumByteHex
            | Token.NumByteOctal
            | Token.NumByteBinary -> TConstValue.Byte(System.Byte.Parse(stripSuffix "uy" text))
            | Token.CharLiteral -> TConstValue.Char(parseCharLiteral text)
            | Token.NumDecimal
            | Token.NumDecimalHex
            | Token.NumDecimalOctal
            | Token.NumDecimalBinary ->
                // Remainder is an invariant-culture decimal. Matches
                // `literalCarrier`'s `tyDecimal`.
                TConstValue.Decimal(
                    System.Decimal.Parse(
                        stripSuffix "M" text,
                        System.Globalization.NumberStyles.Float,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                )
            | _ ->
                // NumInt32 family and anything Unification hasn't classified are
                // treated as plain ints.
                TConstValue.Int(Int32.Parse text)

        match c with
        | Constant.Literal t -> parseLiteral t
        | Constant.MeasuredLiteral(value = t) -> parseLiteral t

    /// Concatenate the literal text of every string part via `ctx.NameOf`,
    /// rendering an interpolation hole (`StringPart.Expr`) through `onHole`.
    /// Shared by the IL-intrinsic and literal-string stitchers, which differ
    /// only in how a hole renders.
    let foldStringParts
        (ctx: PassContext)
        (onHole: unit -> string)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.Expr _ -> sb.Append(onHole ()) |> ignore

        sb.ToString()

    /// Stitch a value-level `Expr.ILIntrinsic` instruction string (e.g.
    /// `(# "ceq" … #)` → `"ceq"`), trimming surrounding whitespace. Mirrors
    /// `NameResolution.ilIntrinsicString` for the type-level intrinsic.
    let stitchIlInstruction (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        (foldStringParts ctx (fun () -> "") parts).Trim()
