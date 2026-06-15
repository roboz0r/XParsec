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
            | Token.CharLiteral -> TConstValue.Char(parseCharLiteral text)
            | _ ->
                // Every remaining literal token is numeric (`Constant.Literal`
                // admits only numeric / bool / char — `ConstantParsing.isLiteralToken`).
                // The lexer owns the radix + suffix grammar via
                // `Lexing.tryParseNumericLiteral` (keyed off the token's classified
                // base/width), so Freeze just projects the value onto `TConstValue`.
                match Lexing.tryParseNumericLiteral t.Token text with
                | ValueSome(NumericLiteralValue.Int32 n) -> TConstValue.Int n
                | ValueSome(NumericLiteralValue.UInt32 n) -> TConstValue.UInt n
                | ValueSome(NumericLiteralValue.Int64 n) -> TConstValue.Int64 n
                | ValueSome(NumericLiteralValue.Byte n) -> TConstValue.Byte n
                | ValueSome(NumericLiteralValue.Float n) -> TConstValue.Float n
                | ValueSome(NumericLiteralValue.Float32 n) -> TConstValue.Float32 n
                | ValueSome(NumericLiteralValue.Decimal n) -> TConstValue.Decimal n
                | ValueNone -> failwithf "Freeze.parseConst: non-literal token %A in constant position" t.Token

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
