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

    /// Decode one backslash escape body (`inner` starts with `\\`) to its char.
    /// The escape set mirrors the lexer's `pCharChar` (Lexing.fs) exactly — a
    /// literal that reaches here already lexed clean, so any unexpected shape is a
    /// broken invariant. Shared by `parseCharLiteral` (a `'\n'` char literal) and
    /// `foldStringParts` (a `\n` *string*-part escape) so the two never diverge.
    let private decodeEscape (inner: string) : char =
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
        | other -> failwithf "Freeze.decodeEscape: unsupported escape '\\%c' in %s" other inner

    /// A char literal that reaches here already lexed clean; decode its (possibly
    /// escaped) single character.
    let private parseCharLiteral (text: string) : char =
        let inner = text.Substring(1, text.Length - 2)

        if inner.Length = 1 then
            inner.[0]
        elif inner.Length >= 2 && inner.[0] = '\\' then
            decodeEscape inner
        else
            failwithf "Freeze.parseCharLiteral: unexpected char literal text %s" text

    /// Total projection of a constant literal onto `TConstValue`. `ValueNone` for
    /// a numeric literal whose lexed text its classified width cannot represent —
    /// notably a negative-signed *unsigned* literal (`-1uy`/`-1u`, formed by the
    /// lexer's negative-literal merge) or an out-of-range magnitude. Bool / char /
    /// well-formed numeric literals always resolve. The throwing `parseConst`
    /// wrapper retains the old "broken invariant" contract for callers that have
    /// no diagnostic channel; consumers that can report a user error (enum case
    /// values) call this directly.
    let tryParseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue voption =
        let parseLiteral (t: SyntaxToken) : TConstValue voption =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> ValueSome(TConstValue.Bool true)
            | Token.KWFalse -> ValueSome(TConstValue.Bool false)
            | Token.CharLiteral -> ValueSome(TConstValue.Char(parseCharLiteral text))
            | _ ->
                // Every remaining literal token is numeric (`Constant.Literal`
                // admits only numeric / bool / char — `ConstantParsing.isLiteralToken`).
                // The lexer owns the radix + suffix grammar via
                // `Lexing.tryParseNumericLiteral` (keyed off the token's classified
                // base/width), so Freeze just projects the value onto `TConstValue`.
                // A non-representable literal yields `ValueNone` (the lexer parse is
                // total — it no longer throws on a negative unsigned / overflow).
                match Lexing.tryParseNumericLiteral t.Token text with
                | ValueSome(NumericLiteralValue.Int32 n) -> ValueSome(TConstValue.Int n)
                | ValueSome(NumericLiteralValue.UInt32 n) -> ValueSome(TConstValue.UInt n)
                | ValueSome(NumericLiteralValue.Int64 n) -> ValueSome(TConstValue.Int64 n)
                | ValueSome(NumericLiteralValue.Byte n) -> ValueSome(TConstValue.Byte n)
                | ValueSome(NumericLiteralValue.Float n) -> ValueSome(TConstValue.Float n)
                | ValueSome(NumericLiteralValue.Float32 n) -> ValueSome(TConstValue.Float32 n)
                | ValueSome(NumericLiteralValue.Decimal n) -> ValueSome(TConstValue.Decimal n)
                | ValueNone -> ValueNone

        match c with
        | Constant.Literal t -> parseLiteral t
        | Constant.MeasuredLiteral(value = t) -> parseLiteral t

    let parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        match tryParseConst ctx c with
        | ValueSome v -> v
        | ValueNone ->
            let t =
                match c with
                | Constant.Literal t
                | Constant.MeasuredLiteral(value = t) -> t

            failwithf "Freeze.parseConst: non-representable literal %A in constant position" t.Token

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
            // The parser folds every string fragment — including escape-sequence
            // tokens (`\n`, `\t`, `\"`, `\uXXXX`) — into a `StringPart.Text`
            // carrying the raw 2+-char source span (`ctx.NameOf` = `\n`, two
            // chars). Decode an escape *token* to the single char it denotes; a
            // plain text fragment appends verbatim. Without this a literal `"\n"`
            // value would emit a backslash-n, not a newline.
            | StringPart.Text t ->
                match t.Token with
                | Token.EscapeSequence -> sb.Append(decodeEscape (ctx.NameOf t)) |> ignore
                | _ -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.EscapeSequence t -> sb.Append(decodeEscape (ctx.NameOf t)) |> ignore
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
