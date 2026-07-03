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

    /// Backslash-escape and string-part folding moved to the shared
    /// `StringLiterals` module (ahead of the passes) so the enum-case reader in
    /// NameResolution shares the identical decoding; re-exported here under the
    /// historical `FreezeLiterals` names the Freeze/Elaborate call sites still use.
    let private decodeEscape = StringLiterals.decodeEscape

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
    /// only in how a hole renders. (Lives in `StringLiterals`; re-exported here.)
    let foldStringParts = StringLiterals.foldStringParts

    /// Stitch a value-level `Expr.ILIntrinsic` instruction string (e.g.
    /// `(# "ceq" … #)` → `"ceq"`), trimming surrounding whitespace. Mirrors
    /// `NameResolution.ilIntrinsicString` for the type-level intrinsic.
    let stitchIlInstruction (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        (foldStringParts ctx (fun () -> "") parts).Trim()
