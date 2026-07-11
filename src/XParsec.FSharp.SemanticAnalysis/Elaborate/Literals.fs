namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Constant / string-literal parsing primitives for the Elaborate pass. No dependency
// on the recursive `translateExpr`; shared by the pattern projection
// (`ElaboratePatterns`) and the expression projection (`ElaborateExpr`).

/// Why a constant literal names no `TConstValue` — the reasons a USER can actually cause,
/// and so exactly the ones a consumer with a diagnostic channel must distinguish (`52I` is
/// not an out-of-range magnitude).
///
/// This is the lexer's `NumericLiteralRejection` MINUS `NotNumeric`, which `tryParseConst`
/// discharges by throwing: `Constant.Literal` admits only numeric / bool / char, so a
/// non-numeric token there is a producer bug, not an answer. Narrowing the type is what
/// keeps that fact out of every caller's match — an arm for an impossible case is an arm
/// nobody can reason about, and the one that used to exist silently dropped the diagnostic.
[<RequireQualifiedAccess>]
type internal ConstRejection =
    /// A custom numeric literal (`52I`): a call into a `NumericLiteral<suffix>` module, so
    /// there is no constant to project, by construction.
    | CustomLiteral
    /// The magnitude or sign does not fit the authored width — `300uy`, or the negative
    /// unsigned `-1uy` the lexer's negative-literal merge forms.
    | OutOfRange

module internal ElaborateLiterals =

    /// Backslash-escape and string-part folding moved to the shared
    /// `StringLiterals` module (ahead of the passes) so the enum-case reader in
    /// NameResolution shares the identical decoding; re-exported here under the
    /// `ElaborateLiterals` names this pass's call sites already use.
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
            failwithf "Elaborate.parseCharLiteral: unexpected char literal text %s" text

    /// Total projection of a constant literal onto `TConstValue`. `Error` carries WHY there
    /// is no constant (`ConstRejection`) — never a truncation, and never merely "no", so a
    /// consumer with a diagnostic channel says the right thing.
    ///
    /// Bool / char / well-formed primitive numeric literals always resolve. The throwing
    /// `parseConst` wrapper retains the "broken invariant" contract for callers that have
    /// no diagnostic channel; consumers that can report a user error (enum case values)
    /// call this directly.
    let tryParseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : Result<TConstValue, ConstRejection> =
        let parseLiteral (t: SyntaxToken) : Result<TConstValue, ConstRejection> =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> Ok(TConstValue.Bool true)
            | Token.KWFalse -> Ok(TConstValue.Bool false)
            | Token.CharLiteral -> Ok(TConstValue.Char(parseCharLiteral text))
            | _ ->
                // Every remaining literal token is numeric (`Constant.Literal` admits only
                // numeric / bool / char — `ConstantParsing.isLiteralToken`), so `NotNumeric`
                // here is a producer bug, not a user error: it throws, as `parseCharLiteral`'s
                // malformed-text arm does, and DOES NOT reach the result type. That is what
                // narrowing to `ConstRejection` states.
                //
                // The numeric-literal reader owns the radix + suffix grammar
                // (`NumericLiterals.parseNumericLiteral`, keyed off the token's classified
                // base/width) and hands back the width as an `IntWidth` witness, so the value
                // carries across untouched. There is no width mapping here to get wrong,
                // because both models key off the same `IntWidth`.
                match NumericLiterals.parseNumericLiteral t.Token text with
                | Ok(NumericLiteralValue.Integral(w, bits)) -> Ok(TConstValue.Integral(w, bits))
                | Ok(NumericLiteralValue.Float n) -> Ok(TConstValue.Float n)
                | Ok(NumericLiteralValue.Float32 n) -> Ok(TConstValue.Float32 n)
                | Ok(NumericLiteralValue.Decimal n) -> Ok(TConstValue.Decimal n)
                | Error NumericLiteralRejection.CustomLiteral -> Error ConstRejection.CustomLiteral
                | Error NumericLiteralRejection.OutOfRange -> Error ConstRejection.OutOfRange
                | Error NumericLiteralRejection.NotNumeric ->
                    failwithf "Elaborate.tryParseConst: %A is not a literal token" t.Token

        match c with
        | Constant.Literal t -> parseLiteral t
        | Constant.MeasuredLiteral(value = t) -> parseLiteral t

    let parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        match tryParseConst ctx c with
        | Ok v -> v
        | Error reason ->
            let t =
                match c with
                | Constant.Literal t
                | Constant.MeasuredLiteral(value = t) -> t

            failwithf "Elaborate.parseConst: non-representable literal %A (%A) in constant position" t.Token reason

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
