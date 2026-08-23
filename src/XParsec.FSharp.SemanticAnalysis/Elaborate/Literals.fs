namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Constant / string-literal parsing primitives for the Elaborate pass. No dependency on the
// recursive `translateExpr`, so both the pattern and the expression projection share them.

/// Why a constant literal does not project to a `TConstValue`: the reasons a USER can cause, so a
/// consumer with a diagnostic channel can distinguish them (`52I` is not an out-of-range
/// magnitude). The lexer's `NumericLiteralRejection` minus the `NotNumeric` that throws.
[<RequireQualifiedAccess>]
type internal ConstRejection =
    /// A custom numeric literal (`52I`): a call into a `NumericLiteral<suffix>` module, so
    /// there is no constant to project, by construction.
    | CustomLiteral
    /// The magnitude or sign does not fit the authored kind: `300uy`, or the negative
    /// unsigned `-1uy` the lexer's negative-literal merge forms.
    | OutOfRange

module internal ElaborateLiterals =

    /// A char literal that reaches here already lexed clean; decode its (possibly escaped)
    /// single character.
    let private parseCharLiteral (text: string) : char =
        let inner = text.Substring(1, text.Length - 2)

        if inner.Length = 1 then
            inner.[0]
        else
            match Lexing.decodeCharEscape inner with
            | ValueSome c -> c
            | ValueNone -> failwithf "Elaborate.parseCharLiteral: unexpected char literal text %s" text

    /// Projection of a constant literal onto `TConstValue`, never a truncation. Bool / char
    /// / well-formed primitive numeric literals always resolve; a consumer that can report a
    /// user error (enum case values) calls this rather than the throwing `parseConst`.
    let tryParseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : Result<TConstValue, ConstRejection> =
        let parseLiteral (t: SyntaxToken) : Result<TConstValue, ConstRejection> =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> Ok(TConstValue.Bool true)
            | Token.KWFalse -> Ok(TConstValue.Bool false)
            | Token.CharLiteral -> Ok(TConstValue.Char(parseCharLiteral text))
            | _ ->
                // `Constant.Literal` admits only numeric / bool / char, so a `NotNumeric`
                // here is a producer bug and throws rather than reaching the result type.
                // The reader hands the kind back as an `IntKind`, which rides through unmapped.
                match NumericLiterals.parseNumericLiteral t.Token text with
                | Ok(NumericLiteralValue.Integral(k, bits)) -> Ok(TConstValue.Integral(k, bits))
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

    /// Concatenate the literal text of every string part, rendering an interpolation hole
    /// (`StringPart.Expr`) through `onHole`, the only thing the IL-intrinsic and
    /// literal-string stitchers differ in. Reports an escape denoting no character
    /// (`"\256"`, `"\U00110000"`) at its token.
    let foldStringParts (ctx: PassContext) (onHole: unit -> string) parts : string =
        StringLiterals.foldStringParts ctx.NameOf onHole (fun t kind -> ctx.Report(t, kind)) parts

    /// Stitch a value-level `Expr.ILIntrinsic` instruction string, trimming surrounding
    /// whitespace: `(# "ceq" … #)` → `"ceq"`.
    let stitchIlInstruction (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        (foldStringParts ctx (fun () -> "") parts).Trim()
