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
type internal LiteralRejection =
    /// A custom numeric literal (`52I`): a call into a `NumericLiteral<suffix>` module, so
    /// there is no constant to project, by construction.
    | CustomLiteral
    /// The magnitude or sign does not fit the authored kind: `300uy`, or the negative
    /// unsigned `-1uy` the lexer's negative-literal merge forms.
    | OutOfRange

module internal ElaborateLiterals =

    /// Projection of a constant literal onto `TConstValue`, never a truncation. Bool / char
    /// / well-formed primitive numeric literals always resolve; a consumer that can report a
    /// user error (enum case values) calls this rather than the throwing `parseConst`.
    let tryParseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : Result<TConstValue, LiteralRejection> =
        match ConstFold.tryLiteral ctx.NameOf c with
        | Ok v -> Ok v
        | Error NumericLiteralRejection.CustomLiteral -> Error LiteralRejection.CustomLiteral
        | Error NumericLiteralRejection.OutOfRange -> Error LiteralRejection.OutOfRange
        | Error NumericLiteralRejection.NotNumeric ->
            // `Constant.Literal` admits only numeric / bool / char, so a `NotNumeric`
            // here is a producer bug and throws rather than reaching the result type.
            let t =
                match c with
                | Constant.Literal t
                | Constant.MeasuredLiteral(value = t) -> t

            failwithf "Elaborate.tryParseConst: %A is not a literal token" t.Token

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
