namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Why an enum case's value expression names no `TEnumLiteral`. These are genuinely
/// different answers: `52I` is not an out-of-range magnitude.
[<RequireQualifiedAccess>]
type internal EnumCaseRejection =
    /// The magnitude or sign does not fit the authored width (`| A = 300uy`).
    | NotRepresentable
    /// `| A = 52I`: a call into a `NumericLiteral` module, so there is no constant.
    | CustomLiteral
    /// A constant that is neither integral nor a string (`true`, `'c'`, `1.0`, `1m`), or an
    /// integer at pointer width, which no enum may be based on. Carries the source spelling.
    | NotAnEnumConstant of spelling: string
    /// `| A = $"…"`: an interpolated string has no constant value.
    | InterpolatedString
    /// `| A = - 1uy`: negation is defined on the signed widths only.
    | NegativeUnsigned
    /// Not a literal at all — `| A = 1 + 1`, `| A = B`.
    | NotConstant

// `| C = v` → the case's compile-time literal. The `.fsi` package extractor and the Elaborate
// pass both read this grammar; reading it twice would let the two drift.

module internal EnumCaseValues =

    let rec tryResolve
        (nameOf: SyntaxToken -> string)
        (v: Expr<SyntaxToken>)
        : Result<TEnumLiteral, EnumCaseRejection> =
        match v with
        // A value-grouping paren (`| C = (1)`) is not itself the constant; peel it.
        | Expr.EnclosedBlock(expr = inner) -> tryResolve nameOf inner
        | Expr.Const c ->
            let t =
                match c with
                | Constant.Literal t
                | Constant.MeasuredLiteral(value = t) -> t

            match NumericLiterals.parseNumericLiteral t.Token (nameOf t) with
            // `isEnumBase` excludes exactly the pointer pair.
            | Ok(NumericLiteralValue.Integral(w, bits)) when IntWidth.isEnumBase w ->
                Ok(TEnumLiteral.Int(TConstValue.Integral(w, bits)))
            | Error NumericLiteralRejection.CustomLiteral -> Error EnumCaseRejection.CustomLiteral
            | Error NumericLiteralRejection.OutOfRange -> Error EnumCaseRejection.NotRepresentable
            // `NotNumeric` is the bool / char token `Constant.Literal` also admits.
            | Error NumericLiteralRejection.NotNumeric
            | Ok _ -> Error(EnumCaseRejection.NotAnEnumConstant(nameOf t))
        // Plain / verbatim / triple-quoted strings are constants; `$"…"` is the one `String`
        // kind that is not.
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            Ok(TEnumLiteral.String(StringLiterals.foldStringParts nameOf (fun () -> "") parts))
        | Expr.String _ -> Error EnumCaseRejection.InterpolatedString
        // The lexer merges `-` into an ADJACENT numeric when what precedes it cannot be a left
        // operand, so `| A = -1` arrives above as one literal. This arm is what the merge
        // misses: the spaced `| A = - 1` and `| A = -(1)`.
        | Expr.PrefixApp(op, operand) when op.Token = Token.OpSubtraction ->
            match tryResolve nameOf operand with
            // Negation wraps AT THE WIDTH: `-(-128y)` stays `-128y`.
            | Ok(TEnumLiteral.Int(TConstValue.Integral(w, bits))) when IntWidth.isSigned w ->
                Ok(TEnumLiteral.Int(TConstValue.Integral(w, IntWidth.negate w bits)))
            | Ok(TEnumLiteral.Int(TConstValue.Integral _)) -> Error EnumCaseRejection.NegativeUnsigned
            // `-"abc"` or a deeper non-int form: nothing negatable came back.
            | Ok(TEnumLiteral.String _)
            | Ok(TEnumLiteral.Int _) -> Error EnumCaseRejection.NotConstant
            | Error e -> Error e
        | _ -> Error EnumCaseRejection.NotConstant
