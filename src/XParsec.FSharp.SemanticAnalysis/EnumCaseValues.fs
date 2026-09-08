namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Why an enum case's value expression does not resolve to a `TEnumLiteral`. These are genuinely
/// different rejections: `52I` is not an out-of-range magnitude.
[<RequireQualifiedAccess>]
type internal EnumCaseRejection =
    /// The magnitude or sign does not fit the authored kind (`| A = 300uy`).
    | NotRepresentable
    /// `| A = 52I`: a call into a `NumericLiteral` module, so there is no constant.
    | CustomLiteral
    /// A constant that is neither integral nor a string (`true`, `'c'`, `1.0`, `1m`), or an
    /// integer at pointer width, which no enum may be based on. Carries the source spelling.
    | NotAnEnumConstant of spelling: string
    /// `| A = $"…"`: an interpolated string has no constant value.
    | InterpolatedString
    /// `| A = - 1uy`: negation is defined on the signed kinds only.
    | NegativeUnsigned
    /// `| A = 1 + 1`, `| A = B`: not a literal at all.
    | NotConstant

// `| C = v` → the case's compile-time literal. Read once, at type registration; every later
// pass reads the registered `EnumTypeInfo.Cases`.

module internal EnumCaseValues =

    /// `onInvalid` receives a string-escape verdict (`Kind.EscapeTrigraphOutOfRange` /
    /// `Kind.EscapeNotUnicodeScalar`) at its token.
    let rec private tryResolve
        (nameOf: SyntaxToken -> string)
        (onInvalid: SyntaxToken -> Kind -> unit)
        (v: Expr<SyntaxToken>)
        : Result<TEnumLiteral, EnumCaseRejection> =
        match v with
        // A value-grouping paren (`| C = (1)`) is not itself the constant; peel it.
        | Expr.EnclosedBlock(expr = inner) -> tryResolve nameOf onInvalid inner
        | Expr.Const c ->
            let t =
                match c with
                | Constant.Literal t
                | Constant.MeasuredLiteral(value = t) -> t

            match ConstLiteral.tryValue nameOf c with
            // `isEnumBase` excludes exactly the pointer pair.
            | Ok(TConstValue.Integral(IntValue.NativeInt _))
            | Ok(TConstValue.Integral(IntValue.UNativeInt _)) -> Error(EnumCaseRejection.NotAnEnumConstant(nameOf t))
            | Ok(TConstValue.Integral v) -> Ok(TEnumLiteral.Int(TConstValue.Integral v))
            | Error NumericLiteralRejection.CustomLiteral -> Error EnumCaseRejection.CustomLiteral
            | Error NumericLiteralRejection.OutOfRange -> Error EnumCaseRejection.NotRepresentable
            // The remaining `Ok`s are the bool / char / float / decimal constants
            // `Constant.Literal` also admits, and the pointer-width pair.
            | Error NumericLiteralRejection.NotNumeric
            | Ok _ -> Error(EnumCaseRejection.NotAnEnumConstant(nameOf t))
        // Plain / verbatim / triple-quoted strings are constants; `$"…"` is the one `String`
        // kind that is not.
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            Ok(TEnumLiteral.String(StringLiterals.foldStringParts nameOf (fun () -> "") onInvalid parts))
        | Expr.String _ -> Error EnumCaseRejection.InterpolatedString
        // The lexer merges `-` into an ADJACENT numeric when what precedes it cannot be a left
        // operand, so `| A = -1` arrives above as one literal. This arm is what the merge
        // misses: the spaced `| A = - 1` and `| A = -(1)`.
        | Expr.PrefixApp(op, operand) when op.Token = Token.OpSubtraction ->
            match tryResolve nameOf onInvalid operand with
            // Negation wraps AT THE WIDTH: `-(-128y)` stays `-128y`.
            | Ok(TEnumLiteral.Int(TConstValue.Integral v)) ->
                match IntValue.negate v with
                | ValueSome n -> Ok(TEnumLiteral.Int(TConstValue.Integral n))
                | ValueNone -> Error EnumCaseRejection.NegativeUnsigned
            // `-"abc"` or a deeper non-int form: nothing negatable came back.
            | Ok(TEnumLiteral.String _)
            | Ok(TEnumLiteral.Int _) -> Error EnumCaseRejection.NotConstant
            | Error e -> Error e
        | _ -> Error EnumCaseRejection.NotConstant

    let private rejectionKind (e: EnumCaseRejection) : Kind =
        match e with
        | EnumCaseRejection.NotRepresentable ->
            Kind.Message
                "An enum case value is not representable at its authored width (a negative value has no unsigned representation)"
        | EnumCaseRejection.CustomLiteral ->
            Kind.Message
                "An enum case value must be a primitive integer literal; a custom numeric literal ('52I') is a call to a NumericLiteral module, not a constant"
        | EnumCaseRejection.NotAnEnumConstant spelling ->
            Kind.Message(
                sprintf
                    "An enum case value must be an integer or string literal; '%s' is not a valid enum constant"
                    spelling
            )
        | EnumCaseRejection.InterpolatedString ->
            Kind.Message "An enum case value must be a literal string; an interpolated string is not a constant"
        | EnumCaseRejection.NegativeUnsigned ->
            Kind.Message "A negative enum case value has no unsigned representation; use a signed integer width"
        | EnumCaseRejection.NotConstant -> Kind.EnumCaseNotConstant

    /// `| C = v` as a `TEnumCase`. A rejected value is `ValueNone`, reported at the case
    /// identifier through `report`; a string-escape verdict is reported at its token.
    let resolveCase
        (nameOf: SyntaxToken -> string)
        (report: SyntaxToken -> Kind -> unit)
        (attributes: TAttributes)
        (ident: SyntaxToken)
        (v: Expr<SyntaxToken>)
        : TEnumCase =
        let value =
            match tryResolve nameOf report v with
            | Ok lit -> ValueSome lit
            | Error e ->
                report ident (rejectionKind e)
                ValueNone

        {
            Name = nameOf ident
            Value = value
            Tok = ident
            Attributes = attributes
        }
