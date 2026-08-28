namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Why an expression does not fold to a `TConstValue` under the attribute-argument constant
/// domain.
[<RequireQualifiedAccess>]
type ConstRejection =
    /// A custom numeric literal (`52I`): a call into a `NumericLiteral<suffix>` module, so
    /// there is no constant.
    | CustomLiteral
    /// The magnitude or sign does not fit the authored kind (`300uy`, the merged `-1uy`).
    | OutOfRange
    /// `$"…"`: an interpolated string has no constant value.
    | InterpolatedString
    /// `- 1u`: negation is defined on the signed kinds only.
    | NegativeUnsigned
    /// A bitwise operator whose operands are not integral constants of one `IntKind`.
    | KindMismatch
    /// Outside the domain: a call, an identifier that does not resolve to a named constant,
    /// string concatenation, any other expression form.
    | NotConstant

/// A folded constant with the enum identity the expression retained: `ValueSome` for an enum
/// case and for a bitwise combination of cases of one enum, `ValueNone` for every other
/// constant.
type FoldedConst =
    {
        Value: TConstValue
        EnumKey: TypeKey voption
    }

/// The attribute-argument constant domain: a literal, a named-constant reference (an enum
/// case or a `[<Literal>]` value), `|||`/`&&&`/`^^^` on two integral constants of one
/// `IntKind`, unary minus on a numeric constant, and grouping parens.
module ConstFold =

    /// The diagnostic a rejected fold reports, at the expression's first token.
    let rejectionKind (r: ConstRejection) : Kind =
        match r with
        | ConstRejection.CustomLiteral ->
            Kind.Message
                "A constant expression cannot be a custom numeric literal ('52I'); it is a call to a NumericLiteral module, not a constant"
        | ConstRejection.OutOfRange ->
            Kind.Message
                "This constant is not representable at its authored width (a negative value has no unsigned representation)"
        | ConstRejection.InterpolatedString -> Kind.Message "A constant expression cannot be an interpolated string"
        | ConstRejection.NegativeUnsigned ->
            Kind.Message "A negative constant has no unsigned representation; use a signed integer width"
        | ConstRejection.KindMismatch ->
            Kind.Message "Bitwise operands of a constant expression must be integral constants of one width"
        | ConstRejection.NotConstant -> Kind.NotConstantExpression

    /// A char literal arrives lexed clean; decode its (possibly escaped) single character.
    let private parseCharLiteral (text: string) : char =
        let inner = text.Substring(1, text.Length - 2)

        if inner.Length = 1 then
            inner.[0]
        else
            match Lexing.decodeCharEscape inner with
            | ValueSome c -> c
            | ValueNone -> failwithf "ConstFold.parseCharLiteral: unexpected char literal text %s" text

    /// Projection of a bool / char / numeric `Constant` onto `TConstValue`, never a
    /// truncation. Shared by the enum-case reader and the elaborator's literal projection.
    let tryLiteral
        (nameOf: SyntaxToken -> string)
        (c: Constant<SyntaxToken>)
        : Result<TConstValue, NumericLiteralRejection> =
        let t =
            match c with
            | Constant.Literal t
            | Constant.MeasuredLiteral(value = t) -> t

        let text = nameOf t

        match t.Token with
        | Token.KWTrue -> Ok(TConstValue.Bool true)
        | Token.KWFalse -> Ok(TConstValue.Bool false)
        | Token.CharLiteral -> Ok(TConstValue.Char(parseCharLiteral text))
        | _ ->
            match NumericLiterals.parseNumericLiteral t.Token text with
            | Ok(NumericLiteralValue.Integral(k, bits)) -> Ok(TConstValue.Integral(k, bits))
            | Ok(NumericLiteralValue.Float n) -> Ok(TConstValue.Float n)
            | Ok(NumericLiteralValue.Float32 n) -> Ok(TConstValue.Float32 n)
            | Ok(NumericLiteralValue.Decimal n) -> Ok(TConstValue.Decimal n)
            | Error e -> Error e

    let private plain (v: TConstValue) : FoldedConst = { Value = v; EnumKey = ValueNone }

    /// `onInvalid` receives a string-escape verdict at its token. `tryNamedConstant` resolves
    /// an identifier's segments (`Mask`, `E.C`, `Path.M.Mask`) to the constant they denote:
    /// an enum case's underlying literal, or a `[<Literal>]` value's folded constant.
    let rec tryConstant
        (nameOf: SyntaxToken -> string)
        (onInvalid: SyntaxToken -> Kind -> unit)
        (tryNamedConstant: ImmutableArray<SyntaxToken> -> FoldedConst voption)
        (e: Expr<SyntaxToken>)
        : Result<FoldedConst, ConstRejection> =
        match e with
        // A value-grouping paren is not itself the constant; peel it.
        | Expr.EnclosedBlock(expr = inner) -> tryConstant nameOf onInvalid tryNamedConstant inner
        | Expr.EmptyBlock _ -> Ok(plain TConstValue.Unit)
        | Expr.Const c ->
            match tryLiteral nameOf c with
            | Ok v -> Ok(plain v)
            | Error NumericLiteralRejection.CustomLiteral -> Error ConstRejection.CustomLiteral
            | Error NumericLiteralRejection.OutOfRange -> Error ConstRejection.OutOfRange
            // `Constant.Literal` admits only numeric / bool / char tokens, so `NotNumeric`
            // is unreachable on a parsed tree; refuse rather than throw.
            | Error NumericLiteralRejection.NotNumeric -> Error ConstRejection.NotConstant
        // Plain / verbatim / triple-quoted strings are constants; `$"…"` is the one `String`
        // kind that is not.
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            Ok(plain (TConstValue.String(StringLiterals.foldStringParts nameOf (fun () -> "") onInvalid parts)))
        | Expr.String _ -> Error ConstRejection.InterpolatedString
        // A named-constant reference: `Mask` (a `[<Literal>]` value), `AttributeTargets.Class`
        // (an enum case), or a qualified spelling of either.
        | Expr.Ident tok ->
            match tryNamedConstant (ImmutableArray.Create tok) with
            | ValueSome v -> Ok v
            | ValueNone -> Error ConstRejection.NotConstant
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            match tryNamedConstant li.Idents with
            | ValueSome v -> Ok v
            | ValueNone -> Error ConstRejection.NotConstant
        // The lexer merges `-` into an ADJACENT numeric when what precedes it cannot be a
        // left operand, so `-1` arrives above as one literal. This arm is what the merge
        // misses: the spaced `- 1` and `-(1)`.
        | Expr.PrefixApp(op, operand) when op.Token = Token.OpSubtraction ->
            match tryConstant nameOf onInvalid tryNamedConstant operand with
            // Negation wraps AT THE WIDTH: `-(-128y)` stays `-128y`.
            | Ok {
                     Value = TConstValue.Integral(k, bits)
                 } when IntKind.isSigned k -> Ok(plain (TConstValue.Integral(k, IntKind.negate k bits)))
            | Ok { Value = TConstValue.Integral _ } -> Error ConstRejection.NegativeUnsigned
            | Ok { Value = TConstValue.Float n } -> Ok(plain (TConstValue.Float(-n)))
            | Ok { Value = TConstValue.Float32 n } -> Ok(plain (TConstValue.Float32(-n)))
            | Ok { Value = TConstValue.Decimal n } -> Ok(plain (TConstValue.Decimal(-n)))
            // `-"abc"`, `-true`: nothing negatable came back.
            | Ok _ -> Error ConstRejection.NotConstant
            | Error e -> Error e
        | Expr.InfixApp(leftExpr = left; infixOp = op; rightExpr = right) ->
            match op.Token with
            | Token.OpBitwiseOr
            | Token.OpBitwiseAnd
            | Token.OpExclusiveOr ->
                let fold = tryConstant nameOf onInvalid tryNamedConstant

                match fold left, fold right with
                | Ok {
                         Value = TConstValue.Integral(lk, lb)
                         EnumKey = lKey
                     },
                  Ok {
                         Value = TConstValue.Integral(rk, rb)
                         EnumKey = rKey
                     } ->
                    if lk = rk then
                        // `bits` is 64-bit-extended at the kind's signedness, and the three
                        // ops are closed over that extension, so no re-normalisation.
                        let bits =
                            match op.Token with
                            | Token.OpBitwiseOr -> lb ||| rb
                            | Token.OpBitwiseAnd -> lb &&& rb
                            | _ -> lb ^^^ rb

                        Ok
                            {
                                Value = TConstValue.Integral(lk, bits)
                                // A combination stays within one enum; mixed identities fold
                                // to a bare integral.
                                EnumKey = if lKey = rKey then lKey else ValueNone
                            }
                    else
                        Error ConstRejection.KindMismatch
                | Ok _, Ok _ -> Error ConstRejection.KindMismatch
                | Error e, _
                | _, Error e -> Error e
            | _ -> Error ConstRejection.NotConstant
        | _ -> Error ConstRejection.NotConstant
