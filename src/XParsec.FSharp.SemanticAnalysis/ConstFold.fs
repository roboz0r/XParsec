namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Why an expression does not fold to a `TConstExpr` under the attribute-argument constant
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
            | Ok(NumericLiteralValue.Integral v) -> Ok(TConstValue.Integral v)
            | Ok(NumericLiteralValue.Float n) -> Ok(TConstValue.Float n)
            | Ok(NumericLiteralValue.Float32 n) -> Ok(TConstValue.Float32 n)
            | Ok(NumericLiteralValue.Decimal n) -> Ok(TConstValue.Decimal n)
            | Error e -> Error e

    /// The node a scalar leaf becomes, typed by the value it holds.
    let private literal (tyOfValue: TConstValue -> FrozenType) (tok: SyntaxToken) (v: TConstValue) : TConstExpr =
        TConstExpr.Literal(v, tyOfValue v, Anchor.ofToken tok)

    let private tryBitwiseOp (t: Token) : BitwiseOp voption =
        match t with
        | Token.OpBitwiseOr -> ValueSome BitwiseOp.Or
        | Token.OpBitwiseAnd -> ValueSome BitwiseOp.And
        | Token.OpExclusiveOr -> ValueSome BitwiseOp.Xor
        | _ -> ValueNone

    /// Negation wraps AT THE WIDTH: `-(-128y)` stays `-128y`.
    let private negateScalar (v: TConstValue voption) : Result<TConstValue, ConstRejection> =
        match v with
        | ValueSome(TConstValue.Integral v) ->
            match IntValue.negate v with
            | ValueSome n -> Ok(TConstValue.Integral n)
            | ValueNone -> Error ConstRejection.NegativeUnsigned
        | ValueSome(TConstValue.Float n) -> Ok(TConstValue.Float(-n))
        | ValueSome(TConstValue.Float32 n) -> Ok(TConstValue.Float32(-n))
        | ValueSome(TConstValue.Decimal n) -> Ok(TConstValue.Decimal(-n))
        // `-"abc"`, `-true`: a non-numeric scalar, `null`, a type value or an array.
        | _ -> Error ConstRejection.NotConstant

    /// `l op r` over two integral scalars of one `IntKind`.
    let private combineScalars
        (op: BitwiseOp)
        (l: TConstValue voption)
        (r: TConstValue voption)
        : Result<TConstValue, ConstRejection> =
        match l, r with
        | ValueSome(TConstValue.Integral lv), ValueSome(TConstValue.Integral rv) ->
            match IntValue.bitwise op lv rv with
            | ValueSome n -> Ok(TConstValue.Integral n)
            | ValueNone -> Error ConstRejection.KindMismatch
        | _ -> Error ConstRejection.KindMismatch

    /// A bitwise combination stays within one enum; operands of differing types take the
    /// result value's own type.
    let private combinedTy
        (tyOfValue: TConstValue -> FrozenType)
        (left: TConstExpr)
        (right: TConstExpr)
        (v: TConstValue)
        : FrozenType =
        let lTy = TConstExpr.ty left

        match lTy with
        | FTEnum _ when lTy = TConstExpr.ty right -> lTy
        | _ -> tyOfValue v

    /// `onInvalid` receives a string-escape verdict at its token. `tryNamedConstant` resolves
    /// an identifier's segments (`Mask`, `E.C`, `Path.M.Mask`) to the node it denotes: an enum
    /// case reference, or a `[<Literal>]` value reference.
    let rec tryConstant
        (nameOf: SyntaxToken -> string)
        (onInvalid: SyntaxToken -> Kind -> unit)
        (tyOfValue: TConstValue -> FrozenType)
        (tryNamedConstant: ImmutableArray<SyntaxToken> -> TConstExpr voption)
        (e: Expr<SyntaxToken>)
        : Result<TConstExpr, ConstRejection> =
        let recur = tryConstant nameOf onInvalid tyOfValue tryNamedConstant

        match e with
        // A value-grouping paren is not itself the constant; peel it.
        | Expr.EnclosedBlock(expr = inner) -> recur inner
        | Expr.EmptyBlock _ -> Ok(literal tyOfValue (CstKeys.firstTokenOfExpr e) TConstValue.Unit)
        | Expr.Const c ->
            match tryLiteral nameOf c with
            | Ok v -> Ok(literal tyOfValue (CstKeys.firstTokenOfExpr e) v)
            | Error NumericLiteralRejection.CustomLiteral -> Error ConstRejection.CustomLiteral
            | Error NumericLiteralRejection.OutOfRange -> Error ConstRejection.OutOfRange
            // `Constant.Literal` admits only numeric / bool / char tokens, so `NotNumeric`
            // is unreachable on a parsed tree; refuse rather than throw.
            | Error NumericLiteralRejection.NotNumeric -> Error ConstRejection.NotConstant
        // Plain / verbatim / triple-quoted strings are constants; `$"…"` is the one `String`
        // kind that is not.
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            let s = StringLiterals.foldStringParts nameOf (fun () -> "") onInvalid parts
            Ok(literal tyOfValue (CstKeys.firstTokenOfExpr e) (TConstValue.String s))
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
            recur operand
            |> Result.bind (fun inner ->
                negateScalar (TConstExpr.tryScalar inner)
                |> Result.map (fun v ->
                    TConstExpr.Unary(
                        RuntimeNames.unaryNegationBindingKey,
                        inner,
                        TConstResult.Scalar v,
                        tyOfValue v,
                        Anchor.ofToken op
                    )
                )
            )
        | Expr.InfixApp(leftExpr = left; infixOp = op; rightExpr = right) ->
            match tryBitwiseOp op.Token with
            | ValueNone -> Error ConstRejection.NotConstant
            | ValueSome bitOp ->
                match recur left, recur right with
                | Error e, _
                | _, Error e -> Error e
                | Ok l, Ok r ->
                    combineScalars bitOp (TConstExpr.tryScalar l) (TConstExpr.tryScalar r)
                    |> Result.map (fun v ->
                        TConstExpr.Binary(
                            RuntimeNames.bitwiseBindingKey bitOp,
                            l,
                            r,
                            TConstResult.Scalar v,
                            combinedTy tyOfValue l r v,
                            Anchor.ofToken op
                        )
                    )
        | _ -> Error ConstRejection.NotConstant
