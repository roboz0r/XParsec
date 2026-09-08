namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open Vesper
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// The attribute-argument constant domain: a literal, a named-constant reference (an enum
/// case or a `[<Literal>]` value), `|||`/`&&&`/`^^^` on two integral constants of one type,
/// unary minus on a numeric constant, an enum conversion, and grouping parens.
module ConstExprCheck =

    /// What a rejected expression reports, beside `Kind.NotConstantExpression` for a form
    /// outside the domain.
    module Rejection =

        /// `52I`: a call into a `NumericLiteral<suffix>` module.
        let customLiteral =
            Kind.Message
                "A constant expression cannot be a custom numeric literal ('52I'); it is a call to a NumericLiteral module, not a constant"

        /// `300uy`, the merged `-1uy`: the magnitude or sign does not fit the authored kind.
        let outOfRange =
            Kind.Message
                "This constant is not representable at its authored width (a negative value has no unsigned representation)"

        /// `$"…"`.
        let interpolatedString =
            Kind.Message "A constant expression cannot be an interpolated string"

        /// `- 1u`: negation is defined on the signed kinds only.
        let negativeUnsigned =
            Kind.Message "A negative constant has no unsigned representation; use a signed integer width"

        /// Bitwise operands that are not integral constants of one type: `1 ||| 2L`, and
        /// `E.A ||| 2` (FS0001 in fsc).
        let kindMismatch =
            Kind.Message "Bitwise operands of a constant expression must be integral constants of one type"

        /// `enum<E> "x"`: the underlying value of an enum constant is integral.
        let enumOperand =
            Kind.Message "An enum conversion in a constant expression takes an integral constant"

    /// Reports `kind` at `e`'s first token. Arms propagate `ValueNone` without reporting again,
    /// so one rejected expression yields one diagnostic.
    let private reject (ctx: PassContext) (e: Expr<SyntaxToken>) (kind: Kind) : TConstExpr voption =
        ctx.Report(CstKeys.firstTokenOfExpr e, kind)
        ValueNone

    let private typeOfValue (ctx: PassContext) (v: TConstValue) : FrozenType =
        LiteralTypes.frozenOfConstValue ctx.Intrinsics v

    let private segmentsOf (ctx: PassContext) (idents: ImmutableArray<SyntaxToken>) : string[] =
        [| for t in idents -> ctx.NameOf t |]

    /// The item the whole spelling denotes in expression position at `useSite`. Leftover
    /// segments are member accesses, outside the constant domain.
    let private resolveWhole (ctx: PassContext) (useSite: UseSite) (segments: string[]) : ResolvedItem voption =
        let r = NameResolutionLongIdent.resolveExpr ctx useSite segments

        if r.Rest = segments.Length then
            ValueSome r.Item
        else
            ValueNone

    /// The binding the whole spelling denotes at `useSite`. An operator's compiled name
    /// (`op_BitwiseOr`) is a bare segment.
    let private tryBinding (ctx: PassContext) (useSite: UseSite) (segments: string[]) : BindingKey voption =
        match resolveWhole ctx useSite segments with
        | ValueSome(ResolvedItem.Value v) -> ValueSome v.BindingKey
        | _ -> ValueNone

    /// The node `E.C` denotes; `ValueNone` where the case's own value was rejected at
    /// registration.
    let private enumCaseNode (at: Anchor) (owner: ResolvedTypeRef) (case: ResolvedEnumCase) : TConstExpr voption =
        case.Value
        |> ValueOption.map (fun v -> TConstExpr.EnumCase(owner.Key, case.Name, TConstResult.Scalar v, at))

    /// The `LiteralRef` node for `m`, carrying the RHS's own checked value and type;
    /// `ValueNone` for any other module value.
    let private literalRefNode (ctx: PassContext) (at: Anchor) (m: LocalModuleMember) : TConstExpr voption =
        ctx.Resolution.LiteralValues.TryGetValue m.BindingSite
        |> ValueOption.map (fun rhs -> TConstExpr.LiteralRef(m.Key, TConstExpr.result rhs, TConstExpr.ty rhs, at))

    /// The constant an identifier's segments (`Mask`, `E.C`, `Path.M.Mask`) denote at `useSite`.
    let private tryNamedConstant
        (ctx: PassContext)
        (useSite: UseSite)
        (idents: ImmutableArray<SyntaxToken>)
        : TConstExpr voption =
        let at = Anchor.ofToken idents.[0]

        match resolveWhole ctx useSite (segmentsOf ctx idents) with
        | ValueSome(ResolvedItem.Value(ResolvedValue.Local m)) -> literalRefNode ctx at m
        | ValueSome(ResolvedItem.EnumCase(owner, case)) -> enumCaseNode at owner case
        | _ -> ValueNone

    /// The enum `written` denotes at `useSite`.
    let private tryEnumKey (ctx: PassContext) (useSite: UseSite) (written: Type<SyntaxToken>) : TypeKey voption =
        match written with
        | Type.NamedType li ->
            match NameResolutionLongIdent.resolveType ctx useSite (ctx.WrittenTypeNameOf li) 0 with
            | TypeNameResolution.Type(ResolvedTypeRef.Local claim) when claim.Kind = TypeDeclKind.Enum ->
                ValueSome claim.Key
            | TypeNameResolution.Type(ResolvedTypeRef.External(key, ExternalTypeShape.Enum _)) -> ValueSome key
            | _ -> ValueNone
        | _ -> ValueNone

    let private tryBitwiseOp (compiledName: string) : BitwiseOp voption =
        match compiledName with
        | OperatorData.OpBitwiseOr -> ValueSome BitwiseOp.Or
        | OperatorData.OpBitwiseAnd -> ValueSome BitwiseOp.And
        | OperatorData.OpExclusiveOr -> ValueSome BitwiseOp.Xor
        | _ -> ValueNone

    /// True where `compiledName` denotes `intrinsic` at `useSite`. A `let (|||)` in scope
    /// shadows the intrinsic.
    let private appliesIntrinsic
        (ctx: PassContext)
        (useSite: UseSite)
        (compiledName: string)
        (intrinsic: BindingKey)
        : bool =
        tryBinding ctx useSite [| compiledName |] = ValueSome intrinsic

    let private tryIntrinsicBitwise (ctx: PassContext) (useSite: UseSite) (op: SyntaxToken) : BitwiseOp voption =
        match OperatorNames.ofSymbolic (ctx.NameOf op) op with
        | ValueSome compiledName ->
            match tryBitwiseOp compiledName with
            | ValueSome bitOp when appliesIntrinsic ctx useSite compiledName (RuntimeNames.bitwiseBindingKey bitOp) ->
                ValueSome bitOp
            | _ -> ValueNone
        | ValueNone -> ValueNone

    let private isIntrinsicNegation (ctx: PassContext) (useSite: UseSite) (op: SyntaxToken) : bool =
        op.Token = Token.OpSubtraction
        && (
            match OperatorNames.ofPrefix (ctx.NameOf op) op with
            | ValueSome compiledName -> appliesIntrinsic ctx useSite compiledName RuntimeNames.unaryNegationBindingKey
            | ValueNone -> false
        )

    /// An enum conversion: `enum<E>` or `LanguagePrimitives.EnumOfValue<int, E>`.
    type private EnumConversion = { Op: BindingKey; Enum: TypeKey }

    /// The conversion `applied<typeArgs>` spells, taking the enum from the type argument at the
    /// intrinsic's enum position.
    let private tryEnumConversion
        (ctx: PassContext)
        (useSite: UseSite)
        (applied: Expr<SyntaxToken>)
        (typeArgs: ImmutableArray<Type<SyntaxToken>>)
        : EnumConversion voption =
        match applied with
        | CstKeys.IdentPath idents ->
            match tryBinding ctx useSite (segmentsOf ctx idents) with
            | ValueSome op ->
                match RuntimeNames.enumConversionTypeArg op with
                | ValueSome i when i < typeArgs.Length ->
                    tryEnumKey ctx useSite typeArgs.[i]
                    |> ValueOption.map (fun enumKey -> { Op = op; Enum = enumKey })
                | _ -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// An enum conversion applied to one argument, and that argument.
    [<return: Struct>]
    let private (|EnumConversion|_|)
        (ctx: PassContext)
        (useSite: UseSite)
        (e: Expr<SyntaxToken>)
        : struct (EnumConversion * Expr<SyntaxToken>) voption =
        match e with
        | Expr.App(funcExpr = Expr.TypeApp(expr = applied; types = typeArgs); argExprs = args) when args.Length = 1 ->
            tryEnumConversion ctx useSite applied typeArgs
            |> ValueOption.map (fun conv -> struct (conv, args.[0]))
        | _ -> ValueNone

    /// Negation wraps AT THE WIDTH: `-(-128y)` stays `-128y`.
    let private negateScalar (v: TConstValue voption) : Result<TConstValue, Kind> =
        match v with
        | ValueSome(TConstValue.Integral v) ->
            match IntValue.negate v with
            | ValueSome n -> Ok(TConstValue.Integral n)
            | ValueNone -> Error Rejection.negativeUnsigned
        | ValueSome(TConstValue.Float n) -> Ok(TConstValue.Float(-n))
        | ValueSome(TConstValue.Float32 n) -> Ok(TConstValue.Float32(-n))
        | ValueSome(TConstValue.Decimal n) -> Ok(TConstValue.Decimal(-n))
        // `-"abc"`, `-true`: a non-numeric scalar, `null`, a type value or an array.
        | _ -> Error Kind.NotConstantExpression

    /// `l op r` over two integral constants of one type, typed as its operands are: two cases
    /// of one enum combine within that enum.
    let private combine
        (op: BitwiseOp)
        (l: TConstExpr)
        (r: TConstExpr)
        : Result<struct (TConstValue * FrozenType), Kind> =
        let ty = TConstExpr.ty l

        match TConstExpr.tryScalar l, TConstExpr.tryScalar r with
        | ValueSome(TConstValue.Integral lv), ValueSome(TConstValue.Integral rv) when ty = TConstExpr.ty r ->
            match IntValue.bitwise op lv rv with
            | ValueSome n -> Ok(struct (TConstValue.Integral n, ty))
            | ValueNone -> Error Rejection.kindMismatch
        | _ -> Error Rejection.kindMismatch

    /// The checked constant `e` denotes at `useSite`; `ValueNone` after reporting the rejection.
    let rec check (ctx: PassContext) (useSite: UseSite) (e: Expr<SyntaxToken>) : TConstExpr voption =
        let literal (v: TConstValue) : TConstExpr voption =
            ValueSome(TConstExpr.Literal(v, typeOfValue ctx v, Anchor.ofToken (CstKeys.firstTokenOfExpr e)))

        match e with
        | Expr.EnclosedBlock(expr = inner) -> check ctx useSite inner
        | Expr.EmptyBlock _ -> literal TConstValue.Unit
        | Expr.Const c ->
            match ConstLiteral.tryValue ctx.NameOf c with
            | Ok v -> literal v
            | Error NumericLiteralRejection.CustomLiteral -> reject ctx e Rejection.customLiteral
            | Error NumericLiteralRejection.OutOfRange -> reject ctx e Rejection.outOfRange
            // `NotNumeric` is unreachable on a parsed tree: `Constant.Literal` admits only
            // numeric / bool / char tokens. Refuse rather than throw.
            | Error NumericLiteralRejection.NotNumeric -> reject ctx e Kind.NotConstantExpression
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            let onInvalid (t: SyntaxToken) (k: Kind) = ctx.Report(t, k)
            literal (TConstValue.String(StringLiterals.foldStringParts ctx.NameOf (fun () -> "") onInvalid parts))
        | Expr.String _ -> reject ctx e Rejection.interpolatedString
        | CstKeys.IdentPath idents ->
            match tryNamedConstant ctx useSite idents with
            | ValueSome node -> ValueSome node
            | ValueNone -> reject ctx e Kind.NotConstantExpression
        // `enum<E> 12`, and the `LanguagePrimitives.EnumOfValue<int, E> 12` spelling the
        // runtime's own attribute masks are written under.
        | EnumConversion ctx useSite (struct (conv, operandExpr)) ->
            check ctx useSite operandExpr
            |> ValueOption.bind (fun operand ->
                match TConstExpr.tryScalar operand with
                | ValueSome(TConstValue.Integral _ as v) ->
                    ValueSome(
                        TConstExpr.Unary(
                            conv.Op,
                            operand,
                            TConstResult.Scalar v,
                            FTEnum conv.Enum,
                            Anchor.ofToken (CstKeys.firstTokenOfExpr e)
                        )
                    )
                | _ -> reject ctx e Rejection.enumOperand
            )
        // The lexer merges `-` into an ADJACENT numeric where the preceding token cannot be a
        // left operand, so `-1` arrives above as one literal. This arm takes the spaced `- 1`
        // and `-(1)`.
        | Expr.PrefixApp(op, operand) when isIntrinsicNegation ctx useSite op ->
            check ctx useSite operand
            |> ValueOption.bind (fun inner ->
                match negateScalar (TConstExpr.tryScalar inner) with
                | Error k -> reject ctx e k
                | Ok v ->
                    ValueSome(
                        TConstExpr.Unary(
                            RuntimeNames.unaryNegationBindingKey,
                            inner,
                            TConstResult.Scalar v,
                            typeOfValue ctx v,
                            Anchor.ofToken op
                        )
                    )
            )
        | Expr.InfixApp(leftExpr = left; infixOp = op; rightExpr = right) ->
            match tryIntrinsicBitwise ctx useSite op with
            | ValueNone -> reject ctx e Kind.NotConstantExpression
            | ValueSome bitOp ->
                check ctx useSite left
                |> ValueOption.bind (fun l -> check ctx useSite right |> ValueOption.map (fun r -> struct (l, r)))
                |> ValueOption.bind (fun (struct (l, r)) ->
                    match combine bitOp l r with
                    | Error k -> reject ctx e k
                    | Ok(struct (v, ty)) ->
                        ValueSome(
                            TConstExpr.Binary(
                                RuntimeNames.bitwiseBindingKey bitOp,
                                l,
                                r,
                                TConstResult.Scalar v,
                                ty,
                                Anchor.ofToken op
                            )
                        )
                )
        | _ -> reject ctx e Kind.NotConstantExpression
