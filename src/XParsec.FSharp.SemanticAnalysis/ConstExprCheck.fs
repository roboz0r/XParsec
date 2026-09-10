namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open Vesper
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// The attribute-argument constant domain: literals, named constants (enum cases and
/// `[<Literal>]` values), the folds of `ConstBinary` and `ConstUnary`, enum conversions,
/// `typeof<T>` / `typedefof<T>`, array literals, `null` and grouping parens.
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
        let bitwiseOperands =
            Kind.Message "Bitwise operands of a constant expression must be integral constants of one type"

        /// `1 + 2L`, `"a" + 1`, `E.A + E.B`: arithmetic takes two integral constants of one
        /// primitive type, and `+` two strings.
        let arithmeticOperands =
            Kind.Message
                "A constant arithmetic expression takes two integral constants of one primitive type, or two strings under '+'"

        /// `1.0 + 2.0`, `1.5M * 2M`: the targets compute inexact arithmetic differently (the
        /// JS bodies compute in float64), so a constant expression carries the literal alone.
        let inexactArithmetic =
            Kind.NotYetSupported "arithmetic on 'float', 'float32' or 'decimal' in a constant expression"

        /// `1 <<< 1L`, `E.A <<< 1`: a shift takes an integral constant of a primitive type
        /// and an `int` count.
        let shiftOperands =
            Kind.Message "A constant shift takes an integral constant of a primitive type and an 'int' count"

        /// `1 <<< 32`, `1 <<< -1`: the targets mask an out-of-range count differently, so a
        /// constant shift takes a count within the shifted type's width.
        let shiftCount =
            Kind.Message "A constant shift count must be between 0 and one below the shifted type's width"

        /// `~~~E.A` (an `int` in fsc, rather than the enum), `~~~1.5`.
        let complementOperand =
            Kind.Message "A constant complement takes an integral constant of a primitive type"

        /// `1 && true`, `not 1`.
        let logicalOperands =
            Kind.Message "A constant '&&', '||' or 'not' takes 'bool' constants"

        /// `1n + 1n`, `1n <<< 3`, `~~~1n`: a fold whose result depends on the operand's width
        /// refuses a pointer-width kind, which takes its width from the target.
        let targetWidth =
            Kind.Message "A constant expression cannot compute at the pointer width of 'nativeint' or 'unativeint'"

        /// `enum<E> "x"`: the underlying value of an enum constant is integral.
        let enumOperand =
            Kind.Message "An enum conversion in a constant expression takes an integral constant"

        /// `typeof<'T>` / `typedefof<Box<'T>>` (FS3187): a reified type is ground.
        let typarReified =
            Kind.Message
                "A constant expression cannot reify a type parameter; a declaration's type parameters are not in scope in a constant position"

        /// `typeof<list<_>>`: a constant position reads every type argument as written.
        let inferredReified =
            Kind.NotYetSupported "an inferred type argument ('_') in a reified type; write the type argument in full"

        /// `typedefof<string | null>`, and a tuple or function under a provider carrying no
        /// platform facts: a generic definition comes from an identity the target supplies.
        let structuralDefinition =
            Kind.Message "'typedefof' takes a type with a generic definition on the target"

        /// `[| 1; 2L |]` (FS0267 at `2L`): every item of a constant array has the array's
        /// element type, which the position declares, else the first item supplies.
        let arrayItemType =
            Kind.Message "Every item of a constant array literal has the array's element type"

        /// `[| [| 1 |] |]` (FS0267 at the inner `[|`): a constant array holds scalars, types
        /// and strings, never an array.
        let nestedArray =
            Kind.Message "A constant array literal does not nest; an item cannot itself be an array"

    /// Reports `kind` at `e`'s first token. Arms propagate `ValueNone` without reporting again,
    /// so one rejected expression yields one diagnostic.
    let private reject (ctx: PassContext) (e: Expr<SyntaxToken>) (kind: Kind) : TConstExpr voption =
        ctx.Report(CstKeys.firstTokenOfExpr e, kind)
        ValueNone

    /// True where `null` inhabits `t`: `string`, `obj`, `Type`, an array, and a class
    /// declared `[<AllowNullLiteral>]`.
    let private admitsNull (ctx: PassContext) (t: FrozenType) : bool =
        match t with
        | FTArray _ -> true
        | FTConst(key, args) when args.IsEmpty ->
            key = RuntimeNames.stringKey
            || key = RuntimeNames.objKey
            || key = RuntimeNames.runtimeTypeKey
        | FTClass(key, _) ->
            match NominalDecl.tryOfKey ctx key with
            | ValueSome decl -> NominalDecl.allowsNullLiteral decl
            | ValueNone -> false
        | _ -> false

    /// True where a constant of type `actual` fills a position declared `expected`: the
    /// types are equal, or the position is `obj`, which boxes any constant.
    let private conforms (expected: FrozenType) (actual: FrozenType) : bool =
        match expected with
        | FTObj -> true
        | _ -> expected = actual

    let private typeOfValue (ctx: PassContext) (v: TConstValue) : FrozenType =
        LiteralTypes.frozenOfConstValue ctx.Intrinsics v

    let private runtimeType (ctx: PassContext) : FrozenType =
        toFrozen (ctx.Intrinsics.OfCanon RuntimeNames.runtimeTypeKey)

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

    /// The `LiteralRef` node for the `[<Literal>]` binding `key`, carrying what its RHS denotes.
    let private literalRefNode (at: Anchor) (key: BindingKey) (rhs: TConstDenotation) : TConstExpr =
        TConstExpr.LiteralRef(key, rhs.Result, rhs.Ty, at)

    /// The `LiteralRef` node for `v`; `ValueNone` for any other value. A value of this file
    /// carries its denotation on `LiteralValues`, a published one on its symbol.
    let private valueNode (ctx: PassContext) (at: Anchor) (v: ResolvedValue) : TConstExpr voption =
        match v with
        | ResolvedValue.Local m ->
            ctx.Resolution.LiteralValues.TryGetValue(BoundVarKey.ofPatKey m.BindingSite)
            |> ValueOption.map (literalRefNode at m.Key)
        | ResolvedValue.External sym -> sym.Literal |> ValueOption.map (literalRefNode at sym.Key)

    /// The constant an identifier's segments (`Mask`, `E.C`, `Path.M.Mask`) denote at `useSite`.
    let private tryNamedConstant
        (ctx: PassContext)
        (useSite: UseSite)
        (idents: ImmutableArray<SyntaxToken>)
        : TConstExpr voption =
        let at = Anchor.ofToken idents.[0]

        match resolveWhole ctx useSite (segmentsOf ctx idents) with
        | ValueSome(ResolvedItem.Value v) -> valueNode ctx at v
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

    /// A binary operator a constant expression folds.
    [<RequireQualifiedAccess>]
    type private ConstBinary =
        /// `|||`, `&&&`, `^^^`: two integral constants of one type, two cases of one enum
        /// included.
        | Bits of IntBitwiseOp
        /// `+`, `-`, `*`: two integral constants of one primitive type, wrapping at its
        /// width; `+` also concatenates two strings.
        | Arith of IntArithOp
        /// `<<<`, `>>>`: an integral constant of a primitive type, by an `int` count within
        /// its width.
        | Shift of IntShiftOp
        /// `&&`: two `bool` constants.
        | AndAlso
        /// `||`: two `bool` constants.
        | OrElse

    /// A unary operator a constant expression folds.
    [<RequireQualifiedAccess>]
    type private ConstUnary =
        /// `-`: a signed numeric constant, wrapping at its width.
        | Negate
        /// `~~~`: an integral constant of a primitive type.
        | Complement
        /// `not`: a `bool` constant.
        | Not

    let private binaryModule (op: ConstBinary) : RuntimeNames.OperatorModule =
        match op with
        | ConstBinary.Bits _
        | ConstBinary.Shift _ -> RuntimeNames.OperatorModule.Bitwise
        | ConstBinary.Arith _ -> RuntimeNames.OperatorModule.Arithmetic
        | ConstBinary.AndAlso
        | ConstBinary.OrElse -> RuntimeNames.OperatorModule.Logical

    let private unaryModule (op: ConstUnary) : RuntimeNames.OperatorModule =
        match op with
        | ConstUnary.Negate -> RuntimeNames.OperatorModule.Arithmetic
        | ConstUnary.Complement -> RuntimeNames.OperatorModule.Bitwise
        | ConstUnary.Not -> RuntimeNames.OperatorModule.Operators

    /// The fold the INFIX operator `compiledName` denotes; `ValueNone` for every operator
    /// outside the constant domain.
    let private tryConstBinary (compiledName: string) : ConstBinary voption =
        match compiledName with
        | OperatorData.OpBitwiseOr -> ValueSome(ConstBinary.Bits IntBitwiseOp.Or)
        | OperatorData.OpBitwiseAnd -> ValueSome(ConstBinary.Bits IntBitwiseOp.And)
        | OperatorData.OpExclusiveOr -> ValueSome(ConstBinary.Bits IntBitwiseOp.Xor)
        | OperatorData.OpLeftShift -> ValueSome(ConstBinary.Shift IntShiftOp.Left)
        | OperatorData.OpRightShift -> ValueSome(ConstBinary.Shift IntShiftOp.Right)
        | OperatorData.OpAddition -> ValueSome(ConstBinary.Arith IntArithOp.Add)
        | OperatorData.OpSubtraction -> ValueSome(ConstBinary.Arith IntArithOp.Subtract)
        | OperatorData.OpMultiply -> ValueSome(ConstBinary.Arith IntArithOp.Multiply)
        | OperatorData.OpBooleanAnd -> ValueSome ConstBinary.AndAlso
        | OperatorData.OpBooleanOr -> ValueSome ConstBinary.OrElse
        | _ -> ValueNone

    /// The fold the PREFIX operator `compiledName` denotes.
    let private tryConstPrefix (compiledName: string) : ConstUnary voption =
        match compiledName with
        | OperatorData.OpUnaryNegation -> ValueSome ConstUnary.Negate
        | OperatorData.OpLogicalNot -> ValueSome ConstUnary.Complement
        | _ -> ValueNone

    /// A fold with the intrinsic binding its spelling resolved to. `Key` is what the checked
    /// node records.
    type private ResolvedOp<'op> = { Op: 'op; Key: BindingKey }

    /// The fold `spelling` denotes at `useSite`; `ValueNone` where the spelling is not in
    /// scope or resolves to any other binding. A `let (|||)` in scope shadows the intrinsic.
    let private tryIntrinsicOp
        (ctx: PassContext)
        (useSite: UseSite)
        (table: string -> 'op voption)
        (homeModule: 'op -> RuntimeNames.OperatorModule)
        (spelling: string voption)
        : ResolvedOp<'op> voption =
        match spelling with
        | ValueSome compiledName ->
            match table compiledName with
            | ValueSome op ->
                let key = RuntimeNames.operatorKey (homeModule op) compiledName

                if tryBinding ctx useSite [| compiledName |] = ValueSome key then
                    ValueSome { Op = op; Key = key }
                else
                    ValueNone
            | ValueNone -> ValueNone
        | ValueNone -> ValueNone

    /// The fold the applied name `idents` denotes at `useSite`: `not`, the one foldable
    /// operator written as an ordinary application.
    let private tryAppliedUnary
        (ctx: PassContext)
        (useSite: UseSite)
        (idents: ImmutableArray<SyntaxToken>)
        : ResolvedOp<ConstUnary> voption =
        let key = RuntimeNames.operatorKey (unaryModule ConstUnary.Not) "not"

        if tryBinding ctx useSite (segmentsOf ctx idents) = ValueSome key then
            ValueSome { Op = ConstUnary.Not; Key = key }
        else
            ValueNone

    /// An ordinary application of a name to exactly one argument: `not b` or `not(b)`.
    [<return: Struct>]
    let private (|AppliedName|_|)
        (e: Expr<SyntaxToken>)
        : struct (ImmutableArray<SyntaxToken> * Expr<SyntaxToken>) voption =
        match e with
        | Expr.App(funcExpr = CstKeys.IdentPath idents; argExprs = args) when args.Length = 1 ->
            ValueSome(struct (idents, args.[0]))
        | Expr.HighPrecedenceApp(funcExpr = CstKeys.IdentPath idents; argExpr = arg) -> ValueSome(struct (idents, arg))
        | _ -> ValueNone

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

    /// Which type a reification denotes.
    [<RequireQualifiedAccess>]
    type Reified =
        /// `typeof<T>`: the type as written.
        | AsWritten
        /// `typedefof<T>`: the generic definition of the written type constructor, its type
        /// arguments discarded.
        | Definition

        member this.SourceName: string =
            match this with
            | Reified.AsWritten -> "typeof"
            | Reified.Definition -> "typedefof"

        /// The inline-IL opcode the backends lower the reification through. Both carry the
        /// WRITTEN type as operand: `ldtokendef` takes the generic definition at run time.
        member this.OpCode: string =
            match this with
            | Reified.AsWritten -> "ldtoken"
            | Reified.Definition -> "ldtokendef"

    module Reified =

        /// The reification `key` denotes; `ValueNone` for any other binding.
        let tryOfBinding (key: BindingKey) : Reified voption =
            if key = RuntimeNames.typeofBindingKey then
                ValueSome Reified.AsWritten
            elif key = RuntimeNames.typedefofBindingKey then
                ValueSome Reified.Definition
            else
                ValueNone

    /// The reification `applied` resolves to at `useSite`; `ValueNone` for any other expression.
    let private tryReified (ctx: PassContext) (useSite: UseSite) (applied: Expr<SyntaxToken>) : Reified voption =
        match applied with
        | CstKeys.IdentPath idents ->
            tryBinding ctx useSite (segmentsOf ctx idents)
            |> ValueOption.bind Reified.tryOfBinding
        | _ -> ValueNone

    /// The first type parameter (`'T` / `^T`) written anywhere inside `written`. A type
    /// parameter is refused before translation, which would mint an undeclared one into the
    /// enclosing `TyparScope`.
    let private tryWrittenTypar (written: Type<SyntaxToken>) : SyntaxToken voption =
        let mutable found = ValueNone

        let visitType _ (t: Type<SyntaxToken>) =
            match t with
            | Type.VarType(Typar.Named(ident = id))
            | Type.VarType(Typar.Static(ident = id)) when found.IsNone -> found <- ValueSome id
            | _ -> ()

            found.IsNone

        CstTypeWalk.iterType
            { CstTypeWalk.identityTypeIter with
                VisitType = visitType
            }
            written

        found

    /// The identity a structural type is laid out under: a tuple as `IPlatformFacts` supplies
    /// it (CLR `System.ValueTuple`n`, JS a rank-1 array), a function as `Vesper.Fun`2`, the
    /// interface a curried value implements on both targets.
    let private tryPlatformIdentity (ctx: PassContext) (t: FrozenType) : TypeKey voption =
        match t with
        | FTTuple items -> ctx.Provider.Platform |> ValueOption.bind (fun p -> p.TupleType items.Length)
        | FTFun _ -> ValueSome(RuntimeNames.vesperFunKey 2)
        | _ -> ValueNone

    /// `t` with its type arguments dropped, where its identity is generic. A tuple and a
    /// function take the identity the target lays them out under. An arity-0 identity
    /// (`int[]`), an enum and an unresolved type are unchanged.
    let private tryDefinitionOf (ctx: PassContext) (t: FrozenType) : FrozenType voption =
        match t with
        | FTConst(key, _) when key.TyparArity > 0 -> ValueSome(FTConst(key, Block.empty))
        | FTRecord(key, _) when key.TyparArity > 0 -> ValueSome(FTRecord(key, Block.empty))
        | FTUnion(key, _) when key.TyparArity > 0 -> ValueSome(FTUnion(key, Block.empty))
        | FTClass(key, _) when key.TyparArity > 0 -> ValueSome(FTClass(key, Block.empty))
        | FTConst _
        | FTRecord _
        | FTUnion _
        | FTClass _
        | FTEnum _
        | FTUnknown _ -> ValueSome t
        | FTTuple _
        | FTFun _ ->
            match tryPlatformIdentity ctx t with
            | ValueSome key when key.TyparArity > 0 -> ValueSome(FTClass(key, Block.empty))
            | _ -> ValueNone
        | _ -> ValueNone

    /// `t` where it is ground. A written `_` is reported at `at`; any other unresolved type was
    /// reported at its written name.
    let private tryGround
        (ctx: PassContext)
        (at: SyntaxToken)
        (translated: SemType)
        (t: FrozenType)
        : FrozenType voption =
        if FrozenTypeBridge.ftIsGround t then
            ValueSome t
        else
            if ctx.HasInferenceHoleIn translated then
                ctx.Report(at, Rejection.inferredReified)

            ValueNone

    /// The ground type `typeof<written>` / `typedefof<written>` reifies, `at` the
    /// reification's own token; `ValueNone` after the rejection is reported.
    let private tryReifiedType
        (ctx: PassContext)
        (at: SyntaxToken)
        (reified: Reified)
        (written: Type<SyntaxToken>)
        : FrozenType voption =
        match tryWrittenTypar written with
        | ValueSome tok ->
            ctx.Report(tok, Rejection.typarReified)
            ValueNone
        | ValueNone ->
            // An attribute argument lies outside the name-resolution walk; an expression-position
            // argument was stamped by the walk, and the verdict is memoised.
            NameResolutionTypeRefStamp.stampTypeRefs ctx written

            // A `_` mints a fresh type variable and an inference-hole mark in `ctx`; a constant
            // position reads the mark once and leaves the variable unlinked.
            let translated = UnificationTranslate.translateType ctx written

            let frozen =
                FrozenTypeBridge.freezeWith ctx.Store (fun _ -> FTUnknown UnknownReason.UnresolvedTypar) translated

            let candidate =
                match reified with
                | Reified.AsWritten -> ValueSome frozen
                | Reified.Definition ->
                    match tryDefinitionOf ctx frozen with
                    | ValueSome definition -> ValueSome definition
                    | ValueNone ->
                        ctx.Report(at, Rejection.structuralDefinition)
                        ValueNone

            candidate |> ValueOption.bind (tryGround ctx at translated)

    /// A reification applied to exactly one written type.
    [<return: Struct>]
    let private (|Reification|_|)
        (ctx: PassContext)
        (useSite: UseSite)
        (e: Expr<SyntaxToken>)
        : struct (Reified * Type<SyntaxToken>) voption =
        match e with
        | Expr.TypeApp(expr = applied; types = typeArgs) when typeArgs.Length = 1 ->
            tryReified ctx useSite applied
            |> ValueOption.map (fun reified -> struct (reified, typeArgs.[0]))
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

    /// The scalar of a node of any type. An enum case matches, carrying its underlying value.
    [<return: Struct>]
    let private (|Scalar|_|) (e: TConstExpr) : TConstValue voption = TConstExpr.tryScalar e

    /// The scalar of a node of a primitive type. An enum case is excluded: bitwise
    /// combination within the enum is the only fold its cases take.
    [<return: Struct>]
    let private (|Primitive|_|) (e: TConstExpr) : TConstValue voption =
        match TConstExpr.ty e with
        | FTEnum _ -> ValueNone
        | _ -> TConstExpr.tryScalar e

    /// The diagnostic an integral fold's rejection reports, `operands` standing for the
    /// operator's own domain.
    let private foldRejection (operands: Kind) (r: IntFoldRejection) : Kind =
        match r with
        | IntFoldRejection.KindMismatch -> operands
        | IntFoldRejection.TargetWidth -> Rejection.targetWidth
        | IntFoldRejection.ShiftCount -> Rejection.shiftCount

    /// `l op r`, typed as its operands are: two cases of one enum combine within that enum.
    let private applyBinary
        (op: ConstBinary)
        (l: TConstExpr)
        (r: TConstExpr)
        : Result<struct (TConstValue * FrozenType), Kind> =
        let ty = TConstExpr.ty l
        let sameType = ty = TConstExpr.ty r
        let integral (n: IntValue) = struct (TConstValue.Integral n, ty)

        match op, l, r with
        | ConstBinary.Bits o, Scalar(TConstValue.Integral a), Scalar(TConstValue.Integral b) when sameType ->
            match IntValue.bitwise o a b with
            | ValueSome n -> Ok(integral n)
            | ValueNone -> Error Rejection.bitwiseOperands
        | ConstBinary.Bits _, _, _ -> Error Rejection.bitwiseOperands
        | ConstBinary.Arith o, Primitive(TConstValue.Integral a), Primitive(TConstValue.Integral b) when sameType ->
            IntValue.arithmetic o a b
            |> Result.map integral
            |> Result.mapError (foldRejection Rejection.arithmeticOperands)
        | ConstBinary.Arith IntArithOp.Add, Primitive(TConstValue.String a), Primitive(TConstValue.String b) ->
            Ok(struct (TConstValue.String(a + b), ty))
        | ConstBinary.Arith _, Scalar(TConstValue.Float _ | TConstValue.Float32 _ | TConstValue.Decimal _), _
        | ConstBinary.Arith _, _, Scalar(TConstValue.Float _ | TConstValue.Float32 _ | TConstValue.Decimal _) ->
            Error Rejection.inexactArithmetic
        | ConstBinary.Arith _, _, _ -> Error Rejection.arithmeticOperands
        | ConstBinary.Shift o, Primitive(TConstValue.Integral a), Primitive(TConstValue.Integral(IntValue.Int32 count)) ->
            IntValue.shift o a count
            |> Result.map integral
            |> Result.mapError (foldRejection Rejection.shiftOperands)
        | ConstBinary.Shift _, _, _ -> Error Rejection.shiftOperands
        | ConstBinary.AndAlso, Scalar(TConstValue.Bool a), Scalar(TConstValue.Bool b) ->
            Ok(struct (TConstValue.Bool(a && b), ty))
        | ConstBinary.OrElse, Scalar(TConstValue.Bool a), Scalar(TConstValue.Bool b) ->
            Ok(struct (TConstValue.Bool(a || b), ty))
        | ConstBinary.AndAlso, _, _
        | ConstBinary.OrElse, _, _ -> Error Rejection.logicalOperands

    /// `op operand`. Negation is typed by the value it yields, the complement and `not` by
    /// the operand's own type.
    let private applyUnary
        (ctx: PassContext)
        (op: ConstUnary)
        (operand: TConstExpr)
        : Result<struct (TConstValue * FrozenType), Kind> =
        let ty = TConstExpr.ty operand

        match op, operand with
        | ConstUnary.Negate, _ ->
            negateScalar (TConstExpr.tryScalar operand)
            |> Result.map (fun n -> struct (n, typeOfValue ctx n))
        | ConstUnary.Complement, Primitive(TConstValue.Integral a) ->
            IntValue.complement a
            |> Result.map (fun n -> struct (TConstValue.Integral n, ty))
            |> Result.mapError (foldRejection Rejection.complementOperand)
        | ConstUnary.Complement, _ -> Error Rejection.complementOperand
        | ConstUnary.Not, Scalar(TConstValue.Bool b) -> Ok(struct (TConstValue.Bool(not b), ty))
        | ConstUnary.Not, _ -> Error Rejection.logicalOperands

    /// The constant `e` denotes at `useSite`, before the position's type is enforced. `null` and
    /// `[||]` take the declared `expected` as their own type and are refused without it, and an
    /// array literal checks its items against it; every other form is typed by its own spelling.
    let rec private checkForm
        (ctx: PassContext)
        (useSite: UseSite)
        (expected: FrozenType voption)
        (e: Expr<SyntaxToken>)
        : TConstExpr voption =
        let literal (v: TConstValue) : TConstExpr voption =
            ValueSome(TConstExpr.Literal(v, typeOfValue ctx v, Anchor.ofToken (CstKeys.firstTokenOfExpr e)))

        match e with
        | Expr.EnclosedBlock(lParen = (ParenKind.Paren _ | ParenKind.BeginEnd _); expr = inner) ->
            checkForm ctx useSite expected inner
        | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner) ->
            checkArray ctx useSite expected e (CstKeys.listLiteralItems inner)
        | Expr.EmptyBlock(lParen = ParenKind.Array _) ->
            match expected with
            | ValueSome(FTArray _ as arrayTy) ->
                ValueSome(TConstExpr.ArrayLit(Block.empty, arrayTy, Anchor.ofToken (CstKeys.firstTokenOfExpr e)))
            | _ -> reject ctx e Kind.NotConstantExpression
        | Expr.Null tok ->
            match expected with
            | ValueSome ty when admitsNull ctx ty -> ValueSome(TConstExpr.Null(ty, Anchor.ofToken tok))
            | ValueSome ty -> reject ctx e (Kind.NullNotProperValue(Conformance.describeType ty))
            | ValueNone -> reject ctx e Kind.NotConstantExpression
        | Expr.EmptyBlock(lParen = (ParenKind.Paren _ | ParenKind.BeginEnd _)) -> literal TConstValue.Unit
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
            checkForm ctx useSite ValueNone operandExpr
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
        | Reification ctx useSite (struct (reified, written)) ->
            let at = CstKeys.firstTokenOfExpr e

            tryReifiedType ctx at reified written
            |> ValueOption.map (fun operand -> TConstExpr.TypeOf(operand, runtimeType ctx, Anchor.ofToken at))
        | AppliedName(struct (idents, arg)) ->
            match tryAppliedUnary ctx useSite idents with
            | ValueNone -> reject ctx e Kind.NotConstantExpression
            | ValueSome resolved -> checkUnary ctx useSite e resolved (Anchor.ofToken idents.[0]) arg
        // The lexer merges `-` into an ADJACENT numeric where the preceding token cannot be a
        // left operand, so `-1` arrives above as one literal. This arm takes the spaced `- 1`
        // and `-(1)`, and every other prefix fold.
        | Expr.PrefixApp(op, operand) ->
            match tryIntrinsicOp ctx useSite tryConstPrefix unaryModule (OperatorNames.ofPrefix (ctx.NameOf op) op) with
            | ValueNone -> reject ctx e Kind.NotConstantExpression
            | ValueSome resolved -> checkUnary ctx useSite e resolved (Anchor.ofToken op) operand
        | Expr.InfixApp(leftExpr = left; infixOp = op; rightExpr = right) ->
            match
                tryIntrinsicOp ctx useSite tryConstBinary binaryModule (OperatorNames.ofSymbolic (ctx.NameOf op) op)
            with
            | ValueNone -> reject ctx e Kind.NotConstantExpression
            | ValueSome resolved ->
                checkForm ctx useSite ValueNone left
                |> ValueOption.bind (fun l ->
                    checkForm ctx useSite ValueNone right
                    |> ValueOption.map (fun r -> struct (l, r))
                )
                |> ValueOption.bind (fun (struct (l, r)) ->
                    match applyBinary resolved.Op l r with
                    | Error k -> reject ctx e k
                    | Ok(struct (v, ty)) ->
                        ValueSome(TConstExpr.Binary(resolved.Key, l, r, TConstResult.Scalar v, ty, Anchor.ofToken op))
                )
        | _ -> reject ctx e Kind.NotConstantExpression

    /// `op operand`, the operator already resolved to the intrinsic it folds as. `at` is the
    /// operator's own token.
    and private checkUnary
        (ctx: PassContext)
        (useSite: UseSite)
        (e: Expr<SyntaxToken>)
        (resolved: ResolvedOp<ConstUnary>)
        (at: Anchor)
        (operand: Expr<SyntaxToken>)
        : TConstExpr voption =
        checkForm ctx useSite ValueNone operand
        |> ValueOption.bind (fun inner ->
            match applyUnary ctx resolved.Op inner with
            | Error k -> reject ctx e k
            | Ok(struct (v, ty)) -> ValueSome(TConstExpr.Unary(resolved.Key, inner, TConstResult.Scalar v, ty, at))
        )

    /// `[| e1; …; en |]` with at least one item: every item at the element type, which an
    /// expected array type declares and the first item supplies otherwise; the node is typed
    /// as that type's array. Each rejected item is reported at the item.
    and private checkArray
        (ctx: PassContext)
        (useSite: UseSite)
        (expected: FrozenType voption)
        (e: Expr<SyntaxToken>)
        (items: Expr<SyntaxToken> list)
        : TConstExpr voption =
        let declaredElemTy =
            match expected with
            | ValueSome(FTArray elemTy) -> ValueSome elemTy
            | _ -> ValueNone

        /// An item written as an array, however grouped, is rejected before it is checked.
        let checkItem (item: Expr<SyntaxToken>) : TConstExpr voption =
            match CstKeys.ungroup item with
            | Expr.EnclosedBlock(lParen = ParenKind.Array _)
            | Expr.EmptyBlock(lParen = ParenKind.Array _) -> reject ctx item Rejection.nestedArray
            | _ -> checkForm ctx useSite declaredElemTy item

        // Every item is checked, so each rejected item reports once.
        let checkedItems = items |> List.map (fun item -> struct (item, checkItem item))

        let elemTy =
            match declaredElemTy, checkedItems with
            | ValueSome t, _ -> ValueSome t
            | ValueNone, struct (_, ValueSome first) :: _ -> ValueSome(TConstExpr.ty first)
            | ValueNone, _ -> ValueNone

        match elemTy with
        | ValueSome elemTy ->
            /// The node at `elemTy`; `ValueNone` after reporting a node of another type.
            let atElemTy (struct (item: Expr<SyntaxToken>, node: TConstExpr voption)) : TConstExpr voption =
                match node with
                | ValueSome n when conforms elemTy (TConstExpr.ty n) -> node
                | ValueSome _ -> reject ctx item Rejection.arrayItemType
                | ValueNone -> ValueNone

            checkedItems
            |> List.map atElemTy
            |> Block.tryOfSeq
            |> ValueOption.map (fun nodes ->
                TConstExpr.ArrayLit(nodes, ftArray elemTy, Anchor.ofToken (CstKeys.firstTokenOfExpr e))
            )
        | ValueNone -> ValueNone

    /// The checked constant `e` denotes at `useSite`; `ValueNone` after reporting the
    /// rejection. A constant of another type than `expected` declares is FS0001 at `e`,
    /// except at an `obj` position, which boxes any constant.
    let check
        (ctx: PassContext)
        (useSite: UseSite)
        (expected: FrozenType voption)
        (e: Expr<SyntaxToken>)
        : TConstExpr voption =
        match checkForm ctx useSite expected e, expected with
        | ValueSome node, ValueSome expected when not (conforms expected (TConstExpr.ty node)) ->
            reject
                ctx
                e
                (Kind.ConstantTypeMismatch(
                    Conformance.describeType expected,
                    Conformance.describeType (TConstExpr.ty node)
                ))
        | node, _ -> node
