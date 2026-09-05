namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open NameResolutionContainers

/// The declaration element an attribute set is written on, classified as fsc classifies
/// elements for `[<AttributeUsage>]` enforcement (each case's flag set below is fsc's, read
/// off FS0842's wording).
[<RequireQualifiedAccess>]
type AttrTarget =
    | Class
    | Struct
    | Enum
    | Interface
    /// A `type X = Y` alias, which fsc admits at every type kind.
    | Abbreviation
    /// A method member, static or instance.
    | Method
    /// A property member, static or instance, including an auto-property.
    | Property
    | RecordField
    | UnionCase
    | EnumCase
    /// A module-level `let` or `val` whose compiled shape is a non-function value.
    | ModuleValue
    /// A module-level `let` or `val` function, or a generalised (explicitly generic) value —
    /// fsc classifies both as methods.
    | ModuleFunction
    /// A position with no element classification (an abstract member signature, a
    /// splice with no CST site); enforcement passes.
    | Unchecked

[<RequireQualifiedAccess>]
module AttrTarget =

    /// The `AttributeTargetFlags` the element occupies; `ValueNone` for `Unchecked`.
    let mask (t: AttrTarget) : int voption =
        match t with
        | AttrTarget.Class -> ValueSome AttributeTargetFlags.Class
        | AttrTarget.Struct -> ValueSome AttributeTargetFlags.Struct
        | AttrTarget.Enum -> ValueSome AttributeTargetFlags.Enum
        | AttrTarget.Interface -> ValueSome AttributeTargetFlags.Interface
        | AttrTarget.Abbreviation ->
            ValueSome(
                AttributeTargetFlags.Class
                ||| AttributeTargetFlags.Struct
                ||| AttributeTargetFlags.Enum
                ||| AttributeTargetFlags.Interface
                ||| AttributeTargetFlags.Delegate
            )
        | AttrTarget.Method -> ValueSome(AttributeTargetFlags.Method ||| AttributeTargetFlags.ReturnValue)
        | AttrTarget.Property ->
            ValueSome(
                AttributeTargetFlags.Property
                ||| AttributeTargetFlags.Event
                ||| AttributeTargetFlags.ReturnValue
            )
        | AttrTarget.RecordField -> ValueSome(AttributeTargetFlags.Property ||| AttributeTargetFlags.Field)
        | AttrTarget.UnionCase -> ValueSome(AttributeTargetFlags.Method ||| AttributeTargetFlags.Property)
        | AttrTarget.EnumCase -> ValueSome AttributeTargetFlags.Field
        | AttrTarget.ModuleValue ->
            ValueSome(
                AttributeTargetFlags.Property
                ||| AttributeTargetFlags.Field
                ||| AttributeTargetFlags.ReturnValue
            )
        | AttrTarget.ModuleFunction -> ValueSome(AttributeTargetFlags.Method ||| AttributeTargetFlags.ReturnValue)
        | AttrTarget.Unchecked -> ValueNone

    /// fsc's classification of a module-level value: a function shape and a generalised
    /// (explicitly generic) value compile as methods, any other value as a property / field.
    let ofModuleValue (isFunctionShaped: bool) (isGeneric: bool) : AttrTarget =
        if isFunctionShaped || isGeneric then
            AttrTarget.ModuleFunction
        else
            AttrTarget.ModuleValue

/// `ResolvedAttributes` → the frozen `TAttributes`: each argument constant-folded through
/// `ConstFold`, each attribute's declared `[<AttributeUsage>]` mask enforced against the
/// `usedOn` element. An attribute with a rejected argument is diagnosed and omitted whole,
/// keeping a stored `Args` list positionally faithful to the written construction.
module internal AttributeFold =

    /// The folded constant of an enum case written `E.C` / `Path.E.C` in attribute-argument
    /// position, carrying `E`'s key: the prefix resolved as a type at `useSite`, a
    /// project-local claim first, then the referenced contracts.
    let private tryEnumCase
        (ctx: PassContext)
        (useSite: UseSite)
        (idents: ImmutableArray<SyntaxToken>)
        : FoldedConst voption =
        let n = idents.Length

        if n < 2 then
            ValueNone
        else
            let caseName = ctx.NameOf idents.[n - 1]

            let ofLiteral (enumKey: TypeKey) (lit: TEnumLiteral) : FoldedConst =
                match lit with
                | TEnumLiteral.Int v ->
                    {
                        Value = v
                        EnumKey = ValueSome enumKey
                    }
                | TEnumLiteral.String s ->
                    {
                        Value = TConstValue.String s
                        EnumKey = ValueSome enumKey
                    }

            let written: WrittenTypeName =
                {
                    Path = String.concat "." (seq { for i in 0 .. n - 3 -> ctx.NameOf idents.[i] })
                    Name = ctx.NameOf idents.[n - 2]
                }

            match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite written 0 with
            | ValueSome claim ->
                match TypeRegistry.tryEnumByKey ctx.Types claim.Key with
                | ValueSome info ->
                    match info.Cases |> EqArray.tryFind (fun c -> c.Name = caseName) with
                    | ValueSome case -> case.Value |> ValueOption.map (ofLiteral claim.Key)
                    | ValueNone -> ValueNone
                | ValueNone -> ValueNone
            | ValueNone ->
                tryPickExternalWritten
                    ctx
                    useSite
                    (WrittenArity.Exact 0)
                    (fun key shape ->
                        match shape with
                        | ExternalTypeShape.Enum(cases = cases) ->
                            match cases |> EqArray.tryFind (fun c -> c.Name = caseName) with
                            | ValueSome case ->
                                match case.Value with
                                | ExternalEnumCaseValue.IntVal(kind, v) ->
                                    ValueSome(ofLiteral key (TEnumLiteral.Int(TConstValue.Integral(kind, v))))
                                | ExternalEnumCaseValue.StringVal s -> ValueSome(ofLiteral key (TEnumLiteral.String s))
                            | ValueNone -> ValueNone
                        | _ -> ValueNone
                    )
                    (Qualifier.ofPath written.Path)
                    written.Name

    /// The folded constant of a `[<Literal>]` module value referenced at `useSite`. A bare
    /// spelling reads the scopes in force there, best rank first; a qualified one reads the
    /// containers the prefix denotes. A nearer non-literal value shadows a farther literal.
    let private tryLiteralValue
        (ctx: PassContext)
        (useSite: UseSite)
        (idents: ImmutableArray<SyntaxToken>)
        : TConstValue voption =
        let n = idents.Length
        let name = ctx.NameOf idents.[n - 1]

        let containers =
            match n with
            | 1 -> containersOf ctx useSite Qualifier.Bare
            | _ -> containersOf ctx useSite (Qualifier.ofSegments [| for i in 0 .. n - 2 -> ctx.NameOf idents.[i] |])

        let rec pick (cs: ModuleContainer list) =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match LocalScope.tryValue ctx useSite c name with
                | ValueSome m -> ctx.Resolution.LiteralValues.TryGetValue m.BindingSite
                | ValueNone -> pick rest

        pick containers

    /// The constant an identifier in attribute-argument (or `[<Literal>]`-RHS) position
    /// denotes at `useSite`: a `[<Literal>]` value first, then an enum case's folded
    /// constant, because a value claim shadows a type's case, matching expression resolution.
    let tryNamedConstant
        (ctx: PassContext)
        (useSite: UseSite)
        (idents: ImmutableArray<SyntaxToken>)
        : FoldedConst voption =
        match tryLiteralValue ctx useSite idents with
        // `LiteralValues` stores the bare constant, so an enum-typed literal folds without
        // its enum identity.
        | ValueSome v -> ValueSome { Value = v; EnumKey = ValueNone }
        | ValueNone -> tryEnumCase ctx useSite idents

    /// The written argument list of a construction: `[<A>]` and `[<A()>]` carry none,
    /// `[<A(x, y)>]` one per tuple component, `[<A "s">]` / `[<A(x)>]` exactly one.
    let private argExprs (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> list =
        match oc with
        | InterfaceConstruction _ -> []
        | ObjectConstruction(_, e) ->
            match e with
            | Expr.EmptyBlock _ -> []
            | Expr.EnclosedBlock(expr = Expr.Tuple(exprs = items)) -> List.ofSeq items
            | Expr.EnclosedBlock(expr = single) -> [ single ]
            | direct -> [ direct ]

    /// `Prop = v` in argument position is a named setter; anything else is positional.
    let private splitNamed (ctx: PassContext) (e: Expr<SyntaxToken>) : struct (string voption * Expr<SyntaxToken>) =
        match e with
        | Expr.InfixApp(leftExpr = CstKeys.SingleIdent nameTok; infixOp = op; rightExpr = v) when
            op.Token = Token.OpEquality
            ->
            struct (ValueSome(ctx.NameOf nameTok), v)
        | _ -> struct (ValueNone, e)

    /// The `[<AttributeUsage>]` mask the used attribute's own declaration carries: the local
    /// registry first, then the referenced contracts, as the folded first fixed argument of
    /// the declaration's `AttributeUsage` entry. A declaration without a reachable
    /// `AttributeUsage` is `AttributeTargetFlags.All`.
    let private declaredValidOn (ctx: PassContext) (attrKey: TypeKey) : int =
        let declAttrs =
            match TypeRegistry.tryClassByKey ctx.Types attrKey with
            | ValueSome info -> info.Attributes
            | ValueNone ->
                match ctx.Provider.TryLookupType attrKey with
                | ValueSome(ExternalTypeShape.Class shape) -> shape.Attributes
                | _ -> EqArray.empty

        match
            declAttrs
            |> EqArray.tryFind (fun a -> a.Key = RuntimeNames.attributeUsageAttributeKey)
        with
        | ValueSome usage ->
            let firstFixed =
                EqArray.toList usage.Args
                |> List.tryPick (fun a ->
                    match a.Name, a.Value with
                    | ValueNone, TConstValue.Integral(_, v) -> Some(int v)
                    | _ -> None
                )

            match firstFixed with
            | Some m -> m
            | None -> AttributeTargetFlags.All
        | ValueNone -> AttributeTargetFlags.All

    let private enforceTarget (ctx: PassContext) (usedOn: AttrTarget) (entry: ResolvedAttribute) : unit =
        match AttrTarget.mask usedOn with
        | ValueNone -> ()
        | ValueSome element ->
            let validOn = declaredValidOn ctx entry.Key

            if validOn &&& element = 0 then
                ctx.Report(entry.TypeRef.Site.Tok, Kind.AttributeTargetInvalid(element, validOn))

    /// `ValueNone` when any argument is outside the constant domain: every rejected argument
    /// is diagnosed and the attribute is omitted whole, so `Args` never carries a
    /// positionally-shifted list.
    let private foldAttribute (ctx: PassContext) (entry: ResolvedAttribute) : TAttribute voption =
        // The attribute's own type-ref site scopes the arguments' named-constant lookups.
        let useSite = ctx.UseSiteAt entry.TypeRef.Site.Key
        let tryNamed (idents: ImmutableArray<SyntaxToken>) = tryNamedConstant ctx useSite idents

        let args = ResizeArray<TAttributeArg>()
        let mutable allFolded = true

        for arg in argExprs entry.Construction do
            let struct (name, valueExpr) = splitNamed ctx arg

            match ConstFold.tryConstant ctx.NameOf (fun t k -> ctx.Report(t, k)) tryNamed valueExpr with
            | Ok v ->
                args.Add
                    {
                        Name = name
                        Value = v.Value
                        EnumKey = v.EnumKey
                    }
            | Error e ->
                ctx.Report(CstKeys.firstTokenOfExpr valueExpr, ConstFold.rejectionKind e)
                allFolded <- false

        if allFolded then
            ValueSome
                {
                    Key = entry.Key
                    Args = EqArray.ofSeq args
                }
        else
            ValueNone

    /// Every resolved attribute whose arguments all folded, in written order, each checked
    /// against `usedOn`. An attribute with a rejected argument was diagnosed and is absent.
    let build (ctx: PassContext) (usedOn: AttrTarget) (attrs: ResolvedAttributes) : TAttributes =
        match attrs.Entries with
        | [] -> EqArray.empty
        | entries ->
            EqArray.ofList
                [
                    for entry in entries do
                        enforceTarget ctx usedOn entry

                        match foldAttribute ctx entry with
                        | ValueSome a -> yield a
                        | ValueNone -> ()
                ]

    let resolveAndBuild (ctx: PassContext) (usedOn: AttrTarget) (attrs: Attributes<SyntaxToken> voption) : TAttributes =
        build ctx usedOn (ctx.ResolveAttributes attrs)
