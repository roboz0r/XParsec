namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// `ResolvedAttributes` → the frozen `TAttributes`, each argument checked through
/// `ConstExprCheck`.
module internal AttributeFold =

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

    /// `ValueNone` when any argument is outside the constant domain: every rejected argument
    /// is diagnosed and the attribute is omitted whole, so `Args` never carries a
    /// positionally-shifted list.
    let private foldAttribute (ctx: PassContext) (entry: ResolvedAttribute) : TAttribute voption =
        // The attribute's own type-ref site scopes the arguments' name resolutions.
        let useSite = ctx.UseSiteAt entry.TypeRef.Site.Key

        let args = ResizeArray<TAttributeArg>()
        let mutable allFolded = true

        for arg in argExprs entry.Construction do
            let struct (name, valueExpr) = splitNamed ctx arg

            match ConstExprCheck.check ctx useSite valueExpr with
            | ValueSome e -> args.Add { Name = name; Expr = e }
            | ValueNone -> allFolded <- false

        if allFolded then
            ValueSome
                {
                    Key = entry.Key
                    Args = Block.ofSeq args
                }
        else
            ValueNone

    /// Every resolved attribute whose arguments all checked, in written order. An attribute
    /// with a rejected argument was diagnosed and is absent.
    let build (ctx: PassContext) (attrs: ResolvedAttributes) : TAttributes =
        match attrs.Entries with
        | [] -> Block.empty
        | entries ->
            Block.ofList
                [
                    for entry in entries do
                        match foldAttribute ctx entry with
                        | ValueSome a -> yield a
                        | ValueNone -> ()
                ]

[<AutoOpen>]
module internal AttributeFoldContext =

    type PassContext with

        /// Check `attrs` under the walk's current environment and file the result at `site`.
        member this.DeclareAttributes(site: NodeKey, usedOn: AttrTarget, attrs: ResolvedAttributes) : unit =
            this.Resolution.AttributePositions.Declare(
                site,
                {
                    UsedOn = usedOn
                    Attributes = attrs
                    Checked = AttributeFold.build this attrs
                }
            )

        member this.DeclareAttributes
            (site: NodeKey, usedOn: AttrTarget, attrs: Attributes<SyntaxToken> voption)
            : unit =
            this.DeclareAttributes(site, usedOn, this.ResolveAttributes attrs)

        /// Declare the attributes written on an element with no anchor token of its own,
        /// filed under `AttributeSite.ofSets`.
        member this.DeclareAttributeSets(attrs: Attributes<SyntaxToken> voption, usedOn: AttrTarget) : unit =
            match attrs with
            | ValueNone -> ()
            | ValueSome sets ->
                match AttributeSite.ofSets sets with
                | ValueNone -> ()
                | ValueSome site -> this.DeclareAttributes(site, usedOn, attrs)

        /// The checked attributes of the position declared at `site`. Fails where no position
        /// is declared.
        member this.AttributesAt(site: NodeKey) : TAttributes =
            AttributePosition.checkedAt site (this.Resolution.AttributePositions.TryGet site)

/// The `[<AttributeUsage>]` target check over declared positions.
module internal AttributeUsageCheck =

    /// The `[<AttributeUsage>]` mask the used attribute's own declaration carries, as the
    /// checked first fixed argument of its `AttributeUsage` entry. A declaration without a
    /// reachable `AttributeUsage` is `AttributeTargetFlags.All`.
    let private declaredValidOn (ctx: PassContext) (positions: SealedAttributePositions) (attrKey: TypeKey) : int =
        let declAttrs =
            match TypeRegistry.tryClassByKey ctx.Types attrKey with
            | ValueSome info -> positions.CheckedAt(AttributeSite.ofSite info.DeclSite)
            | ValueNone -> ctx.Provider.TryLookupAttributes(SymbolKey.Type attrKey)

        match
            declAttrs
            |> Block.tryFind (fun a -> a.Key = RuntimeNames.attributeUsageAttributeKey)
        with
        | ValueSome usage ->
            let firstFixed =
                Block.toList usage.Args
                |> List.tryPick (fun a ->
                    match a.Name, a.Value with
                    // `AttributeTargets` is an `int`-based enum.
                    | ValueNone, ValueSome(TConstValue.Integral(IntValue.Int32 v)) -> Some v
                    | _ -> None
                )

            match firstFixed with
            | Some m -> m
            | None -> AttributeTargetFlags.All
        | ValueNone -> AttributeTargetFlags.All

    let private enforceOne
        (ctx: PassContext)
        (positions: SealedAttributePositions)
        (usedOn: AttrTarget)
        (entry: ResolvedAttribute)
        : unit =
        let element = AttrTarget.mask usedOn
        let validOn = declaredValidOn ctx positions entry.Key

        if validOn &&& element = 0 then
            ctx.Report(entry.TypeRef.Site.Tok, Kind.AttributeTargetInvalid(element, validOn))

    let enforceAll (ctx: PassContext) (positions: SealedAttributePositions) : unit =
        for position in positions.InSourceOrder do
            for entry in position.Attributes.Entries do
                enforceOne ctx positions position.UsedOn entry
