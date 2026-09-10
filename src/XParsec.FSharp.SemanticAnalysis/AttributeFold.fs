namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// `ResolvedAttributes` → the frozen `TAttributes`: the constructor each attribute's
/// arguments select among the class's, and each argument checked through `ConstExprCheck`
/// against the parameter or member it fills.
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

    /// One written argument: the name of a `Name = v` form, and the value expression.
    type private WrittenArg =
        {
            Name: string voption
            /// The argument's first token, where a diagnostic about the whole argument lands.
            Tok: SyntaxToken
            Expr: Expr<SyntaxToken>
        }

    /// `Prop = v` in argument position is a named argument; anything else is positional.
    let private writtenArg (ctx: PassContext) (e: Expr<SyntaxToken>) : WrittenArg =
        match e with
        | Expr.InfixApp(leftExpr = CstKeys.SingleIdent nameTok; infixOp = op; rightExpr = v) when
            op.Token = Token.OpEquality
            ->
            {
                Name = ValueSome(ctx.NameOf nameTok)
                Tok = nameTok
                Expr = v
            }
        | _ ->
            {
                Name = ValueNone
                Tok = CstKeys.firstTokenOfExpr e
                Expr = e
            }

    let private positionalCount (args: WrittenArg list) : int =
        args |> List.sumBy (fun a -> if a.Name.IsNone then 1 else 0)

    /// What one written argument fills on a candidate constructor.
    [<RequireQualifiedAccess>]
    type private Fill =
        | Parameter of index: int
        /// A named argument the constructor declares no parameter for, set on the class member
        /// of that name.
        | Member of TAttributeMember

    /// Each argument's fill on `ctor`, in written order. `ValueNone` where the arguments do not
    /// fit: more positional arguments than parameters, a later parameter left unfilled or filled
    /// twice, or a named argument matching neither a parameter nor a class member.
    let private tryFills
        (settable: string -> TAttributeMember voption)
        (ctor: AttributeCtor)
        (args: WrittenArg list)
        : (WrittenArg * Fill) list voption =
        let positionals = positionalCount args

        let paramIndex (name: string) : int voption =
            ctor.Params |> Block.tryFindIndex (fun p -> p.Name = ValueSome name)

        let mutable nextPositional = 0
        let mutable filled = Set.empty
        let mutable fits = positionals <= ctor.Params.Length

        let fills =
            [
                for a in args do
                    match a.Name with
                    | ValueNone ->
                        let i = nextPositional
                        nextPositional <- i + 1
                        filled <- Set.add i filled
                        yield a, Fill.Parameter i
                    | ValueSome name ->
                        match paramIndex name with
                        | ValueSome i when i >= positionals ->
                            filled <- Set.add i filled
                            yield a, Fill.Parameter i
                        | _ ->
                            match settable name with
                            | ValueSome m -> yield a, Fill.Member m
                            | ValueNone -> fits <- false
            ]

        if fits && Set.count filled = ctor.Params.Length then
            ValueSome fills
        else
            ValueNone

    /// A candidate constructor's reading of the written arguments: the attribute it builds,
    /// `ValueNone` where it refuses an argument, and the diagnostics the reading reported,
    /// held back until the candidate is chosen.
    [<NoEquality; NoComparison>]
    type private Reading =
        {
            Ctor: AttributeCtor
            Attribute: TAttribute voption
            Diagnostics: ResizeArray<XParsec.FSharp.SemanticAnalysis.Diagnostic>
        }

    /// `new: x: int * string -> C`, as a diagnostic lists a candidate.
    let private describeCtor (className: string) (ctor: AttributeCtor) : string =
        let param (p: AttributeCtorParam) =
            let ty =
                match p.Declared with
                | ValueSome t -> Conformance.describeType t
                | ValueNone -> "?"

            match p.Name with
            | ValueSome n -> n + ": " + ty
            | ValueNone -> ty

        let parameters =
            match ctor.Params.Length with
            | 0 -> "unit"
            | _ -> ctor.Params |> Seq.map param |> String.concat " * "

        "new: " + parameters + " -> " + className

    /// Reports `node`, at a position of declared type `declared`, when the compiling target's
    /// attribute metadata cannot encode it. A provider without platform facts reports nothing;
    /// a `.fsi`-only analysis and a `Target = "none"` compilation run under one.
    let private reportUnencodable (ctx: PassContext) (declared: FrozenType) (node: TConstExpr) : unit =
        match ctx.Provider.Platform with
        | ValueNone -> ()
        | ValueSome platform ->
            match platform.ConstEncoding(declared, node) with
            | ConstEncoding.Encodable -> ()
            | ConstEncoding.Unencodable ty ->
                ctx.Report(
                    Anchor.toSite (TConstExpr.tok node),
                    Kind.UnencodableConstant(Conformance.describeType ty, ctx.Target)
                )

    /// `ctor`'s reading of `fills`, its diagnostics collected rather than reported. Every
    /// argument is checked, so each refused one reports. An unencodable argument is reported
    /// and still read, so encodability leaves constructor selection unchanged.
    let private readUnder
        (ctx: PassContext)
        (entry: ResolvedAttribute)
        (className: string)
        (useSite: UseSite)
        (ctor: AttributeCtor)
        (fills: (WrittenArg * Fill) list)
        : Reading =
        let checkAt (expected: FrozenType) (target: TAttributeArgTarget) (arg: WrittenArg) : TAttributeArg voption =
            ConstExprCheck.check ctx useSite (ValueSome expected) arg.Expr
            |> ValueOption.map (fun node ->
                reportUnencodable ctx expected node
                { Target = target; Expr = node }
            )

        let read () : TAttribute voption =
            match ctor.Identity with
            | AttributeCtorIdentity.Unannotated param ->
                ctx.Report(entry.TypeRef.Site.Tok, Kind.AttributeCtorParamUnannotated(className, param))
                ValueNone
            | AttributeCtorIdentity.Key key ->
                Block.ofList fills
                |> Block.map (fun (arg, fill) ->
                    match fill with
                    | Fill.Parameter i -> checkAt key.ArgSig.[i] (TAttributeArgTarget.Parameter i) arg
                    | Fill.Member m -> checkAt m.Ty (TAttributeArgTarget.Member m) arg
                )
                |> Block.tryMap id
                |> ValueOption.map (fun args ->
                    {
                        Key = entry.Key
                        Ctor = key
                        Args = args
                    }
                )

        let struct (attribute, diagnostics) = ctx.Collecting read

        {
            Ctor = ctor
            Attribute = attribute
            Diagnostics = diagnostics
        }

    /// True where `x` is preferred to `y`: the two declare the same parameter count, and `x`
    /// is not `obj` at a position where `y` is.
    let private preferred (x: AttributeCtor) (y: AttributeCtor) : bool =
        let isObj (p: AttributeCtorParam) =
            match p.Declared with
            | ValueSome FTObj -> true
            | _ -> false

        x.Params.Length = y.Params.Length
        && Block.forall2 (fun px py -> not (isObj px) || isObj py) x.Params y.Params
        && Seq.exists2 (fun px py -> isObj py && not (isObj px)) x.Params y.Params

    /// The one reading preferred to every other; `ValueNone` where none is.
    let private tryPreferred (readings: Reading list) : Reading voption =
        let beatsAllOthers (r: Reading) =
            readings
            |> List.forall (fun o -> o.Ctor.Identity = r.Ctor.Identity || preferred r.Ctor o.Ctor)

        match readings |> List.filter beatsAllOthers with
        | [ r ] -> ValueSome r
        | _ -> ValueNone

    /// The attribute `reading` builds, its held diagnostics reported.
    let private commit (ctx: PassContext) (reading: Reading) : TAttribute voption =
        for d in reading.Diagnostics do
            ctx.Diagnostics.Add d

        reading.Attribute

    /// The checked attribute `entry` writes. `ValueNone` where a named argument matches neither
    /// a constructor parameter nor a settable class member, no constructor admits the arguments,
    /// or an argument is outside the constant domain; every rejection is diagnosed.
    let private foldAttribute
        (ctx: PassContext)
        (classes: IAttributeClassSource)
        (entry: ResolvedAttribute)
        : TAttribute voption =
        // The attribute's own type-ref site scopes the arguments' name resolutions.
        let useSite = ctx.UseSiteAt entry.TypeRef.Site.Key
        let className = SymbolKeyOps.typeMetaName entry.Key
        let args = argExprs entry.Construction |> List.map (writtenArg ctx)
        let ctors = Block.toList (classes.Ctors entry.Key)
        let settable (name: string) = classes.TrySettable(entry.Key, name)

        let declaresParam (name: string) =
            ctors
            |> List.exists (fun c -> c.Params |> Block.exists (fun p -> p.Name = ValueSome name))

        let unknownNames =
            args
            |> List.choose (fun a ->
                match a.Name with
                | ValueSome name when (settable name).IsNone && not (declaresParam name) -> Some(a.Tok, name)
                | _ -> None
            )

        let report (tok: SyntaxToken) (kind: Kind) : TAttribute voption =
            ctx.Report(tok, kind)
            ValueNone

        match unknownNames with
        | _ :: _ ->
            for (tok, name) in unknownNames do
                ctx.Report(tok, Kind.AttributeNamedArgUnknown(className, name))

            ValueNone
        | [] ->
            let readings =
                [
                    for ctor in ctors do
                        match tryFills settable ctor args with
                        | ValueSome fills -> yield readUnder ctx entry className useSite ctor fills
                        | ValueNone -> ()
                ]

            let admitting = readings |> List.filter (fun r -> r.Attribute.IsSome)

            match readings, admitting with
            | [], _ ->
                report
                    entry.TypeRef.Site.Tok
                    (Kind.AttributeCtorArgCount(
                        className,
                        positionalCount args,
                        Block.ofList [ for c in ctors -> c.Params.Length ]
                    ))
            | [ one ], [] -> commit ctx one
            | several, [] ->
                report
                    entry.TypeRef.Site.Tok
                    (Kind.AttributeCtorNoOverload(
                        className,
                        Block.ofList [ for r in several -> describeCtor className r.Ctor ]
                    ))
            | _, [ one ] -> commit ctx one
            | _, admitting ->
                match tryPreferred admitting with
                | ValueSome one -> commit ctx one
                | ValueNone ->
                    report
                        entry.TypeRef.Site.Tok
                        (Kind.AttributeCtorAmbiguous(
                            className,
                            Block.ofList [ for r in admitting -> describeCtor className r.Ctor ]
                        ))

    /// Every resolved attribute whose constructor was selected and whose arguments all
    /// checked, in written order. A rejected attribute was diagnosed and is absent.
    let build (ctx: PassContext) (classes: IAttributeClassSource) (attrs: ResolvedAttributes) : TAttributes =
        match attrs.Entries with
        | [] -> Block.empty
        | entries ->
            Block.ofList
                [
                    for entry in entries do
                        match foldAttribute ctx classes entry with
                        | ValueSome a -> yield a
                        | ValueNone -> ()
                ]

[<AutoOpen>]
module internal AttributeFoldContext =

    type PassContext with

        /// File `attrs` at `site`: checked at once where the attribute classes are readable,
        /// else pending with the walk's current environment, for `OpenAttributeChecks`.
        member this.DeclareAttributes(site: NodeKey, usedOn: AttrTarget, attrs: ResolvedAttributes) : unit =
            match this.Resolution.AttributeClasses with
            | ValueSome classes ->
                this.Resolution.AttributePositions.Declare(
                    site,
                    {
                        UsedOn = usedOn
                        Attributes = attrs
                        Checked = AttributeFold.build this classes attrs
                    }
                )
            | ValueNone ->
                this.Resolution.AttributePositions.Defer(
                    site,
                    {
                        UsedOn = usedOn
                        Attributes = attrs
                        Scope = this.AmbientScope
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

        /// Check every pending position against `classes` in source order, each under the
        /// environment it was declared in.
        member this.CheckPendingAttributes(classes: IAttributeClassSource) : unit =
            let ambient = this.AmbientScope

            for (site, pending) in this.Resolution.AttributePositions.TakePending() do
                this.RestoreScope pending.Scope

                this.Resolution.AttributePositions.Declare(
                    site,
                    {
                        UsedOn = pending.UsedOn
                        Attributes = pending.Attributes
                        Checked = AttributeFold.build this classes pending.Attributes
                    }
                )

            this.RestoreScope ambient

        /// Make `classes` the attribute classes every argument checks against, and check every
        /// pending position. A position declared afterwards is checked at declaration.
        member this.OpenAttributeChecks(classes: IAttributeClassSource) : unit =
            this.Resolution.AttributeClasses <- ValueSome classes
            this.CheckPendingAttributes classes

        /// The checked attributes of the position declared at `site`. Fails where no position
        /// is declared, or the position is still pending.
        member this.AttributesAt(site: NodeKey) : TAttributes =
            AttributePosition.checkedAt site (this.Resolution.AttributePositions.TryGet site)

        /// The checked attributes of the position declared at `site`; `ValueNone` where the
        /// element declared none.
        member this.TryAttributesAt(site: NodeKey) : TAttributes voption =
            this.Resolution.AttributePositions.TryGet site
            |> ValueOption.map (fun position -> position.Checked)

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
                    match a.Target, a.Value with
                    // `AttributeTargets` is an `int`-based enum.
                    | TAttributeArgTarget.Parameter 0, ValueSome(TConstValue.Integral(IntValue.Int32 v)) -> Some v
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
