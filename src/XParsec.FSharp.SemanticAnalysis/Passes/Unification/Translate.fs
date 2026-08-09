namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine

module internal UnificationTranslate =

    let enterLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel + 1

    let exitLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel - 1

    /// Fresh unkeyed TypeVar: an intermediate "result" var, tied to no CST node.
    let freshTyVar (ctx: PassContext) : TyVarId =
        let tv = ctx.NewTypeVar()
        ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
        tv

    /// Overwrites any prior entry; get-or-allocate callers must use `tvOf`.
    let freshTv (ctx: PassContext) (key: NodeKey) : TyVarId =
        let tv = ctx.NewTypeVar()
        ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
        ctx.Bindings.TypeVar.Set(key, tv)
        tv

    /// Get-or-allocate, because a missing key is a binding-site pattern not yet walked,
    /// including a forward reference inside a `let rec` group.
    let tvOf (ctx: PassContext) (key: NodeKey) : TyVarId =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Recover with a fresh TyVar, so a broken subtree still yields a type.
    let errorTy (ctx: PassContext) (tok: SyntaxToken) (kind: Kind) : SemType =
        ctx.Report(tok, kind)
        TyVar(freshTyVar ctx)

    /// The tail of a WRITTEN reference no claim of this file holds and no external shape built.
    /// The spelling either names an external type the contract registered without a body, which
    /// is a gap to name, or resolved to nothing at all.
    let private unresolvedRefTy (ctx: PassContext) (site: NodeSite) (name: string) : SemType =
        let unmodelled =
            match ctx.Resolution.TypeRefVerdicts.TryGetValue site.Key with
            | ValueSome(TypeRefVerdict.ExternalType symKey) ->
                match ctx.Provider.TryLookupType(SymbolKey.Type symKey) with
                | ValueSome(ExternalTypeShape.Unmodelled(reason = r)) -> ValueSome r
                | _ -> ValueNone
            | _ -> ValueNone

        match unmodelled with
        | ValueSome(UnmodelledReason.ExtractionFailed reason) ->
            errorTy
                ctx
                site.Tok
                (Kind.Message(
                    sprintf "A referenced package declares '%s', but its body did not extract (%s)" name reason
                ))
        | ValueSome r -> errorTy ctx site.Tok (Kind.NotYetSupported(sprintf "%s — '%s'" r.Description name))
        | ValueNone ->
            // Blamed at the name's first token alone, because the long-ident span is not in
            // hand here.
            ctx.UndefinedType(Site.ofToken site.Tok, name)
            TyUnknown name

    /// Qualified measure names (`Microsoft.FSharp.SI.kg`) and measure typars (`'u`) yield an
    /// empty term plus a diagnostic, so the rest of inference continues without measure noise.
    let rec translateMeasure (ctx: PassContext) (measureTok: SyntaxToken) (m: Measure<SyntaxToken>) : MeasureTerm =
        match m with
        | Measure.One _ -> MeasureTerm.empty
        | Measure.Named li when li.Idents.Length = 1 -> MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], Rational.One ]
        | Measure.Power(inner, _, neg, expTok) ->
            let n = System.Numerics.BigInteger.Parse(ctx.NameOf expTok)
            let signed = if neg.IsSome then -n else n

            MeasureTerm.pow
                (translateMeasure ctx measureTok inner)
                (Rational.create (signed, System.Numerics.BigInteger.One))
        | Measure.Product(l, _, r) ->
            MeasureTerm.mul (translateMeasure ctx measureTok l) (translateMeasure ctx measureTok r)
        | Measure.Quotient(l, _, r) ->
            MeasureTerm.div (translateMeasure ctx measureTok l) (translateMeasure ctx measureTok r)
        | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx measureTok inner)
        | Measure.Paren(_, inner, _) -> translateMeasure ctx measureTok inner
        | Measure.Juxtaposition(elems, _) ->
            (MeasureTerm.empty, elems)
            ||> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx measureTok m))
        | Measure.Anonymous _
        | Measure.Typar _
        | Measure.Named _ ->
            ctx.Report(measureTok, Kind.NotYetSupported "measure typars / wildcards / qualified unit names")

            MeasureTerm.empty

    /// Built-in numeric names that can carry a measure (`float<m>`, `int<kg>`). A
    /// `carrier<arg>` ARGUMENT is a measure atom, never a type reference, so a walk that
    /// diagnoses unknown names must stop at the same carriers, else `float<kg>` blames `kg`.
    ///
    /// A NAME test, necessarily: the carrier is recognised before it resolves to anything, so
    /// an alias spelling (`single`, `double`) reaches here as itself.
    let isNumericCarrier (name: string) : bool =
        RuntimeNames.numericTypeNames.Contains name

    /// A resolved `Class` whose metadata name has a reverse-canon hit is an intrinsic's
    /// platform spelling (`System.Exception` → `exn`) and resolves to the canon `TyConst`,
    /// so no raw BCL nominal enters the unifier. Interfaces are absent from that map.
    let externalClassTy (ctx: PassContext) (key: TypeKey) (args: EqArray<SemType>) : SemType =
        // Built on the resolved `key` directly: re-minting an identity from the flattened
        // metadata name loses the container and gives an unequal key. The probe keys on that
        // NAME, which is sound because its entries are all bare-IL, where name and key agree.
        match ctx.IntrinsicReverseCanon.Value.TryGetValue(SymbolKeyOps.typeMetaName key) with
        | true, (canon :: _) -> TyConst(canon, args)
        | _ -> TyClass(key, args)

    /// DEBUG-only, for a DOTTED name neither the store view nor a local claim answered: every
    /// written reference carries a verdict, so NO verdict is a stamping walk that missed this
    /// syntax position, and an EXTERNAL verdict's key must be servable by the store view.
    let private assertNoDottedStampGap (ctx: PassContext) (nodeKey: NodeKey) (li: LongIdent<SyntaxToken>) : unit =
#if DEBUG
        if li.Idents.Length > 1 then
            let name = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

            match ctx.Resolution.TypeRefVerdicts.TryGetValue nodeKey with
            | ValueNone ->
                failwithf
                    "NameResolution stamping gap: dotted type reference '%s' carries no verdict — a stamping walk missed this syntax position"
                    name
            | ValueSome(TypeRefVerdict.ExternalType stamped) ->
                match ctx.Provider.TryLookupType(SymbolKey.Type stamped) with
                | ValueNone ->
                    failwithf
                        "External identity round-trip broken: dotted type reference '%s' resolved to %s, but the store view cannot serve that key — NameResolution's mint and the store disagree"
                        name
                        (SymbolKeyOps.typeMetaName stamped)
                // Served, but the shape declined to build (no modelled body, or an arity the
                // shape does not carry), so the use site reports it.
                | ValueSome _ -> ()
            | ValueSome TypeRefVerdict.LocalType
            | ValueSome TypeRefVerdict.UnknownType -> ()
#else
        ignore ctx
        ignore nodeKey
        ignore li
#endif

    /// Resolves `'a` through `ctx.Resolution.TyparScope`; callers open a fresh scope per
    /// signature (binding or type defn) before walking. A generic named type written
    /// without its args back-fills the arg list with fresh TyVars for unification to pin.
    let rec translateType (ctx: PassContext) (t: Type<SyntaxToken>) : SemType =
        match t with
        | Type.ParenType(typ = inner) -> translateType ctx inner
        | Type.VarType(Typar.Named(ident = id))
        | Type.VarType(Typar.Static(ident = id)) ->
            let name = ctx.NameOf id

            match ctx.Resolution.TyparScope.TryGetValue name with
            | true, tv -> TyVar tv
            | false, _ ->
                if ctx.Resolution.TyparScopeStrict then
                    // Type-defn fill-in: an implicit free typar isn't legal F#. Diagnose,
                    // but still mint and memoise so later occurrences share the TyVar.
                    ctx.Report(
                        id,
                        Kind.Message(
                            sprintf
                                "Free type parameter %s is not declared in the enclosing type's type-parameter list"
                                name
                        )
                    )

                    let tv = ctx.NewTypeVar()
                    ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
                    ctx.Resolution.TyparScope.[name] <- tv
                    TyVar tv
                else
                    // Implicit typar: mint at the binding's current level so generalisation
                    // at binding-group exit picks it up; memoise to share identity.
                    let tv = ctx.NewTypeVar()
                    ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
                    ctx.Resolution.TyparScope.[name] <- tv
                    TyVar tv
        | Type.VarType(Typar.Anon _) ->
            // `_` typar — always fresh, never stored; distinct per occurrence.
            let tv = freshTyVar ctx
            // `_` is the one INFERRED position inside a written type (`Box<_>`): mark it so a
            // consumer of the annotation tells the hole from written structure or a named `'a`.
            ctx.MarkInferenceHole tv
            TyVar tv
        | Type.NamedType li when li.Idents.Length = 1 ->
            // Bare single-segment name. An EXTERNAL verdict outranks the registry, since only a
            // reference no local claim held where it was written gets one: one written above a
            // same-named local declaration stays external. Any other verdict ⇒ local, or nothing.
            let site = CstKeys.typeRefSite t

            match tryResolveExternalTypeStamped ctx site.Key EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> resolveBareTypeName ctx li.Idents.[0] (fun _name -> ValueNone)
        | Type.NamedType li ->
            // Qualified named type (`A.T`, `System.Text.StringBuilder`); not external ⇒ the
            // qualifier names a scope of THIS file, or the reference does not resolve.
            let site = CstKeys.typeRefSite t

            match tryResolveExternalTypeStamped ctx site.Key EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> resolveQualifiedTypeName ctx site li EqArray.empty
        | Type.GenericType(longIdent = li; typeArgs = args) when
            li.Idents.Length = 1
            && args.Length = 1
            && isNumericCarrier (ctx.NameOf li.Idents.[0])
            ->
            // `float<m>` / `int<kg>` — the measure goes onto a fresh TyVar whose Link carries
            // the carrier. The parser tags an arg `TypeArg.Measure` only where the measure
            // grammar is unambiguous; bare `float<m>` lands as `TypeArg.Type (NamedType "m")`.
            let carrierTok = li.Idents.[0]

            let measureFromTypeArg =
                match args.[0] with
                | TypeArg.Measure m -> ValueSome m
                | TypeArg.Type(Type.NamedType nameLi) ->
                    // Reinterpret a single-segment named type as a measure
                    // atom; multi-segment qualifiers stay a real type.
                    if nameLi.Idents.Length = 1 then
                        ValueSome(Measure.Named nameLi)
                    else
                        ValueNone
                | _ -> ValueNone

            match measureFromTypeArg with
            | ValueSome m ->
                let mt = translateMeasure ctx carrierTok m
                let tv = freshTyVar ctx
                // Resolve the carrier (`float`) BY NAME: the measure arg is not a type arg, so
                // the carrier is wanted at arity 0 and its verdict was recorded at written arity 1.
                ctx.Store.SetLink(
                    UnionFind.find ctx.Store tv,
                    ValueSome(
                        resolveBareTypeName ctx carrierTok (fun name -> tryResolveExternalType ctx name EqArray.empty)
                    )
                )

                ctx.Store.SetUnits(UnionFind.find ctx.Store tv, ValueSome mt)
                TyVar tv
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let site = CstKeys.typeRefSite t
            let name = ctx.NameOf site.Tok

            let translatedArgs =
                EqArray.ofSeq (
                    seq {
                        for a in args ->
                            match a with
                            | TypeArg.Type t -> translateType ctx t
                            // A measure-shaped arg on a non-numeric carrier has no model.
                            | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                    }
                )

            resolveNamedGeneric ctx site name translatedArgs
        | Type.GenericType(longIdent = li; typeArgs = args) ->
            // Qualified generic type (`A.T<int>`). Resolved at the WRITTEN arity:
            // `A.T<int>` names the `T\`1` of module `A`, and a same-named `T` at another
            // arity is a different type.
            let translatedArgs =
                EqArray.ofSeq (
                    seq {
                        for a in args ->
                            match a with
                            | TypeArg.Type t -> translateType ctx t
                            | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                    }
                )

            let site = CstKeys.typeRefSite t

            match tryResolveExternalTypeStamped ctx site.Key translatedArgs with
            | ValueSome ty -> ty
            | ValueNone -> resolveQualifiedTypeName ctx site li translatedArgs
        | Type.SuffixedType(baseType = baseTy; longIdent = li) when li.Idents.Length = 1 ->
            // Postfix generic syntax: `'T list` ≡ `list<'T>`. A multi-arg postfix form
            // (`(int, string) Map`) parses its base as a tuple and falls to the arity diagnostic.
            let site = CstKeys.typeRefSite t
            let name = ctx.NameOf site.Tok
            resolveNamedGeneric ctx site name (EqArray.singleton (translateType ctx baseTy))
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple(EqArray.ofSeq (seq { for t in types -> translateType ctx t }))
        | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
            let inner = translateType ctx inner
            translateConstraints ctx cs
            inner
        | Type.ArrayType(baseType = baseTy; commas = commas) ->
            // `'T[]` / `'T[,]` → `TyConst(arrayKey rank, [elem])`, rank = comma count + 1.
            // Array literals build the same repr.
            let rank = commas.Length + 1

            TyConst(RuntimeNames.arrayKey rank, EqArray.singleton (translateType ctx baseTy))
        | Type.Null _ ->
            // The `null` type — a *member* of an anonymous union (`T | null`), not a nominal.
            // Resolves to the cross-backend `nullKey` intrinsic.
            TyConst(RuntimeNames.nullKey, EqArray.empty)
        | Type.UnionType(left = l; right = r) ->
            // TypeScript-style anonymous structural union (`X | Y`); the CST is left-nested
            // for `a | b | c`. `mkUnion` flattens and dedups, collapses a singleton to its
            // one member, and leaves a set that compares equal in any member order.
            mkUnion [ translateType ctx l; translateType ctx r ]
        | _ ->
            // Shapes with no model: a multi-segment postfix application (`int A.T`), an
            // anonymous record. A free TyVar lets unification pin it from context.
            TyVar(freshTyVar ctx)

    /// The `SemType` of the project-local type `claim` names, applied to `args`. A nominal is
    /// built from the claim's `TypeKey` alone, so `type A = { x: B } and B = { y: A }`
    /// resolves before either detail registers.
    and private resolveClaimedType
        (ctx: PassContext)
        (site: NodeSite)
        (claim: TypeIdentity)
        (args: EqArray<SemType>)
        : SemType voption =
        let key = claim.Key

        match claim.Kind with
        // Primitive binding (`type int = (# "System.Int32" #)`): a nominal intrinsic, NOT a
        // transparent abbreviation. Its canon key is contract-sourced, minted at claim time.
        | TypeDeclKind.IntrinsicRepr -> ValueSome(TyConst(TypeRegistry.intrinsicKeyOf ctx.Types claim.Name, args))
        | TypeDeclKind.Abbreviation ->
            match TypeRegistry.tryAbbrevByKey ctx.Types key with
            | ValueSome info ->
                // Eager expansion: force the body, then substitute the use-site args.
                forceFill ctx info
                ValueSome(expandAbbreviation ctx site.Tok info args)
            | ValueNone -> ValueNone
        | TypeDeclKind.Record -> ValueSome(TyRecord(key, args))
        | TypeDeclKind.Union ->
            ctx.Resolution.ResolvedType.Set(site.Key, key)
            ValueSome(TyUnion(key, args))
        | TypeDeclKind.Enum ->
            // An enum is niladic (no type args), so the reference is just `TyEnum key`.
            ctx.Resolution.ResolvedType.Set(site.Key, key)
            ValueSome(TyEnum key)
        | TypeDeclKind.Class -> ValueSome(TyClass(key, args))

    /// Resolve a bare (single-segment, arity-0) type NAME. `resolveExternal` is the pluggable
    /// external tail: a WRITTEN annotation passes its stamped store-view read, while the
    /// SYNTHESIZED `float<m>` carrier, which nothing stamped, passes a by-name resolver.
    and private resolveBareTypeName
        (ctx: PassContext)
        (nameTok: SyntaxToken)
        (resolveExternal: string -> SemType voption)
        : SemType =
        let name = ctx.NameOf nameTok
        let site = NodeSite.ofToken NodeKind.TypeNamed nameTok

        let claimed =
            match TypeRegistry.tryTypeClaim ctx.Types (ctx.UseSiteAt site.Key) name 0 with
            | ValueSome claim -> resolveClaimedType ctx site claim EqArray.empty
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        // Nothing claims the name at arity 0, so a GENERIC local type of that name answers at
        // any arity, its args back-filled with fresh TyVars at the current level (unpinned
        // here, fixed by surrounding unification). An enum always claims arity 0.
        | ValueNone ->
            let fromLocal =
                match TypeRegistry.tryTypeClaimAnyArity ctx.Types (ctx.UseSiteAt site.Key) name with
                | ValueSome claim ->
                    let args = EqArray.init claim.TyparArity (fun _ -> TyVar(freshTyVar ctx))
                    resolveClaimedType ctx site claim args
                | ValueNone -> ValueNone

            match fromLocal with
            | ValueSome ty -> ty
            | ValueNone ->
                match resolveExternal name with
                | ValueSome ty -> ty
                // `undefined` is a JS-only intrinsic with NO CLR repr. Normally the written
                // name resolves through the provider's `prim-types-undefined.js` contract;
                // this arm mints `undefinedKey` for a stack that has not loaded it.
                | ValueNone when name = RuntimeNames.undefinedTypeName ->
                    TyConst(RuntimeNames.undefinedKey, EqArray.empty)
                | ValueNone -> unresolvedRefTy ctx site name

    /// Resolve a QUALIFIED reference (`A.T`, `N.A.T<int>`) whose external verdict read already
    /// missed: it names a project-local type THROUGH the scope holding it, or it names
    /// nothing. The claim on `(path, name, arity)` answers, as it does for a bare name.
    and private resolveQualifiedTypeName
        (ctx: PassContext)
        (site: NodeSite)
        (li: LongIdent<SyntaxToken>)
        (args: EqArray<SemType>)
        : SemType =
        let written = ctx.WrittenTypeNameOf li
        let useSite = ctx.UseSiteAt site.Key

        let claimed =
            match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite written args.Length with
            | ValueSome claim -> resolveClaimedType ctx site claim args
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        | ValueNone ->
            // The name reaches a local type at some OTHER arity (`A.T<int>` where `A` holds a
            // non-generic `T`): blame the arity, never fall through to an external spelling.
            match TypeRegistry.tryWrittenTypeClaimAnyArity ctx.Types useSite written with
            | ValueSome other ->
                errorTy ctx site.Tok (Kind.TypeArgArity(written.Written, other.TyparArity, args.Length))
            | ValueNone ->
                assertNoDottedStampGap ctx site.Key li
                unresolvedRefTy ctx site written.Written

    /// An EXTERNAL verdict outranks the registry.
    and private resolveNamedGeneric
        (ctx: PassContext)
        (site: NodeSite)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        match tryResolveExternalTypeStamped ctx site.Key translatedArgs with
        | ValueSome ty -> ty
        | ValueNone -> resolveLocalNamedGeneric ctx site name translatedArgs

    and private resolveLocalNamedGeneric
        (ctx: PassContext)
        (site: NodeSite)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        let argCount = translatedArgs.Length

        let claimed =
            match TypeRegistry.tryTypeClaim ctx.Types (ctx.UseSiteAt site.Key) name argCount with
            | ValueSome claim -> resolveClaimedType ctx site claim translatedArgs
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        | ValueNone ->
            // The exact-arity claim missed: the any-arity lookup answers, forwarding the
            // WRITTEN args and blaming the arity. An IntrinsicRepr is left undiagnosed, because
            // a niladic primitive tolerates stray args and a generic one (`array`) forwards them.
            let fromLocal =
                match TypeRegistry.tryTypeClaimAnyArity ctx.Types (ctx.UseSiteAt site.Key) name with
                | ValueSome claim ->
                    if claim.Kind <> TypeDeclKind.IntrinsicRepr then
                        ctx.Report(site.Tok, Kind.TypeArgArity(name, claim.TyparArity, argCount))

                    resolveClaimedType ctx site claim translatedArgs
                | ValueNone -> ValueNone

            match fromLocal with
            | ValueSome ty -> ty
            | ValueNone -> unresolvedRefTy ctx site name

    /// Build the annotation `SemType` from a resolved external shape addressed by the RESOLVED
    /// identity `symKey`, shared by the stamped and by-name paths. `None` for an unmodelled
    /// body, which has no kind a type annotation can take.
    and private buildExternalTy
        (ctx: PassContext)
        (symKey: TypeKey)
        (shape: ExternalTypeShape)
        (translatedArgs: EqArray<SemType>)
        : SemType option =
        match shape with
        // A referenced intrinsic (`exn = (# "System.Exception" #)`) is NON-transparent: its
        // identity is the shape's canon `TyConst` (`Vesper.exn`) whatever base/ctor surface it
        // carries, which is what keeps `exn.Message` routing to the platform type's members.
        | ExternalTypeShape.Intrinsic s -> Some(TyConst(SymbolKey.Type s.Id.Canon, translatedArgs))
        | ExternalTypeShape.Class _ -> Some(externalClassTy ctx symKey translatedArgs)
        // A capability interface (`disposable`) is a `TyClass` CONSTRAINT; the reverse map
        // holds no interface canons, so its identity is the resolved key directly.
        | ExternalTypeShape.IntrinsicInterface _ -> Some(TyClass(symKey, translatedArgs))
        | ExternalTypeShape.Record _ -> Some(TyRecord(symKey, translatedArgs))
        | ExternalTypeShape.Union _ -> Some(TyUnion(symKey, translatedArgs))
        // An external enum annotation `(x: E)` → `TyEnum key` (never generic), keyed off the
        // SAME identity an `E.Ci` use site mints, so the two unify. The enum is a DISTINCT
        // nominal, not its underlying int/string.
        | ExternalTypeShape.Enum _ -> Some(TyEnum(symKey))
        // A transparent abbreviation dealiases to its body: `int32 = int` resolves to the
        // `int` key the IL encoder encodes, never a nominal `int32`. The frozen RHS is
        // already kind-correct; the type args substitute into it.
        | ExternalTypeShape.Abbrev(_, frozen) ->
            Some(FrozenTypeBridge.instantiateDeclaring frozen (translatedArgs.AsSpan().ToArray()))
        // No modelled body, so no kind a *type annotation* can resolve to. Declining routes
        // the reference to `unresolvedRefTy`, which names the gap.
        | ExternalTypeShape.Unmodelled _ -> None

    /// Fetch + build from an already-resolved external type identity. An arity mismatch is
    /// rejected: it is not this type.
    and tryExternalTypeOfKey (ctx: PassContext) (symKey: TypeKey) (translatedArgs: EqArray<SemType>) : SemType voption =
        let arity = translatedArgs.Length

        match ctx.Provider.TryLookupType(SymbolKey.Type symKey) with
        | ValueSome shape when shape.TyparArity = arity ->
            match buildExternalTy ctx symKey shape translatedArgs with
            | Some ty -> ValueSome ty
            | None -> ValueNone
        | _ -> ValueNone

    /// The store-view read of a written external type reference: NameResolution resolved the
    /// spelling opens-aware at its syntactic arity and recorded the `TypeKey`. No by-name
    /// fallback, because any other verdict means local, typar or unresolvable.
    and private tryResolveExternalTypeStamped
        (ctx: PassContext)
        (nodeKey: NodeKey)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        match ctx.Resolution.TypeRefVerdicts.TryGetValue nodeKey with
        | ValueSome(TypeRefVerdict.ExternalType symKey) -> tryExternalTypeOfKey ctx symKey translatedArgs
        | _ -> ValueNone

    /// A type *spelling* resolved by name: its sole caller is the `float<m>` measure carrier,
    /// wanted at arity 0 where any verdict for that name was recorded at its written arity 1.
    and private tryResolveExternalType
        (ctx: PassContext)
        (qualName: string)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        match NameResolutionTypeRefStamp.tryResolveExternalTypeKey ctx qualName translatedArgs.Length with
        | ValueSome symKey -> tryExternalTypeOfKey ctx symKey translatedArgs
        | ValueNone -> ValueNone

    /// Attach to the constrained typar's TyVar through the current
    /// `ctx.Resolution.TyparScope`.
    and private translateConstraint (ctx: PassContext) (c: Constraint<SyntaxToken>) : unit =
        let typarTokenOf (t: Typar<SyntaxToken>) : SyntaxToken voption =
            match t with
            | Typar.Named(ident = id)
            | Typar.Static(ident = id) -> ValueSome id
            | Typar.Anon _ -> ValueNone

        let attach (typar: Typar<SyntaxToken>) (kind: SemanticConstraintKind) (declTok: SyntaxToken) : unit =
            match typarTokenOf typar with
            | ValueNone -> ()
            | ValueSome id ->
                let name = ctx.NameOf id

                match ctx.Resolution.TyparScope.TryGetValue name with
                | true, tv ->
                    let root = UnionFind.find ctx.Store tv

                    let sc =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofToken declTok NodeKind.TypeVarRef
                        }

                    if not (ctx.Store.Constraints.Items root |> List.exists (fun e -> e.Kind = sc.Kind)) then
                        ctx.Store.Constraints.Prepend(root, sc)
                | false, _ ->
                    ctx.Report(
                        id,
                        Kind.Message(
                            sprintf
                                "Type parameter '%s' in constraint clause is not declared in the enclosing scope"
                                name
                        )
                    )

        match c with
        | Constraint.Equality(typar = tp; equalityToken = tok) -> attach tp SemanticConstraintKind.Equality tok
        | Constraint.Comparison(typar = tp; comparisonToken = tok) -> attach tp SemanticConstraintKind.Comparison tok
        | Constraint.Struct(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.Struct tok
        | Constraint.ReferenceType(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.ReferenceType tok
        | Constraint.Nullness(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.Nullness tok
        | Constraint.NotNull(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.NotNull tok
        | Constraint.Coercion(typar = tp; colonGreaterThan = tok; typ = target) ->
            // `'a :> SomeType`: resolve the target now, while the typar scope is live, and
            // stamp a Coercion constraint on the typar's TyVar. Checked later by subsumption.
            attach tp (SemanticConstraintKind.Coercion(translateType ctx target)) tok
        | Constraint.MemberTrait _
        | Constraint.DefaultConstructor _
        | Constraint.Enum _
        | Constraint.Unmanaged _
        | Constraint.Delegate _
        | Constraint.Default _ ->
            // Each has its own resolution phase (SRTPs / IWSAMs / attribute pass), so the
            // silent skip here is not a missing diagnostic.
            ()

    /// The scope must already contain the constrained typars; callers seed it first.
    and translateConstraints (ctx: PassContext) (tcs: TyparConstraints<SyntaxToken>) : unit =
        for c in tcs.Constraints do
            translateConstraint ctx c

    /// Idempotent, because `Filled` short-circuits. Re-entry through a recursive abbreviation sees
    /// `InProgress`, diagnoses, and freezes `Status` without a `Body`; the outer call then
    /// leaves `Body` at `ValueNone`, so each use site expands to a fresh TyVar.
    and forceFill (ctx: PassContext) (info: AbbreviationInfo) : unit =
        match info.Status with
        | AbbreviationStatus.Filled -> ()
        | AbbreviationStatus.InProgress ->
            ctx.Report(info.DeclSite.Tok, Kind.Message(sprintf "Type abbreviation '%s' is cyclic" info.Name))

            info.Status <- AbbreviationStatus.Filled
        | AbbreviationStatus.NotFilled ->
            info.Status <- AbbreviationStatus.InProgress
            let savedScope = ctx.Resolution.TyparScope
            let savedStrict = ctx.Resolution.TyparScopeStrict
            let scope = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

            for (n, tv) in info.TypeParams do
                if not (scope.ContainsKey n) then
                    scope.[n] <- tv

            ctx.Resolution.TyparScope <- scope
            ctx.Resolution.TyparScopeStrict <- true

            try
                match info.TyparConstraints with
                | ValueSome cs -> translateConstraints ctx cs
                | ValueNone -> ()

                let body = translateType ctx info.RhsCst

                if info.Status = AbbreviationStatus.InProgress then
                    info.Body <- ValueSome body
            finally
                ctx.Resolution.TyparScope <- savedScope
                ctx.Resolution.TyparScopeStrict <- savedStrict
                info.Status <- AbbreviationStatus.Filled

    /// A missing `Body` (cycle, or fill-in not yet run) yields a fresh TyVar rather than
    /// cascading. Prototype-typar constraints are checked against the supplied args HERE:
    /// an abbreviation has no fresh-instance step, and a Defer propagates to free arg TyVars.
    and expandAbbreviation
        (ctx: PassContext)
        (blameTok: SyntaxToken)
        (info: AbbreviationInfo)
        (args: EqArray<SemType>)
        : SemType =
        let n = min (info.TypeParams.Length) args.Length

        for i = 0 to n - 1 do
            let (_, protoTv) = info.TypeParams.[i]
            let arg = args.[i]
            let protoRoot = UnionFind.find ctx.Store protoTv

            for c in ctx.Store.Constraints.Items protoRoot do
                match checkConstraint ctx c arg with
                | Satisfied -> ()
                | Violated -> reportConstraintViolation ctx blameTok c arg
                | Defer -> propagateToFreeArgs ctx c arg

        match info.Body with
        | ValueSome body -> instantiateMember ctx.Store (info.TypeParams, args) body
        | ValueNone -> TyVar(freshTyVar ctx)
