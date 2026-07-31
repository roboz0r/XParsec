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

    /// Fresh unkeyed TypeVar — for intermediate "result" TyVars not tied to
    /// a CST node's NodeKey.
    let freshTyVar (ctx: PassContext) : TyVarId =
        let tv = ctx.NewTypeVar()
        ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
        tv

    /// Overwrites any prior entry — callers that need "get or allocate"
    /// (e.g. forward-referenced let-rec siblings) must go through `tvOf`.
    let freshTv (ctx: PassContext) (key: NodeKey) : TyVarId =
        let tv = ctx.NewTypeVar()
        ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
        ctx.Bindings.TypeVar.Set(key, tv)
        tv

    /// Get-or-allocate: fresh-allocates if missing — happens for binding-site
    /// patterns not yet visited by inferPat, including forward references
    /// inside `let rec` groups.
    let tvOf (ctx: PassContext) (key: NodeKey) : TyVarId =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Report `kind` at `tok` and recover with a fresh TyVar — the pervasive
    /// "diagnose and keep going" shape, so a broken subtree still yields a type
    /// rather than aborting the walk. It takes the KIND, not a rendered message: a
    /// producer reaching recovery through here is as much a producer as one calling
    /// `ctx.Report` directly, and hiding it behind a `string` would hide it from the count.
    let errorTy (ctx: PassContext) (tok: SyntaxToken) (kind: Kind) : SemType =
        ctx.Report(tok, kind)
        TyVar(freshTyVar ctx)

    /// The shared tail of every WRITTEN type head no claim of this file holds and no
    /// external shape built — the one place that decides what a head naming nothing IS.
    ///
    /// A STAMP is NameResolution's committed verdict that the spelling DOES name an external
    /// type, so reaching here with one means the store served a body-less `Opaque` residue:
    /// the name is defined and only its structure is missing, so the head keeps `residue`, the
    /// caller's best-effort identity, and the user is not blamed for a name they got right.
    ///
    /// UNSTAMPED, nothing resolved the head at all, so it is NOT A TYPE. A free `TyVar`
    /// (unifies with everything) or an opaque nominal (unifies with itself) would let ANY
    /// spelling type-check silently, surfacing as unencodable output far from the annotation
    /// that caused it. `TyUnknown` is the identity the contract extractor bakes for a name it
    /// cannot resolve, so both sides agree on what an unresolved name means.
    /// `ctx.UndefinedType` is the shared home of the verdict — the head classifier says the
    /// same thing about a bare head, so a head both reach is blamed once.
    let private unresolvedHeadTy (ctx: PassContext) (head: NodeSite) (name: string) (residue: SemType) : SemType =
        if ctx.Resolution.ResolvedTypeHead.ContainsKey head.Key then
            residue
        else
            // A head the parser inserted spells no place, and `translateType` reaches one
            // with no enclosing declaration in hand — so the verdict is still SAID, at no
            // place, rather than dropped. (`TypeRegistration`'s classifier does hold the
            // written long-ident and widens to its span instead.)
            ctx.UndefinedType(Site.ofToken head.Tok, name)
            TyUnknown name

    /// Multi-segment qualified measure names (`Microsoft.FSharp.SI.kg`) and
    /// measure typars (`'u`) are v2 — they produce an empty term plus a
    /// diagnostic so the rest of inference continues without measure noise.
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

    /// Built-in numeric type names that can carry a measure annotation
    /// (`float<m>`, `int<kg>`). User-defined `[<Measure>]`-aware types land
    /// when records / DUs do.
    ///
    /// Public because a `carrier<arg>` head's ARGUMENT is a unit of measure, not a type:
    /// translation reinterprets it as a measure atom and never resolves it as a type head,
    /// so the classifying walk that diagnoses a head naming nothing must stop at the
    /// carrier for exactly the same shapes — one predicate, so the two cannot drift and
    /// `float<kg>` cannot be diagnosed as an undefined type `kg`.
    let isNumericCarrier (name: string) : bool =
        match name with
        | "int"
        | "int64"
        | "byte"
        | "float"
        | "float32"
        | "decimal"
        | "single"
        | "double" -> true
        | _ -> false

    /// The resolution-time canonicalization seam for a SOURCE-WRITTEN external class
    /// name: a resolved `Class` whose compiled name has a reverse-canon hit is an
    /// intrinsic's platform spelling (`System.Exception` → `exn`), so it resolves to the
    /// canon `TyConst` — the source-name twin of `MetadataSymbols.tryBuildType`'s eager
    /// canonicalization — and no raw BCL nominal enters the unifier. No `IsInterface`
    /// partition is needed: `reverseCanon` carries only intrinsic (`TyConst`) canons (the
    /// `TyparCapture` reverse fold omits capability interfaces), so an interface simply
    /// misses the lookup and keeps its `TyClass` form via the fall-through arm.
    let externalClassTy (ctx: PassContext) (key: TypeKey) (args: EqArray<SemType>) : SemType =
        // The nominal identity is the RESOLVED `key` itself, built on directly — never
        // re-cut from its rendered name. `typeMetaName`/`externalTypeKeyOf` round-trip only
        // on the `InNamespace`/`InType` sublattice, so re-minting an `InModule` key through
        // its `+`-metadata name would flatten the module holder into an `InType` class holder
        // (an unequal identity). The reverse-canon probe still keys on the metadata NAME (its
        // platform-repr entries — `System.Exception` → `exn` — are all bare-IL/`InNamespace`,
        // where name and key agree).
        match ctx.IntrinsicReverseCanon.Value.TryGetValue(SymbolKeyOps.typeMetaName key) with
        | true, (canon :: _) -> TyConst(canon, args)
        | _ -> TyClass(key, args)

    /// DEBUG-only witness for a DOTTED written head that neither the store view nor the
    /// project-local claim answered. It guards the premise the undefined-head verdict rests
    /// on: a head reaching it WITHOUT a stamp names nothing the target can resolve, so the
    /// user may be told the type is not defined. A defect in the resolve-once boundary breaks
    /// that premise in one direction or the other, and each is a lie told at a distance:
    ///
    /// - **No stamp at all**, yet the resolver CAN resolve the spelling — the stamping walk
    ///   missed this syntax position. The read side has no by-name fallback, so a perfectly
    ///   good `System.IO.TextWriter` would be blamed on the USER as an undefined type.
    /// - **Stamped, but the store cannot serve the key** — NameResolution's mint and the
    ///   store view disagree on identity, breaking the round-trip the whole boundary rests
    ///   on. The stamp keeps the head off the undefined verdict, so it silently degrades to a
    ///   free `TyVar` that unifies with anything — a baffling error far from the cause.
    ///
    /// The probes are resolver-view / store-view reaches sanctioned as diagnostics only:
    /// their results are never used to resolve, and Release builds compile them out.
    let private assertNoDottedStampGap
        (ctx: PassContext)
        (nodeKey: NodeKey)
        (li: LongIdent<SyntaxToken>)
        (arity: int)
        : unit =
#if DEBUG
        if li.Idents.Length > 1 then
            let name = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

            match ctx.Resolution.ResolvedTypeHead.TryGetValue nodeKey with
            | ValueNone ->
                match NameResolutionTypeHeadStamp.tryResolveExternalTypeKey ctx name arity with
                | ValueSome key ->
                    failwithf
                        "NameResolution stamping gap: dotted type head '%s' (arity %d) resolves externally to %s but carries no ResolvedTypeHead stamp — a stamping walk missed this syntax position"
                        name
                        arity
                        (SymbolKeyOps.typeMetaName key)
                | ValueNone -> ()
            | ValueSome stamped ->
                match ctx.Provider.TryLookupType(SymbolKey.Type stamped) with
                | ValueNone ->
                    failwithf
                        "External identity round-trip broken: dotted type head '%s' (arity %d) is stamped %s, but the store view cannot serve that key — NameResolution's mint and the store disagree"
                        name
                        arity
                        (SymbolKeyOps.typeMetaName stamped)
                // Served, but the shape declined to build (an `Opaque` residue, or an
                // arity the shape does not carry). The walk reached the node and the
                // store answered — the `TyVar` fallback is by design.
                | ValueSome _ -> ()
#else
        ignore ctx
        ignore nodeKey
        ignore li
        ignore arity
#endif

    /// Reads `ctx.Resolution.TyparScope` for `'a` typar resolution; callers open a
    /// fresh scope per signature (binding or type defn) before walking.
    /// Bare references to generic named types back-fill the arg list with
    /// fresh TyVars so unification can pin them.
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
                    // Strict (type-defn fill-in): implicit free typars aren't
                    // legal F#. Diagnose, but still mint and memoise so later
                    // occurrences share the TyVar and don't cascade.
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
                    // Implicit typar: mint at the binding's current level so
                    // generalisation at binding-group exit picks it up;
                    // memoise so later occurrences share identity.
                    let tv = ctx.NewTypeVar()
                    ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)
                    ctx.Resolution.TyparScope.[name] <- tv
                    TyVar tv
        | Type.VarType(Typar.Anon _) ->
            // `_` typar — always fresh, never stored. Distinct per
            // occurrence, same as `Pat.Wildcard`.
            let tv = freshTyVar ctx
            // Type provenance: `_` is the one INFERRED position inside an otherwise
            // written type (`Box<_>` — `Box` declared, this arg inferred). Mark it so a
            // consumer walking a declared annotation's type can tell the hole apart from
            // its written structure and from a named typar `'a`.
            ctx.MarkInferenceHole tv
            TyVar tv
        | Type.NamedType li when li.Idents.Length = 1 ->
            // Bare single-segment name. A STAMP is NameResolution's committed verdict that the
            // head is external (only a head no local claim held where it was written gets one),
            // so it OUTRANKS the registry: a head above a same-named local declaration keeps
            // resolving to the external type once that declaration registers — F#'s file-order
            // shadowing rule. Unstamped ⇒ local (or nothing), and the registry cascade + opaque
            // fallback answers. The head key is derived the SAME way NameResolution stamped it,
            // so the two sides agree by construction.
            let head = CstKeys.typeHeadSite t

            match tryResolveExternalTypeStamped ctx head.Key EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> resolveBareTypeName ctx li.Idents.[0] (fun _name -> ValueNone)
        | Type.NamedType li ->
            // Qualified named type (`A.T`, `N.A.T`, `System.Text.StringBuilder`). A STAMP is
            // NameResolution's committed verdict that it is EXTERNAL — it stamps only a head
            // no local claim held where it was written — so it outranks the registry here
            // exactly as it does for a bare head. Unstamped ⇒ the qualifier names a scope of
            // THIS file, or the head names nothing.
            let head = CstKeys.typeHeadSite t

            match tryResolveExternalTypeStamped ctx head.Key EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> resolveQualifiedTypeName ctx head li EqArray.empty
        | Type.GenericType(longIdent = li; typeArgs = args) when
            li.Idents.Length = 1
            && args.Length = 1
            && isNumericCarrier (ctx.NameOf li.Idents.[0])
            ->
            // `float<m>` / `int<kg>` — stamp the measure onto a fresh TyVar
            // whose Link carries the carrier.
            //
            // The parser only tags an arg as `TypeArg.Measure` when the
            // measure grammar is unambiguous; for bare `float<m>` it lands
            // as `TypeArg.Type (Type.NamedType "m")` because the type
            // grammar can't tell measure names apart from type-arg type names.
            // Both shapes resolve here.
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
                // Resolve the carrier (`float`) BY NAME rather than fabricating a
                // `Type.NamedType` node: the carrier is SYNTHESIZED here, so NameResolution
                // never walked it and no stamp exists — a store-view read would miss it.
                // This is the ONLY by-name reach left in Unification; every WRITTEN
                // annotation is stamped upstream and reads the store view.
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
            let head = CstKeys.typeHeadSite t
            let name = ctx.NameOf head.Tok

            let translatedArgs =
                EqArray.ofSeq (
                    seq {
                        for a in args ->
                            match a with
                            | TypeArg.Type t -> translateType ctx t
                            // A measure-shaped arg landing on a non-numeric
                            // carrier shouldn't happen in well-formed code,
                            // but stay total — emit a free TyVar.
                            | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                    }
                )

            resolveNamedGeneric ctx head name translatedArgs
        | Type.GenericType(longIdent = li; typeArgs = args) ->
            // Qualified generic type (`A.T<int>`,
            // `System.Collections.Generic.EqualityComparer<int>`); the single-segment forms
            // are handled above. Resolved at the head's WRITTEN arity, exactly as a bare
            // generic head is: `A.T<int>` names the `T\`1` of module `A`, and a same-named
            // `T` at another arity is a different type.
            let translatedArgs =
                EqArray.ofSeq (
                    seq {
                        for a in args ->
                            match a with
                            | TypeArg.Type t -> translateType ctx t
                            | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                    }
                )

            let head = CstKeys.typeHeadSite t

            match tryResolveExternalTypeStamped ctx head.Key translatedArgs with
            | ValueSome ty -> ty
            | ValueNone -> resolveQualifiedTypeName ctx head li translatedArgs
        | Type.SuffixedType(baseType = baseTy; longIdent = li) when li.Idents.Length = 1 ->
            // Postfix generic syntax: `'T list` ≡ `list<'T>`. Multi-arg
            // postfix forms (`(int, string) Map`) parse the base as a tuple
            // and fall to the single-arg arity diagnostic — out of scope for v1.
            let head = CstKeys.typeHeadSite t
            let name = ctx.NameOf head.Tok
            resolveNamedGeneric ctx head name (EqArray.singleton (translateType ctx baseTy))
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple(EqArray.ofSeq (seq { for t in types -> translateType ctx t }))
        | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
            let inner = translateType ctx inner
            translateConstraints ctx cs
            inner
        | Type.ArrayType(baseType = baseTy; commas = commas) ->
            // `'T[]` / `'T[,]` → Vesper's array intrinsic
            // `TyConst(arrayName rank, [elem])` (rank = comma count + 1), the same
            // repr the value side uses (`Infer.fs` array literals). The element goes
            // through `translateType`, so it shares the binding's `TyparScope`: a
            // `'T[]` return / body annotation resolves its `'T` to the *same*
            // signature typar rather than a fresh var (which left the annotation
            // unable to constrain the element and broke `Seq.toArray`'s `'T`).
            let rank = commas.Length + 1

            TyConst(RuntimeNames.arrayKey rank, EqArray.singleton (translateType ctx baseTy))
        | Type.Null _ ->
            // The `null` type — a real *member* of an anonymous union (`T | null`),
            // not a nominal type. Resolves to the cross-backend `nullKey` intrinsic
            // (`Vesper.null`) — the SAME identity the contract extractor and the TS
            // manifest mint for a `null` member, so all three unify — with a per-target
            // repr the backend erases (JS `null`; CLR reference-null). Bare `null`
            // outside a union is just `TyConst nullKey`; its (lack of) assignability is
            // decided later, like any other member.
            TyConst(RuntimeNames.nullKey, EqArray.empty)
        | Type.UnionType(left = l; right = r) ->
            // TypeScript-style anonymous structural union (`X | Y`). The
            // CST is a binary node (left-nested for `a | b | c`); translate both
            // sides and hand them to `mkUnion`, which flattens nested unions,
            // dedups, sorts to canonical order, and collapses a singleton — so
            // `int | string`, `string | int`, and `string | int | string` all yield
            // the one canonical `TyOr [int; string]`. `null`/`undefined` members
            // arrive as the reserved `TyConst`s above.
            mkUnion [ translateType ctx l; translateType ctx r ]
        | _ ->
            // Shapes with no model yet — a multi-segment postfix application
            // (`int A.T`), an anonymous record. Hand back a free TyVar so
            // unification can pin it via context.
            TyVar(freshTyVar ctx)

    /// The `SemType` of the project-local type that CLAIMS `(name, arity)`, applied to
    /// `args`. The name table decides the kind — one claim, one owner, any kind — so
    /// cross-kind precedence is a LOOKUP, never a hand-ordered cascade: an alias
    /// `type Foo<'a> = …` cannot answer for a record `Foo`, because it does not hold that
    /// claim. `ValueNone` only when nothing claims `(name, arity)`; the caller then falls
    /// to its lenient by-name tail (a generic type named without its args, an
    /// arity-mismatched application, an external name).
    ///
    /// A NOMINAL is built from the claim's own `TypeKey` and NOTHING ELSE — no read of the
    /// kind table. That is what "identity alone answers a nominal reference" means, and it
    /// is why a type may name itself and its `and`-joined siblings inside its own declared
    /// structure (`type List<'T> = Cons of 'T * List<'T>`, `type A = { x: B } and B = { y: A }`):
    /// the claim exists before ANY detail registers, so the reference resolves while the
    /// referent's detail is still being built. Only the ABBREVIATION arm reads detail (it
    /// expands a body), which is exactly why an alias RHS is deferred to group close.
    and private resolveClaimedType
        (ctx: PassContext)
        (head: NodeSite)
        (claim: TypeIdentity)
        (args: EqArray<SemType>)
        : SemType voption =
        let key = claim.Key

        match claim.Kind with
        // Primitive binding (`type int = (# "System.Int32" #)`): a nominal intrinsic, NOT
        // a transparent abbreviation. Resolves to `TyConst`; the representation string is
        // consumed later by the codegen `encodeType` rekey. No hardcoded
        // `"int" -> BuiltinTypes.tyInt` arms — primitives resolve uniformly through here,
        // the external provider, or the opaque fallback, all yielding a `TyConst`. Its
        // canon key is contract-sourced and minted at claim time, so this too is identity.
        | TypeDeclKind.IntrinsicRepr -> ValueSome(TyConst(TypeRegistry.intrinsicKeyOf ctx.Types claim.Name, args))
        | TypeDeclKind.Abbreviation ->
            match TypeRegistry.tryAbbrevByKey ctx.Types key with
            | ValueSome info ->
                // Eager expansion: force the body, then substitute the use-site args.
                forceFill ctx info
                ValueSome(expandAbbreviation ctx head.Tok info args)
            | ValueNone -> ValueNone
        | TypeDeclKind.Record -> ValueSome(TyRecord(key, args))
        | TypeDeclKind.Union ->
            // Record the resolved union identity at this use site.
            ctx.Resolution.ResolvedType.Set(head.Key, key)
            ValueSome(TyUnion(key, args))
        | TypeDeclKind.Enum ->
            // An enum is niladic (no type args), so the reference is just `TyEnum Key`;
            // stamp the use site like the union arm.
            ctx.Resolution.ResolvedType.Set(head.Key, key)
            ValueSome(TyEnum key)
        | TypeDeclKind.Class -> ValueSome(TyClass(key, args))

    /// Resolve a bare (single-segment, arity-0) type NAME to its `SemType`: the type
    /// CLAIMING `(name, 0)` if one exists, else the lenient by-name tail — a GENERIC local
    /// type named without its arguments back-fills fresh TyVars (`r : Box` pins them from
    /// `r`'s usage) — else `resolveExternal`, then the `undefined` intrinsic and finally the
    /// shared undefined-head verdict.
    ///
    /// `resolveExternal` is the pluggable external tail: a WRITTEN annotation passes the
    /// STAMPED store-view read, whereas a SYNTHESIZED carrier (the `float<m>` measure arm,
    /// which NameResolution never walked and so never stamped) passes the by-name resolver.
    /// Sharing the cascade keeps the two paths resolving a bare name identically apart from
    /// that one external seam, and lets the measure arm resolve its carrier WITHOUT
    /// fabricating a phantom `Type.NamedType` node that a store-view read would miss.
    and private resolveBareTypeName
        (ctx: PassContext)
        (nameTok: SyntaxToken)
        (resolveExternal: string -> SemType voption)
        : SemType =
        let name = ctx.NameOf nameTok
        let head = NodeSite.ofToken NodeKind.TypeNamed nameTok

        let claimed =
            match TypeRegistry.tryTypeClaim ctx.Types (ctx.UseSiteAt head.Key) name 0 with
            | ValueSome claim -> resolveClaimedType ctx head claim EqArray.empty
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        // Lenient tail: nothing claims the name at arity 0, so a GENERIC local type of that
        // name (declared at some other arity) answers through the SAME any-arity claim lookup
        // and `resolveClaimedType` the arity-0 path resolves through — the name table owns the
        // kind, so this is a LOOKUP, never a cascade over kinds. Its args are back-filled with
        // fresh TyVars at the current level (unpinned at the declaration site, fixed by
        // surrounding unification). An enum never reaches here: it is always the arity-0
        // claimant of its name.
        | ValueNone ->
            let fromLocal =
                match TypeRegistry.tryTypeClaimAnyArity ctx.Types (ctx.UseSiteAt head.Key) name with
                | ValueSome claim ->
                    let args = EqArray.init claim.TyparArity (fun _ -> TyVar(freshTyVar ctx))
                    resolveClaimedType ctx head claim args
                | ValueNone -> ValueNone

            match fromLocal with
            | ValueSome ty -> ty
            | ValueNone ->
                match resolveExternal name with
                | ValueSome ty -> ty
                // `undefined` is a JS-only intrinsic with NO CLR repr. A JS compilation
                // resolves the written name through the provider (the `prim-types-undefined.js`
                // contract); this arm is the deliberate fallback for a stack that has NOT
                // loaded that contract — mint the canonical `undefinedKey` directly so the
                // written name still agrees with the optional-default / Elaborate form. (Not
                // `ctx.Intrinsics.Undefined`, which would loud-fail exactly when the contract
                // is absent — the case this handles.) Every other unresolved bare name is
                // genuinely origin-less.
                | ValueNone when name = RuntimeNames.undefinedTypeName ->
                    TyConst(RuntimeNames.undefinedKey, EqArray.empty)
                | ValueNone -> unresolvedHeadTy ctx head name (TyConst(RuntimeNames.opaqueKey name, EqArray.empty))

    /// Resolve a type head written QUALIFIED (`A.T`, `N.A.T<int>`) whose stamped external
    /// read already missed: so it names a project-local type THROUGH the scope holding it, or
    /// it names nothing. The claim on `(path, name, arity)` answers — the same kind-agnostic
    /// name-table lookup a bare head resolves through, reading from the scope the path names,
    /// so a qualified reference to a record / union / class / enum / alias needs no cascade
    /// of its own.
    ///
    /// A head that claims nothing here and carries no stamp names NOTHING — no scope of this
    /// file holds it, and NameResolution, which resolves every written head against the
    /// target's whole external universe, did not resolve it either. It is undefined, under a
    /// local qualifier or any other, and `unresolvedHeadTy` says so: a free type variable
    /// unifies with everything, so leaving one would type-check the mistake here and surface
    /// it as unencodable IL / wrong JS far away.
    and private resolveQualifiedTypeName
        (ctx: PassContext)
        (head: NodeSite)
        (li: LongIdent<SyntaxToken>)
        (args: EqArray<SemType>)
        : SemType =
        let written = ctx.WrittenTypeNameOf li
        let useSite = ctx.UseSiteAt head.Key

        let claimed =
            match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite written args.Length with
            | ValueSome claim -> resolveClaimedType ctx head claim args
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        | ValueNone ->
            // The name reaches a local type at some OTHER arity: `A.T<int>` where `A` holds a
            // non-generic `T`. The head names that type and gets its arity blamed — never a
            // fall-through to an external type of the same spelling.
            match TypeRegistry.tryWrittenTypeClaimAnyArity ctx.Types useSite written with
            | ValueSome other ->
                errorTy ctx head.Tok (Kind.TypeArgArity(written.Written, other.TyparArity, args.Length))
            | ValueNone ->
                assertNoDottedStampGap ctx head.Key li args.Length
                unresolvedHeadTy ctx head written.Written (TyVar(freshTyVar ctx))

    /// Resolve a single-segment generic type reference. A STAMP on the head outranks the
    /// registry (see the `Type.NamedType` arm: a stamp is NameResolution's committed
    /// "external" verdict, made where no local claim held the name). Unstamped, the type
    /// CLAIMING `(name, argCount)` answers exactly (`resolveClaimedType`, no diagnostic —
    /// the right `Foo\`N`); failing that, the lenient tail resolves the same name at a
    /// *different* arity and diagnoses the mismatch while still producing a best-effort
    /// shape.
    and private resolveNamedGeneric
        (ctx: PassContext)
        (head: NodeSite)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        match tryResolveExternalTypeStamped ctx head.Key translatedArgs with
        | ValueSome ty -> ty
        | ValueNone -> resolveLocalNamedGeneric ctx head name translatedArgs

    and private resolveLocalNamedGeneric
        (ctx: PassContext)
        (head: NodeSite)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        let argCount = translatedArgs.Length

        let claimed =
            match TypeRegistry.tryTypeClaim ctx.Types (ctx.UseSiteAt head.Key) name argCount with
            | ValueSome claim -> resolveClaimedType ctx head claim translatedArgs
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        | ValueNone ->
            // The exact-arity claim missed: the name reaches a local type at some OTHER arity.
            // The SAME any-arity claim lookup + `resolveClaimedType` — one kind-agnostic
            // LOOKUP, never a cascade — answers here too, forwarding the WRITTEN args and
            // blaming the arity. An IntrinsicRepr is the one kind left undiagnosed: a niladic
            // primitive tolerates stray args and a generic one (`array`, repr `!0[]`) forwards
            // them to keep its element type structural, both landing in the same `TyConst` an
            // in-arity reference would.
            let fromLocal =
                match TypeRegistry.tryTypeClaimAnyArity ctx.Types (ctx.UseSiteAt head.Key) name with
                | ValueSome claim ->
                    if claim.Kind <> TypeDeclKind.IntrinsicRepr then
                        ctx.Report(head.Tok, Kind.TypeArgArity(name, claim.TyparArity, argCount))

                    resolveClaimedType ctx head claim translatedArgs
                | ValueNone -> ValueNone

            match fromLocal with
            | ValueSome ty -> ty
            // The stamped external read already missed (`resolveNamedGeneric`), so nothing
            // built this head — the shared undefined-head verdict decides, exactly as for the
            // bare-name arm. Its residue drops the type args: a shape-less head has no
            // parameters to apply them to.
            | ValueNone -> unresolvedHeadTy ctx head name (TyConst(RuntimeNames.opaqueKey name, EqArray.empty))

    /// Build the annotation `SemType` from a resolved external shape addressed by the
    /// RESOLVED identity key `symKey`. Shared by both resolution paths (the stamped
    /// store-view read and the by-name resolver read), so the identity a written type
    /// annotation resolves to is minted in exactly one place. `None` for an `Opaque`
    /// residue, which has no kind a type annotation can take.
    ///
    /// The nominal identity is `symKey` DIRECTLY — never a re-cut from its rendered name.
    /// `typeMetaName`/`externalTypeKeyOf` round-trip only on the `InNamespace`/`InType`
    /// sublattice, so re-minting an `InModule` key through its `+`-metadata name would flatten
    /// the module holder into an `InType` class holder: an unequal identity that misses the
    /// by-key store and mismatches the key construction pins via `ExternalRecordCandidate`.
    and private buildExternalTy
        (ctx: PassContext)
        (symKey: TypeKey)
        (shape: ExternalTypeShape)
        (translatedArgs: EqArray<SemType>)
        : SemType option =
        match shape with
        // A referenced intrinsic — scalar (`exn = (# "System.Exception" #)`) or
        // heritable class: NON-transparent, its NOMINAL IDENTITY is the shape's
        // canon `TyConst` (`Vesper.int`, `Vesper.exn`) regardless of the optional
        // base/ctor surface. Preserving the `TyConst` keeps intrinsic member routing
        // intact — `obj.ToString` / `exn.Message` resolve through the per-target
        // PLATFORM type the contract deliberately does NOT name (ToString is
        // CLR-only), so member resolution MERGES the contract ctors with the platform
        // type's members. A two-key capability `Class` (`disposable`) is an INTERFACE,
        // a constraint not a value type, so it stays a `TyClass` below. (The canon is
        // read OFF the shape — the resolved identity, not a by-name re-mint.)
        | ExternalTypeShape.Intrinsic s -> Some(TyConst(SymbolKey.Type s.Id.Canon, translatedArgs))
        // A source-written platform repr with an extracted non-interface
        // canon (`System.Exception` → `exn`, `System.Object` → `obj`,
        // `System.Int32` → `int`) resolves to the canon `TyConst`; capability
        // INTERFACES keep their `TyClass` form. See `externalClassTy`.
        | ExternalTypeShape.Class _ -> Some(externalClassTy ctx symKey translatedArgs)
        // A capability interface (`disposable`) is a `TyClass` CONSTRAINT — the reverse
        // map holds no interface canons, so it never hits `externalClassTy`'s canon path;
        // its value identity is the resolved key directly.
        | ExternalTypeShape.IntrinsicInterface _ -> Some(TyClass(symKey, translatedArgs))
        | ExternalTypeShape.Record _ -> Some(TyRecord(symKey, translatedArgs))
        | ExternalTypeShape.Union _ -> Some(TyUnion(symKey, translatedArgs))
        // An external enum type annotation `(x: E)` → the nominal `TyEnum key` (no args —
        // enums are never generic), keyed off the SAME resolved identity an `E.Ci` use site
        // mints, so the annotation and the case access unify. The enum is a DISTINCT nominal
        // (NOT its underlying int/string), exactly like the authored `TyEnum`.
        | ExternalTypeShape.Enum _ -> Some(TyEnum(symKey))
        // A transparent abbreviation dealiases to its body: `int32 =
        // int` (`int = (# "System.Int32" #)`) resolves to `TyConst
        // "int"`, the form codegen actually encodes — without this an
        // abbrev name (`int32`) leaked through as a nominal `TyConst
        // "int32"` the IL encoder doesn't key. Mirrors the *local*
        // abbrev expansion (`expandAbbreviation`); `instantiateDeclaring`
        // substitutes the type args into the (already-translated) frozen RHS.
        // The RHS is already kind-correct: the extractor's `mkNominal`
        // baked every head against the defining package's scope
        // so a union/class alias (`'T option = Option<'T>`)
        // expands to a properly-kinded body.
        | ExternalTypeShape.Abbrev(_, frozen) ->
            Some(FrozenTypeBridge.instantiateDeclaring frozen (translatedArgs.AsSpan().ToArray()))
        // An `Opaque` residue (a GADT union / enum / unmodelled body)
        // has no kind to resolve a *type annotation* to — skip
        // it, exactly as a name with no shape did before the
        // residue was registered. The val-signature path that needs the
        // `TyRecord` placeholder goes through `mkNominal`, not here.
        | ExternalTypeShape.Opaque _ -> None

    /// Fetch + build from an already-resolved external type identity: the
    /// key-addressed store-view read shared by every consumer holding a
    /// NameResolution-minted `SymbolKey` (the stamped annotation path, the by-name
    /// hatch, and the generic-ctor stamp read), so the `SemType` a resolved head
    /// yields is minted in exactly one place. `buildExternalTy` mints the nominal on
    /// `symKey` DIRECTLY, so a module-held key's identity survives (no round-trip through
    /// its rendered name). An arity-mismatched shape is rejected (a generic type referenced
    /// at the wrong arity isn't this type, and guards the abbrev/record builders against a
    /// wrong-length arg array).
    and tryExternalTypeOfKey (ctx: PassContext) (symKey: TypeKey) (translatedArgs: EqArray<SemType>) : SemType voption =
        let arity = translatedArgs.Length

        match ctx.Provider.TryLookupType(SymbolKey.Type symKey) with
        | ValueSome shape when shape.TyparArity = arity ->
            match buildExternalTy ctx symKey shape translatedArgs with
            | Some ty -> ValueSome ty
            | None -> ValueNone
        | _ -> ValueNone

    /// The store-view read of a written external type head. NameResolution resolved this
    /// head's spelling (opens-aware, at its syntactic arity) and stamped its `SymbolKey` into
    /// `ResolvedTypeHead` keyed by the `Type` node's `NodeKey`; the shape is then fetched
    /// through the key-addressed store view.
    ///
    /// This has **no by-name fallback** — every written-annotation head is stamped upstream,
    /// so the annotation path is purely store-view and never reaches `ctx.Resolver`. An ABSENT
    /// stamp is not an external type: NameResolution walks every written head (field,
    /// member-sig, param, return, cast, type-test, `new`, `inherit`, type-app, the
    /// ILIntrinsic-body result annotation, and a type header's typar constraints), so a node
    /// it left unstamped is project-local, a bare typar, or unresolvable — exactly the cases
    /// the caller resolves as a local shape / `TyVar` / opaque `TyConst`. The one by-name
    /// reach that remains is for a head with NO `Type` node to carry a stamp: the `float<m>`
    /// measure carrier synthesized during inference.
    and private tryResolveExternalTypeStamped
        (ctx: PassContext)
        (nodeKey: NodeKey)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        match ctx.Resolution.ResolvedTypeHead.TryGetValue nodeKey with
        | ValueSome symKey -> tryExternalTypeOfKey ctx symKey translatedArgs
        | ValueNone -> ValueNone

    /// The sanctioned by-name resolver reach — a written type *spelling* resolved
    /// through `ctx.Resolver` for a head with no `Type` node to carry a stamp (its
    /// sole client is the `float<m>` measure carrier synthesized during inference).
    /// Spelling → key goes through NameResolution's own engine
    /// (`tryResolveExternalTypeKey` — the one home for opens-aware external-type
    /// resolution, so the hatch cannot drift from what the stamper would have
    /// stamped), then the identity builds through the same store-view read the
    /// stamped path uses.
    and private tryResolveExternalType
        (ctx: PassContext)
        (qualName: string)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        match NameResolutionTypeHeadStamp.tryResolveExternalTypeKey ctx qualName translatedArgs.Length with
        | ValueSome symKey -> tryExternalTypeOfKey ctx symKey translatedArgs
        | ValueNone -> ValueNone

    /// Attach to the constrained typar's TyVar through the current
    /// `ctx.Resolution.TyparScope`. Unsupported kinds (Coercion, MemberTrait, etc.) are
    /// skipped — they belong to their own resolution phases.
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
            // `'a :> SomeType`: resolve the target now (typar scope is live) and
            // stamp a Coercion constraint on the typar's TyVar, mirroring the
            // trait arms. Checked later by `checkConstraint` via `subsumes`.
            attach tp (SemanticConstraintKind.Coercion(translateType ctx target)) tok
        | Constraint.MemberTrait _
        | Constraint.DefaultConstructor _
        | Constraint.Enum _
        | Constraint.Unmanaged _
        | Constraint.Delegate _
        | Constraint.Default _ ->
            // v1 skips these — each has its own resolution phase (SRTPs /
            // IWSAMs / attribute pass). Silent skip, not a diagnostic.
            ()

    /// The scope must already contain the constrained typars — callers
    /// (binding-level, type-defn fill-in, inline `WhenConstrainedType`)
    /// seed it first.
    and translateConstraints (ctx: PassContext) (tcs: TyparConstraints<SyntaxToken>) : unit =
        for c in tcs.Constraints do
            translateConstraint ctx c

    /// Idempotent — already-`Filled` entries short-circuit. Re-entry through
    /// a recursive abbreviation reference detects the cycle (`InProgress`),
    /// emits a diagnostic, and freezes `Status` to `Filled` without setting
    /// `Body`. The outer call notices `Status` flipped mid-walk and skips
    /// assigning `Body`, leaving `ValueNone` so the expansion arm
    /// substitutes a fresh TyVar per use site instead of a stale one.
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

    /// Returns a fresh TyVar if `Body = ValueNone` (cycle detected, or
    /// fill-in not yet run) — best-effort rather than cascading.
    ///
    /// Constraints on the prototype typars are evaluated against the supplied
    /// args here: unlike records / unions, an abbreviation has no
    /// fresh-instance step that would let `dischargeConstraints` fire on its own.
    /// A Defer outcome propagates the constraint to any free TyVar inside the
    /// supplied arg so a later unification re-fires the check.
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
