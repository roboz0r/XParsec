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
    let freshTyVar (ctx: PassContext) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        tv

    /// Overwrites any prior entry — callers that need "get or allocate"
    /// (e.g. forward-referenced let-rec siblings) must go through `tvOf`.
    let freshTv (ctx: PassContext) (key: NodeKey) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        ctx.Bindings.TypeVar.Set(key, tv)
        tv

    /// Get-or-allocate: fresh-allocates if missing — happens for binding-site
    /// patterns not yet visited by inferPat, including forward references
    /// inside `let rec` groups.
    let tvOf (ctx: PassContext) (key: NodeKey) : TypeVar =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Report an error at `key` and recover with a fresh TyVar — the pervasive
    /// "diagnose and keep going" shape, so a broken subtree still yields a type
    /// rather than aborting the walk.
    let errorTy (ctx: PassContext) (key: NodeKey) (msg: string) : SemType =
        ctx.Error(key, msg)
        TyVar(freshTyVar ctx)

    /// The shared tail of every WRITTEN type head no claim of this unit holds and no
    /// external shape built — the one place that decides what a head naming nothing IS.
    ///
    /// A STAMP is NameResolution's committed verdict that the spelling DOES name a type of
    /// the external universe; reaching here with one means the store served a shape with no
    /// kind an annotation can take (an `ExternalTypeShape.Opaque` residue — a body-less
    /// external type). The name is defined and only its structure is missing, so the head
    /// keeps `residue`, the caller's best-effort identity, and the user is not blamed for a
    /// name they got right.
    ///
    /// UNSTAMPED, nothing resolved the head at all — not a scope of this unit, not the
    /// target's view of the world — so it is NOT A TYPE. Recovering with a free `TyVar` (which
    /// unifies with everything) or an opaque nominal (which unifies with itself) would let ANY
    /// spelling type-check silently, and the mistake would surface as unencodable output far
    /// from the annotation that caused it. `TyUnknown` is the identity the contract extractor
    /// already bakes for a name it cannot resolve, so the two faces agree on what an
    /// unresolved name means. `ctx.UndefinedType` is the shared home of the verdict — the head
    /// classifier reaches a bare head first and says the same thing, and a head both reach is
    /// blamed once.
    let private unresolvedHeadTy (ctx: PassContext) (diagKey: NodeKey) (name: string) (residue: SemType) : SemType =
        if ctx.Resolution.ResolvedTypeHead.ContainsKey diagKey then
            residue
        else
            ctx.UndefinedType(diagKey, name)
            TyUnknown name

    /// Multi-segment qualified unit names (`Microsoft.FSharp.SI.kg`) and
    /// measure typars (`'u`) are v2 — they produce an empty term plus a
    /// diagnostic so the rest of inference continues without measure noise.
    let rec translateMeasure (ctx: PassContext) (diagKey: NodeKey) (m: Measure<SyntaxToken>) : MeasureTerm =
        match m with
        | Measure.One _ -> MeasureTerm.empty
        | Measure.Named li when li.Idents.Length = 1 -> MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], Rational.One ]
        | Measure.Power(inner, _, neg, expTok) ->
            let n = System.Numerics.BigInteger.Parse(ctx.NameOf expTok)
            let signed = if neg.IsSome then -n else n

            MeasureTerm.pow
                (translateMeasure ctx diagKey inner)
                (Rational.create (signed, System.Numerics.BigInteger.One))
        | Measure.Product(l, _, r) -> MeasureTerm.mul (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Quotient(l, _, r) -> MeasureTerm.div (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx diagKey inner)
        | Measure.Paren(_, inner, _) -> translateMeasure ctx diagKey inner
        | Measure.Juxtaposition(elems, _) ->
            (MeasureTerm.empty, elems)
            ||> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx diagKey m))
        | Measure.Anonymous _
        | Measure.Typar _
        | Measure.Named _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = "Measure typars / wildcards / qualified unit names not yet supported"
                    Code = ""
                    Severity = Severity.Error
                }

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
    let externalClassTy
        (ctx: PassContext)
        (compiled: string)
        (info: ExternalClassShape)
        (arity: int)
        (args: EqArray<SemType>)
        : SemType =
        match ctx.IntrinsicReverseCanon.Value.TryGetValue compiled with
        | true, (canon :: _) -> TyConst(canon, args)
        | _ -> TyClass(SymbolKeyOps.externalTypeKeyOf info.Origin compiled arity, args)

    /// DEBUG-only witness for a DOTTED written head that neither the store face nor the
    /// project-local claim answered. It guards the premise `unresolvedHeadTy` rests on: that
    /// a head reaching it WITHOUT a stamp names nothing the target can resolve, so the user
    /// may be told the type is not defined. A defect in the resolve-once boundary breaks that
    /// premise in one direction or the other, and each is a lie told at a distance:
    ///
    /// - **No stamp at all**, yet the resolver CAN resolve the spelling — the stamping walk
    ///   failed to reach this syntax position. The read side has no by-name fallback, so a
    ///   perfectly good `System.IO.TextWriter` would be blamed on the USER as an undefined
    ///   type. Fail loudly at the compiler's own defect instead of accusing the source.
    /// - **Stamped, but the store cannot serve the key** — NameResolution's mint and the
    ///   store face disagree on identity, so the round-trip the whole boundary rests on is
    ///   broken for this key. The stamp keeps the head off the undefined verdict, so it
    ///   silently degrades to a free `TyVar` that unifies with anything — a baffling error
    ///   (or wrong codegen) far from the cause.
    ///
    /// A stamp the store DOES serve but whose shape declines to build a type
    /// (`ExternalTypeShape.Opaque` — a body-less residue with no kind to resolve an
    /// annotation to) is NOT a defect: the walk reached the node and the store answered, so
    /// the name IS defined and the `TyVar` residue is the designed outcome. The witness stays
    /// silent.
    ///
    /// The probes are resolver-face / store-face reaches sanctioned as diagnostics only:
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
                        "External identity round-trip broken: dotted type head '%s' (arity %d) is stamped %s, but the store face cannot serve that key — NameResolution's mint and the store disagree"
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
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken id NodeKind.TypeVarRef
                            Message =
                                sprintf
                                    "Free type parameter %s is not declared in the enclosing type's type-parameter list"
                                    name
                            Code = ""
                            Severity = Severity.Error
                        }

                    let tv = TypeVar()
                    tv.Level <- ctx.CurrentLevel
                    ctx.Resolution.TyparScope.[name] <- tv
                    TyVar tv
                else
                    // Implicit typar: mint at the binding's current level so
                    // generalisation at binding-group exit picks it up;
                    // memoise so later occurrences share identity.
                    let tv = TypeVar()
                    tv.Level <- ctx.CurrentLevel
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
            // Bare single-segment name. A STAMP on this head is NameResolution's committed
            // verdict that it is external (`classifyTypeHead` stamps only a head no local
            // claim held where it was written), so it OUTRANKS the registry: a head above a
            // same-named local declaration keeps resolving to the external type once that
            // declaration registers, which is F#'s file-order shadowing rule. Unstamped ⇒
            // local (or nothing), and the registry cascade + opaque fallback answers — the
            // same cascade the measure carrier below shares through `resolveBareTypeName`.
            // The head key comes from `CstKeys.ofTypeHead`, the SAME derivation
            // NameResolution stamped with, so the two faces agree by construction.
            let headKey = CstKeys.typeHeadKey t

            match tryResolveExternalTypeStamped ctx headKey EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> resolveBareTypeName ctx li.Idents.[0] (fun _name -> ValueNone)
        | Type.NamedType li ->
            // Qualified named type (`A.T`, `N.A.T`, `System.Text.StringBuilder`). A STAMP is
            // NameResolution's committed verdict that it is EXTERNAL — it stamps only a head
            // no local claim held where it was written — so it outranks the registry here
            // exactly as it does for a bare head. Unstamped ⇒ the qualifier names a scope of
            // THIS unit, or the head names nothing.
            let headKey = CstKeys.typeHeadKey t

            match tryResolveExternalTypeStamped ctx headKey EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> resolveQualifiedTypeName ctx headKey li EqArray.empty
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
            // grammar can't tell unit names apart from type-arg type names.
            // Both shapes resolve here.
            let carrierTok = li.Idents.[0]
            let diagKey = NodeKey.ofToken carrierTok NodeKind.TypeGeneric

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
                let mt = translateMeasure ctx diagKey m
                let tv = freshTyVar ctx
                // Resolve the carrier (`float`) BY NAME rather than fabricating a
                // `Type.NamedType li` node and re-entering `translateType`: the carrier
                // is SYNTHESIZED here, so NameResolution never walked it and no stamp
                // exists — a store-face read would miss it. This is the ONLY by-name
                // reach left in Unification: the one head with no `Type` node to carry
                // a stamp. Every WRITTEN annotation is stamped upstream and reads the
                // store face through `tryResolveExternalTypeStamped`, which has no
                // by-name fallback.
                tv.Link <-
                    ValueSome(
                        resolveBareTypeName ctx carrierTok (fun name -> tryResolveExternalType ctx name EqArray.empty)
                    )

                tv.Units <- ValueSome mt
                TyVar tv
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let nameTok = li.Idents.[0]
            let name = ctx.NameOf nameTok
            let diagKey = CstKeys.typeHeadKey t

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

            resolveNamedGeneric ctx diagKey name translatedArgs
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

            let headKey = CstKeys.typeHeadKey t

            match tryResolveExternalTypeStamped ctx headKey translatedArgs with
            | ValueSome ty -> ty
            | ValueNone -> resolveQualifiedTypeName ctx headKey li translatedArgs
        | Type.SuffixedType(baseType = baseTy; longIdent = li) when li.Idents.Length = 1 ->
            // Postfix generic syntax: `'T list` ≡ `list<'T>`. Multi-arg
            // postfix forms (`(int, string) Map`) parse the base as a tuple
            // and fall to the single-arg arity diagnostic — out of scope for v1.
            let nameTok = li.Idents.[0]
            let name = ctx.NameOf nameTok
            let diagKey = CstKeys.typeHeadKey t
            resolveNamedGeneric ctx diagKey name (EqArray.singleton (translateType ctx baseTy))
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
        (diagKey: NodeKey)
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
                ValueSome(expandAbbreviation ctx diagKey info args)
            | ValueNone -> ValueNone
        | TypeDeclKind.Record -> ValueSome(TyRecord(key, args))
        | TypeDeclKind.Union ->
            // Record the resolved union identity at this use site.
            ctx.Resolution.ResolvedType.Set(diagKey, key)
            ValueSome(TyUnion(key, args))
        | TypeDeclKind.Enum ->
            // An enum is niladic (no type args), so the reference is just `TyEnum Key`;
            // stamp the use site like the union arm.
            ctx.Resolution.ResolvedType.Set(diagKey, key)
            ValueSome(TyEnum key)
        | TypeDeclKind.Class -> ValueSome(TyClass(key, args))

    /// Resolve a bare (single-segment, arity-0) type NAME to its `SemType`: the type
    /// CLAIMING `(name, 0)` if one exists (`resolveClaimedType`), else the lenient
    /// by-name tail — a GENERIC local type named without its arguments back-fills fresh
    /// TyVars (`r : Box` pins them from `r`'s usage) — else `resolveExternal`, then the
    /// `undefined` intrinsic and finally `unresolvedHeadTy`, the shared undefined-head
    /// verdict. `resolveExternal` is the pluggable external tail: a
    /// WRITTEN annotation (`Type.NamedType` arm) passes the STAMPED store-face read
    /// (`tryResolveExternalTypeStamped`), whereas a SYNTHESIZED carrier (the `float<m>`
    /// measure arm) — which NameResolution never walked and so never stamped — passes the
    /// by-name resolver. Sharing the cascade keeps the two faces resolving a bare name
    /// identically apart from that one external seam, and lets the measure arm resolve its
    /// carrier WITHOUT fabricating a phantom `Type.NamedType` node that a store-face read
    /// would miss.
    and private resolveBareTypeName
        (ctx: PassContext)
        (nameTok: SyntaxToken)
        (resolveExternal: string -> SemType voption)
        : SemType =
        let name = ctx.NameOf nameTok
        let diagKey = NodeKey.ofToken nameTok NodeKind.TypeNamed

        let claimed =
            match TypeRegistry.tryTypeClaim ctx.Types (ctx.UseSiteAt diagKey) name 0 with
            | ValueSome claim -> resolveClaimedType ctx diagKey claim EqArray.empty
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        // Lenient tail: nothing claims the name at arity 0, so a GENERIC local type of
        // that name (declared at some other arity) answers, its args back-filled with
        // fresh TyVars at the current level — unpinned at the declaration site, fixed by
        // surrounding unification. An enum never reaches here: it is always the arity-0
        // claimant of its name.
        | ValueNone ->
            if ctx.Types.IntrinsicReprTypes.ContainsKey name then
                TyConst(TypeRegistry.intrinsicKeyOf ctx.Types name, EqArray.empty)
            else
                match TypeRegistry.tryAbbrev ctx.Types (ctx.UseSiteAt diagKey) name with
                | ValueSome info ->
                    forceFill ctx info
                    let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))
                    expandAbbreviation ctx diagKey info args
                | ValueNone ->
                    match TypeRegistry.tryRecord ctx.Types (ctx.UseSiteAt diagKey) name with
                    | ValueSome info ->
                        let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))
                        TyRecord(info.TypeKey, args)
                    | ValueNone ->
                        match TypeRegistry.tryUnionBare ctx.Types (ctx.UseSiteAt diagKey) name with
                        | ValueSome info ->
                            let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))
                            ctx.Resolution.ResolvedType.Set(diagKey, info.TypeKey)
                            TyUnion(info.TypeKey, args)
                        | ValueNone ->
                            match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
                            | ValueSome info ->
                                let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))
                                TyClass(info.TypeKey, args)
                            | ValueNone ->
                                match resolveExternal name with
                                | ValueSome ty -> ty
                                // `undefined` is a JS-only intrinsic with NO CLR repr. A
                                // JS compilation resolves the written name through the
                                // provider (the `prim-types-undefined.js` contract); this
                                // arm is the deliberate fallback for a stack that has NOT
                                // loaded that contract — mint the canonical `undefinedKey`
                                // directly so the written name still agrees with the
                                // optional-default / Elaborate form. (Not
                                // `ctx.Intrinsics.Undefined`, which would loud-fail
                                // exactly when the contract is absent — the case this
                                // handles.) Every other unresolved bare name is genuinely
                                // origin-less.
                                | ValueNone when name = RuntimeNames.undefinedTypeName ->
                                    TyConst(RuntimeNames.undefinedKey, EqArray.empty)
                                | ValueNone ->
                                    unresolvedHeadTy
                                        ctx
                                        diagKey
                                        name
                                        (TyConst(RuntimeNames.opaqueKey name, EqArray.empty))

    /// Resolve a type head written QUALIFIED (`A.T`, `N.A.T<int>`) whose stamped external
    /// read already missed: so it names a project-local type THROUGH the scope holding it, or
    /// it names nothing. The claim on `(path, name, arity)` answers — the same kind-agnostic
    /// name-table lookup a bare head resolves through, reading from the scope the path names,
    /// so a qualified reference to a record / union / class / enum / alias needs no cascade
    /// of its own.
    ///
    /// A head that claims nothing here and carries no stamp names NOTHING — no scope of this
    /// unit holds it, and NameResolution, which resolves every written head against the
    /// target's whole external universe, did not resolve it either. It is undefined, under a
    /// local qualifier or any other, and `unresolvedHeadTy` says so: a free type variable
    /// unifies with everything, so leaving one would type-check the mistake here and surface
    /// it as unencodable IL / wrong JS far away.
    and private resolveQualifiedTypeName
        (ctx: PassContext)
        (diagKey: NodeKey)
        (li: LongIdent<SyntaxToken>)
        (args: EqArray<SemType>)
        : SemType =
        let written = ctx.WrittenTypeNameOf li
        let useSite = ctx.UseSiteAt diagKey

        let claimed =
            match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite written args.Length with
            | ValueSome claim -> resolveClaimedType ctx diagKey claim args
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        | ValueNone ->
            // The name reaches a local type at some OTHER arity: `A.T<int>` where `A` holds a
            // non-generic `T`. The head names that type and gets its arity blamed — never a
            // fall-through to an external type of the same spelling.
            match TypeRegistry.tryWrittenTypeClaimAnyArity ctx.Types useSite written with
            | ValueSome other ->
                errorTy
                    ctx
                    diagKey
                    (sprintf "Type '%s' expects %d type argument(s) but got %d" written.Written other.Arity args.Length)
            | ValueNone ->
                assertNoDottedStampGap ctx diagKey li args.Length
                unresolvedHeadTy ctx diagKey written.Written (TyVar(freshTyVar ctx))

    /// Resolve a single-segment generic type reference. A STAMP on the head outranks the
    /// registry (see the `Type.NamedType` arm: a stamp is NameResolution's committed
    /// "external" verdict, made where no local claim held the name). Unstamped, the type
    /// CLAIMING `(name, argCount)` answers exactly (`resolveClaimedType`, no diagnostic —
    /// the right `Foo\`N`); failing that, the lenient tail resolves the same name at a
    /// *different* arity and diagnoses the mismatch while still producing a best-effort
    /// shape.
    and private resolveNamedGeneric
        (ctx: PassContext)
        (diagKey: NodeKey)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        match tryResolveExternalTypeStamped ctx diagKey translatedArgs with
        | ValueSome ty -> ty
        | ValueNone -> resolveLocalNamedGeneric ctx diagKey name translatedArgs

    and private resolveLocalNamedGeneric
        (ctx: PassContext)
        (diagKey: NodeKey)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        let argCount = translatedArgs.Length

        let checkArity (expected: int) : unit =
            if expected <> argCount then
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Type '%s' expects %d type argument(s) but got %d" name expected argCount
                        Code = ""
                        Severity = Severity.Error
                    }

        // The arity-mismatched tail for a project-local nominal: the bare-name resolution
        // of a single, *different* arity, keeping the "expects N got M" diagnostic. `mkTy`
        // builds the shape and performs any kind-specific use-site stamping.
        let resolveLocalGeneric
            (byBareName: string -> 'I voption)
            (typeParamsLen: 'I -> int)
            (mkTy: 'I -> SemType)
            : SemType voption =
            match byBareName name with
            | ValueSome info ->
                checkArity (typeParamsLen info)
                ValueSome(mkTy info)
            | ValueNone -> ValueNone

        let claimed =
            match TypeRegistry.tryTypeClaim ctx.Types (ctx.UseSiteAt diagKey) name argCount with
            | ValueSome claim -> resolveClaimedType ctx diagKey claim translatedArgs
            | ValueNone -> ValueNone

        match claimed with
        | ValueSome ty -> ty
        | ValueNone ->

            if ctx.Types.IntrinsicReprTypes.ContainsKey name then
                // A primitive binding referenced at an arity it was not declared at. A
                // *generic* intrinsic (the array `[]`, repr `!0[]`) forwards its type args so
                // the element type stays structural; an argless primitive referenced with
                // stray args degenerates to the same `TyConst(name, [])` an argless reference
                // produces.
                TyConst(TypeRegistry.intrinsicKeyOf ctx.Types name, translatedArgs)
            else
                match TypeRegistry.tryAbbrev ctx.Types (ctx.UseSiteAt diagKey) name with
                | ValueSome info ->
                    forceFill ctx info
                    checkArity (info.TypeParams.Length)
                    expandAbbreviation ctx diagKey info translatedArgs
                | ValueNone ->
                    // Record, union, then class: a union additionally stamps the resolved use
                    // site; record and class do not.
                    let local =
                        resolveLocalGeneric
                            (TypeRegistry.tryRecord ctx.Types (ctx.UseSiteAt diagKey))
                            (fun i -> i.TypeParams.Length)
                            (fun info -> TyRecord(info.TypeKey, translatedArgs))
                        |> ValueOption.orElseWith (fun () ->
                            resolveLocalGeneric
                                (TypeRegistry.tryUnionBare ctx.Types (ctx.UseSiteAt diagKey))
                                (fun i -> i.TypeParams.Length)
                                (fun info ->
                                    ctx.Resolution.ResolvedType.Set(diagKey, info.TypeKey)
                                    TyUnion(info.TypeKey, translatedArgs)
                                )
                        )
                        |> ValueOption.orElseWith (fun () ->
                            resolveLocalGeneric
                                (TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey))
                                (fun i -> i.TypeParams.Length)
                                (fun info -> TyClass(info.TypeKey, translatedArgs))
                        )

                    match local with
                    | ValueSome ty -> ty
                    // The stamped external read already missed (`resolveNamedGeneric`), so
                    // nothing built this head — the shared undefined-head verdict decides,
                    // exactly as for the bare-name arm. Its residue drops the type args: a
                    // shape-less head has no parameters to apply them to.
                    | ValueNone ->
                        unresolvedHeadTy ctx diagKey name (TyConst(RuntimeNames.opaqueKey name, EqArray.empty))

    /// Build the annotation `SemType` from a resolved external shape + its matched
    /// compiled name. Shared by both resolution faces (the stamped store-face read
    /// and the by-name resolver read), so the identity a written type annotation
    /// resolves to is minted in exactly one place. `None` for an `Opaque` residue,
    /// which has no kind a type annotation can take.
    and private buildExternalTy
        (ctx: PassContext)
        (compiled: string)
        (shape: ExternalTypeShape)
        (arity: int)
        (translatedArgs: EqArray<SemType>)
        : SemType option =
        // Mint the nominal's `SymbolKey` from the resolved shape's origin + the
        // matched compiled name. `asm = Some` marks it external.
        match shape with
        // A referenced intrinsic — scalar (`exn = (# "System.Exception" #)`)
        // or heritable class: NON-transparent, its NOMINAL IDENTITY is the
        // shape's canon `TyConst` (`Vesper.int`, `Vesper.exn`) regardless of
        // the optional base/ctor surface. Preserving the `TyConst` keeps
        // intrinsic member routing intact — `obj.ToString` / `exn.Message`
        // resolve through the PLATFORM type (`IntrinsicBclMember`), which is
        // per-target and which the contract deliberately does NOT name
        // (ToString is CLR-only); member resolution thus MERGES the contract
        // ctors with the platform type's members. A faced capability `Class`
        // (`disposable`) is an INTERFACE, a constraint not a value type, so
        // it stays a `TyClass` below. (The canon is read OFF the shape — the
        // resolved identity, not a by-name re-mint.)
        | ExternalTypeShape.Intrinsic s -> Some(TyConst(SymbolKey.Type s.Id.Canon, translatedArgs))
        // A source-written platform repr with a harvested non-interface
        // canon (`System.Exception` → `exn`, `System.Object` → `obj`,
        // `System.Int32` → `int`) resolves to the canon `TyConst`; capability
        // INTERFACES keep their `TyClass` form. See `externalClassTy`.
        | ExternalTypeShape.Class info -> Some(externalClassTy ctx compiled info arity translatedArgs)
        // A capability interface (`disposable`) is a `TyClass` CONSTRAINT — its
        // value identity key is origin-homed exactly as a `Class`'s (the reverse
        // map holds no interface canons, so `externalClassTy`'s reverse hit never
        // fires for it; this bypasses that check and mints the `TyClass` directly).
        | ExternalTypeShape.IntrinsicInterface iface ->
            Some(TyClass(SymbolKeyOps.externalTypeKeyOf iface.Origin compiled arity, translatedArgs))
        | ExternalTypeShape.Record(origin = origin) ->
            Some(TyRecord(SymbolKeyOps.externalTypeKeyOf origin compiled arity, translatedArgs))
        | ExternalTypeShape.Union(origin = origin) ->
            Some(TyUnion(SymbolKeyOps.externalTypeKeyOf origin compiled arity, translatedArgs))
        // An external enum type annotation `(x: E)` → the nominal
        // `TyEnum key` (no args — enums are never generic), keyed off
        // the same `externalTypeKey origin key 0` an `E.Ci` use site
        // mints, so the annotation and the case access unify. The
        // enum is a DISTINCT nominal (NOT its underlying int/string),
        // exactly like the authored `TyEnum`.
        | ExternalTypeShape.Enum(origin = origin) -> Some(TyEnum(SymbolKeyOps.externalTypeKeyOf origin compiled 0))
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
    /// key-addressed store-face read shared by every consumer holding a
    /// NameResolution-minted `SymbolKey` (the stamped annotation path, the by-name
    /// hatch, and the generic-ctor stamp read), so the `SemType` a resolved head
    /// yields is minted in exactly one place. `qualifiedName` recovers the compiled
    /// name the shape builder needs from the key (identity-preserving: the key was
    /// minted from the same compiled name, so the round-trip is exact). An
    /// arity-mismatched shape is rejected (a generic type referenced at the wrong
    /// arity isn't this type, and guards the abbrev/record builders against a
    /// wrong-length arg array).
    and tryExternalTypeOfKey (ctx: PassContext) (symKey: TypeKey) (translatedArgs: EqArray<SemType>) : SemType voption =
        let arity = translatedArgs.Length

        match ctx.Provider.TryLookupType(SymbolKey.Type symKey) with
        | ValueSome shape when shape.Arity = arity ->
            match buildExternalTy ctx (SymbolKeyOps.typeMetaName symKey) shape arity translatedArgs with
            | Some ty -> ValueSome ty
            | None -> ValueNone
        | _ -> ValueNone

    /// The store-face read of a written external type head. NameResolution resolved
    /// this head's spelling (opens-aware, at its syntactic arity) and stamped its
    /// `SymbolKey` into `ResolvedTypeHead` keyed by the `Type` node's `NodeKey`;
    /// `tryExternalTypeOfKey` fetches the shape through the key-addressed store face.
    ///
    /// This has **no by-name fallback** — every written-annotation head is stamped
    /// upstream, so the annotation path is purely store-face and never reaches
    /// `ctx.Resolver`. An ABSENT stamp is not an external type: NameResolution walks
    /// every written head (`CstWalk.iterType` over field / member-sig / param /
    /// return / cast / type-test / `new` / `inherit` / type-app positions, the
    /// ILIntrinsic-body result annotation, and a type header's typar-definition
    /// constraints), so a node it left unstamped is project-local, a bare typar, or
    /// unresolvable — exactly the cases `translateType`'s caller resolves as a local
    /// shape / `TyVar` / opaque `TyConst`. The one by-name reach that remains
    /// (`tryResolveExternalType`) is for a head with NO `Type` node to carry a
    /// stamp: the `float<m>` measure carrier synthesized during inference.
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
    /// stamped), then the identity builds through the same store-face read the
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
                    let root = UnionFind.find tv

                    let sc =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofToken declTok NodeKind.TypeVarRef
                        }

                    if not (root.Constraints |> List.exists (fun e -> e.Kind = sc.Kind)) then
                        root.Constraints <- sc :: root.Constraints
                | false, _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken id NodeKind.TypeVarRef
                            Message =
                                sprintf
                                    "Type parameter '%s' in constraint clause is not declared in the enclosing scope"
                                    name
                            Code = ""
                            Severity = Severity.Error
                        }

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
        let (TyparConstraints(constraints = cs)) = tcs

        for c in cs do
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
            ctx.Diagnostics.Add
                {
                    Key = info.DeclKey
                    Message = sprintf "Type abbreviation '%s' is cyclic" info.Name
                    Code = ""
                    Severity = Severity.Error
                }

            info.Status <- AbbreviationStatus.Filled
        | AbbreviationStatus.NotFilled ->
            info.Status <- AbbreviationStatus.InProgress
            let savedScope = ctx.Resolution.TyparScope
            let savedStrict = ctx.Resolution.TyparScopeStrict
            let scope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

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
    /// fresh-instance step that would let `drainConstraints` fire on its own.
    /// A Defer outcome propagates the constraint to any free TyVar inside the
    /// supplied arg so a later unification re-fires the check.
    and expandAbbreviation
        (ctx: PassContext)
        (diagKey: NodeKey)
        (info: AbbreviationInfo)
        (args: EqArray<SemType>)
        : SemType =
        let n = min (info.TypeParams.Length) args.Length

        for i = 0 to n - 1 do
            let (_, protoTv) = info.TypeParams.[i]
            let arg = args.[i]
            let protoRoot = UnionFind.find protoTv

            for c in protoRoot.Constraints do
                match checkConstraint ctx c arg with
                | Satisfied -> ()
                | Violated ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message =
                                sprintf
                                    "The type '%A' does not support the '%s' constraint"
                                    (zonk arg)
                                    (constraintKindName c.Kind)
                            Code = ""
                            Severity = Severity.Error
                        }
                | Defer -> propagateToFreeArgs ctx c arg

        match info.Body with
        | ValueSome body -> instantiateMember (info.TypeParams, args) body
        | ValueNone -> TyVar(freshTyVar ctx)
