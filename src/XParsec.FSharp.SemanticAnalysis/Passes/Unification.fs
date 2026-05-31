namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate
open UnificationInfer

// Algorithm J + Rémy's levels.
//
// Pre:  ctx.Desugared and ctx.Bindings.Binding populated.
// Post: ctx.Bindings.TypeVar populated; every TypeVar's Link reaches its solved type
//       via UnionFind.find. ctx.Bindings.Scheme populated for every generalisable
//       `let`-bound name (single-name headPats — see `shouldGeneralise`).
//
// Value restriction is split: the *generalisation gate* on `mutableToken` lives
// here (`shouldGeneralise`); the *diagnostic* for a mutable binding whose
// resolved type still has free TyVars at end of analysis lives in Validation —
// by then every use site has had a chance to pin them via unification.

module Unification =

    // Re-exports for external callers (Freeze.fs, Validation.fs, Pipeline.fs)
    let zonk = UnificationEngine.zonk
    let substituteWith = UnificationEngine.substituteWith
    let mkNamedTypeSubst = UnificationEngine.mkNamedTypeSubst
    let instantiateMember = UnificationEngine.instantiateMember

    let private walkModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            inferBindingGroup ctx bindings
        | ModuleElem.Expression e -> infer ctx e |> ignore
        | _ -> ()

    /// Rebuild a type definition's typar scope from the registry entry's
    /// `TypeParams`, so a field type containing `'name` resolves to the same
    /// root the registry already holds.
    let private scopeOfTypeParams (typeParams: EqArray<string * TypeVar>) : Dictionary<string, TypeVar> =
        let d = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        for (n, tv) in typeParams do
            if not (d.ContainsKey n) then
                d.[n] <- tv

        d

    /// Link each placeholder field TyVar (stamped by NameResolution) to its
    /// real translated CST type. Done as a pre-pass so a record's field type
    /// can reference another record declared elsewhere in the same file —
    /// every record name is already in `ctx.Types.Record` by now.
    let private fillRecordFieldTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Record(typeName = TypeName(ident = nameLi); fields = fields) when nameLi.Idents.Length = 1 ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.Types.Record.TryGetValue name with
                    | true, info ->
                        let savedScope = ctx.Resolution.TyparScope
                        let savedStrict = ctx.Resolution.TyparScopeStrict
                        ctx.Resolution.TyparScope <- scopeOfTypeParams info.TypeParams
                        ctx.Resolution.TyparScopeStrict <- true

                        try
                            match info.TyparConstraints with
                            | ValueSome cs -> translateConstraints ctx cs
                            | ValueNone -> ()

                            let n = min info.Fields.Length fields.Length

                            for i = 0 to n - 1 do
                                let (RecordField(ident = id; typ = t)) = fields.[i]
                                let translated = translateType ctx t

                                match info.Fields.[i].Type with
                                | TyVar tv ->
                                    let root = UnionFind.find tv
                                    root.Link <- ValueSome translated
                                | _ -> ()

                                ignore id
                        finally
                            ctx.Resolution.TyparScope <- savedScope
                            ctx.Resolution.TyparScopeStrict <- savedStrict
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()

    /// Same shape as `fillRecordFieldTypes` for union case fields — runs
    /// after every record/union is in the registry so a case's field type can
    /// name another DU declared elsewhere in the same file.
    let private fillUnionFieldTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(typeName = TypeName(ident = nameLi); cases = cases) when nameLi.Idents.Length = 1 ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.Types.Union.TryGetValue name with
                    | true, info ->
                        let savedScope = ctx.Resolution.TyparScope
                        let savedStrict = ctx.Resolution.TyparScopeStrict
                        ctx.Resolution.TyparScope <- scopeOfTypeParams info.TypeParams
                        ctx.Resolution.TyparScopeStrict <- true

                        try
                            match info.TyparConstraints with
                            | ValueSome cs -> translateConstraints ctx cs
                            | ValueNone -> ()

                            // A case is registered iff its head names a case —
                            // mirror `NameResolution.unionCaseName`'s accept
                            // set so this CST walk stays index-aligned with the
                            // registry's `Cases` array (named cases only).
                            let headNames (head: IdentOrOp<SyntaxToken>) =
                                match head with
                                | IdentOrOp.Ident _
                                | IdentOrOp.ParenOp(opName = OpName.NilOp _)
                                | IdentOrOp.ParenOp(opName = OpName.SymbolicOp _) -> true
                                | _ -> false

                            let caseHeadFields data =
                                match data with
                                | UnionTypeCaseData.Nullary(name = h) -> struct (headNames h, [])
                                | UnionTypeCaseData.GadtNullary(name = h) -> struct (headNames h, [])
                                | UnionTypeCaseData.Nary(name = h; fields = fs) ->
                                    let tys =
                                        [
                                            for f in fs ->
                                                match f with
                                                | UnionTypeField.Unnamed(typ = t) -> t
                                                | UnionTypeField.Named(typ = t) -> t
                                        ]

                                    struct (headNames h, tys)
                                | UnionTypeCaseData.GadtNary(
                                    name = h; sign = UncurriedSig(args = ArgsSpec(args = specs))) ->
                                    let tys = [ for ArgSpec(typ = t) in specs -> t ]
                                    struct (headNames h, tys)

                            let mutable infoIdx = 0

                            for UnionTypeCase(data = data) in cases do
                                let struct (isRegistered, fieldTypes) = caseHeadFields data

                                if isRegistered && infoIdx < info.Cases.Length then
                                    let caseInfo = info.Cases.[infoIdx]
                                    infoIdx <- infoIdx + 1

                                    let fieldTypes = List.toArray fieldTypes
                                    let n = min caseInfo.Fields.Length fieldTypes.Length

                                    for i = 0 to n - 1 do
                                        let translated = translateType ctx fieldTypes.[i]

                                        match caseInfo.Fields.[i] with
                                        | TyVar tv ->
                                            let root = UnionFind.find tv
                                            root.Link <- ValueSome translated
                                        | _ -> ()
                        finally
                            ctx.Resolution.TyparScope <- savedScope
                            ctx.Resolution.TyparScopeStrict <- savedStrict
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()

    /// Link each ctor-param placeholder TyVar to its declared type.
    /// Un-annotated arguments leave the placeholder free so a use site can
    /// pin it via argument-type unification in `inferNew` / `inferApp`.
    let private fillClassCtorParamTypes
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (pcOpt: PrimaryConstrArgs<SyntaxToken> voption)
        : unit =
        match pcOpt with
        | ValueNone -> ()
        | ValueSome(PrimaryConstrArgs(pat = ValueNone)) -> ()
        | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) ->
            let idx = ref 0

            let rec walk (p: Pat<SyntaxToken>) =
                match p with
                | Pat.NamedSimple _ -> incr idx
                | Pat.Typed(pat = inner; typ = t) ->
                    let i = !idx
                    incr idx

                    if i < info.CtorParams.Length then
                        let translated = translateType ctx t

                        match info.CtorParams.[i].Type with
                        | TyVar tv ->
                            let root = UnionFind.find tv
                            root.Link <- ValueSome translated
                        | _ -> ()

                    ignore inner
                | Pat.EnclosedBlock(pat = inner) -> walk inner
                | Pat.Tuple(patterns = pats) ->
                    for sub in pats do
                        walk sub
                | _ -> ()

            walk p

    /// Fold a curried member signature into a `TyFun` chain (a multi-arg
    /// group `a * b` is a tuple parameter), under the caller's typar scope.
    /// Used to fill abstract member signatures, which have no body to infer.
    let private curriedSigToSemType (ctx: PassContext) (CurriedSig(args = args; returnType = ret)) : SemType =
        let groupTy (ArgsSpec(args = specs)) =
            match List.ofSeq specs with
            | [ ArgSpec(typ = t) ] -> translateType ctx t
            | many -> TyTuple(EqArray.ofSeq (seq { for ArgSpec(typ = t) in many -> translateType ctx t }))

        let retTy = translateType ctx ret
        List.foldBack (fun struct (g, _arrow) acc -> TyFun(groupTy g, acc)) (List.ofSeq args) retTy

    /// Parameters for `fillTypeMembers`: a registry-driven walk over a
    /// class or union's member bodies. `MkSelfType` produces the `this`
    /// type-tag (`TyClass` / `TyUnion`); `PrelinkExtras` runs after the
    /// typar scope is set but before `this` is bound (used to fill class
    /// ctor-param placeholders); `AllowAbstractSig` opts in to the
    /// `AbstractSignature` arm (class only — unions have no abstract members).
    [<NoEquality; NoComparison>]
    type private TypeMembersFill =
        {
            TypeParams: EqArray<string * TypeVar>
            Members: TypeMemberInfo[]
            ThisKey: NodeKey
            MkSelfType: EqArray<SemType> -> SemType
            PrelinkExtras: unit -> unit
            Elements: TypeDefnElements<SyntaxToken>
            AllowAbstractSig: bool
        }

    /// Walk every method / property / auto-property body under a typar
    /// scope seeded from `TypeParams` plus a `this` binding linked to
    /// `MkSelfType`. Placeholder member TyVars are pre-populated into
    /// `ctx.Bindings.TypeVar` so `inferBinding`'s `tvOf` reuses them and its
    /// final `unify patTy rhsTy` links the placeholder to the inferred
    /// member type. AutoProperty has no `Binding`, so its placeholder
    /// is linked manually. Abstract signatures (no body to infer)
    /// translate directly when `AllowAbstractSig` is set.
    let private fillTypeMembers (ctx: PassContext) (fc: TypeMembersFill) : unit =
        let savedScope = ctx.Resolution.TyparScope
        let savedStrict = ctx.Resolution.TyparScopeStrict
        ctx.Resolution.TyparScope <- scopeOfTypeParams fc.TypeParams
        ctx.Resolution.TyparScopeStrict <- true

        try
            fc.PrelinkExtras()

            // `this`: fresh TyVar pre-linked to the self-type over the
            // declaration's prototype typars, so a generic member body
            // mentioning `'a` shares identity with them.
            let thisTv = TypeVar()
            thisTv.Level <- ctx.CurrentLevel
            let selfArgs = EqArray.ofSeq (seq { for (_, ptv) in fc.TypeParams -> TyVar ptv })
            thisTv.Link <- ValueSome(fc.MkSelfType selfArgs)
            ctx.Bindings.TypeVar.Set(fc.ThisKey, thisTv)

            // Static member bodies never see `this` / ctor params
            // (NameResolution gives them an empty binding scope);
            // IsStatic discriminates downstream.
            for el in fc.Elements do
                match el with
                | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                    match d with
                    | MethodOrPropDefn.Method(defn = b)
                    | MethodOrPropDefn.Property(defn = b) ->
                        let mNameOpt =
                            let rec walkP (p: Pat<SyntaxToken>) =
                                match p with
                                | Pat.NamedSimple id -> ValueSome id
                                | Pat.EnclosedBlock(pat = inner)
                                | Pat.Typed(pat = inner) -> walkP inner
                                | _ -> ValueNone

                            walkP b.headPat

                        match mNameOpt with
                        | ValueSome mTok ->
                            let mKey = NodeKey.ofToken mTok NodeKind.PatIdent
                            let mInfoOpt = fc.Members |> Array.tryFind (fun m -> m.DeclKey = mKey)

                            match mInfoOpt with
                            | Some mInfo ->
                                match mInfo.Type with
                                | TyVar tv -> ctx.Bindings.TypeVar.Set(mKey, tv)
                                | _ -> ()
                            | None -> ()

                            // Seed the binding's own `<'C, …>` typars (B-12) with
                            // their registration prototypes so `inferBinding`
                            // reuses them in its fresh binding scope. The signature
                            // it infers then shares roots with the member's
                            // `MethodTypeParams` — the same roots Freeze surfaces
                            // and codegen installs as the ambient `!!i` set.
                            let savedSeed = ctx.Resolution.BindingTyparSeed

                            match mInfoOpt with
                            | Some mInfo when not mInfo.MethodTypeParams.IsEmpty ->
                                let seed = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

                                for (n, ptv) in mInfo.MethodTypeParams do
                                    seed.[n] <- ptv

                                ctx.Resolution.BindingTyparSeed <- ValueSome seed
                            | _ -> ()

                            enterLevel ctx

                            try
                                inferBinding ctx b
                            finally
                                exitLevel ctx
                                ctx.Resolution.BindingTyparSeed <- savedSeed
                        | ValueNone -> ()
                    | MethodOrPropDefn.AutoProperty(ident = id; expr = e; returnType = rt) ->
                        enterLevel ctx

                        try
                            let bodyTy = infer ctx e

                            let resultTy =
                                match rt with
                                | ValueSome(ReturnType(typ = t)) ->
                                    let t' = translateType ctx t
                                    unify ctx (CstKeys.ofExpr e) bodyTy t'
                                    t'
                                | ValueNone -> bodyTy

                            let mKey = NodeKey.ofToken id NodeKind.PatIdent

                            match fc.Members |> Array.tryFind (fun m -> m.DeclKey = mKey) with
                            | Some mInfo ->
                                match mInfo.Type with
                                | TyVar tv -> (UnionFind.find tv).Link <- ValueSome resultTy
                                | _ -> ()
                            | None -> ()
                        finally
                            exitLevel ctx
                    | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(ident = idOrOp; sign = csig)) when
                        fc.AllowAbstractSig
                        ->
                        // No body to infer — translate the signature
                        // directly and link the placeholder.
                        let mTokOpt =
                            match idOrOp with
                            | IdentOrOp.Ident t -> ValueSome t
                            | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ValueSome op
                            | _ -> ValueNone

                        match mTokOpt with
                        | ValueSome mTok ->
                            let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                            match fc.Members |> Array.tryFind (fun mm -> mm.DeclKey = mKey) with
                            | Some mInfo ->
                                match mInfo.Type with
                                | TyVar tv ->
                                    let root = UnionFind.find tv

                                    // Extend the scope with the method's own
                                    // `<'C, …>` typars so they resolve to their
                                    // prototype TyVars (not diagnosed as free).
                                    let savedMScope = ctx.Resolution.TyparScope

                                    if not mInfo.MethodTypeParams.IsEmpty then
                                        let extended =
                                            Dictionary<string, TypeVar>(savedMScope, System.StringComparer.Ordinal)

                                        for (n, ptv) in mInfo.MethodTypeParams do
                                            extended.[n] <- ptv

                                        ctx.Resolution.TyparScope <- extended

                                    try
                                        root.Link <- ValueSome(curriedSigToSemType ctx csig)
                                    finally
                                        ctx.Resolution.TyparScope <- savedMScope
                                | _ -> ()
                            | None -> ()
                        | ValueNone -> ()
                    | _ -> ()
                | _ -> ()
        finally
            ctx.Resolution.TyparScope <- savedScope
            ctx.Resolution.TyparScopeStrict <- savedStrict

    /// Link each secondary-ctor param placeholder TyVar to its declared-type
    /// annotation. Index walk mirrors `MemberRegistration.ctorParamsOfPat`'s
    /// param-collection order; un-annotated params are left free so the chain-call
    /// unification pins them. Parallel to `fillClassCtorParamTypes` but driven by a
    /// raw `Pat` (the `new(...)` pattern) rather than `PrimaryConstrArgs`.
    let private fillSecondaryCtorParamTypes
        (ctx: PassContext)
        (parms: ClassCtorParamInfo[])
        (p: Pat<SyntaxToken>)
        : unit =
        let idx = ref 0

        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.EmptyBlock _ -> ()
            | Pat.NamedSimple _ -> incr idx
            | Pat.Typed(pat = Pat.NamedSimple _; typ = t) ->
                let i = !idx
                incr idx

                if i < parms.Length then
                    let translated = translateType ctx t

                    match parms.[i].Type with
                    | TyVar tv -> (UnionFind.find tv).Link <- ValueSome translated
                    | _ -> ()
            | Pat.EnclosedBlock(pat = inner) -> walk inner
            | Pat.Tuple(patterns = pats) ->
                for sub in pats do
                    walk sub
            | _ -> ()

        walk p

    /// Type a secondary ctor body (`new(args) = …; SelfType(primaryArgs)`).
    /// `expected` is the primary ctor's tupled parameter type (the chain-call
    /// target). The `let`-preamble binders are inferred in order; the final chain
    /// call's arguments are unified against `expected`. The chain call's function
    /// position (the self-type name) is never inferred — only its arguments are.
    let rec private inferSecondaryCtorBody
        (ctx: PassContext)
        (expected: SemType)
        (ace: AdditionalConstrExpr<SyntaxToken>)
        : unit =
        match ace with
        | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
            inferBinding ctx b
            inferSecondaryCtorBody ctx expected body
        | AdditionalConstrExpr.SequenceAfter(stmt = s; rest = rest) ->
            infer ctx s |> ignore
            inferSecondaryCtorBody ctx expected rest
        | AdditionalConstrExpr.SequenceBefore(before = before; expr = e) ->
            inferSecondaryCtorBody ctx expected before
            infer ctx e |> ignore
        | AdditionalConstrExpr.Conditional(cond = c; thenBranch = t; elseBranch = el) ->
            infer ctx c |> ignore
            inferSecondaryCtorBody ctx expected t
            inferSecondaryCtorBody ctx expected el
        | AdditionalConstrExpr.Init initExpr ->
            match initExpr with
            | AdditionalConstrInitExpr.Expression e ->
                match e with
                | Expr.HighPrecedenceApp(argExpr = argExpr) ->
                    let argTy = infer ctx argExpr
                    unify ctx (CstKeys.ofExpr argExpr) argTy expected
                | Expr.App(argExprs = argExprs) ->
                    let argTys = [ for a in argExprs -> infer ctx a ]
                    unify ctx (CstKeys.ofExpr e) (tupleOrSingle argTys) expected
                | _ -> infer ctx e |> ignore
            | AdditionalConstrInitExpr.Delegated(expr = e) -> infer ctx e |> ignore
            | AdditionalConstrInitExpr.Explicit(initializers = inits) ->
                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

    /// Type every secondary ctor (B-11) of a class under its typar scope: link
    /// param annotations, seed param binding-site TyVars, then infer each body.
    let private fillSecondaryCtors (ctx: PassContext) (info: ClassTypeInfo) : unit =
        if info.SecondaryCtors.Length > 0 then
            let savedScope = ctx.Resolution.TyparScope
            let savedStrict = ctx.Resolution.TyparScopeStrict
            ctx.Resolution.TyparScope <- scopeOfTypeParams info.TypeParams
            ctx.Resolution.TyparScopeStrict <- true

            try
                let expected =
                    info.CtorParams |> Array.map (fun p -> p.Type) |> Array.toList |> tupleOrSingle

                for sc in info.SecondaryCtors do
                    fillSecondaryCtorParamTypes ctx sc.Params sc.ParamPat

                    for p in sc.Params do
                        match p.Type with
                        | TyVar tv -> ctx.Bindings.TypeVar.Set(p.DeclKey, tv)
                        | _ -> ()

                    enterLevel ctx

                    try
                        inferSecondaryCtorBody ctx expected sc.Body
                    finally
                        exitLevel ctx
            finally
                ctx.Resolution.TyparScope <- savedScope
                ctx.Resolution.TyparScopeStrict <- savedStrict

    /// Type the `inherit Base(args)` invocation (Step 2.2) against the parent's
    /// primary-ctor signature, under the derived class's typar scope (already set
    /// by `fillTypeMembers` before `PrelinkExtras` runs). The parent's ctor-param
    /// types are substituted with the args `inherit Base<…>` supplied — recovered
    /// from `info.BaseType`'s already-translated `TyClass` args. Errors attach at
    /// the base-ctor-args expression and don't cascade into member-body inference.
    /// No-op for parent-less classes and for parents not in `ctx.Types.Class`
    /// (`registerInheritedSlots` already diagnosed those).
    let private fillBaseCtorCall (ctx: PassContext) (info: ClassTypeInfo) : unit =
        match info.BaseType, info.BaseCtorArgs with
        | ValueSome(TyClass(baseName, baseArgs)), ValueSome argExpr ->
            match ctx.Types.Class.TryGetValue baseName with
            | true, baseInfo ->
                let subst = mkNamedTypeSubst baseInfo.TypeParams baseArgs

                let expected =
                    baseInfo.CtorParams
                    |> Array.map (fun p -> substituteWith subst p.Type)
                    |> Array.toList
                    |> tupleOrSingle

                enterLevel ctx

                try
                    let argTy = infer ctx argExpr
                    unify ctx (CstKeys.ofExpr argExpr) argTy expected
                finally
                    exitLevel ctx
            | false, _ -> ()
        | _ -> ()

    /// Mint the `base` TyVar (Step 2.2) pre-linked to the parent's instantiated
    /// `TyClass` and seed `ctx.Bindings.TypeVar` at `info.BaseKey`, mirroring the
    /// `this` mint in `fillTypeMembers`. `info.BaseType` is already substituted
    /// under the derived class's typar scope by `registerInheritedSlots`, so it
    /// links directly. No-op for parent-less classes.
    let private mintBaseTyVar (ctx: PassContext) (info: ClassTypeInfo) : unit =
        match info.BaseType with
        | ValueSome parentTy ->
            let baseTv = TypeVar()
            baseTv.Level <- ctx.CurrentLevel
            baseTv.Link <- ValueSome parentTy
            ctx.Bindings.TypeVar.Set(info.BaseKey, baseTv)
        | ValueNone -> ()

    /// Map the only modelled reference supertype `System.Object` to the front-end
    /// primitive `obj` so a user member annotated `obj` conforms to an external
    /// interface signature that surfaces `System.Object` (the two are
    /// interchangeable — `InferOverload.isObjectTy`). The metadata layer renders
    /// `System.Object` as `TyClass("System.Object", [])` (it isn't in
    /// `IntrinsicRepr.defaults`), while `translateType` renders the user's `obj` as
    /// `TyConst "obj"`; without this bridge `IComparable.CompareTo(obj)` would fail
    /// to unify. Recurses structurally; every other nominal is left untouched.
    let rec private normalizeObj (t: SemType) : SemType =
        match t with
        | TyClass("System.Object", args) when args.IsEmpty -> TyConst "obj"
        | TyClass(n, args) -> TyClass(n, EqArray.map normalizeObj args)
        | TyFun(a, r) -> TyFun(normalizeObj a, normalizeObj r)
        | TyTuple xs -> TyTuple(EqArray.map normalizeObj xs)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map normalizeObj args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map normalizeObj args)
        | other -> other

    /// Type-check the member bodies of one resolved `interface IFace with member …`
    /// block against the interface's external signatures (B-2,
    /// vesper-set-sprint-phase-5 §5.2). For each impl member, unify its
    /// already-inferred signature with the matching `ExternalMember` looked up by
    /// name on `iface` (`TyClass(ifaceName, ifaceArgs)`), substituting the impl's
    /// interface type-args so a generic `IEnumerable<'T>::GetEnumerator() :
    /// IEnumerator<'T>` binds the class typar through. The metadata member walk is
    /// `DeclaredOnly`, so a base interface's members (e.g. `IEnumerable<'T>`'s
    /// inherited non-generic `IEnumerable::GetEnumerator`) live in their *own*
    /// `interface …` block — each block therefore resolves its own `GetEnumerator`
    /// overload unambiguously, which is the §5.2 multiple-`GetEnumerator`
    /// disambiguation. Every declared interface member is required: a missing one
    /// diagnoses at the interface name token.
    let private checkInterfaceConformance (ctx: PassContext) (impl: ClassInterfaceImplInfo) : unit =
        match impl.Resolved with
        | ValueSome(TyClass(ifaceName, ifaceArgs)) ->
            match ctx.Provider.TryLookupType ifaceName with
            | ValueSome(ExternalTypeShape.Class shape) ->
                let argArr = ifaceArgs.AsSpan().ToArray()

                // Interfaces declare no constructors; the `.ctor` guard is
                // belt-and-suspenders against a provider that surfaces one.
                let required = shape.Members |> Array.filter (fun em -> em.Name <> ".ctor")

                for mInfo in impl.Members do
                    match required |> Array.tryFind (fun em -> em.Name = mInfo.Name) with
                    | Some em ->
                        let expected = normalizeObj (em.BuildSignature argArr)
                        unify ctx mInfo.DeclKey mInfo.Type expected
                    | None ->
                        ctx.Error(
                            mInfo.DeclKey,
                            sprintf "Interface '%s' does not define a member '%s'" ifaceName mInfo.Name
                        )

                for em in required do
                    if not (impl.Members |> Array.exists (fun m -> m.Name = em.Name)) then
                        ctx.Error(
                            impl.DeclKey,
                            sprintf "No implementation given for '%s' required by interface '%s'" em.Name ifaceName
                        )
            | _ -> ()
        | _ -> ()

    /// Resolve + verify each `interface IFace with member …` block (B-2,
    /// vesper-set-sprint-phase-5 §5.1) on a class, then type its member bodies.
    /// The interface type resolves under the class's typar scope (so a generic
    /// interface arg like `IEnumerable<'T>` binds to the class's typar); it must
    /// map to a type the provider reports as an interface, else a diagnostic fires
    /// and `Resolved` stays `ValueNone`. Member bodies type-check through
    /// `fillTypeMembers` exactly like the class's own members — `this` re-binds to
    /// the class instance via `info.ThisKey`. Once typed, each body's signature is
    /// conformance-checked against the interface (§5.2, `checkInterfaceConformance`).
    /// Runs after the class's own `fillTypeMembers` / `fillSecondaryCtors`, so ctor
    /// params and the base call are already seeded and `PrelinkExtras` is a no-op here.
    let private fillInterfaceImpls (ctx: PassContext) (info: ClassTypeInfo) : unit =
        for impl in info.InterfaceImpls do
            let resolved =
                let savedScope = ctx.Resolution.TyparScope
                let savedStrict = ctx.Resolution.TyparScopeStrict
                ctx.Resolution.TyparScope <- scopeOfTypeParams info.TypeParams
                ctx.Resolution.TyparScopeStrict <- true

                try
                    translateType ctx impl.InterfaceCst
                finally
                    ctx.Resolution.TyparScope <- savedScope
                    ctx.Resolution.TyparScopeStrict <- savedStrict

            let isInterface =
                match resolved with
                | TyClass(ifaceName, _) ->
                    match ctx.Provider.TryLookupType ifaceName with
                    | ValueSome(ExternalTypeShape.Class shape) -> shape.IsInterface
                    | _ -> false
                | _ -> false

            if isInterface then
                impl.Resolved <- ValueSome resolved
            else
                let shown =
                    match zonk resolved with
                    | TyClass(n, _) -> n
                    | other -> sprintf "%A" other

                ctx.Error(impl.DeclKey, sprintf "Type '%s' is not an interface" shown)

            fillTypeMembers
                ctx
                {
                    TypeParams = info.TypeParams
                    Members = impl.Members
                    ThisKey = info.ThisKey
                    MkSelfType = fun args -> TyClass(info.Name, args)
                    PrelinkExtras = ignore
                    Elements = impl.Elements
                    AllowAbstractSig = false
                }

            // §5.2: now the bodies are typed, conform each member's signature to
            // the interface's external signature. Skipped when resolution failed
            // (`Resolved = ValueNone`) — that diagnostic already fired.
            checkInterfaceConformance ctx impl

    let private fillClassMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        let common (td: TypeDefn<SyntaxToken>) =
            match TypeDefnPatterns.tryClassLikeDecl td with
            | ValueSome d ->
                let (TypeName(ident = nameLi)) = d.TypeName

                if nameLi.Idents.Length = 1 then
                    ValueSome(ctx.NameOf nameLi.Idents.[0], d.PrimaryConstr, d.Body)
                else
                    ValueNone
            | ValueNone -> ValueNone

        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match common td with
                | ValueSome(name, pc, body) ->
                    match ctx.Types.Class.TryGetValue name with
                    | true, info ->
                        let prelinkExtras () =
                            // Fill ctor-param placeholders under the class's
                            // typar scope, then seed `ctx.Bindings.TypeVar` so
                            // `inferIdent` lookups against the param binding
                            // sites return these.
                            fillClassCtorParamTypes ctx info pc

                            for p in info.CtorParams do
                                match p.Type with
                                | TyVar tv -> ctx.Bindings.TypeVar.Set(p.DeclKey, tv)
                                | _ -> ()

                            // Inheritance (Step 2.2): type the base-ctor call and
                            // bring `base` into scope before any member body walks.
                            // Both no-op for parent-less classes. Runs after the
                            // ctor-param binding sites are seeded so an `inherit
                            // Base(p)` arg referencing a derived ctor param `p`
                            // resolves to its declared type (not a fresh TyVar).
                            fillBaseCtorCall ctx info
                            mintBaseTyVar ctx info

                            // `static let` initialisers (B-10): infer each in
                            // declaration order (an earlier static-let binder is
                            // already seeded, so a later initialiser can reference
                            // it), link the placeholder TyVar to the inferred type,
                            // and seed `ctx.Bindings.TypeVar` so a `static let`-bound
                            // name reference in a member body types through it.
                            for sl in info.StaticLets do
                                match sl.Type with
                                | TyVar tv -> ctx.Bindings.TypeVar.Set(sl.DeclKey, tv)
                                | _ -> ()

                                enterLevel ctx

                                try
                                    let initTy = infer ctx sl.Init

                                    match sl.Type with
                                    | TyVar tv -> (UnionFind.find tv).Link <- ValueSome initTy
                                    | _ -> ()
                                finally
                                    exitLevel ctx

                        fillTypeMembers
                            ctx
                            {
                                TypeParams = info.TypeParams
                                Members = info.Members
                                ThisKey = info.ThisKey
                                MkSelfType = fun args -> TyClass(info.Name, args)
                                PrelinkExtras = prelinkExtras
                                Elements = body.elements
                                AllowAbstractSig = true
                            }

                        fillSecondaryCtors ctx info
                        fillInterfaceImpls ctx info
                    | false, _ -> ()
                | ValueNone -> ()
        | _ -> ()

    let private fillUnionMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(
                    typeName = TypeName(ident = nameLi); extensions = ValueSome(TypeExtensionElements(elements = elems))) when
                    nameLi.Idents.Length = 1
                    ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.Types.Union.TryGetValue name with
                    | true, info when not (Array.isEmpty info.Members) ->
                        fillTypeMembers
                            ctx
                            {
                                TypeParams = info.TypeParams
                                Members = info.Members
                                ThisKey = info.ThisKey
                                MkSelfType = fun args -> TyUnion(info.Name, args)
                                PrelinkExtras = ignore
                                Elements = elems
                                AllowAbstractSig = false
                            }
                    | _ -> ()
                | _ -> ()
        | _ -> ()

    /// `forceFill` recurses through `translateType`, so dependencies fill
    /// DFS-style regardless of declaration order. Runs before record / union
    /// field fill so a field or case-arg referencing an abbreviation by name
    /// sees the expanded type.
    let private fillAbbreviationBodies (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) : unit =
        for m in elems do
            match m with
            | ModuleElem.Type defs ->
                for td in defs do
                    match td with
                    | TypeDefn.Abbrev(typeName = TypeName(ident = nameLi)) when nameLi.Idents.Length = 1 ->
                        let name = ctx.NameOf nameLi.Idents.[0]

                        match ctx.Types.Abbreviation.TryGetValue name with
                        | true, info -> forceFill ctx info
                        | false, _ -> ()
                    | _ -> ()
            | _ -> ()

    let private walkElems (ctx: PassContext) (pairs: (ModuleElem<SyntaxToken> * OpenScope) list) =
        let elems = ImmutableArray.CreateRange(pairs |> List.map fst)
        fillAbbreviationBodies ctx elems

        // Set `ctx.Resolution.OpenScope` per element so the provider-probe sites
        // (`inferIdent`, `tryExternalTypeReceiver`) resolve short external names
        // against the `open`s in scope at that element (symbol-resolution-handoff.md, open-resolution).
        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillRecordFieldTypes ctx m

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillUnionFieldTypes ctx m

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillClassMembers ctx m

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillUnionMembers ctx m

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            walkModuleElem ctx m

    /// Resolve the bare-program list literals left flexible by `listLiteralTy`
    /// (R3), after the whole file is walked so every consumer has had its say:
    ///   - still free (no consumer drove it, e.g. `printfn "%A" [1;2;3]`) → link to
    ///     FSharp.Core's `list`, its element carried through;
    ///   - flipped to a list-like type (`List.fold`'s `Vesper.Collections.List`
    ///     parameter) → reconcile the literal's element with the driven one.
    let private resolveListLiterals (ctx: PassContext) : unit =
        let key = NodeKey.ofSource 0 NodeKind.Unknown

        for (lv, elemTy) in ctx.ListLiterals do
            let root = UnionFind.find lv

            match root.Link with
            | ValueNone ->
                unify ctx key (TyVar root) (TyRecord("Microsoft.FSharp.Collections.list", EqArray.singleton elemTy))
            | ValueSome target ->
                match zonk target with
                | TyRecord(_, args) when args.Length = 1 -> unify ctx key args.[0] elemTy
                | TyUnion(_, args) when args.Length = 1 -> unify ctx key args.[0] elemTy
                | _ -> ()

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        // Recompute the same per-element `OpenScope` NameResolution did, from the
        // same stable ambient seed (`AmbientOpenScope`, not the per-element
        // `OpenScope` the walk mutates — symbol-resolution-handoff.md, open-resolution).
        walkElems ctx (CstWalk.walkModuleTree ctx.NameOf ctx.Resolution.AmbientOpenScope file)
        resolveListLiterals ctx
