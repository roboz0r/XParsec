namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate
open UnificationInfer
open UnificationInferForwardSchemes

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
    /// Walk a class receiver's `inherit` chain for a non-static member, yielding
    /// the declaring ancestor's instantiated type + the member's type. `FreezeExpr`
    /// reuses this (the declaring type) so the inherited-member read isn't a second
    /// chain walk that must stay in sync with inference's.
    let tryClassChainMemberDecl = UnificationEngine.tryClassChainMemberDecl

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
                | TypeDefn.Union(typeName = (TypeName(ident = nameLi) as tn); cases = cases) when
                    nameLi.Idents.Length = 1
                    ->
                    let name = ctx.NameOf nameLi.Idents.[0]
                    // Resolve by (name, arity) so an arity-overloaded union
                    // (`Choice\`2`…`Choice\`7`) fills the *right* case fields.
                    let arity =
                        NameResolutionTypeRegistration.typarNamesOfTypeName ctx tn |> List.length

                    match TypeRegistry.tryUnion ctx.Types name arity with
                    | ValueSome info ->
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
                    | ValueNone -> ()
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
                | Pat.EnclosedBlock(pat = inner)
                | Pat.Attributed(pat = inner) -> walk inner
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
            // An interface-impl member's signature is *fixed* by the interface
            // slot (`checkInterfaceConformance` unifies it after the body), so it
            // must never acquire method generic parameters — the slot is
            // non-generic, and a generalised member would emit as
            // `Equals\`1(…)`, whose generic arity (1) no longer matches the
            // `IStructuralEquatable.Equals` slot (arity 0), tripping a CLR
            // "Method 'Equals' … does not have an implementation" type-load
            // failure. `false` for these; `true` for a class's own members,
            // which generalise body-inferred typars per `generaliseMemberTypars`.
            Generalise: bool
        }

    /// Generalise a member's *body-inferred* free typars into its
    /// `MethodTypeParams`. A method with unannotated params (`member s.Fold f z =
    /// SetTree.fold (fun x z -> f z x) z s.Tree`) whose body introduces a fresh
    /// typar (`'State`) that no signature annotation names gets no registered
    /// method typar for it (`implicitMemberTypars` scans annotations only). If the
    /// member is never *called* in this assembly — a library API — that typar
    /// never grounds via a use site and leaks as `?ungrounded-operator` at codegen
    /// (a member-body closure capturing the `'State`-typed `f` froze with it). F#
    /// generalises such typars as method generic parameters; mirror that here:
    /// after the body is inferred, collect the still-free roots in the member's
    /// signature (`Level > outerLevel && Link.IsNone`, the same gate `generalise`
    /// uses) that aren't already a registered method typar, and append them.
    /// `Elaborate`/`Freeze` then surface them as `TyTypar(Method, i)` +
    /// `GenericParam` rows by their position in `MethodTypeParams`, and a member-
    /// body closure inherits them as its own method typars.
    /// Methods only — a property can't carry method typars (mirrors registration).
    let private generaliseMemberTypars
        (ctx: PassContext)
        (outerLevel: int)
        (classTypars: EqArray<string * TypeVar>)
        (mInfo: TypeMemberInfo)
        : unit =
        match mInfo.Type with
        | TyVar tv ->
            let memberTy = zonk (TyVar tv)
            // Resolve defaults first (as `generalise` does) so a defaulted typar
            // links its source and the walk below skips it — `member m.Add a b = a + b`
            // grounds to `int` rather than quantifying the arithmetic typar.
            UnificationInferGeneralize.applyDefaults memberTy outerLevel

            // Typars already accounted for: the enclosing class typars (a `'T` is
            // a declaring-axis param, not a method one) and the member's already-
            // registered method typars (explicit `<'C>` / annotation-implicit). The
            // class typar roots are zonked *here* (not snapshotted at type entry):
            // a class typar's union-find root can move while a member body types
            // (`Holder<'T>(v)` unifies the return through a fresh instantiation), so
            // a stale snapshot would miss it and the `'T` in the member signature
            // would be wrongly generalised into a (dangling) method typar.
            let accounted = HashSet<TypeVar>(HashIdentity.Reference)

            for (_, ptv) in classTypars do
                match zonk (TyVar ptv) with
                | TyVar r -> accounted.Add(UnionFind.find r) |> ignore
                | _ -> ()

            for (_, ptv) in mInfo.MethodTypeParams do
                match zonk (TyVar ptv) with
                | TyVar r -> accounted.Add(UnionFind.find r) |> ignore
                | _ -> ()

            let extra = ResizeArray<string * TypeVar>()
            let seen = HashSet<TypeVar>(HashIdentity.Reference)

            // A member is typed at the module level (level 0), so its body-inferred
            // typars live at the *same* level as the class typars — the level gate
            // `generalise` uses for nested lets can't separate them. Instead exclude
            // the class typars by identity and generalise every other still-free
            // root in the member's signature (this *is* generalisation — F# makes
            // each a method generic parameter). Shares `generalise`'s structural
            // walk; only the per-root predicate differs (identity-exclusion here vs.
            // the level gate there).
            zonk memberTy
            |> UnificationInferGeneralize.iterTypeVarRoots (fun root ->
                if root.Link.IsNone && not (accounted.Contains root) && seen.Add root then
                    // A synthetic metadata typar name; method generic params are
                    // method-scoped, so this can't collide with the class typars.
                    extra.Add(sprintf "M%d" extra.Count, root)
            )

            if extra.Count > 0 then
                mInfo.MethodTypeParams <-
                    EqArray.ofSeq (
                        seq {
                            yield! EqArray.toList mInfo.MethodTypeParams
                            yield! extra
                        }
                    )
        | _ -> ()

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
        let savedEnclosing = ctx.Resolution.EnclosingTypars
        let classScope = scopeOfTypeParams fc.TypeParams
        ctx.Resolution.TyparScope <- classScope
        ctx.Resolution.TyparScopeStrict <- true
        // Keep the class typars in scope across each member body's `inferBinding`
        // (which mints a fresh scope and would otherwise drop them) — G11.
        ctx.Resolution.EnclosingTypars <- ValueSome classScope

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
                        // The member's body-inference key — `CstKeys.ofPat` of the
                        // *leaf* head pattern, the exact key `inferBinding` links the
                        // inferred signature under, and the key registration stamped
                        // as `mInfo.DeclKey`. A `Pat.Op` head keys on `(lParen, PatOp)`
                        // (not `(opToken, PatIdent)`), so the operator member's
                        // `mInfo.Type` placeholder actually receives the body type.
                        let mKeyOpt =
                            let rec walkP (p: Pat<SyntaxToken>) =
                                match p with
                                | Pat.NamedSimple _
                                | Pat.Op _ -> ValueSome(CstKeys.ofPat p)
                                | Pat.EnclosedBlock(pat = inner)
                                | Pat.Typed(pat = inner)
                                | Pat.Attributed(pat = inner) -> walkP inner
                                | _ -> ValueNone

                            walkP b.headPat

                        match mKeyOpt with
                        | ValueSome mKey ->
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
                            let savedMemberEnclosing = ctx.Resolution.EnclosingTypars

                            match mInfoOpt with
                            | Some mInfo when not mInfo.MethodTypeParams.IsEmpty ->
                                let seed = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

                                for (n, ptv) in mInfo.MethodTypeParams do
                                    seed.[n] <- ptv

                                ctx.Resolution.BindingTyparSeed <- ValueSome seed

                                // Keep the member's own typars (explicit `<'C>` +
                                // implicit signature typars, G12) in `EnclosingTypars`
                                // for the body walk, alongside the class typars — so a
                                // nested `let comparer = Comparer<'U>.Default` in the
                                // body resolves `'U` rather than diagnosing it free.
                                // `inferBinding` clears `BindingTyparSeed` after the
                                // member's own binding, so without this the member
                                // typars would vanish in nested scopes (mirror G11's
                                // class-typar persistence).
                                let memberEnclosing =
                                    Dictionary<string, TypeVar>(classScope, System.StringComparer.Ordinal)

                                for (n, ptv) in mInfo.MethodTypeParams do
                                    memberEnclosing.[n] <- ptv

                                ctx.Resolution.EnclosingTypars <- ValueSome memberEnclosing
                            | _ -> ()

                            let outerLevel = ctx.CurrentLevel
                            enterLevel ctx

                            try
                                inferBinding ctx b
                            finally
                                exitLevel ctx
                                ctx.Resolution.BindingTyparSeed <- savedSeed
                                ctx.Resolution.EnclosingTypars <- savedMemberEnclosing

                            // Generalise any body-inferred free typar into the
                            // member's own method typars (the `Set.Fold` leak): a
                            // method whose unannotated param type carries a fresh
                            // typar no annotation named, never grounded by a call.
                            match mInfoOpt with
                            | Some mInfo when
                                fc.Generalise
                                && mInfo.Kind = ClassMemberKind.Method
                                // An `override` conforms to a base virtual slot
                                // (`checkObjectOverrideConformance` pins its
                                // signature), so it is never generic — generalising
                                // an unannotated param (`override _.Equals that`)
                                // into a method typar would make it `Equals\`1`,
                                // which no longer matches the `Object.Equals` slot.
                                && not mInfo.IsOverride
                                ->
                                generaliseMemberTypars ctx outerLevel fc.TypeParams mInfo
                            | _ -> ()
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
            ctx.Resolution.EnclosingTypars <- savedEnclosing

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
        (fieldTypes: Map<string, SemType>)
        (ace: AdditionalConstrExpr<SyntaxToken>)
        : unit =
        match ace with
        | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
            inferBinding ctx b
            inferSecondaryCtorBody ctx expected fieldTypes body
        | AdditionalConstrExpr.SequenceAfter(stmt = s; rest = rest) ->
            infer ctx s |> ignore
            inferSecondaryCtorBody ctx expected fieldTypes rest
        | AdditionalConstrExpr.SequenceBefore(before = before; expr = e) ->
            inferSecondaryCtorBody ctx expected fieldTypes before
            infer ctx e |> ignore
        | AdditionalConstrExpr.Conditional(cond = c; thenBranch = t; elseBranch = el) ->
            infer ctx c |> ignore
            inferSecondaryCtorBody ctx expected fieldTypes t
            inferSecondaryCtorBody ctx expected fieldTypes el
        | AdditionalConstrExpr.Init initExpr ->
            match initExpr with
            | AdditionalConstrInitExpr.Expression e ->
                match e with
                | Expr.HighPrecedenceApp(argExpr = argExpr) ->
                    let argTy = infer ctx argExpr
                    // Chain call to the primary ctor: admit an implicit
                    // class→interface upcast on the args (G19), e.g.
                    // `new() = Set(Comparer<'T>.Default, …)` into an `IComparer<'T>`
                    // primary-ctor param.
                    unifyArg ctx (CstKeys.ofExpr argExpr) argTy expected
                | Expr.App(argExprs = argExprs) ->
                    let argTys = [ for a in argExprs -> infer ctx a ]
                    unifyArg ctx (CstKeys.ofExpr e) (tupleOrSingle argTys) expected
                | _ -> infer ctx e |> ignore
            | AdditionalConstrInitExpr.Delegated(expr = e) -> infer ctx e |> ignore
            // Explicit field-init `{ f = e; … }`: infer each
            // initialiser and unify it against the named field's declared type so a
            // literal (`0`, `false`) or a generic field (`'T`) pins correctly. An
            // unknown field name leaves the type open (no constraint) — the field
            // resolution diagnostic belongs to a later pass, not inference.
            | AdditionalConstrInitExpr.Explicit(initializers = inits) ->
                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let initTy = infer ctx e

                    if not li.Idents.IsEmpty then
                        let fieldName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                        match Map.tryFind fieldName fieldTypes with
                        | Some fieldTy -> unify ctx (CstKeys.ofExpr e) initTy fieldTy
                        | None -> ()

    /// Type every secondary ctor (B-11) of a class under its typar scope: link
    /// param annotations, seed param binding-site TyVars, then infer each body.
    let private fillSecondaryCtors (ctx: PassContext) (info: ClassTypeInfo) : unit =
        if info.SecondaryCtors.Length > 0 then
            let savedScope = ctx.Resolution.TyparScope
            let savedStrict = ctx.Resolution.TyparScopeStrict
            let savedEnclosing = ctx.Resolution.EnclosingTypars
            let classScope = scopeOfTypeParams info.TypeParams
            ctx.Resolution.TyparScope <- classScope
            ctx.Resolution.TyparScopeStrict <- true
            // Class typars stay in scope across each secondary ctor body's
            // `inferBinding` (G11), mirroring `fillTypeMembers`.
            ctx.Resolution.EnclosingTypars <- ValueSome classScope

            try
                let expected =
                    info.CtorParams |> Array.map (fun p -> p.Type) |> Array.toList |> tupleOrSingle

                // Declared field types (ctor-param backing fields + explicit `val`
                // fields), keyed by name, so an explicit field-init `{ f = e }`
                // unifies `e` against `f`'s type. `val` fields win a name clash
                // (a positional ctor param sharing a name is the backing store).
                let fieldTypes =
                    Map.ofSeq (
                        seq {
                            for p in info.CtorParams -> p.Name, p.Type
                            for f in info.InstanceFields -> f.Name, f.Type
                        }
                    )

                for sc in info.SecondaryCtors do
                    fillSecondaryCtorParamTypes ctx sc.Params sc.ParamPat

                    for p in sc.Params do
                        match p.Type with
                        | TyVar tv -> ctx.Bindings.TypeVar.Set(p.DeclKey, tv)
                        | _ -> ()

                    enterLevel ctx

                    try
                        inferSecondaryCtorBody ctx expected fieldTypes sc.Body
                    finally
                        exitLevel ctx
            finally
                ctx.Resolution.TyparScope <- savedScope
                ctx.Resolution.TyparScopeStrict <- savedStrict
                ctx.Resolution.EnclosingTypars <- savedEnclosing

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
        | ValueSome(TyClass(baseKey, baseArgs)), ValueSome argExpr ->
            match TypeRegistry.tryClassByKey ctx.Types baseKey with
            | ValueSome baseInfo ->
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
            | ValueNone -> ()
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

    /// Type-check the member bodies of one resolved `interface IFace with member …`
    /// block against the interface's external signatures (B-2). For each impl member, unify its
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
        | ValueSome(TyClass(ifaceKey, ifaceArgs)) ->
            let ifaceName = SymbolKeyOps.qualifiedName ifaceKey

            match ctx.Provider.TryLookupType ifaceName with
            | ValueSome(ExternalTypeShape.Class shape) ->
                let argArr = ifaceArgs.AsSpan().ToArray()

                // Interfaces declare no constructors; the `.ctor` guard is
                // belt-and-suspenders against a provider that surfaces one.
                let required = shape.Members |> Array.filter (fun em -> em.Name <> ".ctor")

                for mInfo in impl.Members do
                    match required |> Array.tryFind (fun em -> em.Name = mInfo.Name) with
                    | Some em ->
                        let expected = normalizeObj (ExternalSymbols.openSignature em argArr)
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

    /// Conform each `override` member of a class to the `System.Object` virtual
    /// slot it overrides, pinning the (often unannotated) parameter / return
    /// types so they don't leak as free typars. A class with no `inherit` clause
    /// can only override Object's three virtuals — `Equals(obj):bool`,
    /// `GetHashCode():int`, `ToString():string` — so the expected signatures are
    /// fixed. Without this, `override _.Equals that` leaves `that` a free TyVar
    /// that `generaliseMemberTypars` would have quantified (now skipped for
    /// overrides), and Freeze would emit it as `bool Equals<M0>(!!0)` — a generic,
    /// non-Object-matching method. Runs after the member bodies are typed (so the
    /// placeholder `mInfo.Type` carries the inferred `param -> ret` shape), the
    /// `Object`-slot analogue of `checkInterfaceConformance`. v1 supports only
    /// `inherit`-less classes here; a class deriving a project-local base that
    /// declares its own virtuals is a later slice.
    let private checkObjectOverrideConformance (ctx: PassContext) (info: ClassTypeInfo) : unit =
        let objTy = TyConst("obj", EqArray.empty)
        let boolTy = TyConst("bool", EqArray.empty)
        let intTy = TyConst("int", EqArray.empty)
        let unitTy = TyConst("unit", EqArray.empty)
        let stringTy = TyConst("string", EqArray.empty)

        for mInfo in info.Members do
            if mInfo.IsOverride && mInfo.Kind = ClassMemberKind.Method then
                // The expected Object-slot type, keyed by name. A nullary method's
                // inferred type is `unit -> ret`, a 1-arg method's `arg -> ret`.
                let expected =
                    match mInfo.Name with
                    | "Equals" -> ValueSome(TyFun(objTy, boolTy))
                    | "GetHashCode" -> ValueSome(TyFun(unitTy, intTy))
                    | "ToString" -> ValueSome(TyFun(unitTy, stringTy))
                    | _ -> ValueNone

                match expected with
                | ValueSome expectedTy -> unify ctx mInfo.DeclKey mInfo.Type expectedTy
                | ValueNone -> ()

    /// §5.2 resolution pre-pass (B-2): resolve
    /// each `interface IFace with member …` block's interface type and stamp
    /// `impl.Resolved` *before* any member body — the class's own members or a
    /// sibling interface block — is typed. The interface type resolves under the
    /// class's typar scope (so a generic interface arg like `IEnumerable<'T>`
    /// binds to the class's typar); it must map to a type the provider reports as
    /// an interface, else a diagnostic fires and `Resolved` stays `ValueNone`.
    /// `subsumes` reads `InterfaceImpls.Resolved` to admit a class→interface
    /// upcast (G19/G20: `this :> seq<_>`, a `Set` value flowing into an
    /// `IComparer` slot), so the class must already know its declared interfaces
    /// at every coercion site, not only once its own block's body is reached.
    /// Body typing + conformance stay in `fillInterfaceImpls`.
    let private resolveInterfaceImpls (ctx: PassContext) (info: ClassTypeInfo) : unit =
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
                | TyClass(ifaceKey, _) ->
                    match ExternalSymbols.tryLookupType ctx.Provider ifaceKey with
                    | ValueSome(ExternalTypeShape.Class shape) -> shape.IsInterface
                    // A project-local interface has no external-provider entry — its
                    // interface-ness is on the registered `ClassTypeInfo`.
                    | _ ->
                        match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                        | ValueSome localInfo -> localInfo.IsInterface
                        | ValueNone -> false
                | _ -> false

            if isInterface then
                impl.Resolved <- ValueSome resolved
            else
                let shown =
                    match zonk resolved with
                    | TyClass(n, _) -> SymbolKeyOps.qualifiedName n
                    | other -> sprintf "%A" other

                ctx.Error(impl.DeclKey, sprintf "Type '%s' is not an interface" shown)

    /// Type-check each `interface IFace with member …` block's member bodies and
    /// conformance-check them against the interface. Member bodies type through
    /// `fillTypeMembers` exactly like the class's own members — `this` re-binds to
    /// the class instance via `info.ThisKey`. Once typed, each body's signature is
    /// conformance-checked against the interface (§5.2, `checkInterfaceConformance`).
    /// Runs after `resolveInterfaceImpls` (so every `impl.Resolved` is stamped) and
    /// after the class's own `fillTypeMembers` / `fillSecondaryCtors`, so ctor
    /// params and the base call are already seeded and `PrelinkExtras` is a no-op here.
    let private fillInterfaceImpls (ctx: PassContext) (info: ClassTypeInfo) : unit =
        for impl in info.InterfaceImpls do
            fillTypeMembers
                ctx
                {
                    TypeParams = info.TypeParams
                    Members = impl.Members
                    ThisKey = info.ThisKey
                    MkSelfType = fun args -> TyClass(info.Key, args)
                    PrelinkExtras = ignore
                    Elements = impl.Elements
                    AllowAbstractSig = false
                    // The interface slot fixes each member's signature
                    // (`checkInterfaceConformance` below); never generalise.
                    Generalise = false
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
                            // Attach the class's `when 'S :> IFace` typar constraints
                            // to the prototype TyVars (under the class typar scope, set
                            // by `fillTypeMembers` before this runs) — so a member-body
                            // `this.field` access on an interface-constrained class typar
                            // resolves through the interface (rung-3 `CallVia.Interface`).
                            // Mirrors `fillRecordFieldTypes`/`fillUnionFieldTypes`.
                            match info.TyparConstraints with
                            | ValueSome cs -> translateConstraints ctx cs
                            | ValueNone -> ()

                            // Fill ctor-param placeholders under the class's
                            // typar scope, then seed `ctx.Bindings.TypeVar` so
                            // `inferIdent` lookups against the param binding
                            // sites return these.
                            fillClassCtorParamTypes ctx info pc

                            for p in info.CtorParams do
                                match p.Type with
                                | TyVar tv -> ctx.Bindings.TypeVar.Set(p.DeclKey, tv)
                                | _ -> ()

                            // Explicit `val [mutable] x: T` instance fields are
                            // always annotated; translate each under the class's
                            // typar scope (already entered) and link the placeholder
                            // TyVar so `this.x` reads / `this.x <- …` writes type
                            // against the declared field type in member bodies.
                            for fld in info.InstanceFields do
                                match fld.Type with
                                | TyVar tv ->
                                    let translated = translateType ctx fld.TypeCst
                                    (UnionFind.find tv).Link <- ValueSome translated
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

                        // Interface-impl types are resolved up front by
                        // `resolveInterfaceImplsForElem` (walkElems), before *any*
                        // module-function body or class member is typed — so every
                        // `:>` / argument-coercion / `for x in (c: C)` site (G19/G20)
                        // sees the class's declared interfaces, including from a module
                        // function inferred ahead of `fillClassMembers`.
                        fillTypeMembers
                            ctx
                            {
                                TypeParams = info.TypeParams
                                Members = info.Members
                                ThisKey = info.ThisKey
                                MkSelfType = fun args -> TyClass(info.Key, args)
                                PrelinkExtras = prelinkExtras
                                Elements = body.elements
                                AllowAbstractSig = true
                                Generalise = true
                            }

                        // Pin each `override` member to its `System.Object` slot
                        // *after* the bodies are typed (so `mInfo.Type` carries the
                        // inferred shape) but *before* `fillInterfaceImpls` — an
                        // interface-impl member sharing a name with an override
                        // (`Equals`) reads the override's `MethodTypeParams` by name
                        // in Elaborate, so the override must be non-generic first.
                        checkObjectOverrideConformance ctx info
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
                    typeName = (TypeName(ident = nameLi) as tn)
                    extensions = ValueSome(TypeExtensionElements(elements = elems))) when nameLi.Idents.Length = 1 ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    let arity =
                        NameResolutionTypeRegistration.typarNamesOfTypeName ctx tn |> List.length

                    match TypeRegistry.tryUnion ctx.Types name arity with
                    | ValueSome info when not (Array.isEmpty info.Members) ->
                        fillTypeMembers
                            ctx
                            {
                                TypeParams = info.TypeParams
                                Members = info.Members
                                ThisKey = info.ThisKey
                                MkSelfType = fun args -> TyUnion(info.Key, args)
                                PrelinkExtras = ignore
                                Elements = elems
                                AllowAbstractSig = false
                                Generalise = true
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

    /// Stamp every project-local class's `InterfaceImpls.Resolved` (G19/G20) up
    /// front — before module-function bodies or class members type — so a `:>` /
    /// argument-coercion / `for x in (c: C)` site sees the class's declared
    /// interfaces even when it lives in a module function inferred ahead of
    /// `fillClassMembers`. Self-contained (manages its own typar scope); the sole
    /// caller of `resolveInterfaceImpls`.
    let private resolveInterfaceImplsForElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match TypeDefnPatterns.tryClassLikeDecl td with
                | ValueSome d ->
                    let (TypeName(ident = nameLi)) = d.TypeName

                    if nameLi.Idents.Length = 1 then
                        match ctx.Types.Class.TryGetValue(ctx.NameOf nameLi.Idents.[0]) with
                        | true, info -> resolveInterfaceImpls ctx info
                        | false, _ -> ()
                | ValueNone -> ()
        | _ -> ()

    let private walkElems (ctx: PassContext) (pairs: (ModuleElem<SyntaxToken> * OpenScope) list) =
        let elems = ImmutableArray.CreateRange(pairs |> List.map fst)
        fillAbbreviationBodies ctx elems

        // Set `ctx.Resolution.OpenScope` per element so the provider-probe sites
        // (`inferIdent`, `tryExternalTypeReceiver`) resolve short external names
        // against the `open`s in scope at that element.
        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillRecordFieldTypes ctx m

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillUnionFieldTypes ctx m

        // Resolve every class's interface impls before any body types (so a
        // module function's `for x in (c: C)` and any `:>`/coercion sees them).
        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            resolveInterfaceImplsForElem ctx m

        // G19 residue: seed annotation-derived schemes for module-level functions
        // *before* class member bodies are typed, so a class member's forward
        // reference to a sibling-module function (`SetTree.add`) instantiates a
        // fresh signature and the argument-coercion site can upcast a subtype
        // argument (`Comparer<'T>` → `IComparer<'T>`) instead of monomorphically
        // pinning the function's param. See `prebindModuleFunctionSchemes`.
        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope

            match m with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                prebindModuleFunctionSchemes ctx bindings
            | _ -> ()

        // Type bodies in **declaration order**, dispatching each element to its
        // handler (each is a no-op for a non-matching element). This is the key to
        // the module↔class dependency: a class member that calls an *earlier*
        // module function (`Set.Add` → `SetTree.add`, declared above) sees that
        // function's *real* generalised scheme, while a *later* module function over
        // the class (`Set.partition set = set.Partition …`) sees the class member's
        // already-typed body. Batching all classes before all module functions (or
        // vice versa) cannot satisfy both directions; declaration order — sound for
        // non-recursive F#, where a use must follow its definition — does.
        //
        // Without this, a class member calling an earlier module function fell back
        // to the annotation-only `prebindModuleFunctionSchemes` stand-in, which
        // over-generalises an *unannotated* parameter (`let add comparer k (t: …)`,
        // `k` undeclared) into a fresh quantified typar decoupled from the function's
        // `'T`. The member's argument (`value`) then bound that free typar and never
        // grounded, leaking a metavar into the member signature at contract
        // extraction. `prebind` is still seeded above so genuine forward references
        // (mutual recursion, a `rec` module) keep a usable scheme.
        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            fillClassMembers ctx m
            fillUnionMembers ctx m
            walkModuleElem ctx m

    /// Resolve the bare-program list literals left flexible by `listLiteralTy`
    /// (R3), after the whole file is walked so every consumer has had its say:
    ///   - still free (no consumer drove it, e.g. `printfn "%A" [1;2;3]`) → link to
    ///     FSharp.Core's `list`, its element carried through;
    ///   - flipped to a list-like type (`List.fold`'s `Vesper.Collections.List`
    ///     parameter) → reconcile the literal's element with the driven one.
    let private resolveListLiterals (ctx: PassContext) : unit =
        let key = NodeKey.ofSource 0 NodeKind.Unknown

        // A still-free literal defaults to FSharp.Core's `list`, except a
        // self-host package build (no FSharp.Core) defaults it to the Vesper
        // cons-list union so the emission stays BCL-only.
        let defaultListTy (elemTy: SemType) : SemType =
            if ctx.DefaultListIsVesper then
                TyUnion(RuntimeNames.vesperListKey, EqArray.singleton elemTy)
            else
                TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy)

        for (lv, elemTy) in ctx.ListLiterals do
            let root = UnionFind.find lv

            match root.Link with
            | ValueNone -> unify ctx key (TyVar root) (defaultListTy elemTy)
            | ValueSome target ->
                match zonk target with
                | TyRecord(_, args) when args.Length = 1 -> unify ctx key args.[0] elemTy
                | TyUnion(_, args) when args.Length = 1 -> unify ctx key args.[0] elemTy
                | _ -> ()

    /// Bind each operator-as-value site (`(+)` in `Seq.fold (+) …`) to a
    /// project-local static-operator member, after the whole file is typed so every
    /// operand is ground. F# resolves an operator *value* to the operand type's own
    /// `static member (+)`, not the built-in arithmetic operator; the node was typed
    /// with the built-in scheme, so re-decide here by scanning the now-zonked operand
    /// types. The *first* operand (left-to-right) whose type is a project-local
    /// nominal declaring a static member with the operator's compiled name wins
    /// (F#'s left bias) — checking every operand, not just the first, is what lets the
    /// member be declared on the type of *any* operand (`static member (+) (i: int, s: Set)`
    /// resolves on the right). The verdict (the declaring type's key) is recorded for
    /// `Freeze.translateIdent`, which eta-expands the value into a closure calling the
    /// member; no hit ⇒ the built-in / `External` value path is left untouched.
    ///
    /// TODO(heterogeneous SRTP): scanning every operand is the correct
    /// F# rule (`(+): ^T1 -> ^T2 -> ^T3 when (^T1 or ^T2): static member (+)`), but it
    /// is not yet *observable*, because the unifier collapses the three operator typars
    /// to one — the `(+)` *inline body* (`ops-platform.fs`) is written `^T -> ^T -> ^T`
    /// (homogeneous) even though its `.fsi` is `^T1 -> ^T2 -> ^T3`, and the
    /// `default ^T1: ^T3` / `default ^T2: ^T3` chain (`InferGeneralize.applyDefaults`)
    /// fuses what survives. So a mixed-operand operator can't type at all today
    /// (`int * V` → `int vs V`). The exemplar to support is fully-generic mixed-type
    /// SRTP inlining, e.g. `let inline lerp c p t = t * c + p * (GenericOne - c)`
    /// instantiated at `lerp 0.1f Vector2.Zero Vector2.One` (so `*` is `float32 * Vector2`,
    /// resolved via `Vector2`'s `op_Multiply`). When the typar collapse is lifted, this
    /// scan needs no change — the member already resolves off whichever operand declares
    /// it — so the remaining work is in the SRTP/defaulting layer, not here.
    let private resolveOperatorValues (ctx: PassContext) : unit =
        // The static-operator member declared on a project-local nominal named
        // `typeName` — the declaring type's `Key`. Mirrors `FreezeExpr.tryClassMember`
        // but returns only what the verdict needs (Freeze re-forms the member key).
        let tryOwnStaticOp (typeName: string) (opName: string) : SymbolKey voption =
            let pick (key: SymbolKey) (members: TypeMemberInfo[]) =
                if members |> Array.exists (fun m -> m.Name = opName && m.IsStatic) then
                    ValueSome key
                else
                    ValueNone

            match ctx.Types.Class.TryGetValue typeName with
            | true, info -> pick info.Key info.Members
            | false, _ ->
                match ctx.Types.Union.TryGetValue typeName with
                | true, info -> pick info.Key info.Members
                | false, _ -> ValueNone

        // Peel the curried arrows to the list of operand (parameter) types; the
        // trailing return type is not an operand and is dropped.
        let rec operands (t: SemType) : SemType list =
            match zonk t with
            | TyFun(a, b) -> a :: operands b
            | _ -> []

        // `site.Name` is always a compiled `op_*` name — the site is enqueued only
        // from `inferIdent`'s `SymbolicOp` leg, so a plain ident never reaches here.
        for site in ctx.OperatorValueSites do
            let declKey =
                operands site.Ty
                |> List.fold
                    (fun acc operand ->
                        match acc with
                        | ValueSome _ -> acc
                        | ValueNone ->
                            match zonk operand with
                            | TyClass(k, _)
                            | TyUnion(k, _) -> tryOwnStaticOp (SymbolKeyOps.simpleName k) site.Name
                            | _ -> ValueNone
                    )
                    ValueNone

            match declKey with
            | ValueSome dk -> ctx.Resolution.ResolvedOperatorValue.Set(site.Node, dk)
            | ValueNone -> ()

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        // Recompute the same per-element `OpenScope` NameResolution did, from the
        // same stable ambient seed (`AmbientOpenScope`, not the per-element
        // `OpenScope` the walk mutates).
        walkElems ctx (CstWalk.walkModuleTree ctx.NameOf ctx.Resolution.AmbientOpenScope file)
        resolveListLiterals ctx
        resolveOperatorValues ctx
