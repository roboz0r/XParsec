namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInfer
open UnificationInferForwardSchemes

// Algorithm J + Rémy's levels. Fills `ctx.Bindings.TypeVar` for every TypeVar and
// `ctx.Bindings.Scheme` for every generalisable single-name `let`-bound name.

module Unification =

    let zonk = UnificationEngineCore.zonk
    let substituteWith = UnificationEngineCore.substituteWith
    let mkNamedTypeSubst = UnificationEngineCore.mkNamedTypeSubst
    let instantiateMember = UnificationEngineCore.instantiateMember
    let tryClassChainMemberDecl = UnificationEngineCore.tryClassChainMemberDecl

    let private walkModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            inferBindingGroup ctx bindings
        | ModuleElem.Expression e -> infer ctx e |> ignore
        | _ -> ()

    /// Fold a curried member signature into a `TyFun` chain (a multi-arg group `a * b` is
    /// a tuple parameter), under the caller's typar scope.
    let private curriedSigToSemType (ctx: PassContext) (CurriedSig(args = args; returnType = ret)) : SemType =
        let groupTy (ArgsSpec(args = specs)) =
            match List.ofSeq specs with
            | [ ArgSpec(typ = t) ] -> translateType ctx t
            | many -> TyTuple(EqArray.ofSeq (seq { for ArgSpec(typ = t) in many -> translateType ctx t }))

        let retTy = translateType ctx ret
        List.foldBack (fun struct (g, _arrow) acc -> TyFun(groupTy g, acc)) (List.ofSeq args) retTy

    /// The token an abstract slot keys on: the name it declares, or the operator naming it.
    let private abstractSlotToken (idOrOp: IdentOrOp<SyntaxToken>) : SyntaxToken voption =
        match idOrOp with
        | IdentOrOp.Ident t -> ValueSome t
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ValueSome op
        | _ -> ValueNone

    /// The `set` half of `P: T with get, set`, which the source spells only as the getter:
    /// `Item: int -> 'T with get, set` gives the setter `int -> 'T -> unit`, at `groups` = 1.
    let rec private setterSemType (groups: int) (getterTy: SemType) : SemType =
        match getterTy with
        | TyFun(p, r) when groups > 0 -> TyFun(p, setterSemType (groups - 1) r)
        | value -> TyFun(value, TyConst(RuntimeNames.unitKey, EqArray.empty))

    /// The ABI order a member's own typars take, in canonical F# order: the explicitly-declared
    /// `<'C>` typars first (source order), then every remaining free root of `memberTy` by first
    /// appearance. The declaring type's typars are FIXED — they are the type's axis, not the
    /// method's — and a seed typar already linked to a concrete type is no longer one.
    let private canonicalMemberTypars
        (ctx: PassContext)
        (declTypars: EqArray<string * TyVarId>)
        (mInfo: TypeMemberInfo)
        (memberTy: SemType)
        : GeneralizedTypars =
        let rootOf (tv: TyVarId) =
            match zonk ctx.Store (TyVar tv) with
            | TyVar r -> ValueSome(UnionFind.find ctx.Store r)
            | _ -> ValueNone

        // Zonked HERE because a class typar's root can move while a body types.
        let fixedRoots = HashSet<TyVarId>()

        for (_, ptv) in declTypars do
            match rootOf ptv with
            | ValueSome r -> fixedRoots.Add r.Id |> ignore
            | ValueNone -> ()

        let seed = EqArray.toList mInfo.SeedTypars

        // Only the leading `DeclaredTyparCount` are declared-first; the implicit ones order
        // by appearance in the signature.
        let declared =
            seed
            |> List.truncate mInfo.DeclaredTyparCount
            |> List.choose (fun (name, ptv) ->
                match rootOf ptv with
                | ValueSome r when (ctx.Store.Link r).IsNone -> Some(name, r.Id)
                | _ -> None
            )

        // Every registered member typar has a real source name (`'a`) that survives into the
        // emitted GenericParam; keyed by root identity, so `canonical` synthesises `M0`, `M1`, …
        // only for an unregistered root.
        let knownNames = Dictionary<TyVarId, string>()

        for (name, ptv) in seed do
            match rootOf ptv with
            | ValueSome r ->
                if not (knownNames.ContainsKey r.Id) then
                    knownNames.[r.Id] <- name
            | ValueNone -> ()

        GeneralizedTypars.canonical ctx.Store declared fixedRoots knownNames (zonk ctx.Store memberTy)

    /// The kind of declaration the members being filled belong to.
    [<RequireQualifiedAccess>]
    type private MemberFillHost =
        /// A class or interface declaration body.
        | Class
        /// An `interface IFace with member …` block, where each member's signature is fixed
        /// by the slot it implements.
        | InterfaceImpl
        /// A `with` augmentation on a union, record or intrinsic-abbrev host.
        | Augmentation

    /// An `abstract` signature declares a slot only in a class or interface body.
    let private allowsAbstractSig (host: MemberFillHost) : bool =
        match host with
        | MemberFillHost.Class -> true
        | MemberFillHost.InterfaceImpl
        | MemberFillHost.Augmentation -> false

    /// Are a method's body-inferred free typars quantified into its own method typars?
    let private generalisesMembers (host: MemberFillHost) : bool =
        match host with
        | MemberFillHost.Class
        | MemberFillHost.Augmentation -> true
        | MemberFillHost.InterfaceImpl -> false

    /// Parameters for a registry-driven walk over a class or union's member bodies.
    /// `PrelinkExtras` runs after the typar scope is set but before `this` is bound.
    [<NoEquality; NoComparison>]
    type private TypeMembersFill =
        {
            TypeParams: EqArray<string * TyVarId>
            Members: TypeMemberInfo[]
            ThisKey: NodeKey
            MkSelfType: EqArray<SemType> -> SemType
            PrelinkExtras: unit -> unit
            Elements: TypeDefnElements<SyntaxToken>
            Host: MemberFillHost
        }

    /// Stamp a member's `CanonicalTypars` post-inference, after resolving its defaults.
    let private generaliseMemberTypars
        (ctx: PassContext)
        (outerLevel: int)
        (classTypars: EqArray<string * TyVarId>)
        (mInfo: TypeMemberInfo)
        : unit =
        match mInfo.Type with
        | TyVar tv ->
            let memberTy = zonk ctx.Store (TyVar tv)
            // Resolve defaults first so a defaulted typar links its source and the walk
            // below skips it: `member m.Add a b = a + b` grounds to `int` rather than
            // quantifying the arithmetic typar.
            UnificationInferGeneralize.applyDefaults ctx.Store memberTy outerLevel

            mInfo.Generalise(canonicalMemberTypars ctx classTypars mInfo memberTy)
        | _ -> ()

    /// Walk every method / property / auto-property body under a typar scope seeded
    /// from `TypeParams`, plus a `this` binding linked to `MkSelfType`. Placeholder
    /// member TyVars are pre-populated so body inference links them to the inferred type.
    let private fillTypeMembers (ctx: PassContext) (fc: TypeMembersFill) : unit =
        let savedEnclosing = ctx.Resolution.EnclosingTypars
        let classScope = UnificationClassCtors.scopeOfTypeParams fc.TypeParams
        use _ = ctx.PushTyparScope(classScope, true)
        // Keep the class typars in scope across each member body's `inferBinding`
        // (which mints a fresh scope and would otherwise drop them).
        ctx.Resolution.EnclosingTypars <- ValueSome classScope

        try
            fc.PrelinkExtras()

            // `this`: fresh TyVar pre-linked to the self-type over the
            // declaration's prototype typars, so a generic member body
            // mentioning `'a` shares identity with them.
            let thisTv = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store thisTv, ctx.CurrentLevel)
            let selfArgs = EqArray.ofSeq (seq { for (_, ptv) in fc.TypeParams -> TyVar ptv })
            ctx.Store.SetLink(UnionFind.find ctx.Store thisTv, ValueSome(fc.MkSelfType selfArgs))
            ctx.Bindings.TypeVar.Set(fc.ThisKey, thisTv)

            let inferMemberBinding (mKey: NodeKey) (b: Binding<SyntaxToken>) =
                let mInfoOpt = fc.Members |> Array.tryFind (fun m -> m.DeclSite.Key = mKey)

                match mInfoOpt with
                | Some mInfo ->
                    match mInfo.Type with
                    | TyVar tv -> ctx.Bindings.TypeVar.Set(mKey, tv)
                    | _ -> ()
                | None -> ()

                // Seed the binding's own `<'C, …>` typars with their registration
                // prototypes, so the signature inferred in the fresh binding
                // scope shares roots with `mInfo.SeedTypars`.
                let savedSeed = ctx.Resolution.BindingTyparSeed
                let savedMemberEnclosing = ctx.Resolution.EnclosingTypars

                match mInfoOpt with
                | Some mInfo when not mInfo.SeedTypars.IsEmpty ->
                    let seed = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

                    for (n, ptv) in mInfo.SeedTypars do
                        seed.[n] <- ptv

                    ctx.Resolution.BindingTyparSeed <- ValueSome seed

                    // Keep the member's own typars in `EnclosingTypars` for the
                    // body walk, alongside the class typars, so a nested
                    // `let c = Comparer<'U>.Default` resolves `'U`, not free.
                    let memberEnclosing =
                        Dictionary<string, TyVarId>(classScope, System.StringComparer.Ordinal)

                    for (n, ptv) in mInfo.SeedTypars do
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

                // Generalise any body-inferred free typar, an unannotated param no
                // call ever grounded, into the member's own method typars.
                match mInfoOpt with
                | Some mInfo when
                    generalisesMembers fc.Host
                    && mInfo.Kind = ClassMemberKind.Method
                    // An `override` conforms to a base virtual slot, so it is
                    // never generic.
                    && not mInfo.IsOverride
                    ->
                    generaliseMemberTypars ctx outerLevel fc.TypeParams mInfo
                | _ -> ()

            // An abstract slot has no body: its type is the declared signature, computed by
            // `mkSigTy` under the slot's own typar scope.
            let linkAbstractSlot (mTok: SyntaxToken) (mkSigTy: unit -> SemType) =
                let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                match fc.Members |> Array.tryFind (fun mm -> mm.DeclSite.Key = mKey) with
                | Some mInfo ->
                    match mInfo.Type with
                    | TyVar tv ->
                        let root = UnionFind.find ctx.Store tv

                        // Extend the scope with the method's own `<'C, …>`
                        // typars, else they diagnose as free.
                        let memberScope =
                            if mInfo.SeedTypars.IsEmpty then
                                ctx.Resolution.TyparScope
                            else
                                let extended =
                                    Dictionary<string, TyVarId>(
                                        ctx.Resolution.TyparScope,
                                        System.StringComparer.Ordinal
                                    )

                                for (n, ptv) in mInfo.SeedTypars do
                                    extended.[n] <- ptv

                                extended

                        use _ = ctx.PushTyparScope(memberScope, ctx.Resolution.TyparScopeStrict)

                        let sigTy = mkSigTy ()
                        ctx.Store.SetLink(root, ValueSome sigTy)

                        // An abstract method has no body to infer, so mint its
                        // canonical ABI order from the elaborated signature.
                        if mInfo.Kind = ClassMemberKind.Method && not mInfo.SeedTypars.IsEmpty then
                            mInfo.Generalise(canonicalMemberTypars ctx fc.TypeParams mInfo sigTy)
                    | _ -> ()
                | None -> ()

            for el in fc.Elements do
                match el with
                | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                    match d with
                    | MethodOrPropDefn.Method(defn = b)
                    | MethodOrPropDefn.Property(defn = b) ->
                        match MemberNames.declKeyOfBinding b with
                        | ValueSome mKey -> inferMemberBinding mKey b
                        | ValueNone -> ()
                    | MethodOrPropDefn.PropertyWithGetSet(ident = propId; defns = defns) ->
                        for a in PropertyAccessors.accessors ctx propId defns do
                            inferMemberBinding a.Site.Key a.Defn
                    | MethodOrPropDefn.AutoProperty(ident = id; expr = e; returnType = rt) ->
                        enterLevel ctx

                        try
                            let bodyTy = infer ctx e

                            let resultTy =
                                match rt with
                                | ValueSome(ReturnType(typ = t)) ->
                                    let t' = translateType ctx t
                                    unify ctx (CstKeys.firstTokenOfExpr e) bodyTy t'
                                    t'
                                | ValueNone -> bodyTy

                            let mKey = NodeKey.ofToken id NodeKind.PatIdent

                            match fc.Members |> Array.tryFind (fun m -> m.DeclSite.Key = mKey) with
                            | Some mInfo ->
                                match mInfo.Type with
                                | TyVar tv -> ctx.Store.SetLink(UnionFind.find ctx.Store tv, ValueSome resultTy)
                                | _ -> ()
                            | None -> ()
                        finally
                            exitLevel ctx
                    | MethodOrPropDefn.AbstractSignature sign when allowsAbstractSig fc.Host ->
                        match sign with
                        | MemberSig.MethodOrPropSig(ident = idOrOp; sign = csig) ->
                            match abstractSlotToken idOrOp with
                            | ValueSome mTok -> linkAbstractSlot mTok (fun () -> curriedSigToSemType ctx csig)
                            | ValueNone -> ()
                        | MemberSig.PropSig(sign = csig; getSet = getSet) ->
                            let halves = AccessorNames.halvesOf ctx.NameOf getSet
                            let (CurriedSig(args = sigArgs)) = csig

                            match halves.Getter with
                            | ValueSome tok -> linkAbstractSlot tok (fun () -> curriedSigToSemType ctx csig)
                            | ValueNone -> ()

                            match halves.Setter with
                            | ValueSome tok ->
                                linkAbstractSlot
                                    tok
                                    (fun () -> setterSemType sigArgs.Length (curriedSigToSemType ctx csig))
                            | ValueNone -> ()
                    | _ -> ()
                | _ -> ()
        finally
            ctx.Resolution.EnclosingTypars <- savedEnclosing

    /// Conform one resolved `interface IFace with member …` block: unify each impl
    /// member's already-inferred signature with the same-named `ExternalMember`, under
    /// the impl's interface type-args. A missing member diagnoses at the interface name.
    let private checkInterfaceConformance (ctx: PassContext) (impl: ClassInterfaceImplInfo) : unit =
        match impl.Resolution with
        | InterfaceImplResolution.Resolved(ifaceKey, ifaceArgs) ->
            let ifaceName = SymbolKeyOps.typeMetaName ifaceKey

            // A capability interface (`disposable`) is an `IntrinsicInterface`, not a `Class`,
            // but conforms identically off its member surface.
            match ctx.Provider.TryLookupType ifaceKey with
            | ValueSome(ExternalSymbols.ExternalMembers ifaceMembers) ->
                let argArr = ifaceArgs.AsSpan().ToArray()

                let required = ifaceMembers |> EqArray.filter (fun em -> em.Name <> ".ctor")

                for mInfo in impl.Members do
                    match required |> EqArray.tryFind (fun em -> em.Name = mInfo.Name) with
                    | ValueSome em ->
                        let expected = ExternalSymbols.openSignature em argArr
                        // `obj | null` and `obj` are the same slot, so erase reference
                        // nullability on BOTH sides: `CompareTo(that: objnull)` satisfies
                        // an `IComparable.CompareTo(obj)` slot.
                        unify
                            ctx
                            mInfo.DeclSite.Tok
                            (stripReferenceNull ctx.Store mInfo.Type)
                            (stripReferenceNull ctx.Store expected)
                    | ValueNone ->
                        ctx.Report(mInfo.DeclSite.Tok, Kind.NoMember(ifaceName, MemberNoun.Member, mInfo.Name))

                for em in required do
                    if not (impl.Members |> Array.exists (fun m -> m.Name = em.Name)) then
                        ctx.Report(
                            impl.DeclSite.Tok,
                            Kind.Message(
                                sprintf "No implementation given for '%s' required by interface '%s'" em.Name ifaceName
                            )
                        )
            | _ -> ()
        | InterfaceImplResolution.Pending
        | InterfaceImplResolution.Rejected -> ()

    /// The declared slot a same-named `override` conforms to: the nearest instance member
    /// of that name up the `inherit` chain, at the parent's type args. `ValueNone` where the
    /// chain leaves the project — an external base's slots are not read here.
    let private tryBaseSlotType (ctx: PassContext) (info: ClassTypeInfo) (memberName: string) : SemType voption =
        match info.Base with
        | ValueSome inh ->
            match resolveStep ctx.Store (BaseParent.ty inh.Parent) with
            | TyClass(parentKey, parentArgs) -> tryClassChainMember ctx parentKey parentArgs memberName
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// Pin each `override` member to the virtual slot it conforms to, so an unannotated
    /// `override _.Equals that` does not leave `that` free and emit as the generic
    /// `bool Equals<M0>(!!0)`. A base class declaring the name owns the slot; where none
    /// does, the slot is `System.Object`'s.
    let private checkOverrideConformance (ctx: PassContext) (info: ClassTypeInfo) : unit =
        for mInfo in info.Members do
            if mInfo.IsOverride && mInfo.Kind = ClassMemberKind.Method then
                let expected =
                    match tryBaseSlotType ctx info mInfo.Name with
                    | ValueSome slotTy -> ValueSome slotTy
                    // The three Object slots, keyed by name. A nullary method's inferred
                    // type is `unit -> ret`, a 1-arg method's `arg -> ret`.
                    | ValueNone ->
                        match mInfo.Name with
                        | "Equals" -> ValueSome(TyFun(BuiltinTypes.tyObj, BuiltinTypes.tyBool))
                        | "GetHashCode" -> ValueSome(TyFun(BuiltinTypes.tyUnit, BuiltinTypes.tyInt))
                        | "ToString" -> ValueSome(TyFun(BuiltinTypes.tyUnit, BuiltinTypes.tyString))
                        | _ -> ValueNone

                match expected with
                | ValueSome expectedTy ->
                    unify
                        ctx
                        mInfo.DeclSite.Tok
                        (stripReferenceNull ctx.Store mInfo.Type)
                        (stripReferenceNull ctx.Store expectedTy)
                | ValueNone -> ()

    /// Reject authoring a BCL interface a capability already publishes (`interface seq<'T>`
    /// yields `IEnumerable<'T>`). The forbidden set is derived from the capability's `Platform`
    /// plus its inherited closure; implementing another CAPABILITY from it stays legal.
    let private checkCapabilityInterfaceCollisions (ctx: PassContext) (info: IInterfaceImplHost) : unit =
        let resolvedImpls =
            [
                for impl in info.InterfaceImpls do
                    match impl.Resolution with
                    | InterfaceImplResolution.Resolved(key, _) -> impl, key, ctx.Provider.TryLookupType key
                    | InterfaceImplResolution.Pending
                    | InterfaceImplResolution.Rejected -> ()
            ]

        // The transitive interface closure of a capability's platform interface. Metadata
        // `FrozenInterfaces` is already transitive; the walk is what makes a contract-layer
        // provider, which records only direct bases, agree.
        let closeOver (platform: TypeKey) : HashSet<TypeKey> =
            let seen = HashSet<TypeKey>()

            let rec walk (key: TypeKey) =
                if seen.Add key then
                    match ctx.Provider.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        shape.FrozenInterfaces |> EqArray.iter (fun i -> walk i.Key)
                    | _ -> ()

            walk platform
            seen

        let capabilityInterfaces =
            [
                for (_, key, shape) in resolvedImpls do
                    match shape with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface cap) ->
                        // Arity 0 is lossless: a platform type id spells its own `` `N ``.
                        SymbolKeyOps.typeMetaName key, closeOver (SymbolKeyOps.qualifiedTypeKeyOf cap.Platform.Value 0)
                    | _ -> ()
            ]

        if not (List.isEmpty capabilityInterfaces) then
            for (impl, key, shape) in resolvedImpls do
                match shape with
                // Another capability: legal, and the only way to implement an inherited
                // capability (`enumerator` + `disposable`).
                | ValueSome(ExternalTypeShape.IntrinsicInterface _) -> ()
                | _ ->
                    let qual = SymbolKeyOps.typeMetaName key

                    for (capability, published) in capabilityInterfaces do
                        if published.Contains key then
                            ctx.Report(
                                impl.DeclSite.Tok,
                                Kind.Message(
                                    sprintf
                                        "'%s' is part of the platform interface of capability '%s', which this type already implements, because the backend publishes that interface, and everything it inherits, for the capability. Remove this interface implementation."
                                        qual
                                        capability
                                )
                            )

    /// Resolve each `interface IFace with member …` block's interface type under the
    /// class's typar scope and stamp `impl.Resolution` before any member body is typed, because
    /// class→interface upcast sites read it.
    let private resolveInterfaceImpls (ctx: PassContext) (info: IInterfaceImplHost) : unit =
        let isInterfaceKey (ifaceKey: TypeKey) =
            match ctx.Provider.TryLookupType ifaceKey with
            | ValueSome shape -> ExternalSymbols.isInterfaceShape shape
            // A project-local interface has no external-provider entry, so its
            // interface-ness is read off the registered `ClassTypeInfo`.
            | ValueNone ->
                match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                | ValueSome localInfo -> localInfo.IsInterface
                | ValueNone -> false

        for impl in info.InterfaceImpls do
            let resolved =
                use _ =
                    ctx.PushTyparScope(UnificationClassCtors.scopeOfTypeParams info.TypeParams, true)

                translateType ctx impl.InterfaceCst

            match
                BaseEligibility.classifyImpl isInterfaceKey resolved
                |> BaseEligibility.admitImpl ctx impl.DeclSite.Tok
            with
            | ValueSome n -> impl.Resolution <- InterfaceImplResolution.Resolved(n.Key, n.Args)
            | ValueNone -> impl.Resolution <- InterfaceImplResolution.Rejected

        checkCapabilityInterfaceCollisions ctx info

    /// Type each `interface IFace with member …` block's member bodies, then conform them
    /// to the interface. `this` re-binds to the class instance via `info.ThisKey`.
    let private fillInterfaceImpls (ctx: PassContext) (info: IInterfaceImplHost) : unit =
        for impl in info.InterfaceImpls do
            fillTypeMembers
                ctx
                {
                    TypeParams = info.TypeParams
                    Members = impl.Members
                    ThisKey = BoundVarKey.identity info.ThisKey
                    MkSelfType = info.MkSelfType
                    PrelinkExtras = ignore
                    Elements = impl.Elements
                    Host = MemberFillHost.InterfaceImpl
                }

            checkInterfaceConformance ctx impl

    let private fillClassMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match TypeDefnPatterns.tryClassLikeDecl td with
                | ValueSome d ->
                    let body = d.Body

                    match NameResolutionTypeRegistration.tryDeclaredClass ctx d.TypeName with
                    | ValueSome info ->
                        let prelinkExtras () =
                            // Attach the class's `when 'S :> IFace` typar constraints to the
                            // prototype TyVars, so a member-body access on an interface-
                            // constrained class typar resolves through the interface.
                            match info.TyparConstraints with
                            | ValueSome cs -> translateConstraints ctx cs
                            | ValueNone -> ()

                            // A ctor param's TyVar already carries its declared type (linked
                            // at registration); seed the binding site to reuse that cell.
                            for p in info.CtorParams do
                                match p.Type with
                                | TyVar tv -> ctx.Bindings.TypeVar.Set(BoundVarKey.identity p.DeclSite.BoundVar, tv)
                                | _ -> ()

                            // Both no-op for parent-less classes. AFTER the ctor-param
                            // seeding: an `inherit Base(p)` arg reads `p`'s declared type.
                            UnificationClassCtors.fillBaseCtorCall ctx info
                            UnificationClassCtors.mintBaseTyVar ctx info

                            // Seed the registered placeholder first, so a preamble-bound
                            // name used elsewhere in the class types through the same cell.
                            let inferPreamble (entries: ClassPreambleEntry[]) =
                                for entry in entries do
                                    match entry with
                                    | ClassPreambleEntry.Let l ->
                                        match l.Type with
                                        | TyVar tv -> ctx.Bindings.TypeVar.Set(l.DeclKey, tv)
                                        | _ -> ()

                                        enterLevel ctx

                                        try
                                            inferBinding ctx l.Binding
                                        finally
                                            exitLevel ctx
                                    | ClassPreambleEntry.Do e -> infer ctx e |> ignore

                            inferPreamble info.StaticPreamble

                            // The instance sequence runs in the primary ctor, after the
                            // base-ctor call.
                            inferPreamble info.InstancePreamble

                        fillTypeMembers
                            ctx
                            {
                                TypeParams = info.TypeParams
                                Members = info.Members
                                ThisKey = BoundVarKey.identity info.ThisKey
                                MkSelfType = fun args -> TyClass(info.TypeKey, args)
                                PrelinkExtras = prelinkExtras
                                Elements = body.elements
                                Host = MemberFillHost.Class
                            }

                        // Before the impls: an interface-impl `Equals` reads the same-named
                        // override's typars, so pin the override non-generic first.
                        checkOverrideConformance ctx info
                        UnificationClassCtors.fillSecondaryCtors ctx info
                        fillInterfaceImpls ctx (info :> IInterfaceImplHost)
                    | ValueNone -> ()
                | ValueNone -> ()
        | _ -> ()

    let private fillHostMembers (ctx: PassContext) (host: IInterfaceImplHost) (elems: TypeDefnElements<SyntaxToken>) =
        if not (Array.isEmpty host.Members) then
            fillTypeMembers
                ctx
                {
                    TypeParams = host.TypeParams
                    Members = host.Members
                    ThisKey = BoundVarKey.identity host.ThisKey
                    MkSelfType = host.MkSelfType
                    PrelinkExtras = ignore
                    Elements = elems
                    Host = MemberFillHost.Augmentation
                }

        // Outside the `Members`-non-empty guard, so a type with *only* an interface
        // impl (no augmentation members) still fills.
        fillInterfaceImpls ctx host

    /// Fill the members of a union or record carrying a `with` augmentation.
    let private fillNominalMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                // Only a `with` block carries augmentation / interface-impl members.
                match TypeDefnPatterns.tryNonClassMemberHostDecl td with
                | ValueSome(struct (tn, ValueSome elems)) ->
                    match NameResolutionTypeRegistration.tryDeclaredNonClassHost ctx tn with
                    | ValueSome host -> fillHostMembers ctx host elems
                    | ValueNone -> ()
                | _ -> ()
        | _ -> ()

    /// Stamp every project-local type's `InterfaceImpls.Resolved` before any module-function or
    /// member body types, so a `:>` / argument-coercion / `for x in (c: C)` site sees the
    /// declared interfaces wherever it appears.
    let private resolveInterfaceImplsForElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match TypeDefnPatterns.tryClassLikeDecl td with
                | ValueSome d ->
                    match NameResolutionTypeRegistration.tryDeclaredClass ctx d.TypeName with
                    | ValueSome info -> resolveInterfaceImpls ctx (info :> IInterfaceImplHost)
                    | ValueNone -> ()
                | ValueNone ->
                    // A union or record may also declare `interface … with` blocks.
                    match TypeDefnPatterns.tryNonClassMemberHostDecl td with
                    | ValueSome(struct (tn, _)) ->
                        match NameResolutionTypeRegistration.tryDeclaredNonClassHost ctx tn with
                        | ValueSome host -> resolveInterfaceImpls ctx host
                        | ValueNone -> ()
                    | ValueNone -> ()
        | _ -> ()

    let private walkElems (ctx: PassContext) (elems: WalkedElem<SyntaxToken> list) =
        // `EnterElement` sets the per-element `OpenScope` provider probes resolve short
        // names against.
        for w in elems do
            ctx.EnterElement w
            resolveInterfaceImplsForElem ctx w.Elem

        // Seed annotation-derived schemes for module-level functions before member bodies
        // type, so a forward reference instantiates a fresh signature instead of
        // monomorphically pinning the param. Only in a `rec` scope, where one can resolve.
        for w in elems do
            ctx.EnterElement w

            match w.Elem with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when
                w.RecScopeOffset.IsSome
                ->
                prebindModuleFunctionSchemes ctx bindings
            | _ -> ()

        // Type bodies in **declaration order**: a class member calling an earlier module
        // function sees its real generalised scheme, while a later module function over
        // the class sees the member's already-typed body.
        for w in elems do
            ctx.EnterElement w
            fillClassMembers ctx w.Elem
            fillNominalMembers ctx w.Elem
            walkModuleElem ctx w.Elem

    /// Resolve list literals left flexible during inference, once the whole file is
    /// walked: still free (`printfn "%A" [1;2;3]`) → link to the default list type;
    /// already driven to a list-like type → reconcile its element with the literal's.
    let private resolveListLiterals (ctx: PassContext) : unit =
        for lit in ctx.ListLiterals do
            let root = UnionFind.find ctx.Store lit.Var

            match ctx.Store.Link root with
            | ValueSome target ->
                match zonk ctx.Store target with
                | TyRecord(_, args) when args.Length = 1 -> unify ctx lit.Tok args.[0] lit.Elem
                | TyUnion(_, args) when args.Length = 1 -> unify ctx lit.Tok args.[0] lit.Elem
                | _ -> ()
            | ValueNone ->
                if ctx.ConsListInScope then
                    unify ctx lit.Tok (TyVar root.Id) (RuntimeNames.consListTy lit.Elem)
                else
                    ctx.Report(lit.Tok, Kind.IntrinsicNotInScope Intrinsic.ConsList)

    /// Settle the `null`s no context pinned, once the whole file is walked. An `obj`-typed
    /// parameter or field absorbs its argument without unifying, so a `null` handed to one
    /// arrives here still free, and `obj` is the type F# gives it.
    ///
    /// A `null` that a scheme quantifies stays free: `let n = null` is `'a when 'a: null`, and
    /// `isNull`'s `null` merged with its own `'T`.
    let private resolveNullLiterals (ctx: PassContext) : unit =
        for lit in ctx.NullLiterals do
            let root = UnionFind.find ctx.Store lit.Var

            if (ctx.Store.Link root).IsNone && not (ctx.Store.Quantified root) then
                unify ctx lit.Tok (TyVar root.Id) BuiltinTypes.tyObj

    /// For a class, union or record: `EqualitySupport = Custom` ⇒ it must implement the
    /// equatable capability over Self; `ComparisonSupport = Custom` ⇒ the comparable
    /// capability plus `Custom` equality (custom ordering atop structural equality is incoherent).
    let private validateCustomEqCompImpls (ctx: PassContext) : unit =
        // The declaring type's own nominal key: Self is `TyClass`/`TyUnion(info.TypeKey, _)`.
        let argIsSelf (info: IInterfaceImplHost) (arg: SemType) : bool =
            match zonk ctx.Store arg with
            | TyClass(k, _)
            | TyRecord(k, _)
            | TyUnion(k, _) -> k = info.TypeKey
            | _ -> false

        // Best-effort on the arg: a type-constructor match with a Self arg, or with no readable arg
        // at all, satisfies the requirement.
        let implementsSelf (info: IInterfaceImplHost) (cap: RuntimeNames.CapabilityIdentity) : bool =
            info.InterfaceImpls
            |> Array.exists (fun impl ->
                match impl.Resolution with
                | InterfaceImplResolution.Resolved(ifaceKey, ifaceArgs) ->
                    cap.Matches ifaceKey && (ifaceArgs.Length = 0 || argIsSelf info ifaceArgs.[0])
                | InterfaceImplResolution.Pending
                | InterfaceImplResolution.Rejected -> false
            )

        let checkHost (info: IInterfaceImplHost) =
            let nameTok = info.DeclSite.Tok

            let needsEq = info.EqualitySupport = EqualityVerdict.Custom
            let needsCmp = info.ComparisonSupport = ComparisonVerdict.Custom

            // `capWord` is the diagnostic's word for the capability the provider cannot resolve, so
            // the check reports rather than silently passing.
            let requireCapability (cap: RuntimeNames.CapabilityIdentity voption) (attr: string) (capWord: string) =
                match cap with
                | ValueSome c when not (implementsSelf info c) ->
                    ctx.Report(nameTok, Kind.CapabilityNotImplemented(attr, SymbolKeyOps.qualifiedName c.SymKey))
                | ValueSome _ -> ()
                | ValueNone -> ctx.Report(nameTok, Kind.CapabilityNotDeclared(attr, capWord))

            if needsEq then
                requireCapability ctx.CapabilityIds.Equatable "[<CustomEquality>]" "equatable"

            // A `[<CustomEquality>]` type must author its own `override GetHashCode()`
            // (FS0344): the fallback hash is unsound under a non-structural custom
            // `Equals`, because it breaks equal ⇒ same-hash.
            if
                needsEq
                && not (info.Members |> Array.exists (fun m -> m.Name = "GetHashCode" && m.IsOverride))
            then
                ctx.Report(nameTok, Kind.MissingGetHashCodeOverride)

            if needsCmp then
                requireCapability ctx.CapabilityIds.Comparable "[<CustomComparison>]" "comparable"

                // Coherence: custom comparison demands custom equality.
                if not needsEq then
                    ctx.Report(nameTok, Kind.CustomComparisonNeedsEquality)

        for kv in ctx.Types.Class.ByKey do
            checkHost (kv.Value :> IInterfaceImplHost)

        for kv in ctx.Types.Union.ByKey do
            checkHost (kv.Value :> IInterfaceImplHost)

        for kv in ctx.Types.Record.ByKey do
            checkHost (kv.Value :> IInterfaceImplHost)

    /// FS0438: two members agreeing on name, static-ness, kind, value-parameter signature
    /// and method-typar arity are an unreachable duplicate rather than a legal overload.
    /// `Show(int)` / `Show(string)` coexist; `M(int)` declared twice collides.
    let private checkDuplicateMembers (ctx: PassContext) : unit =
        let checkHost (typeParams: EqArray<string * TyVarId>) (members: TypeMemberInfo[]) =
            let seen = HashSet<_>(HashIdentity.Structural)

            for m in members do
                if not (seen.Add(UnificationInferOverload.memberSignatureKey ctx.Store typeParams m)) then
                    ctx.Report(m.DeclSite.Tok, Kind.DuplicateMember m.Name)

        for kv in ctx.Types.Class.ByKey do
            checkHost kv.Value.TypeParams kv.Value.Members

        for kv in ctx.Types.Union.ByKey do
            checkHost kv.Value.TypeParams kv.Value.Members

        for kv in ctx.Types.Record.ByKey do
            checkHost kv.Value.TypeParams kv.Value.Members

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        // Recompute the same per-element `OpenScope` NameResolution did, from the stable
        // `AmbientOpenScope` seed (not the per-element `OpenScope` the walk mutates).
        walkElems ctx (CstModuleTree.walkImpl ctx.NameOf ctx.Resolution.AmbientOpenScope file)
        resolveListLiterals ctx
        UnificationInferGeneralize.applyDefaultsTo ctx.Store ctx.FormatHoles
        resolveNullLiterals ctx
        validateCustomEqCompImpls ctx
        checkDuplicateMembers ctx
