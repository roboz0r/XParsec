namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationTranslate
open NameResolutionTypeRefStamp
open NameResolutionScope
open NameResolutionTypeRegistration

// Registry stamping for class type definitions and union augmentation members, plus the
// `type … and …` group registration algorithm. A class's declared STRUCTURE (ctor-param
// annotations, `val` field types, `inherit` parent) resolves here, in the scope it is written.

module NameResolutionMemberRegistration =

    /// Constructor parameter info from a parameter *pattern* (a primary or a `new(...)`
    /// ctor's). Only simple patterns are accepted (`x`, `(x: T)`, tuples of those, `()` for
    /// none); anything else diagnoses. An annotation is linked to the param's TyVar here.
    let private ctorParamsOfPat (ctx: PassContext) (declTok: SyntaxToken) (p: Pat<SyntaxToken>) : ClassCtorParamInfo[] =
        let results = ResizeArray<ClassCtorParamInfo>()

        // The parameter's binding site is the pattern's own, the key a member body's
        // reference to the parameter resolves through.
        let addParam (p: Pat<SyntaxToken>) (annotation: Type<SyntaxToken> voption) =
            match BoundVarKey.siteOfCstPat p with
            | ValueNone -> () // unreachable: every arm below hands a (wrapped) `NamedSimple`
            | ValueSome site ->
                ctx.SetBoundVarName(site.BoundVar, site.Tok)
                let tv = ctx.NewTypeVar()
                ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)

                match annotation with
                | ValueSome t -> ctx.Store.SetLink(UnionFind.find ctx.Store tv, ValueSome(translateType ctx t))
                | ValueNone -> ()

                results.Add(ClassCtorParamInfo(ctx.NameOf site.Tok, TyVar tv, site))

        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.EmptyBlock _ -> () // `new()` / `C()` — no parameters
            | Pat.NamedSimple _ -> addParam p ValueNone
            | Pat.Typed(pat = Pat.NamedSimple _; typ = t) -> addParam p (ValueSome t)
            | Pat.EnclosedBlock(pat = inner) -> walk inner
            | Pat.Tuple(patterns = pats) ->
                for sub in pats do
                    walk sub
            | _ ->
                // Point at the offending sub-pattern when it has a token; else the
                // declaration's own.
                let patTok =
                    try
                        CstKeys.firstTokenOfPat p
                    with _ ->
                        declTok

                ctx.Report(
                    patTok,
                    Kind.NotYetSupported
                        "a constructor argument pattern other than a simple identifier (with optional type annotation)"
                )

        walk p
        results.ToArray()

    let private extractCtorParams
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (pcOpt: PrimaryConstrArgs<SyntaxToken> voption)
        : ClassCtorParamInfo[] =
        match pcOpt with
        | ValueNone -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueNone)) -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) -> ctorParamsOfPat ctx declTok p

    /// `ClassSecondaryCtorInfo` for a class body's `new(...)` overloads. Each is keyed from
    /// its own `new` token, so two overloads do not collide.
    let private extractSecondaryCtors
        (ctx: PassContext)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : ClassSecondaryCtorInfo[] =
        let acc = ResizeArray<ClassSecondaryCtorInfo>()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(newToken = nt; pat = pat; body = body)) ->
                let ctorKey = NodeKey.ofToken nt NodeKind.PatIdent
                let parms = ctorParamsOfPat ctx nt pat
                acc.Add(ClassSecondaryCtorInfo(ctorKey, parms, body))
            | _ -> ()

        acc.ToArray()

    let private identOrOpNameTok (ctx: PassContext) (id: IdentOrOp<SyntaxToken>) : (string * SyntaxToken) voption =
        match id with
        | IdentOrOp.Ident t -> ValueSome(ctx.NameOf t, t)
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ValueSome(ctx.NameOf op, op)
        | _ -> ValueNone

    /// `<'C, …>` after the member name — a member's own declared typars, in
    /// source order. Skips anonymous typars.
    let private memberTyparNames (ctx: PassContext) (tds: TyparDefns<SyntaxToken> voption) : string list =
        match tds with
        | ValueNone -> []
        | ValueSome(TyparDefns(defns = ds)) ->
            [
                for TyparDefn(typar = t) in ds do
                    match typarName ctx t with
                    | ValueSome n -> yield n
                    | ValueNone -> ()
            ]

    /// Free typar names in a member's *signature* (argument annotations then return type,
    /// source order) that are neither an enclosing-type typar nor one of the member's own
    /// `<'C>`: F# generalises these as method generic params (`member s.Map f : Set<'U>`).
    let private implicitMemberTypars
        (ctx: PassContext)
        (classTypars: string list)
        (b: Binding<SyntaxToken>)
        : string list =
        let known =
            System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)

        for n in classTypars do
            known.Add n |> ignore

        for n in memberTyparNames ctx b.typarDefns do
            known.Add n |> ignore

        let seen = System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        let acc = ResizeArray<string>()

        let addTypar (t: Typar<SyntaxToken>) =
            match typarName ctx t with
            | ValueSome n ->
                if not (known.Contains n) && seen.Add n then
                    acc.Add n
            | ValueNone -> ()

        // `VarType` and a `SubtypeConstraint`'s constrained typar are the only two type forms
        // that bear one. A `when`-clause's constraint types are NOT descended: an implicit
        // method typar is drawn from the signature's arg/return SHAPE, not a constraint target.
        let typarIter: CstTypeWalk.TypeIter =
            { CstTypeWalk.identityTypeIter with
                VisitType =
                    fun it t ->
                        match t with
                        | Type.VarType tp ->
                            addTypar tp
                            true
                        | Type.SubtypeConstraint(typar = tp) ->
                            addTypar tp
                            true
                        | Type.WhenConstrainedType(typ = inner) ->
                            // `false` suppresses the default recursion into the constraints.
                            CstTypeWalk.iterType it inner
                            false
                        | _ -> true
            }

        let walkTy (t: Type<SyntaxToken>) = CstTypeWalk.iterType typarIter t

        // Only a `(p : T)` annotation contributes a signature type; an unannotated
        // bound variable carries no typar.
        let rec walkPat (p: Pat<SyntaxToken>) =
            match p with
            | Pat.Typed(pat = inner; typ = t) ->
                walkTy t
                walkPat inner
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Attributed(pat = inner)
            | Pat.Optional(pat = inner)
            | Pat.As(pat = inner) -> walkPat inner
            | Pat.Tuple(patterns = ps)
            | Pat.StructTuple(patterns = ps)
            | Pat.Elems(pats = ps) ->
                for sub in ps do
                    walkPat sub
            | _ -> ()

        for ap in b.argumentPats do
            walkPat ap

        match b.returnType with
        | ValueSome(ReturnType(typ = t)) -> walkTy t
        | ValueNone -> ()

        List.ofSeq acc

    /// `TypeMemberInfo` placeholders for a type body's / augmentation's member elements.
    /// Shared by class registration and union augmentation. An unsupported element kind
    /// emits a diagnostic at `declTok`.
    let extractMembers
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (classTypars: string list)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : TypeMemberInfo[] =
        let memberInfos = ResizeArray<TypeMemberInfo>()

        let diagnose (kind: Kind) = ctx.Report(declTok, kind)

        let addMember
            mName
            kind
            isStatic
            isOverride
            (mSite: NodeSite)
            (seed: EqArray<string * TyVarId>)
            declaredCount
            : TypeMemberInfo =
            let tv = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)

            let cmi =
                TypeMemberInfo(mName, kind, isStatic, TyVar tv, mSite, seed, declaredCount)

            cmi.IsOverride <- isOverride
            memberInfos.Add cmi
            cmi

        // The name and site are supplied because a `with get`/`set` accessor registers under
        // an accessor name (`set_P`), not the one its own pattern spells.
        let registerBinding mName (mSite: NodeSite) (b: Binding<SyntaxToken>) kind isStatic isOverride =
            // A generic method's own `<'C>` typars (`member this.Map<'C> …`), then its
            // *implicit* ones, a `'U` appearing only in a param/return annotation. Both get
            // prototype TyVars; a property takes no implicit ones.
            let explicit = memberTyparNames ctx b.typarDefns

            let implicit =
                match kind with
                | ClassMemberKind.Method -> implicitMemberTypars ctx classTypars b
                | _ -> []

            // The count marks the leading `explicit` prefix of the seed: only those are
            // "declared-first"; the implicit ones order by appearance per the F# rule.
            let seed = mkTypeParams ctx.Store (explicit @ implicit)

            addMember mName kind isStatic isOverride mSite seed (List.length explicit)
            |> ignore

        let registerNamed (b: Binding<SyntaxToken>) kind isStatic isOverride =
            match MemberNames.ofBinding ctx b with
            | ValueSome m -> registerBinding m.Name m.Site b kind isStatic isOverride
            | ValueNone -> ()

        let registerAutoProperty id isStatic isOverride =
            addMember
                (ctx.NameOf id)
                ClassMemberKind.Property
                isStatic
                isOverride
                (NodeSite.ofToken NodeKind.PatIdent id)
                EqArray.empty
                0
            |> ignore

        let registerAbstractSlot (mName: string) (mTok: SyntaxToken) tds isStatic kind =
            let explicit = memberTyparNames ctx tds
            let seed = mkTypeParams ctx.Store explicit
            // An `abstract` signature is a slot declaration, never an override.
            addMember mName kind isStatic false (NodeSite.ofToken NodeKind.PatIdent mTok) seed (List.length explicit)
            |> ignore

        let registerAbstractMethod idOrOp tds isStatic kind =
            match identOrOpNameTok ctx idOrOp with
            | ValueSome(mName, mTok) -> registerAbstractSlot mName mTok tds isStatic kind
            | ValueNone -> ()

        // `abstract P: T with get, set` declares the same halves an impl-side
        // `with get … and set …` does, each keyed on its own `get` / `set` token.
        let registerAbstractProperty idOrOp tds isStatic (sigArgs: ImmutableArray<_>) getSet =
            match identOrOpNameTok ctx idOrOp with
            | ValueSome(propName, _) ->
                let halves = AccessorNames.halvesOf ctx.NameOf getSet

                match halves.Getter with
                | ValueSome tok ->
                    match sigArgs.Length with
                    | 0 -> registerAbstractSlot propName tok tds isStatic ClassMemberKind.Property
                    | _ ->
                        registerAbstractSlot (AccessorNames.getterName propName) tok tds isStatic ClassMemberKind.Method
                | ValueNone -> ()

                match halves.Setter with
                | ValueSome tok ->
                    registerAbstractSlot (AccessorNames.setterName propName) tok tds isStatic ClassMemberKind.Method
                | ValueNone -> ()
            | ValueNone -> ()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; keyword = kw; defn = d)) ->
                let isStatic = s.IsSome

                let isOverride =
                    match kw with
                    | MemberKeyword.Override _
                    | MemberKeyword.Default _ -> true
                    | MemberKeyword.Member _
                    | MemberKeyword.Abstract _ -> false

                match d with
                | MethodOrPropDefn.Method(defn = b) -> registerNamed b ClassMemberKind.Method isStatic isOverride
                | MethodOrPropDefn.Property(defn = b) -> registerNamed b ClassMemberKind.Property isStatic isOverride
                | MethodOrPropDefn.AutoProperty(ident = id) -> registerAutoProperty id isStatic isOverride
                | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(
                    ident = idOrOp; typarDefns = tds; sign = CurriedSig(args = sigArgs))) ->
                    // An arg-less signature (`abstract member Current : int`, no `->`)
                    // is an abstract *property*; a curried/function signature is a method.
                    let kind =
                        if sigArgs.IsEmpty then
                            ClassMemberKind.Property
                        else
                            ClassMemberKind.Method

                    registerAbstractMethod idOrOp tds isStatic kind
                | MethodOrPropDefn.PropertyWithGetSet(ident = propId; defns = defns) ->
                    PropertyAccessors.reportNonAccessors ctx propId defns

                    for a in PropertyAccessors.accessors ctx propId defns do
                        registerBinding a.Name a.Site a.Defn a.MemberKind isStatic isOverride
                | MethodOrPropDefn.AbstractSignature(MemberSig.PropSig(
                    ident = idOrOp; typarDefns = tds; sign = CurriedSig(args = sigArgs); getSet = getSet)) ->
                    registerAbstractProperty idOrOp tds isStatic sigArgs getSet
            | TypeDefnElement.Member(MemberDefn.Value _) ->
                // `val [mutable] x: T` fields are not members; `extractInstanceFields` has them.
                ()
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor _) ->
                // Secondary ctors are not members; `extractSecondaryCtors` has them. A union
                // augmentation has no primary ctor to chain to, so one there is dropped.
                ()
            | TypeDefnElement.InterfaceImpl _ ->
                // `interface IFace with member …` blocks are not part of the class's own
                // member set; `extractInterfaceImpls` collects them.
                ()
            | TypeDefnElement.InterfaceSpec _ ->
                // A bare `interface IFace` spec carries no member bodies to register.
                ()
            | TypeDefnElement.Inherit _ -> diagnose (Kind.NotYetSupported "inheritance")

        memberInfos.ToArray()

    /// Collect the `interface IFace with member …` blocks declared in a class body. Each
    /// interface member is re-wrapped as a `TypeDefnElement.Member` so the ordinary member
    /// machinery consumes it. The interface *type* is kept as raw CST, resolved later.
    let private extractInterfaceImpls
        (ctx: PassContext)
        (classTypars: string list)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : ClassInterfaceImplInfo[] =
        let acc = ResizeArray<ClassInterfaceImplInfo>()

        for el in elements do
            match el with
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(
                interfaceToken = ifaceTok; typ = ifaceTyp; objectMembers = objMembersOpt)) ->
                let ifaceSite = NodeSite.ofToken NodeKind.TypeNamed ifaceTok

                let memberEls: TypeDefnElements<SyntaxToken> =
                    match objMembersOpt with
                    | ValueSome(ObjectMembers(memberDefns = mds)) ->
                        ImmutableArray.CreateRange(seq { for md in mds -> TypeDefnElement.Member md })
                    | ValueNone -> ImmutableArray.Empty

                let members = extractMembers ctx ifaceTok classTypars memberEls
                acc.Add(ClassInterfaceImplInfo(ifaceTyp, members, memberEls, ifaceSite))
            | _ -> ()

        acc.ToArray()

    /// Collect `val [mutable] x: T` explicit instance fields declared in a class / struct
    /// body. A `val` field is always annotated, so its type resolves outright, under the
    /// class's typar scope. F# accepts no `static val` here, so a `staticToken` is ignored.
    let private extractInstanceFields
        (ctx: PassContext)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : ClassFieldInfo[] =
        let acc = ResizeArray<ClassFieldInfo>()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Value(mutableToken = mut; ident = id; typ = t)) ->
                acc.Add(
                    ClassFieldInfo(
                        ctx.NameOf id,
                        translateType ctx t,
                        mut.IsSome,
                        NodeSite.ofToken NodeKind.DeclLetBinding id
                    )
                )
            | _ -> ()

        acc.ToArray()

    /// `ClassPreambleEntry` placeholders for a class body's `[static] let` / `[static] do`
    /// preamble, split into the STATIC sequence (the `.cctor`'s body) and the INSTANCE one
    /// (the primary ctor's END). Each stays ONE ordered sequence: a `do` may observe a `let`.
    let private extractPreamble
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (hasPrimaryCtor: bool)
        (isValueType: bool)
        (preamble: ImmutableArray<ClassFunctionOrValueDefn<SyntaxToken>>)
        : struct (ClassPreambleEntry[] * ClassPreambleEntry[]) =
        let statics = ResizeArray<ClassPreambleEntry>()
        let instances = ResizeArray<ClassPreambleEntry>()

        // Every rejection here is a property of the CLASS, not of the offending entry, so it
        // is anchored at `declTok` and reported once per distinct verdict.
        let reported = HashSet<Kind>()

        let diagnose (kind: Kind) =
            if reported.Add kind then
                ctx.Report(declTok, kind)

        // An instance `let`/`do` runs in the PRIMARY ctor, and two shapes have none that can:
        // the `val`-field form (`type T = val …; new(…) = …`) declares no primary ctor (FS0963),
        // and a struct's zero-arg default ctor is not ours to write (FS0901 `let`, FS0035 `do`).
        let instanceAllowed (isDo: bool) =
            if not hasPrimaryCtor then
                diagnose (
                    Kind.Message
                        "An instance `let` or `do` binding may only be used in a type with a primary constructor"
                )

                false
            elif isValueType then
                if isDo then
                    diagnose (
                        Kind.Message
                            "Structs cannot contain `do` bindings because the default constructor for structs would not execute these bindings"
                    )
                else
                    diagnose (
                        Kind.Message
                            "Structs cannot contain value definitions because the default constructor for structs will not execute these bindings"
                    )

                false
            else
                true

        for d in preamble do
            match d with
            | ClassFunctionOrValueDefn.LetBindings(staticToken = st; isRec = isRec; bindings = bindings) ->
                let isStatic = st.IsSome

                let target =
                    if isStatic then ValueSome statics
                    elif instanceAllowed false then ValueSome instances
                    else ValueNone

                match target with
                | ValueSome acc ->
                    for b in bindings do
                        match bindingsOfPat ctx b.pattern with
                        | [ (name, key) ] ->
                            let tv = ctx.NewTypeVar()
                            ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)
                            acc.Add(ClassPreambleEntry.Let(ClassLetInfo(name, TyVar tv, key, b, isRec.IsSome)))
                        | _ ->
                            diagnose (Kind.NotYetSupported "a class-preamble binding other than a simple `let x = …`")
                | ValueNone -> ()
            | ClassFunctionOrValueDefn.Do(staticToken = st; expr = e) ->
                if st.IsSome then
                    statics.Add(ClassPreambleEntry.Do e)
                elif instanceAllowed true then
                    instances.Add(ClassPreambleEntry.Do e)

        struct (statics.ToArray(), instances.ToArray())

    /// Stamp `ClassTypeInfo` for every `TypeDefn.Class` (or `TypeDefn.Anon`, which the parser
    /// emits for the bare `type C(...) = member ...` form). The declared STRUCTURE
    /// resolves here, under the class's typar scope; member types are placeholder TyVars.
    let private registerClassTypeDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match TypeDefnPatterns.tryClassLikeDecl td with
        | ValueNone -> ()
        | ValueSome d ->
            let tn, pc, asD, body = d.TypeName, d.PrimaryConstr, d.AsDefn, d.Body
            let name = id.Name
            let declKey = id.DeclSite.Key
            let classTyparNames = typarNamesOfTypeName ctx tn
            let typeParams = mkTypeParams ctx.Store classTyparNames

            // One entry into the class typar scope, so a `'a` in a ctor param, a `val` field
            // or a secondary ctor's parameter all bind the same prototype TyVar.
            let structure =
                underTyparScope
                    ctx
                    typeParams
                    (fun () ->
                        {|
                            CtorParams = extractCtorParams ctx id.DeclSite.Tok pc
                            SecondaryCtors = extractSecondaryCtors ctx body.elements
                            InstanceFields = extractInstanceFields ctx body.elements
                        |}
                    )

            let memberInfos =
                ResizeArray<TypeMemberInfo>(extractMembers ctx id.DeclSite.Tok classTyparNames body.elements)

            let thisName =
                match asD with
                | ValueSome(AsDefn(ident = aid)) -> ctx.NameOf aid
                | ValueNone -> "this"

            let thisKey = BoundVarKey.ofDeclaredThis declKey
            let baseKey = BoundVarKey.ofDeclaredBase declKey

            let members = memberInfos.ToArray()

            // No `PrimaryConstrArgs` ⇒ the `val`-field form (`type T = val …; new(…) = …`):
            // the secondary ctors are the only ctors it has.
            let hasPrimaryCtor = pc.IsSome

            // The SHAPE attributes, matched by short name because the `.fsi` extractor decodes
            // the same ones with no resolver. `AllowNullLiteral` is on the decoded record too,
            // but reading it here would skip the FS0934 kind check below.
            let classAttrs =
                AttributeDecode.decodeClassAttributes ctx.NameOf (Attributes.attributesOfTypeName tn)

            // `[<Struct>]` (or the `type X = struct … end` shape) ⇒ value type. Known before
            // the preamble is extracted: a struct may not carry an instance one.
            let isValueType = classAttrs.IsValueType || TypeDefnPatterns.isStructShape td

            let struct (staticPreamble, instancePreamble) =
                extractPreamble ctx id.DeclSite.Tok hasPrimaryCtor isValueType body.classPreamble

            let info =
                ClassTypeInfo(
                    name,
                    typeParams,
                    structure.CtorParams,
                    members,
                    id.DeclSite,
                    thisName,
                    thisKey,
                    baseKey,
                    id.Key
                )

            info.StaticPreamble <- staticPreamble
            info.InstancePreamble <- instancePreamble
            info.SecondaryCtors <- structure.SecondaryCtors
            info.HasPrimaryCtor <- hasPrimaryCtor

            info.InterfaceImpls <- extractInterfaceImpls ctx classTyparNames body.elements
            // Retained raw: a member body re-enters the class typar scope later and needs
            // `when 'S :> IFace` to resolve an access on a constrained class typar.
            info.TyparConstraints <- NameResolutionTypeRegistration.typarConstraintsOfTypeName tn

            info.IsValueType <- isValueType
            // A project-local interface: an all-abstract class body, with no external
            // provider entry for the subtype check to find.
            info.IsInterface <- TypeDefnPatterns.isInterfaceShape td
            // `[<IsByRefLike>]` ⇒ a byref-like (`ref struct`) value type.
            info.IsByRefLike <- classAttrs.IsByRefLike
            info.InstanceFields <- structure.InstanceFields

            // Validate the declaration's attributes against the class kind (FS0382 / FS0377 /
            // FS0934). The all-abstract form (`type IFoo = abstract M: int`) declares an
            // INTERFACE and is judged as one, though it registers a `ClassTypeInfo` all the same.
            let classKind =
                if info.IsInterface then Attributes.TypeDefnKind.Interface
                elif isValueType then Attributes.TypeDefnKind.Struct
                else Attributes.TypeDefnKind.RefClass

            let attrV =
                Attributes.validateTypeDefnAttributes ctx classKind id.DeclSite.Tok (Attributes.attributesOfTypeName tn)

            info.Declared <-
                {
                    IsSealed = classAttrs.IsSealed
                    IsAbstract = false
                    AllowNullLiteral = attrV.AllowNullLiteral
                }

            info.EqualitySupport <-
                match attrV.Equality with
                | ValueSome v -> v
                | ValueNone ->
                    if isValueType then
                        EqualityVerdict.Structural
                    else
                        EqualityVerdict.Reference

            info.ComparisonSupport <-
                match attrV.Comparison with
                | ValueSome v -> v
                | ValueNone -> ComparisonVerdict.NoComparison

            TypeRegistry.registerClass ctx.Types info

    /// The explicit `interface … end` shape claims no type, so there is no `ClassTypeInfo` for a
    /// verdict to land on, whereas the all-abstract form registers as a class and takes its
    /// verdicts there. Kind LEGALITY still applies here, so run it and discard the rest.
    let private validateInterfaceTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Interface(typeName = tn) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length = 1 then
                Attributes.validateTypeDefnAttributes
                    ctx
                    Attributes.TypeDefnKind.Interface
                    nameLi.Idents.[0]
                    (Attributes.attributesOfTypeName tn)
                |> ignore
        | _ -> ()

    /// The use site the type named at `li` speaks from: its own place in the file, under
    /// the module and `open`s the registration scan currently stands in.
    let private useSiteOfTypeName (ctx: PassContext) (li: LongIdent<SyntaxToken>) : UseSite =
        ctx.UseSiteAt(NodeKey.ofToken li.Idents.[li.Idents.Length - 1] NodeKind.TypeNamed)

    /// Resolve a named type in an `inherit` clause *argument* position (`inherit
    /// Box<int>(v)`'s `int`) to a best-effort `SemType`: a registration-time mini
    /// translation, so an abbrev-named arg lands as an opaque `TyConst`, expanded later.
    let rec private translateInheritArg
        (ctx: PassContext)
        (typarScope: Map<string, TyVarId>)
        (t: Type<SyntaxToken>)
        : SemType =
        let freshTv () =
            let tv = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)
            TyVar tv

        match t with
        | Type.ParenType(typ = inner) -> translateInheritArg ctx typarScope inner
        | Type.VarType(Typar.Named(ident = id))
        | Type.VarType(Typar.Static(ident = id)) ->
            // A typar in the inherit clause binds to the derived class's prototype
            // TyVar so generic inheritance substitutes correctly at member-lookup
            // time (`type Wrapper<'a>(v: 'a) = inherit Box<'a>(v)`).
            match typarScope.TryFind(ctx.NameOf id) with
            | Some tv -> TyVar tv
            | None -> freshTv ()
        | Type.VarType(Typar.Anon _) -> freshTv ()
        | Type.NamedType li when li.Idents.Length = 1 ->
            resolveInheritArgName ctx (useSiteOfTypeName ctx li) (ctx.NameOf li.Idents.[0]) EqArray.empty
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let targs =
                EqArray.ofList
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateInheritArg ctx typarScope at
                            | TypeArg.Measure _ -> ()
                    ]

            resolveInheritArgName ctx (useSiteOfTypeName ctx li) (ctx.NameOf li.Idents.[0]) targs
        | Type.SuffixedType(baseType = bt; longIdent = li) when li.Idents.Length = 1 ->
            resolveInheritArgName
                ctx
                (useSiteOfTypeName ctx li)
                (ctx.NameOf li.Idents.[0])
                (EqArray.singleton (translateInheritArg ctx typarScope bt))
        | Type.TupleType(types = types) ->
            TyTuple(EqArray.ofList [ for ty in types -> translateInheritArg ctx typarScope ty ])
        | Type.FunctionType(fromType = f; toType = into) ->
            TyFun(translateInheritArg ctx typarScope f, translateInheritArg ctx typarScope into)
        | _ -> freshTv ()

    and private resolveInheritArgName
        (ctx: PassContext)
        (useSite: UseSite)
        (name: string)
        (args: EqArray<SemType>)
        : SemType =
        match IntrinsicResolve.tryResolveIntrinsicKey ctx.Resolver ctx.Types.IntrinsicKeys name with
        | Some k -> TyConst(k, args)
        | None ->
            match TypeRegistry.tryRecord ctx.Types useSite name with
            | ValueSome info -> TyRecord(info.TypeKey, args)
            | ValueNone ->
                match TypeRegistry.tryUnionBare ctx.Types useSite name with
                | ValueSome info -> TyUnion(info.TypeKey, args)
                | ValueNone ->
                    match TypeRegistry.tryClass ctx.Types useSite name with
                    | ValueSome info -> TyClass(info.TypeKey, args)
                    | ValueNone -> TyConst(RuntimeNames.opaqueKey name, EqArray.empty)

    /// Resolve an `inherit` clause's parent type to a `TyClass` under the derived class's
    /// typar scope. Diagnoses (and returns `ValueNone`) when the parent is a non-class type,
    /// an unknown name, or a multi-segment name.
    let private resolveInheritParent
        (ctx: PassContext)
        (typarScope: Map<string, TyVarId>)
        (t: Type<SyntaxToken>)
        : SemType voption =
        let rec nameAndArgs (t: Type<SyntaxToken>) : (LongIdent<SyntaxToken> * SemType list) voption =
            match t with
            | Type.ParenType(typ = inner) -> nameAndArgs inner
            | Type.NamedType li -> ValueSome(li, [])
            | Type.GenericType(longIdent = li; typeArgs = args) ->
                let targs =
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateInheritArg ctx typarScope at
                            | TypeArg.Measure _ -> ()
                    ]

                ValueSome(li, targs)
            | Type.SuffixedType(baseType = bt; longIdent = li) ->
                ValueSome(li, [ translateInheritArg ctx typarScope bt ])
            | _ -> ValueNone

        let diagnose (tok: SyntaxToken) (kind: Kind) = ctx.Report(tok, kind)

        match nameAndArgs t with
        | ValueNone -> ValueNone
        | ValueSome(li, targs) ->
            let nameTok = li.Idents.[li.Idents.Length - 1]
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeNamed

            if li.Idents.Length <> 1 then
                let qual = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                diagnose nameTok (Kind.NotYetSupported(sprintf "inheriting from a qualified base type '%s'" qual))
                ValueNone
            else
                let name = ctx.NameOf nameTok

                // A heritable base's platform repr → its external `TyClass`, or a "did not
                // resolve" diagnostic.
                let reprToExternalBase (repr: string) =
                    match tryResolveExternalTypeKey ctx repr targs.Length with
                    | ValueSome extKey -> ValueSome(TyClass(extKey, EqArray.ofList targs))
                    | ValueNone ->
                        diagnose
                            nameTok
                            (Kind.Message(
                                sprintf
                                    "Cannot inherit from external base '%s': its representation '%s' did not resolve to a known external type (is a package dependency missing?)"
                                    name
                                    repr
                            ))

                        ValueNone

                // The name is not a project-local class: a heritable primitive published by a
                // provider (`exn`, or a prior file's `(# class … #)` base like `Attribute`).
                let resolveThroughProvider () =
                    // `inherit X` is a name WRITTEN AT A SITE, so it resolves through the same
                    // opens-aware engine as any written type name. WITH contract ctors (`exn`)
                    // ⇒ the intrinsic canon; WITHOUT (`Attribute`) ⇒ the platform type.
                    match
                        tryPickExternalType
                            ctx
                            (arityProbes targs.Length)
                            (fun hit -> ExternalSymbols.intrinsicClassOf hit.Shape)
                            name
                    with
                    | ValueSome(struct (id, surface)) when surface.Members |> EqArray.exists (fun m -> m.Name = ".ctor") ->
                        ValueSome(TyConst(id.Canon, EqArray.ofList targs))
                    | ValueSome(struct (id, _)) ->
                        match id.Platform with
                        | IntrinsicPlatform.Repr repr -> reprToExternalBase repr
                        | IntrinsicPlatform.Unsupported target ->
                            diagnose nameTok (Kind.UnsupportedOnTarget(name, target))
                            ValueNone
                    | ValueNone ->
                        // A name the name table knows at any arity is a project-local type of
                        // some other kind; one it does not know is unknown *here*, which
                        // includes a type declared below this group.
                        if TypeRegistry.isTypeNameInScope ctx.Types (ctx.UseSiteAt diagKey) name then
                            diagnose
                                nameTok
                                (Kind.Message(
                                    sprintf "Cannot inherit from type '%s', because only classes are inheritable" name
                                ))
                        else
                            diagnose nameTok (Kind.Message(sprintf "Cannot inherit from unknown type '%s'" name))

                        ValueNone

                match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
                | ValueSome info -> ValueSome(TyClass(info.TypeKey, EqArray.ofList targs))
                | ValueNone ->
                    // Heritable-local arm: a `(# class … #)` intrinsic of THIS file. One read
                    // yields both the repr and the `class`-tag verdict. An `inherit` parent is
                    // an ARBITRARY written name (a record, a typo, a provider class), so the
                    // name → key step must be allowed to miss here.
                    let heritableLocalRepr =
                        match TypeRegistry.tryIntrinsicKeyOf ctx.Types name with
                        | ValueNone -> ValueNone
                        | ValueSome canon ->
                            match ctx.Types.IntrinsicReprKeys.TryGetValue canon with
                            | true, repr when repr.Heritable -> ValueSome repr.Platform
                            | _ -> ValueNone

                    match heritableLocalRepr with
                    // The EXTERNAL type the repr denotes, not the opaque value-repr `TyConst`.
                    | ValueSome platform -> reprToExternalBase platform
                    | ValueNone -> resolveThroughProvider ()

    /// Fill `BaseType` / `BaseCtorArgs` on a class with an `inherit` clause. The parent is
    /// resolved against the referent's registered DETAIL, not its identity, so it cannot be
    /// answered where the clause is seen, and `BaseType` is instead a slot filled at group close.
    let private registerInheritedSlot (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match TypeDefnPatterns.tryClassLikeDecl td with
        | ValueNone -> ()
        | ValueSome d ->
            match d.Body.inherits with
            | ValueNone -> ()
            | ValueSome(ClassInheritsDecl(typ = parentTyp; expr = exprOpt)) ->
                match TypeRegistry.tryClassByKey ctx.Types id.Key with
                | ValueSome info ->
                    let typarScope =
                        (Map.empty, info.TypeParams)
                        ||> EqArray.fold (fun acc (n, tv) -> Map.add n tv acc)

                    match resolveInheritParent ctx typarScope parentTyp with
                    | ValueSome parentTy ->
                        info.BaseType <- ValueSome parentTy
                        info.BaseCtorArgs <- exprOpt
                    | ValueNone -> ()
                | ValueNone -> ()

    /// Detect inheritance cycles among the classes of ONE group, once every `BaseType` slot
    /// is filled: on re-entry to the starting class, diagnose and clear its `BaseType` so
    /// later passes treat it as parent-less. A back-edge can only run inside one group.
    let private checkGroupInheritanceCycles (ctx: PassContext) (classes: ClassTypeInfo seq) : unit =
        for start in classes do
            // Compare on `TypeKey` (arity included), not the bare name, so an arity-overloaded
            // self-reference (`Foo\`2` : `Foo\`3`) is not falsely flagged as a cycle.
            let visited = System.Collections.Generic.HashSet<TypeKey>()
            visited.Add start.TypeKey |> ignore

            let rec walk (info: ClassTypeInfo) =
                match info.BaseType with
                | ValueSome(TyClass(parentKey, _)) ->
                    if parentKey = start.TypeKey then
                        ctx.Report(start.DeclSite.Tok, Kind.CyclicType(start.Name, TypeCycle.Inheritance))

                        start.BaseType <- ValueNone
                    elif not (visited.Add parentKey) then
                        // A cycle that doesn't pass back through `start`; it is
                        // diagnosed when iteration reaches a class on that cycle.
                        ()
                    else
                        match TypeRegistry.tryClassByKey ctx.Types parentKey with
                        | ValueSome parentInfo -> walk parentInfo
                        | ValueNone -> ()
                | _ -> ()

            walk start

    /// A bodied member of an intrinsic host must be declared `inline`. The host has no
    /// representation in the output to hang a method on, so its body can only be spliced
    /// into the caller; without `inline` the use site would call a method never emitted.
    let private requireInlineMembers
        (ctx: PassContext)
        (hostName: string)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : unit =
        let diagnose (tok: SyntaxToken) (message: string) = ctx.Report(tok, Kind.Message message)

        // `ValueNone` for an `abstract` slot because it declares no body, so there is nothing
        // to splice and nothing to mark.
        let bodyTok (d: MethodOrPropDefn<SyntaxToken>) : SyntaxToken voption =
            match d with
            | MethodOrPropDefn.Method(defn = b)
            | MethodOrPropDefn.Property(defn = b) -> ValueSome (CstKeys.siteOfBinding b).Tok
            | MethodOrPropDefn.AutoProperty(ident = id)
            | MethodOrPropDefn.PropertyWithGetSet(ident = id) -> ValueSome id
            | MethodOrPropDefn.AbstractSignature _ -> ValueNone

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(keyword = kw; inlineToken = inl; defn = d)) ->
                match bodyTok d with
                | ValueNone -> ()
                | ValueSome tok ->
                    match kw with
                    // `inline` is no remedy here: the host publishes no method table, so
                    // there is no slot to override.
                    | MemberKeyword.Override _
                    | MemberKeyword.Default _ ->
                        diagnose tok (IntrinsicHost.cannotDeclare hostName IntrinsicHost.Construct.Override)
                    | MemberKeyword.Member _
                    | MemberKeyword.Abstract _ ->
                        if inl.IsNone then
                            diagnose tok (IntrinsicHost.memberNeedsInline hostName)
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(newToken = newTok)) ->
                diagnose newTok (IntrinsicHost.cannotDeclare hostName IntrinsicHost.Construct.Constructor)
            // `val` declares storage, not a body; `interface`/`inherit` are not member
            // declarations.
            | TypeDefnElement.Member(MemberDefn.Value _)
            | TypeDefnElement.InterfaceImpl _
            | TypeDefnElement.InterfaceSpec _
            | TypeDefnElement.Inherit _ -> ()

    /// Stamp augmentation members + `interface … with` impls onto an already-registered
    /// union or record; must run after the type itself is registered. The extraction is
    /// kind-agnostic, so only the write-back target differs and each arm sets its own `info`.
    let private registerNominalMember (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        let extract (declKey: NodeKey) (typeParams: EqArray<string * TyVarId>) elems =
            let typarNames = [ for (n, _) in typeParams -> n ]

            {|
                Members = extractMembers ctx id.DeclSite.Tok typarNames elems
                InterfaceImpls = extractInterfaceImpls ctx typarNames elems
                ThisKey = BoundVarKey.ofDeclaredThis declKey
            |}

        match td with
        | TypeDefn.Union(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
            match TypeRegistry.tryUnionByKey ctx.Types id.Key with
            | ValueSome info ->
                let x = extract info.DeclSite.Key info.TypeParams elems
                info.Members <- x.Members
                info.InterfaceImpls <- x.InterfaceImpls
                info.ThisKey <- x.ThisKey
            | ValueNone -> ()
        | TypeDefn.Record(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
            match TypeRegistry.tryRecordByKey ctx.Types id.Key with
            | ValueSome info ->
                let x = extract info.DeclSite.Key info.TypeParams elems
                info.Members <- x.Members
                info.InterfaceImpls <- x.InterfaceImpls
                info.ThisKey <- x.ThisKey
            | ValueNone -> ()
        // An inline intrinsic-abbrev host (`type X = (# … #) with member …`): stamp its
        // augmentation members + `ThisKey` as the union/record arms do. The host is filed
        // under the intrinsic's CANON key, which the claim's name resolves to.
        | TypeDefn.Abbrev(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
            match TypeRegistry.tryIntrinsicAbbrevHostByCanon ctx.Types id.Name with
            | ValueSome info ->
                requireInlineMembers ctx id.Name elems
                let x = extract info.DeclSite.Key info.TypeParams elems
                info.Members <- x.Members
                info.InterfaceImpls <- x.InterfaceImpls
                info.ThisKey <- x.ThisKey
            | ValueNone -> ()
        | _ -> ()

    /// The nominal a `SemType` denotes DIRECTLY, if any. A type argument is NOT direct: a
    /// `B option` field stores a reference to a `B`, so it is an indirection, and only the
    /// outermost type constructor of a field's type is an immediate containment edge.
    let private directNominal (store: TypeStore) (t: SemType) : TypeKey voption =
        match zonk store t with
        | TyRecord(key, _)
        | TyUnion(key, _)
        | TyClass(key, _) -> ValueSome key
        | TyEnum key -> ValueSome key
        | _ -> ValueNone

    /// The types a registered declaration STORES INLINE: a struct record's fields, a struct
    /// union's case fields, a struct class's `val` and ctor-param backing fields. Only asked of
    /// a value type, because a reference type stores a POINTER to its contents instead.
    let private inlineFieldTypes (ctx: PassContext) (id: TypeIdentity) : SemType seq =
        let key = id.Key

        match id.Kind with
        | TypeDeclKind.Record ->
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info -> seq { for f in info.Fields -> f.Type }
            | ValueNone -> Seq.empty
        | TypeDeclKind.Union ->
            match TypeRegistry.tryUnionByKey ctx.Types key with
            | ValueSome info ->
                seq {
                    for c in info.Cases do
                        yield! c.Fields
                }
            | ValueNone -> Seq.empty
        | TypeDeclKind.Class ->
            match TypeRegistry.tryClassByKey ctx.Types key with
            | ValueSome info ->
                seq {
                    for f in info.InstanceFields -> f.Type
                    for p in info.CtorParams -> p.Type
                }
            | ValueNone -> Seq.empty
        | TypeDeclKind.Enum
        | TypeDeclKind.Abbreviation
        | TypeDeclKind.IntrinsicRepr -> Seq.empty

    /// FS0954's other half: a cycle through STRUCT FIELDS. A value type stores its fields inline,
    /// so `[<Struct>] type A = { x: B } and [<Struct>] B = { y: A }` has no finite layout, while
    /// the same pair as reference types compiles because the indirection breaks the cycle.
    let private checkGroupStructFieldCycles (ctx: PassContext) (structs: ClaimedTypeDefn seq) : unit =
        let members = Dictionary<TypeKey, TypeIdentity>()

        for claimed in structs do
            members.[claimed.Identity.Key] <- claimed.Identity

        for KeyValue(startKey, startId) in members do
            let visited = HashSet<TypeKey>()
            visited.Add startKey |> ignore
            let mutable cyclic = false

            let rec walk (id: TypeIdentity) =
                for fieldTy in inlineFieldTypes ctx id do
                    match directNominal ctx.Store fieldTy with
                    | ValueSome fieldKey when not cyclic ->
                        if fieldKey = startKey then
                            cyclic <- true
                        elif visited.Add fieldKey then
                            match members.TryGetValue fieldKey with
                            | true, next -> walk next
                            // A struct field of a type OUTSIDE the group cannot lead back
                            // into it: mutual reference needs `and`, so a cycle stays in one group.
                            | false, _ -> ()
                    | _ -> ()

            walk startId

            if cyclic then
                ctx.Report(startId.DeclSite.Tok, Kind.CyclicType(startId.Name, TypeCycle.Immediate))

    /// Register one accepted declaration's kind-specific DETAIL (fields, cases, enum case
    /// names, class members / ctor params, abbreviation RHS), plus any `with member …`
    /// augmentation on it. The identity is handed in, never re-derived from the CST.
    let private registerDetail (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match id.Kind, td with
        | TypeDeclKind.Record, TypeDefn.Record(typeName = tn; fields = fields) -> registerRecordDecl ctx id tn fields
        | TypeDeclKind.Union, TypeDefn.Union(typeName = tn; cases = cases) -> registerUnionDecl ctx id tn cases
        | TypeDeclKind.Enum, TypeDefn.Enum(typeName = tn; cases = cases) -> registerEnumDecl ctx id tn cases
        | TypeDeclKind.Class, _ -> registerClassTypeDefn ctx id td
        // The abbreviation ENTRY is filed ahead of every other kind's detail, so those two
        // kinds have nothing left to do here.
        | TypeDeclKind.Abbreviation, _
        | TypeDeclKind.IntrinsicRepr, _
        | TypeDeclKind.Record, _
        | TypeDeclKind.Union, _
        | TypeDeclKind.Enum, _ -> ()

        registerNominalMember ctx id td

    /// Register one `type … and …` group, in the phases below: claim, classify, file the
    /// abbreviation entries, register detail, close. File-order type scoping falls out because
    /// at the moment a group registers, `TypeClaims` holds every type above it and none below.
    let registerGroup
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (recScopeOffset: int voption)
        (defs: ImmutableArray<TypeDefn<SyntaxToken>>)
        : unit =
        let claims = ResizeArray<ClaimedTypeDefn>(defs.Length)
        // One offset for the whole group: `and`-joined siblings share one visibility.
        let visibleFrom = typeGroupVisibleFrom recScopeOffset defs

        for td in defs do
            match claimTypeIdentity ctx c visibleFrom td with
            | ValueSome claimed -> claims.Add claimed
            | ValueNone -> ()

        // Over ALL defs, not only the claimed ones: a declaration that claims no type (an
        // `interface … end`, a delegate) still writes type names that must resolve.
        for td in defs do
            classifyDeclaredTypes ctx td

        for claimed in claims do
            match claimed.Identity.Kind with
            | TypeDeclKind.Abbreviation
            | TypeDeclKind.IntrinsicRepr ->
                match claimed.Defn with
                | TypeDefn.Abbrev(typeName = tn; typ = rhs; extensions = ext) ->
                    registerAbbreviationDecl ctx claimed.Identity tn rhs ext.IsSome
                | _ -> ()
            | TypeDeclKind.Record
            | TypeDeclKind.Union
            | TypeDeclKind.Enum
            | TypeDeclKind.Class -> ()

        for claimed in claims do
            registerDetail ctx claimed.Identity claimed.Defn

        // An `interface … end` declares no type to register, but its eq/comp attributes are
        // still illegal, so the kind-legality check runs over the group's CST.
        for td in defs do
            validateInterfaceTypeDefn ctx td

        // Group close: force every alias body. `forceFill` is idempotent, so this reaches
        // exactly the aliases nothing referenced (a cyclic pair among them diagnoses here).
        for claimed in claims do
            if claimed.Identity.Kind = TypeDeclKind.Abbreviation then
                match TypeRegistry.tryAbbrevByKey ctx.Types claimed.Identity.Key with
                | ValueSome info -> forceFill ctx info
                | ValueNone -> ()

        let classes = ResizeArray<ClassTypeInfo>()

        for claimed in claims do
            if claimed.Identity.Kind = TypeDeclKind.Class then
                registerInheritedSlot ctx claimed.Identity claimed.Defn

                match TypeRegistry.tryClassByKey ctx.Types claimed.Identity.Key with
                | ValueSome info -> classes.Add info
                | ValueNone -> ()

        checkGroupInheritanceCycles ctx classes
        checkGroupStructFieldCycles ctx (claims |> Seq.filter (fun cl -> isValueTypeDefn ctx cl.Defn))
