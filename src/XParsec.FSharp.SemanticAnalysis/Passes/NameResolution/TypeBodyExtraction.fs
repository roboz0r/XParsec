namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationTranslate

open NameResolutionTypeRegistration

// One walk over a type body's or augmentation's elements, in source order, producing its
// `TypeBodyMembers`. Member types are placeholder TyVars; ctor-param and `val` annotations
// are resolved under the type's typar scope.

module NameResolutionTypeBodyExtraction =

    /// The declaration a type body belongs to. Only a class body may declare constructors
    /// and `val` fields.
    [<RequireQualifiedAccess>]
    type TypeBodyHost =
        | Class of hasPrimaryCtor: bool
        | Augmentation

    /// Constructor parameter info from a parameter *pattern* (a primary or a `new(...)`
    /// ctor's). Only simple patterns are accepted (`x`, `(x: T)`, tuples of those, `()` for
    /// none); anything else diagnoses. An annotation is linked to the param's TyVar here.
    let private ctorParamsOfPat (ctx: PassContext) (declTok: SyntaxToken) (p: Pat<SyntaxToken>) : ClassCtorParamInfo[] =
        let results = ResizeArray<ClassCtorParamInfo>()

        // The parameter's binding site is the pattern's own; member bodies resolve the
        // parameter through this key.
        let addParam (p: Pat<SyntaxToken>) (annotation: Type<SyntaxToken> voption) =
            match BoundVarKey.siteOfCstPat p with
            | ValueNone -> () // unreachable: every arm below hands a (wrapped) `NamedSimple`
            | ValueSome site ->
                ctx.SetBoundVarName(site.BoundVar, site.Tok)
                let tv = ctx.NewTypeVar()
                ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)

                let declared = annotation |> ValueOption.map (translateType ctx)

                match declared with
                | ValueSome t -> ctx.Store.SetLink(UnionFind.find ctx.Store tv, t)
                | ValueNone -> ()

                results.Add(ClassCtorParamInfo(ctx.NameOf site.Tok, TyVar tv, declared, site))

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

    let extractCtorParams
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (pcOpt: PrimaryConstrArgs<SyntaxToken> voption)
        : ClassCtorParamInfo[] =
        match pcOpt with
        | ValueNone -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueNone)) -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) -> ctorParamsOfPat ctx declTok p

    let private identOrOpNameTok (ctx: PassContext) (id: IdentOrOp<SyntaxToken>) : (string * SyntaxToken) voption =
        match id with
        | IdentOrOp.Ident t -> ValueSome(ctx.NameOf t, t)
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ValueSome(ctx.NameOf op, op)
        | _ -> ValueNone

    /// `<'C, …>` after the member name — a member's own declared typars in source order.
    /// Skips anonymous typars.
    let private memberTypars (ctx: PassContext) (tds: TyparDefns<SyntaxToken> voption) : (string * TyparKind) list =
        match tds with
        | ValueNone -> []
        | ValueSome(TyparDefns(defns = ds)) ->
            [
                for TyparDefn(attributes = attrs; typar = t) in ds do
                    match typarName ctx t with
                    | ValueSome n -> yield n, kindOfSlot ctx attrs
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

        for (n, _) in memberTypars ctx b.typarDefns do
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

    /// One entry per source argument, across every curried group in source order.
    let private sigArgNames
        (ctx: PassContext)
        (sigArgs: ImmutableArray<struct (ArgsSpec<SyntaxToken> * SyntaxToken)>)
        : Block<string voption> =
        Block.ofSeq (
            seq {
                for struct (ArgsSpec(args = args), _) in sigArgs do
                    for ArgSpec(name = name) in args do
                        match name with
                        | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(ctx.NameOf id)
                        | ValueNone -> ValueNone
            }
        )

    /// A member's identity and shape before its type is inferred: everything
    /// `TypeMemberInfo` takes except the placeholder TyVar and the ordinal.
    type private MemberShape =
        {
            Name: string
            Kind: TMemberKind
            IsStatic: bool
            IsOverride: bool
            Site: NodeSite
            SeedTypars: Block<DeclaredTypar>
            DeclaredTyparCount: int
            ArgNames: Block<string voption>
        }

    let private addMember (ctx: PassContext) (acc: ResizeArray<TypeMemberInfo>) (m: MemberShape) : unit =
        let tv = ctx.NewTypeVar()
        ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)

        let cmi =
            TypeMemberInfo(m.Name, m.Kind, m.IsStatic, TyVar tv, m.Site, m.SeedTypars, m.DeclaredTyparCount, m.ArgNames)

        cmi.IsOverride <- m.IsOverride
        acc.Add cmi

    /// The shape of a bodied `member`. `mName` and `mSite` are the registered name and site;
    /// a `with get`/`set` accessor half registers as `get_P`/`set_P`.
    let private bindingMember
        (ctx: PassContext)
        (typarNames: string list)
        (mName: string)
        (mSite: NodeSite)
        (b: Binding<SyntaxToken>)
        (kind: TMemberKind)
        (isStatic: bool)
        (isOverride: bool)
        : MemberShape =
        // A generic method's own `<'C>` typars (`member this.Map<'C> …`), then its
        // *implicit* ones, a `'U` appearing only in a param/return annotation. Both get
        // prototype TyVars; a property takes no implicit ones.
        let explicit = memberTypars ctx b.typarDefns

        // An implicit typar is drawn from a type position, never a `[<Measure>]` declaration.
        let implicit =
            match ClassMemberKind.ofMemberKind kind with
            | ClassMemberKind.Method -> [ for n in implicitMemberTypars ctx typarNames b -> n, TyparKind.Type ]
            | _ -> []

        // The count marks the leading `explicit` prefix of the seed: only those are
        // "declared-first"; the implicit ones order by appearance per the F# rule.
        {
            Name = mName
            Kind = kind
            IsStatic = isStatic
            IsOverride = isOverride
            Site = mSite
            SeedTypars = mkDeclaredTypars ctx.Store (explicit @ implicit)
            DeclaredTyparCount = List.length explicit
            ArgNames = Block.empty
        }

    /// The shape of an `abstract` signature. A slot declaration is never an override.
    let private abstractSlot
        (ctx: PassContext)
        (mName: string)
        (mTok: SyntaxToken)
        (tds: TyparDefns<SyntaxToken> voption)
        (isStatic: bool)
        (kind: TMemberKind)
        (argNames: Block<string voption>)
        : MemberShape =
        let explicit = memberTypars ctx tds

        {
            Name = mName
            Kind = kind
            IsStatic = isStatic
            IsOverride = false
            Site = NodeSite.ofToken NodeKind.PatIdent mTok
            SeedTypars = mkDeclaredTypars ctx.Store explicit
            DeclaredTyparCount = List.length explicit
            ArgNames = argNames
        }

    /// Add the members one `member` / `abstract` / `override` / `default` element declares,
    /// in source order: one, or one per accessor half.
    let private addMembersOfDefn
        (ctx: PassContext)
        (typarNames: string list)
        (acc: ResizeArray<TypeMemberInfo>)
        (staticTok: SyntaxToken voption)
        (kw: MemberKeyword<SyntaxToken>)
        (d: MethodOrPropDefn<SyntaxToken>)
        : unit =
        let add = addMember ctx acc
        let isStatic = staticTok.IsSome

        match kw with
        | MemberKeyword.Abstract(abstractToken = abstractTok) when isStatic ->
            ctx.Report(abstractTok, Kind.NotYetSupported "static abstract member")
        | _ -> ()

        let isOverride =
            match kw with
            | MemberKeyword.Override _
            | MemberKeyword.Default _ -> true
            | MemberKeyword.Member _
            | MemberKeyword.Abstract _ -> false

        let addNamed (b: Binding<SyntaxToken>) kind =
            match MemberNames.ofBinding ctx b with
            | ValueSome m -> add (bindingMember ctx typarNames m.Name m.Site b kind isStatic isOverride)
            | ValueNone -> ()

        match d with
        | MethodOrPropDefn.Method(defn = b) -> addNamed b TMemberKind.Method
        | MethodOrPropDefn.Property(defn = b) -> addNamed b TMemberKind.Property
        | MethodOrPropDefn.AutoProperty(ident = id) ->
            add
                {
                    Name = ctx.NameOf id
                    Kind = TMemberKind.Property
                    IsStatic = isStatic
                    IsOverride = isOverride
                    Site = NodeSite.ofToken NodeKind.PatIdent id
                    SeedTypars = Block.empty
                    DeclaredTyparCount = 0
                    ArgNames = Block.empty
                }
        | MethodOrPropDefn.PropertyWithGetSet(ident = propId; defns = defns) ->
            PropertyAccessors.reportNonAccessors ctx propId defns

            for a in PropertyAccessors.accessors ctx propId defns do
                add (bindingMember ctx typarNames a.Name a.Site a.Defn a.Kind isStatic isOverride)
        | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(
            ident = idOrOp; typarDefns = tds; sign = CurriedSig(args = sigArgs))) ->
            // An arg-less signature (`abstract member Current : int`, no `->`)
            // is an abstract *property*; a curried/function signature is a method.
            let kind =
                if sigArgs.IsEmpty then
                    TMemberKind.Property
                else
                    TMemberKind.Method

            match identOrOpNameTok ctx idOrOp with
            | ValueSome(mName, mTok) -> add (abstractSlot ctx mName mTok tds isStatic kind (sigArgNames ctx sigArgs))
            | ValueNone -> ()
        // `abstract P: T with get, set` declares the same halves an impl-side
        // `with get … and set …` does, each keyed on its own `get` / `set` token.
        | MethodOrPropDefn.AbstractSignature(MemberSig.PropSig(
            ident = idOrOp; typarDefns = tds; sign = CurriedSig(args = sigArgs); getSet = getSet)) ->
            match identOrOpNameTok ctx idOrOp with
            | ValueSome(propName, _) ->
                let halves = AccessorNames.halvesOf ctx.NameOf getSet

                match halves.Getter with
                | ValueSome tok ->
                    match sigArgs.Length with
                    | 0 -> add (abstractSlot ctx propName tok tds isStatic TMemberKind.Property Block.empty)
                    | _ ->
                        add (
                            abstractSlot
                                ctx
                                (AccessorNames.getterName propName)
                                tok
                                tds
                                isStatic
                                (TMemberKind.Accessor(propName, TAccessorRole.Getter))
                                (sigArgNames ctx sigArgs)
                        )
                | ValueNone -> ()

                match halves.Setter with
                | ValueSome tok ->
                    add (
                        abstractSlot
                            ctx
                            (AccessorNames.setterName propName)
                            tok
                            tds
                            isStatic
                            (TMemberKind.Accessor(propName, TAccessorRole.Setter))
                            (sigArgNames ctx sigArgs)
                    )
                | ValueNone -> ()
            | ValueNone -> ()

    /// An `interface IFace with member …` block. Each member is re-wrapped as a
    /// `TypeDefnElement.Member`; the interface type is kept as raw CST.
    let private interfaceImpl
        (ctx: PassContext)
        (typarNames: string list)
        (ifaceTok: SyntaxToken)
        (ifaceTyp: Type<SyntaxToken>)
        (objMembersOpt: ObjectMembers<SyntaxToken> voption)
        : ClassInterfaceImplInfo =
        let ifaceSite = NodeSite.ofToken NodeKind.TypeNamed ifaceTok

        let memberDefns =
            match objMembersOpt with
            | ValueSome(ObjectMembers(memberDefns = mds)) -> mds
            | ValueNone -> ImmutableArray.Empty

        let ifaceMembers = ResizeArray<TypeMemberInfo>()

        for md in memberDefns do
            match md with
            | MemberDefn.Member(attributes = attrs; staticToken = s; keyword = kw; defn = d) ->
                Attributes.declareMemberAttributes ctx attrs kw d
                addMembersOfDefn ctx typarNames ifaceMembers s kw d
            | MemberDefn.Value(ident = id) ->
                ctx.Report(id, Kind.Message "A field declaration is not permitted in an interface implementation")
            | MemberDefn.AdditionalConstructor(newToken = nt) ->
                ctx.Report(nt, Kind.Message "A constructor is not permitted in an interface implementation")

        let memberEls: TypeDefnElements<SyntaxToken> =
            ImmutableArray.CreateRange(seq { for md in memberDefns -> TypeDefnElement.Member md })

        ClassInterfaceImplInfo(ifaceTyp, ifaceMembers.ToArray(), memberEls, ifaceSite)

    /// `TypeBodyMembers` for a type body or augmentation. Unsupported elements are diagnosed
    /// at `declTok`.
    let extractTypeBody
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (typarNames: string list)
        (host: TypeBodyHost)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : TypeBodyMembers =
        let hasPrimaryCtor =
            match host with
            | TypeBodyHost.Class(hasPrimaryCtor = has) -> has
            | TypeBodyHost.Augmentation -> false

        let secondaryCtors = ResizeArray<ClassSecondaryCtorInfo>()
        let instanceFields = ResizeArray<ClassFieldInfo>()
        let members = ResizeArray<TypeMemberInfo>()
        let interfaceImpls = ResizeArray<ClassInterfaceImplInfo>()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(attributes = attrs; staticToken = s; keyword = kw; defn = d)) ->
                Attributes.declareMemberAttributes ctx attrs kw d
                addMembersOfDefn ctx typarNames members s kw d
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(newToken = nt; pat = pat; body = body)) ->
                match host with
                | TypeBodyHost.Class _ ->
                    let parms = ctorParamsOfPat ctx nt pat

                    secondaryCtors.Add(ClassSecondaryCtorInfo(NodeSite.ofToken NodeKind.PatIdent nt, parms, body))
                | TypeBodyHost.Augmentation ->
                    ctx.Report(nt, Kind.Message "Constructors cannot be defined for this type")
            // A `static val` (accepted by F#) is registered as an instance field; `staticToken` is unread.
            | TypeDefnElement.Member(MemberDefn.Value(mutableToken = mut; ident = id; typ = t)) ->
                match host with
                | TypeBodyHost.Class _ ->
                    instanceFields.Add(
                        ClassFieldInfo(
                            ctx.NameOf id,
                            translateType ctx t,
                            mut.IsSome,
                            NodeSite.ofToken NodeKind.DeclLetBinding id
                        )
                    )
                | TypeBodyHost.Augmentation ->
                    ctx.Report(id, Kind.Message "Explicit fields cannot be defined for this type")
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(
                interfaceToken = ifaceTok; typ = ifaceTyp; objectMembers = objMembersOpt)) ->
                interfaceImpls.Add(interfaceImpl ctx typarNames ifaceTok ifaceTyp objMembersOpt)
            | TypeDefnElement.InterfaceSpec _ ->
                // An `interface IFace` spec declares no members.
                ()
            | TypeDefnElement.Inherit _ -> ctx.Report(declTok, Kind.NotYetSupported "inheritance")

        {
            HasPrimaryCtor = hasPrimaryCtor
            SecondaryCtors = secondaryCtors.ToArray()
            InstanceFields = instanceFields.ToArray()
            Members = members.ToArray()
            InterfaceImpls = interfaceImpls.ToArray()
        }
