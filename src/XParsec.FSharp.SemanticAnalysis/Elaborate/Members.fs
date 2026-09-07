namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr
open XParsec.FSharp.SemanticAnalysis.ElaborateTypars

// Member surfacing for the Elaborate pass: declared accessibility, the name / key /
// parameter projections a member binding contributes, and the union / record
// augmentation + `interface … with` bodies an `IInterfaceImplHost` carries.

module internal ElaborateMembers =

    /// An unmarked declaration is `Public`, the F# default.
    let accessibilityOfToken (tok: SyntaxToken voption) : Accessibility =
        match tok with
        | ValueSome t when t.Token = Token.KWPrivate -> Accessibility.Private
        | ValueSome t when t.Token = Token.KWInternal -> Accessibility.Internal
        | _ -> Accessibility.Public

    /// An auto-property carries its OWN access token (`member val private X = …`) as well
    /// as the enclosing member-level one; the property's own modifier wins when present.
    let autoPropertyAccess (memberLevel: Accessibility) (propToken: SyntaxToken voption) : Accessibility =
        match accessibilityOfToken propToken with
        | Accessibility.Public -> memberLevel
        | own -> own

    /// Member parameter list as `(bindingKey, ty)` pairs in declaration order (`this` is
    /// separate). A tupled member `M(a, b)` is ONE `argumentPats` entry, but F# compiles it
    /// to one .NET parameter per tuple component, so the tuple flattens to one pair each.
    let memberParams (ctx: PassContext) (b: Binding<SyntaxToken>) : Block<BoundVarKey * SemType> =
        let rec flatten (tp: TPat) =
            seq {
                match tp with
                | TPat.Tuple(items, _, _) ->
                    for it in items do
                        yield! flatten it
                // A component that binds nothing (a wildcard, a nested destructuring)
                // yields no pair and so occupies no parameter slot.
                | _ ->
                    match BoundVarKey.ofPat tp with
                    | ValueSome boundVar -> yield (boundVar, TPatG.ty tp)
                    | ValueNone -> ()
            }

        Block.ofSeq (
            seq {
                for p in b.argumentPats do
                    yield! flatten (translatePat ctx p)
            }
        )

    /// `override`/`default` ⇒ the member overrides a base virtual slot (for an
    /// `inherit`-less class, one of Object's `Equals`/`GetHashCode`/`ToString`);
    /// `member`/`abstract` do not.
    let isOverrideKeyword (kw: MemberKeyword<SyntaxToken>) : bool =
        match kw with
        | MemberKeyword.Override _
        | MemberKeyword.Default _ -> true
        | MemberKeyword.Member _
        | MemberKeyword.Abstract _ -> false

    /// One `TTypeMember` a member element declares. A `with get`/`set` clause declares one per
    /// accessor; every other member form declares exactly one.
    [<NoEquality; NoComparison>]
    type MemberDecl =
        {
            Name: string
            Kind: TMemberKind
            /// The registration `DeclSite` key same-name overloads differ at; `ValueNone` for
            /// an auto-property, which never carries generic params to look up.
            DeclKey: NodeKey voption
            /// `ValueNone` for an auto-property, whose body is an initialiser and whose
            /// parameter list is therefore empty.
            Defn: Binding<SyntaxToken> voption
            /// `member val private X = …` carries its own modifier; the enclosing member's
            /// otherwise.
            OwnAccess: SyntaxToken voption
            Body: Expr<SyntaxToken>
        }

    /// The members one `MethodOrPropDefn` declares. An abstract slot declares none: it has no
    /// body to translate.
    let memberDecls (ctx: PassContext) (d: MethodOrPropDefn<SyntaxToken>) : MemberDecl[] =
        let ofBinding (kind: TMemberKind) (b: Binding<SyntaxToken>) =
            match MemberNames.ofBinding ctx b with
            | ValueSome m ->
                [|
                    {
                        Name = m.Name
                        Kind = kind
                        DeclKey = ValueSome m.Site.Key
                        Defn = ValueSome b
                        OwnAccess = ValueNone
                        Body = b.expr
                    }
                |]
            | ValueNone -> [||]

        match d with
        | MethodOrPropDefn.Method(defn = b) -> ofBinding TMemberKind.Method b
        | MethodOrPropDefn.Property(defn = b) -> ofBinding TMemberKind.Property b
        | MethodOrPropDefn.PropertyWithGetSet(ident = propId; defns = defns) ->
            PropertyAccessors.accessors ctx propId defns
            |> Array.map (fun a ->
                {
                    Name = a.Name
                    Kind = a.Kind
                    DeclKey = ValueSome a.Site.Key
                    Defn = ValueSome a.Defn
                    OwnAccess = ValueNone
                    Body = a.Defn.expr
                }
            )
        | MethodOrPropDefn.AutoProperty(access = acc; ident = id; expr = e) ->
            [|
                {
                    Name = ctx.NameOf id
                    Kind = TMemberKind.Property
                    DeclKey = ValueNone
                    Defn = ValueNone
                    OwnAccess = acc
                    Body = e
                }
            |]
        | MethodOrPropDefn.AbstractSignature _ -> [||]

    /// What a declaring type's hooks key on: a body rewrite differs by static-ness, a typar
    /// lookup by the exact overload.
    [<NoEquality; NoComparison>]
    type MemberSite =
        {
            Name: string
            Kind: TMemberKind
            DeclKey: NodeKey voption
            IsStatic: bool
        }

    /// The declaring type's contribution to every member elaborated within it.
    [<NoEquality; NoComparison>]
    type DeclaringType =
        {
            TypeKey: TypeKey
            TypeParams: Block<DeclaredTypar>
            /// Every member the type registered, its `interface … with` impl members included.
            Members: TypeMemberInfo seq
            ThisKey: BoundVarKey
            ThisTy: SemType
            /// `ValueNone` for a type with no `inherit`; a static member drops it regardless.
            BaseKey: BoundVarKey voption
            LowerBody: MemberSite -> Expr<SyntaxToken> -> TExpr
        }

    /// The registered info of `site`: by its registration `DeclKey` where it has one, because
    /// same-name overloads share `Name` / `Kind` / `IsStatic`; by those three otherwise.
    let memberInfoOf (members: TypeMemberInfo seq) (site: MemberSite) : TypeMemberInfo option =
        let byKey =
            match site.DeclKey with
            | ValueSome k -> members |> Seq.tryFind (fun mi -> mi.DeclSite.Key = k)
            | ValueNone -> None

        byKey
        |> Option.orElseWith (fun () ->
            members
            |> Seq.tryFind (fun mi -> mi.Name = site.Name && mi.IsStatic = site.IsStatic && mi.Kind = site.Kind)
        )

    /// Every member a host registered, its `interface … with` impl members included.
    let hostMembers (members: TypeMemberInfo[]) (impls: ClassInterfaceImplInfo[]) : TypeMemberInfo seq =
        seq {
            yield! members

            for impl in impls do
                yield! impl.Members
        }

    /// `site`'s registered info. Fails when the registration walk did not mint the member.
    let private registeredMemberOf (declaring: DeclaringType) (site: MemberSite) : TypeMemberInfo =
        match memberInfoOf declaring.Members site with
        | Some mi -> mi
        | None -> failwithf "ElaborateMembers.registeredMemberOf: member '%s' was not registered" site.Name

    /// The member's own generic parameters in the registered `CanonicalTypars` order, each
    /// with its constraints, and the freeze markers pairing each typar's root with
    /// `TyTypar(Member _, i)`.
    let private methodTyparsOf
        (ctx: PassContext)
        (declaring: DeclaringType)
        (mi: TypeMemberInfo)
        : TyparListG<SemType> * (TyVarId * SemType) list =
        // Each root advances to its union-find SURVIVOR, which generalise keyed the body's
        // frozen typar markers on. A root linked to a concrete type is dropped.
        let roots =
            mi.CanonicalTypars
            |> GeneralizedTypars.refreshRoots (fun tv ->
                match Unification.zonk ctx.Store (TyVar tv) with
                | TyVar r -> ValueSome r
                | _ -> ValueNone
            )

        methodTyparList ctx.Store roots, GeneralizedTypars.methodEnv (TyparScope.Member declaring.TypeKey) roots

    /// Translate one member element into its `TTypeMember`s, adding each generic member's own
    /// typar markers to `env`, the decl's freeze env.
    let translateMemberElement
        (ctx: PassContext)
        (declaring: DeclaringType)
        (env: ResizeArray<TyVarId * SemType>)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember[] =
        match el with
        | TypeDefnElement.Member(MemberDefn.Member(
            attributes = memberAttrs
            staticToken = s
            keyword = kw
            inlineToken = inlineTok
            access = memberAccess
            defn = d)) ->
            let isStatic = s.IsSome
            // Member-level accessibility is carried on `MemberDefn.Member.access` (`member private
            // this.M`), NOT the inner `Binding.access`, always `ValueNone` for a member.
            let memberAccessibility = accessibilityOfToken memberAccess
            let decls = memberDecls ctx d

            // Every decl a `PropertyWithGetSet` yields accesses the one property, so the first
            // decl's kind is the element's. `AbstractSignature` yields none: no element to check.
            let usedOn =
                match decls with
                | [||] -> AttrTarget.Unchecked
                | ds ->
                    match TMemberKind.propertyOf ds.[0].Name ds.[0].Kind with
                    | ValueSome _ -> AttrTarget.Property
                    | ValueNone -> AttrTarget.Method

            let attributes = AttributeFold.resolveAndBuild ctx usedOn memberAttrs

            decls
            |> Array.map (fun decl ->
                let site =
                    {
                        Name = decl.Name
                        Kind = decl.Kind
                        DeclKey = decl.DeclKey
                        IsStatic = isStatic
                    }

                let mi = registeredMemberOf declaring site

                let methodTypars =
                    match decl.Defn with
                    // `ValueNone` is an auto-property; only a `Defn`-backed member can be generic.
                    | ValueNone -> TyparList.empty
                    | ValueSome _ ->
                        let typars, markers = methodTyparsOf ctx declaring mi
                        env.AddRange markers
                        typars

                {
                    Name = decl.Name
                    Key =
                        UnificationInferOverload.frozenUserMemberKey
                            ctx.Store
                            declaring.TypeKey
                            declaring.TypeParams
                            mi
                    IsStatic = isStatic
                    Accessibility = autoPropertyAccess memberAccessibility decl.OwnAccess
                    IsInline = inlineTok.IsSome
                    Kind = decl.Kind
                    IsOverride = isOverrideKeyword kw
                    ThisKey = (if isStatic then ValueNone else ValueSome declaring.ThisKey)
                    BaseKey = (if isStatic then ValueNone else declaring.BaseKey)
                    ThisTy = declaring.ThisTy
                    Params =
                        match decl.Defn with
                        | ValueSome b -> memberParams ctx b
                        | ValueNone -> Block.empty
                    Body = declaring.LowerBody site decl.Body
                    ReturnTy = typeOfKey ctx (CstKeys.ofExpr decl.Body)
                    MethodTypars = methodTypars
                    Attributes = attributes
                }
            )
        | _ -> [||]

    /// The `DeclaringType` of a union or record host: a field reference in a member body is
    /// already an explicit `this.N`, so the body passes through unrewritten, and `base` is
    /// out of scope.
    let private nominalDeclaringType (ctx: PassContext) (host: IInterfaceImplHost) : DeclaringType =
        {
            TypeKey = host.TypeKey
            TypeParams = host.TypeParams
            Members = hostMembers host.Members host.InterfaceImpls
            ThisKey = host.ThisKey
            ThisTy = host.MkSelfType(declTyparArgs ctx.Store host.TypeParams)
            BaseKey = ValueNone
            LowerBody = fun _ e -> translateExpr ctx e
        }

    /// Surface a union/record host's augmentation members and its resolved `interface …
    /// with` impl bodies as the `(members, interfaces)` pair. Impls whose interface failed
    /// to resolve are dropped, because the "is not an interface" diagnostic already fired.
    let elaborateHostMembers
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        (env: ResizeArray<TyVarId * SemType>)
        : Block<TTypeMember> * Block<SemType * Block<TTypeMember>> =
        let declaring = nominalDeclaringType ctx host

        let translate (els: TypeDefnElements<SyntaxToken>) : Block<TTypeMember> =
            Block.ofSeq (
                seq {
                    for el in els do
                        yield! translateMemberElement ctx declaring env el
                }
            )

        let members =
            match ext with
            | ValueNone -> Block.empty
            | ValueSome(TypeExtensionElements(elements = elems)) -> translate elems

        let interfaces =
            Block.ofSeq (
                seq {
                    for impl in host.InterfaceImpls do
                        match InterfaceImplResolution.tryIface impl.Resolution with
                        | ValueSome ifaceTy -> yield (ifaceTy, translate impl.Elements)
                        | ValueNone -> ()
                }
            )

        members, interfaces
