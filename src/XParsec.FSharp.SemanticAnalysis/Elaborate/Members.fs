namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr

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
    let memberParams (ctx: PassContext) (b: Binding<SyntaxToken>) : EqArray<BoundVarKey * SemType> =
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
                    | ValueSome boundVar -> yield (boundVar, TastWalk.patTy tp)
                    | ValueNone -> ()
            }

        EqArray.ofSeq (
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

    let tMemberKindOf (kind: ClassMemberKind) : TMemberKind =
        match kind with
        | ClassMemberKind.Method -> TMemberKind.Method
        | ClassMemberKind.Property -> TMemberKind.Property

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
                    Kind = tMemberKindOf a.MemberKind
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

    /// What the DECLARING type contributes to every member built for it: the `this` / `base`
    /// bound variables it synthesises, the self type they carry, the rewrite its own fields
    /// need in a member body, and the member's own generic params.
    [<NoEquality; NoComparison>]
    type DeclaringType =
        {
            ThisKey: BoundVarKey
            ThisTy: SemType
            /// `ValueNone` for a type with no `inherit`; a static member drops it regardless.
            BaseKey: BoundVarKey voption
            LowerBody: MemberSite -> Expr<SyntaxToken> -> TExpr
            MethodTypeParams: MemberSite -> EqArray<string * SemType>
        }

    /// Translate one member element into its `TTypeMember`s. The declaring type supplies
    /// everything a class has and a union / record does not, so both hosts share this walk.
    let translateMemberElement
        (ctx: PassContext)
        (declaring: DeclaringType)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember[] =
        match el with
        | TypeDefnElement.Member(MemberDefn.Member(
            staticToken = s; keyword = kw; inlineToken = inlineTok; access = memberAccess; defn = d)) ->
            let isStatic = s.IsSome
            // Member-level accessibility rides `MemberDefn.Member.access` (`member private
            // this.M`), NOT the inner `Binding.access`, always `ValueNone` for a member.
            let memberAccessibility = accessibilityOfToken memberAccess

            memberDecls ctx d
            |> Array.map (fun decl ->
                let site =
                    {
                        Name = decl.Name
                        Kind = decl.Kind
                        DeclKey = decl.DeclKey
                        IsStatic = isStatic
                    }

                {
                    Name = decl.Name
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
                        | ValueNone -> EqArray.empty
                    Body = declaring.LowerBody site decl.Body
                    ReturnTy = typeOfKey ctx (CstKeys.ofExpr decl.Body)
                    MethodTypeParams =
                        match decl.Defn with
                        // An auto-property never carries its own generic params.
                        | ValueNone -> EqArray.empty
                        | ValueSome _ -> declaring.MethodTypeParams site
                }
            )
        | _ -> [||]

    /// Unions and records carry no primary-ctor params, so a field reference in a member is
    /// already an explicit `this.N` and needs no body rewrite; nor are they inheritable, so
    /// `base` is never in scope and a member is never generic.
    let private nominalDeclaringType (ctx: PassContext) (host: IInterfaceImplHost) : DeclaringType =
        {
            ThisKey = host.ThisKey
            ThisTy = host.MkSelfType EqArray.empty
            BaseKey = ValueNone
            LowerBody = fun _ e -> translateExpr ctx e
            MethodTypeParams = fun _ -> EqArray.empty
        }

    /// Surface a union/record host's augmentation members and its resolved `interface …
    /// with` impl bodies as the `(members, interfaces)` pair. Impls whose interface failed
    /// to resolve are dropped, because the "is not an interface" diagnostic already fired.
    let elaborateHostMembers
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        (elaborateOne: TTypeMember -> TTypeMember)
        : EqArray<TTypeMember> * EqArray<SemType * EqArray<TTypeMember>> =
        let declaring = nominalDeclaringType ctx host

        let translate (els: TypeDefnElements<SyntaxToken>) : EqArray<TTypeMember> =
            EqArray.ofSeq (
                seq {
                    for el in els do
                        for m in translateMemberElement ctx declaring el do
                            yield elaborateOne m
                }
            )

        let members =
            match ext with
            | ValueNone -> EqArray.empty
            | ValueSome(TypeExtensionElements(elements = elems)) -> translate elems

        let interfaces =
            EqArray.ofSeq (
                seq {
                    for impl in host.InterfaceImpls do
                        match impl.Resolved with
                        | ValueSome ifaceTy -> yield (ifaceTy, translate impl.Elements)
                        | ValueNone -> ()
                }
            )

        members, interfaces
