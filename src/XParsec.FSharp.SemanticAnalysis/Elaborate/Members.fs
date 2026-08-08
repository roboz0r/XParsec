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

    /// An unmarked declaration is `Public` — the F# default.
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

    /// Member name from a member binding's `pattern` (`member this.M …` parses
    /// the member name as the bound pattern's ident).
    let memberNameOfBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : string voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id)
            // `member (=)` compiles to `op_Equality`, which is how a use site names it.
            | Pat.Op io -> Desugar.opPatCompiledName ctx.NameOf io
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.pattern

    /// The member's declaration `NodeKey`, minted off the same named pattern that
    /// member registration keys `TypeMemberInfo.DeclSite` from. It carries the member's
    /// source offset, so same-name overloads sharing name + kind + static-ness differ here.
    let memberKeyOfBinding (b: Binding<SyntaxToken>) : NodeKey voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple _
            | Pat.Op _ -> ValueSome(CstKeys.ofPat p)
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.pattern

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
                // A component that binds nothing — a wildcard, a nested destructuring —
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

    /// Translate one union/record augmentation member element into a `TTypeMember`. Unions
    /// and records carry no primary-ctor params, so no ctor-param → `FieldGet` body rewrite
    /// is needed here: a field reference in such a member is already an explicit `this.N`.
    let private translateNominalMember
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        // Unions/records are not inheritable — `base` never in scope.
        let selfTy = host.MkSelfType EqArray.empty

        match el with
        | TypeDefnElement.Member(MemberDefn.Member(
            staticToken = s; keyword = kw; inlineToken = inlineTok; access = memberAccess; defn = d)) ->
            let isStatic = s.IsSome
            let isOverride = isOverrideKeyword kw
            let isInline = inlineTok.IsSome
            // Member-level accessibility rides `MemberDefn.Member.access` (`member private
            // this.M`), NOT the inner `Binding.access`, always `ValueNone` for a member.
            let memberAccessibility = accessibilityOfToken memberAccess

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Accessibility = memberAccessibility
                            IsInline = isInline
                            Kind = kind
                            IsOverride = isOverride
                            ThisKey = (if isStatic then ValueNone else ValueSome host.ThisKey)
                            BaseKey = ValueNone
                            ThisTy = selfTy
                            Params = memberParams ctx b
                            Body = translateExpr ctx b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                            // A member's own generic parameters are class-only.
                            MethodTypeParams = EqArray.empty
                        }
                | ValueNone -> ValueNone

            match d with
            | MethodOrPropDefn.Method(defn = b) -> build TMemberKind.Method b
            | MethodOrPropDefn.Property(defn = b) -> build TMemberKind.Property b
            | MethodOrPropDefn.AutoProperty(access = acc; ident = id; expr = e) ->
                ValueSome
                    {
                        Name = ctx.NameOf id
                        IsStatic = isStatic
                        Accessibility = autoPropertyAccess memberAccessibility acc
                        IsInline = isInline
                        Kind = TMemberKind.Property
                        IsOverride = isOverride
                        ThisKey = (if isStatic then ValueNone else ValueSome host.ThisKey)
                        BaseKey = ValueNone
                        ThisTy = selfTy
                        Params = EqArray.empty
                        Body = translateExpr ctx e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        MethodTypeParams = EqArray.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Surface a union/record host's augmentation members and its resolved `interface …
    /// with` impl bodies as the `(members, interfaces)` pair. Impls whose interface failed
    /// to resolve are dropped — the "is not an interface" diagnostic already fired.
    let elaborateHostMembers
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        (elaborateOne: TTypeMember -> TTypeMember)
        : EqArray<TTypeMember> * EqArray<SemType * EqArray<TTypeMember>> =
        let translate (els: TypeDefnElements<SyntaxToken>) : EqArray<TTypeMember> =
            EqArray.ofSeq (
                seq {
                    for el in els do
                        match translateNominalMember ctx host el with
                        | ValueSome m -> yield elaborateOne m
                        | ValueNone -> ()
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
