namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr

// Member surfacing for the Elaborate pass: declared accessibility, the name / key /
// parameter projections a member binding contributes, and the union / record
// augmentation + `interface … with` bodies an `IInterfaceImplHost` carries. The class
// host's own members lower in ElaborateClassMembers, which needs the field rewrites.

module internal ElaborateMembers =

    /// Classify a CST accessibility keyword token (`private` / `internal` /
    /// `public`, or its absence) into the token-free `Accessibility`. An unmarked
    /// declaration is `Public` — the F# default. The impl-side CST carries the bare
    /// keyword token (`SyntaxToken voption`); its `.Token` discriminates.
    let accessibilityOfToken (tok: SyntaxToken voption) : Accessibility =
        match tok with
        | ValueSome t when t.Token = Token.KWPrivate -> Accessibility.Private
        | ValueSome t when t.Token = Token.KWInternal -> Accessibility.Internal
        | _ -> Accessibility.Public

    /// An auto-property carries its OWN access token (`member val private X = …`) in
    /// addition to the enclosing member-level one; the property's own modifier wins
    /// when present, otherwise it inherits the member-level accessibility.
    let autoPropertyAccess (memberLevel: Accessibility) (propToken: SyntaxToken voption) : Accessibility =
        match accessibilityOfToken propToken with
        | Accessibility.Public -> memberLevel
        | own -> own

    /// Member name from a member binding's `headPat` (`member this.M …` parses
    /// the member name as the head pattern's ident).
    let memberNameOfBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : string voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id)
            // Operator-named binding head: surface the operator's compiled name
            // (`(=)` → `op_Equality`) so the member is addressable from a use
            // site's desugared `External(op_Equality)` head.
            | Pat.Op io -> Desugar.opPatCompiledName ctx.NameOf io
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    /// The member's declaration `NodeKey` — `CstKeys.ofPat` of the same name-head
    /// pattern `MemberRegistration.memberNameOf` keys the `TypeMemberInfo.DeclSite`
    /// from. Unique per declared member (it carries the member's source offset), so
    /// it disambiguates *same-name overloads* that share a name + kind + static-ness
    /// — which a name-only `Array.tryFind` cannot. Used to recover the *right*
    /// overload's `MethodTypeParams` (without it every `Fmt` overload took
    /// the first one's typars, so the others' own `'T` was never generalised and
    /// froze ungrounded).
    let memberKeyOfBinding (b: Binding<SyntaxToken>) : NodeKey voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple _
            | Pat.Op _ -> ValueSome(CstKeys.ofPat p)
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    /// Member parameter list as `(bindingKey, ty)` pairs in declaration order
    /// (`this` is separate). The binding key is the same one `translatePat` mints,
    /// so a `Var` reference in the body resolves to it.
    ///
    /// A tupled member (`M(a, b)`) is *one* `argumentPats` entry that translates to
    /// a `TPat.Tuple`; F# compiles it to a .NET method with one parameter per tuple
    /// component (not an actual `Tuple<_,_>`), so we flatten the tuple to one
    /// `(key, ty)` per component. The sequential order lines up with both
    /// `Emit.buildMember`'s `args.[k] <- baseIdx + i` slots and the emitted method
    /// signature. Curried members (`M a b`) appear as multiple `argumentPats`
    /// entries and compose with the flatten. Non-simple components (wildcards,
    /// nested destructuring) bind nothing and are dropped.
    let memberParams (ctx: PassContext) (b: Binding<SyntaxToken>) : EqArray<BinderKey * SemType> =
        let rec flatten (tp: TPat) =
            seq {
                match tp with
                | TPat.Tuple(items, _, _) ->
                    for it in items do
                        yield! flatten it
                // A parameter's slot is a definition site, so it is taken with the
                // projection that answers for a pattern; a component that binds nothing
                // (a wildcard, a nested destructuring) yields none and is dropped. Its
                // spelling was recorded when `translatePat` built the pattern.
                | _ ->
                    match BinderKey.ofPat tp with
                    | ValueSome binder -> yield (binder, TastWalk.patTy tp)
                    | ValueNone -> ()
            }

        EqArray.ofSeq (
            seq {
                for p in b.argumentPats do
                    yield! flatten (translatePat ctx p)
            }
        )

    /// `override`/`default` ⇒ the member overrides a base virtual slot (Object's
    /// `Equals`/`GetHashCode`/`ToString` for an `inherit`-less class); `member`/
    /// `abstract` do not. Drives virtual emission + the skip-generalise / Object-slot
    /// conformance passes via `TTypeMember.IsOverride`.
    let isOverrideKeyword (kw: MemberKeyword<SyntaxToken>) : bool =
        match kw with
        | MemberKeyword.Override _
        | MemberKeyword.Default _ -> true
        | MemberKeyword.Member _
        | MemberKeyword.Abstract _ -> false

    /// Translate one union/record augmentation member element into a `TTypeMember`.
    /// Instance members reference `this` via `host.ThisKey`; `ThisTy` is the host's
    /// own monomorphic Self (`TyUnion`/`TyRecord` via `MkSelfType`), remapped to
    /// declaring typars later by the caller's `elaborateOne`. Neither unions nor
    /// records carry primary-ctor params, so (unlike `translateClassMember`) no
    /// ctor-param → `FieldGet` rewrite is needed — a field reference is already an
    /// explicit `this.N`. Generic methods on such augmentations are out of scope
    /// (class-only), so `MethodTypeParams` is always empty here.
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
            // Member-level accessibility rides `MemberDefn.Member.access` (`member
            // private this.M`), NOT the inner `Binding.access` (always `ValueNone` for a
            // member). An auto-property's own `member val private X` access takes
            // precedence over the member-level one via `autoPropertyAccess`.
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
                            // Generic methods on union augmentations are out of
                            // B-12 scope (class-only); always non-generic here.
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

    /// Surface a union/record host's augmentation members and resolved `interface …
    /// with` impl bodies as the `(members, interfaces)` pair carried by `TTypeKind`.
    /// Each member/impl-body is translated through `translateNominalMember` then run
    /// through `elaborateOne` (the caller's generic self-type remapper). Impls whose
    /// interface failed to resolve are dropped (that diagnostic already fired).
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
