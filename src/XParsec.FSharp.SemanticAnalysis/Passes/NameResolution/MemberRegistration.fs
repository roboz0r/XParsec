namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationTranslate
open NameResolutionTypeHeadStamp
open NameResolutionScope
open NameResolutionTypeRegistration

// Registry stamping for class type definitions (ctor params, `val` fields, members,
// static lets) and union augmentation members, plus the group registration algorithm
// every kind's detail runs under.
//
// A class's declared STRUCTURE — ctor-parameter annotations, `val` field types, the
// `inherit` parent — resolves here, against the types in scope where it is written. A
// member's TYPE is not structure: it is inferred from its body, so its placeholder TyVar is
// linked by Unification's `fillClassMembers`.

module NameResolutionMemberRegistration =

    /// Constructor parameter info from a parameter *pattern* (the primary ctor's
    /// `PrimaryConstrArgs.pat` or a secondary ctor's `new(...)` pattern). v1
    /// accepts only simple patterns (`NamedSimple`, `Typed (NamedSimple, t)`,
    /// `Tuple` of those, possibly enclosed, and `()` for no params); anything
    /// else diagnoses and contributes nothing.
    ///
    /// A parameter's type is a TyVar in BOTH shapes, because it is the parameter's
    /// binding-site inference cell (`fillClassMembers` seeds `ctx.Bindings.TypeVar` from
    /// it, so a member body's reference to the parameter types through it). An ANNOTATED
    /// parameter's cell is linked to the declared type right here — under the class's typar
    /// scope, against the types claimed at this point — so the annotation resolves in the
    /// declaration's own scope; an unannotated one stays free for the use site to pin.
    /// Called under `underClassTyparScope`.
    let private ctorParamsOfPat (ctx: PassContext) (declKey: NodeKey) (p: Pat<SyntaxToken>) : ClassCtorParamInfo[] =
        let results = ResizeArray<ClassCtorParamInfo>()

        // The parameter's binding site is the pattern's own — the key a body's reference to
        // it resolves through — so it is taken with the projection that answers for a
        // pattern rather than off the identifier token.
        let addParam (p: Pat<SyntaxToken>) (id: SyntaxToken) (annotation: Type<SyntaxToken> voption) =
            match BinderKey.ofCstPat p with
            | ValueNone -> () // unreachable: every arm below hands a (wrapped) `NamedSimple`
            | ValueSome binder ->
                let tv = ctx.NewTypeVar()
                ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)

                match annotation with
                | ValueSome t -> ctx.Store.SetLink(UnionFind.find ctx.Store tv, ValueSome(translateType ctx t))
                | ValueNone -> ()

                results.Add(ClassCtorParamInfo(ctx.NameOf id, TyVar tv, binder))

        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.EmptyBlock _ -> () // `new()` / `C()` — no parameters
            | Pat.NamedSimple id -> addParam p id ValueNone
            | Pat.Typed(pat = Pat.NamedSimple id; typ = t) -> addParam p id (ValueSome t)
            | Pat.EnclosedBlock(pat = inner) -> walk inner
            | Pat.Tuple(patterns = pats) ->
                for sub in pats do
                    walk sub
            | _ ->
                // Point at the offending sub-pattern when keyable; else declKey.
                let patKey =
                    try
                        CstKeys.ofPat p
                    with _ ->
                        declKey

                ctx.Diagnostics.Add
                    {
                        Key = patKey
                        Message =
                            "Constructor argument patterns must be simple identifiers (with optional type annotation) in v1"
                        Code = ""
                        Severity = Severity.Error
                    }

        walk p
        results.ToArray()

    let private extractCtorParams
        (ctx: PassContext)
        (declKey: NodeKey)
        (pcOpt: PrimaryConstrArgs<SyntaxToken> voption)
        : ClassCtorParamInfo[] =
        match pcOpt with
        | ValueNone -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueNone)) -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) -> ctorParamsOfPat ctx declKey p

    /// `ClassSecondaryCtorInfo` for a class body's `new(...)` overloads. Each overload's
    /// params resolve exactly like the primary ctor's (`ctorParamsOfPat`); the synthetic
    /// `DeclKey` keys it from the `new` token so distinct overloads don't collide.
    let private extractSecondaryCtors
        (ctx: PassContext)
        (declKey: NodeKey)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : ClassSecondaryCtorInfo[] =
        let acc = ResizeArray<ClassSecondaryCtorInfo>()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(newToken = nt; pat = pat; body = body)) ->
                let ctorKey = NodeKey.ofToken nt NodeKind.PatIdent
                let parms = ctorParamsOfPat ctx ctorKey pat
                acc.Add(ClassSecondaryCtorInfo(ctorKey, parms, body))
            | _ -> ()

        acc.ToArray()

    /// A member's name + the node key its body is inferred under, from its head
    /// pattern. `this.M`-shaped heads parse as `Pat.NamedSimple` for the
    /// member-name token; the `this`/alias is in `MethodOrPropDefn`'s `ident`
    /// field, not the head pattern. The key is `CstKeys.ofPat` of the *leaf*
    /// pattern — the exact key Unification's `inferBinding` links the inferred
    /// signature under — so a `Pat.Op` head (`static member (+) (a, b) = …`) keys
    /// on `(lParen, PatOp)`, not `(opToken, PatIdent)`; otherwise the registered
    /// `TypeMemberInfo.Type` placeholder never receives the body type (and SRTP /
    /// member dispatch read it back as a free TyVar). For `Pat.NamedSimple` this
    /// equals the old `(id, PatIdent)` key, so named members are unaffected.
    let private memberNameOf (ctx: PassContext) (b: Binding<SyntaxToken>) : (string * NodeKey) voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id, CstKeys.ofPat p)
            // Operator-named member head: register under the operator's compiled
            // name (`op_Addition`) so a use site's desugared `op_*` head finds it.
            | Pat.Op io ->
                match Desugar.opPatCompiledName ctx.NameOf io with
                | ValueSome n -> ValueSome(n, CstKeys.ofPat p)
                | ValueNone -> ValueNone
            | Pat.EnclosedBlock(pat = inner) -> walk inner
            | Pat.Typed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    let private identOrOpNameTok (ctx: PassContext) (id: IdentOrOp<SyntaxToken>) : (string * SyntaxToken) voption =
        match id with
        | IdentOrOp.Ident t -> ValueSome(ctx.NameOf t, t)
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ValueSome(ctx.NameOf op, op)
        | _ -> ValueNone

    /// A member's own declared typars — the `<'C, …>` after the member name, in
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

    /// Free typar names in a member's *signature* (argument-pattern annotations
    /// then return type, source order) that are neither an enclosing-type typar
    /// nor one of the member's own explicit `<'C>` typars — the *implicit*
    /// member-level generic params. In real F# `member s.Map f : Set<'U>` /
    /// `s.PartitionWith(p: 'T -> Choice<'T1,'T2>)` generalise `'U` / `'T1`,`'T2`
    /// as method generic parameters; registering them here lets the machinery
    /// (inference scope seed + Elaborate `GenericMethodParameters`) carry them through
    /// rather than the strict member scope diagnosing them as free. No off-the-shelf
    /// free-typar walker over `Type<SyntaxToken>` exists at this layer, so this
    /// small one walks only the structural cases that can carry a typar.
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

        // Free typar collection reuses `CstWalk.iterType`'s Type recursion — the
        // single enumeration of the 18 `Type` cases — so this consumer supplies only
        // the leaf action. `VarType` and a `SubtypeConstraint`'s constrained typar are
        // the two typar-bearing heads. A `WhenConstrainedType`'s `when`-clause
        // constraint types are deliberately NOT descended: an implicit method typar is
        // drawn from the signature's arg/return SHAPE, not from a constraint target
        // (preserving the reach of the hand-walk this replaced).
        let typarIter: CstWalk.TypeIter =
            { CstWalk.identityTypeIter with
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
                            // Descend only the constrained type; returning false
                            // suppresses the default recursion that would also visit
                            // the `when` constraints (see note above).
                            CstWalk.iterType it inner
                            false
                        | _ -> true
            }

        let walkTy (t: Type<SyntaxToken>) = CstWalk.iterType typarIter t

        // Only a `(p : T)` annotation contributes a signature type; an unannotated
        // binder carries no typar.
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

    /// `TypeMemberInfo` placeholders for a type body's / augmentation's member
    /// elements. Shared by class registration (`body.elements`) and union
    /// augmentation (`extensions.elements`). Unsupported element kinds emit a
    /// diagnostic at `declKey` — each arm is named so individual diagnostics can
    /// be lifted in isolation as features land.
    let extractMembers
        (ctx: PassContext)
        (declKey: NodeKey)
        (classTypars: string list)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : TypeMemberInfo[] =
        let memberInfos = ResizeArray<TypeMemberInfo>()

        let diagnose msg =
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message = msg
                    Code = ""
                    Severity = Severity.Error
                }

        // The seed typars + declared prefix are fixed here, at construction, and never
        // change (immutable on `TypeMemberInfo`); only generalisation later mutates a
        // member, via its write-once canonical cell.
        let addMember
            mName
            kind
            isStatic
            isOverride
            (mKey: NodeKey)
            (seed: EqArray<string * TyVarId>)
            declaredCount
            : TypeMemberInfo =
            let tv = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)
            let cmi = TypeMemberInfo(mName, kind, isStatic, TyVar tv, mKey, seed, declaredCount)
            cmi.IsOverride <- isOverride
            memberInfos.Add cmi
            cmi

        let registerNamed (b: Binding<SyntaxToken>) kind isStatic isOverride =
            match memberNameOf ctx b with
            | ValueSome(mName, mKey) ->
                // A concrete generic method (`member this.Map<'C> …`) carries
                // its own typars on the binding's `typarDefns`. Stamp prototype
                // TyVars so Unification scopes the signature against them and Elaborate
                // surfaces them as GenericMethodParameters — mirroring the abstract
                // path. A property's `typarDefns` is absent ⇒ empty.
                //
                // Then append the member's *implicit* signature typars — a
                // `'U` that appears only in a param/return annotation, never as a
                // class typar or explicit `<'a>`. F# generalises these as method
                // generic params; without registration the strict member scope
                // diagnoses them as free. Only methods can introduce them (a
                // property can't be generic), so skip the property kind.
                let explicit = memberTyparNames ctx b.typarDefns

                let implicit =
                    match kind with
                    | ClassMemberKind.Method -> implicitMemberTypars ctx classTypars b
                    | _ -> []

                // The explicit `<'C>` typars are exactly the leading `explicit`
                // prefix; the count lets `generaliseMemberTypars` pass ONLY them as
                // `canonical`'s `declared` (the implicit tail must be ordered by
                // appearance per the F# rule, not treated as declared).
                let seed = mkTypeParams ctx.Store (explicit @ implicit)

                addMember mName kind isStatic isOverride mKey seed (List.length explicit)
                |> ignore
            | ValueNone -> ()

        let registerAutoProperty id isStatic isOverride =
            addMember
                (ctx.NameOf id)
                ClassMemberKind.Property
                isStatic
                isOverride
                (NodeKey.ofToken id NodeKind.PatIdent)
                EqArray.empty
                0
            |> ignore

        let registerAbstractMethod idOrOp tds isStatic kind =
            match identOrOpNameTok ctx idOrOp with
            | ValueSome(mName, mTok) ->
                // The method's own `<'C, …>` typars get prototype TyVars so
                // Unification scopes the signature against them and Elaborate can
                // surface them as GenericMethodParameters.
                let explicit = memberTyparNames ctx tds
                let seed = mkTypeParams ctx.Store explicit
                // An `abstract` signature is a slot declaration, never an override.
                addMember mName kind isStatic false (NodeKey.ofToken mTok NodeKind.PatIdent) seed (List.length explicit)
                |> ignore
            | ValueNone -> ()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; keyword = kw; defn = d)) ->
                let isStatic = s.IsSome

                // `override`/`default` members set the override flag; plain `member` and `abstract` stay `false`.
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
                    // is an abstract *property*; a curried/arrow signature is a method.
                    let kind =
                        if sigArgs.IsEmpty then
                            ClassMemberKind.Property
                        else
                            ClassMemberKind.Method

                    registerAbstractMethod idOrOp tds isStatic kind
                | MethodOrPropDefn.PropertyWithGetSet _ ->
                    diagnose "Properties with explicit `get`/`set` blocks are not yet supported"
                | MethodOrPropDefn.AbstractSignature _ ->
                    // The non-MethodOrPropSig form is the property-signature form
                    // (`abstract Item : int with get`).
                    diagnose "Abstract property signatures are not yet supported"
            | TypeDefnElement.Member(MemberDefn.Value _) ->
                // `val [mutable] x: T` explicit instance fields are
                // *not* `TypeMemberInfo`s — class registration extracts them
                // separately via `extractInstanceFields`.
                ()
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor _) ->
                // Secondary constructors aren't `TypeMemberInfo`s — class
                // registration extracts them separately via `extractSecondaryCtors`.
                // A union augmentation has no primary ctor to chain to, so one here
                // is meaningless and silently dropped (the parser permits it).
                ()
            | TypeDefnElement.InterfaceImpl _ ->
                // `interface IFace with member …` blocks are *not* part of
                // the class's own member set — they're collected separately by
                // `extractInterfaceImpls` and resolved against the external
                // interface in Unification's `fillClassMembers`.
                ()
            | TypeDefnElement.InterfaceSpec _ ->
                // A bare `interface IFace` spec (no inline members) carries no
                // bodies to register. Spec-only conformance is deferred.
                ()
            | TypeDefnElement.Inherit _ -> diagnose "Inheritance is not yet supported"

        memberInfos.ToArray()

    /// Collect the `interface IFace with member …` blocks declared in a class body. Each interface
    /// member is re-wrapped as a `TypeDefnElement.Member` so the existing member
    /// machinery (`extractMembers`, plus the NameResolution / Unification
    /// member-body walks) consumes it unchanged. The interface *type* is kept as
    /// raw CST — the external provider that resolves it isn't reachable until
    /// Unification, which links each impl's `Resolved` and types its bodies.
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
                let declKey = NodeKey.ofToken ifaceTok NodeKind.TypeNamed

                let memberEls: TypeDefnElements<SyntaxToken> =
                    match objMembersOpt with
                    | ValueSome(ObjectMembers(memberDefns = mds)) ->
                        ImmutableArray.CreateRange(seq { for md in mds -> TypeDefnElement.Member md })
                    | ValueNone -> ImmutableArray.Empty

                let members = extractMembers ctx declKey classTypars memberEls
                acc.Add(ClassInterfaceImplInfo(ifaceTyp, members, memberEls, declKey))
            | _ -> ()

        acc.ToArray()

    /// Collect `val [mutable] x: T` explicit instance fields declared in a class / struct
    /// body. A `val` field is always annotated, so its type resolves outright — under the
    /// class's typar scope, against the types in scope where it is written. `IsMutable`
    /// reflects the `mutable` keyword. `static val` is not a thing F# accepts here, so a
    /// `staticToken` is ignored. Called under `underClassTyparScope`.
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
                        NodeKey.ofToken id NodeKind.DeclLetBinding
                    )
                )
            | _ -> ()

        acc.ToArray()

    /// `ClassPreambleEntry` placeholders for a class body's `[static] let` / `[static] do`
    /// preamble, split into the STATIC sequence (the `.cctor`'s body) and the INSTANCE
    /// sequence (the tail of the primary ctor). Each is ONE ordered sequence: a `do` may
    /// observe a `let` above it, so the interleaving cannot be flattened into parallel lists.
    ///
    /// A generic class's `static let` lowers to a per-instantiation static field
    /// (one field on the open generic `TypeDefinition`, its `.cctor` running once
    /// per closed instantiation — codegen mints the read/store as a `MemberRef` on
    /// the self-`TypeSpec`). An instance binder is a private instance field, so a generic
    /// class carries those for free too.
    ///
    /// Only a simple binder head (`let x = …`, `let f x = …`) is supported.
    let private extractPreamble
        (ctx: PassContext)
        (declKey: NodeKey)
        (hasPrimaryCtor: bool)
        (isValueType: bool)
        (preamble: ImmutableArray<ClassFunctionOrValueDefn<SyntaxToken>>)
        : struct (ClassPreambleEntry[] * ClassPreambleEntry[]) =
        let statics = ResizeArray<ClassPreambleEntry>()
        let instances = ResizeArray<ClassPreambleEntry>()

        // Every rejection here is a property of the CLASS, not of the offending entry, and so
        // is anchored at `declKey`: a class with three instance entries and no primary ctor
        // would otherwise report one identical error per entry at a single site. One message,
        // once.
        let reported = HashSet<string>()

        let diagnose msg =
            if reported.Add msg then
                ctx.Diagnostics.Add
                    {
                        Key = declKey
                        Message = msg
                        Code = ""
                        Severity = Severity.Error
                    }

        // An instance `let`/`do` runs in the PRIMARY ctor. Two shapes have no ctor that can run
        // it, and F# rejects both:
        //  * the `val`-field form (`type T = val …; new(…) = …`) declares no primary ctor at all
        //    (FS0963 — F# does not pick a secondary);
        //  * a STRUCT's zero-arg default ctor is not ours to write, so `Unchecked.defaultof<S>`
        //    would leave the binder's field unset (FS0901 for a `let`, FS0035 for a `do`).
        // Diagnose and drop: an accepted entry would mint a field no ctor ever initialises.
        let instanceAllowed (isDo: bool) =
            if not hasPrimaryCtor then
                diagnose "An instance `let` or `do` binding may only be used in a type with a primary constructor"
                false
            elif isValueType then
                if isDo then
                    diagnose
                        "Structs cannot contain `do` bindings because the default constructor for structs would not execute these bindings"
                else
                    diagnose
                        "Structs cannot contain value definitions because the default constructor for structs will not execute these bindings"

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
                        match bindingsOfPat ctx b.headPat with
                        | [ (name, key) ] ->
                            let tv = ctx.NewTypeVar()
                            ctx.Store.SetLevel(UnionFind.find ctx.Store tv, 0)
                            acc.Add(ClassPreambleEntry.Let(ClassLetInfo(name, TyVar tv, key, b, isRec.IsSome)))
                        | _ -> diagnose "Only simple `let x = …` bindings are supported in a class preamble"
                | ValueNone -> ()
            | ClassFunctionOrValueDefn.Do(staticToken = st; expr = e) ->
                if st.IsSome then
                    statics.Add(ClassPreambleEntry.Do e)
                elif instanceAllowed true then
                    instances.Add(ClassPreambleEntry.Do e)

        struct (statics.ToArray(), instances.ToArray())

    /// Stamp `ClassTypeInfo` for every `TypeDefn.Class` (or `TypeDefn.Anon` — the
    /// parser emits Anon for the bare `type C(...) = member ...` form without an
    /// explicit `class`/`end`). The class's declared STRUCTURE (ctor-parameter annotations,
    /// `val` field types) resolves here, under the class's own typar scope. Member types
    /// are placeholder TyVars; Unification's `fillClassMembers` links them once each member
    /// body is inferred.
    let private registerClassTypeDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match TypeDefnPatterns.tryClassLikeDecl td with
        | ValueNone -> ()
        | ValueSome d ->
            let tn, pc, asD, body = d.TypeName, d.PrimaryConstr, d.AsDefn, d.Body
            let name = id.Name
            let declKey = id.DeclKey
            let classTyparNames = typarNamesOfTypeName ctx tn
            let typeParams = mkTypeParams ctx.Store classTyparNames

            // Every annotated position in the class's declared surface, resolved under the
            // class typar scope in ONE entry so a `'a` in a ctor param, a `val` field or a
            // secondary ctor's parameter all bind the same prototype TyVar.
            let structure =
                underTyparScope
                    ctx
                    typeParams
                    (fun () ->
                        {|
                            CtorParams = extractCtorParams ctx declKey pc
                            SecondaryCtors = extractSecondaryCtors ctx declKey body.elements
                            InstanceFields = extractInstanceFields ctx body.elements
                        |}
                    )

            let memberInfos =
                ResizeArray<TypeMemberInfo>(extractMembers ctx declKey classTyparNames body.elements)

            let thisName =
                match asD with
                | ValueSome(AsDefn(ident = aid)) -> ctx.NameOf aid
                | ValueNone -> "this"

            let thisKey = BinderKey.ofDeclaredThis declKey
            let baseKey = BinderKey.ofDeclaredBase declKey

            let members = memberInfos.ToArray()

            // No `PrimaryConstrArgs` (`pc = ValueNone`) ⇒ the `val`-field form
            // (`type T = val …; new(…) = …`): the secondaries are the only ctors, so
            // codegen must not synthesise a colliding primary `.ctor` — and an instance
            // `let`/`do` has no ctor to run in.
            let hasPrimaryCtor = pc.IsSome

            // `[<Sealed>]` flips TypeAttributes.Sealed on the emitted
            // TypeDefinition; `[<AllowNullLiteral>]` lets Unification's
            // Expr.Null arm unify against this class.
            let classAttrs =
                Attributes.decodeClassAttributes ctx (Attributes.attributesOfTypeName tn)

            // `[<Struct>]` (or the `type X = struct … end` shape) ⇒ value
            // type. A struct is implicitly sealed (no derivation), so the
            // emitted `TypeAttributes.Sealed` rides `IsValueType` too. Known
            // before the preamble is extracted: a struct may not carry an
            // instance one.
            let isValueType = classAttrs.IsValueType || TypeDefnPatterns.isStructShape td

            let struct (staticPreamble, instancePreamble) =
                extractPreamble ctx declKey hasPrimaryCtor isValueType body.classPreamble

            let info =
                ClassTypeInfo(
                    name,
                    typeParams,
                    structure.CtorParams,
                    members,
                    declKey,
                    thisName,
                    thisKey,
                    baseKey,
                    id.Key
                )

            info.StaticPreamble <- staticPreamble
            info.InstancePreamble <- instancePreamble
            info.SecondaryCtors <- structure.SecondaryCtors
            info.HasPrimaryCtor <- hasPrimaryCtor

            info.IsSealed <- classAttrs.IsSealed
            info.AllowNullLiteral <- classAttrs.AllowNullLiteral
            info.InterfaceImpls <- extractInterfaceImpls ctx classTyparNames body.elements
            // The class's `when 'S :> IFace` typar constraints, attached to the
            // prototype TyVars by `fillClassMembers` so a member-body access on
            // a constrained class typar resolves through the interface.
            info.TyparConstraints <- NameResolutionTypeRegistration.typarConstraintsOfTypeName tn

            info.IsValueType <- isValueType
            // A project-local interface (all-abstract body) — so
            // `resolveInterfaceImpls` / the subtype check recognise it without
            // an external-provider entry.
            info.IsInterface <- TypeDefnPatterns.isInterfaceShape td
            // `[<IsByRefLike>]` ⇒ a byref-like (`ref struct`) value type.
            info.IsByRefLike <- classAttrs.IsByRefLike
            info.InstanceFields <- structure.InstanceFields

            // Validate equality / comparison attributes against the class
            // kind (FS0382 / FS0377) and stamp kind-aware verdicts. A value
            // type defaults to `Structural`; a reference class to
            // `Reference`. Comparison opt-in ⇒ `NoComparison`. Custom* and
            // explicit Structural* / No* overrides come from the validator.
            let classKind =
                if isValueType then
                    Attributes.EqCompTargetKind.Struct
                else
                    Attributes.EqCompTargetKind.RefClass

            let eqV, cmpV =
                Attributes.validateEqCompAttributes ctx classKind declKey (Attributes.attributesOfTypeName tn)

            info.EqualitySupport <-
                match eqV with
                | ValueSome v -> v
                | ValueNone ->
                    if isValueType then
                        EqualityVerdict.Structural
                    else
                        EqualityVerdict.Reference

            info.ComparisonSupport <-
                match cmpV with
                | ValueSome v -> v
                | ValueNone -> ComparisonVerdict.NoComparison

            TypeRegistry.registerClass ctx.Types info

            for m in members do
                let entry = { Class = info; Member = m }

                match ctx.Types.ClassMemberIndex.TryGetValue m.Name with
                | true, lst ->
                    let buf = ResizeArray(lst.Length + 1)
                    buf.Add entry

                    for e in lst do
                        buf.Add e

                    ctx.Types.ClassMemberIndex.[m.Name] <- EqArray.ofResizeArray buf
                | false, _ -> ctx.Types.ClassMemberIndex.[m.Name] <- EqArray.singleton entry

    /// An interface carries no `ClassTypeInfo` (no equality / comparison verdict
    /// to stamp), but `[<StructuralEquality>]` / `[<ReferenceEquality>]` /
    /// `[<CustomEquality>]` etc. are still illegal on it — run the kind-legality
    /// check (FS0382 / FS0377) so those produce a diagnostic, discarding the
    /// verdicts.
    /// A `TypeDefn.Interface` claims no name and registers no detail, so it is not a
    /// claimed declaration and this validation is driven from the CST — it is not a
    /// registration.
    let private validateInterfaceTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Interface(typeName = tn) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length = 1 then
                let declKey = NodeKey.ofToken nameLi.Idents.[0] NodeKind.DeclType

                Attributes.validateEqCompAttributes
                    ctx
                    Attributes.EqCompTargetKind.Interface
                    declKey
                    (Attributes.attributesOfTypeName tn)
                |> ignore
        | _ -> ()

    /// The use site a single-segment type head written at `li` speaks from — its own place
    /// in the file, under the module and `open`s the registration scan currently stands in.
    let private useSiteOfHead (ctx: PassContext) (li: LongIdent<SyntaxToken>) : UseSite =
        ctx.UseSiteAt(NodeKey.ofToken li.Idents.[li.Idents.Length - 1] NodeKind.TypeNamed)

    /// Resolve a named type appearing in an `inherit` clause *argument* position
    /// (`inherit Box<int>(v)`'s `int`) to a best-effort `SemType`. A
    /// registration-time mini-`translateType`: NameResolution runs before
    /// `Unification.translateType` exists, and v1 inherit clauses carry only
    /// simple arg types (a builtin, a class typar, or another project-local type).
    /// Abbreviation expansion is deferred to Unification, so an abbrev-named arg
    /// lands as an opaque `TyConst`.
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
            resolveInheritArgName ctx (useSiteOfHead ctx li) (ctx.NameOf li.Idents.[0]) EqArray.empty
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let targs =
                EqArray.ofList
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateInheritArg ctx typarScope at
                            | TypeArg.Measure _ -> ()
                    ]

            resolveInheritArgName ctx (useSiteOfHead ctx li) (ctx.NameOf li.Idents.[0]) targs
        | Type.SuffixedType(baseType = bt; longIdent = li) when li.Idents.Length = 1 ->
            resolveInheritArgName
                ctx
                (useSiteOfHead ctx li)
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
        // An intrinsic's identity is resolved (local registration first, then the provider
        // through ambient opens), never enumerated by name. `args` are empty for the scalar
        // intrinsics, so a uniform arm is behaviour-identical to the old per-name arms.
        match IntrinsicResolve.tryResolveIntrinsicKey ctx.Resolver ctx.Types.IntrinsicKeys name with
        | Some k -> TyConst(k, args)
        | None ->
            // Nominal heads carry their resolved `SymbolKey`; take it
            // off the registry `info` rather than re-stringing the name.
            match TypeRegistry.tryRecord ctx.Types useSite name with
            | ValueSome info -> TyRecord(info.TypeKey, args)
            | ValueNone ->
                match TypeRegistry.tryUnionBare ctx.Types useSite name with
                | ValueSome info -> TyUnion(info.TypeKey, args)
                | ValueNone ->
                    match TypeRegistry.tryClass ctx.Types useSite name with
                    | ValueSome info -> TyClass(info.TypeKey, args)
                    | ValueNone -> TyConst(RuntimeNames.opaqueKey name, EqArray.empty)

    /// Resolve an `inherit` clause's parent type to a `TyClass` under the derived
    /// class's typar scope. Diagnoses (and returns `ValueNone`) when the parent is
    /// a non-class type, an unknown name, or a multi-segment / external name — v1
    /// routes only single-segment project-local classes (multi-segment / BCL base
    /// classes land with the provider catalogue).
    let private resolveInheritParent
        (ctx: PassContext)
        (typarScope: Map<string, TyVarId>)
        (t: Type<SyntaxToken>)
        : SemType voption =
        let rec head (t: Type<SyntaxToken>) : (LongIdent<SyntaxToken> * SemType list) voption =
            match t with
            | Type.ParenType(typ = inner) -> head inner
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

        let diagnose (key: NodeKey) (msg: string) =
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = msg
                    Code = ""
                    Severity = Severity.Error
                }

        match head t with
        | ValueNone -> ValueNone
        | ValueSome(li, targs) ->
            let nameTok = li.Idents.[li.Idents.Length - 1]
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeNamed

            if li.Idents.Length <> 1 then
                let qual = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                diagnose diagKey (sprintf "Inheriting from a qualified base type '%s' is not yet supported" qual)
                ValueNone
            else
                let name = ctx.NameOf nameTok

                // A heritable base's platform repr → its external `TyClass` (codegen's
                // `ExternalClass` encoder maps it to a `TypeRef` for `extends` + base-ctor),
                // or a "did not resolve" diagnostic. Shared by the LOCAL heritable-extern arm
                // and the cross-unit provider arm for a ctor-less `(# class … #)` base.
                let reprToExternalBase (repr: string) =
                    match tryResolveExternalTypeKey ctx repr targs.Length with
                    | ValueSome extKey -> ValueSome(TyClass(extKey, EqArray.ofList targs))
                    | ValueNone ->
                        diagnose
                            diagKey
                            (sprintf
                                "Cannot inherit from external base '%s': its representation '%s' did not resolve to a known external type (is a package dependency missing?)"
                                name
                                repr)

                        ValueNone

                // Fallback when the name is not a project-local class: a referenced heritable
                // primitive published by a provider (`exn`, or a prior compilation unit's
                // `(# class … #)` base like `Attribute`).
                let resolveThroughProvider () =
                    // The provider publishes it as an `Intrinsic` with a class surface.
                    // `inherit X` is a name WRITTEN AT A SITE, so it resolves through the SAME
                    // opens-aware engine every other written type head uses (`tryPickExternalType`
                    // over `ctx.Resolution.OpenScope`) — the file's explicit `open`s, then the
                    // implicit open of its own `namespace N` header (`CstWalk.addNamespacePrefix`,
                    // F#'s `ImplicitlyOpenOwnNamespace`), then the ambient prelude. That implicit
                    // open is what resolves a SAME-namespace prior unit's base (`Vesper.Core`'s
                    // `compiler-attributes.fs` inheriting `prim-types-attr.fs`'s `Attribute`): the
                    // fact lives in the CONSUMER's scope, never in a producer-published ambient.
                    // `pick` returning `ValueNone` scans past a non-intrinsic hit to the next
                    // candidate. Then branch on whether the CONTRACT declares a ctor:
                    //   * WITH ctors (`exn`'s `new: string -> exn` / `new: unit -> exn`): admit
                    //     the intrinsic CANON as a `TyConst` base, so `fillBaseCtorCall` checks
                    //     the base-`.ctor` args against the contract ctor set (and REJECTS a
                    //     mismatch) — the contract IS the constructible surface.
                    //   * WITHOUT ctors (`Attribute = (# class … #)` — the `.fs`/`.fsi` bind only
                    //     the repr): admit the PLATFORM type as a `TyClass` (the SAME base the
                    //     heritable-local arm mints), so the base-`.ctor` binds the runtime type's
                    //     own ctors (`System.Attribute()`); the empty contract surface would
                    //     otherwise reject `inherit Attribute()`.
                    let heritableIntrinsic (shape: ExternalTypeShape) =
                        match shape with
                        | ExternalTypeShape.Intrinsic { Id = id; Class = ValueSome surface } ->
                            ValueSome(struct (id, surface))
                        | _ -> ValueNone

                    match
                        tryPickExternalType
                            ctx
                            (arityProbes targs.Length)
                            (fun hit -> heritableIntrinsic hit.Shape)
                            name
                    with
                    | ValueSome(struct (id, surface)) when surface.Members |> Array.exists (fun m -> m.Name = ".ctor") ->
                        ValueSome(TyConst(SymbolKey.Type id.Canon, EqArray.ofList targs))
                    | ValueSome(struct (id, _)) ->
                        match id.Platform with
                        | Some repr -> reprToExternalBase repr
                        | None ->
                            diagnose
                                diagKey
                                (sprintf
                                    "Cannot inherit from '%s': it has no runtime representation on the compiling target"
                                    name)

                            ValueNone
                    | ValueNone ->
                        // The class arms above have already missed, so a name the NAME TABLE
                        // knows at any arity is a project-local type of some other kind. One
                        // table ⇒ no kind can be forgotten from this disjunction. A name the
                        // table does not know is unknown *here*, which includes a type
                        // declared below this group — nothing later can fill the slot.
                        if TypeRegistry.isTypeNameInScope ctx.Types (ctx.UseSiteAt diagKey) name then
                            diagnose
                                diagKey
                                (sprintf "Cannot inherit from type '%s' — only classes are inheritable" name)
                        else
                            diagnose diagKey (sprintf "Cannot inherit from unknown type '%s'" name)

                        ValueNone

                match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
                | ValueSome info -> ValueSome(TyClass(info.TypeKey, EqArray.ofList targs))
                | ValueNone ->
                    // Heritable-local arm: a `(# class … #)` intrinsic of THIS unit. ONE
                    // key-addressed read (`intrinsicKeyOf` → `IntrinsicReprInfo`) yields BOTH
                    // the repr and the `class`-tag verdict from the SAME entry, so a heritable
                    // base with no repr is structurally unrepresentable — no runtime invariant
                    // to police, and no parallel name-axis set to keep in step.
                    match ctx.Types.IntrinsicReprKeys.TryGetValue(TypeRegistry.intrinsicKeyOf ctx.Types name) with
                    | true, repr when repr.Heritable ->
                        // Resolve to the EXTERNAL type the repr names, so the base freezes to an
                        // `FTClass` the codegen `ExternalClass` encoder maps to a `TypeRef`
                        // (`extends` + base-ctor), rather than the opaque value-repr `TyConst`.
                        reprToExternalBase repr.Platform
                    | _ -> resolveThroughProvider ()

    /// Fill `BaseType` / `BaseCtorArgs` on a class with an `inherit` clause. An `inherit`
    /// parent is the one reference resolved against the referent's registered DETAIL
    /// (`ClassTypeInfo` for a local parent, `IntrinsicReprKeys` for a heritable extern
    /// base) rather than its identity, so it cannot be answered at
    /// the point the clause is seen: `ClassTypeInfo.BaseType` is the PENDING SLOT, filled
    /// once the whole group's detail is registered. A parent above the group is already
    /// registered, a parent inside it registers before the group closes, and a parent below
    /// it never will — which is exactly the unknown-type diagnostic `resolveInheritParent`
    /// raises.
    ///
    /// The DERIVED class is recovered by the `TypeKey` on its own claim, never by name: a
    /// bare name does not address an arity-overloaded class (`Box\`1` / `Box\`2`), so a
    /// name lookup here would drop the `inherit` clause of either.
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

    /// Detect inheritance cycles among the classes of ONE group, once every `BaseType`
    /// slot in it is filled. Walks each class's parent chain; on re-entry to the starting
    /// class emits a "cyclic inheritance" diagnostic on its `DeclKey` and clears its
    /// `BaseType` so later passes treat it as parent-less.
    ///
    /// A group is the whole search space: a class names only what is declared above it or
    /// joined to it by `and`, so an inheritance back-edge — which is what a cycle needs —
    /// can only run between members of one `type … and …` group.
    let private checkGroupInheritanceCycles (ctx: PassContext) (classes: ClassTypeInfo seq) : unit =
        for start in classes do
            // Compare on the class's `TypeKey` (arity included), not its bare name,
            // so an arity-overloaded self-reference (`Foo\`2` : `Foo\`3`) isn't falsely
            // flagged as a cycle.
            let visited = System.Collections.Generic.HashSet<TypeKey>()
            visited.Add start.TypeKey |> ignore

            let rec walk (info: ClassTypeInfo) =
                match info.BaseType with
                | ValueSome(TyClass(parentKey, _)) ->
                    if parentKey = start.TypeKey then
                        ctx.Diagnostics.Add
                            {
                                Key = start.DeclKey
                                Message = sprintf "Type '%s' has a cyclic inheritance hierarchy" start.Name
                                Code = ""
                                Severity = Severity.Error
                            }

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

    /// Stamp augmentation members + `interface … with` impls onto an already-registered
    /// union or record. Must run after the type itself is registered; reads its
    /// `extensions.elements`. A v1 union/record has no primary ctor / `as` alias, so
    /// `this` is always `"this"`. The member/impl extraction is kind-agnostic (the same
    /// collection the class registration uses); only the write-back target type differs,
    /// so the host interface (read-only) can't carry it — each arm sets its own `info`.
    ///
    /// The AUGMENTED type is recovered by the `TypeKey` on its own claim, never by name: a
    /// bare name does not address an arity-overloaded union / record, so a name lookup
    /// here would drop the whole `with member …` block of either.
    let private registerNominalMember (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        let extract (declKey: NodeKey) (typeParams: EqArray<string * TyVarId>) elems =
            let typarNames = [ for (n, _) in typeParams -> n ]

            {|
                Members = extractMembers ctx declKey typarNames elems
                InterfaceImpls = extractInterfaceImpls ctx typarNames elems
                ThisKey = BinderKey.ofDeclaredThis declKey
            |}

        match td with
        | TypeDefn.Union(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
            match TypeRegistry.tryUnionByKey ctx.Types id.Key with
            | ValueSome info ->
                let x = extract info.DeclKey info.TypeParams elems
                info.Members <- x.Members
                info.InterfaceImpls <- x.InterfaceImpls
                info.ThisKey <- x.ThisKey
            | ValueNone -> ()
        | TypeDefn.Record(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
            match TypeRegistry.tryRecordByKey ctx.Types id.Key with
            | ValueSome info ->
                let x = extract info.DeclKey info.TypeParams elems
                info.Members <- x.Members
                info.InterfaceImpls <- x.InterfaceImpls
                info.ThisKey <- x.ThisKey
            | ValueNone -> ()
        // An inline intrinsic-abbrev host (`type X = (# … #) with member …`):
        // stamp its augmentation members + `ThisKey` exactly as the union/record
        // arms do. The host is present in `IntrinsicAbbrevHost` only for an
        // ILIntrinsic RHS (a transparent-alias abbrev with members was rejected
        // at registration), so this arm fires only for the sanctioned host. No
        // `interface … with` on the intrinsic host (out of scope) — the extracted
        // `InterfaceImpls` are always empty. An intrinsic binding is non-generic in
        // practice and its host table is name-keyed, so the name off the claim addresses it.
        | TypeDefn.Abbrev(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
            match ctx.Types.IntrinsicAbbrevHost.TryGetValue id.Name with
            | true, info ->
                let x = extract info.DeclKey info.TypeParams elems
                info.Members <- x.Members
                info.InterfaceImpls <- x.InterfaceImpls
                info.ThisKey <- x.ThisKey
            | false, _ -> ()
        | _ -> ()

    /// The nominal a `SemType` names DIRECTLY, if any. A type argument is NOT direct: a
    /// `B option` field stores a reference to a `B`, so it is an indirection, and only the
    /// head of a field's type is an immediate containment edge.
    let private directNominal (store: TypeStore) (t: SemType) : TypeKey voption =
        match zonk store t with
        | TyRecord(key, _)
        | TyUnion(key, _)
        | TyClass(key, _) -> ValueSome key
        | TyEnum key -> ValueSome key
        | _ -> ValueNone

    /// The types a registered declaration STORES INLINE — the fields a value type lays out
    /// in its own memory. A struct record's fields, a struct union's case fields, a struct
    /// class's `val` fields and its ctor-param backing fields. Only ever asked of a value
    /// type (`isValueTypeDefn`), because a reference type stores a POINTER to each field and
    /// so contains none of them immediately.
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

    /// FS0954's other half: a cycle through STRUCT FIELDS. A value type stores its fields
    /// inline, so a struct that (transitively) contains itself has no finite layout — F#
    /// rejects `[<Struct>] type A = { x: B } and [<Struct>] B = { y: A }` with the same code
    /// it gives an inheritance cycle.
    ///
    /// The edge set is STRUCT-field edges only. A cycle through a REFERENCE-typed field is
    /// legal (`type A = { x: B } and B = { y: A }` compiles — the indirection breaks it), so
    /// a check over all field edges would reject a legal program; and a type argument is an
    /// indirection too (`directNominal`). Group-local, like every other cycle: a type names
    /// only what is declared above it or joined to it by `and`, so the back-edge a cycle
    /// needs can only run between members of one group.
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
                            // into it — nothing outside can name into a group.
                            | false, _ -> ()
                    | _ -> ()

            walk startId

            if cyclic then
                ctx.Diagnostics.Add
                    {
                        Key = startId.DeclKey
                        Message =
                            sprintf
                                "Type '%s' involves an immediate cyclic reference through a struct field or inheritance relation"
                                startId.Name
                        Code = "FS0954"
                        Severity = Severity.Error
                    }

    /// Register one accepted declaration's kind-specific DETAIL — fields, cases, enum case
    /// names, class members / ctor params, abbreviation RHS — plus any `with member …`
    /// augmentation on it. Dispatches on the `TypeDeclKind` its claim recorded and is
    /// HANDED the identity it registers under, so no registrar re-derives a name / arity /
    /// key from the CST and none can be reached for a rejected duplicate.
    let private registerDetail (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match id.Kind with
        | TypeDeclKind.Record -> registerRecordTypeDefn ctx id td
        | TypeDeclKind.Union -> registerUnionTypeDefn ctx id td
        | TypeDeclKind.Enum -> registerEnumTypeDefn ctx id td
        // The abbreviation ENTRY is filed ahead of every other kind's detail (see
        // `registerGroup`), so this arm has nothing left to do for it.
        | TypeDeclKind.Abbreviation
        | TypeDeclKind.IntrinsicRepr -> ()
        | TypeDeclKind.Class -> registerClassTypeDefn ctx id td

        registerNominalMember ctx id td

    /// Register one `type … and …` group — the unit of mutual recursion, and the unit of
    /// registration. `ModuleElem.Type` IS that group, so the driver above is a single
    /// top-down scan and F#'s file-order type scoping falls out of it: at the moment a
    /// group registers, `TypeClaims` holds every type above it and nothing below, so a head
    /// naming a type declared below simply misses the registry — no guard has to say so.
    ///
    /// The phases, in the order the dependencies force:
    ///
    /// 1. CLAIM every member's `(name, arity)` + `TypeKey`, in source order. A reference
    ///    that needs only the referent's key and arity — a nominal head in a field type, a
    ///    member signature, a type argument — is satisfied outright by this, which is the
    ///    whole of `and`-joined mutual recursion for records, unions and member sigs.
    /// 2. CLASSIFY every head written in the group's declared structure against the scope
    ///    the claim phase just fixed: a claimed name is local, anything else is external or
    ///    unknown. This is the resolution `translateType` then executes, so the structure
    ///    resolves in the declaration's own scope — including an external type a local one
    ///    declared BELOW would otherwise shadow.
    /// 3. FILE THE ABBREVIATION ENTRIES, in source order, before any other kind's detail: an
    ///    alias is expanded on demand by whatever names it, so `type R = { x: A } and A = int`
    ///    needs `A`'s entry (not its body) present when `R`'s field translates.
    /// 4. REGISTER DETAIL for every other kind, in source order.
    /// 5. CLOSE: force the group's alias bodies (an alias RHS reads its referent's registered
    ///    detail, so it cannot be answered where it is written); fill the `inherit` slots
    ///    (same reason); then check the two cycles those two relations admit. All four are
    ///    group-local because nothing outside the group can name into it.
    let registerGroup
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (recScopeOffset: int voption)
        (defs: ImmutableArray<TypeDefn<SyntaxToken>>)
        : unit =
        let claims = ResizeArray<ClaimedTypeDefn>(defs.Length)
        // One offset for the whole group — the unit of mutual recursion is the unit of
        // visibility, so an `and`-sibling cannot be visible from a different place than
        // the type it is joined to.
        let visibleFrom = typeGroupVisibleFrom recScopeOffset defs

        for td in defs do
            match claimTypeIdentity ctx c visibleFrom td with
            | ValueSome claimed -> claims.Add claimed
            | ValueNone -> ()

        // Over ALL defs, not only the claimed ones: a declaration that claims no type (an
        // `interface … end`, a delegate) still writes type heads that must resolve.
        for td in defs do
            classifyDeclaredTypes ctx td

        for claimed in claims do
            match claimed.Identity.Kind with
            | TypeDeclKind.Abbreviation
            | TypeDeclKind.IntrinsicRepr -> registerAbbreviationDefn ctx claimed.Identity claimed.Defn
            | TypeDeclKind.Record
            | TypeDeclKind.Union
            | TypeDeclKind.Enum
            | TypeDeclKind.Class -> ()

        for claimed in claims do
            registerDetail ctx claimed.Identity claimed.Defn

        // An `interface … end` declares no type to register — its eq/comp attributes are
        // still illegal, so the kind-legality check runs over the group's CST.
        for td in defs do
            validateInterfaceTypeDefn ctx td

        // Group close. Force every alias body — on demand expansion has already forced the
        // ones something named, and `forceFill` is idempotent, so this reaches exactly the
        // aliases nothing referenced (including a cyclic pair, which diagnoses here).
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
