namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionScope
open NameResolutionTypeRegistration

// Registry stamping for class type definitions (ctor params, members, static
// lets) and union augmentation members. Member/param types start as placeholder
// TyVars; Unification's fill* pre-passes Link them once member bodies are inferred.

module NameResolutionMemberRegistration =

    /// Constructor parameter info from a parameter *pattern* (the primary ctor's
    /// `PrimaryConstrArgs.pat` or a secondary ctor's `new(...)` pattern). v1
    /// accepts only simple patterns (`NamedSimple`, `Typed (NamedSimple, t)`,
    /// `Tuple` of those, possibly enclosed, and `()` for no params); anything
    /// else diagnoses and contributes nothing.
    let private ctorParamsOfPat (ctx: PassContext) (declKey: NodeKey) (p: Pat<SyntaxToken>) : ClassCtorParamInfo[] =
        let results = ResizeArray<ClassCtorParamInfo>()

        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.EmptyBlock _ -> () // `new()` / `C()` — no parameters
            | Pat.NamedSimple id ->
                let name = ctx.NameOf id
                // Synthetic kind keeps the param's binding-site key distinct
                // from a regular Pat.NamedSimple at the same offset.
                let pKey = NodeKey.ofToken id NodeKind.PatIdent
                let tv = TypeVar()
                tv.Level <- 0
                results.Add(ClassCtorParamInfo(name, TyVar tv, pKey))
            | Pat.Typed(pat = Pat.NamedSimple id) ->
                let name = ctx.NameOf id
                let pKey = NodeKey.ofToken id NodeKind.PatIdent
                let tv = TypeVar()
                tv.Level <- 0
                results.Add(ClassCtorParamInfo(name, TyVar tv, pKey))
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

    /// `ClassSecondaryCtorInfo` placeholders for a class body's `new(...)`
    /// overloads. Each overload's params start as placeholder TyVars
    /// (filled by Unification); the synthetic `DeclKey` keys it from the `new`
    /// token so distinct overloads don't collide.
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
                acc.Add(ClassSecondaryCtorInfo(ctorKey, parms, pat, body))
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
    /// (inference scope seed + Freeze `GenericMethodParameters`) carry them through
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

        let rec walkTy (t: Type<SyntaxToken>) =
            match t with
            | Type.VarType tp -> addTypar tp
            | Type.ParenType(typ = inner)
            | Type.SuffixedType(baseType = inner)
            | Type.DottedType(baseType = inner)
            | Type.ArrayType(baseType = inner)
            | Type.AnonymousSubtype(typ = inner) -> walkTy inner
            | Type.FunctionType(fromType = f; toType = into) ->
                walkTy f
                walkTy into
            | Type.TupleType(types = ts)
            | Type.StructTupleType(types = ts) ->
                for ty in ts do
                    walkTy ty
            | Type.GenericType(typeArgs = args) ->
                for a in args do
                    match a with
                    | TypeArg.Type at -> walkTy at
                    | TypeArg.Measure _ -> ()
            | Type.WhenConstrainedType(typ = inner) -> walkTy inner
            | Type.SubtypeConstraint(typar = tp; typ = inner) ->
                addTypar tp
                walkTy inner
            | Type.UnionType(left = l; right = r) ->
                walkTy l
                walkTy r
            | Type.AnonRecordType(fields = fs) ->
                for AnonRecordField(typ = ty) in fs do
                    walkTy ty
            | Type.NamedType _
            | Type.Null _
            | Type.MeasureType _
            | Type.ILIntrinsic _
            | Type.Missing
            | Type.SkipsTokens _ -> ()

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

        let addMember mName kind isStatic isOverride (mKey: NodeKey) : TypeMemberInfo =
            let tv = TypeVar()
            tv.Level <- 0
            let cmi = TypeMemberInfo(mName, kind, isStatic, TyVar tv, mKey)
            cmi.IsOverride <- isOverride
            memberInfos.Add cmi
            cmi

        let registerNamed (b: Binding<SyntaxToken>) kind isStatic isOverride =
            match memberNameOf ctx b with
            | ValueSome(mName, mKey) ->
                let cmi = addMember mName kind isStatic isOverride mKey
                // A concrete generic method (`member this.Map<'C> …`) carries
                // its own typars on the binding's `typarDefns`. Stamp prototype
                // TyVars so Unification scopes the signature against them and Freeze
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

                cmi.MethodTypeParams <- mkTypeParams (explicit @ implicit)
            | ValueNone -> ()

        let registerAutoProperty id isStatic isOverride =
            addMember
                (ctx.NameOf id)
                ClassMemberKind.Property
                isStatic
                isOverride
                (NodeKey.ofToken id NodeKind.PatIdent)
            |> ignore

        let registerAbstractMethod idOrOp tds isStatic kind =
            match identOrOpNameTok ctx idOrOp with
            | ValueSome(mName, mTok) ->
                // An `abstract` signature is a slot declaration, never an override.
                let cmi =
                    addMember mName kind isStatic false (NodeKey.ofToken mTok NodeKind.PatIdent)
                // The method's own `<'C, …>` typars get prototype TyVars so
                // Unification scopes the signature against them and Freeze can
                // surface them as GenericMethodParameters.
                cmi.MethodTypeParams <- mkTypeParams (memberTyparNames ctx tds)
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

    /// Collect `val [mutable] x: T` explicit instance fields declared in a class / struct body. Each becomes
    /// a `ClassFieldInfo` with a placeholder TyVar (linked by Unification from the
    /// annotation `TypeCst`) and the source `mutable` flag. `static val` is not a
    /// thing F# accepts here, so a `staticToken` is ignored.
    let private extractInstanceFields
        (ctx: PassContext)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : ClassFieldInfo[] =
        let acc = ResizeArray<ClassFieldInfo>()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Value(mutableToken = mut; ident = id; typ = t)) ->
                let name = ctx.NameOf id
                let declKey = NodeKey.ofToken id NodeKind.DeclLetBinding
                let tv = TypeVar()
                tv.Level <- 0
                acc.Add(ClassFieldInfo(name, TyVar tv, mut.IsSome, t, declKey))
            | _ -> ()

        acc.ToArray()

    /// `ClassStaticLetInfo` placeholders for a class body's `static let` preamble. Only simple `static let x = …` (single named binder) is supported.
    /// A generic class's `static let` lowers to a per-instantiation static field
    /// A generic class's `static let` lowers to a per-instantiation static field
    /// (one field on the open generic `TypeDefinition`, its `.cctor` running once
    /// per closed instantiation — codegen mints the read/store as a `MemberRef` on
    /// the self-`TypeSpec`). Instance `let` and `[static] do` preamble entries
    /// are not yet modelled (silently skipped).
    let private extractStaticLets
        (ctx: PassContext)
        (declKey: NodeKey)
        (preamble: ImmutableArray<ClassFunctionOrValueDefn<SyntaxToken>>)
        : ClassStaticLetInfo[] =
        let acc = ResizeArray<ClassStaticLetInfo>()

        let diagnose msg =
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message = msg
                    Code = ""
                    Severity = Severity.Error
                }

        for d in preamble do
            match d with
            | ClassFunctionOrValueDefn.LetBindings(staticToken = ValueSome _; bindings = bindings) ->
                for b in bindings do
                    match bindingsOfPat ctx b.headPat with
                    | [ (name, key) ] ->
                        let tv = TypeVar()
                        tv.Level <- 0
                        acc.Add(ClassStaticLetInfo(name, TyVar tv, key, b.expr))
                    | _ -> diagnose "Only simple `static let x = …` bindings are supported"
            | _ -> ()

        acc.ToArray()

    /// Stamp `ClassTypeInfo` for every `TypeDefn.Class` (or `TypeDefn.Anon` — the
    /// parser emits Anon for the bare `type C(...) = member ...` form without an
    /// explicit `class`/`end`). Member types are placeholder TyVars; Unification's
    /// fillClassMembers links them once each member body is inferred.
    let private registerClassTypeDefn (ctx: PassContext) (declNs: string) (td: TypeDefn<SyntaxToken>) : unit =
        match TypeDefnPatterns.tryClassLikeDecl td with
        | ValueNone -> ()
        | ValueSome d ->
            let tn, pc, asD, body = d.TypeName, d.PrimaryConstr, d.AsDefn, d.Body
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

                if
                    TypeRegistry.containsRecord ctx.Types name
                    || ctx.Types.Union.ContainsKey name
                    || TypeRegistry.containsAbbrev ctx.Types name
                    || TypeRegistry.containsClass ctx.Types name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
                    let classTyparNames = typarNamesOfTypeName ctx tn
                    let typeParams = mkTypeParams classTyparNames
                    let ctorParams = extractCtorParams ctx declKey pc

                    let memberInfos =
                        ResizeArray<TypeMemberInfo>(extractMembers ctx declKey classTyparNames body.elements)

                    let thisName =
                        match asD with
                        | ValueSome(AsDefn(ident = id)) -> ctx.NameOf id
                        | ValueNone -> "this"

                    let thisKey = NodeKey.ofSynthetic declKey.Offset NodeKind.SynthThisBinding
                    let baseKey = NodeKey.ofSynthetic declKey.Offset NodeKind.SynthBaseBinding

                    let members = memberInfos.ToArray()

                    let staticLets = extractStaticLets ctx declKey body.classPreamble

                    let key = stampLocalTypeKey ctx declKey declNs name typeParams.Length

                    let info =
                        ClassTypeInfo(name, typeParams, ctorParams, members, declKey, thisName, thisKey, baseKey, key)

                    info.StaticLets <- staticLets
                    info.SecondaryCtors <- extractSecondaryCtors ctx declKey body.elements
                    // No `PrimaryConstrArgs` (`pc = ValueNone`) ⇒ the `val`-field form
                    // (`type T = val …; new(…) = …`): the secondaries are the only ctors,
                    // so codegen must not synthesise a colliding primary `.ctor`.
                    info.HasPrimaryCtor <- pc.IsSome

                    // `[<Sealed>]` flips TypeAttributes.Sealed on the emitted
                    // TypeDefinition; `[<AllowNullLiteral>]` lets Unification's
                    // Expr.Null arm unify against this class.
                    let classAttrs =
                        Attributes.decodeClassAttributes ctx (Attributes.attributesOfTypeName tn)

                    info.IsSealed <- classAttrs.IsSealed
                    info.AllowNullLiteral <- classAttrs.AllowNullLiteral
                    info.InterfaceImpls <- extractInterfaceImpls ctx classTyparNames body.elements
                    // The class's `when 'S :> IFace` typar constraints, attached to the
                    // prototype TyVars by `fillClassMembers` so a member-body access on
                    // a constrained class typar resolves through the interface.
                    info.TyparConstraints <- NameResolutionTypeRegistration.typarConstraintsOfTypeName tn

                    // `[<Struct>]` (or the `type X = struct … end` shape) ⇒ value
                    // type. A struct is implicitly sealed (no derivation), so the
                    // emitted `TypeAttributes.Sealed` rides `IsValueType` too.
                    let isValueType = classAttrs.IsValueType || TypeDefnPatterns.isStructShape td
                    info.IsValueType <- isValueType
                    // A project-local interface (all-abstract body) — so
                    // `resolveInterfaceImpls` / the subtype check recognise it without
                    // an external-provider entry.
                    info.IsInterface <- TypeDefnPatterns.isInterfaceShape td
                    // `[<IsByRefLike>]` ⇒ a byref-like (`ref struct`) value type.
                    info.IsByRefLike <- classAttrs.IsByRefLike
                    info.InstanceFields <- extractInstanceFields ctx body.elements

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
                        Attributes.validateEqCompAttributes ctx classKind nameTok (Attributes.attributesOfTypeName tn)

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

                    TypeRegistry.registerClass ctx.Types name info

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
    let private validateInterfaceTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Interface(typeName = tn) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length = 1 then
                let nameTok = nameLi.Idents.[0]

                Attributes.validateEqCompAttributes
                    ctx
                    Attributes.EqCompTargetKind.Interface
                    nameTok
                    (Attributes.attributesOfTypeName tn)
                |> ignore
        | _ -> ()

    let registerClassTypes (ctx: PassContext) (declNs: string) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerClassTypeDefn ctx declNs td
                validateInterfaceTypeDefn ctx td
        | _ -> ()

    // A post-pass after
    // `registerClassTypes` so a derived class can name a parent declared later in
    // the file.

    /// Resolve a named type appearing in an `inherit` clause *argument* position
    /// (`inherit Box<int>(v)`'s `int`) to a best-effort `SemType`. A
    /// registration-time mini-`translateType`: NameResolution runs before
    /// `Unification.translateType` exists, and v1 inherit clauses carry only
    /// simple arg types (a builtin, a class typar, or another project-local type).
    /// Abbreviation expansion is deferred to Unification, so an abbrev-named arg
    /// lands as an opaque `TyConst`.
    let rec private translateInheritArg
        (ctx: PassContext)
        (typarScope: Map<string, TypeVar>)
        (t: Type<SyntaxToken>)
        : SemType =
        let freshTv () =
            let tv = TypeVar()
            tv.Level <- 0
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
            resolveInheritArgName ctx (ctx.NameOf li.Idents.[0]) EqArray.empty
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let targs =
                EqArray.ofList
                    [
                        for a in args do
                            match a with
                            | TypeArg.Type at -> yield translateInheritArg ctx typarScope at
                            | TypeArg.Measure _ -> ()
                    ]

            resolveInheritArgName ctx (ctx.NameOf li.Idents.[0]) targs
        | Type.SuffixedType(baseType = bt; longIdent = li) when li.Idents.Length = 1 ->
            resolveInheritArgName
                ctx
                (ctx.NameOf li.Idents.[0])
                (EqArray.singleton (translateInheritArg ctx typarScope bt))
        | Type.TupleType(types = types) ->
            TyTuple(EqArray.ofList [ for ty in types -> translateInheritArg ctx typarScope ty ])
        | Type.FunctionType(fromType = f; toType = into) ->
            TyFun(translateInheritArg ctx typarScope f, translateInheritArg ctx typarScope into)
        | _ -> freshTv ()

    and private resolveInheritArgName (ctx: PassContext) (name: string) (args: EqArray<SemType>) : SemType =
        match name with
        | "int" -> BuiltinTypes.tyInt
        | "bool" -> BuiltinTypes.tyBool
        | "unit" -> BuiltinTypes.tyUnit
        | "float" -> BuiltinTypes.tyFloat
        | "string" -> BuiltinTypes.tyString
        | "int64" -> BuiltinTypes.tyInt64
        | "byte" -> BuiltinTypes.tyByte
        | _ when ctx.Types.IntrinsicReprTypes.ContainsKey name -> TyConst(name, args)
        | _ ->
            // Nominal heads carry their resolved `SymbolKey`; take it
            // off the registry `info` rather than re-stringing the name.
            match TypeRegistry.tryRecord ctx.Types name with
            | ValueSome info -> TyRecord(info.Key, args)
            | ValueNone ->
                match ctx.Types.Union.TryGetValue name with
                | true, info -> TyUnion(info.Key, args)
                | false, _ ->
                    match TypeRegistry.tryClass ctx.Types name with
                    | ValueSome info -> TyClass(info.Key, args)
                    | ValueNone -> TyConst(name, EqArray.empty)

    /// Resolve an `inherit` clause's parent type to a `TyClass` under the derived
    /// class's typar scope. Diagnoses (and returns `ValueNone`) when the parent is
    /// a non-class type, an unknown name, or a multi-segment / external name — v1
    /// routes only single-segment project-local classes (multi-segment / BCL base
    /// classes land with the provider catalogue).
    let private resolveInheritParent
        (ctx: PassContext)
        (typarScope: Map<string, TypeVar>)
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

                match ctx.Types.Class.TryGetValue name with
                | true, info -> ValueSome(TyClass(info.Key, EqArray.ofList targs))
                | false, _ ->
                    if
                        ctx.Types.Record.ContainsKey name
                        || ctx.Types.Union.ContainsKey name
                        || ctx.Types.Abbreviation.ContainsKey name
                        || ctx.Types.IntrinsicReprTypes.ContainsKey name
                    then
                        diagnose diagKey (sprintf "Cannot inherit from type '%s' — only classes are inheritable" name)
                    else
                        diagnose diagKey (sprintf "Cannot inherit from unknown type '%s'" name)

                    ValueNone

    /// Stamp `BaseType` / `BaseCtorArgs` on each class with an `inherit` clause.
    /// Runs after `registerClassTypes` so a forward / out-of-order parent
    /// reference resolves. Cycle detection is a separate sweep
    /// (`checkInheritanceCycles`) once every class is stamped.
    let registerInheritedSlots (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match TypeDefnPatterns.tryClassLikeDecl td with
                | ValueNone -> ()
                | ValueSome d ->
                    match d.Body.inherits with
                    | ValueNone -> ()
                    | ValueSome(ClassInheritsDecl(typ = parentTyp; expr = exprOpt)) ->
                        let (TypeName(ident = nameLi)) = d.TypeName

                        if nameLi.Idents.Length = 1 then
                            match ctx.Types.Class.TryGetValue(ctx.NameOf nameLi.Idents.[0]) with
                            | true, info ->
                                let typarScope =
                                    (Map.empty, info.TypeParams)
                                    ||> EqArray.fold (fun acc (n, tv) -> Map.add n tv acc)

                                match resolveInheritParent ctx typarScope parentTyp with
                                | ValueSome parentTy ->
                                    info.BaseType <- ValueSome parentTy
                                    info.BaseCtorArgs <- exprOpt
                                | ValueNone -> ()
                            | false, _ -> ()
        | _ -> ()

    /// Detect inheritance cycles after every class's `BaseType` is stamped. Walks
    /// each class's parent chain; on re-entry to the starting class emits a
    /// "cyclic inheritance" diagnostic on its `DeclKey` and clears its `BaseType`
    /// so later passes treat it as parent-less.
    let checkInheritanceCycles (ctx: PassContext) : unit =
        for kv in ctx.Types.Class do
            let start = kv.Value

            let rec walk (visited: Set<string>) (info: ClassTypeInfo) =
                match info.BaseType with
                | ValueSome(TyClass(parentKey, _)) ->
                    // Project the parent class's bare registry name off its key.
                    let parentName = SymbolKeyOps.simpleName parentKey

                    if parentName = start.Name then
                        ctx.Diagnostics.Add
                            {
                                Key = start.DeclKey
                                Message = sprintf "Type '%s' has a cyclic inheritance hierarchy" start.Name
                                Code = ""
                                Severity = Severity.Error
                            }

                        start.BaseType <- ValueNone
                    elif visited.Contains parentName then
                        // A cycle that doesn't pass back through `start`; it is
                        // diagnosed when iteration reaches a class on that cycle.
                        ()
                    else
                        match ctx.Types.Class.TryGetValue parentName with
                        | true, parentInfo -> walk (Set.add parentName visited) parentInfo
                        | false, _ -> ()
                | _ -> ()

            walk (Set.singleton start.Name) start

    /// Stamp augmentation members onto an already-registered `UnionTypeInfo`
    ///. Must run after registerUnionTypes; reads the union's
    /// `extensions.elements`. A v1 union has no primary ctor / `as` alias, so
    /// `this` is always `"this"`.
    let registerUnionMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
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
                    | true, info ->
                        let unionTyparNames = [ for (n, _) in info.TypeParams -> n ]
                        info.Members <- extractMembers ctx info.DeclKey unionTyparNames elems
                        info.ThisKey <- NodeKey.ofSynthetic info.DeclKey.Offset NodeKind.SynthThisBinding
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()
