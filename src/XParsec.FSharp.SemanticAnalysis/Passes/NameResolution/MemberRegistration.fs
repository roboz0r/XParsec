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
                        Severity = Error
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
    /// overloads (B-11). Each overload's params start as placeholder TyVars
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

    /// A member's name from its head pattern. `this.M`-shaped heads parse as
    /// `Pat.NamedSimple` for the member-name token; the `this`/alias is in
    /// `MethodOrPropDefn`'s `ident` field, not the head pattern.
    let private memberNameOf (ctx: PassContext) (b: Binding<SyntaxToken>) : (string * SyntaxToken) voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id, id)
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

    /// `TypeMemberInfo` placeholders for a type body's / augmentation's member
    /// elements. Shared by class registration (`body.elements`) and union
    /// augmentation (`extensions.elements`). Unsupported element kinds emit a
    /// diagnostic at `declKey` — each arm is named so individual diagnostics can
    /// be lifted in isolation as features land.
    let extractMembers
        (ctx: PassContext)
        (declKey: NodeKey)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : TypeMemberInfo[] =
        let memberInfos = ResizeArray<TypeMemberInfo>()

        let diagnose msg =
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message = msg
                    Code = ""
                    Severity = Error
                }

        let addMember mName kind isStatic mTok : TypeMemberInfo =
            let tv = TypeVar()
            tv.Level <- 0
            let mKey = NodeKey.ofToken mTok NodeKind.PatIdent
            let cmi = TypeMemberInfo(mName, kind, isStatic, TyVar tv, mKey)
            memberInfos.Add cmi
            cmi

        let registerNamed (b: Binding<SyntaxToken>) kind isStatic =
            match memberNameOf ctx b with
            | ValueSome(mName, mTok) ->
                let cmi = addMember mName kind isStatic mTok
                // A concrete generic method (`member this.Map<'C> …`, B-12) carries
                // its own typars on the binding's `typarDefns`. Stamp prototype
                // TyVars so Unification scopes the signature against them and Freeze
                // surfaces them as GenericMethodParameters — mirroring the abstract
                // path. A property's `typarDefns` is absent ⇒ empty.
                cmi.MethodTypeParams <- mkTypeParams (memberTyparNames ctx b.typarDefns)
            | ValueNone -> ()

        let registerAutoProperty id isStatic =
            addMember (ctx.NameOf id) ClassMemberKind.Property isStatic id |> ignore

        let registerAbstractMethod idOrOp tds isStatic =
            match identOrOpNameTok ctx idOrOp with
            | ValueSome(mName, mTok) ->
                let cmi = addMember mName ClassMemberKind.Method isStatic mTok
                // The method's own `<'C, …>` typars get prototype TyVars so
                // Unification scopes the signature against them and Freeze can
                // surface them as GenericMethodParameters.
                cmi.MethodTypeParams <- mkTypeParams (memberTyparNames ctx tds)
            | ValueNone -> ()

        for el in elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
                let isStatic = s.IsSome

                match d with
                | MethodOrPropDefn.Method(defn = b) -> registerNamed b ClassMemberKind.Method isStatic
                | MethodOrPropDefn.Property(defn = b) -> registerNamed b ClassMemberKind.Property isStatic
                | MethodOrPropDefn.AutoProperty(ident = id) -> registerAutoProperty id isStatic
                | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(ident = idOrOp; typarDefns = tds)) ->
                    registerAbstractMethod idOrOp tds isStatic
                | MethodOrPropDefn.PropertyWithGetSet _ ->
                    diagnose "Properties with explicit `get`/`set` blocks are not yet supported"
                | MethodOrPropDefn.AbstractSignature _ ->
                    // The non-MethodOrPropSig form is the property-signature form
                    // (`abstract Item : int with get`).
                    diagnose "Abstract property signatures are not yet supported"
            | TypeDefnElement.Member(MemberDefn.Value _) -> diagnose "`val` members are not yet supported"
            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor _) ->
                // Secondary constructors (B-11) aren't `TypeMemberInfo`s — class
                // registration extracts them separately via `extractSecondaryCtors`.
                // A union augmentation has no primary ctor to chain to, so one here
                // is meaningless and silently dropped (the parser permits it).
                ()
            | TypeDefnElement.InterfaceImpl _ -> diagnose "Interface implementations are not yet supported"
            | TypeDefnElement.InterfaceSpec _ -> diagnose "Interface specifications are not yet supported"
            | TypeDefnElement.Inherit _ -> diagnose "Inheritance is not yet supported"

        memberInfos.ToArray()

    /// `ClassStaticLetInfo` placeholders for a class body's `static let` preamble
    /// (B-10). Only simple `static let x = …` (single named binder) is supported.
    /// Per-instantiation cache lowering for a generic class is deferred, so a
    /// `static let` on a generic class is diagnosed and dropped. Instance `let`
    /// and `[static] do` preamble entries are not yet modelled (silently skipped).
    let private extractStaticLets
        (ctx: PassContext)
        (declKey: NodeKey)
        (isGeneric: bool)
        (preamble: ImmutableArray<ClassFunctionOrValueDefn<SyntaxToken>>)
        : ClassStaticLetInfo[] =
        let acc = ResizeArray<ClassStaticLetInfo>()

        let diagnose msg =
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message = msg
                    Code = ""
                    Severity = Error
                }

        for d in preamble do
            match d with
            | ClassFunctionOrValueDefn.LetBindings(staticToken = ValueSome _; bindings = bindings) ->
                for b in bindings do
                    match bindingsOfPat ctx b.headPat with
                    | [ (name, key) ] ->
                        if isGeneric then
                            diagnose "`static let` on a generic class is not yet supported"
                        else
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
    let private registerClassTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
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
                    ctx.Types.Record.ContainsKey name
                    || ctx.Types.Union.ContainsKey name
                    || ctx.Types.Abbreviation.ContainsKey name
                    || ctx.Types.Class.ContainsKey name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Error
                        }
                else
                    let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
                    let ctorParams = extractCtorParams ctx declKey pc

                    let memberInfos =
                        ResizeArray<TypeMemberInfo>(extractMembers ctx declKey body.elements)

                    let thisName =
                        match asD with
                        | ValueSome(AsDefn(ident = id)) -> ctx.NameOf id
                        | ValueNone -> "this"

                    let thisKey = NodeKey.ofSynthetic declKey.Offset NodeKind.SynthThisBinding
                    let baseKey = NodeKey.ofSynthetic declKey.Offset NodeKind.SynthBaseBinding

                    let members = memberInfos.ToArray()

                    let staticLets =
                        extractStaticLets ctx declKey (not typeParams.IsEmpty) body.classPreamble

                    let info =
                        ClassTypeInfo(name, typeParams, ctorParams, members, declKey, thisName, thisKey, baseKey)

                    info.StaticLets <- staticLets
                    info.SecondaryCtors <- extractSecondaryCtors ctx declKey body.elements

                    // B-8: `[<Sealed>]` flips TypeAttributes.Sealed on the emitted
                    // TypeDefinition; `[<AllowNullLiteral>]` lets Unification's
                    // Expr.Null arm unify against this class.
                    let classAttrs =
                        Attributes.decodeClassAttributes ctx (Attributes.attributesOfTypeName tn)

                    info.IsSealed <- classAttrs.IsSealed
                    info.AllowNullLiteral <- classAttrs.AllowNullLiteral

                    ctx.Types.Class.[name] <- info

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

    let registerClassTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerClassTypeDefn ctx td
        | _ -> ()

    /// Stamp augmentation members onto an already-registered `UnionTypeInfo`
    /// (P3d.3). Must run after registerUnionTypes; reads the union's
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
                        info.Members <- extractMembers ctx info.DeclKey elems
                        info.ThisKey <- NodeKey.ofSynthetic info.DeclKey.Offset NodeKind.SynthThisBinding
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()
