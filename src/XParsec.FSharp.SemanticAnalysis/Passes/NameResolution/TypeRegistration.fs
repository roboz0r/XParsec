namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Registry stamping for record / union / abbreviation type definitions.
// Field/case types start as placeholder TyVars; Unification's fill pre-passes
// Link them once every type is registered, so a type can reference another
// declared elsewhere in the same file regardless of declaration order.

module NameResolutionTypeRegistration =

    /// A `Typar`'s source-text name; the leading `'`/`^` lives on a separate
    /// token. Anon (`_`) typars don't participate in scope — ValueNone.
    let typarName (ctx: PassContext) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome(ctx.NameOf id)
        | Typar.Anon _ -> ValueNone

    /// Declared typars for a `TypeName`, in source order: prefix typars (`'a Box`)
    /// first, then suffix (`Box<'a, 'b>`). Skips anonymous typars.
    let typarNamesOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string list =
        let (TypeName(prefixTypars = pt; typarDefns = td)) = tn

        let prefix =
            [
                match pt with
                | ValueNone -> ()
                | ValueSome(PrefixTypars.Single t) ->
                    match typarName ctx t with
                    | ValueSome n -> yield n
                    | ValueNone -> ()
                | ValueSome(PrefixTypars.Multiple(typars = ts)) ->
                    for t in ts do
                        match typarName ctx t with
                        | ValueSome n -> yield n
                        | ValueNone -> ()
            ]

        let main =
            [
                match td with
                | ValueNone -> ()
                | ValueSome(TyparDefns(defns = ds)) ->
                    for TyparDefn(typar = t) in ds do
                        match typarName ctx t with
                        | ValueSome n -> yield n
                        | ValueNone -> ()
            ]

        prefix @ main

    /// The `when 'a : ...` clause on a `TypeName`, if any. Captured onto the
    /// registry entry so Unification's fill pass attaches each constraint to the
    /// prototype TyVars without re-walking the CST.
    let typarConstraintsOfTypeName (tn: TypeName<SyntaxToken>) : TyparConstraints<SyntaxToken> voption =
        let (TypeName(typarDefns = td)) = tn

        match td with
        | ValueSome(TyparDefns(constraints = ValueSome tc)) -> ValueSome tc
        | _ -> ValueNone

    /// Mint a prototype TyVar per declared typar name. Stored on the registry
    /// entry and substituted out at every use site, so two instantiations share
    /// no variables.
    let mkTypeParams (names: string list) : EqArray<string * TypeVar> =
        EqArray.ofSeq (
            seq {
                for n in names ->
                    let tv = TypeVar()
                    tv.Level <- 0
                    n, tv
            }
        )

    /// Mint a project-local `SymbolKey` for a type declaration and assert it is
    /// unique across the compilation. `declNs` is
    /// the declaring namespace threaded from the module walk, so the key —
    /// `TypeKey(None, declNs, name\`arity)` — equals the identity the type emits as
    /// (its `TDecl.Namespace` + arity-suffixed metadata name). A collision means two
    /// distinct declarations minted the same key, i.e. the walk dropped a
    /// distinguishing namespace; it is reported as an internal error (a user
    /// duplicate is rejected before the stamp and never reaches here). Callers
    /// pass the result into the registered info's constructor as its `Key`.
    let stampLocalTypeKey
        (ctx: PassContext)
        (declKey: NodeKey)
        (declNs: string)
        (name: string)
        (arity: int)
        : SymbolKey =
        // The home assembly is this compilation's target (`ctx.AssemblyName`);
        // `None` on the front-end-only paths that pass no assembly name. Invariant
        // per type, so this local key equals the key a consumer mints for the same
        // type from its `SymbolOrigin.Assembly`.
        let key =
            LocalSymbolKey.ofType (SymbolKeyOps.asmOf ctx.AssemblyName) declNs name arity

        match TypeRegistry.recordKeyOrigin ctx.Types declKey key with
        | ValueSome _ ->
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message =
                        sprintf
                            "Internal error: project-local SymbolKey collision for '%s' (namespace '%s', arity %d)"
                            name
                            declNs
                            arity
                    Code = ""
                    Severity = Severity.Error
                }
        | ValueNone -> ()

        key

    let private registerRecordTypeDefn (ctx: PassContext) (declNs: string) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Record(typeName = tn; fields = fields) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok

                if
                    TypeRegistry.containsRecord ctx.Types name
                    || ctx.Types.Union.ContainsKey name
                    || TypeRegistry.containsAbbrev ctx.Types name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken nameTok NodeKind.DeclType
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
                    let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)

                    let fieldInfos =
                        [|
                            for f in fields do
                                let (RecordField(mutableToken = mt; ident = id)) = f
                                let fName = ctx.NameOf id
                                // Placeholder TyVar (not TyConst) so Unification can
                                // Link the real type later via existing machinery.
                                let tv = TypeVar()
                                tv.Level <- 0
                                yield RecordFieldInfo(fName, TyVar tv, mt.IsSome, NodeKey.ofToken id NodeKind.DeclType)
                        |]

                    let declKey = NodeKey.ofToken nameTok NodeKind.DeclType
                    let key = stampLocalTypeKey ctx declKey declNs name typeParams.Length

                    let info =
                        RecordTypeInfo(name, typeParams, fieldInfos, declKey, typarConstraintsOfTypeName tn, key)

                    // C-Attr: explicit equality attribute wins; absent, the default
                    // ⇒ Structural when every field is immutable,
                    // Reference otherwise. Feeds Unification.checkConstraint and the
                    // codegen triple gate (Freeze copies it onto EqualitySupport).
                    info.EqualitySupport <-
                        match Attributes.decodeEqualityAttributes ctx (Attributes.attributesOfTypeName tn) with
                        | ValueSome v -> v
                        | ValueNone ->
                            if fieldInfos |> Array.forall (fun fi -> not fi.IsMutable) then
                                EqualityVerdict.Structural
                            else
                                EqualityVerdict.Reference

                    // C-Attr (Phase 3): comparison defaults to NoComparison
                    // (brainstorm-comparison §9, opt-in); explicit attribute overrides.
                    info.ComparisonSupport <-
                        match Attributes.decodeComparisonAttributes ctx (Attributes.attributesOfTypeName tn) with
                        | ValueSome v -> v
                        | ValueNone -> ComparisonVerdict.NoComparison

                    TypeRegistry.registerRecord ctx.Types name info

                    for fi in fieldInfos do
                        match ctx.Types.FieldIndex.TryGetValue fi.Name with
                        | true, infos ->
                            let buf = ResizeArray(infos.Length + 1)
                            buf.Add info

                            for i in infos do
                                buf.Add i

                            ctx.Types.FieldIndex.[fi.Name] <- EqArray.ofResizeArray buf
                        | false, _ -> ctx.Types.FieldIndex.[fi.Name] <- EqArray.singleton info
        | _ -> ()

    let registerRecordTypes (ctx: PassContext) (declNs: string) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerRecordTypeDefn ctx declNs td
        | _ -> ()

    /// Map a union-case head to its case name. Delegates to the shared
    /// `OperatorNames.unionCaseCtorName` (the operator-named cases that matter are
    /// the cons-list ctors — `([])`→`Empty`, `(::)`→`Cons`) so the registered name
    /// can't drift from the contract extractor's. Heads we can't name (`(*)`,
    /// range/active-pattern ops) yield `""`, which `inspectCaseData` reads as "drop
    /// this case".
    let private unionCaseName (ctx: PassContext) (head: IdentOrOp<SyntaxToken>) : string =
        match OperatorNames.unionCaseCtorName ctx.NameOf head with
        | ValueSome n -> n
        | ValueNone -> ""

    /// Pull a ctor case's name + arity + per-field names from `UnionTypeCaseData`.
    /// Handles plain forms, operator-named cases, and the explicit-return
    /// (GADT-syntax) forms FSharp.Core's list uses (return type treated as the
    /// declaring union; true GADTs remain out of scope).
    let private inspectCaseData
        (ctx: PassContext)
        (data: UnionTypeCaseData<SyntaxToken>)
        : (string * int * string voption[]) voption =
        let naryNames (fields: ImmutableArray<UnionTypeField<SyntaxToken>>) : string voption[] =
            [|
                for f in fields ->
                    match f with
                    | UnionTypeField.Named(ident = id) -> ValueSome(ctx.NameOf id)
                    | UnionTypeField.Unnamed _ -> ValueNone
            |]

        let gadtNames (specs: ImmutableArray<ArgSpec<SyntaxToken>>) : string voption[] =
            [|
                for ArgSpec(name = nm) in specs ->
                    match nm with
                    | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(ctx.NameOf id)
                    | ValueNone -> ValueNone
            |]

        match data with
        | UnionTypeCaseData.Nullary(name = head)
        | UnionTypeCaseData.GadtNullary(name = head) ->
            let n = unionCaseName ctx head

            if n.Length = 0 then ValueNone else ValueSome(n, 0, [||])
        | UnionTypeCaseData.Nary(name = head; fields = fields) ->
            let n = unionCaseName ctx head

            if n.Length = 0 then
                ValueNone
            else
                ValueSome(n, fields.Length, naryNames fields)
        | UnionTypeCaseData.GadtNary(name = head; sign = UncurriedSig(args = ArgsSpec(args = specs))) ->
            let n = unionCaseName ctx head

            if n.Length = 0 then
                ValueNone
            else
                ValueSome(n, specs.Length, gadtNames specs)

    let private registerUnionTypeDefn (ctx: PassContext) (declNs: string) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Union(typeName = tn; cases = cases) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType
                let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
                // Generic arity overloads the short name (`Choice\`2`…`Choice\`7`),
                // so the duplicate test and the registry key are arity-qualified.
                let typeArity = typeParams.Length

                if
                    TypeRegistry.containsUnion ctx.Types name typeArity
                    || TypeRegistry.containsRecord ctx.Types name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
                    let caseInfos =
                        [|
                            for UnionTypeCase(data = data) in cases do
                                match inspectCaseData ctx data with
                                | ValueSome(caseName, arity, fieldNames) ->
                                    let fieldTys =
                                        Array.init
                                            arity
                                            (fun _ ->
                                                let tv = TypeVar()
                                                tv.Level <- 0
                                                TyVar tv
                                            )

                                    yield UnionCaseInfo(caseName, name, typeArity, fieldTys, fieldNames, declKey)
                                | ValueNone -> ()
                        |]

                    let key = stampLocalTypeKey ctx declKey declNs name typeArity

                    let info =
                        UnionTypeInfo(name, typeParams, caseInfos, declKey, typarConstraintsOfTypeName tn, key)

                    // C-Attr: union equality defaults to Structural
                    // (brainstorm §8); explicit attribute overrides.
                    info.EqualitySupport <-
                        match Attributes.decodeEqualityAttributes ctx (Attributes.attributesOfTypeName tn) with
                        | ValueSome v -> v
                        | ValueNone -> EqualityVerdict.Structural

                    // C-Attr (Phase 3): comparison defaults to NoComparison
                    // (brainstorm-comparison §9, opt-in); explicit attribute overrides.
                    info.ComparisonSupport <-
                        match Attributes.decodeComparisonAttributes ctx (Attributes.attributesOfTypeName tn) with
                        | ValueSome v -> v
                        | ValueNone -> ComparisonVerdict.NoComparison

                    TypeRegistry.registerUnion ctx.Types name typeArity info

                    // Record the decl-site identity
                    // so the type-decl emitter (`Freeze.tryUnionType`) recovers the
                    // union by key rather than re-deriving `(name, arity)`. `info.Key`
                    // is the arity-qualified `TypeKey(None, declNs, name\`arity)`; this
                    // stamp is co-populated with `ctx.Types.Union`, so the emitter's key
                    // lookup is exactly as total as the former `(name, arity)` one.
                    ctx.Resolution.ResolvedType.Set(declKey, info.Key)

                    for c in caseInfos do
                        match ctx.Types.CtorIndex.TryGetValue c.Name with
                        | true, infos ->
                            let buf = ResizeArray(infos.Length + 1)
                            buf.Add c

                            for i in infos do
                                buf.Add i

                            ctx.Types.CtorIndex.[c.Name] <- EqArray.ofResizeArray buf
                        | false, _ -> ctx.Types.CtorIndex.[c.Name] <- EqArray.singleton c
        | _ -> ()

    let registerUnionTypes (ctx: PassContext) (declNs: string) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerUnionTypeDefn ctx declNs td
        | _ -> ()

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS
    /// (`(# "System.Int32" #)` → `"System.Int32"`). Mirrors Freeze.stitchLiteralString.
    let private ilIntrinsicString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()
        // TODO: raise diagnostics for unsupported parts (Expr, InvalidText).
        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.Expr _ -> ()

        sb.ToString()

    /// An abbrev whose RHS is `Type.ILIntrinsic` is a *primitive binding*, not a
    /// transparent alias: recorded in IntrinsicReprTypes (name → IL string) and
    /// kept out of AbbreviationTypes, so translateType resolves the name to
    /// `TyConst name` rather than expanding the RHS. Other bodies are left
    /// unfilled; Unification's fillAbbreviationBodies forces each later, so an
    /// RHS can reference any other same-file type.
    let private registerAbbreviationDefn (ctx: PassContext) (declNs: string) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Abbrev(typeName = tn; typ = rhs) ->
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
                    || ctx.Types.IntrinsicReprTypes.ContainsKey name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
                    match rhs with
                    | Type.ILIntrinsic(instrParts = parts) ->
                        ctx.Types.IntrinsicReprTypes.[name] <- ilIntrinsicString ctx parts
                    | _ ->
                        let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
                        let key = stampLocalTypeKey ctx declKey declNs name typeParams.Length

                        let info =
                            AbbreviationInfo(name, typeParams, rhs, declKey, typarConstraintsOfTypeName tn, key)

                        TypeRegistry.registerAbbrev ctx.Types name info
        | _ -> ()

    let registerAbbreviationTypes (ctx: PassContext) (declNs: string) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerAbbreviationDefn ctx declNs td
        | _ -> ()
