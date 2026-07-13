namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Registry stamping for record / union / abbreviation type definitions.
// Field/case types start as placeholder TyVars; Unification's fill pre-passes
// Link them once every type is registered, so a type can reference another
// declared elsewhere in the same file regardless of declaration order.

module NameResolutionTypeRegistration =

    /// `[<CustomEquality>]` / `[<CustomComparison>]` on a record or union is out of
    /// scope: neither has an interface-impl side table to satisfy the
    /// `IEquatable<_>` / `IComparable<_>` requirement (union interface impls are
    /// unsupported front-to-back), so a `Custom` verdict on either is a diagnostic
    /// directing the user to a class. Shared by the record and union arms.
    let private rejectCustomOnDataType
        (ctx: PassContext)
        (declKey: NodeKey)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : unit =
        if eq = EqualityVerdict.Custom || cmp = ComparisonVerdict.Custom then
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message =
                        "[<CustomEquality>]/[<CustomComparison>] on a record or union is not supported in this compiler — wrap the type in a class that implements IEquatable<_>/IComparable<_>."
                    Code = "FS0378"
                    Severity = Severity.Error
                }

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

    /// The generic arity that keys this type in the registries (`0` for a
    /// non-generic name). The single source for the `(name, arity)` overload key —
    /// every arity-qualified `registerClass`/`tryClassArity`/duplicate-test site
    /// derives its arity through here rather than re-spelling the `List.length`.
    let arityOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : int =
        typarNamesOfTypeName ctx tn |> List.length

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
    let stampLocalTypeKey (ctx: PassContext) (declKey: NodeKey) (declNs: string) (name: string) (arity: int) : TypeKey =
        // The home assembly is this compilation's target (`ctx.AssemblyName`);
        // `None` on the front-end-only paths that pass no assembly name. Invariant
        // per type, so this local key equals the key a consumer mints for the same
        // type from its `SymbolOrigin.Assembly`.
        let key =
            LocalSymbolKey.ofType (SymbolKeyOps.asmOf ctx.AssemblyName) declNs name arity

        match TypeRegistry.recordKeyOrigin ctx.Types declKey (SymbolKey.Type key) with
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
                let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
                let arity = typeParams.Length

                if TypeRegistry.containsAnyType ctx.Types name arity then
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken nameTok NodeKind.DeclType
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
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

                    // Validate the equality / comparison attributes against the
                    // record kind (FS0382 / FS0377) and read the resolved verdicts.
                    let eqV, cmpV =
                        Attributes.validateEqCompAttributes
                            ctx
                            Attributes.EqCompTargetKind.Record
                            nameTok
                            (Attributes.attributesOfTypeName tn)

                    // Explicit equality attribute wins; absent, the default
                    // ⇒ Structural when every field is immutable,
                    // Reference otherwise. Feeds Unification.checkConstraint and the
                    // codegen triple gate (Elaborate copies it onto EqualitySupport).
                    info.EqualitySupport <-
                        match eqV with
                        | ValueSome v -> v
                        | ValueNone ->
                            if fieldInfos |> Array.forall (fun fi -> not fi.IsMutable) then
                                EqualityVerdict.Structural
                            else
                                EqualityVerdict.Reference

                    // Comparison defaults to NoComparison, explicit attribute overrides.
                    info.ComparisonSupport <-
                        match cmpV with
                        | ValueSome v -> v
                        | ValueNone -> ComparisonVerdict.NoComparison

                    rejectCustomOnDataType ctx declKey info.EqualitySupport info.ComparisonSupport

                    TypeRegistry.registerRecord ctx.Types name arity info

                    // Stamp the decl-site key so `Elaborate.tryRecordType` resolves this
                    // record by its arity-qualified `SymbolKey` (via `tryRecordByKey`),
                    // not the bare name — an arity-overloaded record (`Point`2`/`Point`3`)
                    // has no bare alias. Mirrors the union/enum decl-site stamp.
                    ctx.Resolution.ResolvedType.Set(declKey, SymbolKey.Type key)

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

                if TypeRegistry.containsAnyType ctx.Types name typeArity then
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

                    // Validate the equality / comparison attributes against the
                    // union kind (FS0382 / FS0377) and read the resolved verdicts.
                    let eqV, cmpV =
                        Attributes.validateEqCompAttributes
                            ctx
                            Attributes.EqCompTargetKind.Union
                            nameTok
                            (Attributes.attributesOfTypeName tn)

                    // Union equality defaults to Structural, explicit attribute overrides.
                    info.EqualitySupport <-
                        match eqV with
                        | ValueSome v -> v
                        | ValueNone -> EqualityVerdict.Structural

                    // Comparison defaults to NoComparison, explicit attribute overrides.
                    info.ComparisonSupport <-
                        match cmpV with
                        | ValueSome v -> v
                        | ValueNone -> ComparisonVerdict.NoComparison

                    rejectCustomOnDataType ctx declKey info.EqualitySupport info.ComparisonSupport

                    TypeRegistry.registerUnion ctx.Types name typeArity info

                    // Record the decl-site identity
                    // so the type-decl emitter (`Elaborate.tryUnionType`) recovers the
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

    /// Register an enum's nominal identity + case-name set so a `(x: E)` annotation
    /// resolves to `TyEnum Key` (in `translateType`) and a qualified `E.C1` access
    /// can validate the case name. Enums are non-generic (arity 0) and have no
    /// member/augmentation side tables — the case→literal *values* are resolved
    /// later by `Elaborate.tryEnumType` (the only stage with the literal readers in
    /// compile order) and ride the surfaced `TTypeKind.Enum` node. The minted `Key`
    /// is stamped at the decl site into `ResolvedType`, mirroring `registerUnionTypeDefn`.
    let private registerEnumTypeDefn (ctx: PassContext) (declNs: string) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Enum(typeName = tn; cases = cases) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else
                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

                // Enums are non-generic, so they claim their name at arity 0.
                if TypeRegistry.containsAnyType ctx.Types name 0 then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
                    let caseNames = [| for EnumTypeCase(ident = id) in cases -> ctx.NameOf id |]

                    // The case VALUES, but ONLY when EVERY case is a string literal —
                    // the literal-union admission (`subsumes`) runs before Elaborate
                    // resolves the full case table, so read the string form here
                    // through the SAME `StringLiterals.tryEnumCaseStringLiteral`
                    // projection `Elaborate.resolveEnumCaseValue` uses (peels a
                    // value-grouping paren, decodes escapes, admits verbatim/triple),
                    // so a legal `| A = ("auto")` is not silently declined. A single
                    // non-string case ⇒ `ValueNone` (the admission then declines and
                    // the enum stays a plain nominal).
                    let caseStringValues =
                        let vals =
                            [|
                                for EnumTypeCase(constValue = v) in cases do
                                    match StringLiterals.tryEnumCaseStringLiteral ctx v with
                                    | ValueSome s -> yield s
                                    | ValueNone -> ()
                            |]

                        if vals.Length = cases.Length && cases.Length > 0 then
                            ValueSome vals
                        else
                            ValueNone

                    // Enums are non-generic, so the arity is always 0.
                    let key = stampLocalTypeKey ctx declKey declNs name 0
                    let info = EnumTypeInfo(name, caseNames, caseStringValues, declKey, key)
                    TypeRegistry.registerEnum ctx.Types name info

                    // Record the decl-site identity so `Elaborate.tryEnumType`
                    // recovers the SAME key the annotation path resolves to.
                    ctx.Resolution.ResolvedType.Set(declKey, info.Key)
        | _ -> ()

    let registerEnumTypes (ctx: PassContext) (declNs: string) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerEnumTypeDefn ctx declNs td
        | _ -> ()

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS
    /// (`(# "System.Int32" #)` → `"System.Int32"`). Mirrors Elaborate.stitchLiteralString.
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
        | TypeDefn.Abbrev(typeName = tn; typ = rhs; extensions = ext) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

                // An abbreviation is bare-keyed and bare-resolved, so it claims its name
                // at EVERY arity — its own arity does not narrow the claim.
                if TypeRegistry.containsAnyTypeBare ctx.Types name then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }
                else
                    // An inline intrinsic-abbrev may carry a `with member …`
                    // augmentation (`type X = (# … #) with member …`) — but ONLY an
                    // ILIntrinsic RHS may. A transparent-alias abbrev with members
                    // (`type bad = int with member …`) is rejected here (F# rejects it
                    // too): the alias would have no distinct nominal identity to hang a
                    // member on. Registered as a host in `IntrinsicAbbrevHost` so the
                    // members name-resolve / type / elaborate on the shared host path,
                    // WITHOUT withdrawing the type from `IntrinsicReprTypes` (its
                    // `TyConst` identity is preserved at every other use site).
                    let registerMemberHostIfAny () =
                        match ext with
                        | ValueNone -> ()
                        | ValueSome _ ->
                            let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
                            let key = stampLocalTypeKey ctx declKey declNs name typeParams.Length
                            // The self-type key is the contract-sourced intrinsic identity
                            // (`IntrinsicKeys.[name]`, stamped just above), routed through the
                            // single `intrinsicKeyOf` resolver so `MkSelfType` cannot diverge
                            // from the abbrev's use-site key on a non-`Vesper` namespace.
                            let selfKey = TypeRegistry.intrinsicKeyOf ctx.Types name

                            ctx.Types.IntrinsicAbbrevHost.[name] <-
                                IntrinsicAbbrevInfo(name, typeParams, declKey, key, selfKey)

                    match rhs with
                    | Type.ILIntrinsic(kindTag = tag; instrParts = parts) ->
                        ctx.Types.IntrinsicReprTypes.[name] <- ilIntrinsicString ctx parts
                        // Contract-source the intrinsic's identity: mint its qualified key
                        // from the declaring namespace (VERBATIM name, no arity suffix — the
                        // name field is the identity string, arity rides in the `TyConst`
                        // args), so `Translate` resolves `int` to `Vesper.int` from the
                        // contract rather than re-deriving the namespace by name.
                        ctx.Types.IntrinsicKeys.[name] <- SymbolKeyOps.typeKey None declNs name
                        registerMemberHostIfAny ()

                        match tag with
                        // Untagged `(# "…" #)` — an opaque value repr, never a base.
                        | ValueNone -> ()
                        // A `class`-tagged intrinsic (`(# class "…" #)`) is a HERITABLE
                        // external reference base, not an opaque value repr: record the
                        // name so `resolveInheritParent` admits it as a parent.
                        | ValueSome(ExternKind.Class _) -> ctx.Types.HeritableExternBases.Add name |> ignore
                        // `(# interface "…" #)` parses (the AST carries the species for a
                        // future `extends`-less InterfaceImpl path) but has no emit path
                        // yet: an interface goes in `implements`, not the `extends` column,
                        // and has no base `.ctor` to chain to. Reject it here rather than
                        // let it fall through and mis-emit as a class base. Not added to
                        // `HeritableExternBases`, so it can never reach codegen's base path.
                        | ValueSome(ExternKind.Interface _) ->
                            ctx.Diagnostics.Add
                                {
                                    Key = declKey
                                    Message =
                                        sprintf
                                            "Heritable external interface base ('(# interface \"…\" #)') is not yet supported (type '%s'); only '(# class \"…\" #)' may be inherited"
                                            name
                                    Code = ""
                                    Severity = Severity.Error
                                }
                    | _ ->
                        // Guardrail: a transparent-alias abbrev cannot carry members.
                        // Reject with a diagnostic and drop the augmentation; the alias
                        // itself still registers so ordinary references keep resolving.
                        match ext with
                        | ValueSome _ ->
                            ctx.Diagnostics.Add
                                {
                                    Key = declKey
                                    Message =
                                        sprintf
                                            "Type abbreviation '%s' cannot carry augmentation members: only an inline-IL abbreviation ('type %s = (# \"…\" #) with member …') may declare members"
                                            name
                                            name
                                    Code = ""
                                    Severity = Severity.Error
                                }
                        | ValueNone -> ()

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
