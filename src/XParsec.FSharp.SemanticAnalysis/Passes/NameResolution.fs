namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared populated.
// Post: ctx.Binding populated for every ident-use site that resolves to a
//       local binding. Unresolved names that the provider also doesn't know
//       become Error diagnostics.
//
// Recursion is delegated to CstWalk.iterExpr; this pass supplies a Visit
// hook plus the three scope-introducing hooks (EnterFun, EnterBindingRhs,
// EnterLetBody). The walker thread-restores scope automatically at each
// recursive boundary, so the caller's scope is never polluted by a
// lambda/let body's locals.
//
// Notes for the tiny subset:
//   - Operators inside InfixApp / PrefixApp are NOT resolved here. Desugar
//     records them as DesugaredForm.OpName, and Unification consults the
//     provider directly when typing the application.
//   - External-symbol resolution writes NO entry to ctx.Binding; Unification
//     re-queries the provider when it sees a missing binding entry.
//   - IsInline is always false for the tiny subset. It'll get a real value
//     when the inline keyword is handled.
//   - IsMutable mirrors the binding's `mutableToken`. Propagated to every
//     use-site entry so Validation's assignment check can `ctx.Binding[lhsKey]`
//     directly. The binding site itself also gets a self-entry
//     (`BindingSite = key`) so Validation's value-restriction loop can
//     iterate mutable bindings by filtering `kv.Key = rb.BindingSite`.

module NameResolution =

    /// Per-scope entry: the binding site's NodeKey plus its mutability.
    /// Mutability propagates from the scope entry to every use-site
    /// `ResolvedBinding` that resolves through it, so downstream passes
    /// (Validation's immutable-assignment check) don't need a second hop.
    type private Scope = Map<string, NodeKey * bool>

    let private resolveIdent (ctx: PassContext) (scope: Scope list) (tok: SyntaxToken) (useKey: NodeKey) =
        let name = ctx.NameOf tok

        let rec lookup (s: Scope list) =
            match s with
            | [] -> ValueNone
            | head :: rest ->
                match Map.tryFind name head with
                | Some bs -> ValueSome bs
                | None -> lookup rest

        match lookup scope with
        | ValueSome(bindingSite, isMutable) ->
            ctx.Binding.Set(
                useKey,
                {
                    BindingSite = bindingSite
                    IsInline = false
                    IsMutable = isMutable
                }
            )
        | ValueNone ->
            match ctx.Provider.TryLookup name with
            | ValueSome _ -> ()
            | ValueNone ->
                // DU ctor references resolve through `ctx.CtorIndex` in
                // Unification, not through `ctx.Binding`. Same for
                // class names used as ctor-as-function (`Point(3, 4)`)
                // — they live in `ctx.ClassTypes`. Suppress the
                // "Unresolved identifier" diagnostic so neither
                // surfaces as unresolved.
                if ctx.CtorIndex.ContainsKey name || ctx.ClassTypes.ContainsKey name then
                    ()
                else
                    ctx.Diagnostics.Add
                        {
                            Key = useKey
                            Message = sprintf "Unresolved identifier: %s" name
                            Severity = Error
                        }

    /// True if `name` could only be a constructor reference in pattern
    /// position: starts with an uppercase letter AND is registered in
    /// `ctx.CtorIndex`. F# spec convention is to treat uppercase-leading
    /// pattern idents as ctor references, but we additionally require a
    /// registry hit so unrelated uppercase binders (`let X = 1; match v
    /// with | X -> …` in code that has no DU named X) still bind. Empty
    /// strings (virtual tokens) never match.
    let private isCtorName (ctx: PassContext) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && ctx.CtorIndex.ContainsKey name

    /// Every (name, NodeKey) pair introduced by a pattern. Recurses through
    /// parens, tuples, as-bindings, and type annotations; returns [] for
    /// patterns that bind nothing (Wildcard, Const).
    let rec private bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t when isCtorName ctx (ctx.NameOf t) ->
            // Uppercase-leading ident whose name matches a known nullary
            // ctor — reinterpret as a ctor pattern, binds nothing.
            []
        | Pat.NamedSimple t -> [ ctx.NameOf t, CstKeys.ofPat p ]
        | Pat.Wildcard _
        | Pat.Const _
        | Pat.EmptyBlock _ -> []
        | Pat.EnclosedBlock(pat = inner) -> bindingsOfPat ctx inner
        | Pat.Tuple(patterns = pats) -> [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Typed(pat = inner) -> bindingsOfPat ctx inner
        | Pat.As(pat = inner; ident = ident) -> (ctx.NameOf ident, CstKeys.ofPat p) :: bindingsOfPat ctx inner
        | Pat.Record(fieldPats = fieldPats) ->
            [ for FieldPat(pat = sub) in fieldPats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && isCtorName ctx (ctx.NameOf li.Idents.[li.Idents.Length - 1])
            ->
            // Ctor pattern: `Circle r`, `Rectangle(w, h)`, `Result1.Ok x`.
            // The head names bind nothing; sub-patterns introduce binders.
            [
                for sub in args do
                    yield! bindingsOfPat ctx sub
            ]
        | _ -> []

    // Lambda args / for-in / match-arm patterns can't carry `mutable`, so
    // every binder they introduce is immutable.
    let private extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            for n, k in bindingsOfPat ctx p do
                s <- Map.add n (k, false) s

        s

    /// Build the scope additions for a let-group. Also writes a binding-site
    /// self-entry to `ctx.Binding` for every binder — Validation's
    /// value-restriction loop iterates `ctx.Binding` and filters by
    /// `kv.Key = rb.BindingSite` to find one entry per binding.
    let private bindingsToScope (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : Scope =
        let mutable s = Map.empty

        for b in bindings do
            let isMut = b.mutableToken.IsSome

            for n, k in bindingsOfPat ctx b.headPat do
                s <- Map.add n (k, isMut) s

                ctx.Binding.Set(
                    k,
                    {
                        BindingSite = k
                        IsInline = b.inlineToken.IsSome
                        IsMutable = isMut
                    }
                )

        s

    let private visit (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.Ident tok -> resolveIdent ctx scope tok (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            resolveIdent ctx scope li.Idents.[0] (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            // Multi-segment LongIdent: this could be a chained field
            // access (`r.X`, `r.X.Y`) when the head segment is a local,
            // OR a qualified name (`Module.value`, `Math.PI`) that the
            // provider knows about. Try the local first — record-field
            // resolution happens in Unification once it sees the head's
            // resolved type.
            let head = li.Idents.[0]
            let headName = ctx.NameOf head

            let rec lookup (s: Scope list) =
                match s with
                | [] -> ValueNone
                | top :: rest ->
                    match Map.tryFind headName top with
                    | Some bs -> ValueSome bs
                    | None -> lookup rest

            match lookup scope with
            | ValueSome(bindingSite, isMutable) ->
                // Register the head segment as a use of the local binding.
                // The LongIdent expression itself is keyed under
                // NodeKind.ExprLongIdent; the head's binding entry uses the
                // ExprIdent kind on the head token so subsequent passes can
                // look up the receiver's type by the same key.
                ctx.Binding.Set(
                    NodeKey.ofToken head NodeKind.ExprIdent,
                    {
                        BindingSite = bindingSite
                        IsInline = false
                        IsMutable = isMutable
                    }
                )
            | ValueNone ->
                let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                match ctx.Provider.TryLookup qualName with
                | ValueSome _ -> ()
                | ValueNone ->
                    // `Result2.Ok` — two-segment qualified ctor reference.
                    // Resolves through `ctx.UnionTypes`; suppress here so
                    // Unification can pick it up.
                    let isQualifiedCtor =
                        li.Idents.Length = 2
                        && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])
                        && (let info = ctx.UnionTypes.[ctx.NameOf li.Idents.[0]]
                            let caseName = ctx.NameOf li.Idents.[1]
                            info.Cases |> Array.exists (fun c -> c.Name = caseName))

                    // `Math.Pi` / `Box.Empty` — two-segment qualified
                    // static member reference. Same suppression
                    // posture as ctors: Unification handles the type
                    // side.
                    let isQualifiedStatic =
                        li.Idents.Length = 2
                        && ctx.ClassTypes.ContainsKey(ctx.NameOf li.Idents.[0])
                        && (let info = ctx.ClassTypes.[ctx.NameOf li.Idents.[0]]
                            let memberName = ctx.NameOf li.Idents.[1]
                            info.Members |> Array.exists (fun m -> m.IsStatic && m.Name = memberName))

                    if isQualifiedCtor || isQualifiedStatic then
                        ()
                    else
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofExpr e
                                Message = sprintf "Unresolved qualified name: %s" qualName
                                Severity = Error
                            }
        | Expr.LongIdentOrOp lio ->
            // Operator-form long idents (`A.(+)`, `(*)`) still need their
            // own resolution story. Surface the gap rather than silently
            // skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Diagnostics.Add
                {
                    Key = CstKeys.ofExpr e
                    Message = sprintf "Operator-form qualified names not yet resolved (starting at '%s')" displayName
                    Severity = Error
                }
        | _ -> ()

    let private mkWalker (ctx: PassContext) : CstWalk.ExprWalker<Scope list> =
        {
            Visit = visit ctx
            EnterFun = fun scope argPats -> extendScope ctx argPats Map.empty :: scope
            EnterBindingRhs =
                fun scope isRec siblings b ->
                    // `let rec`: sibling names (including this binding's own name,
                    // so recursive self-reference resolves) are in scope for the RHS.
                    // Function-form: push parameter names on top of that.
                    let mutable s = scope

                    if isRec then
                        s <- bindingsToScope ctx siblings :: s

                    if not b.argumentPats.IsEmpty then
                        s <- extendScope ctx b.argumentPats Map.empty :: s

                    s
            EnterLetBody = fun scope bindings -> bindingsToScope ctx bindings :: scope
            EnterForTo =
                fun scope ident ->
                    let name = ctx.NameOf ident
                    let key = CstKeys.ofForToVar ident
                    Map.ofList [ name, (key, false) ] :: scope
            EnterForIn =
                fun scope pat ->
                    let scopeMap =
                        bindingsOfPat ctx pat |> List.map (fun (n, k) -> n, (k, false)) |> Map.ofList

                    scopeMap :: scope
            EnterMatchArm =
                fun scope pat ->
                    let scopeMap =
                        bindingsOfPat ctx pat |> List.map (fun (n, k) -> n, (k, false)) |> Map.ofList

                    scopeMap :: scope
        }

    /// Extract a `Typar`'s source-text name, dropping the leading `'` or
    /// `^` (which live on a separate token). Anon (`_`) typars don't
    /// participate in scope — return ValueNone so callers can skip them.
    let private typarName (ctx: PassContext) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome(ctx.NameOf id)
        | Typar.Anon _ -> ValueNone

    /// Declared typars for a `TypeName`, in source order: prefix typars
    /// (`'a Box`) first, then suffix typars (`Box<'a, 'b>`). Skips
    /// anonymous typars (they can't participate in a name-keyed scope).
    /// Used by both NameResolution (to mint `TypeParams`) and
    /// Unification (to rebuild the scope when filling field types).
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

    /// Extract the `when 'a : ...` clause attached to a `TypeName`'s
    /// `TyparDefns`, if any. Captured onto the registry entry so
    /// Unification's fill-in pass can attach each constraint to the
    /// prototype TyVars without re-walking the CST.
    let private typarConstraintsOfTypeName (tn: TypeName<SyntaxToken>) : TyparConstraints<SyntaxToken> voption =
        let (TypeName(typarDefns = td)) = tn

        match td with
        | ValueSome(TyparDefns(constraints = ValueSome tc)) -> ValueSome tc
        | _ -> ValueNone

    /// Mint a prototype TyVar per declared typar name. Each prototype is
    /// stored on the registry entry and substituted out at every use site
    /// — two `Box<…>` instantiations therefore share no variables.
    let private mkTypeParams (names: string list) : (string * TypeVar) list =
        [
            for n in names ->
                let tv = TypeVar()
                tv.Level <- 0
                n, tv
        ]

    /// Stamp `RecordTypeInfo` entries for every `TypeDefn.Record` in this
    /// group. Field types start as placeholder TyVars; Unification fills
    /// them in once `RecordTypes` is fully populated, so a record's field
    /// type can reference another record declared elsewhere in the same
    /// file. Duplicate single-segment names diagnose here — first wins.
    let private registerRecordTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Record(typeName = tn; fields = fields) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok

                if
                    ctx.RecordTypes.ContainsKey name
                    || ctx.UnionTypes.ContainsKey name
                    || ctx.AbbreviationTypes.ContainsKey name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken nameTok NodeKind.DeclType
                            Message = sprintf "Duplicate type definition: %s" name
                            Severity = Error
                        }
                else
                    let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)

                    let fieldInfos =
                        [|
                            for f in fields do
                                let (RecordField(mutableToken = mt; ident = id)) = f
                                let fName = ctx.NameOf id
                                // Placeholder TyVar — Unification stamps the
                                // real translated type via Link once the
                                // registry is fully populated. Stamping a
                                // fresh TyVar (rather than a TyConst) keeps
                                // the late binding cheap and re-uses the
                                // existing unification machinery.
                                let tv = TypeVar()
                                tv.Level <- 0
                                yield RecordFieldInfo(fName, TyVar tv, mt.IsSome, NodeKey.ofToken id NodeKind.DeclType)
                        |]

                    let info =
                        RecordTypeInfo(
                            name,
                            typeParams,
                            fieldInfos,
                            NodeKey.ofToken nameTok NodeKind.DeclType,
                            typarConstraintsOfTypeName tn
                        )

                    ctx.RecordTypes.[name] <- info

                    for fi in fieldInfos do
                        match ctx.FieldIndex.TryGetValue fi.Name with
                        | true, infos -> ctx.FieldIndex.[fi.Name] <- info :: infos
                        | false, _ -> ctx.FieldIndex.[fi.Name] <- [ info ]
        | _ -> ()

    let private registerRecordTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerRecordTypeDefn ctx td
        | _ -> ()

    /// Pull a ctor case's name + arity + per-field names from
    /// `UnionTypeCaseData`. GADT cases are out of scope for v1 — they
    /// emit a diagnostic so subsequent uses don't cascade.
    let private inspectCaseData
        (ctx: PassContext)
        (diagKey: NodeKey)
        (data: UnionTypeCaseData<SyntaxToken>)
        : (string * int * string voption[]) voption =
        let nameOfHead (head: IdentOrOp<SyntaxToken>) : string =
            match head with
            | IdentOrOp.Ident t -> ctx.NameOf t
            | _ -> ""

        match data with
        | UnionTypeCaseData.Nullary(name = head) ->
            let n = nameOfHead head

            if n.Length = 0 then ValueNone else ValueSome(n, 0, [||])
        | UnionTypeCaseData.Nary(name = head; fields = fields) ->
            let n = nameOfHead head

            if n.Length = 0 then
                ValueNone
            else
                let names =
                    [|
                        for f in fields ->
                            match f with
                            | UnionTypeField.Named(ident = id) -> ValueSome(ctx.NameOf id)
                            | UnionTypeField.Unnamed _ -> ValueNone
                    |]

                ValueSome(n, fields.Length, names)
        | UnionTypeCaseData.GadtNary _
        | UnionTypeCaseData.GadtNullary _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = "GADT-style union cases are not yet supported"
                    Severity = Error
                }

            ValueNone

    /// Stamp `UnionTypeInfo` entries for every `TypeDefn.Union` in this
    /// group. Mirrors `registerRecordTypeDefn`: field types start as
    /// placeholder TyVars; Unification's pre-pass fills them in once the
    /// registry is fully populated.
    let private registerUnionTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Union(typeName = tn; cases = cases) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

                if ctx.UnionTypes.ContainsKey name || ctx.RecordTypes.ContainsKey name then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Severity = Error
                        }
                else
                    let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)

                    let caseInfos =
                        [|
                            for UnionTypeCase(data = data) in cases do
                                match inspectCaseData ctx declKey data with
                                | ValueSome(caseName, arity, fieldNames) ->
                                    let fieldTys =
                                        Array.init
                                            arity
                                            (fun _ ->
                                                let tv = TypeVar()
                                                tv.Level <- 0
                                                TyVar tv
                                            )

                                    yield UnionCaseInfo(caseName, name, fieldTys, fieldNames, declKey)
                                | ValueNone -> ()
                        |]

                    let info =
                        UnionTypeInfo(name, typeParams, caseInfos, declKey, typarConstraintsOfTypeName tn)

                    ctx.UnionTypes.[name] <- info

                    for c in caseInfos do
                        match ctx.CtorIndex.TryGetValue c.Name with
                        | true, infos -> ctx.CtorIndex.[c.Name] <- c :: infos
                        | false, _ -> ctx.CtorIndex.[c.Name] <- [ c ]
        | _ -> ()

    let private registerUnionTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerUnionTypeDefn ctx td
        | _ -> ()

    /// Stamp an `AbbreviationInfo` entry for every `TypeDefn.Abbrev` in
    /// this group. Body is left unfilled (`Status = NotFilled`);
    /// Unification's `fillAbbreviationBodies` pre-pass forces each body
    /// once every type registration is complete, so an abbreviation's
    /// RHS can reference any other type in the same file regardless of
    /// declaration order.
    let private registerAbbreviationDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
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
                    ctx.RecordTypes.ContainsKey name
                    || ctx.UnionTypes.ContainsKey name
                    || ctx.AbbreviationTypes.ContainsKey name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Severity = Error
                        }
                else
                    let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)

                    let info =
                        AbbreviationInfo(name, typeParams, rhs, declKey, typarConstraintsOfTypeName tn)

                    ctx.AbbreviationTypes.[name] <- info
        | _ -> ()

    let private registerAbbreviationTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerAbbreviationDefn ctx td
        | _ -> ()

    /// Walk a `PrimaryConstrArgs` and collect parameter names. v1 accepts
    /// only simple patterns: `Pat.NamedSimple`, `Pat.Typed (NamedSimple, t)`,
    /// `Pat.Tuple` of those, possibly enclosed. Anything else surfaces a
    /// diagnostic and contributes no parameters.
    let private extractCtorParams
        (ctx: PassContext)
        (declKey: NodeKey)
        (pcOpt: PrimaryConstrArgs<SyntaxToken> voption)
        : ClassCtorParamInfo[] =
        match pcOpt with
        | ValueNone -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueNone)) -> [||]
        | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) ->
            let results = ResizeArray<ClassCtorParamInfo>()

            let rec walk (p: Pat<SyntaxToken>) =
                match p with
                | Pat.NamedSimple id ->
                    let name = ctx.NameOf id
                    // Use the ident token's offset under a synthetic kind
                    // so the param's binding-site key is distinct from any
                    // collision with a regular Pat.NamedSimple at the same
                    // offset.
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
                    // Point the diagnostic at the offending sub-pattern
                    // when we can build a key for it; fall back to the
                    // class's `declKey` for shapes that don't have a
                    // CstKeys arm yet.
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
                            Severity = Error
                        }

            walk p
            results.ToArray()

    /// Pull a member's name from its head pattern (`b.headPat`). Members
    /// have `this.M`-shaped head patterns parsed as `Pat.NamedSimple` for
    /// the member-name token; the `this` (or alias) is in `MethodOrPropDefn`'s
    /// `ident` field, not part of the head pattern. Returns `ValueNone`
    /// when we can't extract a single-segment name (unsupported shape).
    let private memberNameOf (ctx: PassContext) (b: Binding<SyntaxToken>) : (string * SyntaxToken) voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id, id)
            | Pat.EnclosedBlock(pat = inner) -> walk inner
            | Pat.Typed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    /// Stamp `ClassTypeInfo` entries for every `TypeDefn.Class` (or
    /// `TypeDefn.Anon` — the parser emits `Anon` for the bare
    /// `type C(...) = member ...` form without an explicit `class`/`end`
    /// wrapper). Member types are placeholder TyVars at this point;
    /// Unification's `fillClassMembers` pre-pass walks each member body
    /// and links the placeholders to the inferred type.
    let private registerClassTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        let common =
            match td with
            | TypeDefn.Class(typeName = tn; primaryConstr = pc; asDefn = asD; body = body) ->
                ValueSome(tn, pc, asD, body)
            | TypeDefn.Anon(typeName = tn; primaryConstr = pc; asDefn = asD; body = body) ->
                ValueSome(tn, pc, asD, body)
            | _ -> ValueNone

        match common with
        | ValueNone -> ()
        | ValueSome(tn, pc, asD, body) ->
            let (TypeName(ident = nameLi)) = tn

            if nameLi.Idents.Length <> 1 then
                ()
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

                if
                    ctx.RecordTypes.ContainsKey name
                    || ctx.UnionTypes.ContainsKey name
                    || ctx.AbbreviationTypes.ContainsKey name
                    || ctx.ClassTypes.ContainsKey name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Severity = Error
                        }
                else
                    let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
                    let ctorParams = extractCtorParams ctx declKey pc

                    let memberInfos = ResizeArray<ClassMemberInfo>()

                    for el in body.elements do
                        match el with
                        | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
                            let isStatic = s.IsSome

                            match d with
                            | MethodOrPropDefn.Method(defn = b) ->
                                match memberNameOf ctx b with
                                | ValueSome(mName, mTok) ->
                                    let tv = TypeVar()
                                    tv.Level <- 0
                                    let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                                    memberInfos.Add(
                                        ClassMemberInfo(mName, ClassMemberKind.Method, isStatic, TyVar tv, mKey)
                                    )
                                | ValueNone -> ()
                            | MethodOrPropDefn.Property(defn = b) ->
                                match memberNameOf ctx b with
                                | ValueSome(mName, mTok) ->
                                    let tv = TypeVar()
                                    tv.Level <- 0
                                    let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                                    memberInfos.Add(
                                        ClassMemberInfo(mName, ClassMemberKind.Property, isStatic, TyVar tv, mKey)
                                    )
                                | ValueNone -> ()
                            | MethodOrPropDefn.AutoProperty(ident = id) ->
                                let mName = ctx.NameOf id
                                let tv = TypeVar()
                                tv.Level <- 0
                                let mKey = NodeKey.ofToken id NodeKind.PatIdent

                                memberInfos.Add(
                                    ClassMemberInfo(mName, ClassMemberKind.Property, isStatic, TyVar tv, mKey)
                                )
                            | MethodOrPropDefn.PropertyWithGetSet _
                            | MethodOrPropDefn.AbstractSignature _ ->
                                ctx.Diagnostics.Add
                                    {
                                        Key = declKey
                                        Message = "This member kind is not yet supported"
                                        Severity = Error
                                    }
                        | TypeDefnElement.Member(MemberDefn.Value _)
                        | TypeDefnElement.Member(MemberDefn.AdditionalConstructor _) ->
                            ctx.Diagnostics.Add
                                {
                                    Key = declKey
                                    Message = "This member kind is not yet supported"
                                    Severity = Error
                                }
                        | TypeDefnElement.InterfaceImpl _
                        | TypeDefnElement.InterfaceSpec _
                        | TypeDefnElement.Inherit _ ->
                            ctx.Diagnostics.Add
                                {
                                    Key = declKey
                                    Message = "Inheritance / interfaces are not yet supported"
                                    Severity = Error
                                }

                    let thisName =
                        match asD with
                        | ValueSome(AsDefn(ident = id)) -> ctx.NameOf id
                        | ValueNone -> "this"

                    let thisKey = NodeKey.ofSynthetic declKey.Offset NodeKind.SynthThisBinding

                    let members = memberInfos.ToArray()

                    let info =
                        ClassTypeInfo(name, typeParams, ctorParams, members, declKey, thisName, thisKey)

                    ctx.ClassTypes.[name] <- info

                    for m in members do
                        match ctx.ClassMemberIndex.TryGetValue m.Name with
                        | true, lst -> ctx.ClassMemberIndex.[m.Name] <- (info, m) :: lst
                        | false, _ -> ctx.ClassMemberIndex.[m.Name] <- [ (info, m) ]

    let private registerClassTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                registerClassTypeDefn ctx td
        | _ -> ()

    /// Walk every class member body with a scope that binds `this`
    /// (or the user-supplied `as` alias) and every primary-constructor
    /// argument. Writes a binding-site self-entry to `ctx.Binding` for
    /// each (mirrors `bindingsToScope` for module-level lets).
    /// Member-body walks share one `Scope` per class; sibling members
    /// in the same class can therefore reference one another only via
    /// `this.OtherMember` (member names are not in lexical scope).
    let private walkClassBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        let bodyOf (td: TypeDefn<SyntaxToken>) =
            match td with
            | TypeDefn.Class(typeName = TypeName(ident = nameLi); body = body)
            | TypeDefn.Anon(typeName = TypeName(ident = nameLi); body = body) when nameLi.Idents.Length = 1 ->
                ValueSome(ctx.NameOf nameLi.Idents.[0], body)
            | _ -> ValueNone

        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match bodyOf td with
                | ValueSome(name, body) ->
                    match ctx.ClassTypes.TryGetValue name with
                    | true, info ->
                        // Build the class-body scope: `this` + every
                        // ctor param. Each entry's binding-site key is
                        // the ident token's PatIdent offset (ctor params)
                        // or the class's synthetic ThisKey (this).
                        let mutable scopeMap: Scope = Map.empty
                        scopeMap <- Map.add info.ThisName (info.ThisKey, false) scopeMap

                        ctx.Binding.Set(
                            info.ThisKey,
                            {
                                BindingSite = info.ThisKey
                                IsInline = false
                                IsMutable = false
                            }
                        )

                        for p in info.CtorParams do
                            scopeMap <- Map.add p.Name (p.DeclKey, false) scopeMap

                            ctx.Binding.Set(
                                p.DeclKey,
                                {
                                    BindingSite = p.DeclKey
                                    IsInline = false
                                    IsMutable = false
                                }
                            )

                        // Instance scope: `this` + ctor params.
                        // Static scope: empty (statics don't see `this`
                        // or ctor args — F# class members spec §8.7).
                        let instanceScope = [ scopeMap ]
                        let staticScope: Scope list = [ Map.empty ]

                        for el in body.elements do
                            match el with
                            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
                                let scope = if s.IsSome then staticScope else instanceScope

                                match d with
                                | MethodOrPropDefn.Method(defn = b)
                                | MethodOrPropDefn.Property(defn = b) ->
                                    // Mirror how module-level lets enter
                                    // a binding RHS: extend scope with
                                    // any argument-pattern binders so
                                    // method parameters resolve from
                                    // inside the body. The headPat
                                    // (member name) doesn't enter scope
                                    // — class members are accessed via
                                    // `this.M`, not as lexical names.
                                    let mutable inner = scope

                                    if not b.argumentPats.IsEmpty then
                                        inner <- extendScope ctx b.argumentPats Map.empty :: inner

                                    CstWalk.iterExpr walker inner b.expr
                                | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker scope e
                                | _ -> ()
                            | _ -> ()
                    | false, _ -> ()
                | ValueNone -> ()
        | _ -> ()

    let private walkModuleElem
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (scope: Scope list)
        (m: ModuleElem<SyntaxToken>)
        : Scope list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(isRec = isRec; bindings = bindings)) ->
            let isRecursive = isRec.IsSome

            for b in bindings do
                let rhsScope = walker.EnterBindingRhs scope isRecursive bindings b
                CstWalk.iterExpr walker rhsScope b.expr
            // Extend the topmost scope so later module elements can see these
            // bindings. `bindingsToScope` writes binding-site self-entries to
            // ctx.Binding as a side effect — same path used by EnterLetBody.
            let newEntries = bindingsToScope ctx bindings

            match scope with
            | [] -> [ newEntries ]
            | top :: rest ->
                let merged = (top, newEntries) ||> Map.fold (fun acc k v -> Map.add k v acc)
                merged :: rest
        | ModuleElem.Expression e ->
            CstWalk.iterExpr walker scope e
            scope
        | ModuleElem.Type _ -> scope
        | _ -> scope

    let private walkElems
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (elems: ModuleElems<SyntaxToken>)
        =
        // Pre-pass: register every record / union type so subsequent
        // expression walks (and Unification) can resolve literals /
        // field accesses / ctor uses against the registry. Mirrors the
        // let-rec collect-then-infer ordering inside a module. Records
        // and unions are independent in v1 (no module/namespace types),
        // so the order between them doesn't matter — but both must
        // finish before `bindingsOfPat` runs on any pattern, since the
        // ctor-vs-binder disambiguation reads `ctx.CtorIndex`.
        for m in elems do
            registerRecordTypes ctx m

        for m in elems do
            registerUnionTypes ctx m

        for m in elems do
            registerAbbreviationTypes ctx m

        for m in elems do
            registerClassTypes ctx m

        // Class member bodies are not walked by `walkModuleElem` (it
        // skips `ModuleElem.Type`). Walk them here with each class's
        // own scope (`this` + ctor params) so member-body idents have
        // Binding entries before Unification types them.
        for m in elems do
            walkClassBodies ctx walker m

        let mutable scope = [ Map.empty ]

        for m in elems do
            scope <- walkModuleElem ctx walker scope m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx

        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx walker elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx walker elems
        | ImplementationFile.Namespaces _ -> ()
