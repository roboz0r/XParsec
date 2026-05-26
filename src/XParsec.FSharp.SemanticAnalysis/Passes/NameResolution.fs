namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared populated.
// Post: ctx.Binding populated for every ident-use site that resolves to a
//       local binding. Unresolved names that the provider also doesn't know
//       become Error diagnostics.
//
// Recursion is delegated to CstWalk.iterExpr; the walker thread-restores
// scope automatically at each recursive boundary, so the caller's scope is
// never polluted by a lambda/let body's locals.
//
// Notes for the tiny subset:
//   - Operators inside InfixApp / PrefixApp are NOT resolved here. Desugar
//     records them as DesugaredForm.OpName, and Unification consults the
//     provider directly when typing the application.
//   - External-symbol resolution writes NO entry to ctx.Binding; Unification
//     re-queries the provider when it sees a missing binding entry.
//   - IsInline is always false for the tiny subset. It'll get a real value
//     when the inline keyword is handled.
//   - IsMutable mirrors the binding's `mutableToken`, propagated to every
//     use-site entry so Validation's assignment check can `ctx.Binding[lhsKey]`
//     directly. The binding site also gets a self-entry (`BindingSite = key`)
//     so Validation's value-restriction loop can iterate mutable bindings by
//     filtering `kv.Key = rb.BindingSite`.

module NameResolution =

    /// Mutability propagates from the scope entry to every use-site
    /// `ResolvedBinding` that resolves through it, so downstream passes
    /// (Validation's immutable-assignment check) don't need a second hop.
    type private Scope = Map<string, NodeKey * bool>

    /// True if `name` (a possibly-dotted path) resolves as an *external type* at
    /// some small arity — i.e. it's a type reference used as a static-member-access
    /// receiver (`EqualityComparer<int>.Default`), not an unresolved value. The
    /// receiver's arity lives on the enclosing `Expr.TypeApp`, which this name's
    /// own `visit` doesn't see, so probe a bounded arity range (results are cached
    /// by the provider). symbol-resolution-plan §7.2, P3.
    let private resolvesAsExternalType (ctx: PassContext) (name: string) : bool =
        // The probe accepts the name as-is or at any small arity; `tryQualify`
        // applies the `open` prefixes in scope, so a short `EqualityComparer`
        // (under `open System.Collections.Generic`) resolves to the arity-suffixed
        // qualified name. (symbol-resolution-handoff.md, open-resolution.)
        let probe n =
            (ctx.Provider.TryLookupType n |> ValueOption.isSome)
            || [ 1; 2; 3; 4 ]
               |> List.exists (fun a -> ctx.Provider.TryLookupType(sprintf "%s`%d" n a) |> ValueOption.isSome)

        OpenScope.tryQualify ctx.OpenScope probe name |> ValueOption.isSome

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
            match OpenScope.tryQualify ctx.OpenScope (fun n -> ctx.Provider.TryLookup n |> ValueOption.isSome) name with
            | ValueSome _ -> ()
            | ValueNone ->
                // DU ctor references resolve through `ctx.CtorIndex` in
                // Unification, not `ctx.Binding`; class names used as
                // ctor-as-function (`Point(3, 4)`) live in `ctx.ClassTypes`.
                // An external type name used as a static-access receiver resolves
                // through the provider in Unification, not as a value. Suppress the
                // "Unresolved identifier" diagnostic for all three.
                if
                    ctx.CtorIndex.ContainsKey name
                    || ctx.ClassTypes.ContainsKey name
                    || resolvesAsExternalType ctx name
                then
                    ()
                else
                    ctx.Diagnostics.Add
                        {
                            Key = useKey
                            Message = sprintf "Unresolved identifier: %s" name
                            Severity = Error
                        }

    /// True if `name` is a constructor reference in pattern position.
    /// F# spec convention treats uppercase-leading pattern idents as ctor
    /// references, but we additionally require a registry hit so unrelated
    /// uppercase binders (`let X = 1; match v with | X -> …` in code with
    /// no DU named X) still bind. Empty strings (virtual tokens) never match.
    let private isCtorName (ctx: PassContext) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && ctx.CtorIndex.ContainsKey name

    /// Every (name, NodeKey) pair introduced by a pattern; [] for patterns
    /// that bind nothing (Wildcard, Const, nullary ctors).
    let rec private bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t when isCtorName ctx (ctx.NameOf t) ->
            // Uppercase-leading ident matching a known nullary ctor —
            // reinterpret as a ctor pattern, binds nothing.
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
            // Ctor pattern (`Circle r`, `Result1.Ok x`): head binds nothing,
            // sub-patterns introduce binders.
            [
                for sub in args do
                    yield! bindingsOfPat ctx sub
            ]
        | Pat.Op io ->
            // Operator-named binding head (`let (=) x y = …`): bind the operator's
            // compiled name (`op_Equality`) so the binding site records a
            // `ctx.Binding` self-entry. Use sites resolve through Desugar→External,
            // not this scope entry, but Validation's per-binding loop expects one.
            match Desugar.opPatCompiledName ctx.NameOf io with
            | ValueSome n -> [ n, CstKeys.ofPat p ]
            | ValueNone -> []
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
            // Multi-segment LongIdent is either a chained field access
            // (`r.X.Y`, head is a local) or a qualified name (`Math.PI`,
            // provider knows it). Try the local first — record-field
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
                // The LongIdent expression is keyed under ExprLongIdent; the
                // head's binding entry uses the ExprIdent kind on the head
                // token so subsequent passes can look up the receiver's type
                // by the same key.
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

                match
                    OpenScope.tryQualify
                        ctx.OpenScope
                        (fun n -> ctx.Provider.TryLookup n |> ValueOption.isSome)
                        qualName
                with
                | ValueSome _ -> ()
                | ValueNone ->
                    // `Result2.Ok` — two-segment qualified ctor reference,
                    // resolves through `ctx.UnionTypes`; suppress so
                    // Unification can pick it up.
                    let isQualifiedCtor =
                        li.Idents.Length = 2
                        && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])
                        && (let info = ctx.UnionTypes.[ctx.NameOf li.Idents.[0]]
                            let caseName = ctx.NameOf li.Idents.[1]
                            info.Cases |> Array.exists (fun c -> c.Name = caseName))

                    // `Math.Pi` / `Box.Empty` — two-segment qualified static
                    // member reference. Same suppression posture as ctors.
                    // Union augmentation static members (`Lst.Empty`, P3d.3)
                    // suppress the same way.
                    let isQualifiedStatic =
                        li.Idents.Length = 2
                        && (let typeName = ctx.NameOf li.Idents.[0]
                            let memberName = ctx.NameOf li.Idents.[1]

                            let staticIn (members: ClassMemberInfo[]) =
                                members |> Array.exists (fun m -> m.IsStatic && m.Name = memberName)

                            (ctx.ClassTypes.ContainsKey typeName
                             && staticIn ctx.ClassTypes.[typeName].Members)
                            || (ctx.UnionTypes.ContainsKey typeName
                                && staticIn ctx.UnionTypes.[typeName].Members))

                    // A fully-qualified external type used as a static-access
                    // receiver (`System.Collections.Generic.EqualityComparer<int>`)
                    // resolves through the provider in Unification, not as a value.
                    //
                    // A non-generic external static member folds into one LongIdent
                    // (`System.Console.Out`), so the receiver type isn't the whole
                    // `qualName` — its *prefix* (all but the last segment) is. If that
                    // resolves as an external type, leave the member to Unification's
                    // `tryExternalStaticLongIdent`. (It falls through silently when the
                    // tail isn't an accessible member — e.g. a not-yet-modelled field —
                    // so suppression here doesn't manufacture a member that isn't there.)
                    let isExternalStaticMember =
                        li.Idents.Length >= 2
                        && (let prefix =
                                seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                                |> String.concat "."

                            resolvesAsExternalType ctx prefix)

                    if
                        isQualifiedCtor
                        || isQualifiedStatic
                        || resolvesAsExternalType ctx qualName
                        || isExternalStaticMember
                    then
                        ()
                    else
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofExpr e
                                Message = sprintf "Unresolved qualified name: %s" qualName
                                Severity = Error
                            }
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) when
            (Desugar.symbolicOpCompiledName op.Token |> ValueOption.isSome)
            ->
            // `(+)` and friends used as a value resolve through the provider
            // in Unification (no local binding), so not an unresolved-name error.
            ()
        | Expr.LongIdentOrOp lio ->
            // TODO: operator-form long idents (`A.(+)`, `(*)`) need their own
            // resolution story. Surface the gap rather than silently skipping.
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
                    // `let rec`: sibling names (including this binding's own
                    // name, so recursive self-reference resolves) are in scope
                    // for the RHS. Function-form: push parameter names on top.
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

    /// A `Typar`'s source-text name; the leading `'`/`^` live on a separate
    /// token. Anon (`_`) typars don't participate in scope — ValueNone.
    let private typarName (ctx: PassContext) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome(ctx.NameOf id)
        | Typar.Anon _ -> ValueNone

    /// Declared typars for a `TypeName`, in source order: prefix typars
    /// (`'a Box`) first, then suffix typars (`Box<'a, 'b>`). Skips anonymous
    /// typars (can't participate in a name-keyed scope). Used by both
    /// NameResolution (mint `TypeParams`) and Unification (rebuild the scope
    /// when filling field types).
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

    /// The `when 'a : ...` clause attached to a `TypeName`, if any. Captured
    /// onto the registry entry so Unification's fill-in pass can attach each
    /// constraint to the prototype TyVars without re-walking the CST.
    let private typarConstraintsOfTypeName (tn: TypeName<SyntaxToken>) : TyparConstraints<SyntaxToken> voption =
        let (TypeName(typarDefns = td)) = tn

        match td with
        | ValueSome(TyparDefns(constraints = ValueSome tc)) -> ValueSome tc
        | _ -> ValueNone

    /// Mint a prototype TyVar per declared typar name. Each is stored on the
    /// registry entry and substituted out at every use site, so two `Box<…>`
    /// instantiations share no variables.
    let private mkTypeParams (names: string list) : (string * TypeVar) list =
        [
            for n in names ->
                let tv = TypeVar()
                tv.Level <- 0
                n, tv
        ]

    /// Stamp `RecordTypeInfo` entries for every `TypeDefn.Record`. Field types
    /// start as placeholder TyVars; Unification fills them once `RecordTypes`
    /// is fully populated, so a field type can reference another record
    /// declared elsewhere in the same file. Duplicate names diagnose here.
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
                                // Placeholder TyVar (not a TyConst) so Unification
                                // can Link the real type later, reusing the
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

                    // C-Attr: record's equality posture (records-handoff §1).
                    // Explicit `[<StructuralEquality>]` / `[<ReferenceEquality>]` /
                    // `[<NoEquality>]` wins; absence falls back to the records-plan
                    // §B4 default ⇒ `Structural` when every field is immutable,
                    // `Reference` otherwise. The verdict feeds both
                    // `Unification.checkConstraint` (use-site diagnosis for
                    // `NoEquality`) and the codegen triple gate (`Freeze` copies
                    // it onto `TTypeDecl.EqualitySupport`).
                    info.EqualitySupport <-
                        match Attributes.decodeEqualityAttributes ctx (Attributes.attributesOfTypeName tn) with
                        | ValueSome v -> v
                        | ValueNone ->
                            if fieldInfos |> Array.forall (fun fi -> not fi.IsMutable) then
                                EqualityVerdict.Structural
                            else
                                EqualityVerdict.Reference

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

    /// Map a union-case head to its case name. The operator-named cases that
    /// matter are FSharp.Core's list constructors — `([])` → `Empty`, `(::)`
    /// → `Cons`. Heads we can't name (`(*)`, range/active-pattern ops) yield
    /// `""`, which `inspectCaseData` reads as "drop this case".
    let private unionCaseName (ctx: PassContext) (head: IdentOrOp<SyntaxToken>) : string =
        match head with
        | IdentOrOp.Ident t -> ctx.NameOf t
        | IdentOrOp.ParenOp(opName = OpName.NilOp _) -> "Empty"
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) ->
            match ctx.NameOf op with
            | "::" -> "Cons"
            | s -> s
        | _ -> ""

    /// Pull a ctor case's name + arity + per-field names from
    /// `UnionTypeCaseData`. Handles plain forms, operator-named cases, and the
    /// explicit-return (GADT-syntax) forms FSharp.Core's list uses. The return
    /// type is treated as the declaring union; true GADTs (a return type
    /// refining the declaring typars) remain out of scope.
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

    /// Stamp `UnionTypeInfo` entries for every `TypeDefn.Union`. Mirrors
    /// `registerRecordTypeDefn`: field types start as placeholder TyVars;
    /// Unification's pre-pass fills them once the registry is populated.
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

                                    yield UnionCaseInfo(caseName, name, fieldTys, fieldNames, declKey)
                                | ValueNone -> ()
                        |]

                    let info =
                        UnionTypeInfo(name, typeParams, caseInfos, declKey, typarConstraintsOfTypeName tn)

                    // C-Attr: a union's equality posture defaults to `Structural`
                    // (records-handoff §1 / brainstorm §8). Explicit
                    // `[<ReferenceEquality>]` / `[<NoEquality>]` overrides the
                    // default.
                    info.EqualitySupport <-
                        match Attributes.decodeEqualityAttributes ctx (Attributes.attributesOfTypeName tn) with
                        | ValueSome v -> v
                        | ValueNone -> EqualityVerdict.Structural

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

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS (e.g.
    /// `(# "System.Int32" #)` → `"System.Int32"`). Parts are almost always a
    /// single `StringPart.Text`; any token-bearing part is stitched
    /// defensively. Mirrors `Freeze.stitchLiteralString`.
    let private ilIntrinsicString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()
        // TODO: Error handling for unsupported parts (Expr, InvalidText); they shouldn't proceed. Raise dianostics.
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

    /// Stamp an `AbbreviationInfo` entry for every `TypeDefn.Abbrev`. Body is
    /// left unfilled; Unification's `fillAbbreviationBodies` pre-pass forces
    /// each body once every type registration is complete, so an RHS can
    /// reference any other type in the same file regardless of declaration order.
    ///
    /// An abbrev whose RHS is `Type.ILIntrinsic` is the exception: it is a
    /// *primitive binding*, not a transparent alias. It's recorded in
    /// `IntrinsicReprTypes` (name → IL string) and kept out of
    /// `AbbreviationTypes`, so `translateType` resolves the name to `TyConst name`
    /// rather than eagerly expanding the RHS. See docs/self-host-rung1-plan.md
    /// ("The intrinsic-impl rule").
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
                    || ctx.IntrinsicReprTypes.ContainsKey name
                then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Severity = Error
                        }
                else
                    match rhs with
                    | Type.ILIntrinsic(instrParts = parts) ->
                        // Primitive binding: record the representation, register
                        // the name as a nominal intrinsic. Not a transparent abbrev.
                        ctx.IntrinsicReprTypes.[name] <- ilIntrinsicString ctx parts
                    | _ ->
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

    /// Collect constructor parameter names. v1 accepts only simple patterns
    /// (`NamedSimple`, `Typed (NamedSimple, t)`, `Tuple` of those, possibly
    /// enclosed); anything else surfaces a diagnostic and contributes nothing.
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
                    // Point the diagnostic at the offending sub-pattern when we
                    // can key it; fall back to `declKey` for shapes without a
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

    /// A member's name from its head pattern. `this.M`-shaped head patterns
    /// parse as `Pat.NamedSimple` for the member-name token; the `this` (or
    /// alias) is in `MethodOrPropDefn`'s `ident` field, not the head pattern.
    /// ValueNone when no single-segment name can be extracted.
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

    /// A member's *own* declared typars — the `<'C, …>` after the member name,
    /// in source order. Skips anonymous typars (can't key a name-scope).
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

    /// Extract `ClassMemberInfo` placeholders from a type body's / augmentation's
    /// member elements. Shared by class registration (`body.elements`) and union
    /// augmentation registration (`extensions.elements`) — same `TypeDefnElement`
    /// shape (P3d.3). Member types are placeholder TyVars here; Unification's
    /// `fillClassMembers` / `fillUnionMembers` links them.
    let private extractMembers
        (ctx: PassContext)
        (declKey: NodeKey)
        (elements: TypeDefnElement<SyntaxToken> seq)
        : ClassMemberInfo[] =
        let memberInfos = ResizeArray<ClassMemberInfo>()

        for el in elements do
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

                        memberInfos.Add(ClassMemberInfo(mName, ClassMemberKind.Method, isStatic, TyVar tv, mKey))
                    | ValueNone -> ()
                | MethodOrPropDefn.Property(defn = b) ->
                    match memberNameOf ctx b with
                    | ValueSome(mName, mTok) ->
                        let tv = TypeVar()
                        tv.Level <- 0
                        let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                        memberInfos.Add(ClassMemberInfo(mName, ClassMemberKind.Property, isStatic, TyVar tv, mKey))
                    | ValueNone -> ()
                | MethodOrPropDefn.AutoProperty(ident = id) ->
                    let mName = ctx.NameOf id
                    let tv = TypeVar()
                    tv.Level <- 0
                    let mKey = NodeKey.ofToken id NodeKind.PatIdent

                    memberInfos.Add(ClassMemberInfo(mName, ClassMemberKind.Property, isStatic, TyVar tv, mKey))
                | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(ident = idOrOp; typarDefns = tds)) ->
                    // Abstract method signature registers as a `Method` member
                    // with a placeholder TyVar; Unification links the resolved
                    // signature, Freeze surfaces the interface. (A `PropSig`
                    // abstract member is a property — out of scope for rung 1.)
                    match identOrOpNameTok ctx idOrOp with
                    | ValueSome(mName, mTok) ->
                        let tv = TypeVar()
                        tv.Level <- 0
                        let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                        let cmi = ClassMemberInfo(mName, ClassMemberKind.Method, isStatic, TyVar tv, mKey)

                        // The method's own `<'C, …>` typars get prototype TyVars
                        // so Unification scopes the signature against them and
                        // Freeze can surface them as `GenericMethodParameter`s.
                        cmi.MethodTypeParams <- mkTypeParams (memberTyparNames ctx tds)
                        memberInfos.Add cmi
                    | ValueNone -> ()
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

        memberInfos.ToArray()

    /// Stamp `ClassTypeInfo` entries for every `TypeDefn.Class` (or
    /// `TypeDefn.Anon` — the parser emits `Anon` for the bare
    /// `type C(...) = member ...` form without an explicit `class`/`end`).
    /// Member types are placeholder TyVars; Unification's `fillClassMembers`
    /// pre-pass walks each member body and links them to the inferred type.
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

                    let memberInfos =
                        ResizeArray<ClassMemberInfo>(extractMembers ctx declKey body.elements)

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

    /// Stamp augmentation members onto an already-registered `UnionTypeInfo`
    /// (P3d.3). Must run after `registerUnionTypes`; reads the union's
    /// `extensions.elements`. A v1 union has no primary ctor / `as` alias, so
    /// `this` is always named `"this"`.
    let private registerUnionMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(
                    typeName = TypeName(ident = nameLi); extensions = ValueSome(TypeExtensionElements(elements = elems))) when
                    nameLi.Idents.Length = 1
                    ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.UnionTypes.TryGetValue name with
                    | true, info ->
                        info.Members <- extractMembers ctx info.DeclKey elems
                        info.ThisKey <- NodeKey.ofSynthetic info.DeclKey.Offset NodeKind.SynthThisBinding
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()

    /// Walk every class member body with a scope that binds `this` (or the
    /// `as` alias) and every primary-constructor argument, writing a
    /// binding-site self-entry to `ctx.Binding` for each. Member names are NOT
    /// in lexical scope: sibling members reference one another only via
    /// `this.OtherMember`.
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

                        // Static scope is empty: statics don't see `this` or
                        // ctor args (F# class members spec §8.7).
                        let instanceScope = [ scopeMap ]
                        let staticScope: Scope list = [ Map.empty ]

                        for el in body.elements do
                            match el with
                            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
                                let scope = if s.IsSome then staticScope else instanceScope

                                match d with
                                | MethodOrPropDefn.Method(defn = b)
                                | MethodOrPropDefn.Property(defn = b) ->
                                    // Extend scope with argument-pattern binders
                                    // so method parameters resolve. The headPat
                                    // (member name) does NOT enter scope — members
                                    // are accessed via `this.M`, not lexically.
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

    /// Walk every union augmentation member body (P3d.3). Mirrors
    /// `walkClassBodies` but reads `extensions.elements` and binds only `this`
    /// (a v1 union has no primary-constructor arguments).
    let private walkUnionBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(
                    typeName = TypeName(ident = nameLi); extensions = ValueSome(TypeExtensionElements(elements = elems))) when
                    nameLi.Idents.Length = 1
                    ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.UnionTypes.TryGetValue name with
                    | true, info when not (Array.isEmpty info.Members) ->
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

                        let instanceScope = [ scopeMap ]
                        let staticScope: Scope list = [ Map.empty ]

                        for el in elems do
                            match el with
                            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
                                let scope = if s.IsSome then staticScope else instanceScope

                                match d with
                                | MethodOrPropDefn.Method(defn = b)
                                | MethodOrPropDefn.Property(defn = b) ->
                                    let mutable inner = scope

                                    if not b.argumentPats.IsEmpty then
                                        inner <- extendScope ctx b.argumentPats Map.empty :: inner

                                    CstWalk.iterExpr walker inner b.expr
                                | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker scope e
                                | _ -> ()
                            | _ -> ()
                    | _ -> ()
                | _ -> ()
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
            // `bindingsToScope` writes binding-site self-entries to ctx.Binding
            // as a side effect — same path used by EnterLetBody.
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
        (pairs: (ModuleElem<SyntaxToken> * OpenScope) list)
        =
        // Pre-pass: register every record / union type so subsequent
        // expression walks (and Unification) resolve against the registry.
        // Both must finish before `bindingsOfPat` runs on any pattern, since
        // the ctor-vs-binder disambiguation reads `ctx.CtorIndex`. Registration
        // resolves no external short names, so it ignores the per-element scope.
        for (m, _) in pairs do
            registerRecordTypes ctx m

        for (m, _) in pairs do
            registerUnionTypes ctx m

        for (m, _) in pairs do
            registerAbbreviationTypes ctx m

        for (m, _) in pairs do
            registerClassTypes ctx m

        // Union augmentation members (P3d.3) register after the union itself.
        for (m, _) in pairs do
            registerUnionMembers ctx m

        // `walkModuleElem` skips `ModuleElem.Type`, so class member bodies are
        // walked here with each class's own scope (`this` + ctor params), so
        // member-body idents have Binding entries before Unification types them.
        // `ctx.OpenScope` is set per element so a member body resolves short
        // external names against the `open`s in scope at that element.
        for (m, openScope) in pairs do
            ctx.OpenScope <- openScope
            walkClassBodies ctx walker m

        for (m, openScope) in pairs do
            ctx.OpenScope <- openScope
            walkUnionBodies ctx walker m

        let mutable scope = [ Map.empty ]

        for (m, openScope) in pairs do
            ctx.OpenScope <- openScope
            scope <- walkModuleElem ctx walker scope m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx
        // Seed the walk with the stable ambient prelude (empty today; the
        // referenced-contract auto-open set later — symbol-resolution-handoff.md, open-resolution).
        // `walkElems` overwrites `ctx.OpenScope` per element, so the seed is read
        // from `AmbientOpenScope`, not the scope it mutates.
        walkElems ctx walker (CstWalk.walkModuleTree ctx.NameOf ctx.AmbientOpenScope file)
