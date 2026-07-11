namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferLiterals
open UnificationInferResolve

module internal UnificationInferPat =

    /// The `'T list` type carrying `elemTy`, resolved exactly like a `[…]`
    /// literal (`Unification.listLiteralTy`): a program that declares its own
    /// `'T list` abbreviation (the self-host `list.fs`) expands eagerly to the
    /// union RHS; a bare program leaves the container flexible (a fresh TyVar
    /// registered in `ctx.ListLiterals`) for consumer-driven resolution.
    let private consListTy (ctx: PassContext) (key: NodeKey) (elemTy: SemType) : SemType =
        match ctx.Types.Abbreviation.TryGetValue "list" with
        | true, info ->
            forceFill ctx info
            expandAbbreviation ctx key info (EqArray.singleton elemTy)
        | false, _ ->
            let tv = freshTyVar ctx
            ctx.ListLiterals.Add(UnionFind.find tv, elemTy)
            TyVar tv

    let rec inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Each pattern node gets its own TypeVar keyed on its NodeKey; for
        // compound patterns the outer TypeVar is linked to the underlying
        // shape so a lookup against any pattern node returns the right type.
        let key = CstKeys.ofPat p

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.Types.CtorIndex.ContainsKey n
            ->
            // Uppercase-leading bare ident matching a known ctor —
            // reinterpret as a nullary ctor pattern. Multi-candidate names
            // require a qualifier; diagnose ambiguity, best-effort otherwise.
            let n = ctx.NameOf t
            let info, count = resolveCtorName ctx n

            match info with
            | ValueSome i when i.Fields.Length = 0 ->
                let unionInfo = TypeRegistry.unionOfCase ctx.Types i
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(unionInfo.Key, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
            | ValueSome i ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Constructor '%s' takes %d argument(s) but is used nullary in pattern position"
                                n
                                i.Fields.Length
                        Code = ""
                        Severity = Severity.Error
                    }

                let unionInfo = TypeRegistry.unionOfCase ctx.Types i
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(unionInfo.Key, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
            | ValueNone when count >= 2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf "Ambiguous constructor '%s'; declared in %d union types — add a qualifier" n count
                        Code = ""
                        Severity = Severity.Error
                    }

                TyVar(freshTv ctx key)
            | ValueNone -> TyVar(freshTv ctx key)
        | Pat.NamedSimple t & Stamped ctx.Resolution.ExternalUnionCaseStamp key uc ->
            // Nullary case of an *external* (referenced-package) union (`None`),
            // recognised upstream by NameResolution and read here by node key — the
            // cross-package analogue of the local nullary-ctor arm above. A bare RQA
            // case is NOT stamped (only its qualified form resolves), so it never
            // reaches this arm — it lands on the `Pat.NamedSimple _` binder arm below,
            // matching F#, which treats a bare uppercase RQA name in a pattern as a
            // fresh variable.
            let unionTy, fields = externalCasePattern ctx uc

            if fields.Length <> 0 then
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Constructor '%s' takes %d argument(s) but is used nullary in pattern position"
                                (ctx.NameOf t)
                                fields.Length
                        Code = ""
                        Severity = Severity.Error
                    }

            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome unionTy
            unionTy
        | Pat.NamedSimple _ ->
            // Use tvOf so a let-rec sibling whose TyVar was already lazy-minted
            // by a forward reference (or pre-allocated by inferBindingGroup)
            // is reused, not overwritten.
            TyVar(tvOf ctx key)
        | Pat.Op _ ->
            // An operator-named binding head (`let (=) x y = …`) introduces a
            // single name, exactly like a `Pat.NamedSimple`; its name is the
            // operator's compiled name (`op_Equality`), surfaced by Elaborate.
            TyVar(tvOf ctx key)
        | Pat.Named(argumentPats = args) & Stamped ctx.Resolution.ExternalEnumCaseStamp key enumKey ->
            // `| E.C1` external enum-case pattern (a TS-manifest enum), recognised upstream
            // by NameResolution and read here by node key. Types as the enum nominal
            // `TyEnum key` — the external mirror of the project-local enum arm below; the key
            // matches the `E.C1` expression access and an `(x: E)` annotation, so the
            // scrutinee unifies. Nullary, but any (ill-formed) sub-patterns are still walked
            // so their binders register.
            for sub in args do
                inferPat ctx sub |> ignore

            let ty = TyEnum enumKey
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome ty
            ty
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length = 2 && ctx.Types.Enum.ContainsKey(ctx.NameOf li.Idents.[0])
            ->
            // `| E.C1` — an enum-case constant pattern: the head names a
            // project-local enum, so the tail must be one of its cases. The
            // pattern's type is the enum nominal (`TyEnum Key`), NOT the underlying
            // int/string — so `inferRules`' `unify` against the scrutinee makes
            // `match (x: E)` check and `match (n: int) with | E.A` a type error
            // (the enum is a distinct nominal). An unknown case is a resolution
            // error, the pattern analogue of `InferIdentExpr`'s enum-expression arm.
            // Enum names are a separate registry, so this can't collide with the
            // class / union / ctor pattern heads handled below. Enum-case patterns
            // are nullary; any (ill-formed) sub-patterns are still walked so their
            // binders register.
            let einfo = ctx.Types.Enum.[ctx.NameOf li.Idents.[0]]
            let caseName = ctx.NameOf li.Idents.[1]

            if not (einfo.HasCase caseName) then
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Enum '%s' has no case '%s'" einfo.Name caseName
                        Code = ""
                        Severity = Severity.Error
                    }

            for sub in args do
                inferPat ctx sub |> ignore

            let ty = TyEnum einfo.Key
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome ty
            ty
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                last.Length > 0 && System.Char.IsUpper last.[0])
            && (li.Idents.Length = 1
                && ctx.Types.CtorIndex.ContainsKey(ctx.NameOf li.Idents.[0])
                || li.Idents.Length = 2
                   && ctx.Types.Union.ContainsKey(ctx.NameOf li.Idents.[0])
                   && (let info = ctx.Types.Union.[ctx.NameOf li.Idents.[0]]
                       let caseName = ctx.NameOf li.Idents.[1]
                       info.Cases |> Array.exists (fun c -> c.Name = caseName)))
            ->
            let info =
                if li.Idents.Length = 1 then
                    let name = ctx.NameOf li.Idents.[0]

                    match resolveCtorName ctx name with
                    | ValueSome i, _ -> ValueSome i
                    | ValueNone, count when count >= 2 ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message =
                                    sprintf
                                        "Ambiguous constructor '%s'; declared in %d union types — add a qualifier"
                                        name
                                        count
                                Code = ""
                                Severity = Severity.Error
                            }

                        ValueNone
                    | _ -> ValueNone
                else
                    let typeName = ctx.NameOf li.Idents.[0]
                    let caseName = ctx.NameOf li.Idents.[1]
                    resolveQualifiedCtor ctx typeName caseName

            match info with
            | ValueNone ->
                for sub in args do
                    inferPat ctx sub |> ignore

                TyVar(freshTv ctx key)
            | ValueSome i ->
                // The parser wraps multi-arg ctor patterns in
                // `EnclosedBlock(Tuple [...])`; flatten to the field list.
                let subPats =
                    if args.Length = 1 then
                        unwrapCtorArgPattern args.[0]
                    else
                        List.ofSeq args

                if subPats.Length <> i.Fields.Length then
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message =
                                sprintf
                                    "Constructor '%s' expects %d argument(s) but got %d"
                                    i.Name
                                    i.Fields.Length
                                    subPats.Length
                            Code = ""
                            Severity = Severity.Error
                        }

                let unionInfo = TypeRegistry.unionOfCase ctx.Types i
                let args, subst = freshNamedInstance ctx unionInfo.TypeParams
                let m = min subPats.Length i.Fields.Length

                for j = 0 to m - 1 do
                    let sub = subPats.[j]
                    let subTy = inferPat ctx sub
                    unify ctx (CstKeys.ofPat sub) subTy (substituteWith subst i.Fields.[j])

                // Walk any extra sub-patterns so binders still register.
                for j = m to subPats.Length - 1 do
                    inferPat ctx subPats.[j] |> ignore

                let ty = TyUnion(unionInfo.Key, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
        | Pat.Named(longIdent = li; argumentPats = args) & Stamped ctx.Resolution.ExternalUnionCaseStamp key uc ->
            // A case (with fields) of an *external* union (`Some x`, `Result.Ok x`),
            // bare or qualified — the cross-package analogue of the local-ctor
            // `Pat.Named` arm above. NameResolution recognised the head (applying the
            // qualifier discipline) and stamped it; read by node key here. Sub-patterns
            // unify against the case's declared field types in the union's fresh
            // instantiation.
            let caseName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
            let unionTy, fields = externalCasePattern ctx uc

            // The parser wraps multi-arg ctor patterns in
            // `EnclosedBlock(Tuple [...])`; flatten to the field list.
            let subPats =
                if args.Length = 1 then
                    unwrapCtorArgPattern args.[0]
                else
                    List.ofSeq args

            if subPats.Length <> fields.Length then
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Constructor '%s' expects %d argument(s) but got %d"
                                caseName
                                fields.Length
                                subPats.Length
                        Code = ""
                        Severity = Severity.Error
                    }

            let m = min subPats.Length fields.Length

            for j = 0 to m - 1 do
                let sub = subPats.[j]
                let subTy = inferPat ctx sub
                unify ctx (CstKeys.ofPat sub) subTy fields.[j]

            // Walk any extra sub-patterns so binders still register.
            for j = m to subPats.Length - 1 do
                inferPat ctx subPats.[j] |> ignore

            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome unionTy
            unionTy
        | Pat.Wildcard _ -> TyVar(freshTv ctx key)
        | Pat.Null _ ->
            // A `null` pattern matches a reference value. Leave the node type a
            // free TyVar so the scrutinee (a reference type — `TextWriter`,
            // `char[]`) pins it via `inferRules`' unify; the real F# nullability
            // constraint is deferred (feedback_relax_parser_defer_to_typecheck).
            TyVar(freshTv ctx key)
        | Pat.EnclosedBlock(lParen = ParenKind.List _; pat = inner) ->
            // `[a; b; c]` list-literal pattern ≡ `a :: b :: c :: []`: every
            // element shares one element type and the whole pattern is that list
            // type. A single-element `[a]` arrives as the bare element (no
            // semicolons → no `Pat.Elems` wrapper); `[]` is `Pat.EmptyBlock`.
            let elems =
                match inner with
                | Pat.Elems(pats = pats) -> List.ofSeq pats
                | single -> [ single ]

            let elemTy = TyVar(freshTyVar ctx)

            for e in elems do
                let eTy = inferPat ctx e
                unify ctx (CstKeys.ofPat e) eTy elemTy

            let listTy = consListTy ctx key elemTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome listTy
            listTy
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Attributed(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Tuple(patterns = pats) ->
            let elemTys = EqArray.ofSeq (seq { for p in pats -> inferPat ctx p })
            let tupleTy = TyTuple elemTys
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome tupleTy
            tupleTy
        | Pat.Const c ->
            let constTy = inferConst ctx c
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome constTy
            constTy
        | Pat.As(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Typed(pat = inner; typ = t) ->
            let innerTy = inferPat ctx inner
            let annTy = translateType ctx t
            // Annotation reconciliation (`x: int | string`): admits value→union but
            // stays symmetric `unify` for a nominal/`obj` annotation, so the binder
            // still grounds to its written type.
            unifyAnnotation ctx key innerTy annTy
            // Type provenance: a typed pattern `(x : T)` — parameter, `let`-binder, or
            // nested destructure — writes the binder's type explicitly. Attribute it to
            // the INNER binder's key (the `Pat.Typed` wrapper is erased in the TAST; a
            // consumer queries the `NamedSimple`), matching how a value binding marks its
            // `headPat`.
            ctx.MarkTypeDeclared(CstKeys.ofPat inner, annTy)
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome annTy
            annTy
        | Pat.TypeTestAs(typ = t; pat = inner) ->
            // `:? T as x` — the inner binder `x` sees the tested type `T`; the
            // pattern itself matches values of the scrutinee's type (left free so
            // the scrutinee, typically `obj`, pins it via `inferRules`' unify).
            // Stash the test type keyed on this node so Elaborate can carry it into
            // `TPat.TypeTestAs.testTy` for the `isinst` operand (mirrors the
            // `:?` *expression* form's `inferDynamicTypeTest`).
            let tgtTy = translateType ctx t
            ctx.Resolution.TypeTestTargets.Set(key, tgtTy)
            let innerTy = inferPat ctx inner
            unify ctx (CstKeys.ofPat inner) innerTy tgtTy
            // Type provenance: `:? T as x` writes the BINDER `x`'s type (the tested
            // `T`), not this pattern node's (which stays the scrutinee's free type).
            ctx.MarkTypeDeclared(CstKeys.ofPat inner, tgtTy)
            TyVar(freshTv ctx key)
        | Pat.TypeTest(typ = t) ->
            // `:? T` — the bare type-test (no `as`-binder). Same as `TypeTestAs`
            // minus the inner binder: stash the tested type for Elaborate's `isinst`
            // operand; the pattern matches the scrutinee's type (left free).
            let tgtTy = translateType ctx t
            ctx.Resolution.TypeTestTargets.Set(key, tgtTy)
            TyVar(freshTv ctx key)
        | Pat.EmptyBlock(lParen = ParenKind.List _) ->
            // `[]` pattern: a list whose element type is left free for the
            // scrutinee to pin (`match xs with [] -> …`).
            let elemTy = TyVar(freshTyVar ctx)
            let listTy = consListTy ctx key elemTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome listTy
            listTy
        | Pat.Cons(head = headPat; tail = tailPat) ->
            // `h :: t`: `h` is an element, `t` the same list type.
            let headTy = inferPat ctx headPat
            let listTy = consListTy ctx key headTy
            let tailTy = inferPat ctx tailPat
            unify ctx (CstKeys.ofPat tailPat) tailTy listTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome listTy
            listTy
        | Pat.EmptyBlock _ ->
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome ctx.Intrinsics.Unit
            ctx.Intrinsics.Unit
        | Pat.Or(left = leftPat; right = rightPat) ->
            // Here we only unify the alternatives' overall types for scrutinee
            // consistency. Binding or-patterns are unsupported — `ElaboratePatterns`
            // rejects any alternative that binds a name — so no name-set reconciliation
            // is needed.
            let leftTy = inferPat ctx leftPat
            let rightTy = inferPat ctx rightPat
            unify ctx key leftTy rightTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome leftTy
            leftTy
        | Pat.Record(fieldPats = fieldPats) ->
            let pairs =
                [
                    for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                        let q, n = fieldNameAndQualifier ctx li
                        q, n, sub
                ]

            let qualifier =
                pairs
                |> List.tryPick (fun (q, _, _) ->
                    match q with
                    | ValueSome q -> Some q
                    | _ -> None
                )

            let names = pairs |> List.map (fun (_, n, _) -> n)

            let candidate =
                match qualifier with
                | Some typeName ->
                    match ctx.Types.Record.TryGetValue typeName with
                    | true, info -> ValueSome info
                    | false, _ ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "Unknown record type qualifier: %s" typeName
                                Code = ""
                                Severity = Severity.Error
                            }

                        ValueNone
                | None ->
                    let cand, count = findUniqueRecordByFieldSet ctx names

                    match cand with
                    | ValueSome _ -> cand
                    | ValueNone ->
                        if count = 0 then
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                    Code = ""
                                    Severity = Severity.Error
                                }
                        else
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf
                                            "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                            count
                                    Code = ""
                                    Severity = Severity.Error
                                }

                        ValueNone

            match candidate with
            | ValueNone ->
                // Walk sub-patterns so binders register as free TyVars.
                for _, _, sub in pairs do
                    inferPat ctx sub |> ignore

                let nodeTv = freshTv ctx key
                TyVar nodeTv
            | ValueSome info ->
                let args, subst = freshNamedInstance ctx info.TypeParams

                for _, fieldName, sub in pairs do
                    let subTy = inferPat ctx sub

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofPat sub) subTy (substituteWith subst field.Type)
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofPat sub
                                Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                                Code = ""
                                Severity = Severity.Error
                            }

                let recTy = TyRecord(info.Key, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome recTy
                recTy
        | _ ->
            // TODO: Named (DU ctor) / Cons patterns — they need
            // provider lookups or recursive shape unification.
            TyVar(freshTv ctx key)
