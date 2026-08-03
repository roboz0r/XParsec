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
    /// `'T list` abbreviation (the self-host `list.clr.fs`) expands eagerly to the
    /// union RHS; a bare program leaves the container flexible (a fresh TyVar
    /// registered in `ctx.ListLiterals`) for consumer-driven resolution.
    let private consListTy (ctx: PassContext) (tok: SyntaxToken) (elemTy: SemType) : SemType =
        match TypeRegistry.tryAbbrevArity ctx.Types UseSite.unbounded "list" 1 with
        | ValueSome info ->
            forceFill ctx info
            expandAbbreviation ctx tok info (EqArray.singleton elemTy)
        | ValueNone ->
            let tv = freshTyVar ctx

            ctx.RegisterListLiteral((UnionFind.find ctx.Store tv).Id, elemTy, tok)
            TyVar tv

    let rec inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Each pattern node gets its own TypeVar keyed on its NodeKey; for
        // compound patterns the outer TypeVar is linked to the underlying
        // shape so a lookup against any pattern node returns the right type.
        let key = CstKeys.ofPat p
        // `key` is projected from this token, so the side-table entry and a diagnostic
        // about the same node cannot name different places.
        let tok = CstKeys.firstTokenOfPat p

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t

            n.Length > 0
            && System.Char.IsUpper n.[0]
            && TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt key) n
            ->
            // Uppercase-leading bare ident matching a ctor IN SCOPE HERE —
            // reinterpret as a nullary ctor pattern. Multi-candidate names
            // require a qualifier; diagnose ambiguity, best-effort otherwise.
            // A case whose union is declared BELOW names nothing here, so the ident
            // stays an ordinary binder (the arm below) — as in F#, where an
            // unrecognised ident in pattern position is a variable pattern.
            let n = ctx.NameOf t
            let info, count = resolveCtorName ctx (ctx.UseSiteAt key) n

            match info with
            | ValueSome i when i.Fields.Length = 0 ->
                let unionInfo = TypeRegistry.unionOfCase ctx.Types i
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(unionInfo.TypeKey, args)
                let nodeTv = freshTv ctx key
                ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome ty)
                ty
            | ValueSome i ->
                ctx.Report(tok, Kind.NullaryConstructorPattern(n, i.Fields.Length))

                let unionInfo = TypeRegistry.unionOfCase ctx.Types i
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(unionInfo.TypeKey, args)
                let nodeTv = freshTv ctx key
                ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome ty)
                ty
            | ValueNone when count >= 2 ->
                ctx.Report(tok, Kind.AmbiguousConstructor(n, count))

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
                ctx.Report(tok, Kind.NullaryConstructorPattern(ctx.NameOf t, fields.Length))

            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome unionTy)
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
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome ty)
            ty
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length = 2
            && (TypeRegistry.tryEnum ctx.Types (ctx.UseSiteAt key) (ctx.NameOf li.Idents.[0])).IsSome
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
            let einfo =
                (TypeRegistry.tryEnum ctx.Types (ctx.UseSiteAt key) (ctx.NameOf li.Idents.[0])).Value

            let caseName = ctx.NameOf li.Idents.[1]

            if not (einfo.HasCase caseName) then
                ctx.Report(tok, Kind.NoCase(CaseOwner.Enum, einfo.Name, caseName))

            for sub in args do
                inferPat ctx sub |> ignore

            let ty = TyEnum einfo.TypeKey
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome ty)
            ty
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                last.Length > 0 && System.Char.IsUpper last.[0])
            && (li.Idents.Length = 1
                && TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt key) (ctx.NameOf li.Idents.[0])
                || li.Idents.Length = 2
                   && (
                       match TypeRegistry.tryUnionBare ctx.Types (ctx.UseSiteAt key) (ctx.NameOf li.Idents.[0]) with
                       | ValueSome info ->
                           let caseName = ctx.NameOf li.Idents.[1]
                           info.Cases |> Array.exists (fun c -> c.Name = caseName)
                       | ValueNone -> false
                   ))
            ->
            let info =
                if li.Idents.Length = 1 then
                    let name = ctx.NameOf li.Idents.[0]

                    match resolveCtorName ctx (ctx.UseSiteAt key) name with
                    | ValueSome i, _ -> ValueSome i
                    | ValueNone, count when count >= 2 ->
                        ctx.Report(tok, Kind.AmbiguousConstructor(name, count))

                        ValueNone
                    | _ -> ValueNone
                else
                    let typeName = ctx.NameOf li.Idents.[0]
                    let caseName = ctx.NameOf li.Idents.[1]
                    resolveQualifiedCtor ctx (ctx.UseSiteAt key) typeName caseName

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
                    ctx.Report(tok, Kind.ConstructorArity(i.Name, i.Fields.Length, subPats.Length))

                let unionInfo = TypeRegistry.unionOfCase ctx.Types i
                let args, subst = freshNamedInstance ctx unionInfo.TypeParams
                let m = min subPats.Length i.Fields.Length

                for j = 0 to m - 1 do
                    let sub = subPats.[j]
                    let subTy = inferPat ctx sub
                    unify ctx (CstKeys.firstTokenOfPat sub) subTy (substituteWith ctx.Store subst i.Fields.[j])

                // Walk any extra sub-patterns so binders still register.
                for j = m to subPats.Length - 1 do
                    inferPat ctx subPats.[j] |> ignore

                let ty = TyUnion(unionInfo.TypeKey, args)
                let nodeTv = freshTv ctx key
                ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome ty)
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
                ctx.Report(tok, Kind.ConstructorArity(caseName, fields.Length, subPats.Length))

            let m = min subPats.Length fields.Length

            for j = 0 to m - 1 do
                let sub = subPats.[j]
                let subTy = inferPat ctx sub
                unify ctx (CstKeys.firstTokenOfPat sub) subTy fields.[j]

            // Walk any extra sub-patterns so binders still register.
            for j = m to subPats.Length - 1 do
                inferPat ctx subPats.[j] |> ignore

            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome unionTy)
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
                unify ctx (CstKeys.firstTokenOfPat e) eTy elemTy

            let listTy = consListTy ctx tok elemTy
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome listTy)
            listTy
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Attributed(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome innerTy)
            innerTy
        | Pat.Tuple(patterns = pats) ->
            let elemTys = EqArray.ofSeq (seq { for p in pats -> inferPat ctx p })
            let tupleTy = TyTuple elemTys
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome tupleTy)
            tupleTy
        | Pat.Const c ->
            let constTy = inferConst ctx c
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome constTy)
            constTy
        | Pat.As(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome innerTy)
            innerTy
        | Pat.Typed(pat = inner; typ = t) ->
            let innerTy = inferPat ctx inner
            let annTy = translateType ctx t
            // Annotation reconciliation (`x: int | string`): admits value→union but
            // stays symmetric `unify` for a nominal/`obj` annotation, so the binder
            // still grounds to its written type.
            unifyAnnotation ctx tok innerTy annTy
            // Type provenance: a typed pattern `(x : T)` — parameter, `let`-binder, or
            // nested destructure — writes the binder's type explicitly. Attribute it to
            // the INNER binder's key (the `Pat.Typed` wrapper is erased in the TAST; a
            // consumer queries the `NamedSimple`), matching how a value binding marks its
            // `headPat`.
            ctx.MarkTypeDeclared(CstKeys.ofPat inner, annTy)
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome annTy)
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
            unify ctx (CstKeys.firstTokenOfPat inner) innerTy tgtTy
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
            let listTy = consListTy ctx tok elemTy
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome listTy)
            listTy
        | Pat.Cons(head = headPat; tail = tailPat) ->
            // `h :: t`: `h` is an element, `t` the same list type.
            let headTy = inferPat ctx headPat
            let listTy = consListTy ctx tok headTy
            let tailTy = inferPat ctx tailPat
            unify ctx (CstKeys.firstTokenOfPat tailPat) tailTy listTy
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome listTy)
            listTy
        | Pat.EmptyBlock _ ->
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome ctx.Intrinsics.Unit)
            ctx.Intrinsics.Unit
        | Pat.Or(left = leftPat; right = rightPat) ->
            // Here we only unify the alternatives' overall types for scrutinee
            // consistency. Binding or-patterns are unsupported — `ElaboratePatterns`
            // rejects any alternative that binds a name — so no name-set reconciliation
            // is needed.
            let leftTy = inferPat ctx leftPat
            let rightTy = inferPat ctx rightPat
            unify ctx tok leftTy rightTy
            let nodeTv = freshTv ctx key
            ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome leftTy)
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

            match resolveRecordFor ctx tok (ctx.UseSiteAt key) qualifier names with
            | ValueNone ->
                // Walk sub-patterns so binders register as free TyVars.
                for _, _, sub in pairs do
                    inferPat ctx sub |> ignore

                let nodeTv = freshTv ctx key
                TyVar nodeTv
            | ValueSome r ->
                let struct (recKey, args, fieldTypeOf) = recordConstructionOf ctx r

                for _, fieldName, sub in pairs do
                    let subTy = inferPat ctx sub

                    match fieldTypeOf fieldName with
                    | ValueSome fieldTy -> unify ctx (CstKeys.firstTokenOfPat sub) subTy fieldTy
                    | ValueNone ->
                        ctx.Report(
                            CstKeys.firstTokenOfPat sub,
                            Kind.NoMember(resolvedRecordDisplayName r, MemberNoun.Field, fieldName)
                        )

                let recTy = TyRecord(recKey, args)
                let nodeTv = freshTv ctx key
                ctx.Store.SetLink(UnionFind.find ctx.Store nodeTv, ValueSome recTy)
                recTy
        | _ ->
            // TODO: Named (DU ctor) / Cons patterns — they need
            // provider lookups or recursive shape unification.
            TyVar(freshTv ctx key)
