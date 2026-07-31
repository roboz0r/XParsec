namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The DRAIN direction of the frozen pools: columns back to the DU. Its inverse — the DU
// vocabulary, the pooling walk and `toPools` — is `TastPools.fs`, and the wire-shape types
// are `TastPoolNodes.fs` (one node) and `TastPoolTypes.fs` (the whole file). Split from the
// fill so the two directions are separately readable and the compiler's dependency edge
// says which way the data flows: this file reads `TastPools`, never the reverse.
//
// Every rebuild here is parameterised by the identity space it lands in, and the columns
// hold no identity but the slot — so the space a drain lands in is the CALLER's to supply
// and the drain itself can only produce `BinderId`s. `Pooled.*` is that space, and
// `ofPools` is the drain at it.
//
// Which exports have production callers, and for narrow reasons:
//
//   * `substituteExpr`/`substitutePat`/`substituteDecl` — the NODE-level inverse, driven
//     by `TastPoolBuilder`'s subtree drain (`declTree`) for the one channel whose far end
//     is still DU-typed: a package's inline template crosses the wire as a
//     `Pooled.TDecl`, a pool id being meaningless outside the pool that issued it.
//
// The whole-file drain has NONE, and that is the point of it: it is what makes the
// columns' tree-sufficiency CHECKABLE.

[<RequireQualifiedAccess>]
module TastUnpool =

    /// Re-author one expression node from its columns — `ty`/`tok`, the `Var` reference
    /// edge, the `ExprPayload` residual scalars/structure — and its ALREADY-REBUILT
    /// child subtrees, with NO template node (the expr pool holds none). The children are
    /// consumed in the exact order `TastPoolShapes.exprChildren`/`exprPatChildren` enumerated
    /// them (`nextE`/`nextP` are order cursors) — the one coupling the round-trip gate
    /// proves. The match on `ExprPayload` is exhaustive with no catch-all (the inverse of
    /// `TastPoolShapes.exprPayload`), so a new shape fails to compile here.
    ///
    /// The identity the rebuilt DU names binders by is the CALLER's: `widenBinder` is
    /// `id` for a tree that stays in the pool's own dense space (`Pooled.TExpr`) and the
    /// column lookup for one that must speak the node space a source-shaped tree is
    /// addressed in (`Frozen.TExpr`). ONE hook, so both the reference edge and the `ForTo`
    /// binder land in the same space rather than each site picking its own way back.
    ///
    /// POSITIONS take no such hook, and must not: the anchors come out of the columns as they
    /// went in, whichever space the rebuilt tree lands in. A drain that could re-axis them would
    /// be a drain that could quietly rebase a producer's indices onto the consuming file — the
    /// one misattribution that resolves in range and never faults.
    let substituteExpr
        (widenBinder: BinderId -> 'id)
        (ty: FrozenType)
        (tok: Anchor)
        (varBinder: BinderId voption)
        (payload: ExprPayload)
        (es: TExprG<FrozenType, Anchor, 'id>[])
        (ps: TPatG<FrozenType, Anchor, 'id>[])
        : TExprG<FrozenType, Anchor, 'id> =
        // A decl's own leading children are all its own, so both cursors start at 0; the
        // `Match`/`TryWith` arms below draw from `nextE` only after the node has taken its
        // scrutinee / body from it.
        let nextE = ExprPayload.cursor es 0
        let nextP = ExprPayload.cursor ps 0

        // The arm / format re-nesting is `ExprPayload.arms` / `ExprPayload.format` — the
        // one walk over the flat child columns, shared with the accessor's views, so the
        // two directions cannot disagree about the order the columns are consumed in.
        let buildArms (guardPresent: bool[]) =
            ExprPayload.arms guardPresent nextP nextE |> EqArray.ofArray

        match payload with
        // `binding` is supplied from the dense id, so the round-trip exercises the remap.
        | ExprPayload.Var ->
            match varBinder with
            | ValueSome id -> TExprG.Var(widenBinder id, ty, tok)
            | ValueNone -> failwith "TastUnpool: a Var entry carries no resolved binder id"
        | ExprPayload.Const value -> TExprG.Const(value, ty, tok)
        | ExprPayload.External p -> TExprG.External(p.CompiledName, p.Key, ty, tok)
        | ExprPayload.Null -> TExprG.Null(ty, tok)
        | ExprPayload.StaticPropertyGet key -> TExprG.StaticPropertyGet(key, ty, tok)
        | ExprPayload.StaticFieldGet p -> TExprG.StaticFieldGet(p.DeclKey, p.FieldName, ty, tok)
        | ExprPayload.Lambda ->
            let param = nextP ()
            let body = nextE ()
            TExprG.Lambda(param, body, ty, tok)
        | ExprPayload.App ->
            let fn = nextE ()
            let arg = nextE ()
            TExprG.App(fn, arg, ty, tok)
        | ExprPayload.Let ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Let(binding, value, body, ty, tok)
        | ExprPayload.Use dispose ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Use(binding, value, body, dispose, ty, tok)
        | ExprPayload.IfThenElse ->
            let cond = nextE ()
            let thenExpr = nextE ()
            let elseExpr = nextE ()
            TExprG.IfThenElse(cond, thenExpr, elseExpr, ty, tok)
        | ExprPayload.Tuple -> TExprG.Tuple(EqArray.ofArray es, ty, tok)
        | ExprPayload.Sequential -> TExprG.Sequential(EqArray.ofArray es, ty, tok)
        | ExprPayload.While ->
            let cond = nextE ()
            let body = nextE ()
            TExprG.While(cond, body, ty, tok)
        | ExprPayload.ForTo p ->
            let startExpr = nextE ()
            let endExpr = nextE ()
            let body = nextE ()
            TExprG.ForTo(widenBinder p.Var, p.IdentTok, startExpr, endExpr, body, ty, tok)
        | ExprPayload.ForIn enumerator ->
            let pat = nextP ()
            let source = nextE ()
            let body = nextE ()
            TExprG.ForIn(pat, source, body, enumerator, ty, tok)
        | ExprPayload.Match guardPresent ->
            let scrutinee = nextE ()
            TExprG.Match(scrutinee, buildArms guardPresent, ty, tok)
        | ExprPayload.TryWith guardPresent ->
            let body = nextE ()
            TExprG.TryWith(body, buildArms guardPresent, ty, tok)
        | ExprPayload.TryFinally ->
            let body = nextE ()
            let cleanup = nextE ()
            TExprG.TryFinally(body, cleanup, ty, tok)
        | ExprPayload.Assignment ->
            let lhs = nextE ()
            let rhs = nextE ()
            TExprG.Assignment(lhs, rhs, ty, tok)
        | ExprPayload.Range hasStep ->
            let startExpr = nextE ()
            let step' = if hasStep then Some(nextE ()) else None
            let stopExpr = nextE ()
            TExprG.Range(startExpr, step', stopExpr, ty, tok)
        | ExprPayload.RecordCons fieldNames ->
            let fields' =
                fieldNames |> Array.map (fun name -> (name, nextE ())) |> EqArray.ofArray

            TExprG.RecordCons(fields', ty, tok)
        | ExprPayload.RecordClone overrideNames ->
            let source = nextE ()

            let overrides' =
                overrideNames |> Array.map (fun name -> (name, nextE ())) |> EqArray.ofArray

            TExprG.RecordClone(source, overrides', ty, tok)
        | ExprPayload.FieldGet fieldName ->
            let receiver = nextE ()
            TExprG.FieldGet(receiver, fieldName, ty, tok)
        | ExprPayload.FieldSet fieldName ->
            let receiver = nextE ()
            let value = nextE ()
            TExprG.FieldSet(receiver, fieldName, value, ty, tok)
        | ExprPayload.UnionCons caseName -> TExprG.UnionCons(caseName, EqArray.ofArray es, ty, tok)
        | ExprPayload.New p -> TExprG.New(p.ClassName, p.Key, EqArray.ofArray es, ty, tok)
        // Wholly POSITIONAL, not a cursor draw: the children are the receiver followed by
        // exactly the args, so the split is the same index either way.
        | ExprPayload.MethodCall p -> TExprG.MethodCall(es.[0], p.Key, p.Via, EqArray.ofArray es.[1..], ty, tok)
        | ExprPayload.PropertyGet p ->
            let receiver = nextE ()
            TExprG.PropertyGet(receiver, p.Key, p.Via, ty, tok)
        | ExprPayload.StaticMethodCall key -> TExprG.StaticMethodCall(key, EqArray.ofArray es, ty, tok)
        | ExprPayload.StaticFieldSet p ->
            let value = nextE ()
            TExprG.StaticFieldSet(p.DeclKey, p.FieldName, value, ty, tok)
        | ExprPayload.ExternalMember p ->
            let receiver' = if p.HasReceiver then ValueSome(nextE ()) else ValueNone
            TExprG.ExternalMember(receiver', p.Key, p.MemberName, p.Storage, ty, tok)
        | ExprPayload.Format p ->
            let sink', segments' = ExprPayload.format p.Sink p.Segments nextE
            TExprG.Format(sink', EqArray.ofArray segments', ty, tok)
        | ExprPayload.ILIntrinsic p -> TExprG.ILIntrinsic(p.OpCode, p.TypeOperand, EqArray.ofArray es, ty, tok)
        | ExprPayload.InlineCall p -> TExprG.InlineCall(p.Spec, EqArray.ofArray es, p.Origin, ty, tok)
        | ExprPayload.StaticOptimization clauseConstraints ->
            let clauses' =
                clauseConstraints
                |> Array.map (fun constraints ->
                    {
                        Constraints = constraints
                        Body = nextE ()
                    }
                )
                |> EqArray.ofArray

            let defaultExpr = nextE ()
            TExprG.StaticOptimization(clauses', defaultExpr, ty, tok)
        | ExprPayload.CallerExpr origin -> TExprG.CallerExpr(nextE (), origin, ty, tok)
        | ExprPayload.Upcast -> TExprG.Upcast(nextE (), ty, tok)
        | ExprPayload.Downcast -> TExprG.Downcast(nextE (), ty, tok)
        | ExprPayload.TypeTest testTy ->
            let source = nextE ()
            TExprG.TypeTest(source, testTy, ty, tok)
        | ExprPayload.TraitCall p -> TExprG.TraitCall(p.Receiver, p.MemberName, EqArray.ofArray es, ty, tok)

    /// Re-author one pattern node from its own payload + rebuilt sub-patterns — see
    /// `substituteExpr`, `widenBinder` included; exhaustive against `TastPoolShapes.patPayload`
    /// the same way.
    let substitutePat
        (widenBinder: BinderId -> 'id)
        (ty: FrozenType)
        (tok: Anchor)
        (payload: PatPayload)
        (ps: TPatG<FrozenType, Anchor, 'id>[])
        : TPatG<FrozenType, Anchor, 'id> =
        match payload with
        // The binder this pattern introduces, named in the caller's identity space — the
        // pat analogue of `ForTo.var`.
        | PatPayload.NamedSimple binder -> TPatG.NamedSimple(widenBinder binder, ty, tok)
        | PatPayload.Wildcard -> TPatG.Wildcard(ty, tok)
        | PatPayload.Null -> TPatG.Null(ty, tok)
        | PatPayload.Const value -> TPatG.Const(value, ty, tok)
        | PatPayload.EnumCase p -> TPatG.EnumCase(p.EnumKey, p.CaseName, ty, tok)
        | PatPayload.Tuple -> TPatG.Tuple(EqArray.ofArray ps, ty, tok)
        | PatPayload.Or -> TPatG.Or(EqArray.ofArray ps, ty, tok)
        | PatPayload.Union caseName -> TPatG.Union(caseName, EqArray.ofArray ps, ty, tok)
        | PatPayload.TypeTestAs testTy -> TPatG.TypeTestAs(testTy, ps.[0], ty, tok)
        | PatPayload.Record fieldNames ->
            // The field names pair off with the sub-pat children in the SAME order
            // `patChildren` enumerated the record's fields.
            let fields' =
                Array.map2 (fun name sub -> (name, sub)) fieldNames ps |> EqArray.ofArray

            TPatG.Record(fields', ty, tok)

    /// A `Type` decl's bodies AND its pattern-less binder slots are named by id INSIDE the
    /// payload's declaration shape (not by the child columns), so this direction needs the
    /// id→expr resolver and `widenBinder` too — the same `TastConvert.typeDecl` traversal,
    /// run at the inverse body and identity mappings.
    let substituteDecl
        (widenBinder: BinderId -> 'id)
        (fromExpr: ExprPoolId -> TExprG<FrozenType, Anchor, 'id>)
        (payload: DeclPayload)
        (es: TExprG<FrozenType, Anchor, 'id>[])
        (ps: TPatG<FrozenType, Anchor, 'id>[])
        : TDeclG<FrozenType, Anchor, 'id> =
        match payload with
        | DeclPayload.Let p -> TDeclG.Let(ps.[0], es.[0], p.IsInline, p.Ty)
        | DeclPayload.Expression ty -> TDeclG.Expression(es.[0], ty)
        | DeclPayload.Type td ->
            TDeclG.Type(
                TastConvert.typeDecl
                    {
                        Ty = id
                        Tok = id
                        Id = BinderKey.identity >> widenBinder
                        Body = fromExpr
                    }
                    td
            )

    /// A dense `BinderId`-keyed side table as the keyed `Map` it was re-keyed FROM, each
    /// id resolved by the rebuild's own inverse of the interning: a `BinderKey` PROJECTED
    /// from a rebuilt node, so a map key can only name a binder the rebuilt tree bears.
    let private binderKeyedMap (resolve: BinderId -> 'k) (dense: (BinderId * 'v)[]) : Map<'k, 'v> =
        dense |> Array.map (fun (id, v) -> resolve id, v) |> Map.ofArray

    /// The same drain for a per-binder COLUMN (`BinderColumn`): the key the fill consumed is
    /// re-minted from the slot's own position, which is the only place it can come from now
    /// — a filled slot is an entry, an empty one is no entry, and there is no third state a
    /// stored key could have put the map in.
    let private binderColumnMap (resolve: BinderId -> 'k) (col: BinderColumn<'v>) : Map<'k, 'v> =
        Map.ofSeq
            [
                for i in 0 .. col.Length - 1 do
                    match col.[i] with
                    | ValueSome v -> yield resolve (BinderId i), v
                    | ValueNone -> ()
            ]

    /// Rebuild the whole-file DU from the pools — the inverse of `toPools`. The `Decls` are
    /// re-authored from the pool roots and the side tables re-keyed back through the
    /// binder/lambda id spaces; only the three `Residue` fields are carried through
    /// verbatim, having no pooled form.
    ///
    /// `widenBinder` decides which identity space the rebuilt file lands in. The columns
    /// carry no identity but the slot, so the drain can only ever offer a `BinderId`: any
    /// OTHER space has to be supplied from outside, by a caller that holds the
    /// correspondence itself. `ofPools` is this at the pool's own space, where the
    /// correspondence is the identity; the round-trip gate is the only other caller, and
    /// what it supplies is a correspondence it derives — and proves bijective — by
    /// correlating the rebuilt tree with the source tree it is being compared to.
    let rebuildFile (widenBinder: BinderId -> 'id) (pools: FrozenPools) : TastFileG<FrozenType, Anchor, 'id> =
        // The binder column back in the BINDER key space, re-admitted by PROJECTION and
        // never by fiat: as the trees below are rebuilt, each node is asked what it binds
        // with the same `BinderKey` projections `toPools` interned by, and only what they
        // answer can key a rebuilt side table. So the drain cannot mint a binder identity
        // the tree does not bear — the round trip proves the key remap, not just the
        // shapes.
        let readmitted = System.Collections.Generic.Dictionary<'id, BinderKeyG<'id>>()

        let readmit (b: BinderKeyG<'id>) : unit = readmitted.[BinderKey.identity b] <- b

        let readmittedBinder (id: BinderId) : BinderKeyG<'id> =
            let k = widenBinder id

            match readmitted.TryGetValue k with
            | true, b -> b
            | false, _ -> failwithf "TastUnpool: binder %O (%O) is interned but no rebuilt node introduces it" id k

        // The inverse of the lambda id space: a lambda's `ExprPoolId` back to the `LambdaKey`
        // its verdict is filed under. The key IS the anchor, and the column holds the
        // anchor, so there is nothing here to resolve.
        let lambdaKeyOf (ExprPoolId i) : LambdaKey = LambdaKey pools.ExprToks.[i]

        let rec fromPat (PatPoolId i) : TPatG<FrozenType, Anchor, 'id> =
            let ps = ChildColumn.slice pools.PatChildren i |> Array.map fromPat

            let p =
                substitutePat widenBinder pools.Types.[pools.PatTys.[i]] pools.PatToks.[i] pools.PatPayloads.[i] ps

            BinderKey.ofPat p |> ValueOption.iter readmit
            p

        let rec fromExpr (ExprPoolId i) : TExprG<FrozenType, Anchor, 'id> =
            let es = ChildColumn.slice pools.ExprChildren i |> Array.map fromExpr
            let ps = ChildColumn.slice pools.ExprPatChildren i |> Array.map fromPat

            let e =
                substituteExpr
                    widenBinder
                    pools.Types.[pools.ExprTys.[i]]
                    pools.ExprToks.[i]
                    pools.ExprVarBinder.[i]
                    pools.ExprPayloads.[i]
                    es
                    ps

            BinderKey.ofExpr e |> ValueOption.iter readmit
            e

        let fromDecl (DeclPoolId i) : TDeclG<FrozenType, Anchor, 'id> =
            let es = ChildColumn.slice pools.DeclExprChildren i |> Array.map fromExpr
            let ps = ChildColumn.slice pools.DeclPatChildren i |> Array.map fromPat
            let d = substituteDecl widenBinder fromExpr pools.DeclPayloads.[i] es ps

            match d with
            | TDeclG.Type td -> Seq.iter readmit (BinderKey.ofTypeDecl td)
            | TDeclG.Let _
            | TDeclG.Expression _ -> ()

            d

        let decls = pools.Roots |> Array.map fromDecl |> EqArray.ofArray

        // The vocabulary rebuilds from its own roots — a distinct tree from the emitted
        // function of the same name, never re-derived from it.
        let inlineBodies =
            pools.InlineTemplates
            |> Array.map (fun t ->
                {
                    Key = t.Key
                    Body =
                        {
                            Decl = fromDecl t.Decl
                            ParamAttrs = t.ParamAttrs
                        }
                }
            )
            |> EqArray.ofArray

        // Likewise the specialization table, in SLOT ORDER — the `SpecializationId`s the
        // rebuilt tree carries index this array, so the drain must not reorder or compact
        // it, even for an entry no surviving call site names.
        let specializations =
            pools.Specializations
            |> Array.map (fun s ->
                {
                    TSpecializationG.Key = s.Key
                    Origin = s.Origin
                    Decl = fromDecl s.Decl
                }
            )
            |> EqArray.ofArray

        // Reconstructing the side-table maps here (rather than retaining the source
        // file's) is what makes the round-trip prove the key remap, not just the decl
        // trees. The keyed binder tables go through the shared `binderKeyedMap` and the two
        // per-binder COLUMNS through `binderColumnMap`, both resolving through the
        // projections the rebuild collected; `FunVerdicts` is the one on the lambda id
        // space, so it inverts through `lambdaKeyOf` right where it is built.
        //
        // That inverse FOLDS: several ids may carry one key (every copy of a spliced inline
        // body's lambda keeps its definition-site token), and `toPools` gave each of them
        // the same verdict, so collapsing them onto the one key restores the source map
        // exactly rather than picking a winner.
        {
            Decls = decls
            Diagnostics = pools.Residue.Diagnostics
            IntrinsicReprKeys = pools.Residue.IntrinsicReprKeys
            ModuleMembers = binderKeyedMap readmittedBinder pools.ModuleMembers
            ClosureReprs = binderKeyedMap readmittedBinder pools.ClosureReprs
            FunVerdicts = pools.FunVerdicts |> Array.map (fun (id, v) -> lambdaKeyOf id, v) |> Map.ofArray
            GenericFnSchemes = binderKeyedMap readmittedBinder pools.GenericFnSchemes
            InlineBodies = inlineBodies
            Specializations = specializations
            Accessibility = pools.Residue.Accessibility
            // No `BindingValReprs`: the DU does not carry one. It is a PROJECTION of the
            // lambda spine, so `toPools` re-derives it off the columns rather than the DU
            // ferrying it across — which is also why the round trip does not have to
            // reconstruct it to stay faithful.
            BindingTyparArities = binderColumnMap readmittedBinder pools.BindingTyparArities
        }

    /// The whole-file drain, in the pool's OWN identity space: every binder the rebuilt
    /// tree names, it names by the `BinderId` the columns address it with, so the drain
    /// consults no retained key to speak at all.
    ///
    /// It has NO production caller: the freeze yields pools, every consumer reads pools,
    /// and `FrozenCodec` stores pools. What it exists for is the OBLIGATION the pools owe
    /// — that the columns are tree-sufficient. `ofPools (toPools f) = f` structurally,
    /// over the whole corpus, is the proof that nothing of the tree was lost on the way
    /// into the columns, and it is checkable only because the DU is still expressible.
    /// (Corpus gates: `TastPoolsTests`, `Codegen.Clr.Tests/TestHelpers.fs`'s
    /// `PoolRoundTripped` codegen-invariance.)
    let ofPools (pools: FrozenPools) : Pooled.TastFile = rebuildFile id pools
