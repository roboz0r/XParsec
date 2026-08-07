namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The UNPOOL direction of the frozen pools: columns back to the DU. Every rebuild here is
// parameterised by the identity space it lands in — the columns hold no identity but the
// slot, so the unpool itself can only produce `BinderId`s.

[<RequireQualifiedAccess>]
module TastUnpool =

    /// Re-author one expression node from its columns and its ALREADY-REBUILT child
    /// subtrees, drawn in the order the pooling walk enumerated them. `tok` goes back
    /// unchanged — re-axising it would silently rebase a producer's indices onto this file.
    let substituteExpr
        (widenBinder: BinderId -> 'id)
        (ty: FrozenType)
        (tok: Anchor)
        (varBinder: BinderId voption)
        (payload: ExprPayload)
        (es: TExprG<FrozenType, Anchor, 'id>[])
        (ps: TPatG<FrozenType, Anchor, 'id>[])
        : TExprG<FrozenType, Anchor, 'id> =
        let nextE = ExprPayload.cursor es 0
        let nextP = ExprPayload.cursor ps 0

        let buildArms (guardPresent: bool[]) =
            ExprPayload.arms guardPresent nextP nextE |> EqArray.ofArray

        match payload with
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
        // Positional, not a cursor draw: the children are the receiver then exactly the args.
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

    let substitutePat
        (widenBinder: BinderId -> 'id)
        (ty: FrozenType)
        (tok: Anchor)
        (payload: PatPayload)
        (ps: TPatG<FrozenType, Anchor, 'id>[])
        : TPatG<FrozenType, Anchor, 'id> =
        match payload with
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
            // Field names pair off with the sub-pat children in the order the pooling walk
            // enumerated the record's fields.
            let fields' =
                Array.map2 (fun name sub -> (name, sub)) fieldNames ps |> EqArray.ofArray

            TPatG.Record(fields', ty, tok)

    /// Re-author one declaration node. A `Type` decl's bodies AND its pattern-less binder
    /// slots are named by id INSIDE the payload's declaration shape, not by the child
    /// columns, so this direction needs the id→expr resolver too.
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

    /// A dense `BinderId`-keyed side table as the keyed `Map` it was re-keyed FROM.
    let private binderKeyedMap (resolve: BinderId -> 'k) (dense: (BinderId * 'v)[]) : Map<'k, 'v> =
        dense |> Array.map (fun (id, v) -> resolve id, v) |> Map.ofArray

    /// The same unpool for a per-binder COLUMN: the key is re-minted from the slot's own
    /// position.
    let private binderColumnMap (resolve: BinderId -> 'k) (col: BinderColumn<'v>) : Map<'k, 'v> =
        Map.ofSeq
            [
                for i in 0 .. col.Length - 1 do
                    match col.[i] with
                    | ValueSome v -> yield resolve (BinderId i), v
                    | ValueNone -> ()
            ]

    /// Rebuild the whole-file DU from the pools: the `Decls` re-authored from the pool roots
    /// and the side tables re-keyed back through the binder/lambda id spaces, the `Residue`
    /// fields carried verbatim. `widenBinder` picks the identity space it all lands in.
    let rebuildFile (widenBinder: BinderId -> 'id) (pools: FrozenPools) : TastFileG<FrozenType, Anchor, 'id> =
        // The binder ids back in the BINDER key space by PROJECTION: as the trees below are
        // rebuilt, each node is asked what it binds, and only what they answer can key a
        // rebuilt side table — so no binder identity the tree does not bear can be minted.
        let readmitted = System.Collections.Generic.Dictionary<'id, BinderKeyG<'id>>()

        let readmit (b: BinderKeyG<'id>) : unit = readmitted.[BinderKey.identity b] <- b

        let readmittedBinder (id: BinderId) : BinderKeyG<'id> =
            let k = widenBinder id

            match readmitted.TryGetValue k with
            | true, b -> b
            | false, _ -> failwithf "TastUnpool: binder %O (%O) is interned but no rebuilt node introduces it" id k

        // A lambda's `ExprPoolId` back to the `LambdaKey` its verdict is filed under: the
        // key IS the anchor, which the column holds, so there is nothing to resolve.
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

        // Likewise the specialization table, in SLOT ORDER: the `SpecializationId`s the
        // rebuilt tree carries index this array, so it must not be reordered or compacted —
        // not even for an entry no surviving call site names.
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

        // The lambda-keyed inverse FOLDS: every copy of a spliced inline body keeps one
        // definition-site token, hence one verdict.
        {
            Decls = decls
            Diagnostics = pools.Residue.Diagnostics
            IntrinsicReprKeys = pools.Residue.IntrinsicReprKeys
            GlobalValueKeys = pools.Residue.GlobalValueKeys
            ModuleMembers = binderKeyedMap readmittedBinder pools.ModuleMembers
            ClosureReprs = binderKeyedMap readmittedBinder pools.ClosureReprs
            FunVerdicts = pools.FunVerdicts |> Array.map (fun (id, v) -> lambdaKeyOf id, v) |> Map.ofArray
            GenericFnSchemes = binderKeyedMap readmittedBinder pools.GenericFnSchemes
            InlineBodies = inlineBodies
            Specializations = specializations
            Accessibility = pools.Residue.Accessibility
            // No `BindingValReprs`: the DU does not carry one — it is a PROJECTION of the
            // lambda chain, re-derived off the columns on the way back in.
            BindingTyparArities = binderColumnMap readmittedBinder pools.BindingTyparArities
        }

    /// The whole-file unpool, in the pool's OWN identity space: a rebuilt binder is named
    /// by the `BinderId` the columns address it with. Only the tests call it — structural
    /// `ofPools (toPools f) = f` is what makes the columns' tree-sufficiency checkable.
    let ofPools (pools: FrozenPools) : Pooled.TastFile = rebuildFile id pools
