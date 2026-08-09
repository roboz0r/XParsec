namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// The enumeration of the TAST's recursion shape, in two flavours: a `Mapper` rebuilds each
// node, an `Iter` only visits it. The TERM shapes are enumerated here; the DECLARATION
// shape is not and must not be, because `mapTypeDecl` delegates it to the shared decl rebuild.

[<RequireQualifiedAccess>]
module TastWalk =

    let exprTy (e: TExprG<'ty, 'tok, 'id>) : 'ty =
        match e with
        | TExprG.Const(ty = ty)
        | TExprG.Var(ty = ty)
        | TExprG.External(ty = ty)
        | TExprG.Lambda(ty = ty)
        | TExprG.App(ty = ty)
        | TExprG.Let(ty = ty)
        | TExprG.Use(ty = ty)
        | TExprG.IfThenElse(ty = ty)
        | TExprG.Tuple(ty = ty)
        | TExprG.Sequential(ty = ty)
        | TExprG.While(ty = ty)
        | TExprG.ForTo(ty = ty)
        | TExprG.ForIn(ty = ty)
        | TExprG.Match(ty = ty)
        | TExprG.TryWith(ty = ty)
        | TExprG.TryFinally(ty = ty)
        | TExprG.Assignment(ty = ty)
        | TExprG.Null(ty = ty)
        | TExprG.Range(ty = ty)
        | TExprG.RecordCons(ty = ty)
        | TExprG.RecordClone(ty = ty)
        | TExprG.FieldGet(ty = ty)
        | TExprG.FieldSet(ty = ty)
        | TExprG.UnionCons(ty = ty)
        | TExprG.New(ty = ty)
        | TExprG.MethodCall(ty = ty)
        | TExprG.PropertyGet(ty = ty)
        | TExprG.StaticMethodCall(ty = ty)
        | TExprG.StaticPropertyGet(ty = ty)
        | TExprG.StaticFieldGet(ty = ty)
        | TExprG.StaticFieldSet(ty = ty)
        | TExprG.ExternalMember(ty = ty)
        | TExprG.Format(ty = ty)
        | TExprG.ILIntrinsic(ty = ty)
        | TExprG.StaticOptimization(ty = ty)
        | TExprG.Upcast(ty = ty)
        | TExprG.Downcast(ty = ty)
        | TExprG.TraitCall(ty = ty)
        | TExprG.InlineCall(ty = ty)
        | TExprG.CallerExpr(ty = ty)
        | TExprG.TypeTest(ty = ty) -> ty

    let exprTok (e: TExprG<'ty, 'tok, 'id>) : 'tok =
        match e with
        | TExprG.Const(tok = tok)
        | TExprG.Var(tok = tok)
        | TExprG.External(tok = tok)
        | TExprG.Lambda(tok = tok)
        | TExprG.App(tok = tok)
        | TExprG.Let(tok = tok)
        | TExprG.Use(tok = tok)
        | TExprG.IfThenElse(tok = tok)
        | TExprG.Tuple(tok = tok)
        | TExprG.Sequential(tok = tok)
        | TExprG.While(tok = tok)
        | TExprG.ForTo(tok = tok)
        | TExprG.ForIn(tok = tok)
        | TExprG.Match(tok = tok)
        | TExprG.TryWith(tok = tok)
        | TExprG.TryFinally(tok = tok)
        | TExprG.Assignment(tok = tok)
        | TExprG.Null(tok = tok)
        | TExprG.Range(tok = tok)
        | TExprG.RecordCons(tok = tok)
        | TExprG.RecordClone(tok = tok)
        | TExprG.FieldGet(tok = tok)
        | TExprG.FieldSet(tok = tok)
        | TExprG.UnionCons(tok = tok)
        | TExprG.New(tok = tok)
        | TExprG.MethodCall(tok = tok)
        | TExprG.PropertyGet(tok = tok)
        | TExprG.StaticMethodCall(tok = tok)
        | TExprG.StaticPropertyGet(tok = tok)
        | TExprG.StaticFieldGet(tok = tok)
        | TExprG.StaticFieldSet(tok = tok)
        | TExprG.ExternalMember(tok = tok)
        | TExprG.Format(tok = tok)
        | TExprG.ILIntrinsic(tok = tok)
        | TExprG.StaticOptimization(tok = tok)
        | TExprG.Upcast(tok = tok)
        | TExprG.Downcast(tok = tok)
        | TExprG.TraitCall(tok = tok)
        | TExprG.InlineCall(tok = tok)
        | TExprG.CallerExpr(tok = tok)
        | TExprG.TypeTest(tok = tok) -> tok

    /// Mark `body` as CALLER material: an expression written in `origin` that a reduction
    /// FUSED into a specialization entry anchored in some other file. Its `ty`/`tok` ARE
    /// its body's by definition, so a mint never supplies them separately.
    let callerExpr (origin: OriginFile) (body: TExprG<'ty, 'tok, 'id>) : TExprG<'ty, 'tok, 'id> =
        TExprG.CallerExpr(body, origin, exprTy body, exprTok body)

    /// The node under any caller marks. `CallerExpr` is semantically transparent, so a SHAPE
    /// test (is this an `External`? an applied function?) must read through it, and marks
    /// NEST, so it pops as many layers as fusion added.
    let rec unmarked (e: TExprG<'ty, 'tok, 'id>) : TExprG<'ty, 'tok, 'id> =
        match e with
        | TExprG.CallerExpr(body = body) -> unmarked body
        | _ -> e

    let patTy (p: TPatG<'ty, 'tok, 'id>) : 'ty =
        match p with
        | TPatG.NamedSimple(ty = ty)
        | TPatG.Wildcard(ty = ty)
        | TPatG.Tuple(ty = ty)
        | TPatG.Const(ty = ty)
        | TPatG.Record(ty = ty)
        | TPatG.Union(ty = ty)
        | TPatG.TypeTestAs(ty = ty)
        | TPatG.Null(ty = ty)
        | TPatG.EnumCase(ty = ty)
        | TPatG.Or(ty = ty) -> ty

    let patTok (p: TPatG<'ty, 'tok, 'id>) : 'tok =
        match p with
        | TPatG.NamedSimple(tok = tok)
        | TPatG.Wildcard(tok = tok)
        | TPatG.Tuple(tok = tok)
        | TPatG.Const(tok = tok)
        | TPatG.Record(tok = tok)
        | TPatG.Union(tok = tok)
        | TPatG.TypeTestAs(tok = tok)
        | TPatG.Null(tok = tok)
        | TPatG.EnumCase(tok = tok)
        | TPatG.Or(tok = tok) -> tok

    /// Peel a curried `App` chain into the applied function and the arguments paired with
    /// each `App` node's *result* type. The inverse of `rebuildApp`.
    let rec collectAppChain
        (acc: (TExprG<'ty, 'tok, 'id> * 'ty * 'tok) list)
        (e: TExprG<'ty, 'tok, 'id>)
        : TExprG<'ty, 'tok, 'id> * (TExprG<'ty, 'tok, 'id> * 'ty * 'tok) list =
        match e with
        | TExprG.App(fn, arg, ty, tok) -> collectAppChain ((arg, ty, tok) :: acc) fn
        | fn -> fn, acc

    /// Re-fold a function + (arg, result-type, tok) arguments back into a curried
    /// `App` chain. The inverse of `collectAppChain`.
    let rebuildApp
        (fn: TExprG<'ty, 'tok, 'id>)
        (args: (TExprG<'ty, 'tok, 'id> * 'ty * 'tok) list)
        : TExprG<'ty, 'tok, 'id> =
        List.fold (fun acc (arg, resTy, tok) -> TExprG.App(acc, arg, resTy, tok)) fn args

    /// Rewrite hooks. Every `OverrideX` receives the active `Mapper`, so an override can
    /// recurse manually (e.g. to bind a key before walking the body). `ValueSome` replaces
    /// the node; `ValueNone` falls through to the default recursive rebuild.
    [<NoEquality; NoComparison>]
    type Mapper =
        {
            /// Applied to the `ty` field of every node the default arm rebuilds. An
            /// override that builds its own replacement must substitute types itself.
            MapType: SemType -> SemType
            OverrideExpr: Mapper -> TExpr -> TExpr voption
            OverridePat: Mapper -> TPat -> TPat voption
            OverrideArm: Mapper -> TMatchArm -> TMatchArm voption
        }

    let identityMapper: Mapper =
        {
            MapType = id
            OverrideExpr = fun _ _ -> ValueNone
            OverridePat = fun _ _ -> ValueNone
            OverrideArm = fun _ _ -> ValueNone
        }

    /// Only `Interface` carries types: its constraining-interface instantiation args.
    let private mapVia (f: SemType -> SemType) (v: CallVia<SemType>) : CallVia<SemType> =
        match v with
        | CallVia.Interface ifaceArgs ->
            match EqArray.mapPreserve f ifaceArgs with
            | ValueNone -> v
            | ValueSome ifaceArgs' -> CallVia.Interface ifaceArgs'
        | CallVia.Self
        | CallVia.Base -> v

    let rec mapPat (m: Mapper) (p: TPat) : TPat =
        match m.OverridePat m p with
        | ValueSome p' -> p'
        | ValueNone ->
            let f = m.MapType

            match p with
            | TPat.NamedSimple(k, ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then p else TPat.NamedSimple(k, ty', tok)
            | TPat.Wildcard(ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then p else TPat.Wildcard(ty', tok)
            | TPat.Const(v, ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then p else TPat.Const(v, ty', tok)
            | TPat.Tuple(items, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve (mapPat m) items with
                | ValueNone -> if refEq ty' ty then p else TPat.Tuple(items, ty', tok)
                | ValueSome items' -> TPat.Tuple(items', ty', tok)
            | TPat.Record(fields, ty, tok) ->
                let ty' = f ty

                let mapField pair =
                    let (n, sub) = pair
                    let sub' = mapPat m sub
                    if refEq sub' sub then pair else (n, sub')

                match EqArray.mapPreserve mapField fields with
                | ValueNone -> if refEq ty' ty then p else TPat.Record(fields, ty', tok)
                | ValueSome fields' -> TPat.Record(fields', ty', tok)
            | TPat.Union(c, fields, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve (mapPat m) fields with
                | ValueNone -> if refEq ty' ty then p else TPat.Union(c, fields, ty', tok)
                | ValueSome fields' -> TPat.Union(c, fields', ty', tok)
            | TPat.TypeTestAs(testTy, inner, ty, tok) ->
                let testTy' = f testTy
                let inner' = mapPat m inner
                let ty' = f ty

                if refEq testTy' testTy && refEq inner' inner && refEq ty' ty then
                    p
                else
                    TPat.TypeTestAs(testTy', inner', ty', tok)
            | TPat.Null(ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then p else TPat.Null(ty', tok)
            | TPat.EnumCase(k, n, ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then p else TPat.EnumCase(k, n, ty', tok)
            | TPat.Or(alts, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve (mapPat m) alts with
                | ValueNone -> if refEq ty' ty then p else TPat.Or(alts, ty', tok)
                | ValueSome alts' -> TPat.Or(alts', ty', tok)

    let rec mapExpr (m: Mapper) (e: TExpr) : TExpr =
        match m.OverrideExpr m e with
        | ValueSome e' -> e'
        | ValueNone ->
            let f = m.MapType
            let pe = mapExpr m
            let pp = mapPat m
            let pa = mapArm m

            let mapNamedExpr pair =
                let (n, v) = pair
                let v' = pe v
                if refEq v' v then pair else (n, v')

            // Sharing-preserving: each arm returns the input `e` when `f` and the child
            // walk leave every field reference-unchanged. `ForIn`/`Format`/
            // `StaticOptimization` always rebuild, because their nested records rebuild anyway.
            match e with
            | TExpr.Const(v, ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then e else TExpr.Const(v, ty', tok)
            | TExpr.Var(k, ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then e else TExpr.Var(k, ty', tok)
            | TExpr.External(n, k, ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then e else TExpr.External(n, k, ty', tok)
            | TExpr.Null(ty, tok) ->
                let ty' = f ty
                if refEq ty' ty then e else TExpr.Null(ty', tok)
            // `pp p` before the body walk: a bound variable is rewritten before any reference
            // to it.
            | TExpr.Lambda(p, b, ty, tok) ->
                let p' = pp p
                let b' = pe b
                let ty' = f ty

                if refEq p' p && refEq b' b && refEq ty' ty then
                    e
                else
                    TExpr.Lambda(p', b', ty', tok)
            | TExpr.App(fn, a, ty, tok) ->
                let fn' = pe fn
                let a' = pe a
                let ty' = f ty

                if refEq fn' fn && refEq a' a && refEq ty' ty then
                    e
                else
                    TExpr.App(fn', a', ty', tok)
            | TExpr.Let(p, v, body, ty, tok) ->
                let p' = pp p
                let v' = pe v
                let body' = pe body
                let ty' = f ty

                if refEq p' p && refEq v' v && refEq body' body && refEq ty' ty then
                    e
                else
                    TExpr.Let(p', v', body', ty', tok)
            | TExpr.Use(p, v, body, dispose, ty, tok) ->
                let p' = pp p
                let v' = pe v
                let body' = pe body
                let ty' = f ty

                if refEq p' p && refEq v' v && refEq body' body && refEq ty' ty then
                    e
                else
                    TExpr.Use(p', v', body', dispose, ty', tok)
            | TExpr.IfThenElse(c, t, el, ty, tok) ->
                let c' = pe c
                let t' = pe t
                let el' = pe el
                let ty' = f ty

                if refEq c' c && refEq t' t && refEq el' el && refEq ty' ty then
                    e
                else
                    TExpr.IfThenElse(c', t', el', ty', tok)
            | TExpr.Tuple(items, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve pe items with
                | ValueNone -> if refEq ty' ty then e else TExpr.Tuple(items, ty', tok)
                | ValueSome items' -> TExpr.Tuple(items', ty', tok)
            | TExpr.Sequential(items, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve pe items with
                | ValueNone ->
                    if refEq ty' ty then
                        e
                    else
                        TExpr.Sequential(items, ty', tok)
                | ValueSome items' -> TExpr.Sequential(items', ty', tok)
            | TExpr.While(c, b, ty, tok) ->
                let c' = pe c
                let b' = pe b
                let ty' = f ty

                if refEq c' c && refEq b' b && refEq ty' ty then
                    e
                else
                    TExpr.While(c', b', ty', tok)
            // The loop variable is a `NodeKey`, not a `TPat`, so `OverridePat` cannot see it;
            // a pass that renames bound variables must therefore override `ForTo` at the expr level.
            | TExpr.ForTo(k, it, s, e2, b, ty, tok) ->
                let s' = pe s
                let e2' = pe e2
                let b' = pe b
                let ty' = f ty

                if refEq s' s && refEq e2' e2 && refEq b' b && refEq ty' ty then
                    e
                else
                    TExpr.ForTo(k, it, s', e2', b', ty', tok)
            // The enumerator descriptor carries types (the enumerator type, and the
            // seq/enumerator interface args) that reference the enclosing function's
            // typars, so a declaring-typar remap must reach them or they never ground.
            | TExpr.ForIn(p, src, b, en, ty, tok) ->
                TExpr.ForIn(pp p, pe src, pe b, TastConvert.forInEnumerator f en, f ty, tok)
            | TExpr.Match(sc, arms, ty, tok) ->
                let sc' = pe sc
                let ty' = f ty

                match EqArray.mapPreserve pa arms with
                | ValueNone ->
                    if refEq sc' sc && refEq ty' ty then
                        e
                    else
                        TExpr.Match(sc', arms, ty', tok)
                | ValueSome arms' -> TExpr.Match(sc', arms', ty', tok)
            | TExpr.TryWith(b, arms, ty, tok) ->
                let b' = pe b
                let ty' = f ty

                match EqArray.mapPreserve pa arms with
                | ValueNone ->
                    if refEq b' b && refEq ty' ty then
                        e
                    else
                        TExpr.TryWith(b', arms, ty', tok)
                | ValueSome arms' -> TExpr.TryWith(b', arms', ty', tok)
            | TExpr.TryFinally(b, c, ty, tok) ->
                let b' = pe b
                let c' = pe c
                let ty' = f ty

                if refEq b' b && refEq c' c && refEq ty' ty then
                    e
                else
                    TExpr.TryFinally(b', c', ty', tok)
            | TExpr.Assignment(l, r, ty, tok) ->
                let l' = pe l
                let r' = pe r
                let ty' = f ty

                if refEq l' l && refEq r' r && refEq ty' ty then
                    e
                else
                    TExpr.Assignment(l', r', ty', tok)
            | TExpr.Range(s, step, e2, ty, tok) ->
                let s' = pe s

                let step' =
                    match step with
                    | Some st ->
                        let st' = pe st
                        if refEq st' st then step else Some st'
                    | None -> step

                let e2' = pe e2
                let ty' = f ty

                if refEq s' s && refEq step' step && refEq e2' e2 && refEq ty' ty then
                    e
                else
                    TExpr.Range(s', step', e2', ty', tok)
            | TExpr.RecordCons(fields, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve mapNamedExpr fields with
                | ValueNone ->
                    if refEq ty' ty then
                        e
                    else
                        TExpr.RecordCons(fields, ty', tok)
                | ValueSome fields' -> TExpr.RecordCons(fields', ty', tok)
            | TExpr.RecordClone(src, ov, ty, tok) ->
                let src' = pe src
                let ty' = f ty

                match EqArray.mapPreserve mapNamedExpr ov with
                | ValueNone ->
                    if refEq src' src && refEq ty' ty then
                        e
                    else
                        TExpr.RecordClone(src', ov, ty', tok)
                | ValueSome ov' -> TExpr.RecordClone(src', ov', ty', tok)
            | TExpr.FieldGet(r, n, ty, tok) ->
                let r' = pe r
                let ty' = f ty

                if refEq r' r && refEq ty' ty then
                    e
                else
                    TExpr.FieldGet(r', n, ty', tok)
            | TExpr.FieldSet(r, n, v, ty, tok) ->
                let r' = pe r
                let v' = pe v
                let ty' = f ty

                if refEq r' r && refEq v' v && refEq ty' ty then
                    e
                else
                    TExpr.FieldSet(r', n, v', ty', tok)
            | TExpr.UnionCons(c, args, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if refEq ty' ty then
                        e
                    else
                        TExpr.UnionCons(c, args, ty', tok)
                | ValueSome args' -> TExpr.UnionCons(c, args', ty', tok)
            | TExpr.New(c, k, args, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone -> if refEq ty' ty then e else TExpr.New(c, k, args, ty', tok)
                | ValueSome args' -> TExpr.New(c, k, args', ty', tok)
            // `CallVia.Interface` carries the constraining interface's instantiation type
            // args, which reference the enclosing type's typars, so a declaring-typar remap
            // must reach them too, else they leak as un-ground `TyVar`s at the freeze cut.
            | TExpr.MethodCall(r, k, via, args, ty, tok) ->
                let r' = pe r
                let via' = mapVia f via
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if refEq r' r && refEq via' via && refEq ty' ty then
                        e
                    else
                        TExpr.MethodCall(r', k, via', args, ty', tok)
                | ValueSome args' -> TExpr.MethodCall(r', k, via', args', ty', tok)
            | TExpr.PropertyGet(r, k, via, ty, tok) ->
                let r' = pe r
                let via' = mapVia f via
                let ty' = f ty

                if refEq r' r && refEq via' via && refEq ty' ty then
                    e
                else
                    TExpr.PropertyGet(r', k, via', ty', tok)
            | TExpr.StaticMethodCall(k, args, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if refEq ty' ty then
                        e
                    else
                        TExpr.StaticMethodCall(k, args, ty', tok)
                | ValueSome args' -> TExpr.StaticMethodCall(k, args', ty', tok)
            | TExpr.StaticPropertyGet(k, ty, tok) ->
                let ty' = f ty

                if refEq ty' ty then
                    e
                else
                    TExpr.StaticPropertyGet(k, ty', tok)
            | TExpr.StaticFieldGet(k, n, ty, tok) ->
                let ty' = f ty

                if refEq ty' ty then
                    e
                else
                    TExpr.StaticFieldGet(k, n, ty', tok)
            | TExpr.StaticFieldSet(k, n, v, ty, tok) ->
                let v' = pe v
                let ty' = f ty

                if refEq v' v && refEq ty' ty then
                    e
                else
                    TExpr.StaticFieldSet(k, n, v', ty', tok)
            | TExpr.ExternalMember(r, k, n, isProp, ty, tok) ->
                // `r` is a struct `voption`, so the object argument's preservation is
                // observed through the wrapped `TExpr`, not the wrapper.
                match r with
                | ValueNone ->
                    let ty' = f ty

                    if refEq ty' ty then
                        e
                    else
                        TExpr.ExternalMember(ValueNone, k, n, isProp, ty', tok)
                | ValueSome x ->
                    let x' = pe x
                    let ty' = f ty

                    if refEq x' x && refEq ty' ty then
                        e
                    else
                        TExpr.ExternalMember(ValueSome x', k, n, isProp, ty', tok)
            | TExpr.Format(sink, segs, ty, tok) ->
                let sink =
                    match sink with
                    | FormatSink.ToWriter(w, nl) -> FormatSink.ToWriter(pe w, nl)
                    | FormatSink.ToBuilder w -> FormatSink.ToBuilder(pe w)
                    | FormatSink.ToStdOut _
                    | FormatSink.ToStdErr _
                    | FormatSink.ToString -> sink

                let segs =
                    segs
                    |> EqArray.map (fun seg ->
                        match seg with
                        | FormatSeg.Lit _ -> seg
                        | FormatSeg.Hole(h, a) -> FormatSeg.Hole({ h with Ty = f h.Ty }, pe a)
                        | FormatSeg.DynHole d ->
                            FormatSeg.DynHole
                                { d with
                                    Width = ValueOption.map pe d.Width
                                    Precision = ValueOption.map pe d.Precision
                                    Spec = { d.Spec with Ty = f d.Spec.Ty }
                                    Value = pe d.Value
                                }
                        | FormatSeg.CallbackHole(spec, residue) ->
                            FormatSeg.CallbackHole({ spec with Ty = f spec.Ty }, pe residue)
                    )

                TExpr.Format(sink, segs, f ty, tok)
            | TExpr.ILIntrinsic(op, operand, args, ty, tok) ->
                let ty' = f ty
                // `operand` is a struct `SemType voption`; observe its preservation
                // through the wrapped type. The self-host prim-types lean on `(# … #)`
                // heavily, so this arm is on the common path and must preserve.
                let struct (operandUnchanged, operand') =
                    match operand with
                    | ValueSome o ->
                        let o' = f o
                        struct (refEq o' o, ValueSome o')
                    | ValueNone -> struct (true, ValueNone)

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if operandUnchanged && refEq ty' ty then
                        e
                    else
                        TExpr.ILIntrinsic(op, operand', args, ty', tok)
                | ValueSome args' -> TExpr.ILIntrinsic(op, operand', args', ty', tok)
            // The default rebuild substitutes typars inside constraints too.
            | TExpr.StaticOptimization(clauses, def, ty, tok) ->
                let mapConstraint c =
                    match c with
                    | TStaticOptConstraint.TyconEquals(tp, req) -> TStaticOptConstraint.TyconEquals(f tp, f req)
                    | TStaticOptConstraint.IsStruct tp -> TStaticOptConstraint.IsStruct(f tp)

                let clauses =
                    clauses
                    |> EqArray.map (fun cl ->
                        {
                            Constraints = EqArray.map mapConstraint cl.Constraints
                            Body = pe cl.Body
                        }
                    )

                TExpr.StaticOptimization(clauses, pe def, f ty, tok)
            | TExpr.Upcast(src, ty, tok) ->
                let src' = pe src
                let ty' = f ty

                if refEq src' src && refEq ty' ty then
                    e
                else
                    TExpr.Upcast(src', ty', tok)
            | TExpr.Downcast(src, ty, tok) ->
                let src' = pe src
                let ty' = f ty

                if refEq src' src && refEq ty' ty then
                    e
                else
                    TExpr.Downcast(src', ty', tok)
            | TExpr.TraitCall(supportTy, memberName, args, ty, tok) ->
                let supportTy' = f supportTy
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if refEq supportTy' supportTy && refEq ty' ty then
                        e
                    else
                        TExpr.TraitCall(supportTy', memberName, args, ty', tok)
                | ValueSome args' -> TExpr.TraitCall(supportTy', memberName, args', ty', tok)
            | TExpr.TypeTest(src, testTy, ty, tok) ->
                let src' = pe src
                let testTy' = f testTy
                let ty' = f ty

                if refEq src' src && refEq testTy' testTy && refEq ty' ty then
                    e
                else
                    TExpr.TypeTest(src', testTy', ty', tok)
            // The type map does NOT reach the entry's body: the table is a separate root
            // and is mapped as one. From here it would rewrite a shared entry per call site.
            | TExpr.InlineCall(spec, args, origin, ty, tok) ->
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if refEq ty' ty then
                        e
                    else
                        TExpr.InlineCall(spec, args, origin, ty', tok)
                | ValueSome args' -> TExpr.InlineCall(spec, args', origin, ty', tok)
            | TExpr.CallerExpr(body, origin, _, _) ->
                let body' = pe body

                if refEq body' body then e else callerExpr origin body'

    and mapArm (m: Mapper) (arm: TMatchArm) : TMatchArm =
        match m.OverrideArm m arm with
        | ValueSome a' -> a'
        | ValueNone ->
            let pat' = mapPat m arm.Pat

            let guard' = arm.Guard |> ValueOption.map (mapExpr m)

            // The guard's PRESENCE is carried across, so it moved exactly when the
            // expression inside it did, which `refEq` cannot ask of the `voption` itself.
            let guardMoved =
                match arm.Guard, guard' with
                | ValueSome g, ValueSome g' -> not (refEq g' g)
                | _ -> false

            let body' = mapExpr m arm.Body

            if refEq pat' arm.Pat && not guardMoved && refEq body' arm.Body then
                arm
            else
                {
                    Pat = pat'
                    Guard = guard'
                    Body = body'
                }

    /// The slot enumeration is delegated, so neither map decides for itself which body
    /// slots a declaration has.
    let mapTypeDecl (fTy: SemType -> SemType) (fExpr: TExpr -> TExpr) (td: TTypeDecl) : TTypeDecl =
        TastConvert.typeDecl
            {
                Ty = fTy
                Tok = id
                Id = BoundVarKey.identity
                Body = fExpr
            }
            td

    /// Visit-only hooks. `false` from a `VisitX` skips the default child recursion, `true`
    /// continues with it. Every `VisitX` receives the active `Iter`, so an override that
    /// returns `false` can recurse manually first.
    [<NoEquality; NoComparison>]
    type Iter =
        {
            VisitExpr: Iter -> TExpr -> bool
            VisitPat: Iter -> TPat -> bool
            VisitArm: Iter -> TMatchArm -> bool
        }

    let identityIter: Iter =
        {
            VisitExpr = fun _ _ -> true
            VisitPat = fun _ _ -> true
            VisitArm = fun _ _ -> true
        }

    let rec iterPat (it: Iter) (p: TPat) : unit =
        if it.VisitPat it p then
            match p with
            | TPat.NamedSimple _
            | TPat.Wildcard _
            | TPat.Null _
            | TPat.EnumCase _
            | TPat.Const _ -> ()
            | TPat.Tuple(items, _, _) ->
                for sub in items do
                    iterPat it sub
            | TPat.Record(fields, _, _) ->
                for (_, sub) in fields do
                    iterPat it sub
            | TPat.Union(_, fields, _, _) ->
                for sub in fields do
                    iterPat it sub
            | TPat.TypeTestAs(_, inner, _, _) -> iterPat it inner
            | TPat.Or(alts, _, _) ->
                for sub in alts do
                    iterPat it sub

    let rec iterExpr (it: Iter) (e: TExpr) : unit =
        if it.VisitExpr it e then
            let walk = iterExpr it
            let walkPat = iterPat it
            let walkArm = iterArm it

            match e with
            | TExpr.Const _
            | TExpr.Var _
            | TExpr.External _
            | TExpr.Null _
            | TExpr.StaticPropertyGet _
            | TExpr.StaticFieldGet _ -> ()
            | TExpr.Lambda(p, b, _, _) ->
                walkPat p
                walk b
            | TExpr.App(fn, a, _, _) ->
                walk fn
                walk a
            | TExpr.Let(p, v, body, _, _)
            | TExpr.Use(p, v, body, _, _, _) ->
                walkPat p
                walk v
                walk body
            | TExpr.IfThenElse(c, t, el, _, _) ->
                walk c
                walk t
                walk el
            | TExpr.Tuple(items, _, _)
            | TExpr.Sequential(items, _, _) ->
                for x in items do
                    walk x
            | TExpr.While(c, b, _, _) ->
                walk c
                walk b
            | TExpr.ForTo(_, _, s, e2, b, _, _) ->
                walk s
                walk e2
                walk b
            | TExpr.ForIn(p, src, b, _, _, _) ->
                walkPat p
                walk src
                walk b
            | TExpr.Match(sc, arms, _, _) ->
                walk sc

                for arm in arms do
                    walkArm arm
            | TExpr.TryWith(b, arms, _, _) ->
                walk b

                for arm in arms do
                    walkArm arm
            | TExpr.TryFinally(b, c, _, _) ->
                walk b
                walk c
            | TExpr.Assignment(l, r, _, _) ->
                walk l
                walk r
            | TExpr.Range(s, step, e2, _, _) ->
                walk s
                step |> Option.iter walk
                walk e2
            | TExpr.RecordCons(fields, _, _) ->
                for (_, v) in fields do
                    walk v
            | TExpr.RecordClone(src, ov, _, _) ->
                walk src

                for (_, v) in ov do
                    walk v
            | TExpr.FieldGet(r, _, _, _) -> walk r
            | TExpr.FieldSet(r, _, v, _, _) ->
                walk r
                walk v
            | TExpr.StaticFieldSet(_, _, v, _, _) -> walk v
            | TExpr.UnionCons(_, args, _, _)
            | TExpr.New(_, _, args, _, _)
            | TExpr.StaticMethodCall(_, args, _, _)
            | TExpr.TraitCall(_, _, args, _, _)
            | TExpr.InlineCall(args = args)
            | TExpr.ILIntrinsic(_, _, args, _, _) ->
                for x in args do
                    walk x
            | TExpr.MethodCall(r, _, _, args, _, _) ->
                walk r

                for x in args do
                    walk x
            | TExpr.PropertyGet(r, _, _, _, _) -> walk r
            | TExpr.ExternalMember(r, _, _, _, _, _) -> r |> ValueOption.iter walk
            | TExpr.Format(sink, segs, _, _) ->
                match sink with
                | FormatSink.ToWriter(w, _)
                | FormatSink.ToBuilder w -> walk w
                | FormatSink.ToStdOut _
                | FormatSink.ToStdErr _
                | FormatSink.ToString -> ()

                for seg in segs do
                    match seg with
                    | FormatSeg.Lit _ -> ()
                    | FormatSeg.Hole(_, a) -> walk a
                    | FormatSeg.DynHole d ->
                        ValueOption.iter walk d.Width
                        ValueOption.iter walk d.Precision
                        walk d.Value
                    | FormatSeg.CallbackHole(_, residue) -> walk residue
            // Constraints hold no expr children, so the default walk skips them.
            | TExpr.StaticOptimization(clauses, def, _, _) ->
                walk def

                for c in clauses do
                    walk c.Body
            | TExpr.Upcast(src, _, _)
            | TExpr.Downcast(src, _, _)
            | TExpr.CallerExpr(body = src)
            | TExpr.TypeTest(src, _, _, _) -> walk src

    and iterArm (it: Iter) (arm: TMatchArm) : unit =
        if it.VisitArm it arm then
            iterPat it arm.Pat
            arm.Guard |> ValueOption.iter (iterExpr it)
            iterExpr it arm.Body

    /// Every value `f` yields over `e`'s nodes, in walk order and with repeats. `f` is
    /// asked at EVERY node and a `ValueNone` prunes nothing, so a caller that must stop the
    /// descent at a node writes its own `Iter` instead.
    let chooseExpr (f: TExpr -> 'a voption) (e: TExpr) : 'a list =
        let acc = ResizeArray<'a>()

        iterExpr
            { identityIter with
                VisitExpr =
                    fun _ n ->
                        match f n with
                        | ValueSome x -> acc.Add x
                        | ValueNone -> ()

                        true
            }
            e

        List.ofSeq acc

    /// Every bound variable a set of declarations introduces, anywhere in their trees: the pattern
    /// bound variables plus the `ForTo` loop variables, which have no pattern node. A `Type` decl
    /// contributes none, so this is NOT the whole-file bound variable set.
    let declBoundVars (decls: TDeclG<SemType, SyntaxToken, NodeKey> seq) : HashSet<BoundVarKey> =
        let acc = HashSet<BoundVarKey>(HashIdentity.Structural)

        let it =
            { identityIter with
                VisitPat =
                    fun _ p ->
                        match BoundVarKey.ofPat p with
                        | ValueSome k -> acc.Add k |> ignore
                        | ValueNone -> ()

                        true
                VisitExpr =
                    fun _ e ->
                        match BoundVarKey.ofExpr e with
                        | ValueSome k -> acc.Add k |> ignore
                        | ValueNone -> ()

                        true
            }

        for d in decls do
            match d with
            | TDecl.Let(p, v, _, _) ->
                iterPat it p
                iterExpr it v
            | TDecl.Expression(e, _) -> iterExpr it e
            | TDecl.Type _ -> ()

        acc

    /// Every bound-variable-site `NodeKey` introduced by a `TPat`. A `TExpr.Var` carries its
    /// binding-site key directly, so a free variable is a `Var` whose key is not in scope.
    let rec boundVarsOfTPat (p: TPat) : NodeKey list =
        match p with
        | TPat.NamedSimple(k, _, _) -> [ k ]
        // An or-pattern that binds names is rejected before lowering, so its alternatives
        // introduce no bound variables here.
        | TPat.Or _
        | TPat.Wildcard _
        | TPat.Null _
        | TPat.EnumCase _
        | TPat.Const _ -> []
        | TPat.Tuple(items, _, _) ->
            [
                for sub in items do
                    yield! boundVarsOfTPat sub
            ]
        | TPat.Record(fields, _, _) ->
            [
                for (_, sub) in fields do
                    yield! boundVarsOfTPat sub
            ]
        | TPat.Union(_, fields, _, _) ->
            [
                for sub in fields do
                    yield! boundVarsOfTPat sub
            ]
        | TPat.TypeTestAs(_, inner, _, _) -> boundVarsOfTPat inner

    /// Free variables of `body` RELATIVE to the `bound0` seed: every `TExpr.Var` whose
    /// binding site is neither in the seed nor introduced by a scope the walk enters
    /// (nested lambda, let/use, for, match arm).
    let freeVars (bound0: NodeKey seq) (body: TExpr) : HashSet<NodeKey> =
        let result = HashSet<NodeKey>(HashIdentity.Structural)
        let bound = HashSet<NodeKey>(HashIdentity.Structural)

        for k in bound0 do
            bound.Add k |> ignore

        let addBoundVars (p: TPat) : NodeKey list =
            [
                for k in boundVarsOfTPat p do
                    if bound.Add k then
                        yield k
            ]

        let removeBoundVars (added: NodeKey list) =
            for k in added do
                bound.Remove k |> ignore

        let iter: Iter =
            { identityIter with
                VisitExpr =
                    fun it e ->
                        match e with
                        | TExpr.Var(k, _, _) ->
                            if not (bound.Contains k) then
                                result.Add k |> ignore

                            false
                        | TExpr.Lambda(p, b, _, _) ->
                            let added = addBoundVars p
                            iterExpr it b
                            removeBoundVars added
                            false
                        | TExpr.Let(p, v, b, _, _) ->
                            iterExpr it v
                            let added = addBoundVars p
                            iterExpr it b
                            removeBoundVars added
                            false
                        | TExpr.Use(p, v, b, _, _, _) ->
                            iterExpr it v
                            let added = addBoundVars p
                            iterExpr it b
                            removeBoundVars added
                            false
                        | TExpr.ForTo(k, _, st, en, b, _, _) ->
                            iterExpr it st
                            iterExpr it en
                            let isNew = bound.Add k
                            iterExpr it b

                            if isNew then
                                bound.Remove k |> ignore

                            false
                        | TExpr.ForIn(p, src, b, _, _, _) ->
                            iterExpr it src
                            let added = addBoundVars p
                            iterExpr it b
                            removeBoundVars added
                            false
                        | _ -> true
                VisitArm =
                    fun it arm ->
                        let added = addBoundVars arm.Pat
                        arm.Guard |> ValueOption.iter (iterExpr it)
                        iterExpr it arm.Body
                        removeBoundVars added
                        false
            }

        iterExpr iter body
        result

    /// Every `Var k` occurrence in `body`, tagged with the count of enclosing lambda and
    /// loop bodies (a `While` CONDITION re-runs per iteration and counts; `ForTo` bounds
    /// and a `ForIn` source do not). `[]` or `[0]` means `k` is safe to substitute.
    let usesOf (k: NodeKey) (body: TExpr) : int list =
        let acc = ResizeArray<int>()
        let mutable depth = 0

        let it =
            { identityIter with
                VisitExpr =
                    fun iter e ->
                        match e with
                        | TExpr.Var(vk, _, _) when vk = k ->
                            acc.Add depth
                            false
                        | TExpr.Lambda(_, b, _, _) ->
                            depth <- depth + 1
                            iterExpr iter b
                            depth <- depth - 1
                            false
                        | TExpr.While(c, b, _, _) ->
                            depth <- depth + 1
                            iterExpr iter c
                            iterExpr iter b
                            depth <- depth - 1
                            false
                        | TExpr.ForTo(_, _, s, e2, b, _, _) ->
                            iterExpr iter s
                            iterExpr iter e2
                            depth <- depth + 1
                            iterExpr iter b
                            depth <- depth - 1
                            false
                        | TExpr.ForIn(_, src, b, _, _, _) ->
                            iterExpr iter src
                            depth <- depth + 1
                            iterExpr iter b
                            depth <- depth - 1
                            false
                        | _ -> true
            }

        iterExpr it body
        List.ofSeq acc
