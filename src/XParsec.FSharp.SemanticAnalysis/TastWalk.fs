namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// Single point where the TAST's recursion shape is enumerated. Six passes used
// to each hand-roll a match over every `TExpr` case (`Inline.substExpr`,
// `Inline.freshen.fE`, `Elaborate.mapExprTypes`, `RefCellPromotion`'s collector +
// rewriter, `ResolvedTypes.walkExpr`); a new TExpr case would silently slip
// past several of them via catch-alls. The walker centralises the enumeration
// — F#'s incomplete-match check now fires here in one place when the TAST
// grows a new case, instead of silently no-oping in five passes.
//
// Two flavours:
//   - `Mapper` (rewrite): the four passes that build a new TExpr per node.
//     `MapType` is applied to the embedded `ty` field of every node the
//     default arm rebuilds; `OverrideX` hooks let a pass short-circuit a case
//     (typically to inject custom binder logic or replace a node entirely).
//   - `Iter` (visit-only): the two passes that collect into mutable state via
//     closures. `VisitX` returns `false` to skip default child recursion
//     (when the handler walked children manually).
//
// Exhaustiveness: every `TExpr` / `TPat` / `FormatSink` / `FormatSeg` /
// `TStaticOptConstraint` case is enumerated explicitly with no `_ -> ()`
// catch-all. ResolvedTypes (and the rest) used to enumerate every case
// individually to keep this guarantee — the walker preserves it by being the
// one place that match happens.

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
        | TExprG.TypeTest(tok = tok) -> tok

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

    /// Peel a curried `App` chain into its head and the arguments paired with
    /// each `App` node's *result* type. The inverse of `rebuildApp`. Shared by
    /// every spine-walking client (`EmitLower`'s eta/lowering, the pre-freeze
    /// `InlineExpansion` pass).
    let rec collectSpine
        (acc: (TExprG<'ty, 'tok, 'id> * 'ty * 'tok) list)
        (e: TExprG<'ty, 'tok, 'id>)
        : TExprG<'ty, 'tok, 'id> * (TExprG<'ty, 'tok, 'id> * 'ty * 'tok) list =
        match e with
        | TExprG.App(fn, arg, ty, tok) -> collectSpine ((arg, ty, tok) :: acc) fn
        | head -> head, acc

    /// Re-fold a head + (arg, result-type, tok) spine back into a curried `App`
    /// chain. The inverse of `collectSpine`.
    let rebuildApp
        (head: TExprG<'ty, 'tok, 'id>)
        (args: (TExprG<'ty, 'tok, 'id> * 'ty * 'tok) list)
        : TExprG<'ty, 'tok, 'id> =
        List.fold (fun acc (arg, resTy, tok) -> TExprG.App(acc, arg, resTy, tok)) head args

    /// Rewrite hooks. Every `OverrideX` receives the active `Mapper` so an
    /// override can recurse manually with the same mapper (e.g. for binders
    /// where the override needs to bind a key before walking the body).
    /// Returning `ValueSome` replaces the node; `ValueNone` falls through to
    /// the default recursive rebuild (which applies `MapType` to the `ty`
    /// field and recurses on children via this mapper).
    [<NoEquality; NoComparison>]
    type Mapper =
        {
            /// Applied to the `ty` field of every node the default arm
            /// rebuilds. An override that constructs its own replacement is
            /// responsible for substituting types in the node it returns.
            MapType: SemType -> SemType
            OverrideExpr: Mapper -> TExpr -> TExpr voption
            OverridePat: Mapper -> TPat -> TPat voption
            OverrideArm: Mapper -> TMatchArm -> TMatchArm voption
        }

    /// Identity mapper: leaves every `ty` and node unchanged. Compose with
    /// `with` to override only the fields a pass needs.
    let identityMapper: Mapper =
        {
            MapType = id
            OverrideExpr = fun _ _ -> ValueNone
            OverridePat = fun _ _ -> ValueNone
            OverrideArm = fun _ _ -> ValueNone
        }

    /// Apply a type map to a `CallVia`'s payload. Only `CallVia.Interface` carries
    /// types (its constraining-interface instantiation args); `Self`/`Base` pass
    /// through unchanged. Sharing-preserving (like `SemType.mapChildren`): returns
    /// the SAME `v` when `f` leaves every payload type reference-unchanged, so an
    /// unaffected `MethodCall`/`PropertyGet` node can itself preserve.
    let mapVia (f: SemType -> SemType) (v: CallVia<SemType>) : CallVia<SemType> =
        match v with
        | CallVia.Interface ifaceArgs ->
            match EqArray.mapPreserve f ifaceArgs with
            | ValueNone -> v
            | ValueSome ifaceArgs' -> CallVia.Interface ifaceArgs'
        | CallVia.Self
        | CallVia.Base -> v

    /// Map the `'ty` payloads of a `for … in` enumerator descriptor — the enumerator
    /// type and (rung-3 constrained-typar source) the seq/enumerator interface
    /// instantiation args. Mirrors `TastConvert.forInEnumerator`, duplicated here
    /// because `TastWalk` precedes `TastConvert` in compile order (the same posture as
    /// `mapVia`). The `SymbolKey` payloads carry no `'ty`, so they pass through.
    let mapForInEnumerator (f: SemType -> SemType) (en: ForInEnumerator) : ForInEnumerator =
        let mapGetEnum ge =
            match ge with
            | ForInGetEnumG.External k -> ForInGetEnumG.External k
            | ForInGetEnumG.Local -> ForInGetEnumG.Local
            | ForInGetEnumG.ConstrainedInterface(iface, args) ->
                ForInGetEnumG.ConstrainedInterface(iface, EqArray.map f args)

        let mapMembers mem =
            match mem with
            | ForInEnumMembersG.External(mn, cur) -> ForInEnumMembersG.External(mn, cur)
            | ForInEnumMembersG.Local -> ForInEnumMembersG.Local
            | ForInEnumMembersG.ConstrainedInterface(iface, args) ->
                ForInEnumMembersG.ConstrainedInterface(iface, EqArray.map f args)

        match en with
        | ForInEnumeratorG.Interface -> ForInEnumeratorG.Interface
        | ForInEnumeratorG.Pattern(enumTy, ge, mem, isVal, disp) ->
            ForInEnumeratorG.Pattern(f enumTy, mapGetEnum ge, mapMembers mem, isVal, disp)

    let rec mapPat (m: Mapper) (p: TPat) : TPat =
        match m.OverridePat m p with
        | ValueSome p' -> p'
        | ValueNone ->
            let f = m.MapType

            // Sharing-preserving, exactly as `SemType.mapChildren`: return the input
            // `p` when `f` and the child walk leave every field reference-unchanged,
            // so preservation propagates up through a `Let`/`Match` that carries it.
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

            // Preserve the `(name, value)` field pair when its value is unchanged, so
            // `mapPreserve` sees a reference-equal element and a record whose fields
            // are all untouched preserves the whole array.
            let mapNamedExpr pair =
                let (n, v) = pair
                let v' = pe v
                if refEq v' v then pair else (n, v')

            // Sharing-preserving, exactly as `SemType.mapChildren`: each arm returns
            // the input `e` when `f` and the child walk leave every field
            // reference-unchanged, so a subtree the pass does not touch walks
            // allocation-free and the sharing propagates up. The rare/gnarly arms
            // (`ForIn`, `Format`, `StaticOptimization`) stay always-rebuilding — their
            // nested record-update shape rebuilds regardless, so preservation there
            // would rarely fire and never carries an unaffected common subtree.
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
            // Tuple-constructor arguments evaluate left-to-right, so `pp p`
            // (binder) always runs before `pe b` / `pe v` / `pe body` — the
            // ordering `Inline.freshen` relies on for `Lambda` / `Let` /
            // `Match`-arm binders.
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
            // `var` is a `NodeKey`, not a `TPat`, so `OverridePat` cannot see
            // it — passes that rename binders (`Inline.freshen`) must override
            // `ForTo` at the expr level.
            | TExpr.ForTo(k, it, s, e2, b, ty, tok) ->
                let s' = pe s
                let e2' = pe e2
                let b' = pe b
                let ty' = f ty

                if refEq s' s && refEq e2' e2 && refEq b' b && refEq ty' ty then
                    e
                else
                    TExpr.ForTo(k, it, s', e2', b', ty', tok)
            // The enumerator descriptor carries `'ty` payloads — the enumerator type
            // and, for a rung-3 constrained-typar source, the seq/enumerator interface
            // instantiation args. They reference the enclosing function's typars, so a
            // declaring-typar remap (`freezeTypars`) must reach them too (the same
            // `mapVia` precedent for `CallVia.Interface`), else they leak as un-ground
            // `TyVar`s → `?free-typar` at the freeze cut.
            | TExpr.ForIn(p, src, b, en, ty, tok) -> TExpr.ForIn(pp p, pe src, pe b, mapForInEnumerator f en, f ty, tok)
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
            // `CallVia.Interface` carries the constraining interface's instantiation
            // type args (rung-3) — they reference the enclosing type's typars, so a
            // declaring-typar remap (`freezeTypars`) must reach them too, else they
            // leak as un-ground `TyVar`s at the freeze cut.
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
                // `r` is a struct `voption`, so the receiver's preservation is
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
            // The default rebuild substitutes typars inside constraints too —
            // `Elaborate.mapExprTypes` (used to push a remap through generic
            // member bodies) needs this. Passes that resolve clauses to a
            // single body (`Inline.substExpr`) override the node explicitly
            // and never reach this arm.
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
            | TExpr.TraitCall(recv, memberName, args, ty, tok) ->
                let recv' = f recv
                let ty' = f ty

                match EqArray.mapPreserve pe args with
                | ValueNone ->
                    if refEq recv' recv && refEq ty' ty then
                        e
                    else
                        TExpr.TraitCall(recv', memberName, args, ty', tok)
                | ValueSome args' -> TExpr.TraitCall(recv', memberName, args', ty', tok)
            | TExpr.TypeTest(src, testTy, ty, tok) ->
                let src' = pe src
                let testTy' = f testTy
                let ty' = f ty

                if refEq src' src && refEq testTy' testTy && refEq ty' ty then
                    e
                else
                    TExpr.TypeTest(src', testTy', ty', tok)

    and mapArm (m: Mapper) (arm: TMatchArm) : TMatchArm =
        match m.OverrideArm m arm with
        | ValueSome a' -> a'
        | ValueNone ->
            let pat' = mapPat m arm.Pat

            let guard' = arm.Guard |> ValueOption.map (mapExpr m)

            // The guard's PRESENCE is carried across, so it moved exactly when the
            // expression inside it did — which `refEq` cannot ask of the `voption` itself.
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

    /// Visit-only hooks. Returning `false` from a `VisitX` skips default child
    /// recursion (the override walked the children it wanted, or wants to skip
    /// them entirely); `true` continues with the default recursive walk.
    /// Every `VisitX` receives the active `Iter` so an override can recurse
    /// manually with the same iter.
    [<NoEquality; NoComparison>]
    type Iter =
        {
            VisitExpr: Iter -> TExpr -> bool
            VisitPat: Iter -> TPat -> bool
            VisitArm: Iter -> TMatchArm -> bool
        }

    /// Identity iter: visits every node and recurses with no extra work.
    /// Compose with `with` to override the cases a pass cares about.
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
            // Default walk skips constraints (no expr children) — the
            // constraint typars are the binding's own quantified typars,
            // already known to passes that care (ResolvedTypes adds them to
            // `allowed`). A pass that needs to visit constraint types
            // overrides this case.
            | TExpr.StaticOptimization(clauses, def, _, _) ->
                walk def

                for c in clauses do
                    walk c.Body
            | TExpr.Upcast(src, _, _)
            | TExpr.Downcast(src, _, _)
            | TExpr.TypeTest(src, _, _, _) -> walk src

    and iterArm (it: Iter) (arm: TMatchArm) : unit =
        if it.VisitArm it arm then
            iterPat it arm.Pat
            arm.Guard |> ValueOption.iter (iterExpr it)
            iterExpr it arm.Body

    /// Every binder a set of declarations introduces, anywhere in their trees: the
    /// pattern binders (`BinderKey.ofPat`) plus the `ForTo` loop variables
    /// (`BinderKey.ofExpr`), which have no pattern node. A `Type` decl contributes none —
    /// its member bodies are walked by no pass here (see `Regions.run`) — so this is NOT
    /// the whole-file binder set (`BinderKey.ofTypeDecl` is the other half).
    ///
    /// This is the pre-freeze twin of the frozen binder pool's enumeration
    /// (`TastPools.toPools`), which is why a table restricted against it is honest: both
    /// sides enumerate through the same projections.
    let declBinders (decls: TDeclG<SemType, SyntaxToken, NodeKey> seq) : HashSet<BinderKey> =
        let acc = HashSet<BinderKey>(HashIdentity.Structural)

        let it =
            { identityIter with
                VisitPat =
                    fun _ p ->
                        match BinderKey.ofPat p with
                        | ValueSome k -> acc.Add k |> ignore
                        | ValueNone -> ()

                        true
                VisitExpr =
                    fun _ e ->
                        match BinderKey.ofExpr e with
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

    /// Every binder-site NodeKey introduced by a `TPat`. A `TExpr.Var` carries
    /// the binding-site key directly, so a free variable is simply a `Var` whose
    /// key is not in scope — no `ctx.Bindings.Binding` resolution needed.
    let rec bindersOfTPat (p: TPat) : NodeKey list =
        match p with
        | TPat.NamedSimple(k, _, _) -> [ k ]
        // An or-pattern binds nothing (name resolution drops its binders), so its
        // alternatives introduce no binders here either.
        | TPat.Or _
        | TPat.Wildcard _
        | TPat.Null _
        | TPat.EnumCase _
        | TPat.Const _ -> []
        | TPat.Tuple(items, _, _) ->
            [
                for sub in items do
                    yield! bindersOfTPat sub
            ]
        | TPat.Record(fields, _, _) ->
            [
                for (_, sub) in fields do
                    yield! bindersOfTPat sub
            ]
        | TPat.Union(_, fields, _, _) ->
            [
                for sub in fields do
                    yield! bindersOfTPat sub
            ]
        | TPat.TypeTestAs(_, inner, _, _) -> bindersOfTPat inner

    /// Free variables of `body` RELATIVE to `bound`: every `TExpr.Var` whose binding
    /// site is neither in the caller-supplied seed nor introduced by a scope the walk
    /// enters (nested lambda, let/use, for, match arm). `bound` grows/shrinks as the
    /// walk enters/leaves each scope.
    ///
    /// The seed is what makes the primitive serve two questions with one walk: a
    /// closure's captures are "free given the lambda's own parameter binders"
    /// (`Regions`), and a published inline template's dangling references are "free
    /// given the template's own binders, after the module-sibling rewrite"
    /// (`Freeze`). Both are the same scope-tracking walk over the same tree, so
    /// neither owns it.
    let freeVars (bound0: NodeKey seq) (body: TExpr) : HashSet<NodeKey> =
        let result = HashSet<NodeKey>(HashIdentity.Structural)
        let bound = HashSet<NodeKey>(HashIdentity.Structural)

        for k in bound0 do
            bound.Add k |> ignore

        let addBinders (p: TPat) : NodeKey list =
            [
                for k in bindersOfTPat p do
                    if bound.Add k then
                        yield k
            ]

        let removeBinders (added: NodeKey list) =
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
                            let added = addBinders p
                            iterExpr it b
                            removeBinders added
                            false
                        | TExpr.Let(p, v, b, _, _) ->
                            iterExpr it v
                            let added = addBinders p
                            iterExpr it b
                            removeBinders added
                            false
                        | TExpr.Use(p, v, b, _, _, _) ->
                            iterExpr it v
                            let added = addBinders p
                            iterExpr it b
                            removeBinders added
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
                            let added = addBinders p
                            iterExpr it b
                            removeBinders added
                            false
                        | _ -> true
                VisitArm =
                    fun it arm ->
                        let added = addBinders arm.Pat
                        arm.Guard |> ValueOption.iter (iterExpr it)
                        iterExpr it arm.Body
                        removeBinders added
                        false
            }

        iterExpr iter body
        result

    /// Every `Var k` occurrence in `body`, each tagged with the count of enclosing
    /// *evaluation-deferring-or-repeating* constructs (lambdas and loop bodies)
    /// above it — `0` for a straight-line or conditional-branch occurrence. A
    /// `While` *condition* re-evaluates each iteration so it counts as repeating;
    /// the bounds of `ForTo` and the source of `ForIn` are evaluated once, so they
    /// stay at the ambient depth; the loop *body* and a lambda body increment. The
    /// canonical primitive for linearity / capture checks
    /// (e.g. `[<CallAtMostOnce>]` validation): a parameter is safe to substitute at
    /// its single use iff `usesOf k scope` is `[]` or `[0]`. Replaces the ad-hoc
    /// depth-tracking iters passes would otherwise hand-roll.
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
