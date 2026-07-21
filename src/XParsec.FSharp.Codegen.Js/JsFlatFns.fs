namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers

/// The Fable-style FLAT module-function helpers, decoupled from the `EmitJs`
/// walker. Each function that needs to lower a sub-expression takes a
/// `build: Frozen.TExpr -> JsExpr` callback (the `EmitJs.buildExpr ctx` closure),
/// exactly as the CLR backend's `EmitCall.flattenGroupPushes` takes a `recur`.
/// Keeping this cluster out of the `buildExpr` mutual-recursion group is what lets
/// it live in its own file and keeps `EmitJs` legible.
///
/// The lone-unit-erase / tuple-flatten dispatch is NOT re-derived here: it is
/// `CompiledFns.flattenPlan` (shared with the CLR backend); this module only renders
/// each `FlatStep` as a `JsExpr`.
module JsFlatFns =

    /// `e[j]` — a positional read of a tuple value (a JS array), used to flatten a
    /// tuple-group argument into its compiled flat parameters.
    let indexMember (e: JsExpr) (j: int) : JsExpr =
        JsExpr.Member(e, JsExpr.Literal(JsLiteral.Number(string j), ValueNone), true, ValueNone)

    /// Does a value-use of a module function with these source groups need a curried
    /// adapter? `TastLower.needsCurryAdapter`: only when the flat call shape differs
    /// from the curried one (arity ≥ 2, or a tuple group); a single `GSimple` / lone
    /// `GUnit` is flat-==-curried (JS ignores the surplus `undefined`), so a bare alias
    /// is enough.
    let needsAdapter = TastLower.needsCurryAdapter

    /// One flat compiled parameter's JS name: a simple binder reads its own slot; a
    /// destructuring leaf (a nested tuple element) renders as a `[a, b]` pattern.
    let paramNameOf (source: string voption) (p: TastLower.StaticParam) : string =
        match p.Pat with
        | None -> binderName source p.Slot
        | Some pat -> lambdaParamName source pat

    /// The SOURCE groups of an EXTERNAL module function, read off the provider's
    /// recorded `ValRepr` (the cross-assembly compiled-form contract, Step C). The
    /// `External` node carries the Elaborate-stamped resolved `SymbolKey`, so the FQN the
    /// provider keys symbols by is `SymbolKeyOps.qualifiedName key` directly — the SAME
    /// string the CLR backend builds from the key in `ClrProvider.TryEmitCall` /
    /// `ClrRecipes.emitExternalCall`. No ambient-prefix re-resolution (and no hard
    /// failure when it diverges): codegen reads what the front end already resolved.
    /// `ValueNone` when there is no key (a test mock) or the symbol carries no `ValRepr`
    /// (a value, a hand-authored runtime primitive, a metadata-layer symbol) — the call
    /// then keeps the curried convention (correct for an all-`GSimple` signature).
    let externalGroups (provider: IExternalSymbolProvider) (key: SymbolKey voption) : Frozen.ArgGroup list voption =
        match key with
        | ValueSome key ->
            match provider.TryLookup(SymbolKeyOps.qualifiedName key) with
            | ValueSome sym -> sym.ValRepr |> ValueOption.map (fun vr -> vr.Groups)
            | ValueNone -> ValueNone
        | ValueNone -> ValueNone

    /// Flatten a saturated call's LEADING spine (one element per source group) to the
    /// flat compiled argument list, rendering each `CompiledFns.flattenPlan` step: a
    /// scalar `Arg` built directly; a `TupleLiteral`'s elements built element-wise; a
    /// `TupleValue` read positionally — a pure value inline (`v[j]`), an impure one
    /// spilled to a temporary (returned in the snd; the caller binds it via `wrapSpills`
    /// so it evaluates exactly once).
    let flattenGroupArgs
        (build: Frozen.TExpr -> JsExpr)
        (groups: Frozen.ArgGroup list)
        (leadingArgs: Frozen.TExpr list)
        : JsExpr list * (string * JsExpr) list =
        let flat = ResizeArray<JsExpr>()
        let spills = ResizeArray<string * JsExpr>()

        for step in CompiledFns.flattenPlan groups leadingArgs do
            match step with
            | CompiledFns.FlatStep.Arg a -> flat.Add(build a)
            | CompiledFns.FlatStep.TupleLiteral elems ->
                for el in elems do
                    flat.Add(build el)
            | CompiledFns.FlatStep.TupleValue(a, elemTys) ->
                let n = List.length elemTys

                if isPureValue a then
                    let je = build a

                    for j in 0 .. n - 1 do
                        flat.Add(indexMember je j)
                else
                    let tmp = "_tg" + string (TastWalk.exprTok a).StartIndex
                    spills.Add(tmp, build a)

                    for j in 0 .. n - 1 do
                        flat.Add(indexMember (JsExpr.Identifier(tmp, ValueNone)) j)

        List.ofSeq flat, List.ofSeq spills

    /// Wrap a flat call in an IIFE binding each spilled tuple value once, so an impure
    /// tuple argument flattened to N reads is still evaluated exactly once.
    let private wrapSpills (spills: (string * JsExpr) list) (call: JsExpr) (loc: JsLoc voption) : JsExpr =
        match spills with
        | [] -> call
        | _ ->
            JsExpr.Call(
                JsExpr.Arrow([ for (n, _) in spills -> n ], JsFnBody.Expr call, loc),
                [ for (_, e) in spills -> e ],
                loc
            )

    /// A saturated module-function call: collapse the leading spine (one element per
    /// source group) into a single flat `callee(flatArgs…)`, then fold any residual
    /// over-application on as unary calls.
    let emitFlatCall
        (build: Frozen.TExpr -> JsExpr)
        (callee: JsExpr)
        (groups: Frozen.ArgGroup list)
        (spine: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        (loc: JsLoc voption)
        : JsExpr =
        let leading, rest = List.splitAt (List.length groups) spine

        let flatArgs, spills =
            flattenGroupArgs build groups (leading |> List.map (fun (a, _, _) -> a))

        let flatCall = wrapSpills spills (JsExpr.Call(callee, flatArgs, loc)) loc

        rest
        |> List.fold (fun acc (a, _, _) -> JsExpr.Call(acc, [ build a ], ValueNone)) flatCall

    /// Wrap a flat `callee` in a curried adapter matching its SOURCE arity, so a
    /// value-use / partial application sees the same currying a curried consumer
    /// expects: `(c0) => (c1) => callee(c0, c1)`. A tuple group's single curried
    /// parameter is destructured into the flat call's positional reads; a lone unit
    /// parameter is accepted and dropped. `off` disambiguates the synthetic names.
    let curryAdapter (callee: JsExpr) (groups: Frozen.ArgGroup list) (off: int) (loc: JsLoc voption) : JsExpr =
        let isLone = TastLower.isLoneUnitGroup groups

        let names = groups |> List.mapi (fun i _ -> "_c" + string off + "_" + string i)

        let flatArgs =
            List.zip names groups
            |> List.collect (fun (pn, g) ->
                match g with
                | ArgGroupG.GUnit _ when isLone -> []
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> [ JsExpr.Identifier(pn, ValueNone) ]
                | ArgGroupG.GTuple pat when TastAccessor.patKind pat = PatShape.Tuple ->
                    let items = TastAccessor.patChildren pat

                    [
                        for j in 0 .. items.Length - 1 -> indexMember (JsExpr.Identifier(pn, ValueNone)) j
                    ]
                | ArgGroupG.GTuple _ -> failwith "EmitJs: curryAdapter: GTuple must carry a tuple pattern"
            )

        nestUnaryArrows loc names (JsFnBody.Expr(JsExpr.Call(callee, flatArgs, ValueNone)))
