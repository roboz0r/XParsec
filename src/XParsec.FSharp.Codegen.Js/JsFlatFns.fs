namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers

module JsFlatFns =

    /// A tuple value is a JS array, so element `j` of `e` reads as `e[j]`.
    let indexMember (e: JsExpr) (j: int) : JsExpr =
        JsExpr.Member(e, JsExpr.Literal(JsLiteral.Number(string j), ValueNone), true, ValueNone)

    let needsAdapter = TastLower.needsCurryAdapter

    /// One flat parameter's JS name; a tuple pattern renders as a `[a, b]` destructuring.
    let paramNameOf (pool: PoolBuilder) (p: TastLower.StaticParam) : string =
        match p.Pat with
        | None -> boundVarNameOf pool p.Slot
        | Some pat -> lambdaParamName pool pat

    /// An external module function's SOURCE groups. `ValueNone` unless the `key` resolves to a
    /// symbol carrying a `ValRepr`, so its call and its value-use stay curried, one at a time.
    let externalGroups (provider: IExternalSymbolProvider) (key: BindingKey) : TastAccessor.ArgGroup list voption =
        match provider.TryLookupByKey key with
        | ValueSome sym -> sym.ValRepr |> ValueOption.map (fun vr -> vr.Groups)
        | ValueNone -> ValueNone

    /// A tuple argument becomes N positional reads, so an impure one is spilled to a `_tg`
    /// temporary (the snd) instead of being read N times; `wrapSpills` binds it.
    let renderFlatSteps
        (pool: PoolBuilder)
        (build: TastAccessor.ExprId -> JsExpr)
        (steps: CompiledFns.FlatStep list)
        : JsExpr list * (string * JsExpr) list =
        let flat = ResizeArray<JsExpr>()
        let spills = ResizeArray<string * JsExpr>()

        for step in steps do
            match step with
            | CompiledFns.FlatStep.Arg a -> flat.Add(build a)
            | CompiledFns.FlatStep.TupleLiteral elems ->
                for el in elems do
                    flat.Add(build el)
            | CompiledFns.FlatStep.TupleValue(a, elemTys) ->
                let n = elemTys.Length

                if isPureValue a then
                    let je = build a

                    for j in 0 .. n - 1 do
                        flat.Add(indexMember je j)
                else
                    let tmp = freshTemp pool "_tg"
                    spills.Add(tmp, build a)

                    for j in 0 .. n - 1 do
                        flat.Add(indexMember (JsExpr.Identifier(tmp, ValueNone)) j)

        List.ofSeq flat, List.ofSeq spills

    let flattenGroupArgs
        (pool: PoolBuilder)
        (build: TastAccessor.ExprId -> JsExpr)
        (groups: TastAccessor.ArgGroup list)
        (leadingArgs: TastAccessor.ExprId list)
        : JsExpr list * (string * JsExpr) list =
        CompiledFns.flattenPlan groups leadingArgs |> renderFlatSteps pool build

    /// Bind the spilled values in an IIFE around the call, evaluating each once:
    /// `((_tg4) => callee(_tg4[0], _tg4[1]))(arg)`.
    let wrapSpills (spills: (string * JsExpr) list) (call: JsExpr) (loc: JsLoc voption) : JsExpr =
        match spills with
        | [] -> call
        | _ ->
            JsExpr.Call(
                JsExpr.Arrow([ for (n, _) in spills -> n ], JsFnBody.Expr call, loc),
                [ for (_, e) in spills -> e ],
                loc
            )

    /// One argument per source group collapses into one flat call, and any surplus folds on
    /// unary: `f a b c` against two groups emits `f(a, b)(c)`.
    let emitFlatCall
        (pool: PoolBuilder)
        (build: TastAccessor.ExprId -> JsExpr)
        (callee: JsExpr)
        (groups: TastAccessor.ArgGroup list)
        (appArgs: TastAccessor.AppliedArg list)
        (loc: JsLoc voption)
        : JsExpr =
        let leading, rest = List.splitAt (List.length groups) appArgs

        let flatArgs, spills =
            flattenGroupArgs pool build groups (leading |> List.map (fun a -> a.Arg))

        let flatCall = wrapSpills spills (JsExpr.Call(callee, flatArgs, loc)) loc

        rest
        |> List.fold (fun acc a -> JsExpr.Call(acc, [ build a.Arg ], ValueNone)) flatCall

    /// Re-curry a flat `callee` to its SOURCE arity, one arrow per group:
    /// `(c0) => (c1) => callee(c0, c1)`. A tuple group's one parameter is read positionally
    /// (`(c0) => callee(c0[0], c0[1])`); a lone unit parameter is taken and dropped.
    let curryAdapter
        (pool: PoolBuilder)
        (callee: JsExpr)
        (groups: TastAccessor.ArgGroup list)
        (loc: JsLoc voption)
        : JsExpr =
        let isLone = TastLower.isLoneUnitGroup groups

        let names = groups |> List.map (fun _ -> freshTemp pool "_c")

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
