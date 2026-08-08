namespace XParsec.FSharp.Codegen.Common

open XParsec.FSharp.SemanticAnalysis

/// Backend-agnostic compiled form of a file's top-level `let f … = …` bindings — the flat
/// parameters and `void`-vs-value return both the CLR and JS backends lower from. A
/// value-use never suppresses the flat form; it ADDS a curried bridge alongside it.
module CompiledFns =

    /// One top-level module function's compiled form. `Groups.Length` is the source
    /// applications a saturated call consumes; `Params` is the flat parameter vector —
    /// a tuple group expands to N, a lone unit group erases to 0, so the lengths differ.
    type CompiledFn =
        {
            Key: BoundVarId
            Groups: TastAccessor.ArgGroup list
            Params: TastLower.StaticParam list
            Body: TastAccessor.ExprId
            ResultTy: FrozenType
            /// `true` when `ResultTy` is `unit` — CLR `void` / JS no return value.
            ReturnsVoid: bool
        }

    /// One source group's contribution to a saturated call's FLAT argument vector. The CLR
    /// pushes each step as IL, the JS builds `JsExpr`s.
    [<RequireQualifiedAccess>]
    type FlatStep =
        /// A scalar group (`GSimple` / non-lone `GUnit`): emit the argument as one value.
        | Arg of TastAccessor.ExprId
        /// A tupled group whose argument is a literal `Tuple`: emit one value per element.
        | TupleLiteral of EqArray<TastAccessor.ExprId>
        /// A tupled group whose argument is a tuple *value*: N values read positionally
        /// (`N = elemTys.Length`). The CLR spills to a local and reads `ItemN`; the JS
        /// reads `v[j]`, spilling an impure value through an IIFE.
        | TupleValue of value: TastAccessor.ExprId * elemTys: FrozenType list

    let private tupleElemsOf (a: TastAccessor.ExprId) : TastAccessor.ExprId list voption =
        match TastAccessor.exprKind a with
        | ExprShape.Tuple -> ValueSome(List.ofArray (TastAccessor.exprChildren a))
        | _ -> ValueNone

    /// The push plan for a TUPLED member call's ONE argument, opened to the `arity`
    /// positions the member's key declares. Elaborate rewrites `w.M t` into
    /// `let (a, b) = t in w.M(a, b)`, so both spellings arrive as a literal tuple.
    let tupledMemberPlan (what: string) (arity: int) (arg: TastAccessor.ExprId) : FlatStep list =
        match SymbolKeyOps.openTupledArg tupleElemsOf arity arg with
        | ValueSome opened -> [ for a in opened -> FlatStep.Arg a ]
        | ValueNone ->
            failwithf "%s expects %d tupled arguments but its argument is typed %A" what arity (TastAccessor.exprTy arg)

    /// Flatten a saturated call's LEADING arguments (one per SOURCE group): a lone `()`
    /// group contributes nothing; a `GSimple` / non-lone `GUnit` one `Arg`; a `GTuple` a
    /// `TupleLiteral` (literal `Tuple` argument) or a `TupleValue` (any other).
    let flattenPlan (groups: TastAccessor.ArgGroup list) (leadingArgs: TastAccessor.ExprId list) : FlatStep list =
        let isLone = TastLower.isLoneUnitGroup groups

        [
            for g, a in List.zip groups leadingArgs do
                match g with
                | ArgGroupG.GUnit _ when isLone -> ()
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> FlatStep.Arg a
                | ArgGroupG.GTuple _ ->
                    match TastAccessor.exprKind a with
                    | ExprShape.Tuple -> FlatStep.TupleLiteral(EqArray.ofArray (TastAccessor.exprChildren a))
                    | _ ->
                        match TastAccessor.exprTy a with
                        | FTTuple xs -> FlatStep.TupleValue(a, EqArray.toList xs)
                        | other -> failwithf "flattenPlan: tuple-group argument is not a tuple type: %A" other
        ]

    /// Gather every top-level `let f … = …` whose value peels to ≥ 1 source group (a
    /// function, not a zero-param value), in declaration order. Needs decls already
    /// flattened by `TastLower.lower`, which leaves the curried `Lambda` chain intact.
    let gather (decls: TastAccessor.DeclId list) : CompiledFn list =
        [
            for d in decls do
                match d with
                | TastAccessor.DLet lv ->
                    match lv.Pattern with
                    | TastAccessor.PNamed k ->
                        match TastLower.peelValRepr lv.Value with
                        | (_ :: _ as groups), body ->
                            let resultTy = TastAccessor.exprTy body

                            let vr: TastLower.ValRepr =
                                {
                                    Typars = 0 // `compiledOf` reads only `Groups` / `ResultTy`
                                    Groups = groups
                                    ResultTy = resultTy
                                }

                            let cf = TastLower.compiledOf lv.Value.Pool vr

                            {
                                Key = k
                                Groups = groups
                                Params = cf.Params
                                Body = body
                                ResultTy = resultTy
                                ReturnsVoid =
                                    match cf.Return with
                                    | CompiledReturnG.RVoid -> true
                                    | CompiledReturnG.RValue _ -> false
                            }
                        | [], _ -> ()
                    | _ -> ()
                | _ -> ()
        ]
