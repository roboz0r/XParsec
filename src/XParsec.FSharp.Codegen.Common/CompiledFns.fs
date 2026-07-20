namespace XParsec.FSharp.Codegen.Common

open XParsec.FSharp.SemanticAnalysis

/// Backend-agnostic compiled-form analysis of a file's top-level module functions —
/// the facts both the CLR and JS backends need to lower a `let f … = …`. `gather`
/// returns each function's flat compiled signature (`CompiledForm`): the SOURCE arity
/// groups (how many spine applications a saturated call collapses), the tuple-expanded
/// / lone-unit-erased flat params, and the `void`-vs-value return — derived via the
/// same `TastLower.peelValRepr` + `compiledOf` builders Elaborate runs.
///
/// The flat form is a function's ABI: a publicly reachable function ALWAYS exports it.
/// A value-use / under-application is
/// therefore ADDITIVE — it signals a curried bridge is *also* required, never that the
/// flat form is suppressed. Both backends share this model: the flat method is always
/// emitted and an escaping reference *adds* a curried bridge — the JS backend via
/// `curryAdapter`, the CLR backend via `EmitClosures.bridgeStaticFnEscapes` (which
/// derives saturation from the spine directly). The only CLR-private demotion left is
/// the *capture* axis (`EmitClosures.staticEligible`), genuinely intrinsic to a
/// `this`-less static method and orthogonal to escape.
module CompiledFns =

    /// One top-level module-function binding's compiled form. `Groups.Length` is the
    /// number of source applications a saturated call consumes; `Params` is the flat
    /// CLR/JS parameter vector (a tupled group expands to N flat params, a lone unit
    /// group erases to none); `Body` is the residual after the groups are peeled.
    type CompiledFn =
        {
            Key: NodeKey
            Groups: Frozen.ArgGroup list
            Params: TastLower.StaticParam list
            Body: Frozen.TExpr
            ResultTy: FrozenType
            /// `true` when the source result is `unit` — CLR `void` / JS no return value.
            ReturnsVoid: bool
        }

    /// One source group's contribution to a saturated call's FLAT pushed-argument
    /// vector — the backend-neutral result of the lone-unit-erase / tuple-flatten
    /// dispatch. Both backends interpret a `FlatStep list`: the CLR pushes IL, the JS
    /// builds `JsExpr`s. The dispatch (and the failwith on a mistyped tuple group)
    /// lives once in `flattenPlan`; only the per-step emission is backend-specific.
    [<RequireQualifiedAccess>]
    type FlatStep =
        /// A scalar group (`GSimple` / non-lone `GUnit`): emit the argument as one value.
        | Arg of Frozen.TExpr
        /// A tupled group whose argument is a literal `Tuple`: emit each element (one
        /// value per element), each evaluated directly.
        | TupleLiteral of EqArray<Frozen.TExpr>
        /// A tupled group whose argument is a tuple *value*: N values read positionally
        /// from it (`elemTys` are its element types; `N = elemTys.Length`). The CLR
        /// spills to a local + reads `ItemN`; the JS reads `v[j]` (spilling an impure
        /// value through an IIFE). Both decide how from `elemTys`/the value itself.
        | TupleValue of value: Frozen.TExpr * elemTys: FrozenType list

    /// Flatten a saturated call's LEADING spine (one element per SOURCE group) into the
    /// backend-neutral push plan: a lone `()` group contributes nothing; a `GSimple` /
    /// non-lone `GUnit` one `Arg`; a `GTuple` either a `TupleLiteral` (its argument is a
    /// literal `Tuple`) or a `TupleValue` (any other tuple-typed expression). The single
    /// home of the lone-unit-erase / literal-vs-value tuple dispatch the CLR
    /// (`EmitCall.flattenGroupPushes`) and JS (`EmitJs.flattenGroupArgs`) interpreters
    /// share.
    let flattenPlan (groups: Frozen.ArgGroup list) (leadingArgs: Frozen.TExpr list) : FlatStep list =
        let isLone = TastLower.isLoneUnitGroup groups

        [
            for g, a in List.zip groups leadingArgs do
                match g with
                | ArgGroupG.GUnit _ when isLone -> () // lone unit erased — contributes nothing
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> FlatStep.Arg a
                | ArgGroupG.GTuple _ ->
                    match a with
                    | TExprG.Tuple(elems, _, _) -> FlatStep.TupleLiteral elems
                    | _ ->
                        match TastLower.typeOfExpr a with
                        | FTTuple xs -> FlatStep.TupleValue(a, EqArray.toList xs)
                        | other -> failwithf "flattenPlan: tuple-group argument is not a tuple type: %A" other
        ]

    /// Gather every top-level `let f … = …` whose value peels to ≥ 1 source group
    /// (a function, not a zero-param value), in declaration order. Must be called on
    /// already-`lower`ed decls (the curried `Lambda` chain still present in `value`).
    let gather (decls: Frozen.TDecl list) : CompiledFn list =
        [
            for d in decls do
                match d with
                | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                    match TastLower.peelValRepr value with
                    | (_ :: _ as groups), body ->
                        let resultTy = TastLower.typeOfExpr body

                        let vr: TastLower.ValRepr =
                            {
                                Typars = 0 // unused by `compiledOf`; the real count is a backend concern
                                Groups = groups
                                ResultTy = resultTy
                            }

                        let cf = TastLower.compiledOf vr

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
        ]
