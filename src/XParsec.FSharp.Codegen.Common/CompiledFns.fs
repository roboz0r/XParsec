namespace XParsec.FSharp.Codegen.Common

open XParsec.FSharp.SemanticAnalysis

/// Backend-agnostic compiled form of a file's top-level `let f … = …` bindings — the flat
/// parameters and `void`-vs-value return both the CLR and JS backends lower from. A
/// value-use never suppresses the flat form; it ADDS a curried bridge alongside it.
module CompiledFns =

    /// A function's SOURCE groups and the FLAT vector they expand to, held as one segment per
    /// group: a tuple group spans several slots, a lone `()` group none. Both views are read
    /// off the segments, so a group count cannot stand in for a flat one.
    [<NoEquality; NoComparison>]
    type FlatParams<'T> =
        private
            {
                Segments: (TastAccessor.ArgGroup * 'T list) list
            }

        /// The source groups: a saturated call applies one argument to each.
        member this.Groups = [ for (g, _) in this.Segments -> g ]
        /// The applications a saturated call consumes.
        member this.GroupCount = List.length this.Segments
        /// The compiled parameter vector, in emitted order.
        member this.Flat = this.Segments |> List.collect snd
        /// The emitted parameter count.
        member this.FlatCount = List.sumBy (fun (_, xs) -> List.length xs) this.Segments
        /// Each source group with the flat slots it expands to, in source order.
        member this.ByGroup = this.Segments

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FlatParams =

        /// The segments must be a flattening of the groups they carry — `TastLower`'s
        /// `compiledSegments` / `groupTypeSegments` are what produce one.
        let ofSegments (segments: (TastAccessor.ArgGroup * 'T list) list) : FlatParams<'T> = { Segments = segments }

        /// Re-read every slot, keeping the grouping: compiled params → their types, names.
        let map (f: 'a -> 'b) (ps: FlatParams<'a>) : FlatParams<'b> =
            {
                Segments = [ for (g, xs) in ps.ByGroup -> g, List.map f xs ]
            }

    /// One top-level module function's compiled form.
    type CompiledFn =
        {
            Key: BoundVarId
            Params: FlatParams<TastLower.StaticParam>
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

    type AppliedArg = TastAccessor.ExprId * FrozenType * Anchor

    /// A member call opened at its argument groups.
    [<NoEquality; NoComparison>]
    type MemberCallPlan =
        {
            /// One push per DECLARED parameter, in .NET argument order.
            Steps: FlatStep list
            /// Applied to what the member RETURNS, so the backend folds these on afterwards.
            Residual: AppliedArg list
        }

    /// Elaborate rewrites `w.M t` into `let (a, b) = t in w.M(a, b)`, so a tupled group always
    /// arrives as a literal tuple. `ValueNone` is an under-applied member or a group that is
    /// not that tuple: neither is a direct call, and both eta-wrap instead.
    let memberCallPlan (widths: EqArray<int>) (args: AppliedArg list) : MemberCallPlan voption =
        let asTuple ((a, _, _): AppliedArg) : AppliedArg list voption =
            tupleElemsOf a
            |> ValueOption.map (List.map (fun e -> e, TastAccessor.exprTy e, TastAccessor.exprTok e))

        SymbolKeyOps.openArgGroups asTuple widths args
        |> ValueOption.map (fun opened ->
            {
                Steps = [ for (a, _, _) in opened.Flat -> FlatStep.Arg a ]
                Residual = opened.Residual
            }
        )

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
                                    Typars = 0 // the segmentation reads `Groups` alone
                                    Groups = groups
                                    ResultTy = resultTy
                                }

                            {
                                Key = k
                                Params = FlatParams.ofSegments (TastLower.compiledSegments lv.Value.Pool vr)
                                Body = body
                                ResultTy = resultTy
                                ReturnsVoid = TastLower.isUnitFrozen resultTy
                            }
                        | [], _ -> ()
                    | _ -> ()
                | _ -> ()
        ]
