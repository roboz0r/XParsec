namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared, ctx.Binding, ctx.TypeVar populated.
// Post: ctx.Escape populated for every TypeVar; TypeVar.Region set on
//       TypeVars that participated in the region graph.
//
// Regions are inequality-only (NOT used to drive type-class dispatch).
// Making them feed back into Unification would turn the whole pipeline
// into a fixpoint — see docs/architecture.md "Pass order is strictly forward".
//
// See docs/regions-plan.md for the algorithm.

module Regions =

    /// `Level` is the let-depth the region lives at (its lifetime upper bound).
    /// `MintFunctionLevel` is the let-depth of the innermost enclosing
    /// function-body at mint time; the seed rule `Level < MintFunctionLevel`
    /// catches values that escape that function's frame.
    type private RegionNode =
        {
            Id: RegionId
            Level: int
            MintFunctionLevel: int
            IsLambda: bool
            /// True for the cell region of a `let mutable` binding. Lowers the
            /// lambda-reach threshold from 2 to 1: any closure capture forces
            /// HeapShared (.NET hoists captured mutables into a ref cell, Rust
            /// requires Rc<RefCell<…>>). See docs/mutable-plan.md.
            IsMutableCell: bool
            /// Force-seed: the conservative fallback uses it to mark unhandled
            /// constructs HeapShared without the level / lambda-count heuristics.
            InitialState: EscapeState voption
            mutable Outlives: ResizeArray<RegionId>
        }

    type private RegionGraph() =
        let nodes = ResizeArray<RegionNode>()

        member _.Fresh
            (level: int, mintFn: int, isLambda: bool, isMutableCell: bool, seed: EscapeState voption)
            : RegionId =
            let id = RegionId(nodes.Count)

            nodes.Add(
                {
                    Id = id
                    Level = level
                    MintFunctionLevel = mintFn
                    IsLambda = isLambda
                    IsMutableCell = isMutableCell
                    InitialState = seed
                    Outlives = ResizeArray()
                }
            )

            id

        member _.AddEdge(longer: RegionId, shorter: RegionId) : unit =
            if longer.Raw < 0 || shorter.Raw < 0 then ()
            elif longer.Raw = shorter.Raw then ()
            else nodes.[longer.Raw].Outlives.Add(shorter)

        member _.NodeOf(id: RegionId) : RegionNode = nodes.[id.Raw]
        member _.Count = nodes.Count

    type private State =
        {
            Graph: RegionGraph
            /// headPat NodeKey -> the binding's region. Lets the Ident rule
            /// look up "the region of the binding I refer to" without
            /// re-deriving it from the TyVar.
            BindingRegions: Dictionary<NodeKey, RegionId>
            mutable LetLevel: int
            /// Let-level of the binding whose RHS we are currently evaluating.
            /// Allocations inside the RHS use this as their `Level` so they
            /// share the binding's lifetime upper bound.
            mutable EnclosingLet: int
            /// Stack of let-levels at function-body entry. The top is the frame
            /// depth of the innermost enclosing function — used by the seed rule
            /// and as `MintFunctionLevel` on new regions.
            FunctionStack: ResizeArray<int>
        }

    let private functionStackTop (s: State) : int =
        if s.FunctionStack.Count = 0 then
            0
        else
            s.FunctionStack.[s.FunctionStack.Count - 1]

    let private enterFun (s: State) : unit = s.FunctionStack.Add(s.LetLevel)

    let private exitFun (s: State) : unit =
        s.FunctionStack.RemoveAt(s.FunctionStack.Count - 1)

    /// Resolve a `SemType` through its UnionFind root's Link chain (no walk
    /// into compound shapes). Same as `Unification.resolveStep` but inlined so
    /// Regions doesn't depend on Unification's private surface.
    let rec private resolveLink (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> resolveLink target
            | ValueNone -> TyVar root
        | _ -> t

    /// Does this type represent an allocation we should track? Primitive
    /// scalars and `unit` don't allocate; closures, tuples, and named
    /// composites do. Free TyVars resolve as non-allocating — conservative on
    /// the "don't stamp" side; the caller can override for known-allocating
    /// constructors (Fun, Tuple).
    let rec private isAllocation (t: SemType) : bool =
        match resolveLink t with
        | TyConst name ->
            match name with
            | "int"
            | "int64"
            | "byte"
            | "float"
            | "float32"
            | "decimal"
            | "single"
            | "double"
            | "bool"
            | "unit"
            | "string"
            | "seq<int>" -> false
            | _ -> true
        | TyFun _
        | TyTuple _ -> true
        | TyRecord _ -> true
        | TyUnion _ -> true
        | TyClass _ -> true
        | TyVar _ -> false

    let private exprIsAllocation (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
        let key = CstKeys.ofExpr e

        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> isAllocation (TyVar tv)
        | ValueNone -> false

    /// Collect every binding-site NodeKey introduced by `p` (mirrors
    /// `NameResolution.bindingsOfPat` but keeps only the keys).
    let rec private bindersOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : NodeKey list =
        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.CtorIndex.ContainsKey n
            ->
            // Nullary ctor pattern in disguise — binds nothing.
            []
        | Pat.NamedSimple _ -> [ CstKeys.ofPat p ]
        | Pat.Wildcard _
        | Pat.Const _
        | Pat.EmptyBlock _ -> []
        | Pat.EnclosedBlock(pat = inner) -> bindersOfPat ctx inner
        | Pat.Tuple(patterns = pats) -> [ for sub in pats -> bindersOfPat ctx sub ] |> List.concat
        | Pat.Typed(pat = inner) -> bindersOfPat ctx inner
        | Pat.As(pat = inner) -> CstKeys.ofPat p :: bindersOfPat ctx inner
        | Pat.Record(fieldPats = fieldPats) ->
            [ for FieldPat(pat = sub) in fieldPats -> bindersOfPat ctx sub ] |> List.concat
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                last.Length > 0
                && System.Char.IsUpper last.[0]
                && (li.Idents.Length = 1 && ctx.CtorIndex.ContainsKey last
                    || li.Idents.Length = 2 && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])))
            ->
            // Ctor pattern: head binds nothing; sub-patterns introduce binders.
            // A multi-field arg may be a single tuple — recurse and let the
            // Tuple arm flatten.
            [
                for sub in args do
                    yield! bindersOfPat ctx sub
            ]
        | _ -> []

    /// Find every binding-site NodeKey referenced by `body` whose binder lies
    /// outside `body` (the free variables). `locals` is seeded with the
    /// lambda's own parameter pattern keys and grown as the walker enters any
    /// internal scope-introducing construct; any Ident use whose `BindingSite`
    /// isn't in `locals` is free.
    let private collectFreeVarBindingSites
        (ctx: PassContext)
        (paramBinders: NodeKey list)
        (body: Expr<SyntaxToken>)
        : HashSet<NodeKey> =
        let result = HashSet<NodeKey>(HashIdentity.Structural)
        let locals = HashSet<NodeKey>(HashIdentity.Structural)

        for k in paramBinders do
            locals.Add(k) |> ignore

        let consider (useKey: NodeKey) =
            match ctx.Binding.TryGetValue useKey with
            | ValueSome rb when not (locals.Contains rb.BindingSite) -> result.Add(rb.BindingSite) |> ignore
            | _ -> ()

        let walker: CstWalk.ExprWalker<unit> =
            {
                Visit =
                    fun () e ->
                        match e with
                        | Expr.Ident _ -> consider (CstKeys.ofExpr e)
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                            consider (CstKeys.ofExpr e)
                        | _ -> ()
                EnterFun =
                    fun () argPats ->
                        for p in argPats do
                            for k in bindersOfPat ctx p do
                                locals.Add(k) |> ignore
                EnterBindingRhs =
                    fun () _ _ b ->
                        // Sibling names are already added via EnterLetBody;
                        // function-form arg pats are local to this RHS.
                        if not b.argumentPats.IsEmpty then
                            for p in b.argumentPats do
                                for k in bindersOfPat ctx p do
                                    locals.Add(k) |> ignore
                EnterLetBody =
                    fun () bindings ->
                        for b in bindings do
                            for k in bindersOfPat ctx b.headPat do
                                locals.Add(k) |> ignore
                EnterForTo = fun () ident -> locals.Add(CstKeys.ofForToVar ident) |> ignore
                EnterForIn =
                    fun () pat ->
                        for k in bindersOfPat ctx pat do
                            locals.Add(k) |> ignore
                EnterMatchArm =
                    fun () pat ->
                        for k in bindersOfPat ctx pat do
                            locals.Add(k) |> ignore
            }

        // A let-group's headPats must be in `locals` before any sibling RHS
        // runs. The walker calls EnterLetBody only when stepping into the
        // let's body, so it pre-collects headPats so they're visible to
        // sibling RHSes.
        CstWalk.iterExpr walker () body
        result

    let private stampTyVar (ctx: PassContext) (key: NodeKey) (r: RegionId) : unit =
        if r.Raw >= 0 then
            match ctx.TypeVar.TryGetValue key with
            | ValueSome tv -> (UnionFind.find tv).Region <- r
            | ValueNone -> ()

    let rec private inferRegion (s: State) (ctx: PassContext) (e: Expr<SyntaxToken>) : RegionId =
        let result = inferRegionImpl s ctx e
        stampTyVar ctx (CstKeys.ofExpr e) result
        result

    and private inferRegionImpl (s: State) (ctx: PassContext) (e: Expr<SyntaxToken>) : RegionId =
        match e with
        | Expr.Const _ -> RegionId.Unknown
        | Expr.Null _ -> RegionId.Unknown
        | Expr.EmptyBlock _ -> RegionId.Unknown
        | Expr.String _ -> RegionId.Unknown
        | Expr.While _
        | Expr.ForTo _
        | Expr.ForIn _ -> walkUnitBody s ctx e
        | Expr.Record(fieldInitializers = inits) -> recordRegion s ctx inits
        | Expr.RecordClone(expr = src; fieldInitializers = inits) -> recordCloneRegion s ctx src inits
        | Expr.New(expr = argExpr) -> newRegion s ctx argExpr
        | Expr.DotLookup(expr = inner) ->
            // Field read produces no new allocation, so the access region is
            // the receiver's region (the field's own allocation is tracked via
            // the receiver). Walk inner so its capture edges still register.
            inferRegion s ctx inner
        | Expr.Assignment(leftExpr = l; rightExpr = r) ->
            // `lhs <- rhs`: the stored value must escape at least as wide as
            // the cell. Edge runs rhs → cell so propagation pushes the cell's
            // state BACK onto every value stored into it (once the cell is
            // seeded HeapShared by the threshold-of-1 closure-capture rule,
            // each rhs lubs up to match). This is the OPPOSITE direction from
            // tuple-holds-item (`AddEdge(tuple, item)`, tuple lubs up from
            // items): tuples need "any item heap-shared ⇒ tuple heap-shared",
            // cells need "cell heap-shared ⇒ stored values heap-shared".
            // AddEdge short-circuits on RegionId.Unknown, so non-Ident LHSes
            // (record fields, array indices) routing through the conservative
            // fallback need no special case here.
            let lhsR = inferRegion s ctx l
            let rhsR = inferRegion s ctx r
            s.Graph.AddEdge(rhsR, lhsR)
            RegionId.Unknown
        | Expr.Ident _ -> identRegion s ctx e
        | Expr.LongIdentOrOp _ -> identRegion s ctx e
        | Expr.EnclosedBlock(expr = inner) -> inferRegion s ctx inner
        | Expr.TypeAnnotation(expr = inner) -> inferRegion s ctx inner
        | Expr.Sequential(exprs = items) -> seqRegion s ctx items
        | Expr.Fun(argumentPats = argPats; expr = body) -> lambdaRegion s ctx argPats body
        | Expr.Function(rules = Rules(rules = rules)) -> functionLikeLambda s ctx e rules
        | Expr.LetOrUse(bindings = bindings; body = body) -> letRegion s ctx bindings body
        | Expr.Tuple(exprs = items) -> tupleRegion s ctx e items
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            ifThenElseRegion s ctx e cond thenE elifs elseB
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) -> matchRegion s ctx e scrutinee rules
        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> tryWithRegion s ctx e body rules
        | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) -> tryFinallyRegion s ctx e body finallyE
        | Expr.App(funcExpr = fn; argExprs = args) -> appRegion s ctx e fn args
        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> appRegion s ctx e fn (ImmutableArray.Create(arg))
        | Expr.InfixApp(leftExpr = l; rightExpr = r) ->
            inferRegion s ctx l |> ignore
            inferRegion s ctx r |> ignore
            primitiveOrFreshResult s ctx e
        | Expr.PrefixApp(expr = operand) ->
            inferRegion s ctx operand |> ignore
            primitiveOrFreshResult s ctx e
        | Expr.Range(fromExpr = a; toExpr = b) ->
            inferRegion s ctx a |> ignore
            inferRegion s ctx b |> ignore
            RegionId.Unknown
        | Expr.SteppedRange(fromExpr = a; stepExpr = step; toExpr = b) ->
            inferRegion s ctx a |> ignore
            inferRegion s ctx step |> ignore
            inferRegion s ctx b |> ignore
            RegionId.Unknown
        | _ ->
            // Conservative fallback for nodes with no precise rule: mint a
            // region pre-seeded HeapShared. Safe but pessimistic — extend the
            // precise cases above as the subset grows. See
            // docs/regions-plan.md §Conservative fallback.
            s.Graph.Fresh(level = 0, mintFn = 0, isLambda = false, isMutableCell = false, seed = ValueSome HeapShared)

    and private walkUnitBody (s: State) (ctx: PassContext) (e: Expr<SyntaxToken>) : RegionId =
        // While / ForTo / ForIn — type unit, no allocation. Walk sub-expressions
        // so any captures inside are still registered.
        match e with
        | Expr.While(condition = cond; body = body) ->
            inferRegion s ctx cond |> ignore
            inferRegion s ctx body |> ignore
        | Expr.ForTo(startExpr = a; endExpr = b; body = body) ->
            inferRegion s ctx a |> ignore
            inferRegion s ctx b |> ignore
            inferRegion s ctx body |> ignore
        | Expr.ForIn(enumerableExpr = src; body = body) ->
            inferRegion s ctx src |> ignore
            inferRegion s ctx body |> ignore
        | _ -> ()

        RegionId.Unknown

    and private identRegion (s: State) (ctx: PassContext) (e: Expr<SyntaxToken>) : RegionId =
        let key = CstKeys.ofExpr e

        match ctx.Binding.TryGetValue key with
        | ValueSome rb ->
            match s.BindingRegions.TryGetValue rb.BindingSite with
            | true, r -> r
            | false, _ -> RegionId.Unknown
        | ValueNone -> RegionId.Unknown // external — no region

    and private seqRegion (s: State) (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : RegionId =
        // Evaluate every item for side-effects (capture edges). The sequence's
        // region is the LAST item — intermediates don't escape.
        let n = items.Length

        if n = 0 then
            RegionId.Unknown
        else
            for i = 0 to n - 2 do
                inferRegion s ctx items.[i] |> ignore

            inferRegion s ctx items.[n - 1]

    and private tupleRegion
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (items: ImmutableArray<Expr<SyntaxToken>>)
        : RegionId =
        let r =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = false,
                isMutableCell = false,
                seed = ValueNone
            )

        for it in items do
            let ri = inferRegion s ctx it
            s.Graph.AddEdge(r, ri)

        r

    and private newRegion (s: State) (ctx: PassContext) (argExpr: Expr<SyntaxToken>) : RegionId =
        // `new T(args)` — like a tuple/record allocation: a region at the
        // enclosing let-level, one outgoing edge per constructor argument so
        // the object's lifetime upper-bounds its arguments' lifetimes.
        let r =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = false,
                isMutableCell = false,
                seed = ValueNone
            )

        let argR = inferRegion s ctx argExpr
        s.Graph.AddEdge(r, argR)
        r

    and private recordRegion
        (s: State)
        (ctx: PassContext)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : RegionId =
        // Records allocate like tuples: one outgoing edge per field initialiser
        // (record outlives each field's value). Mutable-field cell allocation
        // is deferred to v1.5; the conservative approximation here ties the
        // field's storage lifetime to the record's own region. Assignment to a
        // record field then routes the RHS through the receiver's region rather
        // than a separate cell region — classifying that as too escape-wide is
        // the safe direction.
        let r =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = false,
                isMutableCell = false,
                seed = ValueNone
            )

        for FieldInitializer(expr = e) in inits do
            let ri = inferRegion s ctx e
            s.Graph.AddEdge(r, ri)

        r

    and private recordCloneRegion
        (s: State)
        (ctx: PassContext)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : RegionId =
        // Conservative v1: the clone is a new allocation that outlives both the
        // source record and every override RHS. Sharing regions with the
        // source's individual fields lands when the precise field-cell model does.
        let srcR = inferRegion s ctx src

        let r =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = false,
                isMutableCell = false,
                seed = ValueNone
            )

        s.Graph.AddEdge(r, srcR)

        for FieldInitializer(expr = e) in inits do
            let ri = inferRegion s ctx e
            s.Graph.AddEdge(r, ri)

        r

    and private lambdaRegion
        (s: State)
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : RegionId =
        // Mint the closure's region BEFORE entering the body, so the seed rule
        // sees the outer function-stack top (the function this lambda is
        // constructed inside of).
        let r =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = true,
                isMutableCell = false,
                seed = ValueNone
            )

        let paramBinders =
            [
                for p in argPats do
                    yield! bindersOfPat ctx p
            ]

        // Capture edges first, before any body recursion, so the walker's
        // `locals` set sees the right scope shape.
        let freeVars = collectFreeVarBindingSites ctx paramBinders body

        for bs in freeVars do
            match s.BindingRegions.TryGetValue bs with
            | true, captured -> s.Graph.AddEdge(captured, r)
            | false, _ -> ()

        enterFun s

        // Parameter regions live inside the lambda's own frame — register AFTER
        // enterFun so they pick up the new function-stack top as their
        // MintFunctionLevel. Mirrors processBinding's order for function-form
        // bindings.
        for p in argPats do
            registerParam s ctx p

        let bodyRegion = inferRegion s ctx body
        exitFun s

        // If the function returns an allocating value, that value escapes the
        // function's frame and the closure must reflect that.
        s.Graph.AddEdge(r, bodyRegion)
        r

    and private registerParam (s: State) (ctx: PassContext) (p: Pat<SyntaxToken>) : unit =
        // Mint ONE region per parameter pattern, threaded through every binder
        // via recordBindingRegion — same rule let-bindings use. Sharing a
        // region for `(a, b)` / `x as y` over-approximates safely ("if any
        // escapes, treat siblings as escaping") and keeps parameter and
        // let-destructuring on one rule. Empty-binder patterns (Const /
        // Wildcard) skip the mint.
        match bindersOfPat ctx p with
        | [] -> ()
        | _ ->
            let r = s.Graph.Fresh(s.LetLevel, functionStackTop s, false, false, ValueNone)
            recordBindingRegion s ctx p r

    and private functionLikeLambda
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : RegionId =
        // `function p1 -> e1 | …` ~ `fun x -> match x with …` — a closure with
        // one synthetic parameter. No real param NodeKey to register, so we
        // walk the arms via the body region path.
        let r =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = true,
                isMutableCell = false,
                seed = ValueNone
            )

        // Pattern binders within each arm are local to that arm: treat each
        // arm's pattern as the local binder set for its body/guard.
        for r' in rules do
            match r' with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let armBinders = bindersOfPat ctx pat

                let collect e' =
                    let fv = collectFreeVarBindingSites ctx armBinders e'

                    for bs in fv do
                        match s.BindingRegions.TryGetValue bs with
                        | true, captured -> s.Graph.AddEdge(captured, r)
                        | false, _ -> ()

                collect body

                match guard with
                | ValueSome(PatternGuard(expr = g)) -> collect g
                | ValueNone -> ()
            | _ -> ()

        enterFun s

        for r' in rules do
            match r' with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                registerParam s ctx pat

                match guard with
                | ValueSome(PatternGuard(expr = g)) -> inferRegion s ctx g |> ignore
                | ValueNone -> ()

                let bodyR = inferRegion s ctx body
                s.Graph.AddEdge(r, bodyR)
            | _ -> ()

        exitFun s

        ignore e
        r

    and private letRegion
        (s: State)
        (ctx: PassContext)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : RegionId =
        processBindingGroup s ctx bindings

        match body with
        | ValueSome b -> inferRegion s ctx b
        | ValueNone -> RegionId.Unknown

    and private processBindingGroup
        (s: State)
        (ctx: PassContext)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        : unit =
        let savedEnclosing = s.EnclosingLet
        s.EnclosingLet <- s.LetLevel
        s.LetLevel <- s.LetLevel + 1

        // Pre-pass: mint a closure region for every function-form binding and
        // record it under its headPat. Sibling references (mutual let-rec, or
        // `and` clauses) need the region in BindingRegions before any body walk,
        // otherwise the freeVars lookup misses the sibling and drops the capture
        // edge. Plain bindings can't be pre-minted — their region IS the RHS's
        // region, only known after walking the RHS.
        for b in bindings do
            if not b.argumentPats.IsEmpty then
                let r =
                    s.Graph.Fresh(
                        level = s.EnclosingLet,
                        mintFn = functionStackTop s,
                        isLambda = true,
                        isMutableCell = false,
                        seed = ValueNone
                    )

                recordBindingRegion s ctx b.headPat r

        for b in bindings do
            processBinding s ctx b

        s.LetLevel <- s.LetLevel - 1
        s.EnclosingLet <- savedEnclosing

    and private processBinding (s: State) (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        if b.argumentPats.IsEmpty then
            // Plain binding: region(binding) = region(rhs). For pass-through
            // values (Ident on RHS) this naturally shares the source's region.
            let rhsR = inferRegion s ctx b.expr

            if b.mutableToken.IsSome then
                // `let mutable x = rhs`: the cell is distinct from the rhs value.
                // The cell outlives every value stored into it; the rhs lubs up
                // to match if the cell is later classified wider. See
                // docs/mutable-plan.md §Why mutable cells need a separate region.
                let cell =
                    s.Graph.Fresh(
                        level = s.EnclosingLet,
                        mintFn = functionStackTop s,
                        isLambda = false,
                        isMutableCell = true,
                        seed = ValueNone
                    )

                s.Graph.AddEdge(rhsR, cell)
                recordBindingRegion s ctx b.headPat cell
            else
                recordBindingRegion s ctx b.headPat rhsR
        else
            // Function-form binding: the closure region was pre-minted in
            // processBindingGroup; look it up here.
            let headKey = CstKeys.ofPat b.headPat

            let r =
                match s.BindingRegions.TryGetValue headKey with
                | true, r -> r
                | false, _ ->
                    // Defensive: processBindingGroup pre-mints, but mint on
                    // demand so the pass stays total against future callers.
                    let r =
                        s.Graph.Fresh(
                            level = s.EnclosingLet,
                            mintFn = functionStackTop s,
                            isLambda = true,
                            isMutableCell = false,
                            seed = ValueNone
                        )

                    recordBindingRegion s ctx b.headPat r
                    r

            let paramBinders =
                [
                    for p in b.argumentPats do
                        yield! bindersOfPat ctx p
                ]

            let freeVars = collectFreeVarBindingSites ctx paramBinders b.expr

            for bs in freeVars do
                match s.BindingRegions.TryGetValue bs with
                | true, captured when captured.Raw <> r.Raw -> s.Graph.AddEdge(captured, r)
                | _ -> ()

            enterFun s

            for p in b.argumentPats do
                registerParam s ctx p

            let bodyR = inferRegion s ctx b.expr
            s.Graph.AddEdge(r, bodyR)
            exitFun s

    and private recordBindingRegion (s: State) (ctx: PassContext) (p: Pat<SyntaxToken>) (r: RegionId) : unit =
        // Map every binder this pattern introduces to `r`. Tuples/as recurse so
        // each name shares the same region — a rough approximation
        // (destructuring projects each element), but value-shape destructuring
        // is rare in the v1 subset.
        match p with
        | Pat.NamedSimple _ ->
            let key = CstKeys.ofPat p
            s.BindingRegions.[key] <- r
            stampTyVar ctx key r
        | Pat.EnclosedBlock(pat = inner) ->
            let key = CstKeys.ofPat p
            s.BindingRegions.[key] <- r
            stampTyVar ctx key r
            recordBindingRegion s ctx inner r
        | Pat.Typed(pat = inner) ->
            let key = CstKeys.ofPat p
            s.BindingRegions.[key] <- r
            stampTyVar ctx key r
            recordBindingRegion s ctx inner r
        | Pat.As(pat = inner) ->
            let key = CstKeys.ofPat p
            s.BindingRegions.[key] <- r
            stampTyVar ctx key r
            recordBindingRegion s ctx inner r
        | Pat.Tuple(patterns = pats) ->
            let key = CstKeys.ofPat p
            s.BindingRegions.[key] <- r
            stampTyVar ctx key r

            for sub in pats do
                recordBindingRegion s ctx sub r
        | _ -> ()

    and private ifThenElseRegion
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : RegionId =
        inferRegion s ctx cond |> ignore

        let armRegions = ResizeArray<RegionId>()
        armRegions.Add(inferRegion s ctx thenE)

        for el in elifs do
            let elCond, elExpr =
                match el with
                | ElifBranch.Elif(condition = c; expr = e2)
                | ElifBranch.ElseIf(condition = c; expr = e2) -> c, e2

            inferRegion s ctx elCond |> ignore
            armRegions.Add(inferRegion s ctx elExpr)

        match elseB with
        | ValueSome(ElseBranch(expr = elExpr)) -> armRegions.Add(inferRegion s ctx elExpr)
        | ValueNone -> ()

        if exprIsAllocation ctx e then
            let r =
                s.Graph.Fresh(
                    level = s.EnclosingLet,
                    mintFn = functionStackTop s,
                    isLambda = false,
                    isMutableCell = false,
                    seed = ValueNone
                )

            for armR in armRegions do
                s.Graph.AddEdge(r, armR)

            r
        else
            RegionId.Unknown

    and private matchRegion
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : RegionId =
        inferRegion s ctx scrutinee |> ignore
        let armRegions = ResizeArray<RegionId>()

        for r' in rules do
            match r' with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                registerParam s ctx pat

                match guard with
                | ValueSome(PatternGuard(expr = g)) -> inferRegion s ctx g |> ignore
                | ValueNone -> ()

                armRegions.Add(inferRegion s ctx body)
            | _ -> ()

        if exprIsAllocation ctx e && armRegions.Count > 0 then
            let r =
                s.Graph.Fresh(
                    level = s.EnclosingLet,
                    mintFn = functionStackTop s,
                    isLambda = false,
                    isMutableCell = false,
                    seed = ValueNone
                )

            for armR in armRegions do
                s.Graph.AddEdge(r, armR)

            r
        else
            RegionId.Unknown

    and private tryWithRegion
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : RegionId =
        let bodyR = inferRegion s ctx body
        let armRegions = ResizeArray<RegionId>()
        armRegions.Add(bodyR)

        for r' in rules do
            match r' with
            | Rule.Rule(pat = pat; guard = guard; expr = armBody) ->
                registerParam s ctx pat

                match guard with
                | ValueSome(PatternGuard(expr = g)) -> inferRegion s ctx g |> ignore
                | ValueNone -> ()

                armRegions.Add(inferRegion s ctx armBody)
            | _ -> ()

        if exprIsAllocation ctx e then
            let r =
                s.Graph.Fresh(
                    level = s.EnclosingLet,
                    mintFn = functionStackTop s,
                    isLambda = false,
                    isMutableCell = false,
                    seed = ValueNone
                )

            for armR in armRegions do
                s.Graph.AddEdge(r, armR)

            r
        else
            bodyR

    and private tryFinallyRegion
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : RegionId =
        let bodyR = inferRegion s ctx body
        inferRegion s ctx finallyE |> ignore
        ignore e
        bodyR

    and private appRegion
        (s: State)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : RegionId =
        let fnR = inferRegion s ctx fn
        let argRegions = ResizeArray<RegionId>()

        for a in args do
            argRegions.Add(inferRegion s ctx a)

        if exprIsAllocation ctx e then
            let r =
                s.Graph.Fresh(
                    level = s.EnclosingLet,
                    mintFn = functionStackTop s,
                    isLambda = false,
                    isMutableCell = false,
                    seed = ValueNone
                )

            s.Graph.AddEdge(r, fnR)

            for ar in argRegions do
                s.Graph.AddEdge(r, ar)

            r
        else
            RegionId.Unknown

    and private primitiveOrFreshResult (s: State) (ctx: PassContext) (e: Expr<SyntaxToken>) : RegionId =
        if exprIsAllocation ctx e then
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = false,
                isMutableCell = false,
                seed = ValueNone
            )
        else
            RegionId.Unknown

    let private walkModuleElem (s: State) (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            processBindingGroup s ctx bindings
        | ModuleElem.Expression e -> inferRegion s ctx e |> ignore
        | _ -> ()

    /// Distinct lambda regions reachable from `start` via outlives edges (the
    /// HeapShared seed rule's input).
    let private countReachableLambdas (g: RegionGraph) (start: RegionId) : int =
        let visited = HashSet<int>()
        let stack = Stack<RegionId>()
        stack.Push(start)
        let mutable count = 0

        while stack.Count > 0 do
            let cur = stack.Pop()

            if visited.Add(cur.Raw) then
                let n = g.NodeOf cur

                if n.IsLambda && cur.Raw <> start.Raw then
                    count <- count + 1

                for t in n.Outlives do
                    stack.Push(t)

        count

    let private lub (a: EscapeState) (b: EscapeState) : EscapeState =
        match a, b with
        | HeapShared, _
        | _, HeapShared -> HeapShared
        | CallerStack, _
        | _, CallerStack -> CallerStack
        | LocalStack, LocalStack -> LocalStack

    let private solve (g: RegionGraph) : EscapeState[] =
        let n = g.Count
        let state = Array.create n LocalStack

        for i = 0 to n - 1 do
            let node = g.NodeOf(RegionId(i))

            match node.InitialState with
            | ValueSome s -> state.[i] <- s
            | ValueNone ->
                // Level rule fires only inside a function (MintFunctionLevel is
                // 0 for module-top mints — no escape frame to cross). Lambdas
                // need a STRICT inequality: the closure lives at its bind level,
                // so a same-level closure (`let f x = ... in f 3` inside another
                // function) doesn't escape. Non-lambda allocations (tuples, app
                // results, if-results) use `<=` because anything constructed at
                // the function's frame level can flow out as the return value.
                if node.MintFunctionLevel > 0 then
                    let escapes =
                        if node.IsLambda then
                            node.Level < node.MintFunctionLevel
                        else
                            node.Level <= node.MintFunctionLevel

                    if escapes then
                        state.[i] <- lub state.[i] CallerStack

                // Lambda-count rule: reachable through ≥ N distinct lambdas →
                // HeapShared. Skips lambda regions themselves so a closure that
                // captures itself indirectly isn't promoted spuriously.
                // Threshold is 2 for ordinary regions and 1 for mutable cells —
                // any closure capture of a mutable forces heap allocation (.NET
                // ref-cell hoisting / Rust Rc<RefCell<_>>). See docs/mutable-plan.md.
                if not node.IsLambda then
                    let reach = countReachableLambdas g (RegionId(i))
                    let threshold = if node.IsMutableCell then 1 else 2

                    if reach >= threshold then
                        state.[i] <- HeapShared

        // Iterate to fixpoint.
        let mutable changed = true

        while changed do
            changed <- false

            for i = 0 to n - 1 do
                let node = g.NodeOf(RegionId(i))

                for tgt in node.Outlives do
                    let lifted = lub state.[i] state.[tgt.Raw]

                    if lifted <> state.[i] then
                        state.[i] <- lifted
                        changed <- true

        state

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let s: State =
            {
                Graph = RegionGraph()
                BindingRegions = Dictionary<NodeKey, RegionId>(HashIdentity.Structural)
                LetLevel = 0
                EnclosingLet = 0
                FunctionStack = ResizeArray()
            }

        let elems = CstWalk.implFileElems file

        for m in elems do
            walkModuleElem s ctx m

        let state = solve s.Graph

        for kv in ctx.TypeVar.AsDictionary() do
            let tv = UnionFind.find kv.Value

            if tv.Region.Raw >= 0 && tv.Region.Raw < state.Length then
                ctx.Escape.Set(kv.Key, state.[tv.Region.Raw])
