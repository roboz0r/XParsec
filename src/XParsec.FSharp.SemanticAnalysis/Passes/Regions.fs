namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Bindings.Binding / .TypeVar populated; `decls` is post-inline.
// Post: ctx.Bindings.Escape and .Repr populated per binding site; TypeVar.Region set.
// Inlining both removes and creates closures, so escape is computed on what codegen emits.

module Regions =

    /// `Level` is the let-depth the region lives at (its lifetime upper bound).
    /// `MintFunctionLevel` is the let-depth of the innermost enclosing function body
    /// at mint time; `solve` compares the two to seed escape out of that frame.
    type private RegionNode =
        {
            Id: RegionId
            Level: int
            MintFunctionLevel: int
            IsLambda: bool
            /// True for the cell region of a `let mutable` binding: lowers the
            /// lambda-reach threshold from 2 to 1, so any closure capture forces
            /// `HeapShared`.
            IsMutableCell: bool
            /// This region is itself a heap-repr sink — an aggregate container (tuple /
            /// record / union / `new`), or the source of a box / interface upcast.
            /// `solveRepr` flows `RequiresHeapRepr` DOWN this node's `Outlives` edges.
            mutable HeapReprSink: bool
            mutable Outlives: ResizeArray<RegionId>
        }

    type private RegionGraph() =
        let nodes = ResizeArray<RegionNode>()

        member _.Fresh(level: int, mintFn: int, isLambda: bool, isMutableCell: bool) : RegionId =
            let id = RegionId(nodes.Count)

            nodes.Add(
                {
                    Id = id
                    Level = level
                    MintFunctionLevel = mintFn
                    IsLambda = isLambda
                    IsMutableCell = isMutableCell
                    HeapReprSink = false
                    Outlives = ResizeArray()
                }
            )

            id

        member _.AddEdge(longer: RegionId, shorter: RegionId) : unit =
            if longer.Raw < 0 || shorter.Raw < 0 then ()
            elif longer.Raw = shorter.Raw then ()
            else nodes.[longer.Raw].Outlives.Add(shorter)

        /// No-op for `RegionId.Unknown`.
        member _.MarkHeapSink(id: RegionId) : unit =
            if id.Raw >= 0 then
                nodes.[id.Raw].HeapReprSink <- true

        member _.NodeOf(id: RegionId) : RegionNode = nodes.[id.Raw]
        member _.Count = nodes.Count

    type private State =
        {
            Graph: RegionGraph
            /// BoundVar `NodeKey` -> the binding's region; the `TExpr.Var` arm reads it
            /// off the node's carried binding-site key.
            BindingRegions: Dictionary<NodeKey, RegionId>
            mutable LetLevel: int
            /// Let-level of the binding whose RHS is being evaluated. Allocations inside
            /// the RHS take it as their `Level`, sharing the binding's lifetime bound.
            mutable EnclosingLet: int
            /// Stack of let-levels at function-body entry; the top becomes
            /// `MintFunctionLevel` on new regions.
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

    let private freshValue (s: State) : RegionId =
        s.Graph.Fresh(s.EnclosingLet, functionStackTop s, false, false)

    let private freshLambda (s: State) : RegionId =
        s.Graph.Fresh(s.EnclosingLet, functionStackTop s, true, false)

    let private freshCell (s: State) : RegionId =
        s.Graph.Fresh(s.EnclosingLet, functionStackTop s, false, true)

    /// Parameter regions live in the callee frame, one level below the binding's
    /// RHS — hence `LetLevel`, not `EnclosingLet`.
    let private freshParam (s: State) : RegionId =
        s.Graph.Fresh(s.LetLevel, functionStackTop s, false, false)

    /// Resolve a `SemType` through its UnionFind root's Link chain (no walk
    /// into compound shapes).
    let rec private resolveLink (store: TypeStore) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match store.Link root with
            | ValueSome target -> resolveLink store target
            | ValueNone -> TyVar root.Id
        | _ -> t

    /// The primitives that do not allocate. `string` is a reference type but its values
    /// are interned/shared rather than allocated at a use site, so it belongs here with
    /// the scalars.
    let private isNonAllocatingPrimitive =
        RuntimeNames.isKeyIn
            [
                RuntimeNames.intKey
                RuntimeNames.int64Key
                RuntimeNames.byteKey
                RuntimeNames.floatKey
                RuntimeNames.float32Key
                RuntimeNames.decimalKey
                RuntimeNames.boolKey
                RuntimeNames.unitKey
                RuntimeNames.stringKey
            ]

    /// Does this type represent an allocation we should track? Primitive scalars
    /// and `unit` don't allocate; closures, tuples and named composites do.
    /// Unresolved shapes resolve as non-allocating — conservative on "don't stamp".
    let rec private isAllocation (store: TypeStore) (t: SemType) : bool =
        match resolveLink store t with
        // Matched by KEY: a user type merely SPELLED `int` is a nominal composite and
        // does allocate, so a name-only test would wrongly stop tracking it.
        | TyConst(key, _) -> not (isNonAllocatingPrimitive key)
        | TyFun _
        | TyTuple _ -> true
        | TyRecord _ -> true
        | TyUnion _ -> true
        | TyClass _ -> true
        // An anonymous union erases to a boxed reference (`obj` + `isinst`), so a
        // value flowing into one allocates.
        | TyOr _ -> true
        // An unevaluated type-level computation erases like `TyOr` once evaluated.
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> true
        | TyVar _ -> false
        | TyUnknown _ -> false
        // An open typar: like a free `TyVar`, whether it allocates is unknown.
        | TyTypar _ -> false
        // An enum is a value type (numeric → `System.Enum`; string / mixed →
        // a `[<Struct>]` wrapper) — it does not heap-allocate.
        | TyEnum _ -> false
        // A literal erases to its base primitive — an interned `string` or a
        // scalar, both non-allocating here.
        | TyLiteral _ -> false

    let private exprIsAllocation (store: TypeStore) (e: TExpr) : bool = isAllocation store (TastWalk.exprTy e)

    /// Is an `Upcast` to `t` a heap-repr sink? A box to `obj`, or an upcast to the
    /// `Vesper.Fun<_,_>` interface, materialises a reference-typed value and pins the
    /// upcast source to a heap representation.
    let private isHeapReprTarget (store: TypeStore) (t: SemType) : bool =
        match resolveLink store t with
        | TyObj -> true
        | TyFun _ -> true
        | _ -> false

    /// Mint a value region that outlives every child region. Tuples, records, `new`
    /// and clones allocate a composite holding its elements; no such composite can
    /// carry a `ref struct` field, so the region is also a heap-repr sink.
    let private holds (s: State) (children: RegionId seq) : RegionId =
        let r = freshValue s
        s.Graph.MarkHeapSink r

        for c in children do
            s.Graph.AddEdge(r, c)

        r

    /// If `e` allocates, mint a value region that outlives every arm region;
    /// otherwise return `fallback`. The `arms.Count > 0` guard is for a `match`
    /// with no rules, which allocates nothing.
    let private joinArms (store: TypeStore) (s: State) (e: TExpr) (arms: RegionId seq) (fallback: RegionId) : RegionId =
        let arms = ResizeArray(arms)

        if exprIsAllocation store e && arms.Count > 0 then
            let r = freshValue s

            for a in arms do
                s.Graph.AddEdge(r, a)

            r
        else
            fallback

    let private primitiveOrFreshResult (store: TypeStore) (s: State) (e: TExpr) : RegionId =
        if exprIsAllocation store e then
            freshValue s
        else
            RegionId.Unknown

    /// An outlives edge from each captured binding's region to the closure region `r`.
    // TODO(byref-capture): a byref-like capture (`Span` / `ref struct`) of a closure
    // solved `HeapShared` is the reject site — blocked on a byref-like predicate.
    let private addCaptureEdges (s: State) (freeVars: HashSet<NodeKey>) (r: RegionId) : unit =
        for bs in freeVars do
            match s.BindingRegions.TryGetValue bs with
            | true, captured -> s.Graph.AddEdge(captured, r)
            | _ -> ()

    let private stampTyVar (ctx: PassContext) (key: NodeKey) (r: RegionId) : unit =
        if r.Raw >= 0 then
            match ctx.Bindings.TypeVar.TryGetValue key with
            | ValueSome tv -> ctx.Store.SetRegion((UnionFind.find ctx.Store tv).Id, r)
            | ValueNone -> ()

    let rec private inferRegion (s: State) (ctx: PassContext) (e: TExpr) : RegionId =
        match e with
        | TExpr.Const _
        | TExpr.Null _
        | TExpr.External _
        | TExpr.StaticPropertyGet _
        | TExpr.StaticFieldGet _ -> RegionId.Unknown
        | TExpr.Var(k, _, _) ->
            match s.BindingRegions.TryGetValue k with
            | true, r -> r
            | false, _ -> RegionId.Unknown // external / not region-tracked
        // Composite allocations: the region outlives every element's region.
        | TExpr.Tuple(items, _, _) -> holds s [ for it in items -> inferRegion s ctx it ]
        | TExpr.RecordCons(fields, _, _) -> holds s [ for (_, v) in fields -> inferRegion s ctx v ]
        | TExpr.RecordClone(src, ov, _, _) ->
            holds s [ yield inferRegion s ctx src; for (_, v) in ov -> inferRegion s ctx v ]
        | TExpr.New(_, _, args, _, _)
        | TExpr.UnionCons(_, args, _, _) -> holds s [ for a in args -> inferRegion s ctx a ]
        // A field / property read allocates nothing — it rides the receiver's region.
        // Walk the receiver so its capture edges still register.
        | TExpr.FieldGet(r, _, _, _) -> inferRegion s ctx r
        | TExpr.PropertyGet(r, _, _, _, _) -> inferRegion s ctx r
        | TExpr.ExternalMember(rOpt, _, _, _, _, _) ->
            match rOpt with
            | ValueSome r -> inferRegion s ctx r
            | ValueNone -> RegionId.Unknown
        | TExpr.Assignment(l, r, _, _) ->
            // `lhs <- rhs`: the stored value escapes at least as wide as the cell, so
            // the edge runs rhs → cell and propagation pushes the cell's state BACK
            // onto every value stored into it.
            let lhsR = inferRegion s ctx l
            let rhsR = inferRegion s ctx r
            s.Graph.AddEdge(rhsR, lhsR)
            RegionId.Unknown
        | TExpr.FieldSet(recv, _, v, _, _) ->
            // Same direction as `Assignment`: the stored value's lifetime is
            // upper-bounded by the receiver that holds the slot.
            let recvR = inferRegion s ctx recv
            let vR = inferRegion s ctx v
            s.Graph.AddEdge(vR, recvR)
            RegionId.Unknown
        | TExpr.StaticFieldSet(_, _, v, _, _) ->
            // A static field is an `Unknown`-region global, so there is no cell to
            // bound; walk the stored value only for its capture edges.
            inferRegion s ctx v |> ignore
            RegionId.Unknown
        // `:>` / `:?>` adjust the static type of the same runtime value, so the result
        // rides the source's region. `:?` yields a bool, but walking the source
        // registers any inner captures.
        | TExpr.Upcast(src, ty, _) ->
            let r = inferRegion s ctx src

            if isHeapReprTarget ctx.Store ty then
                s.Graph.MarkHeapSink r

            r
        | TExpr.Downcast(src, _, _) -> inferRegion s ctx src
        | TExpr.TypeTest(src, _, _, _) ->
            inferRegion s ctx src |> ignore
            RegionId.Unknown
        | TExpr.Sequential(items, _, _) ->
            // Evaluate every item for side-effects (capture edges). The
            // sequence's region is the LAST item — intermediates don't escape.
            let n = items.Length

            if n = 0 then
                RegionId.Unknown
            else
                for i = 0 to n - 2 do
                    inferRegion s ctx items.[i] |> ignore

                inferRegion s ctx items.[n - 1]
        | TExpr.While(c, b, _, _) ->
            inferRegion s ctx c |> ignore
            inferRegion s ctx b |> ignore
            RegionId.Unknown
        | TExpr.ForTo(_, _, st, en, b, _, _) ->
            inferRegion s ctx st |> ignore
            inferRegion s ctx en |> ignore
            inferRegion s ctx b |> ignore
            RegionId.Unknown
        | TExpr.ForIn(_, src, b, _, _, _) ->
            inferRegion s ctx src |> ignore
            inferRegion s ctx b |> ignore
            RegionId.Unknown
        | TExpr.Range(a, step, b, _, _) ->
            inferRegion s ctx a |> ignore
            step |> Option.iter (fun st -> inferRegion s ctx st |> ignore)
            inferRegion s ctx b |> ignore
            RegionId.Unknown
        | TExpr.Format(sink, segs, _, _) ->
            (match sink with
             | FormatSink.ToWriter(w, _)
             | FormatSink.ToBuilder w -> inferRegion s ctx w |> ignore
             | FormatSink.ToStdOut _
             | FormatSink.ToStdErr _
             | FormatSink.ToString -> ())

            for seg in segs do
                match seg with
                | FormatSeg.Lit _ -> ()
                | FormatSeg.Hole(_, a) -> inferRegion s ctx a |> ignore
                | FormatSeg.DynHole d ->
                    d.Width |> ValueOption.iter (fun w -> inferRegion s ctx w |> ignore)
                    d.Precision |> ValueOption.iter (fun pr -> inferRegion s ctx pr |> ignore)
                    inferRegion s ctx d.Value |> ignore
                | FormatSeg.CallbackHole(_, residue) -> inferRegion s ctx residue |> ignore

            RegionId.Unknown
        | TExpr.ILIntrinsic(_, _, args, _, _) ->
            for a in args do
                inferRegion s ctx a |> ignore

            primitiveOrFreshResult ctx.Store s e
        // Resolved away by the inline-expansion pass; walk defensively in case a
        // residual one survives so any captures inside it still register.
        | TExpr.StaticOptimization(clauses, def, _, _) ->
            for c in clauses do
                inferRegion s ctx c.Body |> ignore

            inferRegion s ctx def
        | TExpr.Lambda(param, body, _, _) -> lambdaRegionWith s ctx (freshLambda s) param body
        | TExpr.Let _
        | TExpr.Use _ -> letChainRegion s ctx e
        | TExpr.IfThenElse(c, t, el, _, _) ->
            inferRegion s ctx c |> ignore
            joinArms ctx.Store s e [ inferRegion s ctx t; inferRegion s ctx el ] RegionId.Unknown
        | TExpr.Match(sc, armRules, _, _) ->
            inferRegion s ctx sc |> ignore
            joinArms ctx.Store s e [ for arm in armRules -> inferRegionArm s ctx arm ] RegionId.Unknown
        | TExpr.TryWith(b, armRules, _, _) ->
            let bodyR = inferRegion s ctx b
            joinArms ctx.Store s e [ yield bodyR; for arm in armRules -> inferRegionArm s ctx arm ] bodyR
        | TExpr.TryFinally(b, c, _, _) ->
            let bodyR = inferRegion s ctx b
            inferRegion s ctx c |> ignore
            bodyR
        | TExpr.App _ ->
            // Absent an effect signature, assume any callee returns its arguments or
            // values reachable through them: the result region outlives the callee and
            // every argument.
            let fn, args = TastWalk.collectAppChain [] e

            joinArms
                ctx.Store
                s
                e
                [ yield inferRegion s ctx fn; for (a, _, _) in args -> inferRegion s ctx a ]
                RegionId.Unknown
        | TExpr.MethodCall(recv, _, _, args, _, _) ->
            joinArms
                ctx.Store
                s
                e
                [ yield inferRegion s ctx recv; for a in args -> inferRegion s ctx a ]
                RegionId.Unknown
        | TExpr.StaticMethodCall(_, args, _, _) ->
            joinArms ctx.Store s e [ for a in args -> inferRegion s ctx a ] RegionId.Unknown
        // Resolved to a `StaticMethodCall` by inline expansion; walk args defensively
        // in case a residual one survives so captures inside it still register.
        | TExpr.TraitCall(_, _, args, _, _) ->
            joinArms ctx.Store s e [ for a in args -> inferRegion s ctx a ] RegionId.Unknown
        // The entry's body is a separate root shared by every call site, so walking it
        // here would mint one region per site for one body's allocations. Treated as
        // the opaque call it is — coarse in the same direction `App` is.
        | TExpr.InlineCall(args = args) ->
            joinArms ctx.Store s e [ for a in args -> inferRegion s ctx a ] RegionId.Unknown
        // Purely an anchor-domain marker: it allocates nothing and evaluates to its body,
        // so it rides the body's region exactly as a `Downcast` rides its source's.
        | TExpr.CallerExpr(body = body) -> inferRegion s ctx body

    /// Process a `TExpr.Lambda` whose closure region is `r` (fresh for an anonymous
    /// lambda, pre-minted for a function-form binding). Params must register AFTER
    /// `enterFun` to pick up the lambda's own frame depth as `MintFunctionLevel`.
    and private lambdaRegionWith (s: State) (ctx: PassContext) (r: RegionId) (param: TPat) (body: TExpr) : RegionId =
        addCaptureEdges s (TastWalk.freeVars (TastWalk.boundVarsOfTPat param) body) r
        enterFun s
        registerParam s ctx param
        let bodyRegion = inferRegion s ctx body
        exitFun s
        s.Graph.AddEdge(r, bodyRegion)
        r

    and private registerParam (s: State) (ctx: PassContext) (p: TPat) : unit =
        // ONE region per parameter pattern, shared by every bound variable in it: for
        // `(a, b)` that over-approximates safely — if any escapes, so do its siblings.
        match TastWalk.boundVarsOfTPat p with
        | [] -> ()
        | _ -> recordBindingRegion s ctx p (freshParam s)

    /// A `match` / `try-with` arm: register its pattern bound variables, walk the guard
    /// for capture edges, and return the body's region. Shared by both joiners.
    and private inferRegionArm (s: State) (ctx: PassContext) (arm: TMatchArm) : RegionId =
        registerParam s ctx arm.Pat
        arm.Guard |> ValueOption.iter (fun g -> inferRegion s ctx g |> ignore)
        inferRegion s ctx arm.Body

    /// Run a binding group: bump the let-level, pre-mint a closure region for every
    /// function-form bound variable so mutual references (let-rec / `and`) resolve before any
    /// body walk. A plain binding's region IS its RHS's, so it cannot be pre-minted.
    and private withBindingGroup
        (s: State)
        (ctx: PassContext)
        (bindings: (TPat * TExpr) seq)
        (body: unit -> RegionId)
        : RegionId =
        let savedEnclosing = s.EnclosingLet
        s.EnclosingLet <- s.LetLevel
        s.LetLevel <- s.LetLevel + 1

        for (p, v) in bindings do
            match v with
            | TExpr.Lambda _ -> recordBindingRegion s ctx p (freshLambda s)
            | _ -> ()

        for (p, v) in bindings do
            processBinding s ctx p v

        let r = body ()

        s.LetLevel <- s.LetLevel - 1
        s.EnclosingLet <- savedEnclosing
        r

    /// Walk a maximal chain of nested `Let`/`Use` as one binding group: `let rec … and …`
    /// arrives flattened into nested lets, so siblings only resolve once the whole
    /// chain is collected and pre-minted together.
    and private letChainRegion (s: State) (ctx: PassContext) (e: TExpr) : RegionId =
        let bindings = ResizeArray<TPat * TExpr>()

        let rec collect (e: TExpr) : TExpr =
            match e with
            | TExpr.Let(p, v, body, _, _) ->
                bindings.Add(p, v)
                collect body
            | TExpr.Use(p, v, body, _, _, _) ->
                bindings.Add(p, v)
                collect body
            | other -> other

        let body = collect e
        withBindingGroup s ctx bindings (fun () -> inferRegion s ctx body)

    and private processBinding (s: State) (ctx: PassContext) (p: TPat) (value: TExpr) : unit =
        match value with
        | TExpr.Lambda(param, lamBody, _, _) ->
            // Function-form binding: reuse the pre-minted closure region, or mint
            // on demand if a caller didn't pre-mint (keeps the pass total).
            let r =
                match p with
                | TPat.NamedSimple(k, _, _) when s.BindingRegions.ContainsKey k -> s.BindingRegions.[k]
                | _ ->
                    let r = freshLambda s
                    recordBindingRegion s ctx p r
                    r

            lambdaRegionWith s ctx r param lamBody |> ignore
        | _ ->
            // Plain binding: region(binding) = region(rhs). For pass-through
            // values (Var on RHS) this naturally shares the source's region.
            let rhsR = inferRegion s ctx value

            let isMutable =
                match p with
                | TPat.NamedSimple(k, _, _) ->
                    match ctx.Bindings.Binding.TryGetValue k with
                    | ValueSome rb -> rb.IsMutable
                    | ValueNone -> false
                | _ -> false

            if isMutable then
                // `let mutable x = rhs`: one cell holds many values over its lifetime
                // (`r <- (3, 4)`), so it gets its own region that outlives every value
                // stored into it, rather than sharing the initial rhs's.
                let cell = freshCell s
                s.Graph.AddEdge(rhsR, cell)
                recordBindingRegion s ctx p cell
            else
                recordBindingRegion s ctx p rhsR

    and private recordBindingRegion (s: State) (ctx: PassContext) (p: TPat) (r: RegionId) : unit =
        // Map every bound variable this pattern introduces to `r`; tuple / record / union
        // sub-patterns recurse so each name shares it. An approximation —
        // destructuring really projects each element separately.
        match p with
        | TPat.NamedSimple(k, _, _) ->
            s.BindingRegions.[k] <- r
            stampTyVar ctx k r
        | TPat.Tuple(items, _, _) ->
            for sub in items do
                recordBindingRegion s ctx sub r
        | TPat.Record(fields, _, _) ->
            for (_, sub) in fields do
                recordBindingRegion s ctx sub r
        | TPat.Union(_, fields, _, _) ->
            for sub in fields do
                recordBindingRegion s ctx sub r
        | TPat.TypeTestAs(_, inner, _, _) -> recordBindingRegion s ctx inner r
        // An or-pattern binds nothing (name resolution drops its bound variables).
        | TPat.Or _
        | TPat.Wildcard _
        | TPat.Null _
        | TPat.EnumCase _
        | TPat.Const _ -> ()

    /// Distinct lambda regions reachable from `start` via outlives edges — the input to
    /// the `HeapShared` seed rule. `visited` / `stack` are caller-owned scratch, cleared
    /// on entry and reused across the per-node calls.
    let private countReachableLambdas
        (g: RegionGraph)
        (visited: HashSet<int>)
        (stack: Stack<RegionId>)
        (start: RegionId)
        : int =
        visited.Clear()
        stack.Clear()
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
        | ReturnOnly, _
        | _, ReturnOnly -> ReturnOnly
        | LocalStack, LocalStack -> LocalStack

    let private solve (g: RegionGraph) : EscapeState[] =
        let n = g.Count
        let state = Array.create n LocalStack

        // Presized to the node count, which bounds both the reachable set and its worklist.
        let reachVisited = HashSet<int>(n)
        let reachStack = Stack<RegionId>(n)

        for i = 0 to n - 1 do
            let node = g.NodeOf(RegionId(i))

            // A lambda needs the STRICT inequality: it lives at its bind level, so
            // `let f x = … in f 3` inside another function doesn't escape. Other
            // allocations use `<=` — anything at frame level can be returned.
            if node.MintFunctionLevel > 0 then
                let escapes =
                    if node.IsLambda then
                        node.Level < node.MintFunctionLevel
                    else
                        node.Level <= node.MintFunctionLevel

                if escapes then
                    state.[i] <- lub state.[i] CallerStack

            // Reachable through ≥ N distinct lambdas → `HeapShared`, N = 1 for a
            // mutable cell (any closure capture of a mutable forces heap allocation)
            // and 2 otherwise. Skips lambdas so indirect self-capture doesn't promote.
            if not node.IsLambda then
                let reach = countReachableLambdas g reachVisited reachStack (RegionId(i))
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

    /// A second fixpoint over the SAME `Outlives` edges as `solve`, with a different
    /// seed set: a region requires a heap representation if `escape` (indexed by
    /// `RegionId.Raw`) says `HeapShared`, or it is a `HeapReprSink`, or one holds it.
    let private solveRepr (g: RegionGraph) (escape: EscapeState[]) : RegionRepr[] =
        let n = g.Count
        let heap = Array.zeroCreate<bool> n

        for i = 0 to n - 1 do
            let node = g.NodeOf(RegionId(i))

            if node.HeapReprSink || escape.[i] = HeapShared then
                heap.[i] <- true

        let mutable changed = true

        while changed do
            changed <- false

            for i = 0 to n - 1 do
                if heap.[i] then
                    let node = g.NodeOf(RegionId(i))

                    for tgt in node.Outlives do
                        if not heap.[tgt.Raw] then
                            heap.[tgt.Raw] <- true
                            changed <- true

        Array.init
            n
            (fun i ->
                if heap.[i] then
                    RegionRepr.RequiresHeapRepr
                else
                    RegionRepr.StackOnlyEligible
            )

    /// `specializations` is the file's resolved-inline table. It must be walked: a caller
    /// local captured by a lambda fused into an inline body is a capture of THIS file's
    /// binding, and leaving it unseen is a `let mutable` left unpromoted.
    let run (ctx: PassContext) (decls: EqArray<TDecl>) (specializations: EqArray<TSpecialization>) : unit =
        let s: State =
            {
                Graph = RegionGraph()
                BindingRegions = Dictionary<NodeKey, RegionId>(HashIdentity.Structural)
                LetLevel = 0
                EnclosingLet = 0
                FunctionStack = ResizeArray()
            }

        // Module-level decls form ONE binding group: `let rec a … and b …` arrive as
        // distinct `TDecl`s, so grouping them (conservatively) is what keeps mutual
        // references resolvable. The group bumps module function bodies to frame depth 1.
        let bindings =
            [
                for d in decls do
                    match d with
                    | TDecl.Let(p, v, _, _) -> yield (p, v)
                    | TDecl.Expression _
                    | TDecl.Type _ -> ()
            ]

        withBindingGroup
            s
            ctx
            bindings
            (fun () ->
                for d in decls do
                    match d with
                    | TDecl.Expression(e, _) -> inferRegion s ctx e |> ignore
                    // Type-member bodies aren't region-analysed; lets are handled above.
                    | TDecl.Let _
                    | TDecl.Type _ -> ()

                RegionId.Unknown
            )
        |> ignore

        // Each entry as its OWN group: folding entries into `decls` would give their
        // bindings the module's group and depth. `BindingRegions` outlives that group,
        // so an entry capturing a module binding still resolves it.
        for i = 0 to specializations.Length - 1 do
            let binding = TSpecializationG.binding (SpecializationId i) specializations.[i]
            withBindingGroup s ctx [ binding ] (fun () -> RegionId.Unknown) |> ignore

        let state = solve s.Graph
        let repr = solveRepr s.Graph state

        for kv in ctx.Bindings.TypeVar.AsDictionary() do
            let tv = UnionFind.find ctx.Store kv.Value

            if (ctx.Store.Region tv.Id).Raw >= 0 && (ctx.Store.Region tv.Id).Raw < state.Length then
                ctx.Bindings.Escape.Set(kv.Key, state.[(ctx.Store.Region tv.Id).Raw])
                ctx.Bindings.Repr.Set(kv.Key, repr.[(ctx.Store.Region tv.Id).Raw])

    /// One verdict per bound variable in `decls`, after `run` has filled both side tables: `Stack`
    /// iff frame-confined (`LocalStack`) AND free of any heap-repr channel. `decls` only —
    /// emit-time expansion re-mints bound variables, so an entry's own bound variable is unlookupable.
    let closureReprSnapshot (ctx: PassContext) (decls: EqArray<TDecl>) : Map<BoundVarKey, ClosureRepr> =
        Map.ofSeq (
            seq {
                for boundVar in TastWalk.declBoundVars decls do
                    let key = BoundVarKey.identity boundVar

                    match ctx.Bindings.Escape.TryGetValue key with
                    | ValueNone -> ()
                    | ValueSome escape ->
                        let stackEligible =
                            escape = LocalStack
                            && (
                                match ctx.Bindings.Repr.TryGetValue key with
                                | ValueSome RegionRepr.StackOnlyEligible -> true
                                | _ -> false
                            )

                        yield
                            boundVar,
                            (if stackEligible then
                                 ClosureRepr.Stack
                             else
                                 ClosureRepr.Heap)
            }
        )
