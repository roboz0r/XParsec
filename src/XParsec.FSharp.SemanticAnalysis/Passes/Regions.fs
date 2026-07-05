namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

// Pre:  Freeze has produced a `TastFile` (so the inline-expansion pass has run);
//       ctx.Bindings.Binding, ctx.Bindings.TypeVar populated.
// Post: ctx.Bindings.Escape populated for every binding-site TypeVar that
//       participated in the region graph; TypeVar.Region set on those TypeVars.
//
// Regions runs on the post-inline `TExpr` tree (after
// `Freeze.run`, before `RefCellPromotion`) rather than the Desugared CST.
// Inlining both removes closures (escape shrinks) and exposes new ones, so the
// escape map must be computed on the tree codegen actually emits. The walk reads
// each node's inline `.ty` and resolves a `TExpr.Var` to its binding region off
// the carried binding-site `NodeKey`. Running post-freeze is escape-equivalent:
// the only type-directed decision is `isAllocation`, and a typar is
// non-allocating whether it shows as `TyVar` (pre-freeze) or `TyTypar` (post).
//
// Two structural facts the CST pass relied on are rebuilt here:
//   - `let … and …` / `let rec` flatten into nested `TExpr.Let`s (and separate
//     top-level `TDecl`s). Every function-form binder in a group is pre-minted
//     before any body walk so mutual references resolve. Over-grouping sequential
//     lets is conservative (a non-`rec` forward reference can't exist, so the
//     extra pre-mint is unreachable).
//   - `let f x = e` is `let f = fun x -> e`, so the binding's closure region is
//     the `TExpr.Lambda` value's region; the pre-minted region is reused as the
//     lambda's own.
//
// Regions are inequality-only (NOT used to drive type-class dispatch); feeding
// them back into Unification would make the pipeline a fixpoint. See
// docs/architecture.md "Pass order is strictly forward".

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
            /// requires Rc<RefCell<…>>).
            IsMutableCell: bool
            /// Force-seed: the conservative fallback uses it to mark unhandled
            /// constructs HeapShared without the level / lambda-count heuristics.
            InitialState: EscapeState voption
            /// Axis-2 seed: this region is itself a
            /// heap-repr *sink* — a non-`ref struct` aggregate container (tuple /
            /// record / union / `new`) or the source of a box / interface upcast.
            /// The representation fixpoint flows `RequiresHeapRepr` DOWN this
            /// node's `Outlives` edges, pinning everything it transitively holds.
            mutable HeapReprSink: bool
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
                    HeapReprSink = false
                    Outlives = ResizeArray()
                }
            )

            id

        member _.AddEdge(longer: RegionId, shorter: RegionId) : unit =
            if longer.Raw < 0 || shorter.Raw < 0 then ()
            elif longer.Raw = shorter.Raw then ()
            else nodes.[longer.Raw].Outlives.Add(shorter)

        /// Seed `id` as an Axis-2 heap-repr sink (no-op for `RegionId.Unknown`).
        member _.MarkHeapSink(id: RegionId) : unit =
            if id.Raw >= 0 then
                nodes.[id.Raw].HeapReprSink <- true

        member _.NodeOf(id: RegionId) : RegionNode = nodes.[id.Raw]
        member _.Count = nodes.Count

    type private State =
        {
            Graph: RegionGraph
            /// binder NodeKey -> the binding's region. Lets the `Var` rule
            /// look up "the region of the binding I refer to" straight off the
            /// node's carried binding-site key.
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

    // Region minting helpers. `level`/`mintFn` always come from State, so they
    // are folded in here; each variant names the kind of region being minted
    // instead of forcing the reader to diff a wall of named arguments.

    let private freshValue (s: State) : RegionId =
        s.Graph.Fresh(s.EnclosingLet, functionStackTop s, false, false, ValueNone)

    let private freshLambda (s: State) : RegionId =
        s.Graph.Fresh(s.EnclosingLet, functionStackTop s, true, false, ValueNone)

    let private freshCell (s: State) : RegionId =
        s.Graph.Fresh(s.EnclosingLet, functionStackTop s, false, true, ValueNone)

    /// Parameter regions live in the callee frame, one level below the binding's
    /// RHS — hence `LetLevel`, not `EnclosingLet`. The ONLY mint that uses
    /// `LetLevel`; do not fold it into `freshValue`.
    let private freshParam (s: State) : RegionId =
        s.Graph.Fresh(s.LetLevel, functionStackTop s, false, false, ValueNone)

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
        | TyConst(name, _) ->
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
        // An anonymous union erases to a boxed reference (`obj`+`isinst`), so a
        // value flowing into one allocates — track it like the other composites.
        | TyOr _ -> true
        // A carried type-level computation erases like a union / `obj` (a boxed
        // reference) once evaluated, so track a value flowing into one as allocating —
        // external-vocabulary only, so this is defensive (it should be evaluated first).
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> true
        | TyVar _ -> false
        // Unresolved contract head: errors before it can reach a region
        // walk; treat as non-allocating so this pass stays conservative.
        | TyUnknown _ -> false
        // A post-freeze open typar (`!i` / `!!i`): like a free `TyVar`, whether
        // it allocates is unknown — treat as non-allocating, matching the
        // pre-freeze `TyVar` view this pass used to see.
        | TyTypar _ -> false
        // An enum is a value type (numeric → `System.Enum`; string / mixed →
        // a `[<Struct>]` wrapper) — it does not heap-allocate.
        | TyEnum _ -> false
        // A literal erases to its base primitive (`string`/`int`), whose allocation
        // status is the primitive's — both non-allocating here (interned string /
        // scalar). It is external-vocabulary only, so this is defensive.
        | TyLiteral _ -> false

    let private exprIsAllocation (e: TExpr) : bool = isAllocation (TastWalk.exprTy e)

    /// Is an `Upcast` to `t` a heap-repr sink (Axis-2)?
    /// `obj` boxes (`TyConst("obj", _)` — what `translateType` produces, see
    /// `RuntimeNames`), and the `Vesper.Fun<_,_>` interface upcast (`TyFun`)
    /// materialises a reference-typed function value. Either pins the upcast
    /// source to a heap representation. (`Downcast` narrows the static type of
    /// an existing value and is not a sink.)
    let private isHeapReprTarget (t: SemType) : bool =
        match resolveLink t with
        | TyConst("obj", _) -> true
        | TyFun _ -> true
        | _ -> false

    /// Mint a value region that outlives every child region. Tuples, records,
    /// `new`, and clones all allocate a composite that holds its elements.
    /// Every such composite is a non-`ref struct` aggregate (a `ValueTuple`
    /// cannot carry a ref-struct field either), so the region is an Axis-2
    /// heap-repr sink: a held closure is pinned to a heap representation even
    /// when it is frame-local by lifetime.
    let private holds (s: State) (children: RegionId seq) : RegionId =
        let r = freshValue s
        s.Graph.MarkHeapSink r

        for c in children do
            s.Graph.AddEdge(r, c)

        r

    /// Shared tail of the branch-joining nodes (if / match / try-with / app):
    /// if `e` allocates, mint a value region that outlives every arm region;
    /// otherwise return `fallback`. The `arms.Count > 0` guard is load-bearing
    /// only for `match` (a match with no rules allocates nothing) and harmless
    /// elsewhere.
    let private joinArms (s: State) (e: TExpr) (arms: RegionId seq) (fallback: RegionId) : RegionId =
        let arms = ResizeArray(arms)

        if exprIsAllocation e && arms.Count > 0 then
            let r = freshValue s

            for a in arms do
                s.Graph.AddEdge(r, a)

            r
        else
            fallback

    let private primitiveOrFreshResult (s: State) (e: TExpr) : RegionId =
        if exprIsAllocation e then
            freshValue s
        else
            RegionId.Unknown

    /// Add an outlives edge from each captured binding's region to the closure
    /// region `r`. (AddEdge drops self-edges, so no `captured <> r` guard needed.)
    //
    // TODO(byref-capture half): this is where a surviving
    // closure's captures are known. Once a byref-like predicate exists, a capture
    // whose binding type is byref-like (`Span`/`ref struct`) combined with this
    // closure's solved escape (`HeapShared`) is the reject site — match F# and
    // error. A non-escaping such capture could instead be made to compile via a
    // ref-struct closure ABI, so a program F# rejects
    // outright could compile here. Both need the predicate we do not have yet.
    let private addCaptureEdges (s: State) (freeVars: HashSet<NodeKey>) (r: RegionId) : unit =
        for bs in freeVars do
            match s.BindingRegions.TryGetValue bs with
            | true, captured -> s.Graph.AddEdge(captured, r)
            | _ -> ()

    /// Every binder-site NodeKey introduced by a `TPat`. A `TExpr.Var` carries
    /// the binding-site key directly, so a free variable is simply a `Var` whose
    /// key is not in scope — no `ctx.Bindings.Binding` resolution needed.
    let rec private bindersOfTPat (p: TPat) : NodeKey list =
        match p with
        | TPat.NamedSimple(k, _, _) -> [ k ]
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

    /// Free variables of a lambda body: every `TExpr.Var` whose binding site is
    /// neither a parameter nor introduced by an inner scope. `bound` is seeded
    /// with the lambda's own parameter binders and grown/shrunk as the walk
    /// enters/leaves any scope-introducing node (nested lambda, let/use, for,
    /// match arm).
    let private collectFreeVars (paramBinders: NodeKey list) (body: TExpr) : HashSet<NodeKey> =
        let result = HashSet<NodeKey>(HashIdentity.Structural)
        let bound = HashSet<NodeKey>(HashIdentity.Structural)

        for k in paramBinders do
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

        let iter: TastWalk.Iter =
            { TastWalk.identityIter with
                VisitExpr =
                    fun it e ->
                        match e with
                        | TExpr.Var(k, _, _) ->
                            if not (bound.Contains k) then
                                result.Add k |> ignore

                            false
                        | TExpr.Lambda(p, b, _, _) ->
                            let added = addBinders p
                            TastWalk.iterExpr it b
                            removeBinders added
                            false
                        | TExpr.Let(p, v, b, _, _) ->
                            TastWalk.iterExpr it v
                            let added = addBinders p
                            TastWalk.iterExpr it b
                            removeBinders added
                            false
                        | TExpr.Use(p, v, b, _, _, _) ->
                            TastWalk.iterExpr it v
                            let added = addBinders p
                            TastWalk.iterExpr it b
                            removeBinders added
                            false
                        | TExpr.ForTo(k, st, en, b, _, _) ->
                            TastWalk.iterExpr it st
                            TastWalk.iterExpr it en
                            let isNew = bound.Add k
                            TastWalk.iterExpr it b

                            if isNew then
                                bound.Remove k |> ignore

                            false
                        | TExpr.ForIn(p, src, b, _, _, _) ->
                            TastWalk.iterExpr it src
                            let added = addBinders p
                            TastWalk.iterExpr it b
                            removeBinders added
                            false
                        | _ -> true
                VisitArm =
                    fun it arm ->
                        let added = addBinders arm.Pat
                        arm.Guard |> Option.iter (TastWalk.iterExpr it)
                        TastWalk.iterExpr it arm.Body
                        removeBinders added
                        false
            }

        TastWalk.iterExpr iter body
        result

    let private stampTyVar (ctx: PassContext) (key: NodeKey) (r: RegionId) : unit =
        if r.Raw >= 0 then
            match ctx.Bindings.TypeVar.TryGetValue key with
            | ValueSome tv -> (UnionFind.find tv).Region <- r
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
        | TExpr.New(_, args, _, _)
        | TExpr.UnionCons(_, args, _, _) -> holds s [ for a in args -> inferRegion s ctx a ]
        // Field / property reads produce no new allocation — the access rides
        // the receiver's region (the field's own storage is tracked via the
        // receiver). Walk the receiver so its capture edges still register.
        | TExpr.FieldGet(r, _, _, _) -> inferRegion s ctx r
        | TExpr.PropertyGet(r, _, _, _, _) -> inferRegion s ctx r
        | TExpr.ExternalMember(rOpt, _, _, _, _, _) ->
            match rOpt with
            | ValueSome r -> inferRegion s ctx r
            | ValueNone -> RegionId.Unknown
        | TExpr.Assignment(l, r, _, _) ->
            // `lhs <- rhs`: the stored value must escape at least as wide as the
            // cell. Edge runs rhs → cell so propagation pushes the cell's state
            // BACK onto every value stored into it. AddEdge short-circuits on
            // RegionId.Unknown, so non-Ident LHSes need no special case.
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
        // `:>` / `:?>` are static-type adjustments over the same runtime value —
        // non-allocating, so the result rides the source's region. `:?` produces
        // a bool (Unknown), but walking the source registers any inner captures.
        | TExpr.Upcast(src, ty, _) ->
            // The upcast rides the source's region, but boxing to `obj` / upcasting
            // to the `Vesper.Fun<_,_>` interface materialises a heap value — seed
            // the source region as an Axis-2 heap-repr sink.
            let r = inferRegion s ctx src

            if isHeapReprTarget ty then
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
        | TExpr.ForTo(_, st, en, b, _, _) ->
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
                | FormatSeg.CallbackHole(_, callback, value) ->
                    inferRegion s ctx callback |> ignore
                    value |> ValueOption.iter (fun v -> inferRegion s ctx v |> ignore)

            RegionId.Unknown
        | TExpr.ILIntrinsic(_, _, args, _, _) ->
            for a in args do
                inferRegion s ctx a |> ignore

            primitiveOrFreshResult s e
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
            joinArms s e [ inferRegion s ctx t; inferRegion s ctx el ] RegionId.Unknown
        | TExpr.Match(sc, armRules, _, _) ->
            inferRegion s ctx sc |> ignore
            joinArms s e [ for arm in armRules -> inferRegionArm s ctx arm ] RegionId.Unknown
        | TExpr.TryWith(b, armRules, _, _) ->
            let bodyR = inferRegion s ctx b
            joinArms s e [ yield bodyR; for arm in armRules -> inferRegionArm s ctx arm ] bodyR
        | TExpr.TryFinally(b, c, _, _) ->
            let bodyR = inferRegion s ctx b
            inferRegion s ctx c |> ignore
            bodyR
        | TExpr.App _ ->
            // The result region (if any) outlives the callee and every argument.
            let head, args = TastWalk.collectSpine [] e
            joinArms s e [ yield inferRegion s ctx head; for (a, _, _) in args -> inferRegion s ctx a ] RegionId.Unknown
        | TExpr.MethodCall(recv, _, _, args, _, _) ->
            joinArms s e [ yield inferRegion s ctx recv; for a in args -> inferRegion s ctx a ] RegionId.Unknown
        | TExpr.StaticMethodCall(_, args, _, _) ->
            joinArms s e [ for a in args -> inferRegion s ctx a ] RegionId.Unknown
        // Resolved to a `StaticMethodCall` by inline expansion; walk args defensively
        // in case a residual one survives so captures inside it still register.
        | TExpr.TraitCall(_, _, args, _, _) -> joinArms s e [ for a in args -> inferRegion s ctx a ] RegionId.Unknown

    /// Process a `TExpr.Lambda` whose closure region is `r` (a fresh region for an
    /// anonymous lambda, or the pre-minted region of a function-form binding).
    /// Capture edges first (before any body recursion), then params register
    /// AFTER `enterFun` so they pick up the lambda's own frame depth as their
    /// `MintFunctionLevel` (the non-strict level rule then seeds an escaping
    /// parameter correctly). Finally the closure outlives its body's value.
    and private lambdaRegionWith (s: State) (ctx: PassContext) (r: RegionId) (param: TPat) (body: TExpr) : RegionId =
        addCaptureEdges s (collectFreeVars (bindersOfTPat param) body) r
        enterFun s
        registerParam s ctx param
        let bodyRegion = inferRegion s ctx body
        exitFun s
        s.Graph.AddEdge(r, bodyRegion)
        r

    and private registerParam (s: State) (ctx: PassContext) (p: TPat) : unit =
        // Mint ONE region per parameter pattern, threaded through every binder
        // via recordBindingRegion — same rule let-bindings use. Sharing a region
        // for `(a, b)` over-approximates safely ("if any escapes, treat siblings
        // as escaping"). Empty-binder patterns (Const / Wildcard) skip the mint.
        match bindersOfTPat p with
        | [] -> ()
        | _ -> recordBindingRegion s ctx p (freshParam s)

    /// A `match` / `try-with` arm: register its pattern binders, walk the guard
    /// for capture edges, and return the body's region. Shared by both joiners.
    and private inferRegionArm (s: State) (ctx: PassContext) (arm: TMatchArm) : RegionId =
        registerParam s ctx arm.Pat
        arm.Guard |> Option.iter (fun g -> inferRegion s ctx g |> ignore)
        inferRegion s ctx arm.Body

    /// Run a binding group: bump the let-level, pre-mint a closure region for
    /// every function-form binder so mutual references (let-rec / `and`) resolve
    /// before any body walk, process each binding, then evaluate `body` at the
    /// raised level before restoring. Plain bindings can't be pre-minted — their
    /// region IS the RHS's region. Shared by `letChainRegion` (nested `Let`/`Use`
    /// chains) and `run` (module-level decls as one group).
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

    /// Walk a maximal chain of nested `Let`/`Use` as one binding group — the
    /// flattening of `let rec … and …` into nested lets means siblings only
    /// resolve once the whole chain is collected and pre-minted together.
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
                // `let mutable x = rhs`: the cell is distinct from the rhs value.
                // The cell outlives every value stored into it; the rhs lubs up
                // to match if the cell is later classified wider. See
                // Mutable cells need a separate region from the initial rhs value.
                let cell = freshCell s
                s.Graph.AddEdge(rhsR, cell)
                recordBindingRegion s ctx p cell
            else
                recordBindingRegion s ctx p rhsR

    and private recordBindingRegion (s: State) (ctx: PassContext) (p: TPat) (r: RegionId) : unit =
        // Map every binder this pattern introduces to `r`. Tuple / record /
        // union sub-patterns recurse so each name shares the same region — a
        // rough approximation (destructuring projects each element), but
        // value-shape destructuring is rare in the v1 subset.
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
        | TPat.Wildcard _
        | TPat.Null _
        | TPat.EnumCase _
        | TPat.Const _ -> ()

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

    // Linear order `HeapShared > CallerStack > ReturnOnly > LocalStack`; lub
    // picks the wider (more-escaping) state. `ReturnOnly` slots between
    // `CallerStack` and `LocalStack` — additive, so every existing verdict is
    // unchanged.
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
                // ref-cell hoisting / Rust Rc<RefCell<_>>).
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

    /// Axis-2 representation fixpoint: a second
    /// forward pass over the SAME `Outlives` edges as `solve`, with a different
    /// seed/sink set. A region requires a heap representation if it escapes to
    /// the heap (Axis-1 `HeapShared`) or is itself a `HeapReprSink` — a
    /// non-`ref struct` aggregate container, or a box / interface-upcast source.
    /// The mark then flows DOWN every `Outlives` edge: a heap container pins
    /// everything it transitively holds into a heap representation too. `escape`
    /// is `solve`'s output, indexed by `RegionId.Raw`. Same loop shape as
    /// `solve`; defaults to `StackOnlyEligible` and only marks on reaching a sink.
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

    let run (ctx: PassContext) (decls: EqArray<TDecl>) : unit =
        let s: State =
            {
                Graph = RegionGraph()
                BindingRegions = Dictionary<NodeKey, RegionId>(HashIdentity.Structural)
                LetLevel = 0
                EnclosingLet = 0
                FunctionStack = ResizeArray()
            }

        // Module-level decls form ONE binding group: `let rec a … and b …` are
        // now distinct `TDecl`s, so grouping them (conservatively) is what keeps
        // mutual references resolvable. `withBindingGroup` bumps the level to 1,
        // so module function bodies start at frame depth 1 as the CST pass did.
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
                    // Type-member bodies aren't region-analysed (`RefCellPromotion`
                    // never rewrites a `TDecl.Type`), and lets are handled above.
                    | TDecl.Let _
                    | TDecl.Type _ -> ()

                RegionId.Unknown
            )
        |> ignore

        let state = solve s.Graph
        let repr = solveRepr s.Graph state

        for kv in ctx.Bindings.TypeVar.AsDictionary() do
            let tv = UnionFind.find kv.Value

            if tv.Region.Raw >= 0 && tv.Region.Raw < state.Length then
                ctx.Bindings.Escape.Set(kv.Key, state.[tv.Region.Raw])
                ctx.Bindings.Repr.Set(kv.Key, repr.[tv.Region.Raw])

    /// Fold the codegen stack/heap verdict for every binder: `ClosureRepr.Stack` iff the
    /// binder is both frame-confined by lifetime (`Axis 1` `EscapeState.LocalStack`)
    /// and free of any heap-repr channel (`Axis 2` `RegionRepr.StackOnlyEligible`);
    /// everything else is `Heap`. Keyed by `NodeKey.Raw`. The map covers all
    /// binders, not only closures — codegen's `discoverClosures` only ever looks up
    /// closure `Closure.SelfKey`s, so non-closure entries are inert. Must run after
    /// `run` has populated both side tables; the Pipeline snapshots the result onto
    /// `TastFile.ClosureReprs`.
    let closureReprSnapshot (ctx: PassContext) : Map<NodeKey, ClosureRepr> =
        ctx.Bindings.Escape.AsDictionary()
        |> Seq.map (fun kv ->
            let stackEligible =
                kv.Value = LocalStack
                && (
                    match ctx.Bindings.Repr.TryGetValue kv.Key with
                    | ValueSome RegionRepr.StackOnlyEligible -> true
                    | _ -> false
                )

            kv.Key,
            (if stackEligible then
                 ClosureRepr.Stack
             else
                 ClosureRepr.Heap)
        )
        |> Map.ofSeq
