# Regions plan

The build plan for **region / escape analysis** in the semantic-analysis
pipeline — Phase 4.4 of [`semantic-analysis.md`](../../../semantic-analysis.md),
the third axis of the [`TypeVar`](typevar.md). Different shape from
[`generalisation-plan.md`](generalisation-plan.md) and
[`measures-plan.md`](measures-plan.md): the type/measure plans extended an
existing pass, this one **wakes up an existing no-op pass** that already
has its slot in the pipeline.

**Status (2026-05-17):** Implemented in
[`Passes/Regions.fs`](../Passes/Regions.fs); 11 tests in
[`RegionsTests.fs`](../../../test/XParsec.FSharp.SemanticAnalysis.Tests/RegionsTests.fs)
covering the headline cases below. Sections marked "Deviation" record
where the implementation diverged from the original sketch and why.

The canonical examples we want to classify after this lands:

```fsharp
// LocalStack: closure captures nothing that escapes.
let useLocal () =
    let f x = x + 1
    f 3

// CallerStack: closure escapes one level — returned from the function.
let mkAdder n =
    fun x -> x + n          // captures n, escapes to caller

// HeapShared: closure escapes through a longer-lived structure.
// (Tiny subset doesn't have ref/mutable yet — this lands later. v1
// classifies anything captured by a deeply-escaping closure as
// HeapShared.)
let mkPair n =
    let f = fun x -> x + n
    f, f                    // returned tuple = two refs to same closure
```

## Goal

After `Regions` finishes, every `TypeVar` whose underlying value
represents an **allocation** (closure, tuple, anything heap-shaped)
carries an [`EscapeState`](../SemanticInfo.fs) in `ctx.Escape`, keyed by
the expression's `NodeKey`. The classification is conservative: when in
doubt, mark wider (`HeapShared` ≥ `CallerStack` ≥ `LocalStack`).

Concretely:

- A bare value (`let x = 1`, `let p = (1, 2)`) is `LocalStack` — it
  doesn't outlive its enclosing scope.
- A value returned from a function (a closure that captures locals, a
  tuple constructed and yielded) is `CallerStack`.
- A value captured by another value that's itself `CallerStack` or
  beyond is at least `CallerStack`. Recursively widened by least-upper-
  bound on the region graph.
- A value reachable through a "heap" sink (a recursive data structure,
  a mutable cell once those land, an async closure once those land) is
  `HeapShared`. For v1 the only `HeapShared` source is "captured by a
  closure that escapes the function via more than one indirection."

Dimensionless / non-allocating values (literals, `int`, primitives) have
no entry in `ctx.Escape` — the absence is meaningful: target lowering
treats them as register/inline values, not heap-tracked allocations.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `RegionId` struct                      | `SemanticInfo.fs:8`                             | Done — sequential `int` with `Unknown = -1`. |
| `EscapeState` DU                       | `SemanticInfo.fs:82`                            | Done — `LocalStack | CallerStack | HeapShared`. |
| `TypeVar.Region`                       | `SemanticInfo.fs:174`                           | Written by Regions on every TyVar whose expression allocates. |
| `ctx.Escape` side table                | `SideTables.fs:53`                              | Populated by Regions for every allocating expression. |
| `Regions.run`                          | `Passes/Regions.fs`                             | Full constraint generator + solver. |
| Pipeline slot                          | `Pipeline.fs`                                   | `Regions.run` invoked between `Unification` and `Validation`. |
| `TypeVar.Level` (let-depth)            | `SemanticInfo.fs:186`                           | Owned by Unification; Regions doesn't read it (uses its own walk-local let-level instead so the pass is self-contained). |
| `ctx.Binding` (resolved binding sites) | `Passes/NameResolution.fs`                      | Done — captures = free variables = bindings whose site is **above** the lambda's level. |

The implementation hangs every piece below off `Passes/Regions.fs`-local
types — `RegionId` and `EscapeState` are already the right shape, no
public-surface changes needed. The pass adds an internal `RegionGraph`
value used during the walk and discarded before `Freeze`.

One ancillary change in the rest of the project:

- [`Pipeline.analyseWithContext`](../Pipeline.fs) returns
  `PassContext * TastFile` so tests can read `ctx.Escape`. The
  original `Pipeline.analyse` is now a thin wrapper that discards the
  context.

## The algorithm: inequality propagation on a region graph

Regions are not a union-find problem — they're a **partial order**.
"`R_A >= R_B`" means region A's scope contains region B's scope, i.e.
A outlives B. The graph captures these constraints; the solver computes
each region's escape state as the least upper bound of the regions it
reaches via outgoing edges.

The mechanics:

- **Each allocating expression gets a fresh `RegionId`.** Literals,
  primitives, `int`/`bool`/`float` arithmetic results don't allocate —
  they get `RegionId.Unknown` and are skipped. "Allocating" is decided
  by checking the expression's zonked TyVar against
  [`isAllocation`](../Passes/Regions.fs) (TyFun, TyTuple, or any
  non-primitive TyConst). **Deviation:** the plan originally minted a
  fresh region for every App / IfThenElse / Match unconditionally; in
  practice that flooded `ctx.Escape` with entries for primitive results
  (e.g. `f 3` returning `int`) and caused over-propagation through
  `let useLocal () = let f x = x + 1 in f 3` (useLocal got lifted to
  `CallerStack` purely because the app result region carried a seed).
  Gating on the inferred type keeps `ctx.Escape` honest: an entry means
  the value is an allocation worth tracking.
- **Each binding site shares its RHS region.** Plain `let x = rhs` does
  `region(x) := region(rhs)` rather than minting a fresh "scope region"
  and adding an edge. This makes the Ident pass-through rule
  (`region(use) = region(bindingSite)`) collapse to identity, so the
  test `let r = let x = (1, 2) in let y = x in y` produces `x ≡ y ≡ r`
  on the same RegionId. Function-form bindings (`let f x = body`) are
  the exception — they mint a fresh closure region, see below.
  **Deviation:** the plan's `region(x) >= region(rhs)` edge made
  binding-site regions separate from value regions. Adopting identity
  for the plain case dropped a graph node per binding without losing
  classification accuracy, and made test 8's "shared region" assertion
  fall out for free.
- **Edges** are added per the rules in [§Constraint generation](#constraint-generation)
  below. Cycles do occur (recursive bindings: `let rec f x = f x`
  produces edges back into `region(f)`), but the fixpoint solver
  converges since the lattice is small.
- **Solving**: seed each region's state via the rules in [§How
  CallerStack and HeapShared are seeded](#how-callerstack-and-heapshared-are-seeded),
  then iterate to fixpoint. For every outgoing edge `R_A ≥ R_B`, lift A
  to at least B's state. **Deviation:** the plan called for Tarjan SCC
  with topological propagation; the implementation runs a flat fixpoint
  loop because the graphs in the v1 subset are tiny (tens of nodes per
  file) and Tarjan's setup cost outweighs the per-iteration savings.
  Swap it back in when files routinely produce thousands of regions.

Why not union-find: union-find merges equivalence classes, which is the
right structure for equality (Algorithm J) but wrong for inequality. Two
regions A and B with `A ≥ B` are NOT the same region — A is wider. If
we tried to merge them we'd lose the directionality and either
under-approximate (too few `HeapShared`) or over-approximate (everything
heap-shared, useless). FCS doesn't have a region pass to learn from
directly; the standard reference is Tofte & Talpin's region inference,
plus Rust's borrow checker — both run as inequality propagation, not
unification.

### Why inequality stays one-pass

The spec ([§3 Phase Pipeline](../../../semantic-analysis.md#3-phase-pipeline-overview))
and [`docs/architecture.md`](architecture.md#pass-order-is-strictly-forward)
both insist regions are **downstream of Unification and don't feed back**.
This is a load-bearing simplification — letting regions influence trait
selection (Rust-style lifetime-driven dispatch) would turn the whole
pipeline into a fixpoint. We accept that .NET's `ref struct` rules and
Rust's lifetime annotations might occasionally require manual hints to
avoid over-conservative `HeapShared` classification; the alternative is
weeks of fixpoint plumbing for a few percent of programs.

## Constraint generation

The walker mirrors `Unification.infer` — same CST traversal, but emits
edges instead of unifying types. Rules below are stated as "given this
CST node, add these edges to the graph." `region(e)` means the RegionId
allocated for expression `e`.

### Literals and primitives

```fsharp
| Expr.Const (Literal _) -> RegionId.Unknown   // no allocation
```

`int`, `float`, `bool`, `byte`, `int64`, `unit`, `string` literals don't
allocate in any sense we care about. Strings are interned constants;
treat as unknown.

### Identifiers

```fsharp
| Expr.Ident _ ->
    let bindingSite = ctx.Binding[key].BindingSite
    region(e) = region(bindingSite)   // same region as the binding
```

A use site of `x` doesn't introduce a new allocation — it's a reference
to the value bound elsewhere. The use's region IS the binding's region.

### Let-bindings

```fsharp
| Expr.LetOrUse (bindings, body) ->
    for b in bindings do
        if b.argumentPats.IsEmpty then
            // Plain binding — region(x) := region(rhs). Identity, no edge.
            recordBindingRegion x (region(b.expr))
        else
            // Function-form — treated as an implicit Fun; see below.
            ...
    // The let-expression's overall region is the body's.
    region(e) = region(body)
```

For `let x = rhs in body`: instead of minting a fresh `region(x)` and
adding `region(x) >= region(rhs)`, the implementation collapses them —
`region(x) := region(rhs)`. The plan's original sketch added a
separate binding-site region and a flow edge; in practice that doubles
the graph and the propagation result is identical for v1 (no
constructs distinguish "binding scope" from "value scope"). Identity
makes the Ident pass-through rule trivial and gives test 8's
shared-region property for free.

The let-expression as a whole evaluates to `body`, so it inherits
body's region.

### Lambdas (the interesting case)

```fsharp
| Expr.Fun (argPats, body) ->
    let r = freshLambdaRegion (level = enclosingLet, mintFn = functionStackTop)
    region(e) = r
    // Each captured variable must outlive the closure.
    for captured in freeVars body do
        addEdge (region(captured)) >= r
    enterFunction ()
    // Parameter regions live inside the lambda's own frame — register
    // them AFTER enterFunction so they pick up the new function-stack
    // top as their MintFunctionLevel.
    for p in argPats do
        registerParam p
    let bodyRegion = inferRegion body
    exitFunction ()
    // Body-escape edge: if the body returns an allocation, the closure
    // inherits its escape state.
    addEdge r >= bodyRegion
```

A lambda allocates a closure. The closure's region is fresh, stamped
at the **enclosing-let level** (the bind level of the closest
surrounding binding — pre-push value of let-level) and tagged
`IsLambda = true` so the seed rules can distinguish it from data
allocations. For every variable the body captures — every free
variable not in `argPats` — add an edge saying that variable's region
must outlive the closure. If the closure escapes upward, that pulls
all its captures along with it.

**Deviation:** the plan omitted the closure→body edge. Without it,
`let mkAdder n = fun x -> x + n` left `region(mkAdder)` stranded as
`LocalStack` — there was no path for "the lambda I evaluate to is
`CallerStack`" to reach the closure's own classification. Adding
`region(closure) >= region(body)` propagates the function's return
escape up to the closure (and through it to any enclosing binding
that holds the closure).

**Deviation (registerParam ordering):** parameter regions must be
minted inside `enterFunction () … exitFunction ()`, otherwise their
`MintFunctionLevel` snapshots the *outer* function's frame and the
non-strict level rule (`Level <= MintFunctionLevel`) misses them. For
example, `let mk = fun a -> a` at module level needs `a`'s
`MintFunctionLevel = 1` (the lambda's own frame); the closure→body
edge then propagates `a`'s `CallerStack` seed up to `mk`. An earlier
version of the implementation registered params before `enterFunction`
and left `mk` classified as `LocalStack`.

**Deviation (registerParam shares region across destructuring):**
`registerParam` mints ONE region for the whole parameter pattern and
threads it through every binder by delegating to
`recordBindingRegion`. For `(a, b)` the elements alias parts of the
same tuple value; for `x as y` both names alias the same value.
Sharing a region matches the let-binding destructuring rule (no
surprises across binding sites) and over-approximates safely.
Empty-binder patterns (`()`, `_`) skip the mint to avoid graph
litter.

`freeVars body` reads `ctx.Binding`: walk the body once, collect the
set of binding-site keys introduced **inside** the body (argPats of
this lambda + nested let headPats + nested lambda argPats + match-arm
patterns + for-loop binders), and any use whose `BindingSite` falls
outside that set is a free variable. Re-computed per lambda; the
v1-subset CSTs are small enough that the duplicate walk is invisible.

### Function-form bindings (deviation: handled directly)

The plan implicitly treats `let f x y = body` as desugared to
`let f = fun x y -> body` and routes everything through the Fun rule.
The implementation handles function-form bindings **without
desugaring** — there is no synthetic `Expr.Fun` node — but applies
the same edge rules:

```fsharp
// processBindingGroup pre-pass: mint and record every function-form
// binding's closure region BEFORE walking any RHS. Sibling references
// (mutual let-rec / `and` clauses) need the region present in
// BindingRegions before the first body walk runs.
for b in bindings do
    if not b.argumentPats.IsEmpty then
        let r = freshLambdaRegion (level = enclosingLet, mintFn = functionStackTop)
        recordBindingRegion b.headPat r

// In processBinding for `let f args = body` with args non-empty:
let r = BindingRegions[b.headPat]  // pre-minted above
for captured in freeVars body do
    addEdge (region(captured)) >= r
enterFunction ()
for p in argumentPats do
    registerParam p                // param binding regions at body-level
let bodyR = inferRegion body
addEdge r >= bodyR                 // closure→body escape propagation
exitFunction ()
```

Why not just synthesise the Fun: writing into a real CST node's
NodeKey requires a real CST node. Desugaring would mean either
inventing synthetic NodeKeys (which the rest of the pipeline doesn't
expect for bindings) or rewriting the CST, which Regions is supposed
to be read-only over. Direct handling keeps the binding's headPat as
the single key for `region(f)` everywhere downstream.

**Deviation (pre-record pass):** an earlier version of
`processBindingGroup` recorded each binding's region as it processed
that binding, with no pre-pass. That dropped capture edges in mutual
recursion: when `a` was processed first, `BindingRegions[b]` wasn't
yet set, so a's body's references to `b` resolved as
`RegionId.Unknown` and a's capture of b was dropped silently. For
example, `let rec a () = b and b () = (1, 2)` left `a` as
`LocalStack` even though it returns the tuple-returning `b`. The
pre-pass plants every function-form sibling's region in
`BindingRegions` up front, so both directions of any `let rec`-cycle
participate in the graph.

### Function application

```fsharp
| Expr.App (fn, args) ->
    let fnR = inferRegion fn
    let argRegions = args |> List.map inferRegion
    if exprIsAllocation e then
        let r = freshRegion (level = enclosingLet, mintFn = functionStackTop)
        region(e) = r
        addEdge r >= fnR
        for ar in argRegions do
            addEdge r >= ar
        r
    else
        RegionId.Unknown
```

Same shape as the plan's "fresh result region with edges to each
argument" — the result region must outlive each argument because we
assume any function might return its arguments or values reachable
through them. **Deviation:** the rule fires only when the application
returns an allocation type (TyFun / TyTuple / non-primitive TyConst).
Primitive returns (the common case in the v1 subset:
`int + int → int`, `f 3 → int`) skip the mint entirely and return
`RegionId.Unknown`. Without this gate, every `+` and `<` poured
spurious `CallerStack` seeds into the graph through the level rule and
lifted unrelated bindings.

When effect signatures land (much later), this rule splits into "pure
function" (no escape edges) vs "effectful function" (the current
behavior).

### Tuples

```fsharp
| Expr.Tuple items ->
    let r = freshRegion (level = enclosingLet, mintFn = functionStackTop)
    region(e) = r
    for it in items do
        addEdge r >= (region(it))   // tuple holds onto each element
```

A tuple is an allocation that holds references to each element. The
tuple's region must outlive each element's region. Always minted —
tuples are always allocations.

### Sequential, if-then-else, match

```fsharp
| Expr.Sequential (items) ->
    // Result is the last item's region; intermediate items don't escape.
    region(e) = region(items.Last)

| Expr.IfThenElse (cond, thenE, elseE) ->
    inferRegion cond |> ignore
    let armRegions = [ inferRegion thenE; inferRegion elseE ]
    if exprIsAllocation e then
        let r = freshRegion (level = enclosingLet, mintFn = functionStackTop)
        region(e) = r
        for ar in armRegions do
            addEdge r >= ar
        r
    else
        RegionId.Unknown

| Expr.Match (scrutinee, arms) ->
    // Same shape: only mint the join region when the match returns an
    // allocation type, else RegionId.Unknown.
```

Branching creates a join point. The result's region must outlive each
branch's region (the result IS one of the branches at runtime).

### Type annotation, parenthesised expressions

```fsharp
| Expr.TypeAnnotation (inner, _)
| Expr.EnclosedBlock (_, inner, _) ->
    region(e) = region(inner)   // pass-through
```

No allocation, no region change.

### While / For / Assignment

```fsharp
| Expr.While _ | Expr.ForTo _ | Expr.ForIn _ ->
    // Walk sub-expressions for capture-edge side effects.
    region(e) = RegionId.Unknown   // unit, no allocation

| Expr.Assignment (lhs, rhs) ->
    region(e) = RegionId.Unknown   // unit
```

Loops return unit. **Deviation, resolved 2026-05-17:** the original sketch
added an `addEdge (region(rhs)) >= (region(lhs))` flow edge on Assignment;
this was deferred for the original v1 because there was no mutable cell to
carry the flow into. With [`mutable-plan.md`](mutable-plan.md) landing,
the edge is now emitted — when LHS is an immutable binding or a
fallback-routed shape, the `lhsR` is `RegionId.Unknown` and `AddEdge`
short-circuits, so the edit is a pure addition.

(Mutability validation is `Validation`'s job — Regions just records the
flow.)

### What we explicitly DON'T handle yet

`Object`, `Record`, `RecordClone`, `ref`, `seq`/`async`
computation expressions — these are all out of subset scope and will
extend the constraint generator when they land. (`let mutable` landed
2026-05-17 — see [`mutable-plan.md`](mutable-plan.md).) The
"every-unknown-construct-is-HeapShared" fallback covers them safely in
the interim. See [§Conservative fallback](#conservative-fallback) below.

## Conservative fallback

For any CST node Regions doesn't have a precise rule for, the fallback
is: mint a fresh region at the **outermost** scope level (level 0),
force its `InitialState` to `HeapShared`, and add no edges. The seed
*has* to be forced because neither of the natural seed rules would
fire on a level-0, edgeless node — the level rule needs
`MintFunctionLevel > 0`, and the lambda-reach rule needs at least two
reachable lambdas. Forcing the seed gets the widest classification
regardless. This is wrong in the sense of "might over-allocate to
heap" but right in the sense of "never lets a stack-allocated value
escape its scope."

The fallback applies to `Object`, `Record`, `String` (interpolation
holes), and anything else not enumerated above. Each falls off the
conservative cliff as Regions grows precise rules for it.

## Data-model changes

Strictly local to `Passes/Regions.fs`. No changes to `SemanticInfo.fs`,
`SideTables.fs`, or `Pipeline.fs`'s public surface (a sibling
`Pipeline.analyseWithContext` was added for tests — see [§Test strategy](#test-strategy)).

```fsharp
module Regions =

    /// Region graph node. `Level` is the let-depth the region lives at
    /// (its lifetime upper bound). `MintFunctionLevel` is the let-depth
    /// of the innermost enclosing function-body at mint time; the seed
    /// rule `Level < MintFunctionLevel` (lambdas) / `Level <=
    /// MintFunctionLevel` (data) catches values that escape that
    /// function's frame.
    type private RegionNode =
        {
            Id: RegionId
            Level: int
            MintFunctionLevel: int
            IsLambda: bool
            /// Force-seed override. Used by the conservative fallback to
            /// mark unhandled constructs as HeapShared without relying on
            /// the level / lambda-count heuristics.
            InitialState: EscapeState voption
            mutable Outlives: ResizeArray<RegionId>
        }

    /// Per-PassContext graph state. Constructed fresh each `run`,
    /// discarded when the pass returns. Lives entirely in this module —
    /// `RegionId` is the only thing that leaks out (into TypeVar.Region
    /// and ctx.Escape).
    type private RegionGraph() =
        let nodes = ResizeArray<RegionNode>()

        member _.Fresh
            (level: int, mintFn: int, isLambda: bool, seed: EscapeState voption)
            : RegionId =
            let id = RegionId(nodes.Count)
            nodes.Add({ Id = id; Level = level; MintFunctionLevel = mintFn;
                        IsLambda = isLambda; InitialState = seed;
                        Outlives = ResizeArray() })
            id

        member _.AddEdge(longer: RegionId, shorter: RegionId) : unit =
            // longer ≥ shorter — `longer` must outlive `shorter`.
            // No-op when either side is RegionId.Unknown or when an
            // expression's region edges to itself (recursive bindings).
            ...

        member _.NodeOf(id: RegionId) : RegionNode = nodes.[id.Raw]
        member _.Count = nodes.Count
```

**Deviation:** the node has four fields the sketch didn't include —
`MintFunctionLevel`, `IsLambda`, and `InitialState` carry the metadata
the seed and fallback rules need; backing storage is a `ResizeArray`
indexed by `RegionId.Raw` (sequential ints) rather than a Dictionary
keyed on RegionId (a Struct — Dictionary works but adds a hash per
lookup the array doesn't need).

The walker also carries a small **mutable `State`** record threaded
through every helper:

```fsharp
type private State =
    {
        Graph: RegionGraph
        BindingRegions: Dictionary<NodeKey, RegionId>
        mutable LetLevel: int       // analogue of Unification's CurrentLevel
        mutable EnclosingLet: int   // let-level of the binding whose RHS
                                    //   we're currently evaluating; new
                                    //   allocations mint at this level
        FunctionStack: ResizeArray<int>   // pushed at function entry
                                          //   with the entry-time LetLevel;
                                          //   top = the innermost
                                          //   function's frame depth
    }
```

`EnclosingLet` is saved-and-restored at every binding-group enter/exit;
`FunctionStack` is pushed/popped at lambda body and function-form
binding body entry. Together they let the seed rule compare "where
does this region live" against "what function frame is it inside" —
the comparison the plan's original "Level on a stack" description was
gesturing at.

The constraint generator is a CST walker shaped like `Unification.infer`
— same DU dispatch, much smaller per-case body. It returns a
`RegionId` per expression and writes `TypeVar.Region` as it goes:

```fsharp
let rec private inferRegion (ctx: PassContext) (g: RegionGraph) (e: Expr<SyntaxToken>) : RegionId =
    let regionId =
        match e with
        | Expr.Const (Literal _) -> RegionId.Unknown
        | Expr.Ident _ -> regionOfBinding ctx g e
        | Expr.Fun (argPats, body) -> regionOfLambda ctx g argPats body
        | Expr.App (fn, args) -> regionOfApp ctx g fn args
        | Expr.Tuple items -> regionOfTuple ctx g items
        // ... rest of the cases ...
        | _ -> g.Fresh(level = 0)   // conservative fallback

    // Stamp the TyVar with its region for downstream consumers.
    match ctx.TypeVar.TryGetValue (CstKeys.ofExpr e) with
    | ValueSome tv -> (UnionFind.find tv).Region <- regionId
    | ValueNone -> ()

    regionId
```

The solver applies seeds, then runs a fixpoint relax over outgoing
edges:

```fsharp
let private solve (g: RegionGraph) : EscapeState[] =
    let n = g.Count
    let state = Array.create n LocalStack

    // Apply seeds (see "How CallerStack and HeapShared are seeded").
    for i = 0 to n - 1 do
        let node = g.NodeOf(RegionId(i))
        match node.InitialState with
        | ValueSome s -> state.[i] <- s          // conservative-fallback override
        | ValueNone ->
            if node.MintFunctionLevel > 0 then
                let escapes =
                    if node.IsLambda then node.Level < node.MintFunctionLevel
                    else node.Level <= node.MintFunctionLevel
                if escapes then state.[i] <- lub state.[i] CallerStack
            if not node.IsLambda then
                if countReachableLambdas g (RegionId(i)) >= 2 then
                    state.[i] <- HeapShared

    // Fixed-point relax: for every edge R_long >= R_short, lift R_long
    // to at least R_short's state.
    let mutable changed = true
    while changed do
        changed <- false
        for i = 0 to n - 1 do
            for tgt in (g.NodeOf(RegionId(i))).Outlives do
                let lifted = lub state.[i] state.[tgt.Raw]
                if lifted <> state.[i] then
                    state.[i] <- lifted
                    changed <- true

    state

let private lub (a: EscapeState) (b: EscapeState) : EscapeState =
    match a, b with
    | HeapShared, _ | _, HeapShared -> HeapShared
    | CallerStack, _ | _, CallerStack -> CallerStack
    | LocalStack, LocalStack -> LocalStack
```

**Deviation:** state is an `EscapeState[]` indexed by `RegionId.Raw`
rather than a `Dictionary<RegionId, EscapeState>`. RegionIds are
sequential ints handed out by the graph, so array indexing is the
natural lookup; the Dictionary in the original sketch was a Struct-key
hash hop with no benefit.

### How `CallerStack` and `HeapShared` are seeded

The solver above starts every region at `LocalStack` (or its
`InitialState` override — used only by the conservative fallback to
force `HeapShared`). Nothing lifts them. We need **seeds**: rules that
classify certain regions as `CallerStack` or `HeapShared` to start,
and then propagation widens their neighbours.

Two seed rules for v1, both gated on `MintFunctionLevel > 0` (i.e.,
the region was minted inside a function body — module-top mints have
no frame to escape):

1. **Level rule (CallerStack)**: a region constructed inside a function
   whose `Level` puts it at or above the function's frame depth seeds
   as `CallerStack`. The threshold differs by region kind:
   - **Lambdas** use a **strict** `Level < MintFunctionLevel`. A
     closure assigned to a same-frame local (`let f x = ... in f 3`
     inside another function) doesn't escape — it's used and discarded
     within the frame. Strict inequality excludes this case.
   - **Non-lambda allocations** (tuples, app results, if-join regions)
     use **non-strict** `Level <= MintFunctionLevel`. Anything
     constructed at the function's frame level can flow out as the
     return value, and there's no syntactic discriminator to tell "this
     tuple gets returned" from "this tuple is consumed locally" without
     tail-position analysis. Erring `<=` is the conservative call.

   **Deviation:** the original plan had a single uniform `Level <
   MintFunctionLevel` rule. That under-fired on test 4 (`let f () =
   (1, 2)`) and on `let f b = if b then (1, 2) else (3, 4)` — the
   tuples and if-join regions ended up at exactly the function's frame
   level and missed the strict-less threshold. The split keeps test 1
   (`f` LocalStack) while making the returned-tuple tests pass.

2. **Lambda-reach rule (HeapShared)**: a non-lambda region whose
   transitive `Outlives`-closure includes **≥ 2 distinct lambda
   regions** seeds as `HeapShared`. This is the "captured by a closure
   that's captured by another closure" pattern — the value can't live
   on any single stack frame.

   **Deviation:** the plan described this as "reachable through ≥ 2
   lambda boundaries." The implementation makes that concrete by
   walking outgoing edges with a DFS that counts distinct `IsLambda`
   targets reachable from the start node (the start itself isn't
   counted — currently the rule fires only on non-lambda starts, so
   the skip is just a defensive guard).

Both rules are heuristics. They over-approximate in some cases (a
`CallerStack` value passed sideways into a deeper lambda might get
marked `HeapShared` incorrectly) and the refinements needed are in
[§Open questions](#open-questions). The conservative direction is
safe: over-marking heap is correct-but-slow, under-marking is unsafe.

## Pipeline integration

`Regions.run` keeps its existing signature. The pass:

1. Allocate a fresh `RegionGraph`.
2. Walk every `ModuleElem` in order, calling `inferRegion` on its
   bindings and expressions.
3. Call `solve` to compute the final escape state per region.
4. For every expression key in `ctx.TypeVar`, look up the TyVar's
   `Region` and write `ctx.Escape[key] = state[region]` (skipping
   `RegionId.Unknown` — primitives have no escape entry).

```fsharp
let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
    let g = RegionGraph()
    walkFile ctx g file
    let state = solve g

    // Project per-expression escape state via TyVar.Region. `state` is
    // an EscapeState[] indexed by RegionId.Raw — sequential ints handed
    // out by the graph, so array lookup is the natural projection.
    for kv in ctx.TypeVar.AsDictionary() do
        let tv = UnionFind.find kv.Value
        if tv.Region.Raw >= 0 && tv.Region.Raw < state.Length then
            ctx.Escape.Set(kv.Key, state.[tv.Region.Raw])
```

Pre/postconditions:

- **Pre:** `ctx.Desugared`, `ctx.Binding`, `ctx.TypeVar` populated.
- **Post:** `ctx.Escape` populated for every expression that allocates;
  `TypeVar.Region` set on the union-find roots of allocating TyVars.

`docs/passes.md` and `docs/architecture.md` already place `Regions` in
the right slot — no doc updates needed beyond this plan.

## TAST representation

`EscapeState` is not yet carried inline in TAST nodes. The frozen TAST
nodes have a `ty: SemType` per case; an `escape: EscapeState option`
field could be added but pulls every consumer (TastShape pretty
printer, target plugins) into a larger surface.

For v1, **escape state stays in `ctx.Escape`** and is discarded at
Freeze (matching how the side tables work today). Target plugins that
need it will re-run a small "compute escape state from frozen TAST"
helper using the same constraint generator — the rules in this doc
work just as well over TAST as over CST.

This is the same trade-off `generalisation-plan.md` made: ship without
TAST surface in v1, revisit when target plugins actually need
monomorphisation / escape state.

When target plugins do come online, v2 adds a fourth field to TAST
nodes: `escape: EscapeState option`, mirroring the third axis of
TypeVar all the way through. Defer that until the first plugin needs it.

## Test strategy

[`RegionsTests.fs`](../../../test/XParsec.FSharp.SemanticAnalysis.Tests/RegionsTests.fs)
follows the `CoverageTests.fs` pattern but asserts against
`ctx.Escape` rather than `tast.Decls`. Since the pass doesn't mutate
the TAST, tests need access to the `PassContext`. The pipeline exposes
a sibling entry point:

```fsharp
// Pipeline.fs
let analyseWithContext provider input lexed file : PassContext * TastFile = ...
let analyse              provider input lexed file : TastFile = ...  // thin wrapper
```

**Deviation:** the plan implied `escapeOf` would work over a frozen
TAST. The implementation goes through `PassContext` directly because
`EscapeState` isn't carried inline in the TAST yet (see §TAST
representation) — the table is the source of truth.

```fsharp
let private analyse input =
    let lexed, file = parseFile input
    let ctx, _ = Pipeline.analyseWithContext MockBuiltins.provider input lexed file
    ctx, file

/// Look up a module-level binding's headPat NodeKey by name, then
/// query ctx.Escape.
let private escapeOf (input: string) (name: string) : EscapeState option = ...
```

Headline tests (all passing):

1. **Local closure doesn't escape.** `let useLocal () = let f x = x + 1
   in f 3` — `f`'s region is `LocalStack`.
2. **Returned closure escapes.** `let mkAdder n = fun x -> x + n` —
   `mkAdder`'s region is `CallerStack` (it returns a closure that
   captures `n`).
3. **Module-level tuple binding is `LocalStack`.** `let p = (1, 2)` —
   `p`'s region is `LocalStack` (no enclosing function frame to escape).
4. **Returned tuple.** `let f () = (1, 2)` — `f` is `CallerStack`.
5. **Local tuple in a function-bound let is `CallerStack`.**
   `let useLocal () = let p = (1, 2) in p` — `useLocal` returns the
   tuple, so the tuple (and `useLocal`) classify as `CallerStack`. The
   `<=` half of the level rule fires this case.
6. **Doubly-captured closure becomes `HeapShared`.** `let mk x = fun y
   -> fun z -> x + y + z` — `x` reaches two distinct lambda regions,
   seeding `HeapShared`. `mk` itself is `CallerStack`.
7. **Pure arithmetic has no escape entry.** `let r = 1 + 2` — `r`
   has no entry in `ctx.Escape` (the value doesn't allocate, and the
   InfixApp's primitive result type skips region minting).
8. **Branching joins regions.** `let f b = if b then (1, 2) else (3, 4)`
   — `f` is `CallerStack`; the if-join region is seeded `CallerStack`
   via the level rule, and `f` lifts via the closure→body edge.
9. **Identifier reuse shares region.** `let r = let x = (1, 2) in let y
   = x in y` — `x` and `y` have the same `RegionId` (Ident pass-through
   plus binding-region-IS-rhs-region identity).
10. **Recursive binding doesn't crash the solver.** `let rec f x = f x`
    — `f`'s region can self-loop through capture/body edges; the
    fixpoint solver converges.
11. **Fun param region uses the lambda's own frame depth.**
    `let mk = fun a -> a` — `a` mints with `MintFunctionLevel = 1`
    (the lambda's frame), the non-strict level rule seeds it
    `CallerStack`, and the closure→body edge lifts `mk` to
    `CallerStack`. Regression for the `registerParam` ordering fix
    (see §Lambdas Deviation).
12. **Mutual recursion sees sibling region during body walk.**
    `let rec a () = b and b () = (1, 2)` — `a` is `CallerStack`. The
    pre-record pass in `processBindingGroup` plants both bindings'
    regions before any body walk, so a's `Ident b` resolves to `r_b`
    and `b`'s CallerStack propagates through the closure→body edge.
13. **Conservative fallback.** `let xs = [ 1; 2 ]` — list literals fall
    through to the fallback path (or, if the parser routes them to a
    handled shape, no entry); both are accepted as correct postures.

## Open questions

- **Effect signatures on function application.** The v1 rule "result
  region ≥ each argument region" is conservative — any call to `id 1`
  pessimistically marks `id`'s result as needing to outlive `1`. Real
  per-function effect signatures (`'a -> 'a` says nothing escapes;
  `'a -> 'b` says nothing escapes either; `'a -> ('a -> 'b)` says the
  argument is captured) would refine this. Lands when we have a clean
  way to encode effects on external symbols and on inferred function
  schemes. Defer until target-plugin requirements force it.
- **Module-level vs function-level scope.** Resolved by the
  `MintFunctionLevel > 0` gate on the level rule: module-top mints
  (where `FunctionStack` is empty and `MintFunctionLevel` defaults to
  `0`) skip the seed entirely. Module-level `let f x = ...` mints `f`
  as a lambda region with `Level = 0`, `MintFunctionLevel = 0`, no
  seed → `LocalStack`. Its body's allocations, minted with
  `MintFunctionLevel = 1`, still fire the level rule and the
  closure→body edge lifts `f` to `CallerStack`. Tests 2, 4, 7 cover
  this.
- **Recursive bindings.** Handled by the fixpoint solver. Test
  "recursive binding doesn't crash the solver" exercises `let rec f x
  = f x`.
- **Mutual recursion + escape.** `let rec a x = b x and b x = a x` —
  the region graph has a 2-cycle once both bindings are pre-recorded
  (see the function-form deviation above). The flat fixpoint loop
  converges via LUB without needing explicit SCC compression. Covered
  by the "sibling region is visible during body walk" test, which
  exercises the more interesting case where one side returns a tuple
  that should propagate through the cycle.
- **`HeapShared` seeding rule precision.** "Reachable through ≥ 2
  lambda boundaries" catches the common closure-of-closure pattern but
  misses cases like "value stored in a returned record." Records aren't
  in the v1 subset, so this is academic until they land. When they do,
  the seed rule extends: any value reachable through a heap-allocated
  data constructor (record / DU / array) seeds as `HeapShared`.

## Out of scope for this plan

- **Effect signatures and per-function escape annotations.** v2 — see
  Open questions above.
- **Borrow-checker-style lifetime annotations.** Rust does these
  syntactically (`'a` lifetimes on function signatures); we don't.
  Region inference is opaque to the user — the `EscapeState` is internal
  metadata, not a surface feature.
- **Region-driven trait dispatch.** Rust uses lifetimes to influence
  trait selection (e.g. `Send` requires `'static` references). The
  spec ([§3 Phase Pipeline](../../../semantic-analysis.md#3-phase-pipeline-overview))
  explicitly forbids this — it would turn the pipeline into a fixpoint.
  Each target plugin handles its own region-aware dispatch downstream
  of Freeze.
- **Target-specific lowering decisions.** Phase 4.6 in the spec —
  `LocalStack -> ref struct` (.NET), `LocalStack -> &T` (Rust),
  `HeapShared -> Rc<T>` / `Arc<T>` (Rust). All downstream of Freeze,
  in target plugins.
- **Ref / mutable / async region rules.** None of these constructs are
  in the v1 subset. When they land, each contributes new seed / edge
  rules:
  - `ref` allocation seeds `HeapShared` (refs are heap cells).
  - `mutable` let-binding: the cell's region is `HeapShared` if any
    use captures it; otherwise `LocalStack`.
  - `async { ... }` body: every captured variable seeds `HeapShared`
    (the continuation might run on another thread).
- **Cross-file region inference.** Same constraint as cross-file
  generalisation — needs the file's exported schemes to carry escape
  annotations. Defer until the multi-file orchestration story (see
  [`docs/architecture.md`](architecture.md#parallelism)) is real.
- **Validation diagnostics that depend on escape state.** "This
  `ref struct` argument escapes its allowed region" is a Validation
  job, not a Regions job. Regions just records the state; Validation
  reads it and emits diagnostics if the target plugin's rules forbid
  the escape. Lands with target plugins.
