# Regions plan

The build plan for **region / escape analysis** in the semantic-analysis
pipeline — Phase 4.4 of [`semantic-analysis.md`](../../../semantic-analysis.md),
the third axis of the [`TypeVar`](typevar.md). Different shape from
[`generalisation-plan.md`](generalisation-plan.md) and
[`measures-plan.md`](measures-plan.md): the type/measure plans extended an
existing pass, this one **wakes up an existing no-op pass** that already
has its slot in the pipeline.

The status quo is a placeholder. `Passes/Regions.fs` is six lines of
`ignore (ctx, file)`. `RegionId` exists ([`SemanticInfo.fs:8`](../SemanticInfo.fs))
as a `[<Struct>]` wrapper over `int` with `Unknown = -1`. `EscapeState`
exists as `LocalStack | CallerStack | HeapShared`. `TypeVar.Region` is
plumbed on the TyVar; `ctx.Escape: SideTable<EscapeState>` is plumbed on
`PassContext`. Nothing reads or writes either.

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
| `TypeVar.Region`                       | `SemanticInfo.fs:174`                           | Plumbed, never written. |
| `ctx.Escape` side table                | `SideTables.fs:53`                              | Plumbed, never written. |
| `Regions.run` skeleton                 | `Passes/Regions.fs:16`                          | No-op. Just `ignore (ctx, file)`. |
| Pipeline slot                          | `Pipeline.fs:19`                                | Called between `Unification` and `Validation`. |
| `TypeVar.Level` (let-depth)            | `SemanticInfo.fs:186`                           | Owned by Unification; the natural ordering for "this region outlives that one." Read-only here. |
| `ctx.Binding` (resolved binding sites) | `Passes/NameResolution.fs`                      | Done — captures = free variables = bindings whose site is **above** the lambda's level. |

The pieces missing are:

1. A **region graph** built per file: nodes = `RegionId`s, edges =
   inequality constraints (`R_A >= R_B` meaning "A outlives B").
2. A **constraint generator** that walks the CST, mints regions, and
   adds edges per allocation / data-flow rule.
3. A **solver** that does transitive closure (Tarjan SCC + topo
   propagation) to compute each region's escape state.
4. Wiring into `TypeVar.Region` (per-tyvar region tag) and `ctx.Escape`
   (per-expression escape state).
5. Conservative "anything that could escape becomes `HeapShared`" fallback
   for constructs the v1 subset doesn't model precisely yet.

No data-model changes outside `Regions.fs`-local types — `RegionId` and
`EscapeState` are already the right shape. The plan adds an internal
`RegionGraph` value used during the pass and discarded before `Freeze`.

## The algorithm: inequality propagation on a region graph

Regions are not a union-find problem — they're a **partial order**.
"`R_A >= R_B`" means region A's scope contains region B's scope, i.e.
A outlives B. The graph captures these constraints; the solver computes
each region's escape state as the least upper bound of the regions it
reaches via outgoing edges.

The mechanics:

- **Each allocating expression gets a fresh `RegionId`.** Literals,
  primitives, `int`/`bool`/`float` arithmetic results don't allocate —
  they get `RegionId.Unknown` and are skipped.
- **Each let-binding site has a "scope region"** corresponding to the
  scope in which the bound name is live. That's the `let`'s enclosing
  scope — module, function body, inner `let`-block, etc.
- **Edges** are added per the rules in [§Constraint generation](#constraint-generation)
  below. The graph is a DAG by construction (no cycles — regions
  can't outlive themselves through their own dependencies).
- **Solving**: Tarjan SCC then topological propagation. Each region's
  escape state starts at `LocalStack`; for every outgoing edge `R_A ≥
  R_B`, lift A to at least B's state. Iterate to fixpoint per SCC (in
  the absence of cycles this is one pass).

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
        // The RHS value flows into the binding site.
        addEdge (region(b.headPat)) >= (region(b.expr))
    // The let-expression's overall region is the body's.
    region(e) = region(body)
```

For `let x = rhs in body`: `x`'s region (the binding site) must
**outlive** rhs's region — i.e., the binding region is at least as
wide as the rhs region. Anything `rhs` holds onto must survive long
enough for `x` to hold onto it. Edge: `region(x) >= region(rhs)`.

The let-expression as a whole evaluates to `body`, so it inherits
body's region.

### Lambdas (the interesting case)

```fsharp
| Expr.Fun (argPats, body) ->
    let r = freshRegion (level = currentLet)
    region(e) = r
    // Each captured variable must outlive the closure.
    for captured in freeVars body do
        addEdge (region(captured)) >= r
```

A lambda allocates a closure. The closure's region is fresh, stamped
at the level of the enclosing `let` (so it's bounded by that scope by
default). For every variable the body captures — that is, every free
variable not in `argPats` — add an edge saying that variable's region
must outlive the closure. If the closure escapes upward, that pulls
all its captures along with it.

`freeVars body` reads `ctx.Binding` to find every use site whose
`BindingSite` lies outside the lambda's argument-pattern keys. We
already do this work implicitly during NameResolution; surfacing the
free-variable set would require a small addition to that pass's output,
OR — simpler — recomputing it in Regions by walking the lambda body
and consulting `ctx.Binding`.

### Function application

```fsharp
| Expr.App (fn, args) ->
    // The result region is fresh — we don't know statically where the
    // callee returned a value from.
    let r = freshRegion (level = currentLet)
    region(e) = r
    // The result must outlive every captured argument (conservative —
    // refines once we have effect signatures on functions).
    for arg in args do
        addEdge r >= (region(arg))
```

This is the v1 over-approximation: we assume any function might return
its arguments (or values reachable through its arguments), so we
constrain the result region to outlive each argument's region. Real
F# / Rust would derive this from effect signatures (`'a -> 'b` makes
no escape claim; `'a -> &'b 'a` does). For v1 we don't have effect
signatures — the conservative edge stands.

When effect signatures land (much later), this rule splits into "pure
function" (no escape edges) vs "effectful function" (the current
behavior).

### Tuples

```fsharp
| Expr.Tuple items ->
    let r = freshRegion (level = currentLet)
    region(e) = r
    for it in items do
        addEdge r >= (region(it))   // tuple holds onto each element
```

A tuple is an allocation that holds references to each element. The
tuple's region must outlive each element's region.

### Sequential, if-then-else, match

```fsharp
| Expr.Sequential (items) ->
    // Result is the last item's region; intermediate items don't escape.
    region(e) = region(items.Last)

| Expr.IfThenElse (cond, thenE, elseE) ->
    // Result region is fresh, must outlive both arms (the result might
    // be either).
    let r = freshRegion (level = currentLet)
    region(e) = r
    addEdge r >= (region(thenE))
    addEdge r >= (region(elseE))

| Expr.Match (scrutinee, arms) ->
    let r = freshRegion (level = currentLet)
    region(e) = r
    for arm in arms do
        addEdge r >= (region(arm.body))
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
    region(e) = RegionId.Unknown   // unit, no allocation

| Expr.Assignment (lhs, rhs) ->
    region(e) = RegionId.Unknown   // unit
    // RHS value flows into LHS — RHS must outlive LHS's binding.
    addEdge (region(lhs)) >= (region(rhs))
```

Loops return unit. Assignment returns unit but generates a flow edge.
(Mutability validation is `Validation`'s job — Regions just records the
flow.)

### What we explicitly DON'T handle yet

`Object`, `Record`, `RecordClone`, ref/mutable bindings, `seq`/`async`
computation expressions — these are all out of subset scope and will
extend the constraint generator when they land. The
"every-unknown-construct-is-HeapShared" fallback covers them safely in
the interim. See [§Conservative fallback](#conservative-fallback) below.

## Conservative fallback

For any CST node Regions doesn't have a precise rule for, the fallback
is: mint a fresh region at the **outermost** scope level (level 0) and
add no edges. That gets it classified as `HeapShared` by the solver
(the widest possible escape state). This is wrong in the sense of
"might over-allocate to heap" but right in the sense of "never lets a
stack-allocated value escape its scope."

The fallback applies to `Object`, `Record`, `String` (interpolation
holes), and anything else not enumerated above. Each falls off the
conservative cliff as Regions grows precise rules for it.

## Data-model changes

Strictly local to `Passes/Regions.fs`. No changes to `SemanticInfo.fs`,
`SideTables.fs`, or the pass pipeline.

```fsharp
module Regions =

    /// Region graph node — captures the let-depth this region was
    /// minted at (matches `TypeVar.Level`, used by the solver to map
    /// from "outlives-relation" to `EscapeState`).
    type private RegionNode =
        {
            Id: RegionId
            Level: int
            /// Outgoing "outlives" edges: this region must outlive
            /// each of these. Mutable during construction; sealed
            /// before solving.
            mutable Outlives: ResizeArray<RegionId>
        }

    /// Per-PassContext graph state. Constructed fresh each `run`,
    /// discarded when the pass returns. Lives entirely in this module —
    /// `RegionId` is the only thing that leaks out (into TypeVar.Region
    /// and ctx.Escape).
    type private RegionGraph() =
        let nodes = Dictionary<RegionId, RegionNode>(HashIdentity.Structural)
        let mutable nextId = 0

        member _.Fresh(level: int) : RegionId =
            let id = RegionId(nextId)
            nextId <- nextId + 1
            nodes.[id] <- { Id = id; Level = level; Outlives = ResizeArray() }
            id

        member _.AddEdge(longer: RegionId, shorter: RegionId) : unit =
            // longer ≥ shorter — `longer` must outlive `shorter`.
            nodes.[longer].Outlives.Add(shorter)

        member _.Nodes = nodes
```

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

The solver walks the graph, computes `EscapeState` per region, and
writes `ctx.Escape` per expression NodeKey:

```fsharp
let private solve (g: RegionGraph) : Dictionary<RegionId, EscapeState> =
    // Initial state: every region is LocalStack.
    let state = Dictionary<RegionId, EscapeState>(HashIdentity.Structural)
    for kv in g.Nodes do
        state.[kv.Key] <- LocalStack

    // Fixed-point relax: for every edge R_long >= R_short, if R_short
    // is wider than R_long currently, lift R_long.
    let mutable changed = true
    while changed do
        changed <- false
        for kv in g.Nodes do
            let r = kv.Value
            for target in r.Outlives do
                let lifted = lub state.[r.Id] state.[target]
                if lifted <> state.[r.Id] then
                    state.[r.Id] <- lifted
                    changed <- true

    state

let private lub (a: EscapeState) (b: EscapeState) : EscapeState =
    match a, b with
    | HeapShared, _ | _, HeapShared -> HeapShared
    | CallerStack, _ | _, CallerStack -> CallerStack
    | LocalStack, LocalStack -> LocalStack
```

### How `CallerStack` and `HeapShared` are seeded

The solver above starts every region at `LocalStack`. Nothing lifts
them. We need **seeds**: rules that classify certain regions as
`CallerStack` or `HeapShared` to start, and then propagation widens
their neighbours.

Two seed rules for v1:

1. **A region whose `Level` is strictly less than the function it was
   constructed in** seeds as `CallerStack`. This catches "returned from
   a function." How we know the "function it was constructed in":
   when constructing a region, we track the enclosing function's level
   on a stack (push at lambda body entry, pop at exit). If the region's
   level < that stack-top, the value lives in the caller's frame.

2. **A region reachable through ≥ 2 lambda boundaries via outlives
   edges** seeds as `HeapShared`. This is the "captured by a closure
   that's captured by another closure" pattern — the value can't live
   on any single stack frame.

Both rules are heuristics. The plan accepts they over-approximate in
some cases (a `CallerStack` value passed sideways into a deeper lambda
might get marked `HeapShared` incorrectly) and notes the refinements
needed in [§Open questions](#open-questions). The conservative direction
is safe: over-marking heap is correct-but-slow, under-marking is unsafe.

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

    // Project per-expression escape state via TyVar.Region.
    for kv in ctx.TypeVar.AsDictionary() do
        let tv = UnionFind.find kv.Value
        if tv.Region.Raw >= 0 then
            match state.TryGetValue tv.Region with
            | true, s -> ctx.Escape.Set(kv.Key, s)
            | false, _ -> ()
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

`RegionsTests.fs` follows the `CoverageTests.fs` pattern but asserts
against `ctx.Escape` rather than `tast.Decls`. Since the pass doesn't
mutate the TAST, tests need a helper to extract escape state from a
named binding:

```fsharp
/// Given an analysed file and a binding name, return its escape state
/// (or None if not classified — i.e. not an allocation).
let private escapeOf (input: string) (name: string) : EscapeState option = ...
```

Headline tests:

1. **Local closure doesn't escape.** `let f x = x + 1` inside a
   function body — `f`'s region is `LocalStack`.
2. **Returned closure escapes.** `let mkAdder n = fun x -> x + n` —
   the inner lambda is `CallerStack`; `n` is also `CallerStack`
   (captured by the escaping closure).
3. **Local tuple.** `let p = (1, 2) in fst p` — `p`'s region is
   `LocalStack`.
4. **Returned tuple.** `let f () = (1, 2)` — the tuple is
   `CallerStack`.
5. **Doubly-captured closure becomes `HeapShared`.** `let mk x = fun y
   -> fun z -> x + y + z` — the inner-inner lambda escapes through two
   levels; `x` is `HeapShared`.
6. **Pure arithmetic has no escape entry.** `let r = 1 + 2` — `r`
   has no entry in `ctx.Escape` (the value doesn't allocate).
7. **Branching joins regions.** `let f b = if b then (1, 2) else (3, 4)`
   — the result region outlives both arms; classified `CallerStack`.
8. **Identifier reuse shares region.** `let x = (1, 2) in let y = x
   in y` — `x` and `y` share the same region (Ident pass-through rule).

Each test:

```fsharp
test "returned closure escapes" {
    let escape = escapeOf "let mkAdder n = fun x -> x + n\nlet a = mkAdder 5" "mkAdder"
    Expect.equal escape (Some CallerStack) "mkAdder returns a closure"
}
```

A handful of additional tests cover the **conservative fallback** (an
unsupported construct gets `HeapShared`), the **idempotence** of
re-running the pass (it shouldn't crash on already-classified TyVars),
and the **interaction with `migrateBounds`** (after union, region info
on the survivor — though this lands fully when SRTPs do).

## Open questions

- **Effect signatures on function application.** The v1 rule "result
  region ≥ each argument region" is conservative — any call to `id 1`
  pessimistically marks `id`'s result as needing to outlive `1`. Real
  per-function effect signatures (`'a -> 'a` says nothing escapes;
  `'a -> 'b` says nothing escapes either; `'a -> ('a -> 'b)` says the
  argument is captured) would refine this. Lands when we have a clean
  way to encode effects on external symbols and on inferred function
  schemes. Defer until target-plugin requirements force it.
- **Module-level vs function-level scope.** `let f x = ...` at module
  level — `f` is `LocalStack` (lives for the module's life) but its
  return value is `CallerStack` (escapes out of `f`). The level-stack
  needs to distinguish "module top" from "function nesting" so we don't
  mark every module-level binding as escaping. Solvable but not pretty;
  the current sketch assumes module level = 0 and function entry pushes
  level 1+. Verify with a test before declaring the design final.
- **Recursive bindings.** `let rec f x = f x` — `f`'s region is its
  own (potentially through the App rule). Easy case: f's region edges
  point to itself, the solver's fixpoint handles it without blowing
  up. Verify with a test.
- **Mutual recursion + escape.** `let rec a x = b x and b x = a x` —
  same as above but the region graph has a 2-cycle. SCC compression
  in the solver collapses the cycle; the SCC's escape state is the
  LUB of every member's. Already in the algorithm; no special case
  needed, but worth a test.
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
