# `let mutable` plan

The build plan for **`let mutable` bindings** in the semantic-analysis
pipeline — the natural next step after [`regions-plan.md`](regions-plan.md).
Same shape as [`generalisation-plan.md`](generalisation-plan.md): the
parser already produces the syntax (`Binding.mutableToken`), and the
semantic-side wiring is split across several passes.

**Status (2026-05-17):** Implemented across
[`NameResolution.fs`](../Passes/NameResolution.fs),
[`Unification.fs`](../Passes/Unification.fs),
[`Regions.fs`](../Passes/Regions.fs), and
[`Validation.fs`](../Passes/Validation.fs). Tests:
[`NameResolutionTests`](../../../test/XParsec.FSharp.SemanticAnalysis.Tests/NameResolutionTests.fs)
(IsMutable propagation),
[`GeneralisationTests`](../../../test/XParsec.FSharp.SemanticAnalysis.Tests/GeneralisationTests.fs)
(no scheme + monomorphic uses + assignment pin),
[`RegionsTests`](../../../test/XParsec.FSharp.SemanticAnalysis.Tests/RegionsTests.fs)
(cell escape states), and
[`ValidationTests`](../../../test/XParsec.FSharp.SemanticAnalysis.Tests/ValidationTests.fs)
(immutable-assignment + VR). Coverage's pre-existing `x <- y` test had to
be rewritten — it relied on the assignment being silent on an immutable
parameter, which is now correctly diagnosed.

Different shape from `regions-plan` (single pass) and
`generalisation-plan` / `measures-plan` (single pass internal to
`Unification`): mutable bindings touch **four passes**
(`NameResolution`, `Unification`, `Regions`, `Validation`) plus the
deferred value-restriction lift that `generalisation-plan` left
pending.

The canonical examples we want to handle after this lands:

```fsharp
// LocalStack: mutable cell with no closure capture.
let useLocal () =
    let mutable n = 0
    n <- n + 1
    n

// HeapShared: mutable cell captured by a closure that escapes.
let mkCounter () =
    let mutable n = 0
    fun () ->
        n <- n + 1
        n          // returned closure mutates n → cell is heap-allocated

// Diagnostic: assignment to an immutable binding.
let x = 1
x <- 2            // error: x is not mutable

// Soundness: a mutable binding does NOT generalise, but it does
// participate in inference normally.
let mutable r = []
//      r : 'a list initially — 'a is free, not yet pinned.
r <- [1]
//      Unifies r's type with int list — pins 'a = int globally,
//      because no scheme was issued and every use of r sees the
//      same TyVar. r : int list from here on.
// At end of analysis, r's resolved type has no free TyVars → no
// value-restriction diagnostic. Compare:
let mutable s = fun x -> x
//      s : 'a -> 'a. With no use to pin 'a, the VR check at end
//      of analysis fires a diagnostic.
```

## Goal

After the pipeline finishes:

- **`ResolvedBinding.IsMutable`** is `true` exactly when the binding's
  CST has a `mutableToken`.
- **`TypeScheme` is never written for a mutable binding** — but the
  binding's type still participates in inference. Every use site
  unifies against the binding's own TyVar (no instantiation), so
  whichever use first pins a free TyVar pins it for everyone.
- **`Validation`** emits a **value-restriction diagnostic** for every
  mutable binding whose resolved type *still contains free TyVars
  at end of analysis* — the unsound case (`let mutable s = fun x ->
  x` with no use to pin `'a`). Mutables whose free TyVars got
  pinned by some use (`let mutable r = []; r <- [1]`) emit nothing.
- **`ctx.Escape`** classifies every mutable cell whose RHS-type
  allocates: `LocalStack` when the cell is uncaptured, `HeapShared`
  when any closure captures it (regardless of how many — the
  threshold-of-2 rule for the immutable case doesn't apply).
- **`Validation`** also emits a diagnostic on `x <- v` when `x`'s
  `ResolvedBinding` has `IsMutable = false`.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `Binding.mutableToken: 'T voption`     | `XParsec.FSharp/Expr.fs:181`                    | Done — parser preserves the keyword. |
| `ResolvedBinding.IsMutable: bool`      | `SemanticInfo.fs:231`                           | Field exists; hardcoded `false` at [`NameResolution.fs:49`](../Passes/NameResolution.fs). |
| `Expr.Assignment`                      | `XParsec.FSharp/Expr.fs`                        | Done — parser produces the node. |
| `inferAssignment`                      | `Unification.fs:1058`                           | Typechecks `lhs <-> rhs`; comments out mutability as a `Validation` concern. |
| Regions `Expr.Assignment` handler      | [`Passes/Regions.fs:246`](../Passes/Regions.fs) | Walks children, emits no edge. The `regions-plan` deviation note explicitly defers the flow edge to "when mutables land." |
| `RegionNode` flag plumbing             | `Passes/Regions.fs`                             | Has `IsLambda`, `InitialState` voption — extending with `IsMutableCell` is the same shape. |
| `Scheme` write site                    | `Unification.fs` `inferBinding` / `generalise`  | Needs a gate on `b.mutableToken.IsSome` to skip generalisation. |
| `Validation` pass                      | `Passes/Validation.fs`                          | Currently mostly empty; the immutable-assignment check and value-restriction check both land here. |

The pieces missing are:

1. **NameResolution**: read `b.mutableToken`, populate `IsMutable`.
2. **Unification**: gate `generalise` on `IsMutable` so mutable bindings
   stay monomorphic — inference itself is unchanged, so free TyVars
   in a mutable binding's type persist past the `let` and can be
   pinned later by any use.
3. **Regions**: a new `IsMutableCell` region kind, a cell-vs-rhs
   separation for mutable bindings, an `Assignment` edge, and a
   threshold-of-1 lambda-reach seed rule.
4. **Validation**: the immutable-assignment diagnostic, plus a
   post-inference value-restriction diagnostic for mutable bindings
   whose resolved type still contains free TyVars.
5. Tests across all four passes.

## Why mutable cells need a separate region

For an immutable binding `let x = rhs`, `recordBindingRegion` makes
`region(x) := region(rhs)` — binder and RHS share one region. The
identity is sound because `x` and `rhs` are two names for the same
immutable value; any escape claim about one is true about the other.

A mutable binding breaks that identity. `let mutable r = (1, 2); r <- (3, 4)`
has *one cell* that holds *two distinct tuple values* over its
lifetime. Sharing a region between `r` and the initial tuple would say
"this region equals exactly this tuple" — but later we assign a
different tuple, which the analysis would have to retroactively also
fold into the same region. That doesn't work.

So: mutable bindings get a **cell region** that is distinct from any
specific RHS region. The cell outlives every value stored into it. The
constraint becomes "RHS values must escape at least as wide as the
cell," which is the conservative direction the existing edge-direction
convention already encodes.

## The algorithm: cell region + threshold-of-1 lambda-reach

The mechanics:

- **Fresh cell region.** `let mutable x = rhs` mints a fresh
  `RegionNode` with `IsMutableCell = true`. The cell is recorded as
  `BindingRegions[x] = cell` and stamped onto `x`'s TyVar.
- **RHS-to-cell flow edge.** For every value flowing into the cell
  (initial binding RHS, every `Assignment` RHS), add
  `addEdge (region(rhs)) >= (cell)` — rhs is the longer side, cell
  the shorter. Propagation pushes the cell's state UP onto rhs: when
  the cell is later seeded `HeapShared` (closure capture), each rhs
  lubs up to match. This is the **opposite direction** from the
  tuple-holds-item rule (`addEdge tuple item`, items propagate up to
  the tuple) — the use cases differ. Tuples need "any item
  heap-shared ⇒ tuple heap-shared" so items are the shorter side;
  cells need "cell heap-shared ⇒ stored values heap-shared" so the
  cell is the shorter side.
- **Capture flow edge.** Unchanged from the existing capture rule:
  when a closure's free vars include the mutable name, add
  `addEdge (cell) >= (closure)`. The cell outlives the closure.
- **Threshold-of-1 lambda-reach seed.** The existing rule for
  non-lambda regions is "reach ≥ 2 lambdas → HeapShared." For
  `IsMutableCell` regions, the threshold drops to **1**. Any closure
  capture forces heap allocation, because .NET hoists captured
  mutables into a compiler-generated ref cell and Rust requires an
  explicit `Rc<RefCell<…>>` / `Arc<Mutex<…>>` (or equivalent) for
  cross-frame mutation.
- **No level-rule seed for the cell.** The level rule
  (`Level <= MintFunctionLevel`) still applies — a mutable cell minted
  inside a function frame that's returned, e.g. via the closure→body
  edge, still gets `CallerStack`. But the mutable-specific
  HeapShared seed dominates when applicable.

Why threshold-of-1 instead of letting propagation do the work:
propagation alone can only push the cell's state UP through outgoing
edges. The capture edge `cell ≥ lambda` would propagate the lambda's
state (`CallerStack`, typically) up to the cell — landing on
`CallerStack`, not `HeapShared`. The conservative classification for
.NET / Rust requires `HeapShared`, so we seed it directly when the
mutable cell has any lambda in its reach.

## Pass-by-pass changes

### `NameResolution`

One-line wiring. `NameResolution.fs:49` currently hardcodes
`IsMutable = false`; read the CST instead.

```fsharp
// Passes/NameResolution.fs ~line 49
ctx.Binding.Set(
    key,
    {
        BindingSite = key
        IsInline = b.inlineToken.IsSome
        IsMutable = b.mutableToken.IsSome
    })
```

No other NameResolution change. The comment at `NameResolution.fs:24`
("They'll get real values when inline / mutable keywords are handled")
can be deleted in the IsMutable half.

### `Unification` — skip generalisation, leave inference otherwise alone

Mutable bindings must NOT generalise. Generalising
`let mutable r = []` to `∀a. a list` and then instantiating fresh
`'a`s per use would let `r <- [1]` pin one instantiation and
`let bad : string list = r` pin a different one — the classic value-
restriction soundness gap from
[`generalisation-plan.md`](generalisation-plan.md#value-restriction).

The fix is one gate in `inferBinding`: skip the call to `generalise`
when the binding's `mutableToken` is set. The binding's headPat TyVar
still gets the inferred type — possibly with free TyVars in it — but
no `TypeScheme` is written to `ctx.Scheme`.

```fsharp
// Passes/Unification.fs, inside inferBinding after RHS inference:
if b.mutableToken.IsNone then
    let scheme = generalise outerLevel ty
    ctx.Scheme.Set(headKey, scheme)
// else: no scheme. Use sites fall through inferIdent's
// Scheme.TryGetValue ValueNone branch and unify against the
// binding's TyVar directly — so whichever use first constrains
// a free TyVar pins it globally.
```

That's the only change in `Unification`. Use sites are unchanged
(`inferIdent`'s `Scheme.TryGetValue` already falls through to the
monomorphic path on `ValueNone`), and the inference walk leaves any
free TyVars in place. Following `let mutable r = []`:

- `r : 'a list` with `'a` unresolved.
- `r <- [1]` calls `inferAssignment`, which unifies the LHS type
  (`'a list`) with the RHS type (`int list`). `'a` gets `Link = int`.
- Subsequent reads of `r` see `int list` through the unionfind.

The post-inference question — "did anything pin those free TyVars?"
— is a `Validation` concern, not a `Unification` one (see below).
This answers the
[`generalisation-plan.md`](generalisation-plan.md#value-restriction)
"Open questions" entry on value-restriction placement: the
*generalisation gate* lives in `Unification`; the *value-restriction
diagnostic* lives in `Validation`, where it can read fully-resolved
types after every use site has been typed.

### `Regions`

The constraint generator extends in three places:

**Mutable binding path in `processBinding`.** Plain bindings currently
share `region(binder) = region(rhs)`. For mutable, mint a separate
cell region and add the RHS edge:

```fsharp
// Passes/Regions.fs processBinding, replacing the plain-binding branch:
if b.argumentPats.IsEmpty then
    let rhsR = inferRegion s ctx b.expr
    if b.mutableToken.IsSome then
        // Mutable: fresh cell region, distinct from rhs.
        let cell =
            s.Graph.Fresh(
                level = s.EnclosingLet,
                mintFn = functionStackTop s,
                isLambda = false,
                isMutableCell = true,
                seed = ValueNone
            )
        s.Graph.AddEdge(rhsR, cell)         // rhs ≥ cell
        recordBindingRegion s ctx b.headPat cell
    else
        // Immutable: existing identity rule.
        recordBindingRegion s ctx b.headPat rhsR
else
    // Function-form binding — unchanged; functions are never mutable.
    ...
```

**`Expr.Assignment` rule.** The current Regions handler walks
children for capture-edge side effects but emits no flow edge. Add the
edge from RHS region into LHS region (which, for an `Ident` LHS on a
mutable binding, is the cell):

```fsharp
// Passes/Regions.fs inferRegionImpl:
| Expr.Assignment(leftExpr = l; rightExpr = r) ->
    let lhsR = inferRegion s ctx l
    let rhsR = inferRegion s ctx r
    s.Graph.AddEdge(rhsR, lhsR)      // value stored ≥ cell
    RegionId.Unknown                 // Assignment expression is unit
```

For non-`Ident` LHS (record field, array index) the LHS region is
`RegionId.Unknown` today (those constructs route through the
conservative fallback), so `AddEdge` short-circuits via the existing
unknown-filter — no special-case needed.

**`RegionNode.IsMutableCell` flag + threshold-of-1 seed.** Extend
`RegionNode`:

```fsharp
type private RegionNode =
    {
        Id: RegionId
        Level: int
        MintFunctionLevel: int
        IsLambda: bool
        /// True for the cell region of a `let mutable` binding. Lowers
        /// the lambda-reach threshold from 2 to 1.
        IsMutableCell: bool
        InitialState: EscapeState voption
        mutable Outlives: ResizeArray<RegionId>
    }
```

`RegionGraph.Fresh` takes a new `isMutableCell: bool` parameter;
existing callsites pass `false`. In `solve`'s seed pass, the
lambda-reach gate becomes:

```fsharp
// Passes/Regions.fs solve, replacing the existing non-lambda reach check:
if not node.IsLambda then
    let reach = countReachableLambdas g (RegionId(i))
    let threshold = if node.IsMutableCell then 1 else 2
    if reach >= threshold then
        state.[i] <- HeapShared
```

That's the only solver change. Everything else (level rule, fixpoint
relax, lub) is unchanged.

### `Validation`

Two diagnostics, both surfacing facts that earlier passes already
have but don't act on.

**1. Assignment to an immutable binding.** For each `Expr.Assignment`
whose LHS is `Expr.Ident` (or single-name
`LongIdentOrOp.LongIdent`), look up `ctx.Binding[lhsKey]` and emit
when `IsMutable = false`.

```fsharp
// Passes/Validation.fs:
let private checkAssignment (ctx: PassContext) (l: Expr<SyntaxToken>) =
    match l with
    | Expr.Ident _
    | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
        let lhsKey = CstKeys.ofExpr l
        match ctx.Binding.TryGetValue lhsKey with
        | ValueSome rb when not rb.IsMutable ->
            ctx.Diagnostics.Add({
                Key = lhsKey
                Message = "assignment to immutable binding"
                Severity = Error
            })
        | _ -> ()
    | _ -> ()
    // Record fields / array slots out of scope — handled when those land.
```

**2. Value restriction on mutable bindings.** Iterate every binding
site that was recorded by `NameResolution` with `IsMutable = true`,
walk its resolved type, and emit a diagnostic if any reachable TyVar
is still free (i.e. has no `Link` after `UnionFind.find`). This is
the post-inference half of the value-restriction story — by the time
`Validation` runs, every use site has already typechecked and had
the chance to pin free TyVars through unification.

```fsharp
// Passes/Validation.fs:
let rec private hasFreeTyVar (t: SemType) : bool =
    match t with
    | TyVar tv ->
        let root = UnionFind.find tv
        match root.Link with
        | ValueSome target -> hasFreeTyVar target
        | ValueNone -> true                  // free
    | TyConst _ -> false
    | TyFun(a, r) -> hasFreeTyVar a || hasFreeTyVar r
    | TyTuple items -> items |> List.exists hasFreeTyVar

let private checkValueRestriction (ctx: PassContext) =
    for kv in ctx.Binding.AsDictionary() do
        let rb = kv.Value
        if rb.IsMutable then
            match ctx.TypeVar.TryGetValue rb.BindingSite with
            | ValueSome tv when hasFreeTyVar (TyVar tv) ->
                ctx.Diagnostics.Add({
                    Key = rb.BindingSite
                    Message =
                        "value restriction: mutable binding has \
                         unresolved type variable(s); add a type \
                         annotation or constrain via a use site"
                    Severity = Error
                })
            | _ -> ()
```

Both checks are wired into the existing `Validation` walker — the
assignment check piggybacks on the per-expression visit, the
value-restriction check runs once at end of file.

Why the VR check belongs here and not in `Unification`: `Unification`
runs the let-group's inference in source order, so at the point
`let mutable r = []` is typed, the body's later `r <- [1]` hasn't
been seen yet. Emitting a diagnostic at let-time would fire
prematurely on every "free-TyVar then later pinned" case the user
expects to be legal. Deferring to `Validation` — which runs after
all of `Unification` — lets the natural unification of LHS and RHS
types pin the free TyVars first, leaving only the genuinely
unconstrained cases to diagnose.

## Data-model changes

Strictly local to `Passes/Regions.fs` and `Passes/Validation.fs`. No
changes to `SemanticInfo.fs`, `SideTables.fs`, or the public
`Pipeline.fs` surface. `RegionId` and `EscapeState` are unchanged.
`TypeVar.Region` still carries the cell region for a mutable binding's
headPat — downstream consumers don't need to know the cell is
"special," just that it has an `EscapeState`.

## TAST representation

`IsMutable` is already carried into TAST via the same `ResolvedBinding`
projection the rest of the pipeline uses. No new TAST surface for the
cell-region distinction — same posture as
[`regions-plan.md`](regions-plan.md#tast-representation): escape state
stays in `ctx.Escape`, target plugins re-derive it if they need it.

When target lowering arrives, the `IsMutable + HeapShared` pair is the
signal for ".NET: emit a `'a ref` field on the closure environment" /
"Rust: emit `Rc<RefCell<'a>>`."

## Test strategy

Tests split across the four pass-test files. Each pass owns its own
half of the contract:

**`NameResolutionTests.fs`:**

1. **Mutable flag is set.** `let mutable n = 0` — `ctx.Binding[n]`
   has `IsMutable = true`.
2. **Immutable flag stays false.** `let n = 0` — `IsMutable = false`.

**`GeneralisationTests.fs`:**

3. **Mutable bindings don't generalise.** `let mutable id = fun x -> x`
   — no entry in `ctx.Scheme[id]`.
4. **Mutable + monomorphic use site.** `let mutable id = fun x -> x;
   id 1; id true` — first use pins `id : int -> int`; second use
   emits a mismatch diagnostic. Mirrors today's pre-generalisation
   monomorphic behaviour.
5. **Mutable + use pins free TyVar via assignment.** `let mutable r =
   fun x -> x; r <- (fun (n : int) -> n + 1)` — `'a` gets pinned to
   `int` by the assignment's unification. `r`'s resolved type is
   `int -> int` afterwards. (Tested via the binding's TyVar's
   resolved type.)

**`RegionsTests.fs`:**

5. **Uncaptured mutable cell is LocalStack.** `let mutable n = (1, 2)
   in n <- (3, 4); n` — `n` classifies `LocalStack` (no closure
   captures it).

   *Actually:* at module top with no enclosing function, the cell
   stays `LocalStack`. Inside a function, the level rule fires and
   the cell goes to `CallerStack` (consistent with
   `let useLocal () = let p = (1, 2) in p` already classifying its
   tuple `CallerStack`).
6. **Captured mutable cell is HeapShared.** `let mkCounter () = let
   mutable n = 0 in fun () -> n <- n + 1; n` — the cell `n` is
   captured by the returned closure → `HeapShared` via the
   threshold-of-1 rule.
7. **Values stored into a captured cell are HeapShared.**
   `let mkCache () = let mutable c = (1, 2) in let _ = fun () -> c
   <- (3, 4) in c` — both `(1, 2)` and `(3, 4)` propagate up to
   `HeapShared` via the RHS-to-cell flow edge.
8. **Module-top mutable with no escape is LocalStack.** `let mutable
   r = (1, 2)` at module level — `r` is `LocalStack` (no enclosing
   function frame to escape, no closure capture).

**`ValidationTests.fs` (new or existing):**

9. **Assignment to immutable emits a diagnostic.** `let x = 1; x <- 2`
    — diagnostic on `x`'s assignment.
10. **Assignment to mutable is clean.** `let mutable x = 1; x <- 2`
    — no diagnostic.
11. **Unresolved LHS doesn't crash.** `unknownName <- 1` — no
    Validation crash (the existing unresolved-name diagnostic from
    NameResolution covers it; Validation's mutability check just
    skips).
12. **Value restriction: free TyVar at end of analysis → diagnostic.**
    `let mutable id = fun x -> x` (no uses) — VR diagnostic on
    `id`'s binding, because `'a -> 'a` still has a free `'a`.
13. **Value restriction: pinned by use → no diagnostic.**
    `let mutable id = fun x -> x; id 1` — `'a` pinned to `int` by
    the application; no VR diagnostic.
14. **Value restriction: pinned by assignment → no diagnostic.**
    `let mutable r = fun x -> x; r <- (fun (n : int) -> n + 1)` —
    assignment unifies `'a -> 'a` with `int -> int`; no VR
    diagnostic. This is the headline test for the
    "let-time-doesn't-fire" behaviour that motivates putting the
    check in Validation rather than Unification.
15. **Value restriction: concretely-typed mutable is clean.**
    `let mutable n = 0` — `int` has no free TyVars; no VR
    diagnostic regardless of use.
16. **Value restriction: annotated mutable is clean.**
    `let mutable r : int list = []` — the annotation eliminates
    free TyVars before VR runs. No diagnostic. (Lands when type-
    annotated bindings of polymorphic concrete types work in the
    subset; in the meantime the test can use a function annotation
    like `let mutable f : int -> int = fun x -> x`.)

## Open questions

- **Mutable record fields.** F#'s `mutable` keyword also annotates
  record fields. Field-level mutation interacts with record
  destructuring and field projection in ways this plan doesn't
  address. Lands with records — see
  [`regions-plan.md`](regions-plan.md#out-of-scope-for-this-plan)
  for the records-out-of-scope note. The cell-region machinery in
  this plan extends naturally: each mutable field of a heap-allocated
  record is its own cell, with edges from every field-assignment RHS.
- **`ref` cells.** F#'s `ref` builds an explicit `Ref<'a>` record
  (compiled to a single-mutable-field type). The region treatment is
  the same shape as `let mutable` — a fresh cell region with the
  threshold-of-1 rule — but the construct is allocation-explicit
  (`ref x` is a heap allocation, always), so the cell seeds
  `HeapShared` unconditionally, not just on capture. Lands when the
  `Ref<'a>` provider entry does.
- **`byref<'a>`, `inref<'a>`, `outref<'a>`.** .NET-specific managed
  pointers with strict scope rules (no field-of, no array-element,
  no escape from `ref struct`). These are type-system features more
  than region-analysis ones; the right move is probably a separate
  `byref-plan.md` that treats them as a constrained subset of region
  classification. Out of scope here.
- **Threading model.** Captured mutables are unsafe to share across
  threads in F# / .NET without explicit synchronisation. The plan's
  `HeapShared` classification is the right region-level
  conservatism, but the *diagnostic* "this mutable closure-capture
  needs synchronisation if shared cross-thread" is a Validation
  concern tied to async / `Task` / `Thread.Start` handling. Defer
  until any of those lands.
- **Validation-of-assignment placement.** Could go in `Unification`
  (cheaper — emit during the existing typecheck walk) or
  `Validation` (matches the "validate, don't infer" contract). This
  plan picks `Validation` to match the docs/passes.md contract
  and keep `Unification` focused on type inference.

## Out of scope for this plan

- **Mutable record fields, refs, byrefs.** See Open questions.
- **Mutable struct fields and `inref`/`outref` parameter modes.**
  Same family as byref; defer with it.
- **`StringBuilder`-style mutable APIs from .NET.** External symbols
  with mutable methods are handled by their `IExternalSymbolProvider`
  contract; no semantic-analysis change needed.
- **Cross-thread soundness.** See Open questions / threading model.
- **Validation diagnostics that depend on `HeapShared`
  classification.** E.g. ".NET `ref struct` cannot be captured by a
  closure" lands with target plugins, same posture as
  [`regions-plan.md`](regions-plan.md#out-of-scope-for-this-plan)'s
  "Validation diagnostics that depend on escape state" item.
- **Inline `let mutable`-as-`ref-cell` sugar.** Some compilers rewrite
  `let mutable x` captured-by-closure into `let x = ref _; …; !x` /
  `x := _`. That's a target-lowering decision (in `Phase 4.6`), not
  a semantic-analysis one. The escape state on the cell tells the
  target plugin whether the rewrite is required.
