# Generalisation plan

This is the build-plan for `let`-generalisation in the semantic-analysis
pipeline. It's the natural next step after the current expression-coverage
expansion: the infrastructure (passes, side tables, union-find, fresh-TyVar
minting) is already there; we just don't compute or use schemes.

The status quo is monomorphic. Every binding pins exactly one inferred type
on its `BindingSite` TypeVar, shared by every use site
(`Unification.inferIdent`, [`Passes/Unification.fs:297`](../Passes/Unification.fs)).
The canonical failure case is in the README's *What this is not (yet)* list:

```fsharp
let id = fun x -> x in id 1 + (if id true then 0 else 1)
```

Today's pipeline solves `'a -> 'a` for `id`'s TypeVar, then the first use
pins `'a = int`, then the second use tries to unify `int` with `bool` and
emits a mismatch. After generalisation it should type-check.

## Goal

After `Unification` finishes, every local `let`-bound name has a **type
scheme** `∀ᾱ. τ` stored in a side table. Every reference to that name
**instantiates** the scheme with fresh TypeVars so independent use sites
don't share variables.

External symbols already work this way — `ExternalSymbol.Instantiate :
unit -> SemType` ([`ExternalSymbols.fs:11`](../ExternalSymbols.fs)) is
exactly the per-use freshen-up pattern. This plan extends that model from
provider-supplied symbols to user-written `let`s.

## What we have to build on

| Piece                              | Where                                      | Status |
|------------------------------------|--------------------------------------------|--------|
| Per-use fresh TypeVars for externals | `ExternalSymbols.fs` `poly`                | Done — direct template for local schemes. |
| Union-find with `Link` for solved vars | `UnionFind.fs`, `SemanticInfo.fs` `TypeVar` | Done — orthogonal to levels; no rework. |
| Occurs check                       | `Unification.fs` `occurs`                  | Done — needs to grow a level-adjustment side effect. |
| Side-table-per-fact pattern        | `SideTables.fs` `SideTable<'V>`            | Done — schemes go in a new table. |
| Forward pipeline contract          | `docs/passes.md`                           | Generalisation is internal to `Unification`; no new pass. |
| `inferIdent` reads `Binding`       | `Unification.fs:297`                       | Needs to consult the new Scheme table first. |
| `inferBinding`                     | `Unification.fs:647`                       | The generalisation site; needs to wrap RHS with `generalise`. |

The pieces missing are:
1. A **level** on `TypeVar`.
2. A `TypeScheme` value + a `Scheme` side table.
3. **Level-tracking** in `Unification` (current level, push/pop at `let`).
4. **`instantiate`** at use sites and **`generalise`** at binding sites.
5. **Value restriction** to gate generalisation. *Deferred — see §Value restriction.*
6. TAST representation for schemes — *minimal in v1, see §TAST.*

## The algorithm: Rémy's levels

The naive Damas–Milner rule for "which TyVars in `τ` can I generalise?" is
"those not free in the surrounding type environment Γ." Computing free-in-Γ
is O(|Γ|) per `let` and Γ grows with nesting. Rémy's level-based
implementation makes the test O(1) per TyVar by **marking each TyVar with
the let-depth at which it was minted** and generalising those whose level
strictly exceeds the level of the enclosing scope at generalisation time.

The mechanics:

- A mutable `currentLevel: int` is threaded through inference. Push it on
  entering the RHS of a `let`, pop it after generalising.
- `freshTv` stamps the new TypeVar with `Level = currentLevel`.
- When `unify` links a TyVar at level `N` to a compound type, walk that
  type and **lower every reachable TyVar's level to min(level, N)**.
  Combine with the existing occurs check — same traversal.
- `generalise τ outerLevel` walks `τ` and collects every TyVar with
  `level > outerLevel`. Those become the scheme's quantified variables.
- `instantiate scheme` walks the body, substituting each quantified TyVar
  with a fresh one **at the current level**. Equivalent to
  `ExternalSymbol.Instantiate` but with a finite, captured set of `'a`s.

Why levels rather than scanning Γ: levels add one `int` per TypeVar
(`+8` bytes via padding — see [[feedback_reader_size_irrelevant]] —
already negligible against the existing class layout). They eliminate
the env-scanning cost, which would otherwise be quadratic on deeply-nested
expressions.

## Data-model changes

### `SemanticInfo.fs`

Add a `Level` field to `TypeVar`. It's authoritative on the union-find
*representative* — call `UnionFind.find` before reading it. When `union`
merges classes, the surviving root inherits `min(rootA.Level, rootB.Level)`.

```fsharp
and [<Sealed>] TypeVar() =
    // existing fields...
    /// Let-depth at which this TyVar was minted. Lowered by unify when
    /// the TyVar becomes reachable from a shallower scope. Generalisation
    /// quantifies TyVars whose level strictly exceeds the enclosing scope's
    /// level. Authoritative on the union-find root — call UnionFind.find
    /// before reading. (Could move to every TyVar with lazy reconciliation
    /// in generalise if level reads become hot; measure first.)
    member val Level: int = 0 with get, set
```

New type next to `SemType`. A record (rather than a `unit -> SemType`
closure like `ExternalSymbol.Instantiate`) so monomorphisation can later
enumerate the quantified vars and reconstruct substitutions:

```fsharp
/// `Quantified` are the TyVars that act as ∀-bound parameters of `Body`.
/// `instantiate` walks Body, substituting each Quantified TyVar with a
/// fresh one at the current level — independent use sites get independent
/// variables, mirroring [[ExternalSymbol.Instantiate]] but for finitely
/// many user-declared `'a`s.
[<Sealed>]
type TypeScheme =
    val Quantified: TypeVar list
    val Body: SemType
    new(qs, body) = { Quantified = qs; Body = body }
```

### `SideTables.fs`

Add a new table and a current-level slot on `PassContext`:

```fsharp
/// Written by Unification. Keyed by binding-site NodeKey (the headPat /
/// lambda-param / TypeMember key). Present only for `let`-bound names
/// after generalisation. Module-level lets, nested lets, lambda params
/// *not* generalised (params are monomorphic by the standard ML rule).
member val Scheme = SideTable<TypeScheme>() with get

/// Current let-depth. Owned by Unification — push on entering a binding
/// RHS, pop after generalising. Reset to 0 between top-level bindings.
/// Lives on PassContext (rather than threaded as a parameter) to match
/// ctx.Diagnostics — same scope, same mutation pattern, same 30+
/// call-site reach without parameter pollution.
member val CurrentLevel = 0 with get, set
```

Keep `Binding`'s `ResolvedBinding` thin — adding a `Scheme` field there
mixes name-resolution with type-scheme storage. The Scheme table follows
the one-table-per-fact principle from
[`docs/architecture.md`](architecture.md#cst-in-tast-out-side-tables-in-between).

### `UnionFind.fs`

`union` already picks the surviving root by rank. Add one line to inherit
the lower level so the representative's `Level` is authoritative:

```fsharp
let union (a: TypeVar) (b: TypeVar) : unit =
    let rootA = find a
    let rootB = find b
    if not (System.Object.ReferenceEquals(rootA, rootB)) then
        let mergedLevel = min rootA.Level rootB.Level
        // ... existing rank-based reparenting ...
        let newRoot = if /* rootA wins */ then rootA else rootB
        newRoot.Level <- mergedLevel
```

## Pipeline integration

Generalisation lives entirely inside `Unification` — no new pass, no
change to `docs/passes.md`'s pass order. The rules above are an extension
of the algorithm, not a separate phase.

### `Unification.fs` — current level

`ctx.CurrentLevel` starts at 0. Increment on entering a `let`-binding's
RHS, decrement after generalising. Reset to 0 between top-level bindings
(each module-level `let` is its own outer scope). The level *crosses*
lambda boundaries — lambda parameters take the current level (they're
not generalised).

### `freshTv` and `TypeVar()`

`freshTv` becomes the only place TyVars are minted from inside passes.
Set `Level = currentLevel` on each new TypeVar. (The bare `TypeVar()`
constructor still exists but should be considered internal — callers in
`Passes/` go through `freshTv`. Worth marking the constructor `internal`
if F# tolerates it on a sealed class; otherwise add a code-review note.)

### `unify` — level adjustment

When a `TyVar tv` unifies with a non-variable type `t`, walk `t` and for
each reachable TyVar with `level > tv.Level`, lower it to `tv.Level`. Fold
this into the existing `occurs` check — it already walks the same
structure, just for a different purpose:

```fsharp
let rec private occursAndAdjust (target: TypeVar) (t: SemType) : bool =
    match resolveStep t with
    | TyVar tv ->
        let root = UnionFind.find tv
        if System.Object.ReferenceEquals(root, target) then
            true
        else
            if root.Level > target.Level then
                root.Level <- target.Level
            false
    | TyConst _ -> false
    | TyFun(a, r) -> occursAndAdjust target a || occursAndAdjust target r
    | TyTuple xs -> List.exists (occursAndAdjust target) xs
```

(The existing short-circuit `||` is fine here — early exit on occurs-fail
means we don't adjust everything, but a failed unification produces a
diagnostic anyway, and adjusting after the fact is wasted work.)

### `inferIdent` — instantiate on lookup

Before the existing local-binding TypeVar lookup, consult the Scheme
table. If a scheme is present, return a fresh instantiation:

```fsharp
and private inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
    match ctx.Binding.TryGetValue key with
    | ValueSome rb ->
        match ctx.Scheme.TryGetValue rb.BindingSite with
        | ValueSome scheme -> instantiate ctx scheme
        | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
    | ValueNone ->
        // External-symbol fallback unchanged.
        ...
```

`instantiate` is a 10-line helper: build `Dictionary<TypeVar, TypeVar>`
mapping each `scheme.Quantified` entry to a fresh TyVar at the current
level, then walk `scheme.Body` rewriting `TyVar v` to `TyVar substitute[v]`
where the substitution applies.

### `inferBinding` — generalise after RHS

After typing the RHS and unifying with the head pattern, generalise:

```fsharp
and private inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
    enterLevel ctx
    let patTy = inferPat ctx b.headPat
    let rhsTy = /* existing inference */
    unify ctx (CstKeys.ofBinding b) patTy rhsTy
    exitLevel ctx
    if shouldGeneralise b then
        let scheme = generalise ctx (zonk patTy) outerLevel
        ctx.Scheme.Set(CstKeys.ofBinding b, scheme)
```

`generalise` walks the (zonked) type and collects every TyVar root with
`level > outerLevel`, dedupes (a single TyVar can appear in multiple
positions), and returns the scheme. Quantified TyVars stay live in the
union-find graph — they're just no longer "free" with respect to the
outer scope.

## Recursion

The conventional ML rule applies: **polymorphic recursion is not
supported**. A `let rec f = …` allocates `f`'s TypeVar at the outer level,
types the RHS using `f`'s monomorphic type (no instantiation), then
generalises once the RHS is fully typed.

For `let rec … and …`, all sibling bindings live at the outer level
during their joint type-checking; each gets generalised independently
after the full group is solved. This already matches what
`NameResolution.EnterBindingRhs` exposes ([`Passes/NameResolution.fs:138`](../Passes/NameResolution.fs))
— the sibling-scope hook is reused unchanged.

The `let rec f x = f` occurs-check diagnostic
(`ExpansionTests.fs`'s "occurs check rejects let rec f x = f") continues
to fire — generalisation runs *after* RHS unification, so a recursive
self-application that creates an infinite type still bombs first.

## TAST representation

Two options, with a clear short-term/long-term split:

**v1 (this plan):** TAST stays monomorphic per use site. `TPat.NamedSimple`
keeps its single `ty: SemType` — but for a generalised binding, that `ty`
becomes the **scheme's body type**, with the quantified TyVars surviving
as their union-find roots. `TExpr.Var` carries the **instantiated** type
at that specific use. Downstream consumers that want the principal scheme
can re-query the Scheme side table — except we discard side tables at
Freeze. So for v1, ship without a TAST scheme representation; revisit
when a target plugin actually needs to monomorphise.

**v2 (deferred, blocks monomorphisation):** Add `TPat.NamedScheme of
binding: NodeKey * scheme: TypeScheme` and `TExpr.Var` carries either
its concrete type *or* a substitution map mapping scheme TyVars to
concrete types. A monomorphiser walks call sites and emits specialised
variants per substitution. This is the bridge to the "monomorphisation"
goal but is a significantly larger build-out and depends on real type
definitions (records, DUs) being present.

The README's *What this is not (yet)* line about generalisation should
be deleted in v1; the line about monomorphisation should stay.

## Value restriction

F#'s rule mirrors ML: only **syntactic values** generalise. Refs,
mutables, sequenced expressions, and applications of polymorphic
functions don't. Without value restriction, this is unsound:

```fsharp
let r = ref []          // would generalise to ∀a. a list ref
r := [1]                // pins it to int
let bad : string list = !r   // boom at runtime, type-check passed
```

The tiny subset has no `ref`, no `mutable` bindings on `let`, no I/O —
nothing that breaks soundness if we generalise everything. So **v1 skips
value restriction** and generalises every `let`. Add it when refs land.

When it does land, the check goes in `Validation` per
[`docs/passes.md`](passes.md#validation-diagnostics), reading the Scheme
table and emitting a diagnostic if a generalised binding's RHS isn't a
syntactic value. (Or, more conservatively, hoist the check into
`Unification` so we *don't* generalise in the first place — saves the
generalisation work and means downstream passes see the right scheme.
Decision deferred.)

## Test strategy

The failure case `let id = fun x -> x in id 1 + ...` becomes the
headline test. The full set:

1. **Polymorphic identity used at two types.** `let id = fun x -> x in
   id 1, id true` types as `int * bool`.
2. **Polymorphic constant.** `let k x = fun _ -> x in k 1 true` types as `int`.
3. **Polymorphic recursion explicitly rejected.** `let rec f x = f true; x`
   used at `f 1` should *not* type-check (f is monomorphic during its own
   RHS — pinning to `bool -> ?` makes the call `f 1` mismatch).
4. **Mutual recursion + generalisation.** `let rec id x = x and pair x = id x, id x`
   — `id` generalises after the mutual group.
5. **Lambda params don't generalise.** `fun id -> id 1 + id true` *should*
   fail (param is monomorphic).
6. **Nested let-poly.** `let outer () = let inner x = x in inner 1, inner true`
   — `inner` generalises inside `outer`'s body.
7. **External vs local schemes coexist.** `let f = (|>) in f 1 (fun x -> x + 1)`
   — local `f` instantiates from the pipe scheme; later use takes a fresh
   instantiation.
8. **Occurs check still fires.** `let rec f x = f` keeps emitting the
   existing diagnostic.

Each test follows the existing `CoverageTests.fs` pattern — `analyse +
declType + Expect.equal + Expect.isEmpty Diagnostics`.

## Open questions

Both deferred — answer when we get to the work that depends on them.

- **Value-restriction placement** (defer until refs / mutable bindings
  land). Two viable spots: inside `Validation` reading the Scheme table
  and emitting a diagnostic on unsound schemes, or hoisted into
  `Unification` so we don't generalise in the first place. The latter
  avoids paying generalisation cost on bindings that shouldn't be
  generalised and means downstream passes always see the right scheme;
  the former is read-only and matches the `Validation` contract more
  cleanly. Pick when there's something soundness-breaking to validate
  against.
- **Interaction with `migrateBounds`** (defer until SRTP / IWSAM
  resolution is wired). The SRTP/IWSAM migration in `Unification.fs:61`
  is unaffected today — bounds attach to TypeVars and follow the union-
  find rules. Open question is what happens when a *quantified* TyVar
  carries bounds: presumably the bounds get re-instantiated alongside
  the TyVar on each use site, but there's no callback machinery to
  verify against. Revisit when on-unified callbacks land.

## Out of scope for this plan

- **Higher-rank polymorphism** (`forall a. (a -> a) -> int`). HM-style
  generalisation handles let-polymorphism only; ranked types need
  bidirectional inference. Not a near-term need for an F# subset.
- **Value restriction implementation** — designed for, not built.
- **Monomorphisation.** Generalisation produces schemes; a monomorphiser
  walks call sites and specialises. Different pass, different design,
  different prerequisites (type definitions must exist first).
- **Generalisation of type-class bounds** (SRTP / IWSAM). The bounds list
  on `TypeVar` is the right place; the resolution callback story per
  [`docs/typevar.md`](typevar.md#srtp-and-iwsam-bounds) needs to land
  first.
- **TAST-level scheme representation** — v2 above. Defer until a
  monomorphisation pass is on the docket.
