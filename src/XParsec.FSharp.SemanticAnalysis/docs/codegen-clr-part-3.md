# XParsec.FSharp.Codegen.Clr — slice 3 plan

Follow-on to [codegen-clr-part-2](codegen-clr-part-2.md), whose thin-slice #2
(`let x = 1 + 2` / `printfn "%d" x`) is implemented and passing. This doc plans
[il-emission-roadmap](il-emission-roadmap.md)'s **thin-slice #3**:

```fsharp
let inline succ x = x + 1
printfn "%d" (succ 41)
```

It assumes the part-2 file layout (`Types`, `Cil`, `CilBuilder`, `Metadata`,
`ICodegenProvider`, `ClrProvider`, `Emit`, `Codegen`) and the part-2 deviations
all carry forward (host-derived assembly identities; untyped depth-tracked
walker; `Main`-local slots keyed by binding `NodeKey`; the `Emit`-closure
`CallRecipe`; the `FSharpFunc.Invoke` consumption path).

## The headline: slice 3 is TAST surgery, not new IL

The whole point of `inline` is that the call site needs **no function value at
runtime**. `succ` is never represented as an `FSharpFunc`, never closed over,
never `Invoke`d. Instead its retained body is expanded *into* the call site and
beta-reduced against the actual arguments, leaving plain arithmetic over a
local. So:

```
succ 41                       (* App(Var succ, Const 41) *)
  ──expand──►  (fun x -> x + 1) 41
  ──β-reduce─►  let x = 41 in x + 1
```

and that lowers with **only slice-2 mechanics** — a local slot (`stloc`/`ldloc`)
and the `add` intrinsic. The consequence worth stating up front:

> **Slice 3 touches no IL, metadata, or provider code.** `Cil`, `Metadata`,
> `ICodegenProvider`, `ClrProvider`, `Codegen` are all unchanged. The work is
> entirely in the TAST walker (`Emit.fs`) plus one reusable TAST utility
> (`Inline.freshen`). This is the slice that proves the *front-end / back-end
> seam* — `Inline.inlineExpand` already exists ([front-end-gaps-plan](front-end-gaps-plan.md) §C);
> slice 3 is the first consumer.

`printfn "%d" (...)` is unchanged from slice 2 — the `(...)` is just an `int`
expression now produced by inline expansion instead of `1 + 2`.

## Implementation status

**Thin-slice #3 is implemented and passing end-to-end.** All three build
milestones below are green:

1. `Inline.freshen` + unit tests (`InlineTests.fs`: binders renamed and
   references rewired across two shared-minter expansions; free vars left
   alone).
2. The expression-level `TExpr.Let` arm — `printfn "%d" (let y = 41 in y + 1)`
   → `42` (`Slice3Tests.fs`).
3. Inline collection + call-site expansion + beta reduction — the full sample
   `let inline succ x = x + 1` / `printfn "%d" (succ 41)` → `42`, plus the
   nesting case `printfn "%d" (succ (succ 40))` → `42` that proves freshening.

Decisions taken as planned: `Inline.freshen` is a pure TAST→TAST rewrite in
`Inline.fs`; `Emit` owns the build-wide minter (a counter packed into
`NodeKind.SynthInlineExpansion` keys); the threaded context is bundled into a
private `EmitEnv` record (`il` stays a separate output-buffer parameter); inline
collection is a `emitMain` pre-pass so use-before-definition resolves; beta
reduction lowers to nested `TExpr.Let` nodes re-emitted through the one `Let`
arm. As promised, `Cil.fs`, `Metadata.fs`, `ICodegenProvider.fs`,
`ClrProvider.fs`, `Codegen.fs` are untouched.

## What the sample demands that slice 2 lacks

| Need | New mechanic |
|---|---|
| `let inline succ` not emitted as a value | `emitMain` collects inline lets into a side table; does **not** allocate a slot or emit the `Lambda`. |
| `succ 41` at the call site | dispatch an `App` whose spine head is a `Var` bound to an inline decl → expand, don't call. |
| same body reused at independent call sites | `NodeKey` **freshening** so two expansions don't share a bound name (and thus a local slot). |
| applying the expanded lambda to its args | **beta reduction**: peel the curried `Lambda`s, binding each param to its argument as a `TExpr.Let`. |
| `TExpr.Let` (expression-level) | local-slot store + body emit — the generalisation of slice 2's top-level `TDecl.Let`. |

## Scope: monomorphic inline only

The roadmap's slice-3 sample is deliberately **monomorphic**: `x + 1` forces
`x : int` via `MockBuiltins`' int-only `op_Addition`, so `succ : int -> int`
has *no quantified typars* (`Inline.quantifiedTypars (TyFun(int,int)) = []` —
verified in `InlineTests`). Expansion therefore calls `inlineExpand decl [||]`,
which returns the retained body verbatim (the empty-substitution fast path).

Deriving the `typeArgs` for a **polymorphic** inline (matching the call-site's
concrete types against the binding's generalised scheme in `quantifiedTypars`
order) is the harder half and is deferred to **slice 5**, where the polymorphic
`let inline sum xs = List.fold (+) 0 xs` lands. Slice 3 wires the expansion
machinery with `[||]` so slice 5 only has to compute the array.

## TAST shapes (what `Emit` will walk)

Grounded in `test/XParsec.FSharp.SemanticAnalysis.Tests/InlineTests.fs`. The
inline binding freezes to its own decl (it is `let … in` at module scope —
[[reference_module_let_in_splits]]), and the body to a second decl:

```
TDecl.Let(
  TPat.NamedSimple(k_succ, TyFun(int, int)),
  Lambda(NamedSimple(k_x, int),
         App(App(External "op_Addition", Var(k_x, int)), Const(Int 1)),   // : int
         TyFun(int, int)),
  isInline = true,
  TyFun(int, int))

TDecl.Expression(
  App(App(External "printfn", New("…PrintfFormat", [Const(String "%d")])),  // : int -> unit
      App(Var(k_succ, TyFun(int, int)), Const(Int 41))),                    // : int
  unit)
```

Read off the nodes:
- The call site `succ 41` is `App(Var(k_succ, …), Const(Int 41), int)` — the
  spine head is a **`Var`**, not an `External`. `k_succ` is the binding-site
  `NodeKey`, equal to the inline decl's `TPat.NamedSimple` key (the same
  Var-key == binding-key invariant slice 2 relies on and tests).
- `inlineExpand succDecl [||]` returns the `Lambda(NamedSimple(k_x, int), …)`
  body **by reference**, sharing `k_x` with the decl — hence freshening before
  use.

## Design changes, file by file

### `NodeKey.fs`
- Add one synthetic kind, `NodeKind.SynthInlineExpansion`, so freshened keys
  never collide with source keys (the sign bit already separates them) nor with
  other synthetics (distinct kind). `NodeKey.ofSynthetic` already exists; the
  minter packs a monotone counter into its `spawningOffset` slot for
  uniqueness.

### `Inline.fs` (SemanticAnalysis — reusable, not CLR-specific)
- Add `freshen : (unit -> NodeKey) -> TExpr -> TExpr`. A single pre-order
  rewrite: at every binder (`TPat.NamedSimple` keys, `TExpr.ForTo` vars) mint a
  fresh key via the supplied minter and record `old → new`; rewrite every
  `TExpr.Var` / binder reference through the map; **free** vars (keys not bound
  within the body) pass through untouched. The caller owns the minter so the
  counter is shared across all expansions in one build.
- Update the module header: NodeKey freshening is now provided here; beta
  reduction stays the caller's responsibility (it needs the call-site args).
- `inlineExpand` / `quantifiedTypars` unchanged.

### `Emit.fs`
- **Bundle the threaded context into an `EmitEnv` record** —
  `{ Provider; Ctx; Slots: Dictionary<NodeKey,int>; Inlines: Dictionary<NodeKey,TDecl>; Mint: unit -> NodeKey }`.
  Slice 3 pushes past the comfortable positional-arg count (provider, ctx,
  slots, inlines, mint, il); the record keeps `emitExpr`/`foldInvoke` legible.
  `il` stays a separate parameter (it is the output buffer).
- **`emitMain`**: a pre-pass populates `Inlines` from every
  `TDecl.Let(NamedSimple(k,_), _, isInline=true, _)` (keyed by `k`, value the
  whole decl), emitting nothing for them. The second pass emits non-inline
  `let`s (slot + value + `stloc`, as slice 2) and expressions. The pre-pass
  makes expansion order-independent (an inline used before its textual
  definition still resolves).
- **`TExpr.Let(NamedSimple(k, ty), value, body, _)`**: `DeclareLocal ty`,
  `Slots.[k] <- slot`, emit `value`, `stloc`, emit `body` (its value is left on
  the stack). The expression-level twin of slice 2's top-level let. Non-
  `NamedSimple` bound patterns (tuple/record/union destructuring) are out of
  scope → fail loudly.
- **`App` arm — extend the head dispatch** after `collectSpine`:
  1. head `External name` → slice-2 recipe path (unchanged).
  2. head `Var k` with `k ∈ Inlines` → **inline expansion**:
     `inlineExpand decl [||]` → `Inline.freshen env.Mint` → peel the curried
     `Lambda`s, binding param `pᵢ` to spine arg `aᵢ`, into a nested `TExpr.Let`
     chain wrapping the lambda body → `emitExpr` that chain. **Guard:** lambda
     count must equal the spine-arg count; a residual lambda (partial
     application) or leftover args (over-application) means a closure → fail
     (slice 5).
  3. head `Var k` not inline → a local function value → fail (closure, slice 5).
  4. otherwise → slice-2 function-value path (`emit head; foldInvoke`).
- **`TExpr.Var k`**: check `Inlines` *first* — a bare inline value reference
  (`let inline k = 5`) expands (`inlineExpand` + `freshen`) and emits its body
  with no beta step; otherwise `ldloc Slots.[k]` as slice 2. (Not in the
  sample, but a one-line generalisation that keeps the inline rule uniform.)
- **`TExpr.Lambda` standalone** → fail loudly. A `Lambda` only legitimately
  reaches the walker as inline-expansion output immediately consumed by beta
  reduction; emitting one as a value is closure synthesis (slice 5).

### unchanged
`Types.fs`, `Cil.fs`, `CilBuilder.fs`, `Metadata.fs`, `ICodegenProvider.fs`,
`ClrProvider.fs`, `Codegen.fs` — no edits. Slice 3 reuses slice-2 locals,
arithmetic, and the `printfn`/`Invoke` path verbatim.

## Build order and testable staging

Each milestone ends with a runnable assertion, per the repo's thin-slice
discipline.

1. **`Inline.freshen` (unit-level).** Add `freshen` + a `SemanticAnalysis.Tests`
   case: freshen the frozen `succ` body twice with a shared minter and assert
   (a) every binder key differs from the original and between the two results,
   (b) the body's internal `Var` is rewired to the new binder, (c) a free `Var`
   is left unchanged. No codegen.
2. **`TExpr.Let` (end-to-end).** Add the expression-level `Let` arm. Test
   `printfn "%d" (let y = 41 in y + 1)` → stdout `42` (exercises `Let` with no
   inline machinery).
3. **Inline expansion (end-to-end).** Wire the inline table + call-site
   expansion + beta reduction. Test the full sample
   (`let inline succ x = x + 1` / `printfn "%d" (succ 41)`) → `42`, **plus a
   nesting test** `printfn "%d" (succ (succ 40))` → `42`. The nested case is the
   one that proves freshening: without it the inner and outer expansions share
   `k_x`'s slot and the program computes `41`.

(1) proves the reusable utility; (2) proves the lowering target; (3) proves the
seam and the freshening invariant.

## Verified facts

Confirmed by `InlineTests.fs` against `MockBuiltins`:

- `let inline succ x = x + 1 in succ 41` → `[ TDecl.Let(NamedSimple, Lambda,
  true, TyFun(int,int)); TDecl.Expression(App(Var, Const(Int 41)), _) ]`.
- `inlineExpand succDecl [||]` returns `Lambda(NamedSimple(_, int),
  App(App(External "op_Addition", Var(_, int)), Const(Int 1)), TyFun(int,int))`
  — by reference (monomorphic no-op substitution).
- `quantifiedTypars (TyFun(int,int)) = []`; a polymorphic `let inline id x = x`
  exposes exactly one typar (the slice-5 case).

## Out of scope (still)

- Closure **synthesis** / emitting a `Lambda` as a value
  ([function-representation-plan](function-representation-plan.md)) — slice 5.
- **Polymorphic** inline: deriving `typeArgs` from the call site, and
  substituting typars through the body. Slice 3 passes `[||]`; slice 5 computes
  the array via `quantifiedTypars` order.
- **Partial / over-application** of an inline function (residual lambda or
  surplus args) — needs the closure path.
- Inline bodies that **capture outer locals** (free vars beyond params +
  externals). The sample has none; freshening deliberately leaves free vars
  alone, so simple captures of `Main`-locals *may* work, but it is untested and
  unscoped here.
- Destructuring inline parameters (`let inline f (a, b) = …`) — non-
  `NamedSimple` bound patterns in `Let`/`Lambda`.
- List literals (slice 4), `List.fold` + `(+)`-as-value (slice 5).

## Open questions / decisions

- **Beta reduction shape.** Lower the peeled application to nested `TExpr.Let`
  nodes and re-`emitExpr`, rather than emitting arg→`stloc`→body inline.
  Reusing the `Let` arm keeps one lowering path and one place that allocates
  param slots. Prefer the `TExpr.Let` construction.
- **`freshen` home.** It is a pure, backend-agnostic TAST→TAST transform, so it
  belongs in `Inline.fs` beside `inlineExpand`, not in the CLR `Emit`. The CLR
  layer owns only the counter/minter.
- **Inline collection: pre-pass vs during-walk.** Prefer the pre-pass so
  ordering (or a future `let rec inline`) doesn't matter.
- **`EmitEnv` record vs positional args.** Adopt the record — slice 3 is where
  the positional threading stops paying for itself.
- **Local slot reuse.** Still deferred (part-2 open question): each beta-bound
  param and each `Let` gets its own slot. A recycling pass is unnecessary until
  bodies get large.

## Cross-references

- [codegen-clr-part-2](codegen-clr-part-2.md) — slice 2; the locals + arithmetic
  + `Invoke` mechanics this slice reuses unchanged.
- [codegen-clr-plan](codegen-clr-plan.md) — part 1; file layout + overall status.
- [il-emission-roadmap](il-emission-roadmap.md) §Thin-slice ordering — slice 3
  in the progression.
- [front-end-gaps-plan](front-end-gaps-plan.md) §C — the `inline` marker +
  `Inline.inlineExpand` this slice first consumes.
- [function-representation-plan](function-representation-plan.md) — the closure
  synthesis slice 3 sidesteps by expanding instead of representing.
- [backend-design-plan](backend-design-plan.md) — side-table posture for the
  inline + slot maps.
