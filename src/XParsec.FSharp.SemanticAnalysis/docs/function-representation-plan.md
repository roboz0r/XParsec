# Function representation plan

How to represent function values — at inference time and at codegen
time — so the canonical sample and everything past it gets a
performant IL story without churning the type-checker.

## The reframing

Treat the function-arrow surface as sugar over an interface constraint:

```
'A -> 'B   ≡   'T when 'T :> Fun<'A, 'B>
```

with a longer-term variant that adds `and 'T : allows ref struct` when
region analysis ([regions-plan](regions-plan.md)) proves the value
doesn't escape its frame.

Practical consequence: every "function value" in IL is a value of some
generic typar bounded by `Fun<_, _>`. `Microsoft.FSharp.Core.FSharpFunc<_, _>`
becomes one impl among many. Value-type impls give zero-alloc closures
and JIT devirtualisation of `Invoke`.

## Two layers, kept separate

The alias is a **codegen contract, not an inference rewrite**. Two
distinct layers:

### Inference layer (unchanged)

`SemType.TyFun(a, b)` stays the structural arrow the unifier sees today.
Algorithm J keeps working as-is; the SRTP / IWSAM drain sites keep
firing as today. No new constraint plumbing in the unifier.

Rationale: making every arrow-introduction a fresh typar with an
`Fun` bound forces the unifier to handle "interface-bounded typars
unify structurally with each other and with concrete impls" everywhere.
Doable, but you pay it everywhere for value you only need at codegen.

### Codegen layer (new)

At the IL-emission boundary, each parameter of arrow type at a generic
method becomes a fresh `<TF> where TF : Fun<a, b>` in the IL signature;
every application of that parameter rewrites to a constrained
`callvirt TF.Invoke`. Concrete (non-parameter) arrow types lower to a
chosen impl — `FSharpFunc<a, b>` by default.

This is where the alias lives. The TAST node `TExpr.App` is the trigger:
its `fn` is either (a) a `TExpr.External` or `TExpr.Var` referring to a
generic typar, in which case the call goes through the Fun constraint,
or (b) a concrete value, in which case the call goes through the
chosen impl directly.

## First-pass shape: FSharpFunc implements Fun

Additive, no break to existing F# semantics, the cleanest migration
path. Implementation choice:

- **(a) Own `FSharpFunc` in the lib's CLR target.** Once
  `Core.CLR.fs` ships real implementations
  ([fsi-target-brainstorm](fsi-target-brainstorm.md)), the lib's
  `FSharpFunc<_, _>` is *our* type and trivially implements `Fun<_, _>`.
  Code we emit references the lib's `FSharpFunc`, not BCL's. This is
  the long-term shape.
- **(b) Wrapper struct in user assemblies.** Until (a) is real, the
  codegen emits a thin wrapper struct per arrow type at each
  assembly boundary that adapts the BCL `FSharpFunc` to `Fun`. Adds
  one struct per arrow-arity in use; effectively free.

(b) is the bridge; (a) is the destination. Both keep BCL FSharp.Core
runnable.

The `Fun<'A, 'B>` interface itself lives in the
[minimum-requirements `.fsi`](fsi-target-brainstorm.md) — it's
load-bearing for every function call in IL, can't be opt-in.

## What this does to the canonical sample

```fsharp
let inline sum xs = List.fold (+) 0 xs
let nums = [1; 2; 3; 4; 5]
printfn "%d" (sum nums)
```

With `inline fold` and `inline (+)` (both currently in the lib), the
call site for `sum nums` lowers to something like:

```
; sum monomorphised at the call site to:
;   sum (xs: FSharpList<int32>) : int32 =
;     List.fold<TF, int32, int32> (synth OpAddInt32) 0 xs
; where TF : Fun<int32, Fun<int32, int32>>
; and List.fold is inline, so its body splices in too.

loop:
  ldloc state
  ldloc x
  call int32 …Operators::op_Addition(int32, int32)
  stloc state
  ; advance xs by tail …
```

Zero `FSharpFunc` allocations, zero `callvirt` through `Fun`. The
synthesised `OpAddInt32` struct (one per arrow-shape per program) is a
stateless value type whose `Invoke` is the lowered `(+)` body, which
the JIT inlines via constrained-call devirt.

Without `inline` on `fold`, you'd still call `TF.Invoke` via the
`constrained.` prefix — JIT devirts for value-type instantiations of
`TF` even across the call boundary, so you still avoid the boxing path.
`inline` is what gets you full inlining of both `Invoke` and `add`
into the loop body.

## SRTP coexists

These mechanisms live at different layers and don't fight:

- **SRTP** is the inference mechanism that picks *which* member to
  call when the type checker sees `1 + 2`. Output: a resolved
  compiled-name reference (`op_Addition` against `Int32`).
- **Fun constraint** is the codegen representation of where *that
  resolved member* lives when the function value crosses a parameter
  boundary. Output: a generic-typar bound at the calling method.

The unifier still does its SRTP work; codegen wraps the resolved
member in an `Fun`-implementing struct when it needs to be passed.

## Return positions

`'A -> 'B` in *parameter* position becomes a constrained typar
cleanly. In *return* position the constraint form doesn't work —
you can't return "some `TF` where `TF : Fun<a, b>`" without an
existential.

Two escape hatches, in priority order:

1. **Inline the factory.** If `let inline mkAdder x = fun y -> x + y`,
   the caller never sees a returned function — the lambda body splices
   in directly. This subsumes most "immediately consumed" cases for
   free and rides on the `inline` work already on the front-end gap
   list ([front-end-gaps-plan](front-end-gaps-plan.md) §C).
2. **Specialise the return type via escape analysis.** Caller-side
   pass proves the returned closure doesn't escape its frame and
   rewrites the factory's signature to return the concrete struct
   type. Requires monomorphising the factory per call site when the
   struct type isn't nameable in the source signature — same machinery
   that powers `allows ref struct`.
3. **Fall back to `FSharpFunc`.** When neither (1) nor (2) applies,
   return `FSharpFunc<a, b>` (the v1 default impl). Caller pays the
   interface dispatch cost unless inlined at its own use site.

v1 ships (3). (1) comes with `inline`. (2) comes with regions.

## Region / ref-struct extension

`allows ref struct` is the .NET 9 constraint that permits a generic
type parameter to be instantiated with a `ref struct`. Adding it to
the `Fun`-bounded typar lets stack-only closures pass through
generic combinators without escaping to the heap.

Strictly opt-in and additive — relaxing `TF : Fun<…>` to
`TF : Fun<…>, allows ref struct` is non-breaking, because the call
sites that were already passing a heap-allocated impl keep working
unchanged. The widening happens only at sites where escape analysis
proves the value doesn't outlive its frame.

Out of scope for v1. Lands as a follow-up pass that runs *after*
codegen has chosen `Fun`-constrained signatures — strictly a
constraint-loosening rewrite.

## Out of scope

- Higher-arity `Fun<...>` overloads as a perf hack (avoiding curried
  `Fun<a, Fun<b, c>>`). v1 emits curried chains; flattening is a
  follow-up codegen opt.
- Optimised currying / `OptimizedClosures.FSharpFunc` parity. v1's
  Fun-constraint mode subsumes the same wins where it applies;
  keeping the BCL `OptimizedClosures` shim is a compat concern, not a
  perf one.
- Cross-assembly Fun shapes for libraries already shipped against
  BCL `FSharpFunc`. Handled by the wrapper-struct bridge described
  in §First-pass shape until those libs are recompiled against the
  curated lib.

## Cross-references

- [il-emission-roadmap](il-emission-roadmap.md) — the surrounding
  roadmap; this plan is the codegen-layer item.
- [front-end-gaps-plan](front-end-gaps-plan.md) §C — the `inline`
  work, which this plan depends on for full devirtualisation.
- [regions-plan](regions-plan.md) — escape analysis, prerequisite
  for the `allows ref struct` extension.
- [fsi-target-brainstorm](fsi-target-brainstorm.md) — where the
  `Fun<_, _>` interface lives (minimum-requirements `.fsi`) and
  the per-target FSharp.Core split that lets the lib's `FSharpFunc`
  implement it.
