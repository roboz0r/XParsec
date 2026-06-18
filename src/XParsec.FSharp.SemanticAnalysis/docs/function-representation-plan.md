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

## First-pass shape: Vesper-native `Fun`

Closures emit as **reference-type subclasses of `Vesper.Fun<'A, 'B>`**, a
Vesper-native interface declared in
[Vesper.Core](../../Vesper.Core/README.md). No
`Microsoft.FSharp.Core.FSharpFunc` dependency — Vesper.Core ships its
own arrow representation, and the broader trajectory is to remove
`FSharp.Core` references entirely.

The `Fun<'A, 'B>` interface itself lives in the
[minimum-requirements `.fsi`](fsi-target-brainstorm.md) — it's
load-bearing for every function call in IL, can't be opt-in.

### Why reference type for v1

A reference-type closure base mirrors the existing IL story for
"function values" without needing struct-generic typar dance or
`allows ref struct` constraints. It's the path of least resistance to
correct semantics; per-impl perf shape is a follow-up.

The struct-closure shape (no allocation, JIT devirtualisation of
`Invoke` via constrained-call) is **deferred until generics work
correctly end-to-end** — see §Generic closures for the current
correctness gap. Tactically choosing struct or class per local `Fun`
implementation is on the table once that lands. The .NET 9 `allows ref
struct` story is later still — see §Region / ref-struct extension.

An earlier draft of this plan used `FSharpFunc<_,_>`-implements-`Fun` as
a transitional bridge (own a copy in the lib's CLR target, or wrap the
BCL one). That bridge is **obsolete**: Vesper.Core ships its own
`Fun<_,_>` and closures inherit from it directly, no FSharpFunc in
sight. Cross-assembly interop with `fsc`-compiled DLLs is the only
remaining concern, handled separately by
[`fsharp-compat-plan`](fsharp-compat-plan.md).

## What this does to the canonical sample

> Note: the lowering below shows the **perf-mature shape** — struct
> closures, constrained calls, JIT devirt. v1 emits reference-type
> closures (per §First-pass shape), so the per-iteration cost still
> includes a `callvirt` on `Invoke`. Struct-impl synthesis per local
> `Fun` lands as a follow-up pass once generic closures
> (§Generic closures) are working correctly.

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

Zero `Fun` allocations, zero `callvirt` through `Fun`. The synthesised
`OpAddInt32` struct (one per arrow-shape per program) is a stateless
value type whose `Invoke` is the lowered `(+)` body, which the JIT
inlines via constrained-call devirt.

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

Three escape hatches, in priority order:

1. **Inline the factory.** If `let inline mkAdder x = fun y -> x + y`,
   the caller never sees a returned function — the lambda body splices
   in directly. This subsumes most "immediately consumed" cases for
   free and rides on the `inline` work already on the front-end gap
   list ([front-end-gaps-plan](front-end-gaps-plan.md) §C).
2. **Specialise the return type via escape analysis.** Caller-side
   pass proves the returned closure doesn't escape its frame and
   rewrites the factory's signature to return the concrete closure
   type. Requires monomorphising the factory per call site when the
   closure type isn't nameable in the source signature — same machinery
   that powers `allows ref struct`.
3. **Return a `Fun`-typed value.** When neither (1) nor (2) applies,
   the factory returns a `Vesper.Fun<a, b>` — the closure value's
   static type is the interface, and the caller pays virtual dispatch
   on `Invoke` unless inlined at its own use site. This is the v1
   default. When the factory itself is generic (`mkConst : 'a -> Fun
   <unit, 'a>`), the *closure being returned* needs generic
   synthesis — see §Generic closures.

v1 ships (3). (1) comes with `inline`. (2) comes with regions.

## Generic closures

A closure declared inside a generic static method observes the
enclosing method's typars in its captures, its parameter type, or its
result type. Each `fun …` in the source emits a sealed
`TypeDefinition` (§First-pass shape); that type must encode its
capture-field signatures, its ctor, and its `Invoke` against typars
the enclosing method introduced.

The design is the same machinery generic unions and generic records
already use: the closure becomes its **own generic `TypeDefinition`**
whose typar set mirrors its enclosing method, the construction site
`Newobj`s through a `MemberRef` on the closure's instantiated
`TypeSpec`, and member accesses inside the closure's own body route
through the same `TypeSpec` parent.

The canonical motivator:

```fsharp
let mkConst x =                // 'a -> unit -> 'a
    let f = fun () -> x        // the let-f indirection blocks peelLambda;
    f                          //   without it, peelLambda peels both args
let always10 = mkConst 10
let alwaysHi = mkConst "hi"
```

The inner closure captures `x : 'a`. No operator is applied to `x`
inside the inner closure, so no operator-default mechanism can collapse
`'a` to a concrete type — `mkConst` stays genuinely polymorphic. The
synthesis (landed 2026-05-26) emits the inner closure as a generic
`TypeDefinition` `<closure>$0\`1`, one capture field encoded as `!0`,
a `Fun\`2<unit, !0>` `InterfaceImpl`, and `Newobj`s through a
`MemberRef` on `<closure>$0<!!0>` from inside `mkConst`'s body.

### Not the same as the `mkAdder` case

`let mkAdder x = fun y -> y + x` is **not** a generic-closure case —
it's a defaults-not-applied symptom. F#'s `+` carries `… and default
^T1: int and default ^T1: ^T2 …` static-member constraint defaults, so
in correctly-defaulting unification `x` and `y` both collapse to
`int`. If Vesper's defaults aren't yet resolving the operator's
defaults, that's a separate gap (operator-defaults / static-member-
constraint resolution), not a closure gap.

`mkConst` cannot be defaulted under any operator-defaulting scheme —
there is no operator. It is the genuine trigger.

### Why typars, not monomorphisation per call site

A theoretically valid alternative — emit one closure `TypeDefinition`
per distinct instantiation observed at call sites — is rejected for
the same reason generic unions aren't monomorphised: unbounded code-
size growth, and a poor fit for a contract stack that doesn't see
every consumer at compile time. The generic-typar path is the one
already proven for unions and records.

### Scope

- **Closure-in-closure with a deeper typar context** is in scope and
  landed — each closure carries the transitive typar set it observes
  (`Closure.Typars` is inherited verbatim by reference, pinned by
  `ReferenceEquals` in the C1 tests). No additional design.
- **Sibling closures in the same generic static fn's scope** are in
  scope and landed — `mkPair`'s pattern (three closures inheriting one
  typar each, see `CapturedMutableTests`) verifies all three emit as
  generic `TypeDef`s with one `GenericParam` row each.

### Implementation status (as of 2026-05-26)

The synthesis is implemented end-to-end against the C1 / C2 / C3 plan:

- **C1 (TAST + discovery).** `Emit.Closure` carries `Typars: TypeVar
  list`; `Emit.discoverClosures` threads `currentTypars` along the
  walker. A closure inside a generic static fn inherits that fn's
  typars; a closure resident in `Main` / a top-level value let / a
  monomorphic static fn carries `[]`; inner closures inherit the
  parent's set verbatim. `Emit.staticFnTypars` is the seed.
- **C2 (provider).** `ClosureMember` DU (`Ctor | CaptureField i |
  Invoke`) on `ICodegenProvider`. `ClrProvider` carries a third
  ambient (`closureTyparRoots` + `closureTyparLeaf`) chained into
  `ambientTyparLeaf = methodTyparLeaf || typeTyparLeaf ||
  closureTyparLeaf`. Public surface: `RegisterClosure`,
  `GenericClosureTypeSpec`, `GenericClosureMemberRef`,
  `GenericCaptureFieldSignature`, `SetClosureTypars` /
  `ClearClosureTypars`.
- **C3 (codegen).** `Codegen.fs`'s closure loop predicts each
  generic closure's `TypeDefinition` handle (row `2 + interfaceCount
  + unionCount + recordCount + i`), registers it with the provider,
  installs `SetClosureTypars` around field / ctor / Invoke /
  `FunInterfaceSpec` emissions, populates `captureFields` with
  `MemberRef`s on the closure's self-`TypeSpec`, adds `GenericParam`
  rows to the shared sort buffer, and lets `Emit.fs`'s `TExpr.Lambda`
  arm mint the construction-site `Newobj` target through
  `env.Provider.GenericClosureMemberRef`. Monomorphic closures keep
  the prior `Def`-token path unchanged.

### Deferred follow-ups

These three items were noted in the original sprint as gaps but
explicitly out of scope for the F3.2 work:

#### Top-level escaped generic-value gap

`let mkConst x = fun () -> x; let always10 = mkConst 10` — the
direct-form source from the original ptest — doesn't fit C1's
static-fn-resident-closure model: `peelLambda` peels both lambdas,
escape analysis flags partial application (`mkConst 10` has 1 arg vs
arity 2), and the closure lands at top-level scope where
`discoverClosures` assigns `Typars = []`. The runtime then hits
`cannot encode SemType: TyVar` on the unbound `'a`.

Fixing this requires one of (all sizeable on their own):

1. **Preserve declared arity through the front-end** — add an explicit
   arity field on bindings; `peelLambda` respects the syntactic form
   (`let f x = fun y -> …` is arity 1, not arity 2). Touches TAST +
   every consumer.
2. **Partial-application closure synthesis** — a new pass that wraps
   under-applied saturated chunks in synthetic generic closures,
   mirroring what F# does at the IL level. New architecture.
3. **Chained-application escape analysis** — recognize `mkConst 10 ()`
   as saturated and stop flagging `mkConst` as escaping. Smaller in
   isolation but interacts subtly with other escape semantics.

The C1-style indirection `let mkConst x = let f = fun () -> x in f`
keeps `mkConst` as a 1-arg static method and exercises the canonical
generic-closure case the sprint was designed against — that variant
runs end-to-end today.

#### First-class generic closure values across method boundaries (C4)

A `Vesper.Fun\`2` is monomorphic at the IL level — `Fun<int, int>`
and `Fun<string, string>` are distinct `TypeSpec`s, so a value-level
"generic closure" passed as a function argument would need rank-2
polymorphism (the receiver can't `Invoke` it at a type it doesn't
know). The closure-synthesis sprint covers closures constructed and
consumed in a single generic static method's scope, which is the
bulk of user code. Cross-package generic closures (a referenced
assembly exporting a `Fun<…>`-bearing public value whose underlying
closure type is generic) sit on the same axis — also deferred.

Concretely, that would need:

- Reading the closure type out of the referenced assembly's
  metadata (the symbol provider stack — `ReferencedProject.wrap`).
- A `ClrProvider.externalClosureRef` (sibling of `externalClassRef` /
  `externalRecordRef`) that mints a TypeRef + `MemberRef` against the
  referenced metadata.

Forward-compatibility concern: user-facing closures aren't published
as named library exports today; only static *methods* are. So this is
a future need, not a near-term one.

#### Local generic closures unrelated to the enclosing method's typars

`let f () : 'a -> 'a = fun y -> y` declares a closure with its own
free `'a` that doesn't come from any enclosing method's typar set.
Today this can't happen at the front-end level (the value restriction
makes the natural surface "doesn't compile"). Revisit if / when
generic locals land.

#### Targeted Ref + generic-closure intersection test

A captured `let mutable` of typar type goes through both the F2 cell
promotion ([`records-architecture.md`](records-architecture.md)) and the C3 generic-closure synthesis.
The mechanism is exercised indirectly (a `Vesper.Ref<'T>` capture
flows through the same `encodeType` path that the C3 tests pin), but
no explicit `let mkCell x = let mutable n = x; fun () -> n <- n; n`
test exists yet. Small additive test — add when the surrounding
syntax (the trivial `n <- n; n` body in particular) types cleanly in
Vesper.

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

The classification this widening consumes — *which* closures are
stack-only — is the escape lattice aligned to Roslyn's ref-safe-context
(RS1: `ReturnOnly` tier), a second representation axis for the
boxing/containment channels (a stack closure is illegal the moment it is
upcast to `Fun<_,_>`, boxed, or stored in a `ValueTuple`/record/array —
independent of lifetime — RS2), and a `Repr: ClosureRepr` field on
`Emit.Closure` carrying the `Stack | Heap` verdict to this pass (RS3).

## Out of scope

- Higher-arity `Fun<...>` overloads as a perf hack (avoiding curried
  `Fun<a, Fun<b, c>>`). v1 emits curried chains; flattening is a
  follow-up codegen opt.
- Optimised currying / `OptimizedClosures.FSharpFunc` parity. v1's
  Fun-constraint mode subsumes the same wins where it applies; the
  BCL `OptimizedClosures` shim is a compat concern, not a perf one,
  and falls out of the FSharp.Core removal trajectory.
- Cross-assembly Fun shapes for libraries shipped against BCL
  `FSharpFunc` (i.e., `fsc`-compiled DLLs). Handled by
  [`fsharp-compat-plan`](fsharp-compat-plan.md) — its dual
  `FSharpFunc` + `Vesper.Fun` closures and boundary adapters cover the
  interop case.

## Cross-references

- [front-end-gaps-plan](front-end-gaps-plan.md) §C — the `inline`
  work, which this plan depends on for full devirtualisation.
- [regions-plan](regions-plan.md) — escape analysis, prerequisite
  for the `allows ref struct` extension.
- [fsi-target-brainstorm](fsi-target-brainstorm.md) — where the
  `Fun<_, _>` interface lives (minimum-requirements `.fsi`) and
  the per-target FSharp.Core split that lets the lib's `FSharpFunc`
  implement it.
- [fsharp-compat-plan](fsharp-compat-plan.md) — the deferred
  `--fsharp-compat` flag: dual `FSharpFunc`+`Vesper.Fun` closures and
  boundary adapters, making the §First-pass-shape bridge concrete for
  interop with `fsc`-compiled DLLs.
