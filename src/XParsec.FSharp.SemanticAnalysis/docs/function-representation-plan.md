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
region analysis ([`Passes/Regions.fs`](../Passes/Regions.fs)) proves the value
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
Algorithm J keeps working as-is; the SRTP / IWSAM discharge sites keep
firing as today. No new constraint plumbing in the unifier.

Rationale: making every arrow-introduction a fresh typar with an
`Fun` bound forces the unifier to handle "interface-bounded typars
unify structurally with each other and with concrete impls" everywhere.
Doable, but you pay it everywhere for value you only need at codegen.

### Codegen layer (new)

At the IL-emission boundary, each parameter of arrow type at a generic
method becomes a fresh `<TF> where TF : Fun<a, b>` in the IL signature;
every application of that parameter rewrites to a constrained
`callvirt TF.Invoke`. Concrete (non-parameter) function types lower to
`Vesper.Fun<a, b>` itself.

This is where the alias lives. The TAST node `TExpr.App` is the trigger:
its `fn` is either (a) a `TExpr.External` or `TExpr.Var` referring to a
generic typar, in which case the call goes through the Fun constraint,
or (b) a concrete value, in which case the call goes through the
chosen impl directly.

#### Blocker: the synthesised typar is contract-visible

This lowering is **not** a private codegen decision, and that is what has kept
it unimplemented. A consumer derives a callee's IL shape from the parsed
*source contract*, never from the callee's metadata:

- `ReferencedProject` resolves a package to `manifest.<target>.toml` and parses
  its `[core]` `.fsi` files into the symbol provider.
- `CodegenSymbols.TryLookupOpenSignature:85-103` builds `CodegenOpenSignature`
  from that symbol's `Scheme` and `Constraints`.
- `ClrRecipes.fs:322-356` mints the `MethodSpec` from the resulting
  `MethodTyparArity` + `Constraints`.

Nothing reads the referenced PE. So a backend that privately synthesised `TF`
would emit `map<TF,'T,'U,…>(TF, …)` while every consumer computed arity and
parameter types from `('a -> 'b) -> …` and minted a `MemberRef` blob for a
method that does not exist — a missing-method fault at run time, not a
compile error. That is the worst available failure shape for what is supposed
to be a representation choice.

Reading the PE instead is closed by design, not omission: the contract is
deliberately target-neutral ([publishing-format-plan](publishing-format-plan.md))
because a JS target has no PE to read.

The case that already works proves the rule. `StructSeqTests:1031` compiles a
driver against the separately built `Vesper.Seq` package and its source lambdas
stay value-structs across the boundary — because `struct-seq.fsi:73-76`
*declares* `'TFunc :> Fun<'T,'U>`. The constrained form is in the contract, so
both sides derive the same shape with no shared secret. That is the entire
reason the hand-written form exists.

Nothing else is missing. `matchInstantiationPartial` + `solvePhantomTypars`
recover the instantiation, `constrained. callvirt` dispatch works, the closure
structs are emitted. The lowering is blocked purely on being an ABI change.

##### Routes

1. **Assembly-internal only** — apply it to functions appearing in no `.fsi`,
   where producer and consumer are the same compilation and there is no
   contract to disagree with. Unblocked today, and it covers user code, which
   is where the closure allocations are. **This is the subset to build first.**
2. **A published canonicalisation rule** — the `.fsi` keeps `('a -> 'b)` and
   both sides apply an identical derivation. Acceptable in principle, but it
   makes the rule part of the package format: versioned with it, and binding on
   every backend that reads a contract. **Wants deliberate design before anyone
   attempts it**, not an incremental slide out of route 1.
3. **Read the referenced PE.** Rejected — it contradicts the target-neutral
   contract.

Two restrictions survive whichever route is taken, because they are properties
of the target rather than of the contract:

- A virtual / abstract / interface member cannot gain typars; an override's
  signature must match its slot's generic arity exactly.
- A function used as a first-class value has no typar to instantiate.

Both fall back to the interface representation, which under §Retype by
substitution costs one `box` and nothing else.

## Landed shape: value-struct in the constrained slot, heap elsewhere

Closures implement **`Vesper.Fun<'A, 'B>`** (curried, `Invoke(arg)`) or
**`Vesper.Fun<'A, 'B, 'C>`** (flat arity-2, `Invoke(a, b)` — the two are one name
overloaded by generic arity, CLR `Fun`2`/`Fun`3`) — Vesper-native
interfaces in the [minimum-requirements `.fsi`](fsi-target-brainstorm.md),
load-bearing for every function call in IL so not opt-in. No
`Microsoft.FSharp.Core.FSharpFunc` dependency: Vesper.Core ships its own arrow
representation and the trajectory is to remove `FSharp.Core` entirely. Cross-assembly
interop with `fsc`-compiled DLLs is the only remaining FSharpFunc concern, handled
separately by [`fsharp-compat-plan`](fsharp-compat-plan.md).

The original plan called for reference-type closures in v1 with the struct shape
deferred "until generics work correctly end-to-end." That has been **superseded** —
the zero-alloc value-struct shape landed (rung-4). There are now two representations,
chosen by where the closure lands:

- **Zero-alloc value-struct** — a source lambda passed to a **constrained
  `'TF :> Fun<a,b>` / `'TF :> Fun<a,b,c>` method-typar slot** lowers to a readonly
  `System.ValueType` struct: captureless → `initobj` (M1); capturing → captures stored
  by value into struct fields via a value-type ctor (M2), a mutable capture promoted to
  a heap ref-cell captured by value; a saturated 2-arg lambda → a flat `Fun<a,b,c>` struct
  with one `Invoke(a,b)` (M3). Dispatch is `constrained. !TF callvirt` — JIT
  devirtualises `Invoke`, **no box, no heap**. The trigger is a node-keyed *fun-arity
  verdict* (`FunVerdict`, recorded at the application site by
  `Passes/Unification/InferApp.fs`, read by `EmitClosures.collectStackLambdaArgs`); the
  closure is given a synthetic project-local value-type `FrozenType`
  (`RegisterStackClosureValueType`) so the call site can instantiate `!TF` with the
  struct type. **Gated to anonymous, monomorphic** lambdas on such a slot — the narrow
  blast radius the design chose.
- **Reference-type heap closure** — everything else (stored as a concrete
  `Fun<_,_>`-typed value, returned, passed to a non-generic param, or generic). A
  non-capturing monomorphic heap closure is cached as a **static-readonly singleton**
  (`ldsfld`, one alloc per closure type — fsc's no-capture caching); capturing and
  generic closures `newobj` per construction.

The `Vesper.Seq` `map`/`fold` flip proves the value-struct pipeline end-to-end: `map`
takes `'TFunc :> Fun<'T,'U>`, `fold` takes `'TFunc :> Fun<'State,'T,'State>`, and the
hot loop dispatches `constrained.` with no box.

### Not yet landed

The **general** lambda→value-struct lowering — emitting *every* escape-free
(`ClosureRepr.Stack`) lambda as a value-struct regardless of whether it lands on a
constrained slot, plus canonicalisation and `curryFun`/`flatten` adapter insertion at
the boundaries — is the remaining pass. Today value-structs are reached only through
the constrained-slot path above; a closure stored or passed as a plain `Fun<_,_>`
interface value still heaps. The .NET 9 `allows ref struct` story is later still — see
§Region / ref-struct extension.

## Retype by substitution

### The invariant

Two representations carry a function value: the concrete closure type (`<closure>$N`, a
value-struct where the slot admits one) and `Vesper.Fun`k`` itself. **Both are correct.**
Lowering to the interface is not a failure mode — it is what a slot that cannot hold the
concrete type asks for, and there are slots that genuinely cannot: a parameter declared
`f: 'a -> 'b` rather than `'TF :> Fun<'a,'b>`, a stored field of function type, a return
position (§Return positions), a value that must outlive the frame that minted it.

So the contract is:

> Choosing between the two representations is a **representation** decision and never a
> semantic one. Widening a value-struct closure to `Fun`k`` costs one `box` and nothing
> else. It must never produce a diagnostic, a `failwith`, a wrong instantiation, or a
> confusion between two function values that happen to share a frozen type.

Today the third clause does not hold. `ClosureVerdictRewrite.record` throws on
`zip s1 s2` — two same-typed arguments bound to different closures — and the throw is a
property of how the substitution is derived, not of the shape being unsupportable.

### Why the current shape cannot hold it

The type checker accepts a structural `TyFun(a,b)` as a subtype of `Fun<a,b>`
(`Passes/Unification/Subsume.fs:205-211`, `Engine.fs:694`), so `map f src` freezes as
`MapSeq<…, (int -> int), …>`. That type is legal; it just spells the boxing
representation. The concrete closure is known only at the call site, and three separate
sites now re-derive it, each its own way:

| site | how it recovers the closure | exact? |
| --- | --- | --- |
| `EmitCall.fs:93-105` (external callee) | rebuilds a synthetic curried type from actual argument types, substituting by node | yes |
| `EmitCall.fs:166-176` (project-local static fn) | overrides `instArr` slots by node | yes |
| `ClosureVerdictRewrite` (module-value slot + `App` result types) | typar POSITION (`FunVerdict.ResultTyparPos`) plus a lockstep old/new nominal diff walk | no |

The first two are exact because they key on the argument node. The third re-derives, by
structurally matching types the callee's scheme already determined, a fact the other two
had exactly — which is what forces `nestedSubst`, `recordNominalDiff`, and the collision
`failwithf`. `EmitCall.fs:90-92` names the root in as many words: the instantiation "is
stale once an argument became a value-struct closure."

One fact, three derivations, two of which agree. The fix is to stop deriving it and make
the expression's own type true.

### The pass

One bottom-up walk computing a **representation type** `R(e)` per node, replacing
`ClosureVerdictRewrite` entirely.

- **`Lambda` with a value-struct verdict** — `R` is its `<closure>$N` type, straight out
  of `closureValueTypeByNode`. This is the only place a new type is introduced; every
  other rule propagates.
- **`App` chain** — peel with `collectAppChain`, take the callee's declared scheme, and
  run the instantiation the emitters already run: `TastLower.matchInstantiationPartial`
  over the declared parameter types against the arguments' `R`, then
  `TastLower.solvePhantomTypars` for the bound-only typars. `R(app)` is the declared
  result under that instantiation. Scheme sources: `plan.StaticFns` (`Params`,
  `ResultTy`, `Constraints`, `plan.StaticFnTypars`) for a project-local static fn;
  `symbols.TryLookupOpenSignature` (`Signature`, `MethodTyparArity`, `Constraints`) for
  an external one; the declared ctor / member signature for a construction or member
  call.
- **`Var` of a retyped binding** — that binding's `R`.
- **`FieldGet`** — the field's declared type instantiated by `R(objArg)`'s arguments, the
  operation `Assembler.enumeratorOf` already performs via `substituteDeclaring`. `h.F`
  needs no association list: `val F: 'TFunc` under `R(h) = MapSeq<…, <closure>$0, …>`
  *is* the struct.
- **Everything else** — its own type with children's `R` substituted.

`ResultTyparPos` then has no reader: the position a `'TFunc` occupies falls out of the
callee's declared result. `FunVerdict` keeps only `Arity`, which discovery needs to peel
a flat closure.

The one new primitive is a method-axis substituter — `FTTypar(Method, i)` under a
recovered instantiation — as the sibling of `FrozenTypeBridge.substituteDeclaring:166`,
which walks the declaring axis and rejects a method typar outright. Nothing else in the
codebase substitutes the method axis into a frozen template today; the emitters go
straight from `matchInstantiationPartial` to a `MethodSpec` without ever forming the
substituted result type, which is exactly the intermediate this pass needs and they
discard. Going the other way, `ClosureVerdictRewrite`'s argument-vector rewriting
(`TastLower.Nominal`, `tryNominal`, `ofNominal`, `mapFrozenArgs`) loses its only readers
and is deleted with the file.

### Widening

A value-struct `R` meeting a slot whose instantiated declared type is the interface —
a plain `Fun<_,_>` parameter, a function-typed field, a return position, any slot the
matcher did not resolve to a `Fun`-bounded typar — gets an explicit `Upcast` node to the
interface type, and `R` at that boundary is the interface.

This reuses the convention already in place: widening is a TAST `Upcast` inserted
upstream, and `EmitIntrinsic.buildUpcast:134-145` boxes a value-type source. No new
coercion path, no emitter special case, and the `box` becomes visible in the TAST where
a test can assert on it.

The multi-source case needs no rule at all. In `zip s1 s2` each argument is matched
against its **own** declared parameter, so two closures sharing a frozen type land in
different typars and cannot collide. `record`'s `failwithf` has no analogue and is
deleted rather than downgraded to a diagnostic.

### Ordering and node identity

The pass runs where `ClosureVerdictRewrite.build` runs today
(`Assembler.buildPrelude:292-298`), because the field pass at `:321` needs module-value
slot types. Every input is available there: `closureValueTypeByNode` is minted at
`:232-241`, `plan.StaticFns` is read at `:202`, and `TryLookupOpenSignature` is a symbol
lookup needing no `EmitContext`. A module value's slot type stops being a computed
rewrite and becomes `R` of its initialiser.

Two ordering facts constrain the implementation:

1. **Cross-file schemes.** Preludes run for every file before any `completeFile`
   (`Assembler.fs:316`, `:447`), and `staticMethods` is assembled per-file at `:393-406`.
   A call into a sibling file's static fn needs that file's scheme, so the scheme table
   must be collected across all files' `plan.StaticFns` in a first phase, before any
   file's retype runs.
2. **Node-keyed side tables.** `TastAccessor.retype` / `retypeWithChildren`
   (`:1007-1013`) append pool rows, so a retyped node gets a fresh `ExprId` while
   `FunVerdicts`, `ClosureValueTypeByNode`, `ClosureByNode`, `CtorHandleByNode`, and
   `CachedClosureFieldByNode` are all node-keyed. Today's `rw` re-authors only the
   affected spine precisely to avoid invalidating them. This pass retypes more nodes, so
   it must **return the remapped tables alongside the retyped tree** — it is the only
   code that knows the old→new correspondence, and a consumer that re-derives it will
   disagree with it. Keeping the spine-only discipline instead is possible but re-imports
   the constraint that made the current file fragile.

The deeper fix for (2) is a closure key that does not move under retyping, which would
also let the pass run without ordering ceremony. Out of scope here; noted because every
future rewriting pass in the backend pays this same tax.

### What this does not do

It does not lower a declared `'a -> 'b` parameter into a fresh `'TF :> Fun<'a,'b>` method
typar. That is §Codegen layer's contract, unimplemented because it is an ABI change rather
than a codegen one — see §Blocker: the synthesised typar is contract-visible. The only
constrained slots in the system are hand-written in library source
(`src/Vesper.Seq/struct-seq.clr.fs:107` for `map`, `:113` for `fold`), and
`ClrEncoder.fs:83` encodes `FTFun(a,b)` directly as `class Vesper.Fun`2<a,b>`. This pass
makes the representation of a function value follow its slot correctly and cheaply; it
does not add slots that admit the concrete type.

### Test obligations

The `StructSeqTests` pipeline cases must hold with no change in expected IL: multi-map
chain (`:977`), three-map chain (`:989`), nested temp (`:919`), and external combinators
(`:1028`) all keep asserting `constrained.` in the fold loop and no `box`. New: a
multi-source combinator (`zip s1 s2`) compiles and runs — that program throws today — and
a case pinning that a closure passed to a plain `Fun<_,_>` parameter emits exactly one
`box` and no diagnostic.

## What this does to the canonical sample

> Note: the lowering below is the **value-struct shape** — constrained calls, JIT
> devirt, no box. For a source lambda or operator passed to `fold`'s constrained
> `'TFunc :> Fun<_,_,_>` slot this is the **landed** shape (rung-4; the `Vesper.Seq`
> `map`/`fold` flip proves the zero-alloc pipeline end-to-end). A closure that instead
> escapes into a plain `Fun<_,_>` interface value still takes the reference-type heap
> path with a `callvirt` on `Invoke` — see §Landed shape.

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
   free and builds on the `inline` work (the `Inline` marker on
   `TDecl.Let` plus `Inline.inlineExpand`), which has landed.
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
result type. Each `fun …` in the source emits a sealed **reference-type**
`TypeDefinition`; that type must encode its capture-field signatures, its
ctor, and its `Invoke` against typars the enclosing method introduced. The
value-struct shape (§Landed shape) is **monomorphic-only** (`Typars = 0`
gate), so a generic closure always takes the heap path.

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
  landed — each closure carries the count of the transitive typar set it
  observes (`Closure.Typars: int`, with `DeclaringTypars: int` splitting the
  enclosing-class prefix from the method-axis suffix). No additional design.
- **Sibling closures in the same generic static fn's scope** are in
  scope and landed — `mkPair`'s pattern (three closures inheriting one
  typar each, see `CapturedMutableTests`) verifies all three emit as
  generic `TypeDef`s with one `GenericParam` row each.

### Implementation status (as of 2026-05-26)

The synthesis is implemented end-to-end against the C1 / C2 / C3 plan:

- **C1 (TAST + discovery).** `Emit.Closure` carries `Typars: int` (the count
  of enclosing typars — the §9 rework reduced the earlier `TypeVar list` to a
  count); `Emit.discoverClosures` threads `currentTypars` along the walker. A
  closure inside a generic static fn inherits that fn's typar count; a closure
  resident in `Main` / a top-level value let / a monomorphic static fn carries
  `0`; inner closures inherit the parent's count. `Emit.staticFnTypars` is the seed.
- **C2 (provider).** `ClosureMember` DU (`Ctor | CaptureField i |
  Invoke`) on `ICodegenProvider`. `ClrProvider` carries a third
  ambient (`closureTyparRoots` + `closureTyparResolver`) chained into
  `ambientTyparResolver = methodTyparResolver || typeTyparResolver ||
  closureTyparResolver`. Public surface: `RegisterClosure`,
  `GenericClosureTypeSpec`, `GenericClosureMemberRef`, and
  `EnterClosureTyparScope` / `ExitClosureTyparScope` (capture-field
  signatures go through `GenericClosureMemberRef` + `ClosureMember.CaptureField`,
  not a separate `GenericCaptureFieldSignature`).
- **C3 (codegen).** The closure loop predicts each
  generic closure's `TypeDefinition` handle (row `2 + interfaceCount
  + unionCount + recordCount + i`), registers it with the provider,
  installs `EnterClosureTyparScope` around field / ctor / Invoke /
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
polymorphism (the caller can't `Invoke` it at a type it doesn't
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
takes the same `encodeType` path that the C3 tests pin), but
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

- Flat arity beyond 4 (`Fun`6`+`), and the printf-specific consumers of the flat
  substrate. Flat arity **2/3/4 has landed** — `Fun<'A,'B,'C>`/`Fun<'A,'B,'C,'D>`/
  `Fun<'A,'B,'C,'D,'E>` (CLR `Fun`3`/`Fun`4`/`Fun`5`), the `K = 4` cap: a saturated `N`-arg
  lambda (`N ∈ 2..4`) into a `Fun<…>`-bounded slot dispatches in one flat `Invoke(a,b,c[,d])`,
  no intermediate arrows. The arity-`≤ K` closure codegen is the delivered mechanism — the
  `Emit.Closure` `Param2 voption` → `ExtraParams` list refactor, the `InvokeSignatureN` /
  `FlatFunInterfaceSpecN` arity-parametric encoders, `ClrEnv.flatFunEntity`, and the closure
  peel that takes up to `K` boundVars into the flat prefix. The flat interfaces **overload the
  curried `Fun<'A,'B>` by generic arity** (CLR `Fun`(k+1)` vs `Fun`2`, no interface-inheritance
  bridge) — native k-arg `Invoke` value-structs, not the `curryFun`/`flatten` runtime adapters
  (those exist in `Vesper.Core/core-types` for boundary adaptation and are emitted only for
  Phase-B within-chunk partial application). Resolvable because the project-local class registry
  is arity-keyed (the former `arity-overloaded-classes-design` epic).
  Still driven by the printf epic ([printf-partial-app-plan](printf-partial-app-plan.md)): the
  printf **gate** that mints these structs from an under-applied literal (step 4), and the
  `n > K` **curried-residual codegen** — a flat-`K` prefix `Invoke` that captures its args and
  returns a curried closure for the tail (option A; step 5, greenfield). The greedy flat→flat
  chain (option B, `Fun<_,_,_,_,Fun<_,_,_>>`) is a deferred non-breaking promotion.
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

- `Inline.fs` — the `inlineExpand` helper this plan depends on for
  full devirtualisation.
- [`Passes/Regions.fs`](../Passes/Regions.fs) — escape analysis, prerequisite
  for the `allows ref struct` extension.
- [fsi-target-brainstorm](fsi-target-brainstorm.md) — where the
  `Fun<_, _>` interface lives (minimum-requirements `.fsi`) and
  the per-target FSharp.Core split that lets the lib's `FSharpFunc`
  implement it.
- [fsharp-compat-plan](fsharp-compat-plan.md) — the deferred
  `--fsharp-compat` flag: dual `FSharpFunc`+`Vesper.Fun` closures and
  boundary adapters, making the §First-pass-shape bridge concrete for
  interop with `fsc`-compiled DLLs.
