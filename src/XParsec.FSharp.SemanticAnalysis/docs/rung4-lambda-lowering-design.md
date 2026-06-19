# Design: general lambda → flat-closure lowering pass (rung 4)

**Deliberate design doc — written BEFORE any implementation, the way `Fun2` was
designed (`rung3-handoff.md` §2).** *Ephemeral plan doc* per
[[feedback_plan_docs_ephemeral]] — scoped to exactly one body of work (making
ordinary source lambdas ride the zero-alloc struct-`Seq` pipeline). Delete this
file (and every comment that references it) once the work below lands; the CODE +
isolation tests are the durable record, and `[[project_seq_struct_pipeline_ladder]]`
is the authoritative landed-state memory.

This doc is READ-ONLY analysis + a staged plan. It names real `file:line`
touch-points. Where a touch-point could not be found, it is flagged as an **open
question**, not invented.

---

## 0. The gap, precisely

`rung3-handoff.md` §3 closed the *capability*: a `[<Struct>]` closure that the
user writes by hand (`StructSeqTests.fs:1022` `AddN : Fun<int,int>`, `:1028`
`SumAcc : Fun2<int,int,int>`) dispatches through `StructSeq.map`/`fold`'s
`'TFunc :> Fun<…>` typars via `constrained. !TFunc callvirt`, with no box — proven
by the "wall iv" test (`StructSeqTests.fs:978`).

The remaining work is *ergonomics*: today

```fsharp
StructSeq.map (fun x -> x + 1) s        // a SOURCE lambda, not a hand-written struct
```

cannot ride the zero-alloc path, for two compounding reasons:

1. **Emission shape.** A source `fun x -> x+1` is discovered as a `Closure`
   (`EmitClosures.discoverClosures`, `EmitClosures.fs:664`) and emitted as a
   **reference-type** `System.Object` subclass implementing `Vesper.Fun\`2`
   (`Assembler.fs:519-525`: `BaseType = provider.ObjectType`,
   `Interfaces = [ifaceSpec]`). It allocates on the heap and dispatches via
   `callvirt Vesper.Fun\`2::Invoke` (`ClrRecipes.fs:96 funInvokeRef`,
   `:119 EmitInvoke`). The `ClosureRepr.Stack` verdict that Regions already
   computes is **inert** — emission never reads `c.Repr` (`EmitTypes.fs:54`
   docstring says so explicitly: *"Inert in v1 — emission ignores it"*).

2. **Representation threading.** `StructSeq.map`'s `'TFunc` is a single concrete
   type chosen at the call site (the library threads it BY HAND — `struct-seq.fsi:92`).
   A source lambda has *no nameable type* the user can pass; the pass must synthesise
   the closure's value-struct type, infer it INTO the `'TFunc` slot, and decide a
   single representation (flat `Fun2` vs curried `Fun`) per function-typed slot.

The pass is two cooperating halves — **emit `Stack` closures as value-structs**
(§1, pure codegen) and **canonicalize the function representation at combinator
boundaries + insert flat↔curried adapters** (§2–3, a lowering decision). They are
separable: §1 makes a `Stack`-verdict lambda *become* a struct implementing `Fun`;
§2 makes the combinator call site *accept* it as `'TFunc`.

---

## 1. Flipping `ClosureRepr.Stack` from inert to an emitted value-struct

### 1.1 Where the heap shape is forced (the inert flip point)

The single emission surface for a closure's `TypeDefinition` is
`Assembler.PrepareClosures()` (`Assembler.fs:445-525`). For every `Closure c` it:

- builds the ctor body (`Assembler.fs:478-482`, `Emit.buildClosureCtor`,
  `Emit.fs:350`) which **chains `provider.ObjectCtorRef`** then `stfld`s captures;
- builds `Invoke` (`Assembler.fs:484-485`, `Emit.buildClosureInvoke`);
- emits the ctor + `Invoke` signatures (`provider.ClosureCtorSignature`,
  `provider.InvokeSignature`, `Assembler.fs:490,500`);
- registers the type row extras (`Assembler.fs:519-525`):
  `Interfaces = [ provider.FunInterfaceSpec(...) ]`,
  **`BaseType = provider.ObjectType`**.

The actual `TypeDefinition` row is then written in the
`TypeSlotKind.Closure` arm (`Assembler.fs:743-766`): it adds `closureAttrs`
(`Assembler.fs:283`), the `BaseType` from `typeRowExtras`, and the interface impls.
The slot itself is created in `Layout.fs:837-843` (`Kind = TypeSlotKind.Closure`,
`MetaName = SymbolKeyOps.arityName c.Name c.Typars`).

`c.Repr` (`EmitTypes.fs:54`) is read **nowhere** in this path. That is the inert
flip point: every `Stack`-verdict closure must take a parallel value-type
emission instead of the `Object`-subclass one.

### 1.2 What a `Stack` closure as a value-struct requires

A value-struct closure mirrors the hand-written `AddN` (`StructSeqTests.fs:1022`):

| aspect | heap (today) | stack (new) |
|---|---|---|
| base type | `System.Object` (`Assembler.fs:523`) | `System.ValueType` — `[<Struct>]` codegen already emits structs ([[project_struct_codegen]]); reuse its base/attr choice |
| ctor | chains `Object::.ctor`, then `stfld` (`Emit.buildClosureCtor`, `Emit.fs:350`) | **no base-ctor chain** — value types don't chain (`Emit.fs:365-368` `buildValueTypeCtor` precedent); `ldarg.0` is a managed pointer, `stfld` captures into fields by address |
| fields = captures | `Captures` (`EmitTypes.fs:27`) | identical — same `freeVars` order |
| `Invoke` | interface method on a class | interface method on a struct (a non-virtual `MethodDef` + `MethodImpl` to the `Fun\`2` slot — the SAME wiring the struct-iface fixtures already prove, `StructSeqTests.fs:293,336`) |
| dispatch at use site | `callvirt Fun\`2::Invoke` (`ClrRecipes.fs:119`) | `constrained. !TF callvirt` — already emitted for hand-threaded typars (`StructSeqTests.fs:85` asserts the prefix, `:88` asserts no box) |
| construction | `newobj` (`EmitConstruct.buildLambda`, `EmitConstruct.fs:288-332`) | `initobj` + field stores, OR a value-type ctor `call` — same choice `[<Struct>]` `new()` already makes |
| `$type` / branding | none (CLR uses the nominal type) | none — no branding needed on CLR; the JS backend's `$type` brand ([[project_js_union_layout_compositional]]) is irrelevant to this CLR pass |

**Minimal change surface for §1 (codegen only):**

1. `Assembler.PrepareClosures` (`Assembler.fs:445`): branch on `c.Repr`. For
   `Stack`, emit the value-type ctor (no `ObjectCtorRef` chain) and set
   `BaseType = <ValueType>` + the `[<Struct>]` type attrs in the `typeRowExtras` /
   `TypeSlotKind.Closure` arm (`Assembler.fs:519-525,743-766`). The `Invoke` /
   ctor *signatures* and bodies are otherwise unchanged.
2. `EmitConstruct.buildLambda` (`EmitConstruct.fs:288`): for a `Stack` closure,
   emit value-type construction (`initobj`/value-ctor `call` to a local address,
   leaving the value — not a heap reference — on the stack) instead of `newobj`.
3. The application site: a `Fun`-typed *value* application currently goes through
   `EmitInvoke`/`funInvokeRef` (`ClrRecipes.fs:96,119`, reference dispatch). When
   the value's static type is the synthesised struct (or a `'TF` typar bounded by
   `Fun`), dispatch must be `constrained. !TF callvirt` — which is §2's job
   (the application has to *see* the struct type, which only happens once the
   combinator slot is a constrained typar).

**Decision still to make (§1):** whether a `Stack` closure is *only ever* emitted
as a struct when it actually flows into a constrained-typar slot, or
unconditionally whenever the Regions verdict is `Stack`. Emitting it
unconditionally means a frame-local lambda *not* passed to a struct combinator
also becomes a value struct — correct and non-allocating, but its application sites
(ordinary `f x`) must then ALSO switch from `callvirt Fun::Invoke` to a by-address
`constrained.` call, widening the blast radius. **Recommended:** gate §1 emission
on the conjunction of `Stack` verdict AND the closure flowing into a constrained-
typar combinator slot (the §2 decision), so the first milestone touches only the
struct-`Seq` path. Unconditional `Stack`-struct emission is a follow-up.

### 1.3 Prerequisite — fieldless struct (§3.1 gap 2)

A captureless `fun x -> x+1` has **zero** capture fields. The ideal struct shape
is a fieldless `[<Struct>]` whose body is only the `Fun` interface impl — which
**trips parse recovery today** (`rung3-handoff.md` §3.1 #2; every fixture dodges it
with a dummy `val`, e.g. `StructSeqTests.fs:1024` `val N : int`, `:930` comment).
The synthesised closure type the pass mints is internal metadata, NOT source, so it
does **not** go through the parser — meaning this gap may **not** block the
synthesised path (open question, must verify the synthesised type-decl path
bypasses the parser). But any *test* that hand-writes a captureless struct closure
to isolate the pass hits it. Fix the parser gap first so isolation tests can
express the captureless shape directly.

---

## 2. Function-representation canonicalization + adapter insertion

### 2.1 The decision: ONE representation per function-typed slot

`function-representation-plan.md` §"Codegen layer (new)" frames the rule: at a
generic combinator, an arrow-typed parameter becomes a fresh
`<TF> where TF : Fun<a,b>` (or `Fun2<a,b,c>` for a saturated 2-arg site) typar in
the IL signature, and every application of that parameter lowers to
`constrained. !TF callvirt Invoke`. The library does this BY HAND already
(`struct-seq.fsi:92-96` `map` rides `'TFunc :> Fun<'T,'U>`; `:102-109` `fold` rides
`'TFunc :> Fun2<'State,'T,'State>`). The pass must reproduce it automatically when
the combinator's *source* still says `f: 'A -> 'B`.

The representation (flat vs curried, and which arity) is chosen from the
**saturated-application count** at the slot's use sites inside the combinator body:

- A function param applied to exactly its full arity in one spine → flat `FunN`
  (capped at `Fun2`, §3).
- A function param partially applied (or escaping as a value) → curried `Fun<,>`;
  a flat value reaching a curried slot is adapted with `curryFun`.
- A curried value reaching a flat slot is adapted with `flatten`.

### 2.2 Where the decision is made — the layering question

`function-representation-plan.md` §"Two layers" is emphatic: the `Fun` alias is a
**codegen contract, not an inference rewrite** — `SemType.TyFun` stays the
structural arrow the unifier sees (`Regions.fs:182` treats `TyFun` as an
allocation; the unifier never sees a `Fun`-bounded typar). So the canonicalization
must live at or just before the codegen boundary, reading frozen types
(`FTFun`, `EmitClosures.fs:222` `FTFun(a,b)`), NOT in `Passes/Unification`.

**This is the central architectural decision still open.** Two candidate homes:

- **(A) A new lowering pass over the frozen TAST** (a `TastLower`-style rewrite,
  sibling to `bridgeStaticFnEscapes` in `EmitClosures.fs:421`), run after Freeze /
  Regions and before `discoverClosures`. It would (i) identify combinator params of
  arrow type whose body applies them saturated, (ii) retype those params to a
  constrained typar, (iii) rewrite the matching argument's closure to the value-
  struct repr, and (iv) splice `curryFun`/`flatten` `App` nodes at mismatch sites.
  This mirrors the existing eta-bridge precedent exactly (a frozen-decl-list →
  frozen-decl-list rewrite, `EmitClosures.fs:487-493`).
- **(B) Inline at emission** in `EmitCall`/`EmitConstruct`. Rejected: the
  representation choice is non-local (it spans a param decl, every use site in the
  body, AND the argument at the call site), so a per-node emission decision can't
  see enough.

**Recommended: (A).** It keeps the unifier untouched (honouring the §2 layering
lock), reuses the `bridgeStaticFnEscapes` pattern, and produces a frozen tree whose
shape `discoverClosures` and `EmitCall` then emit with NO new emission-time
branching beyond §1's `c.Repr` check.

### 2.3 Adapter insertion — `curryFun` / `flatten` are ordinary-code targets

`rung3-handoff.md` §2 LOCKS that `curryFun`/`flatten` are *codegen-known
canonical-name lowering targets* whose bodies are ordinary Vesper.Core code
(`core-types.fs:31-33`), **not magic IL**. **Important finding:** they are **not yet
recognized as canonical names anywhere in CLR codegen** — a grep of `EmitCall.fs`,
`Emit.fs`, and the Clr backend for `"curryFun"` / `"flatten"` / `canonName` finds
**no match**. Today they are plain library functions, `call`ed by hand
(`StructSeqTests.fs:946-947`, `core-types.fsi:46,49`).

So "lowering target" here means only: the pass **synthesises an `App` node** of
the resolved `curryFun`/`flatten` symbol around the mismatched value — an ordinary
saturated call that `collectStaticFns`/`EmitCall` already compile. No special
canonical-name machinery is needed; the pass just needs the resolved symbol for
`Vesper.FunAdapters.curryFun` / `flatten` (an external-symbol lookup against
Vesper.Core, the same provider stack `[[project_contract_demotion]]` uses).

- **Partial-application site** (a flat `Fun2` value used where only one argument is
  applied, or the value escapes): wrap in `curryFun f a` → residual `Fun<'B,'C>`
  (`core-types.fs:31`, `Curried` adapter `:13`).
- **Curried-meets-flat site** (a curried `Fun<'A,Fun<'B,'C>>` value reaching a flat
  `Fun2` slot): wrap in `flatten f` → `Fun2<'A,'B,'C>` (`core-types.fs:33`,
  `Flattened` `:17`). Note `Flattened.Invoke` already carries the §3.1-gap-1
  `let`-split workaround (`core-types.fs:25-26`).

### 2.4 The inference reality check (open question)

For `StructSeq.map (fun x -> x+1) s` to typecheck with `map`'s `'TFunc :> Fun<'T,'U>`
constraint, the front end must accept a source lambda (structural `TyFun`,
`Regions.fs:182`) as an argument to a `'TFunc :> Fun<…>` constrained param. Today
the user passes a *named* struct type (`AddN`) whose `:> Fun` is declared. Whether
the unifier already coerces a bare `TyFun` argument to a `Fun`-bounded typar — or
whether that needs a coercion at the boundary — is **not established by this
read-through** and is the single biggest inference-side open question. The §2
layering lock says we must NOT add `Fun`-constraint plumbing to the unifier; if the
coercion isn't already there, the pass-(A) rewrite must retype the *argument* (the
lambda) to the synthesised struct type so the structural arrow never reaches the
constrained slot — i.e. the argument-side rewrite and the param-side retype happen
together, keeping the unifier's view purely structural. **Milestone 0 below probes
exactly this.**

---

## 3. Arity-2 cap

LOCKED (`rung3-handoff.md` §2): flat-first with an arity cap of 2.

- **Saturated 2-arg** application of the param → flat `Fun2<'A,'B,'C>` (the single
  2-arg `Invoke`, `core-types.fsi:34`, proven `StructSeqTests.fs:1032` `SumAcc`).
- **Partial** (1-of-2) → `curryFun` to the residual `Fun<'B,'C>` (`core-types.fs:31`).
- **Arity > 2** → curried chains, UNCHANGED (today's reference-closure curried
  `Fun<a,Fun<b,c>>` shape). `Fun3`/`Fun4` are explicitly *additive when a concrete
  combinator demands one* (`function-representation-plan.md` §"Out of scope" item 1),
  never speculative.

The cap is purely a decision in the pass-(A) representation chooser: count the
saturated args in the param's largest application spine; `=2` picks `Fun2`,
`=1` picks `Fun`, `>2` leaves the curried chain.

---

## 4. Interaction with the two §3.1 prerequisite gaps

Both are being fixed in parallel; the pass depends on them as follows.

### Gap 1 — chained-receiver freeze mistype (`f.Invoke(a).Invoke(b)`)

`rung3-handoff.md` §3.1 #1: a method call on the *result* of a method call inside
an interface-impl member mis-types the member return as the inner call's result;
worked around in `core-types.fs:25-26` (`Flattened.Invoke` `let`-split). The
`flatten` adapter (§2.3) is exactly such a chain, so the workaround is currently
load-bearing for the curried→flat path. **Dependency:** the pass can SHIP on the
`let`-split workaround (it already compiles), but the genuine front-end inferencer
fix should land before any *synthesised* adapter body or struct-seq body needs an
un-split `f.x(a).y(b)` chain. Design choice that depends on it: whether the pass
may emit adapter bodies / `Invoke` bodies containing raw chained calls (only after
the fix) or must keep the `let`-split discipline (until then).

### Gap 2 — fieldless `[<Struct>]` interface-only parse recovery

Covered in §1.3. A captureless source lambda is the *ideal* `Stack` shape, so this
bites the pass directly **if** the synthesised closure type routes through the
parser (open question — likely it does NOT, since the type is internal metadata).
Regardless, fix it so isolation tests can hand-write the captureless struct closure.
Design choice that depends on it: whether Milestone 1's isolation test can use a
zero-field struct closure (after the fix) or must carry a dummy field (as
`StructSeqTests.fs:930,1024` do today).

---

## 5. LOCKED decisions (do NOT relitigate) + open questions

### 5.1 Locked (from `rung3-handoff.md` §2 / `function-representation-plan.md`)

1. **Distinct `FunN` names**, NOT arity-overloaded. `Fun` and `Fun2` are separate
   nominal types because `Types.Class` is bare-name-keyed (`rung3-handoff.md` §2,
   §4). Arity-overloaded local classes are a *separate deferred epic*.
2. **`Fun2` does NOT inherit `Fun<'A,Fun<'B,'C>>`.** Adaptation is explicit and
   reference-typed (`curryFun`/`flatten`), never a subtype relation
   (`rung3-handoff.md` §2; `core-types.fsi:38-40` docstring confirms "no
   interface-inheritance bridge").
3. **Flat-first with arity cap 2.** Curried `Fun<,>` stays canonical for partial
   application and arity-over-cap (`rung3-handoff.md` §2; §3 here).
4. **`curryFun`/`flatten` are ordinary-code lowering targets, not magic IL**
   (`rung3-handoff.md` §2; their bodies are `core-types.fs:31-33`). The pass
   synthesises ordinary `App` nodes around mismatches.
5. **The unifier stays structural** (`SemType.TyFun` unchanged); the `Fun` alias is
   a codegen contract only (`function-representation-plan.md` §"Two layers").
6. **The struct-seq library threads `'TFunc` by hand** — that needs no new pass,
   only the already-landed constrained dispatch (`rung3-handoff.md` §2;
   `struct-seq.fsi`). The pass is purely about *source-lambda* arguments.

### 5.2 Top open questions / decisions still to make

1. **Does the unifier already coerce a structural `TyFun` argument into a
   `'TFunc :> Fun<…>` constrained slot?** (§2.4) If not, the pass must retype the
   argument lambda to the synthesised struct type in lockstep with the param
   retype, so the constrained slot never sees a bare arrow — keeping the §2
   layering lock intact. This is the highest-risk unknown; Milestone 0 probes it.
2. **Where does the representation-choice pass live, and does pass-(A) (a frozen
   TAST rewrite sibling to `bridgeStaticFnEscapes`, `EmitClosures.fs:421`) have
   enough information** — specifically, can it see each combinator param's
   saturated-application count from the frozen body alone, and can it mint a fresh
   constrained-typar param + retype every use site without re-running inference?
3. **Gating of `Stack`-struct emission** (§1.2): conditional on flowing into a
   constrained combinator slot (recommended, narrow blast radius) vs unconditional
   on the Regions `Stack` verdict (wider — every frame-local lambda's application
   sites must also flip to `constrained.`). Decide before Milestone 1.

Secondary: whether the synthesised closure type's name (`<closure>$N`,
`EmitClosures.fs:721`) needs an arity/`Fun2` discriminator; whether
`discoverClosures`' anonymous-lambda → `Heap` default (`EmitClosures.fs:716`)
needs an anonymous-`Stack` path (today only `SelfKey`-bound closures get a non-Heap
verdict — `EmitClosures.fs:710-716` — but a `StructSeq.map (fun x -> …)` argument is
an ANONYMOUS lambda, so it currently CANNOT receive a `Stack` verdict at all). This
last point is load-bearing and is folded into Milestone 1.

---

## 6. Staged implementation plan (isolation-test-driven)

Per [[feedback_systematic_tests_over_whackamole]]: each milestone is the smallest
isolation test that forces the capability, then diagnose the exact wall, fix,
iterate. Ordered by dependency and risk. All tests are `compileSource`-style CLR
fixtures in the spirit of `StructSeqTests.fs` (assert exit/output, then assert the
IL has a `constrained.` prefix `0xFE 0x16` and NO `box` `0x8C`).

> Build/test ONLY via `./claude_tools.cmd` (`rung3-handoff.md` §5) — **but another
> agent is editing the tree concurrently; do not build while that holds.**

### M0 — Inference probe: a source lambda into a `'TFunc :> Fun` slot
**Smallest test:** feed `StructSeq.map (fun x -> x+1) s` (or the inline rung-4
fixture form, `StructSeqTests.fs:1035`) and observe the *diagnostic*. Pure probe —
no fix. Determines whether §2.4's coercion exists. **Proves:** the exact wall (a
type error, a miscompile, or a clean pass that just emits a heap closure). Gates the
shape of every later milestone. Risk: highest (architectural unknown).

### M1 — Anonymous-`Stack` verdict + value-struct emission (captureless)
**Depends on:** M0; §3.1 gap 2 fixed (so the test can be captureless).
**Smallest test:** a captureless `fun x -> x+1` passed to a *hand-written* generic
combinator `let apply (f: 'TF when 'TF :> Fun<int,int>) x = f.Invoke x` — but as a
SOURCE lambda, asserting the synthesised closure type is a `[<Struct>]` (value type)
and the dispatch is `constrained.` with no `box`. **Proves:** §1 (flip
`ClosureRepr.Stack` to value-struct emission, `Assembler.fs:519-525`,
`EmitConstruct.fs:288`) AND the anonymous-`Stack` verdict path
(`EmitClosures.fs:710-716`). Risk: medium (new emission branch; struct-iface wiring
already proven `StructSeqTests.fs:293`).

### M2 — Captures: a `Stack` struct closure with one capture field
**Depends on:** M1.
**Smallest test:** `let n = 1 in StructSeq.map (fun x -> x + n) s` — the closure
captures `n`. Assert the struct has one capture field, constructs by value
(`initobj`/value-ctor, not `newobj`), and dispatches non-allocating. **Proves:** the
capture-field path (`EmitClosures.fs:730` `Captures`) works for the value-struct
ctor (`Emit.fs:350` heap ctor → value ctor `Emit.fs:365`). Risk: medium (value-ctor
field stores by address).

### M3 — Saturated-2 → `Fun2` representation choice
**Depends on:** M1 (emission), M0 (inference path).
**Smallest test:** `StructSeq.fold (fun acc x -> acc + x) 0 s` — a 2-arg source
lambda into `fold`'s `'TFunc :> Fun2<…>` slot. Assert the synthesised closure
implements `Fun2` (one 2-arg `Invoke`) and the fold loop dispatches
`constrained. !TFunc callvirt Fun2::Invoke` with no box (mirrors the hand-written
`SumAcc` proof, `StructSeqTests.fs:1073-1092`). **Proves:** §3 arity-2 cap +
flat-first choice in the pass-(A) chooser. Risk: medium.

### M4 — `curryFun` adapter at a partial-application site
**Depends on:** M3.
**Smallest test:** a flat-`Fun2` source lambda used where only one arg is applied
(forcing a residual `Fun<'B,'C>`). Assert a synthesised `call` to
`Vesper.FunAdapters.curryFun` (`core-types.fs:31`) wraps the value. **Proves:** §2.3
adapter-insertion mechanics (resolved-symbol `App` synthesis, not canonical-name
magic). Risk: low-medium (depends on the external-symbol lookup for the adapter).

### M5 — `flatten` adapter at a curried-meets-flat site
**Depends on:** M4; §3.1 gap 1 fixed (so the adapter/Invoke body may use an
un-split chain — until then ride the `core-types.fs:25` `let`-split).
**Smallest test:** a genuinely curried source value (`fun a -> fun b -> a+b`, an
arity-2 curried chain) reaching a flat `Fun2` slot. Assert a `flatten` `call`
(`core-types.fs:33`) and a correct round-trip. **Proves:** §2.3 curried→flat
adaptation. Risk: medium (interacts with §3.1 gap 1).

### M6 — End-to-end zero-alloc `ofArray |> map |> fold` from source lambdas
**Depends on:** M1–M5.
**Smallest test:** the rung-4 "wall iv" pipeline (`StructSeqTests.fs:978`) but with
`map (fun x -> x+1)` / `fold (fun acc x -> acc+x)` SOURCE lambdas instead of the
hand-written `AddN`/`SumAcc` structs. Assert identical output (`14`) AND the same
no-`box` constrained-dispatch IL assertions. **Proves:** the whole epic. Risk: low
(integration of proven parts).

### M7 — Library graduation: drop the hand-written closures from a Vesper.Seq client
**Depends on:** M6.
**Smallest test:** a `buildPackage`-gated client of `Vesper.Seq` (the strict gate,
[[reference_buildpackage_gates_on_diagnostics]]) that calls `StructSeq.map`/`fold`
with source lambdas. **Proves:** the pass survives the package path, not just inline
`compileSource`. Risk: low-medium (package path surfaces front-end gaps the lenient
path hides).

---

## 7. Cross-references

- `rung3-handoff.md` — the epic statement (§3) and locked decisions (§2).
- `brainstorm-seq-module.md` — north star (the zero-alloc struct `Seq`).
- `function-representation-plan.md` — §"Codegen layer (new)" / §"Generic closures"
  / §"Region / ref-struct extension" this pass realises.
- `get-enumerator-gaps.md` — adjacent `for…in` work (ref-struct `Dispose`,
  co-blocked with `allows ref struct`; out of scope here per `rung3-handoff.md` §4).
- Memories: `[[project_seq_struct_pipeline_ladder]]` (authoritative landed state),
  `[[project_struct_codegen]]`, `[[project_function_method_compiled_form]]`,
  `[[reference_constrained_callvirt_nonvirtual_struct]]` (constrained.callvirt is
  correct ONLY for virtual/interface methods — the struct `Invoke` must be reached
  through the `Fun` interface slot, not a direct struct `MethodDef`),
  `[[reference_buildpackage_gates_on_diagnostics]]`.
