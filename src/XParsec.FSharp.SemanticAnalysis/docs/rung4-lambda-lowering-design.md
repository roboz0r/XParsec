# Design: source-lambda → flat-closure lowering (rung 4) — M3–M7

*Ephemeral plan doc* per [[feedback_plan_docs_ephemeral]] — scoped to making ordinary
source lambdas ride the zero-alloc struct-`Seq` pipeline. Delete this file (and every
comment that references it) once M3–M7 land. The CODE + isolation tests are the durable
record; `[[project_seq_struct_pipeline_ladder]]` is the authoritative landed-state memory.

This doc supersedes `rung3-handoff.md` (folded in here, then deleted). It is READ-ONLY
analysis + a staged plan with the **same isolation-test-driven, milestone-by-milestone,
sub-agent-with-review-gate workflow** that landed Steps A/B/C (see §6).

---

## 0. Status — what landed (M0–M2), what remains (M3–M7)

The headline single-arg case **`apply (fun x -> x+1) 41`** (where `apply` takes
`'TF :> Fun<int,int>`) now compiles, runs, and lowers a source lambda to a zero-alloc
value-struct dispatched `constrained.` with no box. Landed 2026-06-20 on `vesper-seq`:

| Step | Commit | What |
|---|---|---|
| **A** front-end accept | `d1418ac` | `subsumes(TyFun(a,b), Vesper.Fun\`2<a,b>) = Subtype` — one arm in `Passes/Unification/Engine.fs`. The arrow↔`Fun` correspondence; `TyFun` stays structural elsewhere. |
| **B** singleton cache | `45d6f62` | Non-capturing monomorphic closure → `static readonly` singleton (`.cctor` + `ldsfld`). |
| **C / M1** captureless struct | `91b8a65` | Captureless source lambda → `System.ValueType` closure, `initobj`, `constrained.` devirt, no box. |
| **C / M2** capturing struct | `d7e423f` | Capturing source lambda → value-struct, by-value capture fields, value-ctor construction. |

**Key resolutions from the landing (these retire this doc's original open questions):**
- §2.4's "does the unifier coerce `TyFun` into a `Fun`-bounded slot?" — it did NOT; Step A
  *made* it (a single `subsumes` arm), so there is **no argument-retype pass-(A) rewrite** —
  the unifier accepts the arrow directly and stays structurally pure.
- §1.2's gating question — resolved to **constrained-slot-only**: `EmitClosures.collectStackLambdaArgs`
  marks an anonymous lambda a value-struct ONLY when it lands on a bare `FTTypar(Method,i)`
  param of an all-`GSimple` callee. `Closure.IsValueStruct` is the codegen trigger, split
  from the inert front-end `Repr` snapshot.
- §2.2's "where does emission live?" — value-struct emission + the call-site `!TF` override
  were done **inline** (no separate frozen-TAST pass). A closure has no `FrozenType`, so
  `ClrProvider.RegisterStackClosureValueType` mints a synthetic project-local value-type
  `FrozenType` riding the existing `encodeType` value-type arm, and `EmitCall` overrides the
  typar instantiation (`matchInstantiation` bound it to the arrow → the `Fun\`2` *interface*,
  which would force a box). Gating is a discovery pre-walk (`collectStackLambdaArgs`).
- §3.1's two prerequisite gaps (chained-receiver freeze mistype; fieldless-`[<Struct>]` parse
  recovery) were already fixed pre-A (`eed60c7`, `064f8ec`).

**Reframing for what remains:** because Step A is a `subsumes` arm and the value-struct
emission already generalises, **M3 (flat `Fun2`) is much smaller than originally feared** —
it follows the A+C template (a new `subsumes` arm for the flat-2 correspondence + the existing
value-struct emission with a 2-arg `Invoke`), NOT the big canonicalization pass. Only the
adapters (M4 `curryFun`, M5 `flatten`) genuinely require representation-mismatch detection.

---

## 1. The remaining gap, precisely

Today only a **single-arg** source lambda through a `Fun<_,_>` slot is zero-alloc. The
struct-`Seq` north star also needs:

```fsharp
StructSeq.fold (fun acc x -> acc + x) 0 s     // a 2-arg source lambda → Fun2 slot
```

`fold` rides `'TFunc :> Fun2<'State,'T,'State>` (`struct-seq.fsi:102-109`), a flat 2-arg
`Invoke(a,b)`. A source `fun acc x -> …` is a *curried* arrow `'State -> 'T -> 'State`
(`TyFun(a, TyFun(b,c))`). Two things are missing:

1. **Front-end accept of the flat-2 correspondence** — `subsumes(TyFun(a, TyFun(b,c)),
   Fun2<a,b,c>)` must discharge as `Subtype` (the M3 analog of Step A's arrow→`Fun` arm).
2. **A 2-arg value-struct `Invoke`** — the synthesised closure implements `Fun2` with one
   flat `Invoke(a,b)` whose body is the (peeled) curried lambda. The C/M1+M2 emission path
   already mints value-struct closures; M3 generalises it to a 2-param `Invoke`.

Then the *adapters* handle representation mismatch at non-saturated sites (M4/M5), and M6/M7
prove the end-to-end pipeline + library graduation.

---

## 2. Function-representation canonicalization + adapters (M3–M5)

### 2.1 ONE representation per function-typed slot

`function-representation-plan.md` §"Codegen layer (new)": at a generic combinator an
arrow-typed param becomes a `<TF> where TF : Fun<a,b>` (or `Fun2<a,b,c>` for a saturated
2-arg site) typar, and every application lowers to `constrained. !TF callvirt Invoke`. The
library does this BY HAND (`struct-seq.fsi:92-96` `map` rides `'TFunc :> Fun<'T,'U>`;
`:102-109` `fold` rides `'TFunc :> Fun2<'State,'T,'State>`). The remaining work is choosing
the representation when the combinator slot is already `Fun`/`Fun2`-typed and the *argument*
is a source lambda whose arrow arity may or may not match.

The representation (flat vs curried, which arity) is chosen from the **saturated-application
count** at the slot's use sites inside the combinator body:
- applied to exactly its full arity in one spine → flat `FunN` (capped at `Fun2`, §3);
- partially applied (or escaping as a value) → curried `Fun<,>`; a flat value reaching a
  curried slot is adapted with `curryFun`;
- a curried value reaching a flat slot is adapted with `flatten`.

### 2.2 Where the representation CHOICE lives

The unifier stays structural (`SemType.TyFun` unchanged — `function-representation-plan.md`
§"Two layers"); Step A's `subsumes` arm is the only arrow↔`Fun` touch-point and it is
read-only. So the flat-vs-curried choice + adapter insertion lives at/just before the codegen
boundary over frozen types — the **proven template** is the C/M1+M2 machinery:
- a **discovery pre-walk** (like `EmitClosures.collectStackLambdaArgs`) classifies each
  lambda-argument site by the saturated-application count of the matching slot;
- emission consumes that classification inline (the value-struct closure's `Invoke` arity,
  and any `curryFun`/`flatten` `App` the call site synthesises).

A full separate frozen-TAST rewrite pass (the original "pass-(A)") is **not required** for
M3 — M3 is a new `subsumes` arm + 2-arg value-struct emission. M4/M5 add adapter-`App`
synthesis at mismatch sites, which the discovery pre-walk can flag.

### 2.3 Adapters — `curryFun` / `flatten` are ordinary-code targets

LOCKED: `curryFun`/`flatten` are *codegen-known canonical-name lowering targets* whose bodies
are ordinary Vesper.Core code (`core-types.fs:31-33`), **not magic IL**. They are NOT yet
recognised as canonical names in CLR codegen (grep finds no `"curryFun"`/`"flatten"`/`canonName`
match); today they are plain library functions `call`ed by hand (`core-types.fsi:46,49`).

So "lowering target" means only: the pass **synthesises an `App` node** of the resolved
`curryFun`/`flatten` symbol around the mismatched value — an ordinary saturated call that
`collectStaticFns`/`EmitCall` already compile. No canonical-name machinery needed; just the
resolved symbol for `Vesper.FunAdapters.curryFun` / `flatten` (an external-symbol lookup
against Vesper.Core, the same provider stack `[[project_contract_demotion]]` uses).

- **Partial-application site** (a flat `Fun2` value used 1-of-2, or escaping): wrap in
  `curryFun f a` → residual `Fun<'B,'C>` (`core-types.fs:24`, `Curried` `:13`).
- **Curried-meets-flat site** (a curried `Fun<'A,Fun<'B,'C>>` reaching a flat `Fun2` slot):
  wrap in `flatten f` → `Fun2<'A,'B,'C>` (`core-types.fs:26`, `Flattened` `:17`). The
  `Flattened.Invoke` body is ALREADY the un-split `f.Invoke(a).Invoke(b)` chain
  (`core-types.fs:19`) and compiles — the §3.1-gap-1 freeze bug is fixed (`eed60c7`), so the
  `let`-split workaround a prior draft of this doc described is gone; there is nothing left to
  retire (M5 just proves the round-trip).

---

### 2.4 M3–M7 mechanism: the node-keyed `Fun`-arity verdict (LOCKED)

The flat-vs-curried (which `FunN`) decision is **made at inference time**, by the Step-A/M3
`subsumes` arm — that arm is literally the code that matches a source lambda's arrow against
`Fun`/`Fun2` (`Passes/Unification/Engine.fs:687`). The frozen `FTTypar(Method,i)` param the
codegen gate sees is **bare** (axis + index only, `SemanticInfo.fs:374`) — the `:> FunN` bound
is a *constraint*, not part of the type — so codegen cannot re-derive the arity structurally.
The decision therefore has exactly one correct home (inference) and must be *carried forward*,
not reconstructed.

LOCKED: record the verdict in a **node-keyed side table**, riding the identical path the
Regions `ClosureRepr` snapshot already uses:
- produced in a pass and snapshotted in `Pipeline.fs` (next to `ClosureReprs =
  Regions.closureReprSnapshot ctx`, `Pipeline.fs:45`);
- stored on `TastFile` (sibling of `ClosureReprs : Map<NodeKey, ClosureRepr>`, `Tast.fs:789`);
- carried by `TastConvert` (`TastConvert.fs:268`);
- passed into `EmitClosures.discoverClosures` (`Layout.fs:587`) and consumed in
  `registerClosure` (`EmitClosures.fs:757`) to set the value-struct `Invoke`'s arity — the
  M1/M2 single-arg path is just the `arity = 1` case.

**Recording point.** `subsumes` itself stays pure (it compares types, not expressions —
`Engine.fs:707` is a read-only relation). The verdict is recorded by the *caller* that holds
the lambda argument's `NodeKey`: the coercion-constraint discharge
(`SemanticConstraintKind.Coercion`, `SemanticInfo.fs:694`, drained on unify via
`checkConstraint`) or the arg-coercion check in `InferApp`. When the new `Fun2` arm returns
`Subtype` for an arrow-typed argument, the caller keys `node → FunN arity`. This is the same
decide-here / snapshot-in-Pipeline / read-in-codegen split as `closureReprSnapshot`
(`Regions.fs:858`).

**Why this is the right factoring, not a shortcut to land M3 early.** Two properties the
alternative (a frozen per-axis typar-constraint table, §5) does not have:
1. **One source of truth.** Inference *decides* the representation; the verdict table *is* that
   decision. A frozen constraint table re-derives at codegen what inference already concluded.
2. **External and project-local collapse to one path.** `subsumes` fires at the application
   site whether the combinator is the project-local `apply2` (M3) or the external
   `StructSeq.fold` (M6/M7), so one mechanism covers both — and it dissolves the
   gate-extension problem: `collectStackLambdaArgs` cannot see an external
   `TExpr.External`/`ExternalMember` head today (it walks only `staticFnKeys`,
   `EmitClosures.fs:688`), but a node-membership test against the verdict table needs no such
   walk. The current structural re-derivation in `collectStackLambdaArgs` (all-`GSimple` +
   bare-typar param) is itself a fragile reconstruction of what `subsumes` already knew; M3
   narrows it toward "is this node in the verdict table, and at what arity?".

---

## 3. Arity-2 cap (M3)

LOCKED: flat-first with an arity cap of 2.
- **Saturated 2-arg** → flat `Fun2<'A,'B,'C>` (single 2-arg `Invoke`, `core-types.fsi:34`,
  proven `StructSeqTests.fs` `SumAcc`).
- **Partial** (1-of-2) → `curryFun` to residual `Fun<'B,'C>`.
- **Arity > 2** → curried chains, UNCHANGED. `Fun3`/`Fun4` are additive *when a concrete
  combinator demands one* (`function-representation-plan.md` §"Out of scope" item 1), never
  speculative.

The cap is a decision in the representation chooser: count saturated args in the param's
largest application spine; `=2` → `Fun2`, `=1` → `Fun`, `>2` → curried chain.

---

## 4. LOCKED decisions (do NOT relitigate)

From `function-representation-plan.md`, and confirmed by the A/B/C landing:

1. **Distinct `FunN` names**, NOT arity-overloaded. `Fun` and `Fun2` are separate nominal
   types because `Types.Class` is bare-name-keyed (no arity dim), so two `Fun`s collide.
   Arity-overloaded local classes are a separate deferred epic (`arity-overloaded-classes-design.md`).
2. **`Fun2` does NOT inherit `Fun<'A,Fun<'B,'C>>`.** Adaptation is explicit and reference-typed
   (`curryFun`/`flatten`), never a subtype relation (`core-types.fsi:38-40`).
3. **Flat-first with arity cap 2.** Curried `Fun<,>` stays canonical for partial application
   and arity-over-cap (§3).
4. **`curryFun`/`flatten` are ordinary-code lowering targets, not magic IL** (`core-types.fs:31-33`).
   The pass synthesises ordinary `App` nodes around mismatches (§2.3).
5. **The unifier stays structural** (`SemType.TyFun` unchanged); the `Fun`/`Fun2` alias is a
   codegen contract — the ONLY exception is Step A's (and M3's) read-only `subsumes` arm.
6. **The struct-seq library threads `'TFunc` by hand** — no new pass needed for the library
   itself; this work is purely about *source-lambda* arguments to those combinators.

---

## 5. Deferred / out of scope

- **Arity-overloaded project-local classes/interfaces** (`(name, arity)` key + ~47 lookup
  sites). Independently valuable (F#-compat `FSharpFunc` family) but not needed while distinct
  `FunN` names are used. See `arity-overloaded-classes-design.md`.
- **`allows ref struct` / ref-struct closures + ref-struct enumerator `Dispose`**
  (`get-enumerator-gaps.md` item 1) — waits on a byref-like predicate on `SemType`. The M1/M2
  value-struct closures are PLAIN value types (copied by value, escape-free), so they need no
  ref-struct machinery; `allows ref struct` is a strictly opt-in, additive constraint-loosening
  *after* M3–M7.
- **Mutable-capture-through-the-constrained-slot value-struct** — M2 left this noted-untested.
  A mutable capture is promoted to a heap ref-cell captured by value, so a value-struct closure
  is still correct; add a test when the `n <- …; n` body shape types cleanly in Vesper.
- **`Fun3`/`Fun4`** — additive only when a concrete combinator demands an arity-3+ flat slot.
- **A frozen per-axis typar-constraint table** — the fully general alternative to §2.4's
  node-keyed verdict (make every typar's `:> FunN`, and other, bound durably available at
  codegen). **The PROJECT-LOCAL slice is now being built — see §9** (Direction B): the
  consuming-combinator for-in `'E` recovery is exactly the "second codegen-native consumer of
  typar bounds" this item said would justify it. The EXTERNAL slice (the codegen-view reopening)
  stays deferred to M7. Deferred until a SECOND, codegen-native consumer of typar bounds exists. It is real
  work and splits in two because the binder surfaces differ:
  - *project-local*: a new `FrozenConstraint` DU (none exists today), a freeze step
    `SemanticConstraint`→`FrozenConstraint` indexed by the method axis (the inference-side
    source is `TypeScheme.Constraints`, `SemanticInfo.fs:989`), a field on `StaticFn`, and
    every construction site;
  - *external*: carry the already-frozen `ExternalConstraint.Coercion` (`ExternalSymbols.fs:53`
    — its target is a `FrozenType` over the symbol's typars) from the inference surface
    (`ExternalSymbol.Constraints`) onto the **codegen** view — `CodegenOpenSignature`
    (`ExternalSymbols.fs:578`) and `ICodegenSymbols` are *deliberately* constraint-free today
    (`ExternalSymbols.fs:597-604` — "emission … can no longer reach … constraints"), so this is
    the part that re-opens a wall the architecture closed on purpose.

  Half the design already exists (`ExternalConstraint` is frozen-target-based and reusable as
  the `FrozenConstraint` model); the genuinely new cost is the codegen-view reopening, best paid
  once a real consumer (e.g. the compiler auto-deriving `FunN` constraints at generic
  combinators — NOT happening while §4.6's hand-threaded-library decision holds) justifies it.
  Until then §2.4's node-keyed verdict is both less work and better-factored.

---

## 6. Staged plan (M3–M7) — isolation-test-driven, review-gated

Per [[feedback_systematic_tests_over_whackamole]]: each milestone is the smallest isolation
test that forces the capability → run → diagnose the EXACT wall → fix → iterate. This is the
workflow that landed A/B/C: one sub-agent per milestone, the orchestrator reviews the diff and
commits on `vesper-seq` between milestones. All tests are `compileSource` CLR fixtures in the
spirit of `StructSeqTests.fs` (assert exit/output, then assert `constrained.` `0xFE 0x16`
present and NO `box` `0x8C`; for value-struct shape also assert `System.ValueType` base via
`peTypeBaseTypeName`).

### M3 — saturated-2 → `Fun2` value-struct (the A+C template, for arity 2)
**Smallest test:** `let apply2 (f: 'TF when 'TF :> Fun2<int,int,int>) (a:int) (b:int) = f.Invoke(a,b)`
then `apply2 (fun x y -> x+y) 20 22` → 42, value-struct base, `constrained.`, no box.
**Proves:** (a) a NEW `subsumes` arm `subsumes(TyFun(a,TyFun(b,c)), Fun2\`3<a,b,c>) = Subtype`
(the flat-2 correspondence — name `Vesper.Fun2`, arity 3; sibling of the arity-2 `Vesper.Fun`
arm at `Engine.fs:687`); (b) the **node-keyed `Fun`-arity verdict** of §2.4 — the arm's caller
records `lambda-node → arity 2`, snapshotted in `Pipeline.fs` and threaded to
`discoverClosures` exactly like `ClosureReprs`; (c) the C/M1+M2 value-struct emission
generalised to a 2-param flat `Invoke` (peel the curried 2-arg lambda body into one
`Invoke(a,b)`), driven off the verdict's arity; (d) the `EmitCall` `!TF` override already keys
off `ClosureValueTypeByNode`, so it needs no change. Risk: medium (the verdict table + the
2-param `Invoke` body peel are the new parts). Then repeat against the real
`StructSeq.fold (fun acc x -> acc+x) 0 s` — which §2.4 covers with the SAME verdict mechanism
(the external head needs no `collectStackLambdaArgs` extension).

### M4 — `curryFun` adapter at a partial-application site
**Depends on:** M3. **Smallest test:** a flat-`Fun2` source lambda used where only one arg is
applied (forcing a residual `Fun<'B,'C>`). Assert a synthesised `call` to
`Vesper.FunAdapters.curryFun` (`core-types.fs:31`) wraps the value. **Proves:** §2.3
adapter-`App` synthesis (resolved-symbol lookup, not canonical-name magic) + the discovery
pre-walk flagging the mismatch. Risk: low-medium (external-symbol lookup for the adapter).

### M5 — `flatten` adapter at a curried-meets-flat site
**Depends on:** M4. **Smallest test:** a genuinely curried source value (`fun a -> fun b -> a+b`,
an arity-2 curried chain via a `let`-indirection so it stays curried) reaching a flat `Fun2`
slot. Assert a `flatten` `call` (`core-types.fs:26`) and a correct round-trip. **Proves:**
§2.3 curried→flat adaptation. No `let`-split workaround remains to retire — `Flattened.Invoke`
is already the un-split `f.Invoke(a).Invoke(b)` chain (`core-types.fs:19`) and compiles, the
§3.1-gap-1 freeze bug being fixed. Risk: medium.

### M6 — end-to-end zero-alloc `ofArray |> map |> fold` from SOURCE lambdas
**Depends on:** M3. **Smallest test:** the rung-4 "wall iv" pipeline but with
`map (fun x -> x+1)` / `fold (fun acc x -> acc+x)` SOURCE lambdas instead of hand-written
`AddN`/`SumAcc` structs. Assert identical output (`14`) AND the same no-`box`
constrained-dispatch IL. **Proves:** the whole epic composes.

**M6 is NOT integration — it surfaced a real gap (verified `fd77db1`).** The M3 verdict +
`EmitCall`'s `!TF` override fix the CALL site, so a **terminal** combinator whose result type
does not mention the function typar (`fold : … -> 'State`, `apply2 : … -> int`) works with
source lambdas today. But a **transformer** combinator whose result type CARRIES the typar
(`map : … -> MapSeq<…,'TFunc,…>`) breaks: the front end freezes the result/receiver type with
`'TFunc := arrow`, which `encodeType` lowers to the `Vesper.Fun`/`Fun2` INTERFACE (reference),
while the call actually returns the `<closure>$` value-struct instantiation — a struct↔reference
layout mismatch in the receiving slot (`NullReferenceException`/`InvalidProgram`). Fails in BOTH
nested (`fold f 0 (map g (ofArray xs))`) and stored (`let s1 = map g s0`) top-level forms.

#### M6 sub-plan — verdict-keyed result-type propagation (Approach B, P-a…P-e)

LOCKED approach: **B (freeze-/codegen-time substitution)**, NOT inference-side (Approach A
would reopen §4.5's structural-unifier lock for a late-minted identity), and a NARROW
verdict-propagation — NOT §5's full frozen constraint table. Reuse the `<closure>$`
value-type `FrozenType` already minted in `Assembler.BindClosures`
(`ClosureValueTypeByNode`); add a recursive `substituteVerdictClosures : FrozenType ->
FrozenType` that replaces the matching `'TFunc`-position leaf of a binding/temp's frozen type
with that lambda's closure value-type, applied at every slot-type derivation site
(`collectModuleValues`/`collectGenericModuleValues` and the Main-local path).

- **Match by typar POSITION, not just `FTFun` shape** (largest risk): a genuine
  function-valued field of the same shape must not be miscoerced. Thread the result-typar
  index forward from `InferApp` (the one place it is known), keyed like `FunSlotArity`.
- **Verify the mint-vs-consume ordering FIRST**: `substituteVerdictClosures` can only read
  `ClosureValueTypeByNode` if the closure value-type is minted before slot types are chosen.
  If not, the rewrite moves to field-emission time. This determines where the code lives.
- **§5 still deferred:** B fires only on GROUND binding types. Storing a mapped seq in a
  *generic* helper (receiver type is `FTTypar`, not `FTClass(MapSeq,[…FTFun…])`) needs §5; out
  of scope for M6/M7 (the pipeline is ground at top level).

Sub-milestones, each smallest-test-first, review-gated:
- **P-a** — single `map`, stored, consumed: the reduced repro (`mk : ('TF:>Fun<int,int>) ->
  Holder<'TF>`; `let h = mk (fun x -> x+1)`; assert the field sig's `'TF` arg is
  `ELEMENT_TYPE_VALUETYPE <closure>$`, not `class Fun\`2`). Resolves the position-vs-shape
  question in the simplest setting.
- **P-b** — nested temp (`fold f 0 (map g (ofArray xs))`): forces the Main-local slot path.
- **P-c** — the M6 capstone: un-`ptest` the committed M6 fixture; full `let s1=…; let total=…`
  pipeline → `14`, no box.
- **P-d** — multi-`map` chain (`map g0 |> map g1 |> fold f`): forces the RECURSIVE rewrite over
  nested `MapSeq<MapSeq<…>,…>` with two distinct `<closure>$` slots. **Must make the
  consuming-body rewrite collision-safe.** P-b/P-c (`aa2cdd1`) landed it as a program-wide
  arrow-TYPE-keyed table (`arrowClosurePairs` in `Assembler.fs`), because the consuming
  combinator's `for-in` carries no verdict of its own to key on. Two same-typed transformer
  lambdas (two `int->int` maps) have structurally identical frozen arrows → they collide
  (first wins), and a genuine same-typed function value in a for-in/body would be miscoerced.
  P-d must eliminate arrow-equality. **SUPERSEDED — see §9:** the original framing ("thread the
  producing call's verdict to its consumer") is unworkable while `fold`'s body is emitted once
  and shared (one grounded body cannot serve multiple instantiations). The LOCKED fix (Direction
  B) instead removes the grounding entirely — `'E` becomes a real call-site-recoverable generic
  parameter — so there is no baked body to disambiguate. The multi-`map` `ptest` is the gate.
- Regression gate every milestone: re-run the green M1/M2/M3 suite (terminal path must stay
  untouched — `substituteVerdictClosures` is a no-op when no matching leaf exists).

### M7 — library graduation (capstone) — the P-e milestone of the M6 sub-plan
**Depends on:** M6 P-c/P-d. Also confirms the result-type propagation survives the EXTERNAL
combinator head (the `MapSeq` result type comes from the contract's `FrozenType` template /
`ExternalSignature`, not a project-local decl). **Smallest test:** a `buildPackage`-gated client of `Vesper.Seq`
([[reference_buildpackage_gates_on_diagnostics]]) calling `StructSeq.map`/`fold` with source
lambdas. **Proves:** the pass survives the strict package path, not just inline `compileSource`
(the package path surfaces front-end gaps the lenient path hides). Risk: low-medium.

---

## 7. Dev workflow & gotchas

- **Build/test ONLY via `./claude_tools.cmd`** (Bash), never raw `dotnet`:
  - `-Action Build -SourceProject "XParsec.FSharp.SemanticAnalysis"` (or `XParsec.FSharp.Codegen.Clr`)
    — fast F# error check.
  - `-Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests"` — focus one test by editing
    `test "…"` → `ftest "…"`. `-SummaryLines N` widens output. Full unfiltered output is always
    in `./claude_tools_output.log` (Read it; don't re-run). NOTE Expecto suppresses stdout for
    PASSING tests — surface diagnosis via assertion messages.
  - The **strict gate** is `buildPackage "<name>"` — fails on any error diagnostic
    ([[reference_buildpackage_gates_on_diagnostics]]), unlike the lenient inline `compileSource`.
    Build library changes (M7) through it.
  - `-Action Format` (Fantomas) before finishing. Regenerate `.parsed` goldens for changed
    `.fsi`/`.fs` (xparsec-dev `-UpdateSnapshots`); regenerate the committed `Vesper.Core.dll`
    ref (`REGEN_VESPER_CORE_REF=1`) when Vesper.Core source changes.
- **Crash semantics:** malformed IL crashes the host with `Internal CLR error 0x80131506`
  (ExecutionEngine) and ABORTS the whole parallel run — keep exactly ONE `ftest` focused when
  isolating a crash. A normal managed exception (`TypeLoadException`, `InvalidProgram`) =
  valid-but-wrong IL / bad metadata.
- **IL / metadata inspection:** `peMethodIlWhere bytes "Type" pred` / `peMethodNames` /
  `peTypeBaseTypeName` (added in M1) dump/inspect; `openPe bytes → GetMetadataReader()` maps
  tokens → names and reads type/field rows. Opcodes: `ldarg.0`=02, `ldfld`=7B, `ldflda`=7C,
  `ldloca.s`=12, `initobj`=FE 15, `call`=28, `callvirt`=6F, `constrained.`=FE 16, `box`=8C,
  `newobj`=73, `ldsfld`=7E, `stsfld`=80, `ret`=2A. Don't `%A` the TAST (EqArray hides contents
  — [[reference_eqarray_percentA_cache_key]]).
- **Guardrails:** don't `git commit` ([[feedback_user_commits]]) — the orchestrator reviews +
  commits; temp files in `./tmp/` ([[feedback_tmp_dir]]); `constrained. callvirt` is correct
  ONLY for interface (virtual) methods ([[reference_constrained_callvirt_nonvirtual_struct]]) —
  the struct `Invoke` must be reached through the `Fun`/`Fun2` interface slot, never a direct
  struct `MethodDef`; a non-generic *local* type reference uses the registered `TypeDef`
  (`env.UserTypes.[key]`), never a `TypeSpec`; mirror F#'s grammar ([[feedback_match_fsharp_grammar]]).

---

## 9. Redesign: eliminate arrow-equality keying (Direction B — the §5 project-local slice)

**Status:** LOCKED direction, not yet implemented. Supersedes the P-d "make the
consuming-body rewrite collision-safe" bullet (§6) and promotes the §5 frozen-constraint
table from "deferred" to "doing it now, project-local only". Delete the
`ClosureVerdictRewrite` for-in arm (and this section's open items) once it lands.

### 9.1 What is wrong today

`ClosureVerdictRewrite` (CLR codegen) rewrites the `'TFunc`-arrow leaf to the
`<closure>$` value-struct in three places. Two are **node/position-keyed and
collision-safe**:
- the stored module-value slot (`substituteVerdictClosures`, keyed by the producing
  lambda node's reference identity);
- the inline `App`-result type (`appOwnVerdict`, keyed by walking the call's own
  argument spine).

The third — the **consuming combinator's `for-in` enumerator descriptor**
(`rewriteForInEnumerator` / `verdictArrowToClosure` / `rewriteClosureLeaves`) — falls
back to **program-wide arrow-TYPE equality**: it builds `[(typeOfExpr lambdaNode,
closureFt)]` and replaces any structurally-equal `FrozenType` leaf. Two same-typed
transformer lambdas (two `int -> int` maps) share one frozen arrow and **collide**
(first wins); a genuine same-typed function value would be miscoerced. This is the
`ptest "rung 4 (M6 P-d): multi-map chain"` wall. Accepting arrow-equality was the
original mistake.

### 9.2 Root cause — a front-end representation defect, not a codegen bug

The for-in path *cannot* use node identity because the arrow it must rewrite lives in
`fold`'s **own grounded body**, which does not lexically contain the producing lambda
(that lambda is in the caller's `map (fun x -> …) s0`). The arrow is there at all
because of a chain of front-end decisions:

1. `fold`'s enumerator typar `'E` is a **phantom constraint typar** — it appears in no
   parameter or result, only in `'S :> IStructSeq<'T,'E>` (`InferControlFlow.fs`
   `tryTyparSeqSource`, which freezes the for-in's `ConstrainedInterface` ifaceArgs from
   `resolveStep` of `'T`/`'E`).
2. `generalise` (`InferGeneralize.fs:378-394`) *does* quantify `'E` (the dependent-typar
   fixpoint over `Coercion` bounds), so `fold`'s scheme is `∀ 'TFunc 'State 'T 'S 'E. …`.
3. But `instantiate` (`InferGeneralize.fs:53-95`) **deliberately leaves phantom
   quantified roots verbatim** in the per-call constraint substitution ("the body
   grounds them at the binding") — only *surface* roots are freshened. So the single
   `fold` call unifies the **original** `'E` root with the concrete
   `MapEnumerator<…, 'TFunc, …>`, grounding it — and the body's for-in, which references
   that original root, freezes with the concrete enumerator carrying the arrow.
4. Codegen compounds this: `staticFnTypars` (`EmitClosures.fs:609-638`) counts method
   typars **only from param/result types**, so even if `'E` survived as a typar it would
   not be emitted as a generic slot, and `matchInstantiation` (`TastLower.fs:99`)
   recovers typars **only by matching params against args**, so `'E` (in no param) is
   unrecoverable. Both mechanisms *require* `'E` to be grounded.

So the arrow in the body is the grounded `'E`, and arrow-equality is a codegen
symptom-patch for "the front end baked one call's concrete enumerator (with the arrow
where the value-struct closure belongs) into a method that is supposed to be generic
over `'E`."

### 9.3 The fix is the faithful F# representation

These two are equivalent F# definitions (confirmed semantics):

```fsharp
let fold (f: 'TFunc when 'TFunc :> Fun2<'State,'T,'State>) (seed: 'State)
         (source: 'S when 'S :> IStructSeq<'T,'E> and 'E :> IStructEnumerator<'T>) : 'State = …
// ≡
let fold<'TFunc,'T,'State,'S,'E when 'TFunc :> Fun2<'State,'T,'State>
                                 and 'S :> IStructSeq<'T,'E>
                                 and 'E :> IStructEnumerator<'T>>
        (f: 'TFunc) (seed: 'State) (source: 'S) : 'State = …
```

F# generalizes phantom constraint typars (`'E`) into the method's generic parameter
list; both forms compile to **one generic method of arity 5** with the same three
constraints and the same `MethodSpec`-per-call obligation. The only difference is
type-parameter *order* (implementation-defined for the inline form; observable only
under explicit `fold<…>` application). **Direction B makes Vesper model `'E` as the
real, call-site-recoverable generic parameter F# already treats it as.** Today's
grounding is the divergence.

### 9.4 The plan (stages are COUPLED — must land together, gated on the M6 `ptest`)

Flipping the front end without the codegen recovery yields a body with `!E` and no
MethodSpec slot → invalid IL / `matchInstantiation` failwith. So:

1. **Phantom typars become independent generic slots.** Flip `instantiate` to freshen
   phantom-quantified roots in the constraint substitution too (seed `constraintSubst`
   from the full `subst`, not the surface restriction — the operative lines are
   `InferGeneralize.fs:81-85`; rationale comment runs to `:101`) so each call gets its
   own `'E` and the body's `'E` stays free → freezes as `FTTypar(Method, idx_E)`.
   **The index minter already handles phantoms** — `Elaborate.mkMethodQuantEnv`
   (`Elaborate.fs:218-267`) runs the identical dependent-typar `Coercion` fixpoint
   (`:257-265`) `generalise` does, so once `'E` survives un-grounded it gets its
   `FTTypar(Method, idx_E)` index with NO change to the minter. Then carry the scheme's
   true quantified-typar **count** to codegen (source: `mkMethodQuantEnv`'s `acc.Count`
   / `scheme.Quantified.Length`; `CompiledFns.gather` currently zeroes `ValRepr.Typars`,
   `CompiledFns.fs:95`) and retire the param/result-only `staticFnTypars` re-derivation
   (`EmitClosures.fs:609-638` — the same fragile-reconstruction anti-pattern already
   deleted from `collectStackLambdaArgs`).
2. **Carry typar bounds to codegen** — the project-local half of §5's frozen-constraint
   table. Add method-axis-indexed `FrozenConstraint`s on `StaticFn`/`StaticMethodRef`,
   populated at freeze (`Elaborate.run`, which holds `ctx` and mints the indices) from
   `TypeScheme.Constraints` (`SemanticInfo.fs:989-998`; scheme lives in
   `ctx.Bindings.Scheme`), snapshotted onto `TastFile` exactly like `FunVerdicts`
   (`Pipeline.fs:43-54`) and threaded `TastFile → HolderPlan → StaticFn/StaticMethodRef`
   like `StaticFnTypars`. Reuse the `ExternalConstraint.Coercion` frozen-target model
   (`ExternalSymbols.fs:39-53`, `Coercion of typarIndex:int * target:FrozenType`) as the
   DU shape.
3. **Call-site phantom-typar solve** (`EmitCall.fs:252-324`). Three coupled parts the
   verified map surfaced:
   - **(3a)** Relax `matchInstantiation` (`TastLower.fs:135-140`) — it currently hard-
     `failwith`s on any unrecovered index; a phantom typar in no param/result is
     unrecoverable by param-matching, so leave those slots unresolved (`ValueNone`) /
     partition the typar set, rather than failing.
   - **(3b)** Give codegen an interface-impl walk. `EmittedClass` (`EmitTypes.fs:160-203`)
     carries NO interface-impl list today — the impl templates exist only at the
     emission-input layer (`ClassDecl.Interfaces`, `CodegenTypes.fs:36`) and are
     discarded after nominal emission. Add an impl field onto `EmittedClass`, populate it
     from `ClassDecl.Interfaces`, and write a `FrozenType` impl-walk — the codegen analog
     of front-end `Engine.tryUpcastWitness` / `subtypeInterfacesOf` (`Engine.fs:553-611`),
     which instantiates an impl template by the receiver's args via `instantiateMember`.
   - **(3c)** The solve (insert just after the M1/M2/M3 closure-override loop,
     `EmitCall.fs:309`, before `StaticFnMethodSpec` `:311`): for `'S :> IStructSeq<'T,'E>`
     with `'S := instArr.[idx_S]` (already the node-key-rewritten `<closure>$`-bearing
     type, since `'S` is param-visible), run (3b)'s `IStructSeq` witness over
     `instArr.[idx_S]` and read `'E := MapEnumerator<…,<closure>$,…>` from the bound's
     template. **The closure rides in for free** through `'S`'s already-rewritten arg
     (§9.1 path 1/2 is node-keyed) — collision-free, no arrow-equality anywhere.
4. **Delete** `rewriteForInEnumerator` / `verdictArrowToClosure` / `rewriteClosureLeaves`
   and the `ForIn` arm of `retypeBody`. `ClosureVerdictRewrite` keeps only the
   stored-binding-slot and `App`-result paths (the *producing* `'TFunc`, a recoverable
   param — those stay node-keyed).
5. **Un-`ptest` the multi-map test**; regression-gate M1/M2/M3/P-a/P-b/P-c + wall-iv.

### 9.5 Scope and risk

- **Project-local only now.** Every current test defines `map`/`fold` in source, so the
  bounds come from `TypeScheme.Constraints` and the impl walk from the type registry.
- **External head (M7) deferred.** Carrying `ExternalConstraint.Coercion` onto the
  *codegen* view (`CodegenOpenSignature` / `ICodegenSymbols`, deliberately constraint-free
  — `ExternalSymbols.fs:597-604`) is the genuinely wall-reopening half (§5). No external
  consumer exists until the `buildPackage` client of `Vesper.Seq` (M7), so it waits.
- **Risk:** Stage 1's `instantiate` flip inverts a load-bearing decision; the regression
  gate is the existing green phantom-typar tests (the chained-`wrap` GeneralisationTests
  case, the rung-3 generic struct-seq for-in tests). Stage 3's impl walk is new codegen
  reach into typar bounds — the cost §5 names; here it is paid only for project-local
  types whose impls the registry already holds.

### 9.6 Why this is better than the alternative kept-grounding fix

Threading the producing call's verdict into the *consuming body* (the literal P-d
wording) cannot work while `fold`'s body is emitted once and shared: a single grounded
body can serve only one instantiation. Direction B removes the grounding entirely, so
there is no shared baked body to disambiguate — the body is genuinely generic and the
closure identity flows through the normal `MethodSpec` instantiation, the same channel
every other type argument already uses. The collision is not *patched*, it is made
*impossible*.

## 8. Cross-references

- `brainstorm-seq-module.md` — the north star (zero-alloc struct `Seq`).
- `function-representation-plan.md` — §"Codegen layer (new)" / §"Generic closures" /
  §"Region / ref-struct extension" this work realises.
- `get-enumerator-gaps.md` — adjacent `for…in` work (ref-struct `Dispose`, co-blocked with
  `allows ref struct`; out of scope here).
- `arity-overloaded-classes-design.md` — the deferred `(name, arity)`-keyed-class epic.
- Memories: `[[project_seq_struct_pipeline_ladder]]` (authoritative landed state, incl. the
  A/B/C commit detail), `[[project_struct_codegen]]`, `[[project_function_method_compiled_form]]`,
  `[[reference_constrained_callvirt_nonvirtual_struct]]`, `[[reference_buildpackage_gates_on_diagnostics]]`,
  `[[reference_infix_op_needs_provider_symbol]]`.
