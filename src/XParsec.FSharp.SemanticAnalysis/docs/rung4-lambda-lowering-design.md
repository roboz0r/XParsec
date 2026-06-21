# Design: source-lambda → flat-closure lowering (rung 4) — remaining work (M7)

*Ephemeral plan doc* per [[feedback_plan_docs_ephemeral]]. Originally scoped M3–M7;
**M3–M6 (incl P-d) have landed** (see §1), so this doc is now refocused on the one
remaining milestone — **M7, library graduation** — plus the documented boundary and the
locked invariants. Delete this file (and comments referencing it) once M7 lands. The CODE
+ isolation tests are the durable record; [[project_seq_struct_pipeline_ladder]] is the
authoritative landed-state memory.

---

## 1. Landed (M0–M6 + "Direction B") — concise record

Headline: `ofArray |> map |> fold` from **source lambdas** (not hand-written structs),
zero-alloc, including a multi-`map` chain of structurally-identical closures. All on
`vesper-seq`; full per-commit detail in [[project_seq_struct_pipeline_ladder]].

- **A/B/C (M0–M2)** — front-end accept of `subsumes(TyFun, Vesper.Fun`2)`; non-capturing
  singleton cache; captureless then capturing anonymous source lambda → zero-alloc
  value-struct closure, `constrained. !TF callvirt`, no box.
- **M3** — saturated-2 source lambda → flat `Fun2` value-struct (one `Invoke(a,b)`). New
  `subsumes(TyFun(a,TyFun(b,c)), Fun2`3)` arm + a node-keyed `FunVerdict`
  (`TastFile.FunVerdicts`) recorded in `inferApp`, read by `discoverClosures`. The old
  structural `collectStackLambdaArgs` re-derivation was deleted.
- **M6 + P-d, via "Direction B"** — the consuming combinator's phantom enumerator typar
  `'E` (in no param/result, only in `'S :> IStructSeq<'T,'E>`) is now a **real,
  call-site-solved generic method slot**, not grounded into the shared body. Key pieces:
  - `instantiate` (`InferGeneralize.fs`) freshens phantom-quantified roots per call (seed
    `constraintSubst` from the full `subst`), so the body's `'E` stays free → freezes as
    `FTTypar(Method, idx_E)`.
  - Emitted arity = a **body-inclusive `staticFnTypars` sweep** (NOT
    `scheme.Quantified.Length`, which over-counts freeze-erased typars).
  - Bounds carried front→codegen as a per-binding `FrozenConstraint list`
    (`TastFile.GenericFnSchemes`); `EmitCall` solves each phantom slot from its bound via a
    codegen interface-impl witness (`EmitResolve.tryInterfaceWitness` + Assembler
    `enumeratorOf`), the closure riding in through the already-node-keyed `'S` arg.
  - This **deleted the collision-prone arrow-equality for-in rewrite** that earlier P-b/P-c
    drafts used. Gated by 2-map and 3-map isolation tests (`StructSeqTests.fs`).
- **M4/M5 adapters (`curryFun`/`flatten`) were NOT needed** for the saturated map/fold
  pipeline (they matter only at partial-application / curried-meets-flat sites) and were
  not built. `curryFun`/`flatten` remain ordinary Vesper.Core code, available if a future
  non-saturated site needs them.

---

## 2. NEXT — M7: library graduation (the only remaining milestone)

**Goal:** a `buildPackage`-gated ([[reference_buildpackage_gates_on_diagnostics]]) client of
`Vesper.Seq` calling `StructSeq.map` / `StructSeq.fold` with **source lambdas**, proving the
whole epic survives the strict package path and an EXTERNAL combinator head.

M7 is **four stages**, not the three the earlier sketch assumed — empirical probing of the
extracted `Vesper.Seq` contract (the "verify before building" step) found the contract symbol
itself was wrong BEFORE the codegen-view question even arose. Stage 0 is now LANDED.

### Stage 0 — contract extraction yields the full generalised symbol — LANDED

The `.fsi`-extracted `StructSeq.fold` symbol was malformed in two independent ways (both
proven by a throwaway probe over `SymbolProviders.buildContract`, both fixed; the producer/impl
side — `struct-seq.fs` compiled — was always correct, this is purely the consumer-facing
contract extraction):

1. **PHANTOM typars were not counted.** `fold`'s `'T`/`'E` appear only in its `when` clauses
   (`'S :> IStructSeq<'T,'E>`), never in a parameter/result. `finalizeVal` (`VesperLib.fs`)
   snapshotted `typarCount = dc.Typars.Count` BEFORE `resolveConstraints` — but the
   `when`-clause-only typars are interned only while their coercion targets are translated,
   INSIDE `resolveConstraints`. So `fold` extracted with **3** typars, the phantom `'E`'s
   constraint index (4) overran the 3-element fresh array, and `Instantiate` threw
   `IndexOutOfRange`. **Fix:** snapshot `typarCount` AFTER `resolveConstraints` (the contract
   analogue of the impl-side `mkMethodQuantEnv` dependent-typar pass) — `fold` now carries all
   5 typars `'TFunc,'T,'State,'S,'E`, `Instantiate` succeeds.
2. **Dependency-prelude bounds froze as `FTUnknown`.** The `'TFunc :> Fun<…>` / `Fun2<…>`
   targets resolved to `FTUnknown "Fun"` / `"Fun2"` — extraction's `resolveTypeName` only
   tried the file's own open prefixes (`["Vesper.Collections"]`), never the dependency
   providers' `AmbientOpenPrefixes` (`Vesper`, where `Fun`/`Fun2` live), which the *consumer*
   front end uses. **Fix:** thread the dependency composite's `AmbientOpenPrefixes` through
   `composeProviders` → `buildProviderWith` → `ExtractCtx.DependencyAmbientPrefixes`, seeding
   them as lowest-priority file opens in `extractSymbols`. The bounds now resolve to the real
   `Vesper.Fun`2` / `Fun2`3` nominals. Regression-gated by the full Codegen.Clr (1051),
   SemanticAnalysis (627), Vesper.Tests (47), PackageBuild (10) suites.

### Remaining blocker — the EXTERNAL half of the frozen-constraint table

Direction B's call-site phantom-typar solve (§1) works for a PROJECT-LOCAL combinator head:
the bound comes from `TypeScheme.Constraints` → `FrozenConstraint`, and the interface-impl
witness walks `env.Classes`. With the head external, the (now-correct) `ExternalSymbol`
carries the bounds but they are not yet reachable by emission, and the witness has no data:
- The bound on `ExternalSymbol.Constraints` (`ExternalConstraint.Coercion`) is **stripped from
  the codegen view**: `CodegenOpenSignature` (`ExternalSymbols.fs:578-595`) / `ICodegenSymbols`
  (`:605-619`) carry no constraints, by the explicit decision at `:597-604`. M7 must re-open
  this — the wall the architecture closed on purpose. Also: `Inline.openMethodSignature`
  currently walks ONLY the signature, so it still misses the phantom `'T`/`'E` (the consumer's
  analogue of the Stage-0 count bug) — it must run the same dependent-typar fixpoint over the
  instantiated symbol's constraints so its reconstructed method-typar order matches the
  producer's emitted IL (params-then-result, then bounds).
- The interface-impl witness (`tryInterfaceWitness`) is project-local-only, and an external
  seq type's impls live on `ExternalClassShape.FrozenInterfaces` — which the `.fsi` extractor
  **does not populate** (always `ExternalClassShape.basic`'s empty default; only the metadata
  layer fills it). Publishing `FrozenInterfaces` from the `.fsi` `interface …` declarations is
  its own extraction stage, a prerequisite for the witness's external arm.

**Remaining sketch (verify before building):**
1. Codegen-view reopening: add a `FrozenConstraint list` channel to `CodegenOpenSignature` /
   `ICodegenSymbols`; extend `openMethodSignature` to append phantom typars via the constraint
   fixpoint (matching producer order) and emit the bounds, mapped into the same
   `FrozenConstraint` shape `EmitCall`'s solve already consumes, so the solve is head-agnostic.
2. Publish `ExternalClassShape.FrozenInterfaces` from `.fsi` class/struct `interface …` decls
   (frozen over declaring typars), then give `tryInterfaceWitness` an external arm reading it
   via `ICodegenSymbols.TryLookupType`.
3. Run the phantom solve in `ClrRecipes.emitExternalCall` before minting the `MethodSpec`
   (the external analogue of the `EmitCall` project-local block), and confirm the external
   `MapSeq` result flows through the producing `App`-result / stored-slot rewrites unchanged.

**Smallest test:** a `buildPackage` client `let total = StructSeq.fold (fun a x -> a+x) 0
(StructSeq.map (fun x -> x+1) (StructSeq.ofArray xs))` → same output + no-box constrained
dispatch as the inline project-local M6 test. Risk: medium — the codegen-view reopening and the
`FrozenInterfaces` publish are the real cost; the solve itself is shared with the (landed)
project-local path.

---

## 3. Known boundary (in scope, documented)

The chained-binding nested-slot rewrite (`substituteVerdictClosures` in
`ClosureVerdictRewrite.fs`) is collision-free for **linear** pipelines (`map |> map |>
fold`): each closure sits at a structurally-distinct depth, and the rewrite keys on the
whole depth-carrying nominal, node-anchored on the `Var` reference of the earlier binding.

A **multi-source** combinator (a hypothetical `zip` / `combine` taking two same-typed seq
args bound to DIFFERENT closures) would record two structurally-identical OLD nominals with
different NEW ones → a last-write-wins collision. No such combinator exists or is planned
(the struct-seq design is single-source). The true fix would be node-tagged frozen types.
Flagged in code at `substituteVerdictClosures`; revisit there if a multi-source seq
combinator is ever added.

---

## 4. LOCKED invariants (do NOT relitigate)

1. **Distinct `FunN` names**, NOT arity-overloaded (`Types.Class` is bare-name-keyed; two
   `Fun`s would collide). Arity-overloaded local classes are a separate deferred epic
   (`arity-overloaded-classes-design.md`).
2. **`Fun2` does NOT inherit `Fun<'A,Fun<'B,'C>>`.** Adaptation is explicit and
   reference-typed (`curryFun`/`flatten`), never a subtype relation.
3. **Flat-first, arity cap 2.** Curried `Fun<,>` stays canonical for partial application
   and arity > 2.
4. **`curryFun`/`flatten` are ordinary-code lowering targets, not magic IL.**
5. **The unifier stays structural** (`SemType.TyFun` unchanged); the only arrow↔`Fun`
   touch-points are the read-only `subsumes` arms (arity-1 and the M3 flat-2).
6. **The struct-seq library threads `'TFunc` by hand** — no auto-derivation of `FunN`
   constraints at generic combinators.

---

## 5. Cross-references

- `brainstorm-seq-module.md` — the north star (zero-alloc struct `Seq`).
- `function-representation-plan.md` — the codegen-layer / generic-closures plan this realises.
- `get-enumerator-gaps.md` — adjacent `for…in` work (ref-struct `Dispose`; out of scope).
- `arity-overloaded-classes-design.md` — the deferred `(name, arity)`-keyed-class epic.
- Memories: [[project_seq_struct_pipeline_ladder]] (authoritative landed state),
  [[project_struct_codegen]], [[project_function_method_compiled_form]],
  [[reference_constrained_callvirt_nonvirtual_struct]],
  [[reference_buildpackage_gates_on_diagnostics]].
- Dev workflow / IL-inspection gotchas (build via `./claude_tools.cmd`, `peMethodIlWhere`
  etc., crash semantics): unchanged from prior revisions — see
  [[project_seq_struct_pipeline_ladder]] and `§7`-era git history if needed.
