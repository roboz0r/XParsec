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

## 2. M7 — library graduation (stages 0–2 LANDED; stage 3 is the last)

**Goal:** a `buildPackage`-gated ([[reference_buildpackage_gates_on_diagnostics]]) client of
`Vesper.Seq` calling `StructSeq.map` / `StructSeq.fold` with **source lambdas**, proving the
whole epic survives the strict package path and an EXTERNAL combinator head.

M7 is **four stages** (0–3). **Stages 0, 1, and 2 have landed**; **stage 3** — the codegen
consumption in `emitExternalCall` — is the only remaining work and the only thing between here
and the headline test. Stages 1+2 are behavior-neutral on their own (the data they thread is
unconsumed until stage 3), gated green by Codegen.Clr (1051) + SemanticAnalysis (628).

### Stage 0 — contract extraction yields the full generalised symbol — LANDED

The `.fsi`-extracted `StructSeq.fold` symbol was malformed two ways, both fixed: (1) **phantom
typars not counted** — `finalizeVal` (`VesperLib.fs`) now snapshots `typarCount` AFTER
`resolveConstraints`, so `fold` carries all 5 typars `'TFunc,'T,'State,'S,'E` and `Instantiate`
succeeds; (2) **dependency-prelude bounds froze as `FTUnknown`** — the dependency composite's
`AmbientOpenPrefixes` are now threaded through `composeProviders` → `buildProviderWith` →
`ExtractCtx.DependencyAmbientPrefixes`, so `'TFunc :> Fun<…>` / `Fun2<…>` resolve to the real
`Vesper.Fun`2` / `Fun2`3` nominals.

### Stage 1 — codegen-view constraint channel reopened — LANDED

Reopens the wall `ExternalSymbols.fs` `CodegenOpenSignature` / `ICodegenSymbols` closed on
purpose (the `Constraints`-stripped codegen view), and fixes the consumer-side analogue of the
Stage-0 count bug:

- **`Inline.openMethodSignature`** now runs the dependent-typar fixpoint (the consumer mirror
  of `Elaborate.mkMethodQuantEnv`): after the signature pre-order `collect`, a `ResizeArray`
  worklist walks each collected typar's `Coercion` bounds, appending phantom typars (the `'E`
  in `'S :> IStructSeq<'T,'E>`) to a fixpoint. So `MethodArity` now counts them and the
  reconstructed method-typar ORDER matches the producer's emitted IL. For `fold` this yields
  arity **5**, order `['TFunc,'State,'S,'T,'E]` — identical to the producer's
  `mkMethodQuantEnv` emit order. Kept `zonk`-free (link-chased through `collect`) per the
  existing "UnificationEngine compiles later" constraint. It also emits the bounds as a
  `FrozenConstraint list` over the method axis (`typarIndex` = the constrained `'S` slot;
  `target` = `IStructSeq<'T,'E>` carrying the phantom slots), the SAME shape `EmitCall`'s
  project-local solve already consumes — so the stage-3 solve is head-agnostic.
- A **`Constraints: FrozenConstraint list`** field added to `Inline.OpenMethodSignature` and to
  `CodegenOpenSignature`; `CodegenSymbols.ofProvider` threads `os.Constraints` through.
- Latent gap flagged in a code comment: the fixpoint chases only DIRECTLY-named target typars,
  not a freshly-interned typar's own further bounds (transitively). None of the struct-seq
  signatures need it (`'E :> IStructEnumerator<'T>` only reintroduces `'T`, already named by
  `'S`'s bound). Harden to a full transitive fixpoint only if a future signature needs it.

### Stage 2 — `FrozenInterfaces` published from the `.fsi` — LANDED

The `.fsi` extractor now fills `ExternalClassShape.FrozenInterfaces` (was always
`basic`'s empty default; only the metadata layer filled it), mirroring the existing
`inherit` → `FrozenBaseType` deferral:

- **`DeferredBody.Class`** (`VesperLib/TyparCapture.fs`) gains an `interfaces: Type list`
  channel.
- The **Class/Anon extraction arm** (`VesperLib.fs`) collects
  `TypeSignatureElement.Interface(InterfaceSpec(typ = t))` decls and defers them (the interface
  type may forward-reference a sibling). NOTE the seq types are `[<Struct>]`-ATTRIBUTED, so
  they parse through the Class/Anon arm, NOT `TypeSignature.Struct` (the `struct…end` form).
- The **finalize `DeferredBody.Class` arm** freezes each into `FrozenInterfaces` via the new
  `nominalInterface` helper (splits a frozen nominal into `(qualifiedName, FrozenType[])` over
  the declaring typars — the `.fsi` analogue of the metadata layer's `buildClassInterfaces`).
  A BCL interface that doesn't resolve in the `.fsi` (e.g. `IEnumerable<'T>`) freezes to
  `FTUnknown` and `nominalInterface` skips it — harmless, the witness only needs `IStructSeq`.
- Verified by a new `VesperLibTests.fs` test: a `[<Struct>] type Holder<'T>` with
  `interface IBox<'T>` extracts `FrozenInterfaces = [("…IBox`1", [FTTypar(Declaring,0)])]`.

### Stage 3 — NEXT — consume it all in `emitExternalCall`

The external analogue of the project-local `EmitCall` block (`§1`). All the data is now flowing
(stage-1 `Constraints` + arity on the open signature, stage-2 `FrozenInterfaces` on the shape);
stage 3 wires it into emission:

1. Give **`EmitResolve.tryInterfaceWitness`** an EXTERNAL arm: today it only walks project-local
   `env.Classes`; add a branch that reads `ICodegenSymbols.TryLookupType name` →
   `ExternalClassShape.FrozenInterfaces` and instantiates the matching interface's args over the
   receiver's declaring args (the data-form `ExternalSymbols.instantiateInterfaces` already does
   this realisation — reuse or mirror it).
2. In **`ClrRecipes.emitExternalCall`**: the open signature now reports `MethodArity` = 5 and
   carries `Constraints`. `recoverOpenTypars 0 methodArity openSig.Signature fnTy` recovers only
   the signature-reachable slots (`'TFunc,'State,'S` — and `'T` if it appears) but CANNOT
   recover the phantom `'E` (in no param/result). Run the phantom solve — the
   `FrozenConstraint.Coercion` fixpoint from `EmitCall.fs:314-388`, now reading
   `openSig.Constraints` and calling the stage-1 external `tryInterfaceWitness` — to fill the
   phantom slots BEFORE minting the `MethodSpec`, and encode the member-ref `msig` with
   `genericParameterCount = methodArity` (already 5). Confirm the external `MapSeq` result flows
   through the producing `App`-result / stored-slot rewrites unchanged.

**Likely sub-task uncovered during stage 1/2:** `[<Struct>]`-ATTRIBUTED types extract from the
`.fsi` with `IsValueType = false` — the Class/Anon arm does not decode the `[<Struct>]`
attribute (only the `struct…end` form sets `Flags.IsValueType`). The no-box / `constrained.`
dispatch on an external `MapSeq` may need that flag set from the `.fsi`; if the stage-3 test
shows a box or a `value type mismatch`, decoding `[<Struct>]` in the Class/Anon arm (or via the
attribute decoder) becomes part of stage 3.

**Smallest test (now a near one-liner — the harness landed):** the consumption harness is in
place ([[reference_declarative_package_test_harness]]) — `runPackagesInspect ["Vesper.Seq"] src`
compiles a driver against the built `Vesper.Seq` (+ transitive deps) in `packageAlc`, runs it,
AND returns the emitted bytes for a `peMethodsIlWhere` no-box / `constrained.` scan in one pass.
So the M7 test is:

```fsharp
let (exit, out), bytes =
    runPackagesInspect ["Vesper.Seq"]
        "let xs = [|1;2;3;4|]\n\
         let total = StructSeq.fold (fun a x -> a + x) 0 (StructSeq.map (fun x -> x + 1) (StructSeq.ofArray xs))\n\
         printfn \"%d\" total"
// assert exit 0, out = "14", and bytes carry constrained./no box on the Invoke + fold sites
```

→ same output + no-box constrained dispatch as the inline project-local M6 test
(`StructSeqTests.fs`). Risk: medium — the witness external arm + the `[<Struct>]` flag are the
real cost; the solve itself is shared with the (landed) project-local path.

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
  [[reference_buildpackage_gates_on_diagnostics]],
  [[reference_declarative_package_test_harness]] (the `runPackagesInspect` M7 test seam).
- Dev workflow / IL-inspection gotchas (build via `./claude_tools.cmd`, `peMethodIlWhere`
  etc., crash semantics): unchanged from prior revisions — see
  [[project_seq_struct_pipeline_ladder]] and `§7`-era git history if needed.
