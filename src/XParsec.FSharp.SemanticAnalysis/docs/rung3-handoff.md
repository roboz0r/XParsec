# Handoff: struct-closure `Seq` — what landed, what remains

**Live plan as of 2026-06-19.** *Ephemeral plan doc* — delete it (and comment
references) once the remaining work below lands. The CODE + tests are the durable
record; the authoritative *landed* state is the `[[project_seq_struct_pipeline_ladder]]`
memory, not this file.

The zero-allocation struct-closure `Seq` **capability is proven end-to-end**. What
remains is the *ergonomic* general pass that lets source lambdas (not hand-written
struct closures) ride it. That pass is a separable epic — design it deliberately
before starting (the way `Fun2` was designed).

---

## 1. What landed this session (capability complete)

The north star (`brainstorm-seq-module.md`): a `Seq` whose combinators are concrete
generic structs chained by value, so a `xs |> Seq.map f |> Seq.fold g` pipeline
fuses into one stack loop with zero heap allocation. Status:

| Rung | What | Status |
|---|---|---|
| 1 | Value-type `for…in` source | ✅ landed (pre-session) |
| 2 | Concrete `[<Struct>] MapSeq` over a concrete struct source | ✅ landed (pre-session) |
| 3 | **Generic** struct dispatch (`constrained. !T callvirt`); generic `for…in`; generic map pipeline | ✅ landed (pre-session) |
| — | §2.1 "generic struct interface impls" gap | ✅ **stale** — both sub-gaps already supported; pinned by tests (`424fff2`, `9d0f773`) |
| — | §2.2 graduate generic-`'T` struct-seq → `src/Vesper.Seq` | ✅ landed (`49aa554`) |
| — | external-interface constrained dispatch (`'T :> Vesper.Fun`) | ✅ landed (`d12eab0`) — rung-4 prerequisite |
| — | `Fun2<'A,'B,'C>` flat arity-2 + `Curried`/`curryFun` + `Flattened`/`flatten` | ✅ landed (`ad63579`) |
| 4 | **struct-closure dispatch** — flip struct-seq `map`/`fold` to `'TFunc` constrained typars | ✅ landed (`3879b7e`) — zero-alloc pipeline proven |

`src/Vesper.Seq/struct-seq.{fsi,fs}` now carries the mapping/folding function as an
explicit constrained typar dispatched by value: `map` rides `'TFunc :> Fun<'T,'U>`,
`fold` rides `'TFunc :> Fun2<'State,'T,'State>` (single constrained 2-arg `Invoke`).
A zero-alloc `ofArray |> map |> fold` driven by hand-written struct closures runs
with `constrained.` dispatch and **no box** on the hot methods (proof test in
`StructSeqTests.fs`).

---

## 2. Design decisions already made (do NOT relitigate)

- **`Fun2` is a DISTINCT nominal type, NOT name-overloaded with `Fun`.** Vesper's
  project-local class/interface registry (`Types.Class`) is keyed by **bare name**
  with no arity dimension (unlike unions, which are `(name, arity)`-keyed), so
  declaring two interfaces both named `Fun` collides. The general fix
  (arity-overloaded local classes/interfaces, ~47 lookup sites) was explicitly
  rejected for now in favour of the distinct name. See §4.
- **`Fun2` does NOT inherit `Fun<'A, Fun<'B,'C>>`** (no interface-inheritance
  chain). Inheritance only buys the flat→curried direction, but a flat-first
  combinator world needs curried→flat (not a sound subtype relation anyway), so
  adaptation is explicit and reference-typed.
- **`curryFun` / `flatten` are codegen-known canonical-name lowering targets**
  (bodies are ordinary Vesper.Core code), not magic IL — the general pass (§3)
  inserts them at flat↔curried representation-mismatch sites.
- **Flat-first with an arity cap.** Start cap = 2 (`Fun2`); arity 3/4/5 are
  *additive when a concrete combinator demands it*, not speculative. Curried
  `Fun<,>` stays canonical for partial application and arity-over-cap.
- **The struct-seq library threads `'TFunc` BY HAND** (mirroring its explicit
  `'S`/`'E` typars) — that needs no new compiler pass, only the constrained
  dispatch that already exists.

---

## 3. Remaining work: the general lambda → flat-closure lowering pass

Today the pipeline is zero-alloc only when the caller passes a **hand-written
struct closure**. To make `StructSeq.map (fun x -> x+1) s` zero-alloc, codegen must:

1. **Emit `ClosureRepr.Stack` source lambdas as value-structs** implementing
   `Vesper.Fun` / `Fun2`. The `Stack` verdict is ALREADY computed by
   `Passes/Regions.fs` and snapshotted on `TastFile.ClosureReprs` but is **inert**
   — `EmitClosures.fs` still forces `Heap` (`SemanticInfo.fs` `ClosureRepr`). This
   is the core of the pass — codegen emission, not region-analysis augmentation.
2. **Canonicalize function representation + insert adapters.** At a generic
   combinator, an arrow-typed param becomes a `'TF : Fun<…>` (or `Fun2<…>` for a
   saturated 2-arg site) constrained typar; applications lower to
   `constrained. !TF callvirt Invoke`. Insert `curryFun` at partial-application
   sites and `flatten` where a curried value meets a flat slot. Decide ONE
   representation per function-typed slot from the saturated-application count.
   (See `function-representation-plan.md` §"Codegen layer (new)".)
3. **Arity-2 cap.** Saturated 2-arg → `Fun2`; partial → `curryFun`; arity > cap →
   curried chains (unchanged).

### 3.1 Two prerequisite gaps (discovered this session) — ✅ BOTH FIXED

1. ✅ **FIXED (`eed60c7`).** Chained method-call receiver in an interface-impl
   member body — `f.Invoke(a).Invoke(b)` mis-typed the member's return as the inner
   call's result. Root cause: nested method-call `App`s collided on one `NodeKey`
   (both keyed off the receiver's leftmost token). Fixed in `CstKeys.fs` by keying
   such `App`/`HighPrecedenceApp` off the member-name token (per-level
   disambiguation, mirroring nested `DotLookup`/`InfixApp`). The `Flattened.Invoke`
   `let`-split workaround was removed.
2. ✅ **FIXED (`064f8ec`).** Fieldless `[<Struct>]` whose body is only an interface
   impl tripped parse recovery. Root cause: a body leading with `interface` was
   always parsed as an explicit interface *type* (`interface … end`), so a light-
   syntax interface-*impl* body never found its `end`. Fixed in `TypeDefnParsing.fs`
   with a `interface <Type> with` lookahead routing impls to the implicit-class path
   (`TypeDefn.Anon`). Captureless struct closures now parse + codegen end-to-end.

---

## 4. Deferred / out of scope

- **Arity-overloaded project-local classes/interfaces** (give `Types.Class` an
  `(name, arity)` key + update the ~47 name-resolution/freeze/codegen lookups,
  mirroring the union machinery). Independently valuable (F#-compat `FSharpFunc`
  family) but NOT needed while distinct `FunN` names are used. A separate epic.
- **`allows ref struct` / ref-struct closures + ref-struct enumerator `Dispose`**
  (`get-enumerator-gaps.md` item 1) — waits on a byref-like predicate on `SemType`.
  Strictly opt-in, additive constraint-loosening, after the general pass.

---

## 5. Dev workflow & gotchas (read before starting)

- **Methodology** ([[feedback_systematic_tests_over_whackamole]]): smallest
  isolation test that forces the capability → run → diagnose the exact wall → fix →
  iterate. Don't pre-build fixes from theory.
- **Build/test ONLY via `./claude_tools.cmd`** (Bash), never raw `dotnet`:
  - `-Action Build -SourceProject "XParsec.FSharp.SemanticAnalysis"` (or
    `XParsec.FSharp.Codegen.Clr`) — fast F# error check.
  - `-Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests"` — focus one test
    by `test "…"` → `ftest "…"`. `-SummaryLines N` widens output. Full unfiltered
    output is always in `./claude_tools_output.log` (Read it; don't re-run).
  - The **strict gate** is the package path: `buildPackage "<name>"` fails on any
    error diagnostic ([[reference_buildpackage_gates_on_diagnostics]]), unlike the
    lenient inline `compileSource`. Build library changes through it.
  - `-Action Format` (Fantomas) before finishing. Regenerate `.parsed` goldens for
    changed `.fsi`/`.fs` (xparsec-dev `-UpdateSnapshots`); `Vesper.Tests` parses
    each Vesper.Core/Vesper.Seq source. Regenerate the committed `Vesper.Core.dll`
    ref (`REGEN_VESPER_CORE_REF=1`) when Vesper.Core source changes.
- **Crash semantics:** malformed IL crashes the host with
  `Internal CLR error 0x80131506` (ExecutionEngine) and ABORTS the whole parallel
  run — keep exactly ONE `ftest` focused when isolating a crash. A normal managed
  exception (`TypeLoadException`, `InvalidProgram`) = valid-but-wrong IL / bad
  metadata.
- **IL / metadata inspection:** `peMethodIl bytes "Type" "Method"` /
  `peMethodIlWhere bytes "Type" pred` dump raw IL; decode by hand (`ldarg.0`=02,
  `ldfld`=7B, `ldflda`=7C, `ldloca.s`=12, `call`=28, `callvirt`=6F,
  `constrained.`=FE 16, `box`=8C, `initobj`=FE 15, `ret`=2A). `openPe bytes` →
  `GetMetadataReader()` maps tokens → names. Don't `%A` the TAST (EqArray hides
  contents — [[reference_eqarray_percentA_cache_key]]).
- **Guardrails:** don't `git commit` ([[feedback_user_commits]]); temp files in
  `./tmp/` ([[feedback_tmp_dir]]); `constrained. callvirt` is correct ONLY for
  interface (virtual) methods ([[reference_constrained_callvirt_nonvirtual_struct]]);
  a non-generic *local* type reference uses the registered `TypeDef`
  (`env.UserTypes.[key]`), never a `TypeSpec`; mirror F#'s grammar
  ([[feedback_match_fsharp_grammar]]).

## 6. Cross-references

- `brainstorm-seq-module.md` — the north star.
- `function-representation-plan.md` — the §"Codegen layer (new)" / §"Generic
  closures" / §"Region / ref-struct extension" the general pass (§3) realises.
- `get-enumerator-gaps.md` — remaining `for…in` work (item 1 ref-struct `Dispose`,
  co-blocked with `allows ref struct`).
- Memories: `[[project_seq_struct_pipeline_ladder]]` (authoritative landed state),
  `[[project_struct_codegen]]`, `[[project_function_method_compiled_form]]`,
  `[[reference_constrained_callvirt_nonvirtual_struct]]`,
  `[[reference_buildpackage_gates_on_diagnostics]]`,
  `[[reference_infix_op_needs_provider_symbol]]`.
