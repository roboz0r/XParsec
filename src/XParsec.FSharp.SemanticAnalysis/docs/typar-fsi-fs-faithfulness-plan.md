# T8 — `.fsi`/`.fs` faithfulness: outstanding work

Status (2026-06-30): the `.fsi`↔`.fs` faithfulness SEAM is LANDED. What remains is one
genuine sprint (a real per-compilation reference-assembly set, "Sprint A" below) plus
two retirements blocked on unrelated upstream runtime work. EPHEMERAL like all
`docs/*-plan.md` — delete once the three items below land (the CODE is the canonical
record of everything already done; see [feedback_plan_docs_ephemeral]).

This file was rewritten down from the full design+history once Steps 1–6 landed. If you
need the blow-by-blow of a finished step, read the code and `git log`, not this doc.

## What the subsystem guarantees (LANDED — context, not work)

Two goals, one mechanism (per-binding `.fsi`↔`.fs` pairing):

- **G1 — pairing + presence.** Every contract `.fsi` binding has a paired `.fs` (per
  target); a `.fsi` with no `.fs` (and not declared `[core] sig-only`) is an FS0240-style
  hard error. Intrinsic/operator impls may not silently drift into codegen tables.
- **G2 — typar-order faithfulness.** A paired binding's `.fsi`-declared generic
  type-parameter order DRIVES the emitted/extracted order, enforced by a conformance
  check, not coincidence.

Realized by (all DONE; anchors for the curious):
- `Conformance.fs` / `ConformancePass.fs` — CST-level pairing, presence, extern/intrinsic
  pairing, the `extern class` heritable-base species, the module-decl guard. Manifest-
  driven (`checkManifest`), hard-gated via `ConformancePass.enforce` (codes `V240`–`V243`)
  into the package build (`Codegen.Clr.Tests/TestHelpers.buildPackage`).
- `ConformanceTypars.fs` — SEMANTIC typar-order conformance. `checkFile` (module
  functions, single typar axis, `schemesAgree`/`normAxis`) + `checkMembers` (generic type
  MEMBERS, two axes, direct structural equality over `TryLookupMembers`). Generic members
  are now PUBLISHED cross-package (`VesperLib` `reaxisMethodTypars` + computed
  `MethodArity`; the old `MethodArity = 0` member hard-code is gone — ctors stay `0`).
- Single-source primitive reprs: the encoder reads reprs via the provider's harvested
  forward `{canon → platform}` map (own-unit `IntrinsicReprTypes` → provider, no codegen
  fallback); `IntrinsicRepr.defaults` / `reprToName` deleted. `IntrinsicRepr.tryEncodeValueType`
  (SRM IL knowledge) STAYS, single-sourced with `isEncodableValueType`, and a repr-
  encodability conformance test (`Codegen.Clr.Tests/IntrinsicReprConformanceTests.fs`)
  pins that every directly-encodable scalar primitive's harvested repr is encodable.
- `MockBuiltins` fully removed; every test resolves through real `Vesper.*` contracts.

Key memories: [project_typar_ordering_unification], [project_contract_demotion],
[project_intrinsic_repr_provider_native], [feedback_mockbuiltins_is_a_trap].

## Outstanding work

### Sprint A — a real per-compilation reference-assembly set (the next sprint)

**The problem.** The .NET metadata provider's leaf is still built from the *compiler
host's* runtime assemblies — `bclMetaTail` calls `MetadataSymbols.runtimeAssemblyPaths ()`
(the host TPA) — not from the *compilation's own* reference set. In a real .NET build the
BCL surface a program sees is the **reference pack for its target TFM** plus its
`<Reference>`/`<PackageReference>` assemblies, which is NOT the host's runtime. So today
every compile "cheats" by reflecting over whatever assemblies happen to be loaded in the
test host. This is correct enough for the self-host suite (host ≈ target) but is wrong in
principle and blocks compiling against a pinned TFM / a non-host BCL.

**Status.** The surrounding restructuring already LANDED (Step 1.5 a/b/c-partial/d):
`MetadataSymbols` lives in `Codegen.Clr`; `Codegen.Common.SymbolProviders` is leaf-
agnostic (`composeProviders` threads a `MetaTailFactory` into both the per-package
`depComposite` and the final composite); the BCL-defaulting conveniences live in
`Codegen.Clr.ClrSymbolProviders`; the leaf is seeded with the harvested `{platform →
canon}` reverse map and `tryBuildType` canonicalizes through it (sealed-BCL-types only —
the unsealed subtype roots `System.Object → obj` / `System.Exception → exn` reconcile at
the unification bridge, NOT eagerly at the leaf, or ctor/`new`/subtype resolution breaks).

**What's left — three pieces (a genuine design step, not a mechanical follow-on):**
1. A path-taking `bclMetaTail` / convenience variant (build the leaf from an explicit
   `dllPaths`, not `runtimeAssemblyPaths ()`).
2. A way to SOURCE the target TFM's reference pack + referenced assemblies. Today
   `ProjectInfo.References` (`Codegen.Clr/ProjectInfo.fs`) carries only `Vesper.*` +
   optional FSharp.Core — NOT the BCL ref pack. This is the crux: decide where the ref-
   pack path set comes from (MSBuild-resolved list passed in? a TFM → ref-pack resolver?).
3. A real production CLR driver that calls `buildContract` with that set. None does yet —
   the only consumers are tests, which is why the host-TPA shortcut has survived. The
   host-TPA singleton should remain ONLY as a `Codegen.Clr.Tests` convenience
   (`MetadataSymbols.provider` for `MetadataSymbolsTests`); the leaf-empty-reverse
   (extraction-composite) optimization currently relies on it, so don't delete it
   wholesale.

Once this lands, every build's BCL surface comes from its own reference set; the
"cheating with the test host's assemblies" note disappears.

### Sprint B — retire `BuiltinOps` (drift inventory Species 2)

`Codegen.Clr/EmitLower.fs` `module BuiltinOps` hard-codes 18 operators → IL, a
"DELETE-WHEN-COMPLETE" fallback for un-ground / eta operator values (it emits monomorphic
bodies). Most operator emission already flows through real `ops-platform.fs` bodies spliced
by `InlineExpansion`; this is the residue.

**Blocked on:** the homogeneous `^T -> ^T -> ^T` inline-operator body's non-inlined (eta /
`reduce (+)`) form needs the ported **dynamic-operator runtime** (`AdditionDynamic` &c.),
which Vesper has not ported — owned by the per-target inline-IL stack, NOT this seam (see
[project_inline_il_target_specific]). NOT a `.fsi`-substitute-for-missing-`.fs`, so the
hard gate does not regress on it; it is a pure codegen EMISSION fallback. Retire once the
inline path covers the residue.

### Sprint C — retire the FSharp.Core `PrintfFormat` substitution (Species 4)

The cold printf path still instantiates FSharp.Core's `PrintfFormat\`4` (`ClrRecipes.fs`,
`ClrEnv.fs`); `Vesper.Printf/printf-format.fsi` has no `.fs`.

**Blocked on:** the vesper-printf cold-path self-host (self-host `printf-format.fs` +
retarget the cold-path recipe off FSharp.Core onto the Vesper type). Until then
`printf-format.fsi` is a declared `[core] sig-only` exemption, so it is ENFORCED as a known
impl-free contract (not a silent gap). The asm-split blocker is already resolved (PP7d);
this is the one remaining FSharp.Core tie on the printf stack.

## Irreducible non-targets (name them, don't "fix")

- **`Codegen.Js/JsNativeSymbols.fs`** hand-fabricates `Error` (ctor + `message`) and
  `IEnumerable`/`IEnumerator` for JS, which has no reflectable metadata. NOT fixable with a
  `.fs` — it is the explicit JS target boundary, not drift.
- **`IntrinsicRepr.tryEncodeValueType`** (repr string → `te.Int32()`) is SRM IL-encoding
  knowledge, not a Vesper contract. STAYS (single-sourced with `isEncodableValueType`).
- The homogeneous one-typar inline-operator body (`ops-platform.fs` `(+)` as
  `^T -> ^T -> ^T` vs the contract's general `^T1 -> ^T2 -> ^T3`) is a LONG-LIVED
  simplification, not a T8 blocker — inline bodies have no emitted/extracted typar order
  for the contract to drive (the signature is the sole ABI surface; `Freeze` drops inline
  templates so they never reach `checkFile`/`checkMembers`). "T8 done" does NOT assert
  operator FSharp.Core parity. Same dependency as Sprint B.
