# T8 — `.fsi`/`.fs` faithfulness: outstanding work

Status (2026-07-01): the `.fsi`↔`.fs` faithfulness SEAM is LANDED, and Sprint A (a real
per-compilation reference-assembly set) is DONE. What remains is two retirements blocked
on unrelated upstream runtime work. EPHEMERAL like all `docs/*-plan.md` — delete once
the items below land (the CODE is the canonical record of everything already done; see
[feedback_plan_docs_ephemeral]).

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

### Sprint A — a real per-compilation reference-assembly set (DONE 2026-07-01)

LANDED. A compilation's BCL surface now comes from its OWN reference set, not the
compiler host's runtime. Anchors (read the code, not this doc):
- `ClrSymbolProviders.bclMetaTailWith` / `buildContractWithRefs` /
  `contractInlineBodiesWithRefs` — the path-taking leaf (per-instance memo; the path
  set enters the contract-cache identity via a `bcl-refs:` tag).
- `Codegen.Clr/RefPack.fs` — TFM → `Microsoft.NETCore.App.Ref` resolver, the
  no-MSBuild convenience; explicit `dllPaths` remains the primary, MSBuild-shaped
  mechanism.
- `Codegen.Clr/ClrDriver.fs` — the library-level production driver (`ClrCompilation`
  record carries `BclReferences` at DRIVER level; `ProjectInfo` stays backend-neutral).
  No CLI yet — the pluggable `vesperc` CLI is a separate design effort.
- `Codegen.compileWithBclReferences` + de-hosted `ClrEnv.coreRef`/`consoleRef` — the
  bootstrap `System.Object`/`Console` `AssemblyRef`s bind the reference identity
  (`System.Runtime`), not the host `System.Private.CoreLib`; identity only, never the
  `materialiseApp` copy set (a ref assembly has no IL and must never ship).
- Acceptance gate: `ClrDriverTests` compiles AND RUNS an app against the pinned net8.0
  ref pack on a net10 test host, asserting ref-pack `AssemblyRef`s and no
  `System.Private.CoreLib`.

The host-TPA conveniences (`bclMetaTail`, `buildContract`, `MetadataSymbols.provider`)
remain as `Codegen.Clr.Tests` conveniences; existing tests stay on them (a systematic
test refactor is a later, separate effort).

### Sprint B — retire `BuiltinOps` (drift inventory Species 2) — ✅ DONE

`BuiltinOps` is deleted. Every operator now emits from its `ops-platform.clr.fs` /
`comparison.clr.fs` contract body, spliced by `SymbolKey` in `Passes.InlineExpansion` —
applied *and* eta'd-as-a-value (that pass eta-reifies an inline external pre-freeze, so
the `App` it mints is spliced by the pass that minted it). Codegen holds no op→opcode
table and recognises no operator by name.

No dynamic-operator runtime (`AdditionDynamic` &c.) was needed after all. The premise
that one was — that the non-inlined eta form has no home — dissolved once the eta moved
pre-freeze. The arithmetic bodies now put the SRTP trait call in the static-optimization
BASE with one explicit clause per primitive, so an operand with no clause and no
resolvable trait call is a *diagnostic* rather than a garbage `add`.

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
- ~~The homogeneous one-typar inline-operator body is a LONG-LIVED simplification, not a
  blocker.~~ **FALSIFIED — it was a miscompile, and it is fixed.** The reasoning here was
  that an inline body has no emitted typar order for the contract to drive (the signature
  is the sole ABI surface; `Freeze` drops inline templates before `checkFile`). True of
  the *ABI*, false of the *body*: the body's typars are the substitution slots
  `InlineExpansion.deriveInlineTypeArgs` fills, one per BODY root, first-ground-wins. With
  one root, a heterogeneous `Vec2 * int -> Vec2` folded both operands into `^T := Vec2`
  and bound the `int` argument into a `Vec2`-typed `let` — type-checking with zero
  diagnostics and emitting a PE that threw `InvalidProgramException`. `ops-platform.clr.fs`'s
  `+ - * / %` now carry the contract's `^T1 / ^T2 / ^T3`.

  The general lesson for this doc: `.fsi`/`.fs` typar drift is **not** confined to the ABI
  just because the ABI is the only thing extracted. An inline body's typars are load-bearing
  at the splice, and nothing checks them — `ConformanceTypars.fs:24-36` exempts `let inline`
  by construction. That exemption is now the last thing standing between this class of bug
  and the compiler; treat it as a gap, not a scope boundary.
