# T8 — `.fsi`/`.fs` faithfulness: a paired-implementation subsystem

Status: PLANNED, not started. This supersedes the earlier "design only" note that
split out of `typar-ordering-unification-plan.md` when T1–T7 landed (commit
`8efadbf1`). It now carries a concrete audit + sequencing. EPHEMERAL like all
`docs/*-plan.md` — delete once T8 lands.

Prerequisite context: T1–T7 unified generic method/value typar ORDERING on the F#
rule through the single `GeneralizedTypars.canonical` (`GeneralizedTypars.fs`) —
see the `project_typar_ordering_unification` memory. T8 is the remaining seam.

## Two intertwined goals

This work has grown from "verify typar order across the seam" into the broader
invariant that **a contract `.fsi` is backed by a real, paired `.fs`** — no
implicit codegen substitution standing in for a missing implementation. The two
goals share one new mechanism (per-binding `.fsi`↔`.fs` pairing), so they ship
together:

- **G1 — Pairing + presence.** Every contract `.fsi` binding has a corresponding
  `.fs` implementation (per target). A `.fsi` binding with no `.fs` is a compiler
  error, matching F#'s FS0240. Intrinsic/operator implementations may NOT drift
  into hard-coded codegen tables or be silently substituted.
- **G2 — Typar-order faithfulness.** For a paired binding, the `.fsi`-declared
  generic type-parameter order DRIVES the emitted/extracted order (the original
  T8), enforced by a conformance check, not just produced by coincidence.

## Problem (why nothing enforces this today)

A package declares its surface in `.fsi` and implements it in `.fs`. F# guarantees
the two agree; here `.fsi` and `.fs` are processed in **entirely separate passes**:

- A package's own `.fsi` becomes a contract/provider surface
  (`ReferencedProject.fs:443-545`), consumed by *downstream* packages.
- The package's own `.fs` is compiled to a DLL in a different pass.
- A package's own `.fsi` is **never loaded while compiling its own `.fs`** — there
  is no per-binding `.fsi`↔`.fs` pairing to hook a check into.

So today's order agreement is "by construction": two implementations of the F#
rule (`GeneralizedTypars.canonical` on the `.fs` side; `VesperLib`'s
`translateCurriedSig` args-first-then-return interning on the `.fsi` side),
cross-referenced in comments + cross-package round-trip tests. And presence is not
enforced at all — see the drift inventory below.

## How F# actually does it (corrected mechanism)

The earlier draft said F# guarantees order "because `GeneralizeVal` prepends the
`.fsi`-declared typars." That is WRONG. `GeneralizeVal` never reads the `.fsi`.
The real chain (FCS at `D:\roboz0r\fsharp\src`):

1. **Impl generalizes independently.** `PlaceTyparsInDeclarationOrder`
   (`Compiler/Checking/Expressions/CheckExpressions.fs:1643`) puts *syntactically
   declared* typars first (`let f<'a,'b>`), then the rest by `freeInTypeLeftToRight`
   first-appearance. No `.fsi` involved.
2. **Signature is published wholesale.** `CheckModuleSignature`
   (`Compiler/Checking/CheckDeclarations.fs:5720-5757`) discards the impl's inferred
   file-signature and returns `sigFileType` — so the `.fsi`'s typar order is what
   the world sees. `SignatureConformance` (`SignatureConformance.fs:129-135,358-363`)
   only checks typar **count-equality + α-equivalence**; it never reorders.
3. **Codegen remaps impl→sig.** `IlxGen.fs:10370-10379` /
   `TypedTreeOps.fs:5561-5661` rewrite impl `Val` references onto the sig `Val`s, so
   the emitted order is the `.fsi`'s.

Pairing key: F# correlates files by `QualifiedNameOfFile`
(`Compiler/Driver/ParseAndCheckInputs.fs:57-80`) — the leading `module Foo.Bar`
declaration if the file is a single named module, else the canonicalized filename.
The "no implementation" error (`ParseAndCheckInputs.fs:1363-1374`):

> `FS0240 buildSignatureWithoutImplementation`: "The signature file '%s' does not
> have a corresponding implementation file. If an implementation file exists then
> check the 'module' and 'namespace' declarations … match."

## Decision

Do the **F#-faithful two-step**, NOT a cheap detection check and NOT the
order-forcing shortcut:

> Pair each `.fsi` binding with its `.fs` implementation; (a) CHECK conformance
> (typar count + α-equivalence, presence) and diagnose mismatches the way F# does;
> then (b) PUBLISH the `.fsi`'s typar order onto the impl (extraction + emit both
> follow the `.fsi`), so drift is impossible by construction.

WHY the full subsystem and not a check: the expensive part is establishing the
per-binding pairing; once paid, *driving* the order is barely more than *comparing*
it, and driving is correct-by-construction. The seed-only shortcut (feed the
`.fsi` typars as `canonical`'s `declared`) is rejected because it would silently
force a non-conforming `.fs` into the `.fsi` order instead of erroring — it
collapses F#'s separate *check* and *publish* steps.

## Build on the existing kernel

`Conformance.fs` (`XParsec.FSharp.SemanticAnalysis/Conformance.fs`) already pairs a
`.fsi`/`.fs`, summarises declared types, and emits the right error family —
`MissingInImpl` (the FS0240 analogue), `MissingInSig`, `ExternWithoutIntrinsic`,
`IntrinsicWithoutExtern`. Three gaps are exactly T8's payload:

1. **Test-only, not a pass.** Invoked solely from `ConformanceTests.fs`, which
   hard-codes the pair list AND pins "known drift" rows as *expected* — drift is
   currently locked in as a golden, not rejected.
2. **Types only, no values.** v1 "does not deep-compare member signatures"; it
   never looks at `let`/`val` bindings, so it cannot check typar order at all.
3. **No manifest-driven pairing.** Pairs are a literal list, not derived from the
   manifest.

T8 = promote `Conformance.check` to a manifest-driven pipeline pass, extend it from
type-presence to value bindings + the faithful two-step, and flip `MissingInImpl`
from pinned-golden to hard error.

## Name resolution (pairing rules)

- **Key:** filename stem (`foo.fsi` ↔ `foo.fs`), matching the existing
  `ReferencedProject.buildProviderWith` extension-rewrite (`fs:443-456`). Add the
  F#-faithful guard: assert the paired files' leading `module`/`namespace`
  declarations agree (what FS0240's message is really about).
- **Per target.** One `.fsi`, N impls: `foo.fs` (base/CLR) + `foo.<target>.fs`.
  The impl candidate set for target T is `resolveImpl T ∪ resolveInlineBodies T`
  (∪ `files-<t>` extras for the sig side). `impl-<t>`/`inline-bodies-<t>` REPLACE;
  `files-<t>` APPENDS (`ReferencedProject.fs:118-138`). The `.fsi` is the single
  invariant every target's `.fs` conforms to.
- **Per-binding, not per-file.** Presence is required only for bindings that demand
  an implementation (concrete `let`/member). Exemptions (below) are evaluated
  per-binding and per-target.

## Drift inventory (audit, 2026-06-28)

Four species of implementation supplied OUTSIDE a manifest-listed `.fs`:

**Species 1 — TWO sources of truth for primitive reprs (front-end `.fs` vs
hard-coded codegen map).** Primitive reprs flow through THREE channels:
1. `IntrinsicReprTypes` (front-end side table) — the compilation unit's OWN `.fs`
   intrinsics, populated by `NameResolution` (`TypeRegistration.fs:459`);
   `SideTables.fs:1224` "holds ONLY this compilation unit's own intrinsic".
2. Provider harvest (`ReferencedProject.buildProviderWith:510-514`) — for
   DOWNSTREAM consumers, harvests `(# repr #)` from a dependency's `.fs` by
   rewriting each `.fsi` in `manifest.Files` to `.fs`. Reads the orphaned
   `prim-types-*.fs` regardless of `impl`; comment: "The `.fs` is the only place
   the repr lives." So the orphaned `.fs` are NOT dead — the front-end uses them.
3. `IntrinsicRepr.defaults` (hard-coded map, `Codegen.Common/IntrinsicRepr.fs:12-32`)
   — the CODEGEN ASSEMBLER's primitive table: `Assembler.fs:37`
   (`merge tast.IntrinsicReprTypes` = defaults overlaid by the unit's own
   intrinsics), `AssemblerScaffold.fs:108` (fixture, no `.fs`),
   `MetadataSymbols.fs:21` (`reprToName` BCL→Vesper reverse), and the parallel
   `tryEncodeValueType` match (`ClrEncoder.fs:127`).

The drift: the front-end (1+2) reads reprs from the `.fs`; the codegen assembler
(3) reads them from a hard-coded duplicate. They can disagree. FIX (a real BACKEND
change, NOT a manifest edit): make the assembler obtain primitive reprs from the
same harvested `.fs` source the front-end uses, so `defaults`/`reprToName`/
`tryEncodeValueType` stop being an independent source. Note: `defaults` CANNOT be
simply deleted — a consumer using `int16` gets `System.Int16` from it (the IL
encoder reads channel 3, not the provider harvest), and `AssemblerScaffold` reads
it directly with no `.fs`. The orphaned `.fs` should still be wired into the
manifest for conformance/presence (G1), but that is orthogonal to the repr-source
unification.

**Species 2 — operator bodies, partially drifted.** `Codegen.Clr/EmitLower.fs:61-82`
(`module BuiltinOps`) hard-codes 18 operators → IL. Mostly paid down (real bodies
in `ops-platform.fs`, spliced by `InlineExpansion`); table is a "DELETE-WHEN-
COMPLETE" fallback for un-ground/eta operator values, emitting monomorphic bodies.
FIX: retire once the inline path covers the residue.

**Species 3 — irreducible target-native fabrication.** `Codegen.Js/JsNativeSymbols.fs`
hand-fabricates `Error` (ctor + `message`) and `IEnumerable`/`IEnumerator` for JS,
which has no reflectable metadata. NOT fixable with a `.fs` — name it as the
explicit target boundary, do not error on it.

**Species 4 — `.fsi` with no `.fs` (classification):**

| `.fsi` | Verdict | Action |
|---|---|---|
| `Vesper.Core/compiler-attributes.fsi` | needs a `.fs` (attribute classes) | **write `compiler-attributes.fs`** |
| `Vesper.Printf/structural-printer.fs` (no `.fsi`!) | port exists, never got a contract | **write `structural-printer.fsi`** (ref: `StructuralFormat.cs`; surface = `RuntimeFormatState : IFormatSink`, `StructuralPrinter.Print`) |
| `Vesper.Printf/printf-format.fsi` (`PrintfFormat`) | NOT dead — cold path instantiates it as **FSharp.Core**'s `PrintfFormat\`4` (`ClrRecipes.fs:170-177`, `ClrEnv.fs:126`) | **self-host `.fs` + retarget cold-path recipe** off FSharp.Core onto the Vesper type. Sequenced dependency on `vesper-printf-plan` cold path; tracked exemption until done. |
| `Vesper.Printf/printf.fsi` (printf/printfn/sprintf) | front-end intrinsic, lowered inline to `Formatter`/`Format` (like operators) | formal exemption |
| `Vesper.Printf/formatter.fsi` ↔ `formatter.fs` | paired, but member-level NEVER verified (Conformance is types-only) | first real client of value-level conformance |
| `Vesper.Exceptions/exceptions.fsi` | impl-free on CLR (BCL-resolved); JS repr via `prim-types-exn` | **per-target** exemption |
| `Vesper.Core/capabilities-compat.js.fsi` | pure type abbreviations | formal exemption |

Note on the C# bootstrap: the "StructuralFormat asm split" blocker is RESOLVED.
PP7d ported the `%A` engine (`StructuralFormat.cs` → `structural-printer.fs`), so a
single Vesper-compiled `Vesper.Printf.dll` now holds both `Formatter` and
`StructuralPrinter` — no second assembly. That Vesper-built DLL is already the live
runtime peer (in-process + on-disk) in the test harness (`PackageBuildTriage.fs:32-34`
asserts it builds BCL-only; `RunnableAppTests`/`SelfHostTests` run it); the C#
`Formatter.cs`/`StructuralFormat.cs` survive ONLY as the `PrintfDifferentialTests`
oracle + C# design-time reference. Dropping them to reference-only is unblocked
today: (1) remove the two `<Compile>` items from `Vesper.Printf.csproj:24-27`;
(2) retire or repoint `PrintfDifferentialTests` (it is the "before the C# handler is
deleted" safety net). The `project_pp6_compile_milestone.md` memory and the csproj's
"No `.fsproj`"/"only `Formatter.cs` is compiled" header are stale (predate PP7d).
The one remaining FSharp.Core tie on this stack is the cold-path `PrintfFormat`
(Species 4 above), orthogonal to the asm split.

## Step 1(b) design — single-source primitive reprs via the provider

The unified-provider design (codegen.x supplies one object that serves the
front-end as `IExternalSymbolProvider` AND the backend through a narrowed API) is
ALREADY REALIZED — for type/member shapes. `SymbolProviders.composeProviders`
builds the front-end stack (layer-1 packages + the layer-2 `MetadataSymbols.provider`
BCL tail); that same instance is handed to the `Assembler` as `symbols` and wrapped
by `CodegenSymbols.ofProvider` (`ClrProvider.fs:30`) into an `ICodegenSymbols` — the
"same object, optionally different API" exactly as originally sketched. The narrowing
adapter (codegen sees only type/member shapes, not `Instantiate`/constraints/inline
bodies) is the right seam and should stay.

The ONE axis that bypasses it is primitive reprs: the encoder read
`IntrinsicRepr.defaults` (`ClrEncoder.fs:122`) instead of the provider. `defaults`
is an early codegen-layer shortcut that was never refactored away — not a deliberate
parallel authority.

⚠️ CORRECTION (probe, T8 1.3): the provider does NOT currently resolve a bare
primitive canon name. `ExternalTypeShape.Intrinsic` is keyed into `ctx.TypeShapes`
by the COMPILED/qualified name (`VesperLib.fs:1342` — `TypeShapes.[compiled]`), so
`TryLookupType "int"` (bare canon, which is all the encoder has) MISSES. Disabling
the bootstrap (`| _ -> None`) failed 819-823/1064 with `cannot encode FTConst("int"
/ "string")` — proving the bootstrap, not the provider, has been the FUNCTIONAL
primary for primitive reprs all along (1.1's read-through fires only for the rare
intrinsic keyed by short name). The repr data exists keyed by short canon in
`ctx.IntrinsicReprs` (`{short → repr}`), it is just not EXPOSED on
`IExternalSymbolProvider` by that key — the mirror of `IntrinsicReverseCanon`
(`{platform → canon}`, which IS exposed) is missing.

So the real unlock for deleting the bootstrap is a bounded provider-API addition:
expose a forward `{canon → platform}` intrinsic map on `IExternalSymbolProvider`
(populate from `IntrinsicReprs` in `ExtractCtx.toProvider`; `Map.empty` for the
metadata/JS-native/test/null providers, exactly like `IntrinsicReverseCanon`), then
have `ClrEnv.TryPrimitiveRepr` query THAT. With 1.3a (Core's channel-1 complete) +
this exposure, every build's primitives resolve through the provider and the
bootstrap is removable. This is the genuine completion of 1.1's intent.

Target architecture (single source of truth = the harvested `.fs`):

- **Encoder forward path:** primitive arm = own-unit `IntrinsicReprTypes` (channel 1)
  → the provider's forward `{canon → platform}` intrinsic map → error. No hard-coded
  production fallback. `IntrinsicRepr.defaults` deleted.
- **Manifest:** every `.fs` is listed; an intrinsic-only `.fs` (`type int =
  (# "System.Int32" #)`, no real bindings) emits NO IL/JS — already the behavior via
  `registerAbbreviationDefn` (`TypeRegistration.fs:424-430`: recorded as a repr, kept
  out of the emitted-type registries). This dissolves the old "minimal DLL subset"
  exception: `impl` = all `.fs`, emission decided per-declaration.
- **Compiling Vesper.Core itself:** the BCL (or JS-native) intrinsic provider —
  already the layer-2 tail of every stack — resolves the `(# "System.Int32" #)` RHS
  to a real type for TypeRef minting (`decimal`→`System.Decimal`); Core's own
  prim-types `.fs` (all wired) populate channel 1 for the name→repr side.
- **The `AssemblerScaffold` / `nullProvider` fixture** gets the REAL Vesper.Core
  provider as its `symbols`, so even codegen unit tests read the single source. (This
  path is an early-test shortcut that predates the provider stack; refactor it away
  rather than keep a fixture-local repr seed.) After this, no hard-coded primitive
  repr map survives anywhere.
- **`tryEncodeValueType` (repr string → `te.Int32()`) STAYS** — it is SRM IL-encoding
  knowledge, not a Vesper contract. Add a conformance assertion that every harvested
  scalar repr is encodable, so a new primitive in a `.fs` cannot silently fail at the
  encoder.

Reverse direction (`MetadataSymbols.reprToName`, BCL→canon): there are TWO
reconciliation paths today, covering different seams — `reprToName` runs at the LEAF,
eagerly (`tryBuildType:74` turns a BCL `System.Int32` member type into `FTConst "int"`
at template-build time; `MetadataSymbols.IntrinsicReverseCanon = Map.empty`), while
`IntrinsicReverseCanon` runs at UNIFICATION, lazily (`Engine.fs:445`, proven for
`System.Exception → exn` off `inherit` chains; complete map folded at the composite
`ExternalSymbols.fs:1010-1012` / unit `SideTables.fs:1273-1280`). Cleanest end-state:
collapse onto the single `IntrinsicReverseCanon` mechanism — the leaf stops
canonicalizing (emits `FTClass(platform-key)`), unification canonicalizes it exactly
as it already does for `exn`; delete `reprToName`. CAVEATS (why this is sequenced
last): it shifts BCL-primitive canonicalization from eager-at-leaf to
lazy-at-unification (every primitive in every member sig now relies on the
reconciliation firing — CONFIRM the `Translate.fs` external-member realization path
routes through reverse-canon, today only proven for nominal heads), and it couples
BCL-primitive canonicalization to Vesper.Core's harvest being present (reverse map
empty without it).

## Sequencing

1. **De-drift Species 1 — single-source primitive reprs** (design above). Internal
   order, low-risk first:
   1. DONE. Encoder reads via `ClrEnv.TryPrimitiveRepr`: own `IntrinsicReprTypes` →
      provider → `defaults` (demoted to bootstrap last resort; `IntrinsicRepr.merge`
      deleted, `Assembler` passes own-only). NOTE: the provider step is currently
      INERT for bare primitives (see the ⚠️ correction — `TryLookupType` is keyed by
      compiled name), so `defaults` is still the functional primary. Step 1.5 fixes
      that.
   2. DONE. `AssemblerScaffold.assembleWith` (+ the `Codegen.assembleMainEmit`/
      `…WithProvider` wrappers) now take an `IExternalSymbolProvider`; the `IlIrTests`
      hand-written-IL fixtures pass the real Vesper.Core provider
      (`SymbolProviders.buildContract [vesperCoreManifest]`), reprs `Map.empty`. The
      scaffold no longer references `IntrinsicRepr.defaults` / `nullProvider`.
   3. DONE (Core). `Vesper.Core/manifest.toml impl` now lists every `.fs` (the
      orphaned intrinsic-only prim-types + `capabilities.fs`); `vesperCoreDll` is now
      MANIFEST-DRIVEN (compiles `resolveImpl None` of the manifest, killing the
      hardcoded-3-file wart and completing Core's channel-1). 1064 green; the
      intrinsic-only files emit zero rows (DLL types unchanged). TODO: the broad
      "all `.fs` in every package's manifest" sweep + per-target `.js.fs`.
   4. DONE — provider forward-repr exposure (the bootstrap unlock). Added
      `IExternalSymbolProvider.IntrinsicForwardRepr : Map<canon,platform>` (built in
      `ExtractCtx.toProvider` from the published `Intrinsic` shapes — mirror of
      `intrinsicReverse`; `Map.empty` in the metadata/JS-native/test/null providers;
      folded in the composite), surfaced on `ICodegenSymbols`, queried by
      `TryPrimitiveRepr` (own `IntrinsicReprTypes` → forward map, NO fallback). The
      `defaults` bootstrap is removed from `ClrEnv`. Re-probe (was 819 failing) → 1064
      green. One MockBuiltins-only consumer test (`SelfHostTests` "FSharp.Core in
      References…") was migrated to `buildContract defaultManifests` (a down-payment on
      the MockBuiltins side goal — it had no channel-1 and relied on the bootstrap for
      `int`). `IntrinsicRepr.defaults` now has ONE consumer left: `reprToName` (1.5).
   5. Reverse-canon unification (deletes `defaults` outright), gated on tests: delete
      `MetadataSymbols.reprToName`, route BCL types through `IntrinsicReverseCanon`
      at the unification seam — AFTER confirming `Translate.fs` realization covers it.
   Keep `tryEncodeValueType` (IL knowledge) + add the "every scalar repr is
   encodable" conformance assertion.
2. **Fill Species 4 pairs.** Write `compiler-attributes.fs` and
   `structural-printer.fsi`. Record the per-target / front-end-intrinsic /
   FSharp.Core-interop exemptions as an explicit, tested list (not silent).
3. **Promote `Conformance.fs` to a manifest-driven pass.** Drive pairing from the
   manifest (stem + module-decl guard, per-target impl set). Replace
   `ConformanceTests`' pinned-drift rows with fixes or the exemption list.
4. **Extend conformance to value bindings + the faithful two-step.** α-equivalence
   + count check, then publish the `.fsi` typar order onto the impl. This is the
   original T8 typar-order payload.
5. **Flip `MissingInImpl` to a hard error**; retire `BuiltinOps` (Species 2) and
   the FSharp.Core `PrintfFormat` substitution (Species 4) where a `.fs` now covers
   them.
6. **Extend to generic MEMBERS** once published cross-package (drop the
   `VesperLib.fs` `MethodArity = 0` hard-code, `VesperLib.fs:1105`).

## Side goal — remove `MockBuiltins`

`MockBuiltins` (`test/XParsec.FSharp.SemanticAnalysis.Tests/MockBuiltins.fs`, ~138
lines) is an early bootstrap hack: a VALUE-symbol-only fixture (monomorphic ops
`(+): int→int→int`, pipe/compose, `List.fold`, printf, `failwith`, `hash`) that
deliberately diverges from the real SRTP contract and provides ZERO type shapes
(`TryLookupType _ = ValueNone`). Its header already says callers needing real
behaviour should wire `ReferencedProject`/`VesperLib` providers instead; see the
`feedback_mockbuiltins_is_a_trap` / `project_contract_demotion` memories. The real
contract infra (`SymbolProviders.buildContract` over the actual manifests) is a
superset and is what the codegen path already treats as primary (MockBuiltins is the
"lowest-priority backstop"). Removing it entirely is worthwhile and overlaps T8 step
1.4/1.5 (it's why primitive reprs don't flow through the provider in MockBuiltins
builds), but it is its OWN effort (~27 files), staged:

- **Group A — codegen tests** (`Codegen.Clr.Tests`/`Codegen.Js.Tests`, mostly via
  `TestHelpers`): already nearly migrated; swap the few `MockBuiltins.provider`
  uses (`analyse`, the package `*Dll` fixtures) for `buildContract`. Low risk —
  these compile real Vesper source and the real provider is a superset.
- **Group B — SA front-end tests** (the bulk: `UnificationTests`, `FreezeTests`,
  `NameResolutionTests`, …): the riskier half — migrating to the real SRTP contract
  may shift some inferred types (the monomorphic-op divergence) and need test
  updates. Do after Group A.
- Then delete `MockBuiltins.fs` and its `.fsproj` includes.

Note: removing the `defaults` bootstrap (1.5) does NOT strictly require finishing
MockBuiltins removal — once the provider exposes forward reprs, even a
MockBuiltins-backstopped build resolves primitives through the real Core provider in
the stack. But the two are kin (both are "the real provider should be the source").

## Why exposure was low while deferred

- The double-implementation + round-trip tests catch realistic drift for module
  FUNCTIONS.
- Generic *members* aren't published cross-package yet (`MethodArity = 0`).
- T4's correct-by-construction carrier removed the `.fs`-internal divergence; T8 is
  purely the `.fsi`↔`.fs` seam.

## Tests to add

- A package whose `.fsi` declares `<'b,'a>` for a binding the `.fs` infers `'a,'b`
  → emitted/extracted order follows the `.fsi`; a downstream consumer's MethodSpec
  lines up.
- `formatter.fsi` ↔ `formatter.fs` member-level conformance (first value-level
  client) goes green.
- A `.fsi` binding with no `.fs` (a deliberately deleted impl) → hard FS0240-style
  error, NOT a pinned golden.
- Per-target: `exceptions.fsi` conforms (impl-free) on CLR and via `prim-types-exn`
  on JS.
- Round-trip: `.fsi` extract → downstream consume → call, asserting GenericParam
  order == MethodSpec order for a declared-≠-appearance binding.
