# T8 — `.fsi`/`.fs` faithfulness: a paired-implementation subsystem

Status (2026-06-29): IN PROGRESS. Steps 1 (except the 1.5c "per-compilation leaf paths"
sub-step, still PARTIAL — orthogonal to 4.2), 2, 2(a), 3, 4.1 (+ the 4.1b accuracy
refinement), **4.2**, and **the Step 5 hard-error flip** are DONE — see the per-step
DONE/PARTIAL markers in **Sequencing**, the authoritative tracker. **The next unstarted
work is Step 6** (generic members). Step 5's two RETIREMENT clauses (`BuiltinOps`,
FSharp.Core `PrintfFormat`) remain deferred on their stated upstream dependencies (the
dynamic-operator runtime; the vesper-printf cold path) — see Step 5 in Sequencing.
This supersedes the earlier "design only" note that split out of
`typar-ordering-unification-plan.md` when T1–T7 landed (commit `8efadbf1`). EPHEMERAL
like all `docs/*-plan.md` — delete once T8 lands.

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

`Conformance.fs` (`XParsec.FSharp.SemanticAnalysis/Conformance.fs`) pairs a `.fsi`/`.fs`,
summarises declared types + module-level values, and emits the error family
`MissingInImpl` (the FS0240 analogue), `ExternWithoutIntrinsic`, `IntrinsicWithoutExtern`,
`HeritabilityMismatch`, `ValueMissingInImpl`. (The old `MissingInSig` was DELETED in 4.1b:
an impl type absent from the sig is a HiddenTycon, F#-legal, not drift.) Three gaps were
T8's payload — ALL now closed:

1. ~~Test-only, not a pass.~~ CLOSED (Step 3): `ConformancePass.fs` derives pairs from the
   manifest; no hard-coded pair list, no pinned "known drift" golden (`acceptedFindings`
   emptied in 4.1b — real drift fixed at source, not accepted).
2. ~~Types only, no values.~~ CLOSED (Step 4.1: presence; Step 4.2: typar order via the
   semantic `ConformanceTypars` kernel). Type MEMBERS still open — Step 6.
3. ~~No manifest-driven pairing.~~ CLOSED (Step 3).

T8 = promote `Conformance.check` to a manifest-driven pass (DONE), extend it from
type-presence to value bindings (presence DONE; typar-order = 4.2) + the faithful
two-step, and flip `MissingInImpl` from a finding to a hard error (Step 5).

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
   `MetadataSymbols.fs:20` (`reprToName` BCL→Vesper reverse), and the parallel
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
| `Vesper.Core/compiler-attributes.fsi` | needs a `.fs` (attribute classes inheriting `Attribute`) | **DONE** — `compiler-attributes.fs` written + in Vesper.Core `impl`; unblocked by the `extern class` heritable-base construct (Step 2(a) below). The 8 attribute classes emit, inherit `System.Attribute`, and construct (verified at runtime). |
| `Vesper.Printf/structural-printer.fs` (no `.fsi`!) | port exists, never got a contract | **write `structural-printer.fsi`** (ref: `StructuralFormat.cs`; surface = `RuntimeFormatState : IFormatSink`, `StructuralPrinter.Print`) |
| `Vesper.Printf/printf-format.fsi` (`PrintfFormat`) | NOT dead — cold path instantiates it as **FSharp.Core**'s `PrintfFormat\`4` (`ClrRecipes.fs:170-177`, `ClrEnv.fs:126`) | **self-host `.fs` + retarget cold-path recipe** off FSharp.Core onto the Vesper type. Sequenced dependency on `vesper-printf-plan` cold path; tracked exemption until done. |
| `Vesper.Printf/printf.fsi` (printf/printfn/sprintf) | front-end intrinsic, lowered inline to `Formatter`/`Format` (like operators) | formal exemption |
| `Vesper.Printf/formatter.fsi` ↔ `formatter.fs` | paired; module-level value presence now checked (4.1), but its MEMBERS (overloaded `AppendFormatted`, ctors) are still unverified | first real client of MEMBER-level conformance = Step 6 |
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

Reverse direction (`MetadataSymbols.reprToName`, BCL→canon): `reprToName` runs at the
LEAF, eagerly — `tryBuildType:74` turns a BCL `System.Int32` member type into
`FTConst "int"` at template-build time, so a Vesper `int` argument can be passed to a
BCL member. This is the .NET symbol provider mapping an IL-declared method onto Vesper
types so semantic analysis knows how/if to call it; it is NOT a codegen concern.

REJECTED end-state (the earlier draft): "leaf stops canonicalizing, unification
canonicalizes `int ≡ System.Int32` as it does for `exn`." `int` must NOT unify with
`System.Int32`. The unify route keys reconciliation on the platform repr, which is
many-to-one per target — on JS both `int` and `float` are `number`. (The `exn`
precedent is safe only because `System.Exception ↔ exn` happens to be 1:1.) Primitive
identity stays nominal and target-invariant; the platform repr is a per-target
*attribute* the side tables already carry (forward `IntrinsicForwardRepr`, reverse
`IntrinsicReverseCanon`), not a basis for type identity.

The many-to-one reverse case (AGREED 2026-06-28): when a platform repr maps to several
canons (`number → {int, float}`), the reverse lookup yields the UNION
`TyOr(["int"; "float"])` (`FTOr` at the frozen layer), not an arbitrary single winner —
the incoming value is "one of these primitives", narrowed by context. The CLR metadata
leaf is 1:1 (`System.Int32 → int`), so today's reverse map is `Map<string, string>`;
the type widens to a union value (`Map<string, FrozenType>` / a canon-set → `FTOr`) when
the JS-native / TS providers actually populate a reverse map (both `Map.empty` now). The
1:1 CLR mechanism below does not foreclose it.

AGREED end-state — the leaf keeps canonicalizing, but driven by the *dynamically
harvested* relationship instead of the static `reprToName`/`defaults` table, AND the
.NET provider is restructured (see "Step 1.5 (revised)" below):

- `System.Int32 → int` is the reverse face of `type int = (# "System.Int32" #)`,
  already harvested into `IntrinsicReverseCanon` (`TyparCapture.fs:378`) when that
  declaration is analysed — the same side table `canonName` uses for `exn`. The leaf's
  canonicalization is seeded from that harvested `{ platform → canon }` map (folded
  from the layer-1 providers' `IntrinsicReverseCanon`), not the `defaults`-derived
  `reprToName`. Delete `reprToName` + `IntrinsicRepr.defaults`.
- CAVEAT (chicken-and-egg): Vesper.Core's OWN extraction runs before its `int↔System.
  Int32` relationship is harvested, so its leaf sees an empty reverse map. Fine iff
  Vesper.Core's `.fsi` member signatures never name a BCL primitive by its `System.*`
  name (they use `int`/`string`; the `(# … #)` RHS is handled by
  `registerAbbreviationDefn`, not member-sig canonicalization). CONFIRM before landing.

## Step 1.5 (revised) — per-compilation .NET symbol provider in Codegen.Clr

The reverse-canon plumbing forced a deeper, correct restructuring of the BCL provider
(agreed 2026-06-28). The .NET metadata provider is .NET-specific and per-compilation;
nothing in this is target-generic.

STATUS (see sequencing 1.5 for the authoritative per-substep state): the physical move
+ leaf-agnostic Common + reverse-map seeding all LANDED (1.5 a/b/c-partial/d). The one
piece still open is the last bullet below — **per-compilation leaf paths**: `bclMetaTail`
still builds the leaf from the compiler host's TPA (`MetadataSymbols.runtimeAssemblyPaths
()`), not from the compilation's own reference set. It needs (1) a path-taking
`bclMetaTail`/convenience variant, (2) a way to source the target TFM's reference-pack +
referenced assemblies (today `ProjectInfo.References` carries only `Vesper.*` + optional
FSharp.Core, NOT the BCL ref pack), and (3) a real production CLR driver consumer (none
calls `buildContract` yet). That is a separate design step, not a mechanical follow-on.

Target architecture:

- **`MetadataSymbols` moves `Codegen.Common` → `Codegen.Clr`.** A `System.Reflection`-
  backed provider is .NET-specific; JS / future targets have no reason to want it.
  SemanticAnalysis never names it (it consumes `IExternalSymbolProvider` abstractly),
  so the front end is unaffected.
- **No singleton in production.** `MetadataSymbols.provider` today is
  `create (runtimeAssemblyPaths ())` — a process-wide leaf over the *compiler host's*
  TPA. Replace with `MetadataSymbols.create dllPaths`, constructed per-compilation from
  the **compiler context** — the reference-assembly list a real .NET compilation gets
  from MSBuild (today: `ProjectInfo.References`, `Codegen.Clr/ProjectInfo.fs`), NOT the
  host runtime. The host-TPA singleton survives ONLY as a convenience in
  `Codegen.Clr.Tests` (`MetadataSymbols.provider` for `MetadataSymbolsTests`).
- **`SymbolProviders` (Common) becomes leaf-agnostic.** Drop `bclMetaTail` and the
  hardcoded `MetadataSymbols.provider` at the per-package `depComposite`
  (`SymbolProviders.fs:50`) — `composeProviders` already takes a `metaTail` for the
  final composite; thread that SAME tail into the depComposite so Common never names a
  concrete leaf. (Aside: today the depComposite is BCL even on a JS build — a latent
  inconsistency this fixes.) The BCL-defaulting conveniences (`build`/`buildContract`/
  `buildContractFor`/`contractInlineBodies*`) move to a CLR-side module that injects the
  per-compilation metadata tail; `buildContractWithMetadata` (the injection seam) stays
  in Common. The JS backend already injects its own tail and is unaffected.
- **Reverse map seeding.** `composeProviders` folds the layer-1 providers'
  `IntrinsicReverseCanon` into a `{ platform → canon }` map and seeds the per-build
  metadata leaf with it; `tryBuildType` canonicalizes through that map. No static table.

Blast radius: ~60 `buildContract*` call sites, almost all in tests (Clr.Tests +
8 in Js.Tests — the JS tests' use of the BCL-defaulted `buildContract` is itself
suspect and resolved as part of this). No production CLR driver calls these yet.

## Step 2(a) — heritable external base classes: the `extern class` construct

Writing `compiler-attributes.fs` surfaced a language gap: there is no way to say "this
external type is a heritable reference base," only "this external type is an opaque
value repr." Both spellings collapse to the same channel today —

| | `.fsi` contract | `.fs` impl | front-end registry |
|---|---|---|---|
| `int` (opaque value) | `type int = extern` | `type int = (# "System.Int32" #)` | `IntrinsicReprTypes` |
| `Attribute` (heritable base) | `[<AbstractClass>] type Attribute = extern` | `type Attribute = (# "System.Attribute" #)` | `IntrinsicReprTypes` |

`registerAbbreviationDefn` (`TypeRegistration.fs:458`) shoves ANY `(# … #)` RHS into
`IntrinsicReprTypes` as an opaque name→repr string. That table is correct for `int`
and wrong for `Attribute`: it feeds (1) the encoder (`tryEncodeValueType`: repr →
`ELEMENT_TYPE_I4`) — `Attribute` is never an encodable scalar — and (2) the
`resolveInheritParent` rejection ("Cannot inherit from type 'Attribute' — only classes
are inheritable", `MemberRegistration.fs:783`), because an `IntrinsicReprTypes` entry
is by construction not a class. There is no prior CODEGEN precedent for extending a BCL
base through Vesper's own backend: the only `inherit exn` carrier (`exceptions.fsi`) is
impl-free (BCL-resolved, never compiled); JS handles `exn`→`Error` by repr harvest, not
by emitting an `extends`.

**Premise (agreed): two species of external type, marked at the declaration site.**
1. **Opaque value repr** — `int/bool/float/string`: encodable IL element type, sealed,
   never a base. Keeps `(# "…" #)`.
2. **Heritable reference base** — `Attribute` (and conceptually `obj`/`exn`): a TypeRef
   you may `inherit`, emitted with `extends` + a base-ctor call; never an encodable value.

**Construct (agreed): each side marks heritability in its OWN native syntax**, so that
either file is unambiguous read in isolation (no reliance on an attribute or on the
paired file). The `.fsi` already says `extern`; the `.fs` already says `(# … #)` — extend
each with a `class`/`interface` kind tag. `(# … #)` WITHOUT the tag stays exclusively the
opaque-value-repr form.

```
// contract .fsi  — capability, no repr (harvested from impl)
type Attribute = extern class

// impl .fs       — heritable external class bound to its BCL TypeRef
type Attribute = (# class "System.Attribute" #)
```

The `class` keyword is unambiguous in both positions: after `extern` no verbose `… end`
body follows (the parser commits on `extern class`), and inside `(# … #)` the hash-paren
delimiters bound it (it cannot be read as a verbose class body). Generalizes to
`extern interface` / `(# interface "…" #)`.

**Implementation surface:**
- **Parser/AST.** Sig side: extend `TypeSignature.Extern` (`Signatures.fs:113`) with an
  optional `class`/`interface` kind token after `externToken`. Impl side: extend
  `Type.ILIntrinsic` (`Expr.fs:87`) with an optional leading `class`/`interface` kind
  token before the repr string (`TypeParsing.fs:419-425`). Plus `AstTraversal` + golden
  snapshots. The tagged `(# class "…" #)` is a heritable external class; the untagged
  `(# "…" #)` is unchanged (opaque value repr).
- **Front-end registration.** New `ExternalClassTypes` table (name → repr / qualified
  BCL TypeRef). `registerAbbreviationDefn` (`TypeRegistration.fs:458`) routes a TAGGED
  `Type.ILIntrinsic` here instead of `IntrinsicReprTypes`; `translateType` resolves the
  name to an external-class `SemType` (not a `TyConst` opaque). Provider extraction
  surfaces the `extern class` sig as a class shape so downstream `inherit` resolves.
- **`resolveInheritParent`** (`MemberRegistration.fs:728`): accept an `ExternalClassTypes`
  name, returning the external-class base `SemType` (+ its base-ctor for the synthesized
  primary ctor).
- **Codegen.** Mint a TypeRef from the repr (`icodegen.TypeToken`, the path
  `NominalEmit.fs:373` already takes for non-local bases), set `BaseType = extends`, and
  emit the base-ctor `call instance void System.Attribute::.ctor()`. This is the first
  emitted `extends`-BCL; verify the protected-ctor call resolves.
- **Conformance.** New pairing rule `extern class` (sig) ↔ `(# class "repr" #)` (impl),
  alongside the existing `extern` ↔ `(# … #)`. `Conformance.SigShape`/`ImplShape` grow a
  heritable-class arm so the two species don't cross-pair.

**Payoff.** `compiler-attributes.fs` migrates its eight bases to `(# class
"System.Attribute" #)` (and the `.fsi` to `extern class`), joins Vesper.Core `impl`, and
the single-source invariant holds. The construct also unblocks any future Vesper-compiled
type that must extend a BCL base, and gives `exn`/`obj` a principled spelling if their
handling is ever unified here.

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
   5. Per-compilation .NET provider + dynamic reverse map (deletes `defaults` +
      `reprToName` outright). The full restructuring is "Step 1.5 (revised)" above;
      sub-steps, low-risk first:
      a. DONE. `composeProviders` is leaf-agnostic: `metaTail` is now a
         `MetaTailFactory` (`Map<platform,canon> -> provider list`), threaded into BOTH
         the per-package `depComposite` and the final composite. The hardcoded
         `MetadataSymbols.provider` at `:50` is gone.
      b. DONE (the physical move). `MetadataSymbols.fs` moved `Codegen.Common` →
         `Codegen.Clr` (namespace + `System.Reflection.MetadataLoadContext` package ref
         followed it). `Codegen.Common.SymbolProviders` is now leaf-agnostic: it names no
         concrete leaf, exposing the metaTail-parameterized `buildWith` /
         `buildContractWith` (+ the existing `buildContractWithMetadata` injection seam).
         The BCL-defaulting conveniences (`build`/`buildContract`/`buildContractFor`/
         `contractInlineBodies[For]`) moved to the new CLR-side
         `Codegen.Clr.ClrSymbolProviders`, which supplies `bclMetaTail`. The host-TPA
         `MetadataSymbols.provider` singleton is NOT relocated to tests after all — it is
         load-bearing for `bclMetaTail`'s empty-reverse (extraction-composite)
         optimization, so it stays as Clr production infra (no test referenced it
         directly). ~50 CLR-test call sites repointed to `ClrSymbolProviders`. The
         Js.Tests use was resolved by SPLITTING `OpsPlatformJsTests`: the JS-target
         assertions stay there, now built through `JsNativeSymbols.jsNativeInlineBodiesFor`
         / `buildJsNativeContractFor` (JS-native leaf, no CLR-backend dependency); the two
         cross-target / CLR-repr assertions (which need the BCL leaf) moved to the new
         `Codegen.Clr.Tests.OpsPlatformClrTests`. Suites green (CLR 1064→1066, JS 177→175).
      c. PARTIAL. The leaf is now SEEDED with the harvested `{ platform → canon }` map
         (folded from layer-1 `IntrinsicReverseCanon` in `composeProviders`) and
         `tryBuildType` canonicalizes through it — but only SEALED BCL types
         (`t.IsSealed` guard): scalar primitives + `string` canonicalize; the unsealed
         subtype ROOTS (`System.Object → obj`, `System.Exception → exn`) and capability
         interfaces keep their BCL nominal form and reconcile at the unification bridge
         (`Engine.canonName`) as before — eager leaf-canon of those breaks ctor/`new`/
         subtype resolution (8 InferResolution + base-type tests). Still TODO: build the
         leaf from the compiler context (`ProjectInfo.References`) instead of the host
         runtime TPA (currently `MetadataSymbols.runtimeAssemblyPaths ()`).
      d. DONE. `reprToName` + `IntrinsicRepr.defaults` deleted. `IntrinsicRepr` now holds
         only `tryEncodeValueType` (SRM IL knowledge). A contract built without
         Vesper.Core (`build []`) no longer canonicalizes BCL primitives (the
         relationship lives in Core's `prim-types`); two `ExternalMemberTests` that used
         `build []` now pass `[ vesperCoreManifest ]`. `MetadataSymbolsTests` seeds its
         leaf from the real harvested map (`buildContract [vesperCoreManifest]
         |> .IntrinsicReverseCanon`). All suites green (CLR 1064, JS 177, SA 643, Vesper 49).
   Keep `tryEncodeValueType` (IL knowledge) + add the "every scalar repr is
   encodable" conformance assertion (TODO).
2. **Fill Species 4 pairs.**
   - DONE. `structural-printer.fsi` written + wired into `Vesper.Printf/manifest.toml`
     `files`; encapsulates the layout internals, publishes `StructuralPrinter.Print` +
     `RuntimeFormatState : IFormatSink`. `ConformanceTests` gains the green pair-adjacent
     `knownDriftPairs` row (`Doc`/`FrameKind`/`Frame` private). Contract extraction
     verified (134 `PrintfHappyPath` green).
   - DONE. Exemptions recorded as the tested `implFreeExemptions` list in
     `ConformanceTests` (front-end-intrinsic `printf.fsi`, FSharp.Core-interop
     `printf-format.fsi`, per-target `exceptions.fsi`, type-abbreviation
     `capabilities-compat.js.fsi`) — each test-asserted to lack a companion `.fs`, so
     adding an impl forces promotion to `conformingPairs`. Replaces the silent omission.
   - DONE. `compiler-attributes.fs` written; its source-level conformance pair is green
     (added to `conformingPairs`).
   - BLOCKED → Step 2(a). `compiler-attributes.fs` cannot join Vesper.Core `impl` (the
     DLL compile) until the `extern class` heritable-base construct lands — inheriting the
     intrinsic-repr `Attribute` base is rejected today. Reverted from `impl` to keep the
     tree green.
   2a. **Implement `extern class` / `(# class "repr" #)`** (design: "Step 2(a)" above).
       - DONE. Parser/AST: `Type.ILIntrinsic` (`Expr.fs`) + `TypeSignature.Extern`
         (`Signatures.fs`) gained an optional `kindTag: 'T voption`; parsed in
         `TypeParsing.fs` (`opt (class|interface)` inside `(# … #)`) and
         `SignatureParsing.fs` (`opt class` after `extern` — `interface` deferred there,
         it collides with an `interface …` capability member; the impl-side `(# … #)`
         has no clash so it takes both). `AstTraversal` walks the tag. Golden inputs
         `370_extern_class_intrinsic.fs` + `sig_17_extern_class.fsi` added; snapshots
         regenerated; whole solution + FSharp.Tests green (tag present iff tagged).
       - DONE (front-end). `TypeRegistration.registerAbbreviationDefn` routes a TAGGED
         `Type.ILIntrinsic` into the new `HeritableExternBases` set (`SideTables.fs`)
         alongside the repr in `IntrinsicReprTypes`. `resolveInheritParent`
         (`MemberRegistration.fs`) admits a `HeritableExternBases` name and returns the
         base as `TyClass(extKey, [])` where `extKey` is resolved FROM THE REPR via
         `NameResolutionScope.tryResolveExternalTypeKey` (now exposed) — an `FTClass`
         keyed to the external `System.Attribute`, which the codegen `ExternalClass`
         encoder path resolves.
       - DONE (codegen). The `extends` column: an external non-generic base resolves to
         its raw external `TypeRef` (new `ICodegenProvider.ExternalClassTypeRef`) instead
         of `provider.UserTypeHandle` (`NominalEmit.fs`). The base-CTOR chain: a new
         `ICodegenProvider.ExternalParameterlessBaseCtor` mints a `MemberRef` to
         `System.Attribute::.ctor()` DIRECTLY off the external `TypeRef` (the `protected`
         base ctor isn't in the member harvest; `call`ing it from a subclass ctor is
         legal); `NominalEmit` intercepts an external base before the local-`classes`
         path and chains to it. Verified by a `PackageBuild` runtime assertion:
         `Vesper.StructuralEqualityAttribute.BaseType = typeof<System.Attribute>` and
         `Activator.CreateInstance` succeeds (base-ctor IL runs).
       - DONE. Conformance grew `SigShape.ExternClass` / `ImplShape.IntrinsicClass` +
         `HeritabilityMismatch`; pairs `extern class` ↔ `(# class "repr" #)` and flags a
         tag mismatch (tests added). `prim-types-attr` migrated (`extern class` /
         `(# class "System.Attribute" #)`); `compiler-attributes.fs` added to Vesper.Core
         `impl`. All suites green (Clr 1066, SA 652, JS 175, goldens 1422, Vesper 49).
       This completes Step 2(a) and unblocks the `compiler-attributes` row of Step 2.
3. **Promote `Conformance.fs` to a manifest-driven pass.** DONE. New
   `ConformancePass.fs` (after `ReferencedProject.fs`, so it sees the manifest +
   parser the pure `Conformance.fs` kernel is compiled before): `checkManifest target
   manifestPath` derives the `.fsi`↔`.fs` pairs from a package `manifest.toml` —
   stem rule (`foo.fsi` ↔ `foo.<t>.fs` override else `foo.fs`), impl candidate set =
   `resolveImpl T ∪ resolveInlineBodies T`, and the F#-faithful module-decl guard
   (paired files must agree on their leading `module`/`namespace` — the
   `QualifiedNameOfFile` basis; see "How F# actually does it"). Returns a
   `PackageOutcome` of `Paired`/`SigOnly` per contract + `ImplOnly` (compiled `.fs`
   with no contract). `ConformanceTests` now DISCOVERS the `Vesper.*` packages from
   the tree and drives one `checkManifest None` per package. The hardcoded
   `conformingPairs`/`knownDriftPairs`/`implFreeExemptions` file lists are deleted;
   manifest-driving immediately widened coverage to the previously-omitted
   `capabilities`/`structural-format`/`ops-*`/`struct-seq` files. (The `acceptedFindings`
   multiset that initially classified the surfaced drift was later EMPTIED — see the
   accuracy refinement under Step 4 — once the check was made F#-accurate and the real
   discrepancies fixed at source.) `SigOnly` == the recorded impl-free exemptions, and
   zero module-guard/`ImplOnly` violations. All suites green.
4. **Extend conformance to value bindings + the faithful two-step.** Split into
   two sub-steps once the surface was mapped — the α-equivalence/typar-order half
   provably needs a layer the CST does not have:
   4.1. **Value-binding PRESENCE** — DONE. The kernel (`Conformance.fs`) now also
      extracts MODULE-LEVEL `val` (`ModuleSignatureElement.Val`/`ValLiteral`) and `let`
      (`ModuleElem.FunctionOrValue`) bindings — flattened across nested modules by
      `CstWalk`, operator heads keyed off `Pat.Op`/`IdentOrOp` raw spelling — and
      `checkValuePresence` reports `ValueMissingInImpl` for every `.fsi` `val` with no
      `.fs` `let`. The converse (impl `let`, no sig `val`) is NOT reported: F# silently
      allows it (a HiddenVal), confirmed by the FCS scout (`accValRemap`), so a private
      helper is not drift — the value-level mirror of the type-level asymmetry. `checkPair`
      + `ConformancePass` fold value findings in after the type findings. TYPE MEMBERS
      (e.g. `formatter`'s overloaded `AppendFormatted`) are NOT extracted here — those are
      Step 6.

   4.1b. **Accuracy refinement — make the check F#-faithful, fix the real drift at
      source (NOT an acceptance list).** The first cut pinned the surfaced findings in an
      `acceptedFindings` golden; that baked acceptance of things that were either F#-legal
      (so the check was wrong to flag them) or genuine source drift (so they should be
      fixed, not accepted). Resolved both:
      - **Kernel made accurate (false positives removed by construction).** A sig type
        ABBREVIATION (`type X = Y`, new `SigShape.Abbrev`) resolves transitively and needs
        no `.fs` companion → no `MissingInImpl` for `ref`/`seq`/`ResizeArray`. An impl type
        absent from the sig is a HiddenTycon (F# hides it) → the `MissingInSig` error was
        DELETED outright; an impl `(# … #)` intrinsic with no `extern` still reports
        `IntrinsicWithoutExtern`. This dissolved all eight type "drift" rows
        (`SetTree`/`Doc`/`ListEnumerator`/… + the abbreviations) without any acceptance.
      - **Four real `val`-without-`let` discrepancies fixed at source.**
        `structuralEquals`/`structuralHash` (Vesper.Core) + `structuralCompare`
        (Vesper.Comparison) were JS-only runtime entries wrongly declared in the *shared*
        CLR-visible contract (CLR never references them — grep-verified); moved to JS-only
        `ops-platform-runtime.js.fsi` / `comparison-runtime.js.fsi` in each manifest's
        `files-js`, so the CLR contract no longer over-declares them and the JS front end
        still resolves them. `List.ofSeq` was a real forward-declaration consumed by
        `set.fs`; IMPLEMENTED in `list.fs` (`for x in source` consing + `rev` — the
        `for .. in` form so each backend lowers the enumeration its own way, closure-free
        accumulator), retiring the "neither backend implements it" gap.
      Result: `acceptedFindings` is now EMPTY — every package conforms with zero findings,
      and the suite carries no pinned drift. Goldens regenerated; CLR (1066) / JS (175) /
      SA (645) / Vesper (49) green.
   4.2. **Typar count + α-equivalence + ORDER** — DONE. New `ConformanceTypars.fs`
      (`XParsec.FSharp.SemanticAnalysis`, after `ConformancePass.fs` — it needs only
      `FrozenType` / `ExternalSymbol` / `Frozen.TastFile`, NOT `Pipeline`, so the caller runs
      the front end and hands it the frozen tree). The kernel is `schemesAgree (declared:
      FrozenType) (inferred: FrozenType) = normAxis declared = normAxis inferred`: a
      structural `FrozenType` equality after `normAxis` collapses the two sides' single typar
      axis onto `Method` (a free value/function has exactly ONE typar axis — the `.fsi`'s
      `FTTypar(Declaring, i)` and the `.fs` module binding's `FTTypar(Method, i)` denote it).
      Because `FTTypar` is positional, this IS α-equivalence-WITH-ORDER: it fails exactly when
      the two sides number their typars differently. `checkFile provider tast` walks the
      frozen `TDecl.Let`s, skips inline (dropped by `Freeze` anyway) and monomorphic / unpublished
      bindings, resolves each by `Holder.Name` (the source-alias the contract publishes
      alongside the compiled name) via `provider.TryLookup`, and yields a `TyparMismatch` per
      generic binding whose inferred `ty` disagrees with the declared `Scheme`.

      WHY this works (the two orders line up by construction): the `.fsi` side is quantified by
      `VesperLib.translateCurriedSig` (args-first appearance) into `ExternalSymbol.Scheme`; the
      `.fs` side by `GeneralizedTypars.canonical` (declared-first, then appearance) into the
      frozen `TDecl.Let.ty` — `Elaborate.freezeTypars` applies the per-binding `quantEnv` so the
      module-let's `ty` carries `FTTypar(Method, i)` in canonical order. The one drift species
      this catches is a `.fs` that declares `<'b,'a>` against a `.fsi` whose appearance order is
      `'a,'b` (see `FreezeTests`' "free function honours declared `<'b,'a>` typar order over
      appearance").

      Tests: `SemanticAnalysis.Tests/ConformanceTests.fs` "TyparConformance" (5) — the kernel
      axis-normalization, the canonical `<'b,'a>`-reorder mismatch over the REAL frozen pipeline
      (stub contract provider pinning the declared order), the conforming control, and the
      unpublished-binding skip. `Codegen.Clr.Tests/ConformanceTyparsTests.fs` (1) — drives
      `checkFile` against a contract provider EXTRACTED from a real `.fsi`
      (`ClrSymbolProviders.buildContract [vesperCoreManifest; vesperListManifest]`) and asserts
      `list.fs`'s generic module functions (`fold`/`map`/`append`/…) conform, proving the
      extracted-vs-inferred typar orders agree end-to-end (incl. nominal-key alignment across
      the two extraction paths). SA 650 / new Clr row green.

      Scoping decisions retained from the design (still load-bearing):
      - **EXCLUDES SRTP / inline operators** — for a STRUCTURAL reason, not because their
        `.fsi`/`.fs` typars differ today. An inline body is expanded + SRTP-solved at each
        call site and NEVER emitted as a fixed-arity generic method, so it has no
        emitted/extracted typar order for the contract's order to drive; the signature is the
        sole ABI surface. `checkFile` matches non-inline `Let`s only (and `Freeze` drops inline
        templates), so they never reach it — and would be exempt even if the body matched the
        contract exactly. (CORRECTION to the earlier draft, which called this an "intentional
        divergence": `ops-platform.fs` implements `(+)` as the homogeneous `^T -> ^T -> ^T`
        while the contract — and real FSharp.Core's body, `prim-types.fs`
        `let inline (+) (x:^T) (y:^U) : ^V` — is the general `^T1 -> ^T2 -> ^T3`. That
        one-typar body is a known SIMPLIFICATION, not the intended end state; it is orthogonal
        to 4.2.)

        **SCOPE BOUNDARY (decided): the one-typar body is a LONG-LIVED exclusion, NOT a T8
        blocker.** Reconciling the inline-operator body with its three-typar contract is OUT of
        T8 scope, for a structural reason: the SRTP form `^T1 -> ^T2 -> ^T3 when (^T1 or ^T2):…`
        has no CLR representation, so a NON-inlined (eta / `reduce (+)`) use is served in
        FSharp.Core by the `AdditionDynamic`/`CheckedAdditionDynamic` runtime helper
        (`prim-types.fs:4595`, the body's first line) — machinery Vesper has not ported. The
        homogeneous `^T -> ^T -> ^T` body is the form that IS emittable as a real generic method
        (dispatched to the operand type's own `op_Addition`), which is what Step 5's `BuiltinOps`
        retirement needs. Full parity therefore depends on porting the dynamic-operator runtime —
        owned by the arithmetic / per-target inline-IL stack (see
        `project_inline_il_target_specific`), not this `.fsi`/`.fs` faithfulness seam. T8 closes
        without it: G1 (presence — a paired `.fs` exists) holds and G2 (emitted-method typar
        order) does not apply to inline bodies. "T8 done" does NOT assert operator FSharp.Core
        parity.
      - **Type MEMBERS deferred to Step 6** — `checkFile` reads only module-level
        `TDecl.Let`s, not `TTypeMemberG.MethodTypeParams`. Cross-package member extraction is
        Step 6's `MethodArity = 0` work.
      - **Blocked surface unchanged:** packages whose `.fs` does not yet compile through the
        pipeline (`set.fs`) are not driven; 4.2's tests cover `list.fs`, which compiles
        end-to-end. (A whole-tree sweep driving `checkFile` over every package's frozen `.fs`
        + extracted `.fsi` is a natural follow-on once those compile.)
5. **Flip `MissingInImpl` to a hard error** — DONE for the hard-error flip; the two
   retirements are DEFERRED on their upstream dependencies.
   - DONE (the flip). New `ConformancePass.enforce : PackageOutcome -> Diagnostic list`
     promotes EVERY conformance discrepancy to a hard `Severity.Error` diagnostic — the
     FS0240 family (`MissingInImpl` / `ValueMissingInImpl`, code `V240`), extern/intrinsic
     drift, and an un-exempted impl-free `SigOnly` `.fsi` (`V240`); plus a module-decl
     mismatch (`V241`), a contract-less `.fs` (`V242`), and a stale/unknown `sig-only`
     exemption (`V243`). The exemption list moved OUT of the test into the manifest: a new
     `[core] sig-only` key (`ReferencedProject.SigOnly` + `resolveSigOnly`, REPLACE-style
     per-target overrides), populated for `Vesper.Printf` (`printf.fsi` / `printf-format.fsi`)
     and `Vesper.Exceptions` (`exceptions.fsi`). So a `.fsi` whose `.fs` was deleted — and
     which is not declared `sig-only` — is an FS0240 hard error BY CONSTRUCTION, not a pinned
     golden. The pass is wired as a HARD GATE into the package-build harness
     (`Codegen.Clr.Tests/TestHelpers.buildPackage`): a non-conforming package fails its build
     rather than emitting a degraded DLL with a codegen substitution standing in for a missing
     `.fs`. The old diagnostic-only projections (`pairErrors`/`sigOnlyFiles`/`moduleMismatches`)
     + the test-side `acceptedFindings`/`implFreeExemptions` maps are deleted; the
     `PackageConformance` test now asserts `enforce = []` per package, and a new
     `ConformanceEnforcement` testList pins the promotion (deleted-impl `SigOnly` → `V240`,
     `MissingInImpl` → `V240`, exempt control, stale-exemption → `V243`). All suites green
     (SA 653/+1 skip, Clr 1067, Js 175, Vesper 49).
   - DEFERRED. Retire `BuiltinOps` (Species 2): blocked on the homogeneous `^T->^T->^T`
     inline-operator body, whose non-inlined (eta) form needs the ported dynamic-operator
     runtime — OUT of T8 scope per the 4.2 SCOPE BOUNDARY (owned by the per-target inline-IL
     stack, `project_inline_il_target_specific`). The enforcement above does NOT regress on it:
     `BuiltinOps` is a codegen EMISSION fallback, not a `.fsi`-substitute-for-missing-`.fs`,
     so it is not a `SigOnly`/`MissingInImpl` finding.
   - DEFERRED. Retire the FSharp.Core `PrintfFormat` substitution (Species 4): sequenced on
     the vesper-printf cold-path self-host; until then `printf-format.fsi` is a declared
     `sig-only` exemption (above), so it is enforced as a KNOWN impl-free contract, not a
     silent gap.
6. **Extend to generic MEMBERS** once published cross-package (drop the
   `VesperLib.fs` `MethodArity = 0` hard-codes, `VesperLib.fs:535` and `:1105`).

## Side goal — remove `MockBuiltins` — DONE

`MockBuiltins` was an early bootstrap hack: a VALUE-symbol-only fixture (monomorphic
ops `(+): int→int→int`, pipe/compose, `List.fold`, printf, `failwith`, `hash`) that
deliberately diverged from the real SRTP contract and provided ZERO type shapes
(`TryLookupType _ = ValueNone`). It is now DELETED; every test resolves through real
`Vesper.*` contracts. See the `feedback_mockbuiltins_is_a_trap` /
`project_contract_demotion` memories. The staged removal that landed:

- **Group A — codegen tests** (`Codegen.Clr.Tests`/`Codegen.Js.Tests`): DONE. Clr
  `TestHelpers.analyse` → `ClrSymbolProviders.buildContract defaultManifests`;
  `vesperCoreDll` fixture → `buildContract []` (Core's own self-compile provider, the
  same `buildPackage "Vesper.Core"` uses); Js `TestHelpers.frozenOf` → the real
  JS-native `jsProvider`. Both projects' `MockBuiltins.fs` `Link` includes dropped.
  Two `analyse`-shape assertions (`BindingTests`/`FunctionTests`) updated: the real
  `(+)` inline-expands to `ILIntrinsic "add"` where the value-only mock left an
  `External op_Addition` call head — the incidental arithmetic shape relaxed, each
  test's actual anchor (NodeKey linkage / pre-freeze beta-reduction) kept.
- **Group B — SA front-end tests** (the bulk): DONE. A shared `TestHelpers.realProvider`
  builds the default `Vesper.*` stack (Core/List/Comparison/Printf) IN-ASSEMBLY via
  `ReferencedProject.buildProviderWith` in dependency order — each package extracted
  with the already-built providers' type shapes as its `ambientShapes` (the
  hand-rolled analogue of codegen's `SymbolProviders.composeProviders` dep wiring,
  since `Codegen.Common`/`MetadataSymbols` are off the SA-test reference graph). Every
  `MockBuiltins.provider` site repointed at `realProvider.Value`. Two transitional
  VesperLib chaining tests (which existed to prove the lib-ahead-of-MockBuiltins chain)
  were folded into their lib-only twins.
  - **Provider choice (load-bearing):** Vesper.Core, NOT the FSharp.Core port
    `XParsec.FSharp.Lib`. The port canonicalises `int`→`int32` and resolves `string`
    to an unfreezable external template, diverging from the front-end's `BuiltinTypes`
    (`int`/`string`) — using it broke ~93 SA tests; the `Vesper.*` contracts (whose
    primitives ARE `int`/`string`) dropped that to 0.
  - **One production change** (`Passes/NameResolution/Scope.fs`): the printf family is
    a front-end intrinsic (typed by `InferApp`/`PrintfSpec`, not a provider symbol), so
    a `PrintfSpec.tryFamily` name no longer raises "Unresolved identifier" even when the
    contract doesn't declare it. The mock fabricated every family (incl. the writer
    `fprintf`/`fprintfn`, which the real `Vesper.Printf` contract omits); this makes the
    front end natively own the family instead of leaning on a provider crutch. Dead
    `PrintfSpec.genericSignature`/`genericSignatureFrozen` (mock-only consumers) deleted.
- `MockBuiltins.fs` + all three `.fsproj` includes deleted. Suites green: SA 649/+1
  skip, Clr 1067, Js 175, Vesper 49.

Note: removing the `defaults` bootstrap (1.5) did NOT require this — once the provider
exposes forward reprs, even a MockBuiltins-backstopped build resolved primitives
through the real Core provider — but the two are kin (both are "the real provider is
the source").

## Why exposure was low while deferred

- The double-implementation + round-trip tests catch realistic drift for module
  FUNCTIONS.
- Generic *members* aren't published cross-package yet (`MethodArity = 0`).
- T4's correct-by-construction carrier removed the `.fs`-internal divergence; T8 is
  purely the `.fsi`↔`.fs` seam.

## Tests to add (by step)

- **[4.2 — DONE]** A `.fs` that declares `<'b,'a>` for a binding whose `.fsi` appearance
  order is `'a,'b` → `ConformanceTypars.checkFile` reports a `TyparMismatch`; the conforming
  control (no explicit `<…>`) reports none. Plus the real-package end-to-end check
  (`list.fs` vs extracted `list.fsi`). (`ConformanceTests.fs` "TyparConformance" +
  `Codegen.Clr.Tests/ConformanceTyparsTests.fs`.)
- **[6]** Round-trip: `.fsi` extract → downstream consume → call, asserting
  GenericParam order == MethodSpec order for a declared-≠-appearance binding.
- **[6]** `formatter.fsi` ↔ `formatter.fs` MEMBER-level conformance (overloaded
  `AppendFormatted`, ctors) goes green.
- **[5 — DONE]** A `.fsi` binding with no `.fs` (a deliberately deleted impl) → hard
  FS0240-style error (`V240`), NOT a pinned golden. (`ConformanceTests.fs`
  "ConformanceEnforcement": un-exempted `SigOnly` → `V240`; `MissingInImpl` → `V240`;
  exempt + stale-exemption controls.)
- **[DONE, 4.1/3/5]** Per-target: `exceptions.fsi` conforms (impl-free) on CLR (now
  declared `[core] sig-only` in `Vesper.Exceptions/manifest.toml`, enforced by
  `ConformancePass.enforce`) and via `prim-types-exn` on JS.
