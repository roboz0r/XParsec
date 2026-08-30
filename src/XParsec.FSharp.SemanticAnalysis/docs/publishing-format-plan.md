# Publishing-format plan — how the contract metadata is distributed

## Why

`Vesper.Core` (and every `Vesper.*` package after it — see
[core-lib-architecture](core-lib-architecture.md)) carries two kinds of
information:

1. a **runtime** — the **target artifact** the backend emits, holding only
   what the target ABI can express (real types, real method bodies). On CLR that
   is the BCL-only `Vesper.Core.dll`; on JS it is a directory of `.mjs` modules;
   a future target has its own native format. "Target artifact" below means any
   of these; `.dll` appears only where the point is CLR-specific.
2. a **contract** — the `.fsi` signatures plus the manifest, holding everything
   the front-end needs that **IL (or JS, or any target ABI) cannot represent**.

The runtime's distribution form is settled: it's a normal per-target artifact —
one target artifact per package ([core-lib-architecture](core-lib-architecture.md)).
This plan settles the open
question for the *contract*: **in what form is the not-IL-representable metadata
published, and where does it live relative to the target artifact?**

Two well-known poles bound the design space, and this plan rejects both:

- **F# pickled metadata** — the compiler serialises its typed signature +
  inlinable bodies into two private binary resources (`FSharpSignatureData.*`,
  `FSharpOptimizationData.*`) **embedded in the PE**. One artifact carries both
  runtime and metadata.
- **Fable source bundle** — ship *all* `.fs` source in the package and rebuild
  the whole thing from source on every consume; there is no precompiled runtime.

## What IL can't carry (the thing being published)

Grounded in the current contract files:

| Construct | Example | Why no IL slot |
|---|---|---|
| Extern/intrinsic type mapping | `type int = extern` (`../../Vesper.Core/prim-types-min.fsi:13`) | In IL `int` *is* `System.Int32`; "the target provides this" is compiler-only. On JS the same line maps to a number. |
| Type abbreviations | `'T array = 'T[]`, `'T ref = Ref<'T>`, `'A -> 'B` ≡ `Fun<'A,'B>` | IL erases abbreviations to the underlying type; they exist only for name resolution. |
| Inline signatures + SRTP | `val inline (+): … when (^T1 or ^T2): (static member (+)…)` (`../../Vesper.Core/ops-platform.fsi:40`) | Statically-resolved type params and `default ^T: int` have no runtime existence; solved per call site. |
| Inline bodies | `let inline hash (obj:'T) = EqualityComparer<'T>.Default.GetHashCode obj` (`../../Vesper.Core/ops-platform.clr.fs:30`) | To inline across the package boundary you need the *expression* the backend re-lowers per target, not a compiled method. |
| Name-resolution metadata | `[<AutoOpen>]` on the operator modules, `[<CompiledName>]`, module-vs-namespace, RQA | AutoOpen must be honoured by the *consumer's* resolver; not all of it survives as plain attributes. |

This is the same partition F# pickles: `FSharpSignatureData` (the contract) +
`FSharpOptimizationData` (inlinable bodies). Vesper's `.fsi` ≙ signature data;
the manifest's `impl` inline `.fs` (e.g. `ops-platform.clr.fs`) ≙ optimization data
(read across the package boundary by
`InlineBodies.collect` and spliced by `Emit.lowerWith`, milestone M).

## Three positions, not two

| | Targets | Own backend / stable IR? | Therefore ships |
|---|---|---|---|
| **F#** | .NET only | yes | metadata + runtime both in the PE (one artifact) |
| **Fable** | JS / others | no (transpiler) | all source, rebuild everything each consume |
| **Vesper** | **multi-target** | **yes** (per-target artifact: BCL-only `.dll`, `.mjs` tree, …) | **per-target runtime + one neutral contract** |

The two facts that place Vesper in its own position:

1. **F# can embed pickled metadata in the PE only because F# is single-target.**
   Go multi-target and the universal contract cannot live inside any one
   target's artifact — a CLR `.dll` is the wrong home for metadata a JS target
   must also read. This is precisely why Fable *cannot reuse* F#'s pickle and
   falls back to source.
2. **Fable rebuilds from source only because it has no stable runtime to ship —
   Vesper does.** Each backend emits a real target artifact; rebuilding the
   runtime from source on every consume buys nothing. Fable's source bundle is a
   workaround for a missing backend, not a virtue to copy.

So Vesper lands where **OCaml, Scala, and TypeScript** already live: a per-target
artifact + a target-neutral metadata channel, tied by a manifest — which
is what `../../Vesper.Core/manifest.toml` + `.fsi` + the selective inline `.fs`
already are. This plan ratifies and bounds that, it does not invent it.

## Target artifact ≠ distribution package

The decisive sharpening: the **target artifact** (a CLR `.dll`, a JS `.mjs`
directory, a future target's native format) and the **distribution package**
(`.nupkg` / npm tarball / …) are *different containers*, and the metadata
belongs to the second, never the first.

- **Embedding metadata in the target artifact is pure deadweight.** The runtime
  loader cannot use the contract — only the *compiler* reads it, and only at
  compile time. F#'s `FSharpSignatureData`/`FSharpOptimizationData` resources sit
  inside `FSharp.Core.dll` in every deployed app forever, swelling every copy for
  data the CLR never touches. Kotlin's `@Metadata` annotation baked into each
  `.class` is the same anti-pattern on the JVM.
- **A package manager already gives a neutral, multi-file container.** A `.nupkg`
  or tarball is an archive with arbitrary entries; it can hold the target
  artifact, the `.fsi` contract, the manifest, and (if ever needed) a serialised
  metadata sidecar as *separate entries*. The metadata is carried in the package, the
  target artifact stays lean. This is exactly OCaml's `.cmi`/`.cmx` (sidecar files next to the
  `.cmo`/`.cmxa`) and Scala's `.tasty` (a **separate entry in the jar**, not bytes
  inside the `.class`).

## The design axes

Three independent choices, so prior art can be placed precisely:

- **Carrier**: (a) source files in the package · (b) serialised sidecar file in
  the package · (c) embedded resource/section in the **target artifact** ·
  (d) custom attributes on runtime members.
- **Encoding**: (a) the language's own surface syntax · (b) a structured *public*
  spec (JSON / protobuf / TASTy-style) · (c) a private binary pickle.
- **Resolution timing**: (a) re-parse/re-resolve at consume · (b) ship
  pre-resolved.

Multi-target (PF7) eliminates carriers (c) and (d) for the universal contract:
both are bound to one target's artifact. That leaves (a)/(b) — package-level
carriers — which is the whole point of the section above.

## Prior art

| System | Carrier · Encoding | The bill they paid |
|---|---|---|
| **F#** `FSharpSignatureData` | **artifact** · private pickle | version-brittle (FCS-locked magic number); .NET-only; deadweight in every deployed PE |
| **OCaml** `.cmi` / `.cmx` | package sidecar · binary | `.cmi`=interface, `.cmx`=cross-module inline info — *exactly* the `.fsi`/inline-`.fs` split; magic-number breaks per compiler version |
| **Scala 3** TASTy | package sidecar (separate `.tasty` in jar) · **public spec** | typed tree as a documented, versioned interchange — "pickle done right"; large, but enables non-compiler tooling |
| **Kotlin** `@Metadata` | **artifact** (annotation in `.class`) · protobuf | reuses the platform annotation reader, but JVM-bound and in-artifact deadweight |
| **Rust** `.rmeta` | package sidecar · private | `-C metadata` hash; not stable cross-version |
| **TypeScript** `.d.ts` | package source · surface syntax | the runaway success: human-readable, toolable, *is* the language — cost is "re-parse to know anything" |

Two takeaways for Vesper: **OCaml `.cmi`/`.cmx` is the closest twin** (validates
keeping the inline-body channel separate from the contract — the manifest's
`impl` list already does this), and **every in-artifact option (F#, Kotlin) is
single-target by construction** — they fail the carrier axis here for the same
reason as the F# pickle.

## Decisions

- **PF1 — The published contract is source (`.fsi` + `manifest.toml`),
  re-resolved at consume.** Target-neutral, human-readable, diffable, and reuses
  the front-end parser as the single source of truth — no second format to
  version against the type system while the language still churns. This is the
  current state — `ReferencedProject` parses each package's `manifest.toml` and
  `.fsi` set and resolves the `depends-on` closure itself
  (`ReferencedProject.fs:246-345`); ratify it. TypeScript's `.d.ts` is the
  precedent.
- **PF2 — Metadata is never carried in the target artifact.** No pickled resource or PE
  section in any `Vesper.*.dll`, no contract-bearing module in an emitted `.mjs`
  tree; no metadata-bearing custom attribute beyond what the runtime itself
  needs. The runtime loader cannot use the contract, so any such bytes are
  deadweight in every deployed copy (the F#/Kotlin anti-pattern).
- **PF3 — The distribution package carries the metadata, as separate entries.**
  A package = { per-target artifact(s) + neutral contract (`.fsi`) +
  manifest [+ optional serialised sidecar, PF6] }, each a distinct entry in the
  package container (`.nupkg`/…). Mirrors OCaml's sidecar files and Scala's
  separate `.tasty` jar entries; explicitly **not** Kotlin's in-`.class`
  annotation or F#'s in-PE resource.
- **PF4 — Inline bodies travel as TAST/source, never as compiled per-target
  IL.** `hash`'s body lowers to `callvirt EqualityComparer::GetHashCode` on CLR
  and to something else on JS; there is no portable "compiled inline body." This
  is the `.cmx`/optimization-data channel and it is inherently source-first. Keep
  it a separate manifest list (`impl`) from the contract, per OCaml.
  Mechanistically this is *why* source suffices, and it is the load-bearing
  simplification: because the body ships as source and is re-typechecked at consume
  (PF1), an imported inline re-enters the consumer's compilation as an ordinary
  `SemType` body — handled by the **same local inline pass** as a same-package inline
  (the pre-freeze inline-expansion pass), with no serialised template and no
  `FrozenType` round-trip. The cross-package case **collapses into the local case**;
  the serialised `FrozenType` template (PF6) is needed only when source is *not*
  shipped. Caveat to watch: re-typechecking an inline body needs the library's
  *original typing environment* (its `open`s, internals, and transitive dependency
  signatures), not just the contract surface — clean for shallow-dependency libraries,
  and the depth at which it degrades is exactly the pressure toward PF6. PF9's
  published-surface-only rule bounds this caveat: a conforming inline body needs only
  the contract closure.
- **PF5 — Any resolved/serialised form is a local, content-addressed build
  cache — never published.** If consume-time parse+resolve shows up in a profile,
  cache the resolved `IExternalSymbolProvider` keyed on source content hash, in
  `obj/`, regenerable from source. The committed `.parsed` snapshots are its
  embryo. This gets pickle-speed without inheriting pickle version-brittleness or
  a place in the shipped package.
- **PF6 — If a *published* serialised form is ever needed, it is a public
  versioned spec (TASTy-model), shipped as a package sidecar (PF3) — never a
  private pickle and never in the artifact (PF2).** Trigger is a **parser-less or
  stateless** consumer (below), not "non-compiler" and not aesthetics. Its
  advantage over source is random-access / demand-loading of one symbol *without
  running the parser and without holding state* — which only pays off for a
  consumer that has neither. The concrete in-memory form such a sidecar would
  serialise is **`TExpr<FrozenType>`** — the ground, `FTTypar`-only typed term the
  frozen-type split already produces.
  That shrinks PF6's *marginal* cost (the compiler builds the payload regardless; PF6
  adds a pickler over a closed DU, not a new IR) but does **not** lower the
  version-brittleness bar — the schema-stamp burden below is unchanged.
  **Trust gate before any PF6 ship — the equality oracle:** compiling a library to
  `TExpr<FrozenType>` in memory and re-typechecking its *shipped source* (PF1) to
  `SemType → freeze → TExpr<FrozenType>` must produce **equal** trees (modulo typar
  indexing). That equality is what licenses consuming the serialised form *instead of*
  source; it generalises the Edge-A typar-index round-trip test, and it can
  be run from the day `TExpr<FrozenType>` exists — long before any sidecar is shipped.
- **PF7 — Multi-target shapes the carrier.** One target artifact per target; **one
  neutral contract** shared across targets. This is *why* the contract cannot
  live in any target's artifact (reinforces PF2) and points toward a shared
  contract package consumed by per-target runtime packages — to be reconciled
  with the deferred rollup/merge decision
  ([core-lib-architecture](core-lib-architecture.md)).
- **PF8 — The contract is hand-authored and committed, or printed from the
  checked surface at build time.** Two provenances, one format:
  - A **contract-first** package commits its hand-authored `.fsi`, which
    *constrains* the implementation — each `Vesper.*` is this kind, its `.fsi`
    being carried FSharp.Core documentation. The source-level conformance gate
    (`Conformance.fs`, P4) checks impl against contract, and reviewers see
    contract changes in the diff.
  - Every other package's `.fsi` is a **build output**: the compiler already
    holds the checked surface (`PublishedSurface` / `FrozenSignature`), and a
    signature printer emits the `.fsi` beside the target artifact at the publish
    boundary. A user authors only `.fs`. Drift is structurally impossible; the
    gate is the round-trip property `parse(print(surface)) = surface`, runnable
    in CI from the day the printer exists. The printer emits everything the
    target ABI cannot carry — RQA, `[<AutoOpen>]`, abbreviations, and a
    `[<CompilationRepresentation(ModuleSuffix)>]` module's source name.

  A consumer cannot tell the provenances apart: both publish identically
  (PF1/PF3) and are read through the same signature-resolution path, so a
  publishable `.fsi` is always present and PF1 can never fall through. Within
  one build, a project reference hands the consumer the in-memory surface
  directly; the printed `.fsi` exists only where the producer's compilation is
  not in memory — a referenced prebuilt target artifact, or a distribution
  package. This is TypeScript's `declaration: true` (emitted `.d.ts` beside the
  build output), and it is how OCaml's `.cmi` and Scala's `.tasty` are produced
  — nobody hand-commits a sidecar; only the encoding here is surface syntax
  rather than binary (PF1).
- **PF9 — Inline bodies ship as printed source under a published-surface-only
  rule; no bundled binary format.** The build prints the `inline`-marked
  bindings, fully qualified, into a companion fragment beside the printed
  `.fsi` — the generated twin of the manifest's hand-listed `impl` files. What
  makes re-typechecking at consume sound is a publish-time check the language
  already implies: **an inline body may reference only the published surface**
  (F# rejects an `inline` function whose body uses insufficiently accessible
  values), so the printed body re-resolves in a fresh environment seeded with
  the package's own contract closure. This closes PF4's typing-environment
  caveat for conforming packages, and a package whose inline bodies cannot meet
  the rule is a PF6 trigger, not a reason to bundle.

  A custom binary format bundling contract + bodies is rejected on the carrier
  axis: the package container already provides cohabitation (two entries in one
  `.nupkg`/tarball are as together as two sections in one blob), and bundling
  welds the stability-wanting textual contract to the churn-prone bodies
  channel, inheriting the schema-brittleness only the bodies could ever
  justify. The two channels also have different readers: every consumer reads
  the contract (resolver, LSP, doc tooling, humans), only an optimising compile
  reads bodies, and bundling forces the union of requirements on every reader.
  If a serialised typed form is ever warranted it is PF6, **bodies-only**,
  beside a still-textual `.fsi` — OCaml's actual layout (`.cmi` interface
  beside `.cmx` inline info). PF6's equality oracle is shared: the test that
  validates a printed body today licenses the pickle tomorrow.

## Triggers to revisit

- **A *parser-less or stateless* consumer must read metadata** — a tool in a
  foreign ecosystem that can't link the front-end parser, or a per-request /
  serverless context that can't amortise a parse. This is the one thing that
  justifies PF6. The condition is *not* "non-compiler": an **LSP is explicitly a
  non-trigger** — it bundles the same front-end and is a long-running stateful
  server, so it reads the shared `.fsi` (<1 MB total), parses once into an
  in-process cache, and is fully served by PF1 + PF5. The first anticipated
  non-compiler consumer (LSP) therefore pushes PF6 *further* out, not closer.
  Make "can't run the parser, or can't keep the cache" the trigger.
- **Consume-time parse+resolve shows up in a profile** — reach for PF5 (local
  cache) first, *not* PF6 (published serialised form). The cache solves the speed
  problem without the distribution-format cost. The measured shape today:
  `PackageProviders.buildProviderSeeded` (`PackageProviders.fs:115`) re-analyses the full
  dependency closure on every contract build — nine packages for `Vesper.Set` — so the cache
  key is the closure's source content, and a hit skips `AssemblyAnalysis.analyseUnits`
  entirely.
- **The contract stabilises** — once the language stops churning, the cost of
  maintaining a serialised schema (PF6) drops, lowering the bar to adopt it.

## Risks / open questions

- **CLR attribute rows are advisory; the contract is the carrier (2026-08-28).** The CLR
  backend emits `CustomAttribute` rows (`AttributeRowPrep`), but they serve external .NET
  tooling and the one metadata readback that exists, `MetadataSymbols.hasAllowNullLiteral`.
  Every other F# marker — the equality/comparison family, `[<RequireQualifiedAccess>]`,
  `[<AutoOpen>]` — has no metadata reader; the `.fsi` contract carries their meaning between
  Vesper compilations. A `.dll` consumed *without* its contract therefore loses a type's
  equality and comparison posture and its null-inhabitation. PF1/PF3 make that configuration
  unsupported (the package always ships the contract), but a raw-assembly-reference path, if
  one is ever added, must either require the contract or grow readers for the marker rows.
- **Contract ↔ runtime drift.** `.fsi` (contract) and `.fs` (runtime) must
  agree. The gate depends on provenance (PF8): a hand-authored contract is
  checked by the source-level conformance check (`Conformance.fs`, P4),
  mirroring how F# checks impl against signature *before* it pickles; a printed
  contract cannot drift, and its gate is the `parse(print(surface)) = surface`
  round-trip. A stale printed `.fsi` beside a rebuilt target artifact is still
  possible operationally — the publish step must emit both from one
  compilation. A PF6 sidecar would have to validate the serialised artifact
  too.
- **Canonical direction of the `.fsi`/`.fs` pair — RESOLVED (PF8).** Per
  package: contract-first (hand-authored, committed, constrains the impl — the
  `Vesper.*` libraries) or implementation-first (`.fsi` printed at build). Both
  publish identically, so the choice is an authoring discipline, not a
  distribution-format question, and a future PF6 sidecar derives from the
  checked surface in either case.
- **The signature printer is a new component (PF8/PF9).** Print direction does
  not exist yet; only parse does. Its correctness burden is the round-trip gate,
  and its completeness burden is the not-target-representable metadata table
  above — a construct the printer drops is a construct consumers silently lose.
- **Versioning.** Source-contract version = package version (free). A PF6 sidecar
  needs its own schema version stamp — the exact thing that bites F# / OCaml /
  Rust cross-version. Treat the schema as a public, versioned contract from day
  one (the TASTy discipline) or not at all.
- **Package-manager heterogeneity.** `.nupkg` is the CLR container; other targets
  use other managers (npm, …). The neutral contract must be carriable by *any* of
  them — another argument for plain source files (PF1), which every package format
  can hold, over a bespoke serialised blob.
- **Packaging unit overlap with the deferred rollup.** Whether the rollup is
  per-target runtime packages + a shared contract package, or one fat package per
  target, is taken with the rollup/merge decision
  ([core-lib-architecture](core-lib-architecture.md)); PF7 biases it toward a
  shared neutral contract package.

## Cross-references

- [core-lib-architecture](core-lib-architecture.md) — the per-package runtime
  split (one target artifact per package), the contract/runtime two-artifact split
  (`.fsi` vs `.fs`) this plan distributes, and the deferred rollup/merge decision
  this plan's PF7 feeds.
- [function-representation-plan](function-representation-plan.md) — `Fun`, the
  representation behind the arrow-sugar abbreviation that IL can't carry.
- The (completed) `SemType → FrozenType` split — its `TExpr<FrozenType>` is the
  in-memory artifact a PF6 sidecar would serialise, and its source-reship path
  (re-typecheck → `SemType` → local inline pass) implements PF1/PF4.
- [`../../Vesper.Core/manifest.toml`](../../Vesper.Core/manifest.toml) — the
  current contract (`files`) + inline-body (`impl`) manifest this plan publishes.
