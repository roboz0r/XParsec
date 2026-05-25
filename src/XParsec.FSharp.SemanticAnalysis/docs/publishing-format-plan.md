# Publishing-format plan — how the contract metadata is distributed

## Why

`Vesper.Core` (and every `Vesper.*` package after it — see
[package-split-plan](package-split-plan.md)) carries two kinds of information:

1. a **runtime** — the BCL-only `Vesper.Core.dll` the backend emits, holding only
   what the target ABI can express (real types, real method bodies); and
2. a **contract** — the `.fsi` signatures plus the manifest, holding everything
   the front-end needs that **IL (or JS, or any target ABI) cannot represent**.

The runtime's distribution form is settled: it's a normal per-target artifact
([package-split-plan](package-split-plan.md) PS2). This plan settles the open
question for the *contract*: **in what form is the not-IL-representable metadata
published, and where does it live relative to the runtime artifact?**

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
| Inline bodies | `let inline hash (obj:'T) = EqualityComparer<'T>.Default.GetHashCode obj` (`../../Vesper.Core/ops-platform.fs:30`) | To inline across the package boundary you need the *expression* the backend re-lowers per target, not a compiled method. |
| Name-resolution metadata | `[<AutoOpen>]` on the operator modules, `[<CompiledName>]`, module-vs-namespace, RQA | AutoOpen must be honoured by the *consumer's* resolver; not all of it survives as plain attributes. |

This is the same partition F# pickles: `FSharpSignatureData` (the contract) +
`FSharpOptimizationData` (inlinable bodies). Vesper's `.fsi` ≙ signature data;
the manifest's `impl` inline `.fs` (e.g. `ops-platform.fs`) ≙ optimization data
(symbol-resolution-plan §5.2/§5.3; read across the package boundary by
`SymbolProviders.inlineBodies` and spliced by `Emit.lowerWith`, milestone M).

## Three positions, not two

| | Targets | Own backend / stable IR? | Therefore ships |
|---|---|---|---|
| **F#** | .NET only | yes | metadata + runtime both in the PE (one artifact) |
| **Fable** | JS / others | no (transpiler) | all source, rebuild everything each consume |
| **Vesper** | **multi-target** | **yes** (per-target BCL-only `.dll`) | **per-target runtime + one neutral contract** |

The two facts that place Vesper in its own position:

1. **F# can embed pickled metadata in the PE only because F# is single-target.**
   Go multi-target and the universal contract cannot live inside any one
   target's artifact — a CLR `.dll` is the wrong home for metadata a JS target
   must also read. This is precisely why Fable *cannot reuse* F#'s pickle and
   falls back to source.
2. **Fable rebuilds from source only because it has no stable runtime to ship —
   Vesper does.** The backend emits a real BCL-only DLL; rebuilding the runtime
   from source on every consume buys nothing. Fable's source bundle is a
   workaround for a missing backend, not a virtue to copy.

So Vesper lands where **OCaml, Scala, and TypeScript** already live: a per-target
runtime artifact + a target-neutral metadata channel, tied by a manifest — which
is what `../../Vesper.Core/manifest.toml` + `.fsi` + the selective inline `.fs`
already are. This plan ratifies and bounds that, it does not invent it.

## Runtime artifact ≠ distribution package

The decisive sharpening: the **runtime artifact** (`.dll`) and the **distribution
package** (`.nupkg` / npm tarball / …) are *different containers*, and the
metadata belongs to the second, never the first.

- **Embedding metadata in the runtime artifact is pure deadweight.** The runtime
  loader cannot use the contract — only the *compiler* reads it, and only at
  compile time. F#'s `FSharpSignatureData`/`FSharpOptimizationData` resources sit
  inside `FSharp.Core.dll` in every deployed app forever, swelling every copy for
  data the CLR never touches. Kotlin's `@Metadata` annotation baked into each
  `.class` is the same anti-pattern on the JVM.
- **A package manager already gives a neutral, multi-file container.** A `.nupkg`
  is a zip with arbitrary entries; it can hold the runtime `.dll`, the `.fsi`
  contract, the manifest, and (if ever needed) a serialised metadata sidecar as
  *separate entries*. The metadata rides the package, the runtime artifact stays
  lean. This is exactly OCaml's `.cmi`/`.cmx` (sidecar files next to the
  `.cmo`/`.cmxa`) and Scala's `.tasty` (a **separate entry in the jar**, not bytes
  inside the `.class`).

## The design axes

Three independent choices, so prior art can be placed precisely:

- **Carrier**: (a) source files in the package · (b) serialised sidecar file in
  the package · (c) embedded resource/section in the **runtime artifact** ·
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
| **Kotlin** `@Metadata` | **artifact** (annotation in `.class`) · protobuf | rides the platform annotation reader, but JVM-bound and in-artifact deadweight |
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
  current state (`FSharpLib.loadAll`/`topoSort` repointed per package-split-plan);
  ratify it. TypeScript's `.d.ts` is the precedent.
- **PF2 — Metadata never rides the runtime artifact.** No pickled resource or PE
  section in any `Vesper.*.dll`; no metadata-bearing custom attribute beyond what
  the runtime itself needs. The runtime loader cannot use the contract, so any
  such bytes are deadweight in every deployed copy (the F#/Kotlin anti-pattern).
- **PF3 — The distribution package carries the metadata, as separate entries.**
  A package = { per-target runtime artifact(s) + neutral contract (`.fsi`) +
  manifest [+ optional serialised sidecar, PF6] }, each a distinct entry in the
  package container (`.nupkg`/…). Mirrors OCaml's sidecar files and Scala's
  separate `.tasty` jar entries; explicitly **not** Kotlin's in-`.class`
  annotation or F#'s in-PE resource.
- **PF4 — Inline bodies travel as TAST/source, never as compiled per-target
  IL.** `hash`'s body lowers to `callvirt EqualityComparer::GetHashCode` on CLR
  and to something else on JS; there is no portable "compiled inline body." This
  is the `.cmx`/optimization-data channel and it is inherently source-first. Keep
  it a separate manifest list (`impl`) from the contract, per OCaml.
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
  consumer that has neither.
- **PF7 — Multi-target shapes the carrier.** Per-target runtime artifacts; **one
  neutral contract** shared across targets. This is *why* the contract cannot
  live in any target's artifact (reinforces PF2) and points toward a shared
  contract package consumed by per-target runtime packages — to be reconciled
  with the packaging unit decision ([package-split-plan](package-split-plan.md)
  PS6).
- **PF8 — The `.fsi` contract is always a committed artifact.** Either
  hand-authored, or generated from `.fs` *and committed alongside it* — never an
  ephemeral build product regenerated on the fly. Consequences: a publishable
  `.fsi` is always present (PF1 can never fall through), the source-level
  conformance gate (`Conformance.fs`, P4) always has both sides committed to
  diff, and any future PF6 sidecar derives from the committed `.fsi` (or the TAST
  checked from it) regardless of how the `.fsi` was produced. Reviewers see the
  contract change in the diff either way.

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
  problem without the distribution-format cost.
- **The contract stabilises** — once the language stops churning, the cost of
  maintaining a serialised schema (PF6) drops, lowering the bar to adopt it.

## Risks / open questions

- **Contract ↔ runtime drift.** `.fsi` (contract) and `.fs` (runtime) must agree;
  the source-level conformance check (`Conformance.fs`, P4) is the publish-time
  gate, mirroring how F# checks impl against signature *before* it pickles. PF1
  keeps that gate as the same source-level check already run per package; a PF6
  sidecar would have to validate the serialised artifact too.
- **Canonical direction of the `.fsi`/`.fs` pair — RESOLVED (PF8).** The `.fsi`
  is always a committed artifact (hand-authored, or generated-but-committed), so
  it is always available to publish and to conformance-check, and a future PF6
  sidecar derives from it. The only residual choice — hand-author vs. generate —
  is a per-package authoring convenience, not a distribution-format question.
- **Versioning.** Source-contract version = package version (free). A PF6 sidecar
  needs its own schema version stamp — the exact thing that bites F# / OCaml /
  Rust cross-version. Treat the schema as a public, versioned contract from day
  one (the TASTy discipline) or not at all.
- **Package-manager heterogeneity.** `.nupkg` is the CLR container; other targets
  use other managers (npm, …). The neutral contract must be carriable by *any* of
  them — another argument for plain source files (PF1), which every package format
  can hold, over a bespoke serialised blob.
- **Packaging unit overlap with PS6.** Whether the rollup is per-target runtime
  packages + a shared contract package, or one fat package per target, is taken
  with [package-split-plan](package-split-plan.md) PS6; PF7 biases it toward a
  shared neutral contract package.

## Cross-references

- [package-split-plan](package-split-plan.md) — the per-package runtime split
  (PS2) and the deferred rollup/merge decision (PS6) this plan's PF7 feeds.
- [minimal-core-lib-plan](minimal-core-lib-plan.md) — the contract/runtime
  two-artifact split (`.fsi` vs `.fs`) this plan distributes.
- [symbol-resolution-plan](symbol-resolution-plan.md) — §5.2/§5.3 the inline-body
  loader (`SymbolProviders.inlineBodies`) PF4 governs; the manifest `impl` list.
- [function-representation-plan](function-representation-plan.md) — `Fun`, the
  representation behind the arrow-sugar abbreviation that IL can't carry.
- [`../../Vesper.Core/manifest.toml`](../../Vesper.Core/manifest.toml) — the
  current contract (`files`) + inline-body (`impl`) manifest this plan publishes.
- [`../../Vesper.Core/README.md`](../../Vesper.Core/README.md) — the "two
  artifacts that must agree" framing and the drift/conformance note.
