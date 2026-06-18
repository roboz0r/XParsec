# Package-split plan — one standalone package per core type

## Why

The core's "language fundamentals" started as a single `Vesper.Core` tree: one
flat `manifest.toml`, one `.fsi` contract fed to the front-end symbol provider,
one `Vesper.Core.dll` emitted by our own backend. This plan **splits the core
types into standalone packages** —
`option`, `result`, `list`, `map`, `set` — so a consumer either references the
**`Vesper` rollup** for the batteries-included experience or picks the
individual packages it needs.

The driving reason is **not** product packaging — it is **self-hosting work
sequencing**. The compiler toolchain supports only a tiny subset of the language
today (rung 2: generic unions, `match`, recursion, module-fn → static method).
Standing up each type *with its
module functions* end-to-end through that toolchain is its own pipeline of work.
A per-type package is therefore the natural **unit of self-hosting work**: a type
+ its module, compiled to a BCL-only DLL and conformance-checked against its
`.fsi`. **Merging packages is deferred to a later publishing concern** (PS6).

This generalises a split the plan already contains: [vesper-printf-plan](vesper-printf-plan.md)
(D4) already carves `Vesper.Printf` out as its own library depending on
`Vesper.Core`. Per-type packages are the same pattern, applied to the collection
and option/result types.

## The mechanism already exists

The package resolver is **already written and tested** — for
`XParsec.FSharp.Lib`, not yet pointed at `Vesper.Core`. `VesperLib.loadAll`
([`../VesperLib.fs:158`](../VesperLib.fs)) reads a root `manifest.toml` whose
`[[bucket]]` entries carry `name` / `path` / `description` / `depends-on`,
`topoSort` (`VesperLib.fs:121`) resolves the `depends-on` DAG, and each bucket's
own `manifest.toml` lists its `files = [...]`. The result is a topologically
sorted flat file list fed to the type-checker, with cross-bucket references
resolving through one accumulated `ExtractCtx` ("cross-bucket references resolve
through the accumulated tables", `VesperLib.fs:257`).

That is exactly "standalone packages + a dependency graph fed to the front-end in
compile order." `Vesper.Core`'s `manifest.toml` today uses the older flat
`[core]` schema instead (single section, flat `files`). **Adopting packages is,
on the contract side, switching `Vesper.Core` onto the bucket schema the loader
already supports** — plus one small extension (PS-Loader below).

## The dependency DAG — packages are not a flat set

The five types sit at different dependency tiers and self-host rungs:

| Package | Needs beyond base | Self-host rung | Weight |
|---|---|---|---|
| **`Vesper.Core`** (base) | — (`int`/`bool`/array/`Fun`/`unit`/`Ref`/ops, primitive aliases) | rung 1 (done) | mandatory; everything roots here |
| **`Vesper.Option`** | a generic union + module | rung 2 (union emit) | trivial type, higher-order module |
| **`Vesper.Result`** | a struct union + module | rung 2 | trivial type, module |
| **`Vesper.List`** | union + recursion + module | rung 2 (mostly landed) | small |
| **`Vesper.Comparison`** | `< > <= >=` / `compare` / `min` / `max`; BCL `Comparer<T>.Default` | — (designed) | prerequisite for Map/Set |
| **`Vesper.Map`** | base + Comparison; tree + rebalancing; List interop *additive* | rung 2+/3 | heavy tail |
| **`Vesper.Set`** | base + Comparison; tree + rebalancing; List interop *additive* | rung 2+/3 | heavy tail |

```
Vesper.Core ──┬─ Vesper.Option
              ├─ Vesper.Result
              ├─ Vesper.List ······· (List interop, additive) ······┐
              └─ Vesper.Comparison ──┬─ Vesper.Map  ◄───────────────┤
                                     └─ Vesper.Set  ◄───────────────┘
Vesper  (rollup: depends-on all; no files of its own)
```

## Decisions

- **PS1 — One package per type.** `Vesper.Option`, `Vesper.Result`,
  `Vesper.List`, `Vesper.Map`, `Vesper.Set` are each standalone, even though
  Option/Result add no dependencies beyond base. Rationale: the package is a
  **unit of self-hosting work**, not a product SKU — each type + module is its
  own toolchain pipeline. Over-decomposition is acceptable here because the cost
  of an extra manifest/DLL is small relative to the work of standing each type up,
  and consolidation is deferred (PS6).
- **PS2 — One impl DLL per package.** Each package compiles to its own BCL-only
  DLL; cross-package type references are cross-assembly references
  (`Vesper.Map.dll` → `Vesper.Comparison.dll` → `Vesper.Core.dll`). The backend
  already emits cross-assembly refs, and the P2/G6 **lazy-ref** machinery already
  drops unused references — so a consumer's emitted PE references *exactly* the
  package DLLs its IL touches, with no extra work. "Pay for what you use" extends
  all the way to the shipped bundle.
- **PS3 — Each package milestone is two legs, gated on R1.** The *type* leg
  (emit the union/struct/record) can land before R1 (the
  `Fun`-not-`FSharpFunc` cutover). The *module* leg almost always cannot:
  module functions are overwhelmingly higher-order (`Option.map`, `List.fold`,
  `Set.filter`), so they need `Fun`. **R1 is the shared linchpin for the module
  leg of every package.** A type-only half-package is a coherent shippable unit
  (PS2 gives it its own DLL), so type legs may ship ahead of R1 with the module
  leg on the C# interim per package — or each package may be held until R1 so it
  ships whole. Decide per package.
- **PS4 — Map/Set need a sixth capability: ordered comparison — DESIGNED.**
  [brainstorm-structural-equality](brainstorm-structural-equality.md) covers
  `=`/`hash` (compiler-emitted `IEquatable<T>` + `EqualityComparer<T>.Default`;
  §5.4 already has Set/Map implement `IEquatable<T>`), but balanced trees need
  **ordering** (`compare` / `IComparer<'T>`), which that spec does not address.
  This is a hard prerequisite for both tree packages and is **not** one of the
  five listed types. **Resolved** (PS4-Q closed) in
  A **`Vesper.Comparison` package** that Map/Set `depends-on`. Its shape:
  1. holds the four ordering operators (`< > <= >=`) **and** `compare` / `min` /
     `max` — the whole ordering surface;
  2. ships **no runtime type** — structural ordering rides BCL
     `Comparer<T>.Default` + compiler-emitted `IComparable<T>`/`CompareTo`, just
     as equality rides `EqualityComparer<T>.Default`;
  3. the recognised attributes (`[<StructuralComparison>]` / `[<NoComparison>]` /
     `[<CustomComparison>]`) live in **`Vesper.Core`**, not this package — the DAG
     forbids Core depending on Comparison and Core types (e.g. `Result`) must be
     annotatable;
  4. primitive ordering (`1 < 5`, CIL `clt`) is in the **default contract
     closure**, so writing `<` does not force a `Vesper.Comparison` reference and
     the emitted PE carries no `AssemblyRef` for it; only per-type `CompareTo`
     **generation** is the genuinely opt-in part, gated on
     `[<StructuralComparison>]`.

  Still must be stood up before either tree package, but the design is no longer
  open.
- **PS5 — Package name ≠ namespace.** `Vesper.Option` contributes type `Option`
  in namespace `Vesper`; `Vesper.List`/`Vesper.Map`/`Vesper.Set` all contribute
  into namespace `Vesper.Collections`. A namespace spanning three DLLs is legal in
  .NET, and the loader already accumulates symbols across files regardless of
  namespace — but it means **`Vesper.Collections` is no longer owned by a single
  package**. State this explicitly so the conformance check and any future
  namespace-ownership assumptions account for it.
- **PS6 — Merge / rollup is a later publishing concern.** The `Vesper` rollup is
  just the root manifest enumerating every package (or a bucket with
  `depends-on = [all]` and `files = []`). A la carte is a *subset* root manifest
  + transitive closure. Whether the rollup ships as a meta-package (a DLL set) or
  an IL-merged single `Vesper.dll` is a packaging decision taken after the
  individual packages self-host; it does not affect the per-package work.
- **PS7 — Keep collection interop additive.** Map/Set's `ofList`/`toList` need
  `Vesper.List`; `ofSeq`/`toSeq` need only BCL `IEnumerable`. Keep the
  type + core ops depending on **base + Comparison only**, and add list-interop as
  an additive layer, so the tree packages don't take a hard dependency on
  `Vesper.List`. Keeps the DAG shallow.

## Milestone ordering

Ordered by toolchain readiness, with the shared linchpin and the Map/Set
prerequisite called out:

1. **R1 — `Vesper.Fun` cutover.**
   Not a package, but the gate for every package's module leg. Highest priority.
   **Done.**
2. **`Vesper.List`** — **done.** The type + `List.fold` landed (rung 2 / R3)
   and are now carved into their own
   `src/Vesper.List/` package compiling to a standalone `Vesper.List.dll` (PS2): an
   emitted program that uses lists carries a `Vesper.List` `AssemblyRef` alongside
   `Vesper.Core` (for `Fun`). `List.fold` is now compiled *into* `Vesper.List.dll`
   as `Vesper.Collections.ListModule::fold` (public module-function compilation,
   R3 deferred — done); the consumer calls it via a `MethodSpec`.
   `Vesper.List.dll` therefore references `Vesper.Core` (fold's folder is a `Fun`).
   This is the template the other packages copy.
3. **`Vesper.Option`, `Vesper.Result`** — simplest new types (generic union /
   struct union, no recursion in the *type*). Type legs need no new type
   machinery beyond what List proved; module legs ride R1. `Option` is a
   **struct** (`None` = zero-init, no `UseNullAsTrueValue`), so its F# sibling
   `ValueOption` is **removed** as redundant; `Result` gets opt-in
   `[<StructuralComparison>]`.
4. **`Vesper.Comparison`** (PS4) — stand up the ordered-comparison package
   (design settled; no longer a spike). Gates the tree packages.
5. **`Vesper.Map`, `Vesper.Set`** — the heavy tail: balanced tree + rebalancing
   (heavier than List's flat cons-list), plus the comparison and equality
   capabilities. List-interop additive (PS7).

## Concrete restructure tasks

The contract loader already does the topo-sort and cross-package resolution; the
physical work is layout + manifests + one loader extension.

1. **Decompose `core-types.fsi`.** `Option`/`'T option` + module → `Vesper.Option`
   and `Collections.List` + `List.fold` → `Vesper.List` are **already split out**;
   [`../../Vesper.Core/core-types.fsi`](../../Vesper.Core/core-types.fsi) now holds
   only `Ref` + `ValueOption` + `Result`. What remains: `Ref` stays with base,
   re-annotated `[<ReferenceEquality>]`/`[<NoComparison>]` (mutable State);
   `ValueOption`/`voption` is **removed**, not moved — redundant now that `Option`
   is a struct; `Result` → a new `Vesper.Result` package (opt-in
   `[<StructuralComparison>]`). Finishing this unblocks the
   per-package conformance work below.
2. **One directory per package** — `src/Vesper.Option/`, `src/Vesper.Result/`,
   `src/Vesper.List/`, `src/Vesper.Comparison/`, `src/Vesper.Map/`,
   `src/Vesper.Set/`. Each holds its `.fsi` contract, its `.fs` impl, and a
   **bucket-schema `manifest.toml`** (`name`, `depends-on`, `files`), every one
   `depends-on`-ing `Vesper.Core`. `Vesper.Core` keeps the base
   (`prim-types-*`, `ops-*`, `Ref`).
3. **A root manifest** enumerating the packages — the rollup. Pick a lib-root dir
   for it (cosmetic, but `loadAll`'s bucket paths are `Path.Combine`-relative to
   that root; a dedicated `src/Vesper/` lib-root with bucket paths to the sibling
   package dirs keeps `src/` clean).
4. **Per-package golden `.parsed` snapshots + conformance.** Extend
   `VesperCoreContractTests` so each package's `.fsi`/`.fs` parses with zero
   recovery diagnostics, and run the existing source-level conformance check
   (`Conformance.fs`, P4) per package pair.

## Loader change

One extension to `VesperLib.loadAll` ([`../VesperLib.fs:158`](../VesperLib.fs)):
it currently loads **every** bucket in the root manifest (`for b in buckets`).
The a la carte story needs it to **seed the topo-sort from a requested subset**
and pull in only the transitive closure. `topoSort`'s `visit` recursion already
computes closures via `depends-on`; the change is to start from the requested set
rather than from all buckets. The rollup is then "request all"; a la carte is
"request {X} and let the closure pull in Core/Comparison/etc."

(The bucket schema reuse may warrant a small refactor: `parseRootManifest` is
currently hard-wired to require an `[upstream]` table — appropriate for the
FSharp.Core *port*, but `Vesper.*` is authored, not ported, so the Vesper root
manifest should make `[upstream]` optional. cf. the `manifest.toml` note that
this tree has no `[upstream]` pin.)

## Risks / open questions

- **PS4-Q — Comparison: package vs compiler intrinsic — RESOLVED.** The
  `Vesper.Comparison` **package** (the clean dependency edge) holds the ordering
  operators + `compare`/`min`/`max`,
  with structural `CompareTo` emitted by the compiler against BCL
  `Comparer<T>.Default` (no `Runtime` helper, no runtime type) and the recognised
  attributes kept in `Vesper.Core` (PS4). The alternative — couple ordering into
  codegen with no package — is rejected; the package remains another self-host
  pipeline to stand up before either tree package.
- **R1 is the dominant blocker.** Until the `Fun` cutover lands, no package's
  module leg can ship BCL-free. The per-package DLL split (PS2) lets type legs
  proceed in parallel, but the modules — the bulk of each package's value —
  queue behind R1.
- **Namespace ownership split (PS5).** `Vesper.Collections` spanning three DLLs
  is legal but breaks any "one package owns one namespace" assumption; verify the
  conformance check and symbol provider don't rely on it.
- **Map/Set are the heaviest self-host rung.** Balanced trees + rebalancing +
  comparison + structural equality are well past List's flat cons-list — likely
  rung 2+/3. Expect the C# interim (PS3) to carry these longest.
- **Version pinning of the rollup (PS6).** Once packages version independently,
  the rollup must pin a compatible set. Deferred with the merge decision, but
  flagged so per-package conformance stays the source of truth.

## Cross-references

  per-package.
- [function-representation-plan](function-representation-plan.md) — `Vesper.Fun`,
  the representation R1 cuts over to (PS3 gate).
- [vesper-printf-plan](vesper-printf-plan.md) — `Vesper.Printf`, the existing
  precedent for a sibling `Vesper.*` library depending on `Vesper.Core`.
- [brainstorm-structural-equality](brainstorm-structural-equality.md) — `=`/`hash`
  for collections; the equality half of the Map/Set capability.
- [brainstorm-comparison](brainstorm-comparison.md) — the ordering half (`<`…,
  `compare`/`min`/`max`), opt-in `[<StructuralComparison>]`, §7c generic
  arg-recursion.
- [`../VesperLib.fs`](../VesperLib.fs) — the bucket loader (`loadAll` / `topoSort`
  / `ExtractCtx`) this plan repoints at `Vesper.Core`; the PS-Loader change.
- [extract-symbols-plan](extract-symbols-plan.md) — the symbol-extraction design
  behind the loader's `ExtractCtx`.
