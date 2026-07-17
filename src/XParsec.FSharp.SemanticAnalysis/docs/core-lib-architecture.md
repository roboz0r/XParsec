# Vesper core library — architecture

Durable record of the `Vesper.*` core-library split: what the packages are, how
the dependency graph is declared and resolved, and the *why* behind choices the
code cannot state for itself.

The core library is **eleven standalone packages** under `src/Vesper.*`, each a
directory holding a `manifest.toml`, its `.fsi` contract files, and (usually) the
`.fs` implementation the backend compiles into that package's DLL. There is no
rollup manifest and no umbrella package: a consumer names the root manifests it
wants and the resolver pulls in the transitive `depends-on` closure. À la carte is
the only mode.

Two neighbouring docs own the parts this one deliberately does not: how a
contract's signatures become fully-kinded `SemType`s is
[package-type-extraction-architecture](package-type-extraction-architecture.md);
`Vesper.Printf`'s internals are [printf-architecture](printf-architecture.md).

## A package is a directory with a manifest

Everything a package declares lives in one `[core]` table
(`ReferencedProject.parseManifest`, `ReferencedProject.fs:172-220`):

| key | meaning |
|---|---|
| `namespace` | the namespace the package's symbols live in, and its implicit auto-open prefix |
| `files` | contract `.fsi` files **in compile order** — the front-end symbol contract |
| `impl` | the `.fs` files compiled into the package DLL |
| `inline-bodies` | the `.fs` files whose `let inline` bodies are spliced across the package boundary; defaults to `impl` |
| `sig-only` | `.fsi` files deliberately impl-free — an accepted conformance exemption |
| `depends-on` | the other packages this one needs |
| `name` | **optional** — see below |
| `<key>-<target>` | per-target overrides (`impl-js`, `files-js`, `runtime-js`, …) |

**The directory name is the package identity.** `[core] name` is optional —
`Vesper.Core`, `Vesper.Exceptions` and `Vesper.Printf` omit it entirely — and when
present it is only *validated* to match the directory, never used as an
independent identity (`ReferencedProject.fs:188-195`). This matters because
`depends-on "Vesper.Core"` resolves by *path*, to the sibling
`src/Vesper.Core/manifest.toml` (`dependencyManifestPath`, `ReferencedProject.fs:234-244`).
Had an explicit `name` been allowed to diverge, a dependency would be resolved by
directory but reported by `Name`, and a `depends-on` written against `Name` would
silently miss. Parse-time rejection keeps the two identities from drifting.

**No package has a `.fsproj`.** None of this tree is built by `dotnet`/`fsc`. The
operators alone force it: a `let inline (+)` body needs inline IL /
`--compiling-fslib` (`Vesper.Core/ops-platform.fs`), so the contract is
signature-only from `fsc`'s point of view — which is why those bodies sit in
`inline-bodies`, not `impl` (`Vesper.Core/manifest.toml`). The `.fsi` are parsed by
`XParsec.FSharp` and walked into an `IExternalSymbolProvider`; the `.fs` are
compiled by this repo's own backend. Parser coverage is held by golden `.parsed`
snapshots committed next to each source (`test/Vesper.Tests/VesperCoreContractTests.fs`).

## The dependency graph

Read from each package's `[core] depends-on`:

| Package | Namespace | `depends-on` | Holds |
|---|---|---|---|
| **`Vesper.Core`** | `Vesper` | — | language fundamentals: `unit`, `Fun`, `Ref`, the primitive types, the operator + `hash` bodies, the compiler-recognised attributes, the `%A` interfaces |
| `Vesper.Array` | `Vesper.Collections` | Core | the `Array` module over the intrinsic `'T[]` |
| `Vesper.Choice` | `Vesper` | Core | `Choice<'T1,'T2>` |
| `Vesper.Comparison` | `Vesper` | Core | the ordering operators (`< > <= >=`) |
| `Vesper.Exceptions` | **`System`** | Core | the common BCL exception roots as contract types inheriting `exn` |
| `Vesper.List` | `Vesper.Collections` | Core | the cons-list type + the `List` module |
| `Vesper.Option` | `Vesper` | Core | the option type + the `Option` module |
| `Vesper.Result` | `Vesper` | Core | the result type + the `Result` module |
| `Vesper.Printf` | `Vesper` | Core, List | `printf`/`printfn`/`sprintf`, the format handler, the `%A` engine |
| `Vesper.Seq` | `Vesper.Collections` | Core, List, Comparison | the `Seq` module over `seq<'T>` |
| `Vesper.Set` | `Vesper.Collections` | Core, List, Array, Seq, Choice, Option, Comparison, Printf | the AVL-tree set type + the `Set` module |

Four depth tiers. Every package depends on `Vesper.Core`; only the non-Core edges
are interesting:

```txt
tier 0   Core

tier 1   Array   Choice   Comparison   Exceptions   List   Option   Result
                             ▲                       ▲
tier 2                       └──── Seq ──────────────┤
                                                     └──── Printf

tier 3   Set ──▶ List, Array, Seq, Choice, Option, Comparison, Printf
```

`Vesper.Set` is the only wide node, and its width is a property of its *source*,
not of the design: `set.fsi`/`set.fs` are verbatim copies of FSharp.Core's, with
the namespace and opens patched (`Vesper.Set/set.fsi:1-2`). The verbatim
body reaches for `sprintf` in its invariant-violation messages (⇒ Printf) and
`Set.toList`/`ofList` in its surface (⇒ List), so the edges follow the copy. A
package written for this tree rather than transliterated into it would not need
most of them.

`Vesper.Exceptions` is in the graph but not in every consumer's root set: it is
referenced by the **JS provider only**. A CLR build resolves the same names
through `System.Private.CoreLib`, so the contract never shadows the BCL type in a
`newobj` (which would mint a TypeRef into the wrong assembly). The shared seam
stays the `.fsi`; the per-target binding is `prim-types-exn`'s `(# … #)` repr —
`System.Exception` on CLR (`Vesper.Core/prim-types-exn.fs`), `Error` on JS
(`Vesper.Core/prim-types-exn.js.fs`).

## Why one package per type

The split is **not** product packaging. A package is the **unit of self-hosting
work**: a type plus its module, compiled to a BCL-only DLL and conformance-checked
against its `.fsi`. Standing each one up end-to-end through a toolchain that only
supports a subset of the language is its own pipeline. Over-decomposition is
therefore cheap — an extra manifest and DLL cost little next to the work of
standing the type up — and `Vesper.Option` / `Vesper.Result` are standalone even
though they add no dependency beyond Core.

**One impl DLL per package.** Cross-package type references are cross-assembly
references (`Vesper.Set.dll` → `Vesper.Comparison.dll` → `Vesper.Core.dll`). The
provider's refs are `lazy`, so an `AssemblyRef` row is added only when a ref is
actually forced — an emitted PE references *exactly* the package DLLs its IL
touches, with no dead rows (`Codegen.Clr.Tests/FSharpCoreDepsTests.fs:48-61`
holds the property for FSharp.Core; the same machinery drops unused `Vesper.*`
refs). "Pay for what you use" reaches the shipped bundle.

Not every package emits a DLL. `Vesper.Comparison` declares `impl = []`
explicitly: its four operators are signature-only (`let inline` over
static-optimized inline IL, spliced at each use site), so it is an inline-body
source and nothing else. `Vesper.Exceptions` is `sig-only` for the same
structural reason from the other direction — its mechanism is BCL-resolved.

**Merging is a later publishing concern, and nothing depends on it.** There is no
root manifest in the tree. A consumer hands `composeContract` the roots it wants
and the closure supplies the rest, so a la carte needs no mechanism it does not
already have; a rollup would only be a root set naming everything. Whether one
ever ships as a meta-package or an IL-merged `Vesper.dll` is a decision taken
after the packages self-host, and it does not reach back into the per-package
shape. See [publishing-format-plan](publishing-format-plan.md).

## The type and the module are separate legs

A package's two halves land independently, because they need different things
from the backend:

- **The type** — the union itself (`List<'T>`, `Option<'T>`, `Result<'T,'TError>`)
  needs nominal type emit, and a struct union needs value-typed union emit. It
  does not need the function representation.
- **The module** — `List.fold`, `Option.map`, `Result.bind` … are overwhelmingly
  higher-order, so the module cannot be emitted until `Vesper.Fun` is in place. It
  compiles *into* the package DLL as a static class
  (`Vesper.Collections.ListModule`) which a consumer `call`s via a `MethodSpec`.
  Module functions are deliberately **not `inline`** (unlike FSharp.Core): each
  functional-argument application lowers to `callvirt Fun::Invoke` instead of
  expanding at the call site.

The legs are separable because each package gets its own DLL — a type-only
half-package is still a coherent unit, which is why a type can land while its
module still waits on the function representation. A package is not all-or-nothing
against the self-hosting ladder.

## The data types are structs

`Option`, `Result` and `Choice` are `[<Struct>]` — a deliberate break with F#,
which represents `option` as a reference type with `UseNullAsTrueValue`
(`None` = `null`). A value type means `default(Option<'T>)` *is* `None`, an array
of options starts all-`None`, and neither arm allocates. The price is the usual
struct trade-off: copied by value (payload + tag), and boxed if it crosses an
`obj` boundary — worth it where the values are small and mostly non-escaping,
which is what these three are. The contract fixes only "a struct whose default is
the empty case"; the exact layout stays a backend decision. See
[brainstorm-option-representation](brainstorm-option-representation.md).

## Package name ≠ namespace

Namespaces **cross-cut** packages, and no namespace is owned by one package:

- `Vesper.Collections` spans **four** packages — Array, List, Seq, Set.
- `Vesper` spans Core, Choice, Comparison, Option, Result, Printf.
- `Vesper.Exceptions` publishes into **`System`**, matching the BCL names a
  consumer reaches via `open System`.

A namespace spanning several DLLs is legal in .NET, and the extractor accumulates
symbols across files regardless of namespace — but it means any "one package owns
one namespace" assumption is false here, and the conformance check and symbol
provider must not rely on one.

What *does* hold the line is the **composition-time duplicate sweep**
(`ReferencedProject.fs:699-746`): a qualified type name declared twice across the
referenced set would resolve as a silent first-hit shadow — the loser's type minted
with a correct key but unreachable by lookup — so composition refuses it outright, a
CS0433-equivalent naming both homes. Any second sighting is a collision: one
package never declares a key twice, so a repeat is either two peers sharing
namespace + name or two copies of one package. Intrinsics and capability faces are
excluded by design (every package's `int` is *the* `int`; a shared canon there is
not a collision).

## The ordering surface, and why the attributes stay in Core

`Vesper.Comparison` holds the ordering operators, while the recognised attributes
they key on — `StructuralComparisonAttribute` / `CustomComparisonAttribute` /
`NoComparisonAttribute` — live in **`Vesper.Core`**
(`Vesper.Core/compiler-attributes.fsi:34,81,97`). This is forced, not stylistic:
the DAG forbids Core depending on Comparison, and Core's own types must be
annotatable — `core-types.fsi:13` annotates `[<NoComparison>]`.

The mirror-image constraint is that a package depending only on Core must still be
able to write `1 < 5`. That is met by `Vesper.Core/int-comparison.fsi`, the
**primitive subset** of the ordering family (`IntComparison`, `int`-only), so
ordering on primitives costs no `Vesper.Comparison` reference and the emitted PE
carries no `AssemblyRef` for it. Only the polymorphic family — dispatching through
`Comparer<^T>.Default` for aggregates — is the genuinely opt-in part. Equality
(`=`/`<>`/`hash`) is not split this way: it stays whole in Core's
`ops-platform.fs`. See [brainstorm-comparison](brainstorm-comparison.md).

## Contract vs implementation, and per-target companions

The `.fsi` is the **target-agnostic contract** (`type int = extern`); the matching
`.fs` is the **per-target binding** (`type int = (# "System.Int32" #)`).
Resolution needs only the `.fsi` — an absent `.fs` is a codegen-side concern, not a
resolution failure (`ReferencedProject.fs:6-16`).

Per-target divergence is expressed by suffixed manifest keys, resolved by the
*backend* asking for its own suffix. SemanticAnalysis stores them inertly and never
enumerates target names (`collectOverrides`, `ReferencedProject.fs:112-125`):

| resolver | key | semantics |
|---|---|---|
| `resolveImpl` (`:130`) | `impl-<t>` | **replaces** the base list |
| `resolveInlineBodies` (`:138`) | `inline-bodies-<t>` | replaces |
| `resolveSigOnly` (`:165`) | `sig-only-<t>` | replaces |
| `resolveExtraFiles` (`:147`) | `files-<t>` | **appends** after the base contract |
| `resolveRuntime` (`:156`) | `runtime-<t>` | **no base key** — a runtime asset is inherently target-specific |

`files-<t>` appends rather than replaces so a shim may name a base-declared type
(the JS capability compat shim's RHS is `Vesper.disposable`, from the base
`capabilities.fsi`). `runtime-<t>` has no base because a `.mjs` is not a parsed
source at all — it is a hand-authored or backend-generated platform-support asset
the backend ships beside its output (`runtimeModules`, `:409`).

Three distinct companion patterns coexist, and the distinction is load-bearing:

- **`prim-types-int.fs`** — a base `.fs` binding a contract `extern` to its
  intrinsic repr, with `prim-types-int.js.fs` as the per-target override.
- **`ops-platform.fs`** — an inline-body source that is *not* a DLL compile target:
  its `let inline` bodies are read across the package boundary by the codegen
  inline-body loader and spliced at each use site. `Vesper.Core`'s DLL is the
  prim-types/`Ref` bodies; its operator semantics live here and nowhere else —
  codegen holds no op→opcode table.
- **`comparison.js.fs`** — a whole-file per-target re-authoring, selected by
  `inline-bodies-js`.

**Intrinsic reprs are harvested from the `.fs` before the `.fsi` is walked**
(`buildProviderWith`, `ReferencedProject.fs:531-580`), because the `.fs` is the
only place the repr lives — the `.fsi` commits `type exn = extern` and no repr.
There are two faces:

- the **base** `.fs` ⇒ `IntrinsicBaseReprs`: the primitive *marker*. Its presence
  is what publishes the `extern` as an `ExternalTypeShape.Intrinsic` rather than an
  opaque `Class`, and on CLR it is also the platform repr.
- the per-target `<base>.<target>.fs` ⇒ `IntrinsicReprs`: the platform face for
  *this* target (`prim-types-int.js.fs` ⇒ `number`).

A primitive the target omits — `decimal` ships no `.js.fs` — is in the base map but
not the target map, so it stays an `Intrinsic` with no platform face rather than
silently falling back to a BCL repr that has no JS runtime. The `canon` face is the
`.fsi` name itself, so an override never moves the unifier's identity key. A
target-only intrinsic (`undefined`, `dynamic`) has no base/override split at all:
its single `.js.fs` is both marker and platform face.

## Resolving the graph

`buildClosureWithDeps` (`ReferencedProject.fs:358`) closes a root manifest set over
`depends-on` and returns the manifest paths in **dependency order** plus each
package's transitive closure; `buildClosure` (`:349`) is the ordered-paths-only
projection. Both share the private `closeAndOrder` (`:265-345`), a post-order DFS
over the discovery order with gray/black colouring. Properties that callers rely on:

- **Dependency order**, always — each package appears after everything it depends
  on. That is what lets each package's extraction read its dependencies'
  already-built type shapes.
- **Stable over discovery order** — an already-ordered input is returned unchanged,
  so composition is deterministic.
- **A cycle is a hard error**, naming the package it runs through: contract
  packages may not be mutually recursive. So is a `depends-on` naming a package
  whose manifest is absent.
- Paths are normalised and de-duplicated, so `Vesper.Core` — named by every other
  package — is processed once.

`composeOrdered` (`:675`) is the single dependency-order wiring shared by the
codegen `SymbolProviders` stack and the in-assembly test fixtures; do not
re-implement its ambient-shape loop at a call site. It is **leaf-agnostic**: a
backend injects its own metadata tail (BCL `MetadataSymbols` on CLR, the JS-native
tail on JS) through the `MetaTailFactory` seam, and an in-assembly caller that
needs no metadata passes `noMetaTail`. `composeContract` (`:762`) is the
order-it-yourself convenience over a raw manifest set; `provider` (`:783`) is the
per-path cached single-package entry point.

The tests for these properties — including the name-vs-directory rejection above —
are `ReferencedProjectTests.fs`'s `buildClosure` list (`:490`).

## Cross-references

- [package-type-extraction-architecture](package-type-extraction-architecture.md) —
  how a contract `.fsi`'s signatures become kinded `SemType`s: ambient shapes, the
  dependency-scoped extraction, `TyUnknown`.
- [printf-architecture](printf-architecture.md) — `Vesper.Printf`'s internals; the
  first sibling `Vesper.*` library to depend on `Vesper.Core`.
- [publishing-format-plan](publishing-format-plan.md) — what a package's compiled
  artifacts look like on disk; the rollup/merge question.
- [function-representation-plan](function-representation-plan.md) — `Vesper.Fun`,
  the function representation every package's module functions are typed against.
- [brainstorm-comparison](brainstorm-comparison.md) — the ordering family
  (`< > <= >=`, `compare`/`min`/`max`) and opt-in `[<StructuralComparison>]`.
- [brainstorm-structural-equality](brainstorm-structural-equality.md) — `=`/`hash`,
  the equality half, which stays in `Vesper.Core`.
- [`../ReferencedProject.fs`](../ReferencedProject.fs) — the manifest schema, the
  `depends-on` closure, and the provider composition described here.
</content>
