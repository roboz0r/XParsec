# Vesper core library — architecture

Durable record of the `Vesper.*` core-library split: what the packages are, how
the dependency graph is declared and resolved, and the *why* behind choices the
code cannot state for itself.

The core library is **eleven standalone packages** under `src/Vesper.*`, each a
directory holding one `manifest.<target>.toml` per target it builds for, its `.fsi`
contract files, and (usually) the `.fs` implementation the backend compiles into
that package's DLL. There is no rollup manifest and no umbrella package: a consumer
names the root manifests it wants and the resolver pulls in the transitive
`depends-on` closure. À la carte is the only mode.

Two neighbouring docs own the parts this one deliberately does not: how a
contract's signatures become fully-kinded `SemType`s is
[package-type-resolution-architecture](package-type-resolution-architecture.md);
`Vesper.Printf`'s internals are [printf-architecture](printf-architecture.md).

## A package is a directory with one manifest per target

Everything a package declares for a target lives in one flat `[core]` table, in
`manifest.<target>.toml`. Each list is that target's compile order, written once and
read once; the file name is the only place a manifest states which target it is for.

| key | meaning |
|---|---|
| `files` | every source file **in compile order**, each `.fsi` contract immediately ahead of its companion `.fs`. The `.fsi` half is the front-end symbol contract; the `.fs` half is compiled into the package DLL and published as the splice sources |
| `runtime` | hand-authored runtime *assets* (the JS `.mjs`) the backend ships beside its output |
| `depends-on` | the other packages this one needs, **for this target**, each a path relative to this package's directory |
| `name` | **optional** — see below |

A package that builds for both targets writes both files, and the shared entries are
duplicated between them. That is deliberate: the alternative — a shared tier the
per-target lists inherit — has to pick an append order, and the only order it can
pick puts a target-neutral body *before* the target-specific declarations it needs.

`depends-on` is per target for the same reason a file list is. `Vesper.Printf`
depends on `Vesper.List` on the CLR, whose `%A` engine uses the cons-list as its
`Doc` child lists and frame stack, and on Core alone on JS, whose `%A` engine is a
free function.

**The directory name is the package identity**, and the assembly name it emits under.
`[core] name` is optional — `Vesper.Core` and `Vesper.Printf` omit it entirely — and
when present it is only *validated* to match the directory, never used as an
independent identity. Had an explicit `name` been allowed to diverge, a package would
be resolved by directory but reported by `Name`, and the DLL a consumer references
would not be the one its `AssemblyRef` names. Parse-time rejection keeps the two
identities from drifting.

**A `depends-on` entry is a PATH, relative to the depending package's own directory**,
resolved to the dependency's manifest **for the same target**:
`src/Vesper.List/manifest.js.toml` writes `"../Vesper.Core"` and reaches
`src/Vesper.Core/manifest.js.toml`. A path rather than a bare name because a package
that does not sit beside its dependency — a test fixture, anything outside `src/` —
has no other way to reach it. The resolved path is canonicalised, so two spellings of
one package are one package.

A package that ships no `manifest.<target>.toml` does not build for that target, and
naming it from a closure for that target is a hard error rather than an empty
contribution. `Vesper.Set` is CLR-only.

**No package has a `.fsproj`.** None of this tree is built by `dotnet`/`fsc`. The
operators alone force it: a `let inline (+)` body needs inline IL /
`--compiling-fslib` (`Vesper.Core/ops-platform.clr.fs`), so the contract is
signature-only from `fsc`'s point of view. The `.fsi` are parsed by
`XParsec.FSharp` and walked into an `IExternalSymbolProvider`; the `.fs` are
compiled by this repo's own backend. Parser coverage is held by golden `.parsed`
snapshots committed next to each source (`test/Vesper.Tests/VesperCoreContractTests.fs`).

## The dependency graph

Read from each package's `[core] depends-on`. `clr` unless a `js` column differs:

| Package | Namespace | `depends-on` | Holds |
|---|---|---|---|
| **`Vesper.Core`** | `Vesper` | — | language fundamentals: `unit`, `Fun`, `Ref`, the primitive types, the operator + `hash` bodies, the compiler-recognised attributes, the `%A` interfaces |
| `Vesper.Array` | `Vesper.Collections` | Core | the `Array` module over the intrinsic `'T[]` |
| `Vesper.Choice` | `Vesper` | Core | `Choice<'T1,'T2>` |
| `Vesper.Comparison` | `Vesper` | Core | the ordering operators (`< > <= >=`) |
| `Vesper.List` | `Vesper.Collections` | Core | the cons-list type + the `List` module |
| `Vesper.Option` | `Vesper` | Core | the option type + the `Option` module |
| `Vesper.Result` | `Vesper` | Core | the result type + the `Result` module |
| `Vesper.Printf` | `Vesper` | Core, List — **js: Core** | `printf`/`printfn`/`sprintf`, the format handler, the `%A` engine |
| `Vesper.Seq` | `Vesper.Collections` | Core, List, Comparison, Array | the `Seq` module over `seq<'T>` |
| `Vesper.Set` | `Vesper.Collections` | Core, List, Array, Seq, Choice, Option, Comparison, Printf — **clr only** | the AVL-tree set type + the `Set` module |

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
not of the design: `set.fsi`/`set.clr.fs` are verbatim copies of FSharp.Core's, with
the namespace and opens patched (`Vesper.Set/set.fsi:1-2`). The verbatim
body reaches for `sprintf` in its invariant-violation messages (⇒ Printf) and
`Set.toList`/`ofList` in its surface (⇒ List), so the edges follow the copy. A
package written for this tree rather than transliterated into it would not need
most of them.

The BCL exception roots are **not a package**: they are `Vesper.Core`'s
`exceptions.js.fsi`, named in the JS manifest's `files` and `sig-only` and in no
CLR manifest at all. A CLR build resolves those names through
`System.Private.CoreLib`, so the contract never shadows the BCL type in a `newobj`
(which would mint a TypeRef into the wrong assembly); being reachable from no CLR
manifest is what makes that unreachable rather than merely unexercised. They ride
in with Core rather than through a `depends-on` edge a consumer must remember. The
shared seam stays the `.fsi`; the per-target binding is `prim-types-exn`'s
`(# … #)` repr —
`System.Exception` on CLR (`Vesper.Core/prim-types-exn.clr.fs`), `Error` on JS
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

Not every package emits IL. Every declaration in `Vesper.Comparison`'s `.fs` is
`let inline` over static-optimized inline IL, spliced at each use site, so its DLL
is empty and the package is effectively an inline-body source.

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
- `Vesper.Core` publishes into **`System`** as well as `Vesper`: its
  `exceptions.js.fsi` carries the BCL exception roots a consumer reaches via
  `open System`.

A namespace spanning several DLLs is legal in .NET, and the extractor accumulates
symbols across files regardless of namespace — but it means any "one package owns
one namespace" assumption is false here, and the conformance check and symbol
provider must not rely on one.

What *does* hold the line is the **composition-time duplicate sweep**
(`PackageProviders.composeOrdered`): a qualified type name declared twice across the
referenced set would resolve as a silent first-hit shadow — the loser's type minted
with a correct key but unreachable by lookup — so composition refuses it, a
CS0433-equivalent naming both homes. Any second sighting is a collision: one
package never declares a key twice, so a repeat is either two peers sharing
namespace + name or two copies of one package. Intrinsics and capability canons are
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
`ops-platform.clr.fs`. See [brainstorm-comparison](brainstorm-comparison.md).

## Contract vs implementation, and per-target companions

The `.fsi` is the **target-agnostic contract** (`type int = extern`); the matching
`.fs` is the **per-target binding** (`type int = (# "System.Int32" #)`).
Resolution needs only the `.fsi` — an absent `.fs` is a codegen-side concern, not a
resolution failure.

Per-target divergence is expressed by the manifest a build reads, not by a key
inside one: `manifest.clr.toml` and `manifest.js.toml` each name the whole ordered
list their target compiles, so nothing has to be replaced or appended and
SemanticAnalysis never enumerates target names. A `.fsi` a target does not build —
the JS capability compat shim, the CLR `formatter.fsi` — is simply absent from the
other target's `files`.

The `.<target>.fs` **filename** suffixes remain, because two targets' bodies coexist
in one package directory. They are what `pairingKey` strips to marry a `.fs` to its
`.fsi`, and it strips only the suffix of the manifest's own target: reading
`manifest.js.toml`, `prim-types-int.js.fs` keys on `prim-types-int` and
`prim-types-int.clr.fs` would key on `prim-types-int.clr`. The manifest parse also
enforces that a paired `.fs` sits immediately after its `.fsi` in `files`, so the
key pairing and the written layout always agree.

`runtime` is the one key with no `.fs` counterpart, because a `.mjs` is not a parsed
source at all — it is a hand-authored or backend-generated platform-support asset the
backend ships beside its output.

Three distinct companion patterns coexist, and the distinction is load-bearing:

- **`prim-types-int.clr.fs`** — a `.fs` binding a contract `extern` to its intrinsic
  repr, with `prim-types-int.js.fs` as the JS manifest's counterpart.
- **`ops-platform.clr.fs`** — a splice source that is *not* a DLL compile target: its
  `let inline` bodies are read across the package boundary by the codegen inline-body
  loader and spliced at each use site. `Vesper.Core`'s DLL is the prim-types/`Ref`
  bodies; its operator semantics live here and nowhere else — codegen holds no
  op→opcode table.
- **`comparison.js.fs`** — a whole-file per-target re-authoring, the only `impl`
  entry `Vesper.Comparison`'s JS manifest names.

**Intrinsic reprs are read from a contract's PAIRED `.fs` before that `.fsi` is
resolved** (`PackageProviders.buildProviderWith`), because the `.fs` is the only place
the repr lives — the `.fsi` commits `type exn = extern` and no repr. The companion and
no other file, which is what the in-assembly `.fsi` path reads too.
There are two sources:

- the **base** `.fs` ⇒ `IntrinsicBaseReprs`: the primitive *marker*. Its presence
  is what publishes the `extern` as an `ExternalTypeShape.Intrinsic` rather than an
  opaque `Class`, and on CLR it is also the platform repr.
- the per-target `<base>.<target>.fs` ⇒ `IntrinsicReprs`: the platform name for
  *this* target (`prim-types-int.js.fs` ⇒ `number`).

A primitive the target omits — `decimal` ships no `.js.fs` — is a marker with no entry
in the target map, so it stays an `Intrinsic` whose platform axis NAMES that target as
binding no representation (`IntrinsicPlatform.Unsupported "js"`), rather than silently
falling back to a BCL repr that has no JS runtime. Mentioning it in a program is then an
error carrying the target's name; the absence of the `.fs` is the whole statement, so no
manifest key lists what a target lacks. The `canon` key is the
`.fsi` name itself, so an override never moves the unifier's identity key. A
target-only intrinsic (`undefined`, `dynamic`) has no base/override split at all:
its single `.js.fs` is both marker and platform name.

## Resolving the graph

`ReferencedProject.buildClosureWithDeps` closes a root manifest set over
`depends-on` and returns the manifest paths in **dependency order** plus each
package's transitive closure; `buildClosure` is the ordered-paths-only
projection. Both share the private `closeAndOrder`, a post-order DFS
over the discovery order with gray/black colouring. Properties that callers rely on:

- **Dependency order**, always — each package appears after everything it depends
  on. That is what lets each package's extraction read its dependencies'
  already-built type shapes.
- **Stable over discovery order** — an already-ordered input is returned unchanged,
  so composition is deterministic.
- **A cycle is refused**, naming the package it runs through: contract packages may
  not be mutually recursive. So is a `depends-on` whose manifest is absent. Both
  come back as a `PackageSetFault` diagnostic, not an exception — the compilation is
  over either way, and a caller that already gates on diagnostics should not also
  have to catch.
- Paths are normalised and de-duplicated, so `Vesper.Core` — named by every other
  package — is processed once.

`PackageProviders.composeOrdered` is the single dependency-order wiring shared by the
codegen `SymbolProviders` stack and the in-assembly test fixtures; do not
re-implement its dependency loop at a call site. It is **layer-2-agnostic**: a
backend injects its own platform metadata (BCL `MetadataSymbols` on CLR, the
JS-native stubs on JS) through the `PlatformMetadataFactory` seam, and an in-assembly
caller that needs none passes `noPlatformMetadata`. It hands back a
`ComposedContract` — the provider AND everything resolving those contracts found,
because a caller that drops the second gets a provider publishing less than the
contracts say. `composeContract` is the order-it-yourself convenience over a raw
manifest set; `buildProvider` is the single-package entry point.

The tests for these properties — including the name-vs-directory rejection above —
are in `ReferencedProjectTests.fs`.

## Cross-references

- [package-type-resolution-architecture](package-type-resolution-architecture.md) —
  how a contract `.fsi`'s signatures become kinded `SemType`s: the per-file fold, the
  dependency-scoped visibility, `TyUnknown`.
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
