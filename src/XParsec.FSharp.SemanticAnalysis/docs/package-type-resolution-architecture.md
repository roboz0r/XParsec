# Package type resolution — architecture

How a referenced package's contract `.fsi` signatures become fully-kinded
`SemType`s. The defining principle: **kind every nominal type reference where the
contract is resolved**, against the type shapes of the package's declared
dependencies, so a consumer reads a contract whose `val` signatures are already
correct and needs no reconciliation pass of its own.

This document describes the steady-state design. For the layering it sits inside
(layer 1 = referenced projects, layer 2 = referenced assemblies) see the broader
[architecture.md](architecture.md).

## The problem this solves

A project is a `manifest.<target>.toml` plus a set of contract `.fsi` files. ("Package"
throughout the rest of this document means *project* in that sense; the word now names the
distributable artifact, and the sweep has not reached this doc. See
`.claude/skills/comment-hygiene/vocabulary.md`.) A signature in one project routinely names a
type defined in another:
`Vesper.Result`'s `.fsi` mentions a `Vesper.Core` type, `Vesper.List.fold`'s
signature mentions `'T list`, and so on.

To bake a reference like `'T option` into a `SemType`, the resolver must know
the referent's **kind** — is `option` a union (`TyUnion`), a class (`TyClass`),
a record (`TyRecord`), a transparent abbreviation to expand, or an intrinsic
primitive that collapses to a short `TyConst`? For a type the same file declares,
that kind is in this compilation's `TypeRegistry`; for anything else it is the
*defining* package's published shape.

The design makes that cross-package kind information available at the moment a
type constructor is baked, by processing packages in dependency order and giving
each package a provider over its dependencies' already-built surfaces.

## One front end

There is no separate signature extractor. A `.fsi` — whether it is a referenced
package's contract or the sibling of a `.fs` in the assembly being compiled — goes
through [`SignatureResolution.run`](../Passes/SignatureResolution.fs) on a
`PassContext` built from its own `LexedFile`, driving the same `OpenScope`,
`TypeRegistry` and `translateType` every other pass drives. What it returns is a
[`PublishedSurface`](../PublishedSurface.fs), the same value a frozen `.fs` file
projects to, which becomes an `IExternalSymbolProvider`.

## Data flow

```txt
 manifest.<target>.toml (roots)
        │
        ▼
┌───────────────────────────────────────────────────────────────────┐
│ ReferencedProject.buildClosureWithDeps                            │
│   • transitively load every manifest reachable via `depends-on`   │
│   • topo-sort (cycle / missing dep ⇒ hard error)                  │
│   • emit: ordered manifests  +  per-package transitive dep closure │
└───────────────────────────────────────────────────────────────────┘
        │  ordered = [ dep … dependent ]      transitiveDeps : path → paths
        ▼
┌───────────────────────────────────────────────────────────────────┐
│ PackageProviders.composeOrdered  (builds BOTTOM-UP)                │
│                                                                    │
│   for manifest in ordered:                                         │
│     deps     = composite( dep providers ++ layer 2 )               │
│     provider = buildProviderWith deps manifest                     │
│                                                                    │
│   ┌──────────────────────────────────────────────────────────────┐ │
│   │ buildProviderWith                                            │ │
│   │   reprs = (# … #) bindings read from each `.fsi`'s paired .fs│ │
│   │   for each `.fsi` in `[core] files`:    (declared order)     │ │
│   │     ctx     = PassContext(own ++ deps ++ prelude, source)    │ │
│   │     surface = SignatureResolution.run ctx { target; reprs }  │ │
│   │     own     = surface.toProvider :: own      (nearest first) │ │
│   │                                                              │ │
│   │   nominal tyCtor, inside `translateType`:                    │ │
│   │     this file's TypeRegistry ─┐                              │ │
│   │     the provider's shapes ────┴▶ buildExternalTy / TyClass…  │ │
│   │     neither ─────────────────────▶ report + bake TyUnknown   │ │
│   └──────────────────────────────────────────────────────────────┘ │
│                                                                    │
│   composite( built ++ layer 2 )  ◀── final layer-1 ++ layer-2 stack │
└───────────────────────────────────────────────────────────────────┘
        │  IExternalSymbolProvider (symbols carry baked, kinded SemTypes)
        ▼
┌──────────────────────────┐     ┌───────────────────────────────────┐
│ Front end                │     │ Codegen (ClrEncoder)              │
│ Pipeline.analyse         │     │ sym.Instantiate → encode          │
│ sym.Instantiate          │     │ TyUnknown ⇒ internal error        │
│ TyUnknown ⇒ use-site err │     │ (front end should have errored)   │
└──────────────────────────┘     └───────────────────────────────────┘
```

## The pieces

### Dependency ordering — `ReferencedProject`

[`buildClosureWithDeps`](../ReferencedProject.fs) closes a root manifest set over
`[core] depends-on`, topologically sorts it (a cycle or a missing dependency
manifest is a hard `Error`), and returns two things:

- the **dependency-ordered** manifests (each package after everything it
  depends on), and
- a **transitive `depends-on` closure** lookup (normalised path → the paths it
  depends on, directly or transitively, itself excluded), each closure itself in
  dependency-first order.

`depends-on "X"` resolves to the sibling `X/manifest.<target>.toml` — the same
target as the manifest that declared it — by the convention that **a package's
directory name is its identity**. `parseManifest` rejects an explicit
`[core] name` that diverges from the directory name, so the name a `depends-on`
resolves against and the name a manifest reports can't drift apart.

This file knows nothing about resolution: it reads manifests and orders them.

### Building bottom-up — `PackageProviders.composeOrdered`

[`composeOrdered`](../PackageProviders.fs) walks the ordered manifests and builds
each one's provider in turn, accumulating into a `byPath` index. Before building a
package it composes that package's **dependency provider**:

- the providers of its **transitive `depends-on` closure** (resolved from
  `byPath` — dependency order guarantees they are already built), plus
- **layer 2** (a backend's platform metadata: the BCL via `MetadataLoadContext`,
  or the JS-native stubs), seeded with the intrinsic axis of those dependencies.

Scoping visibility to the *declared* closure rather than to all topological
predecessors is deliberate: a package that never declared a `depends-on` for
another can't silently kind a type constructor against it just because it happened
to sort earlier. An undeclared cross-package reference is reported and bakes
`TyUnknown`.

Layer 2 is in scope so a contract naming a raw BCL nominal with no `extern` alias
in its own package (e.g. `System.Text.StringBuilder`) kinds correctly, instead of
baking a spurious `TyUnknown` for a type the consumer would resolve through layer 2
anyway. The result: a package resolves against *exactly* "what the consumer sees,
restricted to this package's dependency closure".

A qualified type name declared by two packages in the referenced set is refused
here rather than resolving as a silent first-hit shadow.

### One package, file by file — `buildProviderWith`

The fold matches what `AssemblyFiles.analyseAssemblyWith` does over `.fs` units:
each `[core] files` entry is resolved against the ones before it, nearest first,
over the dependency provider. Declarations therefore come into scope where they are
written, as F# resolves them, across files as well as within one.

Two things sit under the package's own files in that stack:

- the **dependency provider** described above, and
- the package's **own prelude**, read off the `[<assembly: AutoOpen("…")>]`
  attributes of its implementation files before the fold begins, as a source that
  resolves nothing and publishes only auto-opens, so a `.fsi` in
  `namespace Vesper.Collections` names `unit` exactly as a consumer would.

Before each `.fsi` is resolved, the `(# … #)` bindings of its paired `.fs` are read
into one table. A contract commits `type exn = extern` and leaves the spelling to its
implementation, so the repr has to be available when the `extern` is published.

What the package publishes is the composite of its own files' surfaces, stamped
with `SymbolHome.InAssembly` and carrying its `[<AutoOpen>]` modules ahead of the
namespaces its assembly-level attributes name.

### Kinding a type constructor

`translateType` resolves a written name through the ordinary resolution stack: the
open scope and this compilation's `TypeRegistry` first, then the provider. A hit on
the provider is kinded by
[`buildExternalTy`](../Passes/Unification/Translate.fs) off the published shape:

| shape | baked `SemType` |
| ------- | ----------------- |
| `Union` | `TyUnion(key, args)` |
| `Class` | `TyClass(key, args)` |
| `Record` | `TyRecord(key, args)` |
| `Enum` | `TyEnum key` — no args; enums are never generic |
| `Abbrev` | expand the frozen RHS, already kind-correct from the defining package |
| `Intrinsic` | `TyConst(canon, args)` — the canon, not the platform name |
| `IntrinsicInterface` | `TyClass(key, args)` — a capability is a nominal constraint |
| `Unmodelled` | declines: no body, so no kind a type annotation can take |

A declaration must therefore CLAIM the kind a consumer will read off its published
shape. The subtle case is `extern`: an untagged one and an `extern class` are
primitives and claim `IntrinsicRepr`, but an `extern interface` is a capability —
a nominal interface that merely carries a platform spelling — so it claims a class
and publishes `IntrinsicInterface` where the target binds a repr and a plain
interface `Class` where it binds none. Both read back as `TyClass`.

### Shapes — `ExternalTypeShape`

[`ExternalTypeShape`](../ExternalSymbols.fs) is the kind vocabulary a surface
publishes. `Unmodelled of reason * arity` is the residue: a type whose *name +
arity* the front end registered but whose body it does not model. Its
`UnmodelledReason` says which gap — `Delegate` / `TypeExtension` for a declaration
form this compiler stubs, or `ExtractionFailed reason` for a body that used a form
it could not translate. Carrying the reason is what lets a use site name the gap
instead of degrading silently; carrying the arity is what lets the name resolve at
all.

An `Enum` body needs no deferral: a case value is a literal, never a type
reference, so the case → value table is read straight off the CST, through
[`EnumCaseValues.tryResolve`](../EnumCaseValues.fs) — the same projection the
Elaborate pass runs, so a referenced package's `E.C1` and a locally-compiled `E.C1`
cannot disagree about the constant. One case that projects to no literal downgrades
the WHOLE enum to `ExtractionFailed`: a partial case table would resolve the
cases that survived and deny the rest.

### What a signature could not publish

Two failure modes, deliberately different:

- A **member** whose signature names a type this compilation cannot resolve is
  DROPPED, refusals and all, and the drop is REPORTED as
  `ConformanceVerdict.SignatureNotPublished` — a warning, because it is a gap in
  what this compiler models rather than a fault in the program. Discovering the
  absence as an unresolved name three files later is worse than a warning here.
- A **val** that fails is an error, and the val is retained carrying `TyUnknown`.

### The unresolved marker — `TyUnknown`

[`SemType.TyUnknown of name`](../SemanticInfo.fs) is the absorbing element for a
nominal reference that resolved to nothing in scope.
[`unify`](../Passes/Unification/Engine.fs) matches it first
(`TyUnknown name, _ | _, TyUnknown name`), emits a use-site diagnostic — *"Type
'name' could not be resolved during contract extraction — is a package
dependency missing?"* — and leaves the other side untouched (no `Link`), so one
broken type constructor can't cascade. Every other exhaustive `SemType` match
carries an inert arm, and `ClrEncoder.encodeTypeCore` rejects it with a pointed
internal-error message: `TyUnknown` must never reach the backend, because the front
end errors on it first.

## What the consumer no longer does

Because val signatures are baked kind-correct where the contract is resolved, the
consumer-side reconciliation that an earlier design needed is gone:
`ExternalSymbols.normalizeNominal`, `UnificationTranslate.normalizeExternalValueTy`,
and `ClrRecipes.normalizeSig` were deleted; their callers use bare
`sym.Instantiate` directly. An abbreviation expands in place on both sides, because
its frozen RHS was already kinded against the defining package's scope and the
wider consumer scope can't improve it.

## Invariants worth preserving

- **A declaration claims the kind its published shape reads back as.** The two are
  filled by different code — the registrar and the publisher — and a use site in the
  declaring file takes the first while every other file takes the second, so a
  divergence shows up as `TyConst` against `TyClass` for one written name.
- **The intrinsic axis is DERIVED, never filled.** `PublishedSurface.ofBuilder`
  reads it off the published `Intrinsic` shapes, which is what keeps a capability
  interface — whose platform name is carried on its own identity — off it by construction.
- **Visibility = declared dependency closure + layer 2 + the prelude.** Widening it
  to all topological predecessors would re-admit undeclared cross-package
  references; narrowing it would mis-bake raw BCL nominals or leave a contract
  unable to name `int`.
- **Directory name is the package identity.** `parseManifest` enforces it so
  `depends-on` resolution and cycle/error reporting can't disagree.
