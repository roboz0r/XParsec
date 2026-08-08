# Package type extraction — architecture

How a referenced package's contract `.fsi` signatures become fully-kinded
`SemType`s. The defining principle: **kind every nominal type reference at
extraction time**, against the type shapes of the package's declared
dependencies, so a consumer reads a contract whose `val` signatures are already
correct and needs no reconciliation pass of its own.

This document describes the steady-state design. For the layering it sits inside
(layer 1 = referenced projects, layer 2 = referenced assemblies) see the broader
[architecture.md](architecture.md).

## The problem this solves

A package is a `manifest.toml` plus a set of contract `.fsi` files. A signature
in one package routinely names a type defined in another:
`Vesper.Result`'s `.fsi` mentions a `Vesper.Core` type, `Vesper.List.fold`'s
signature mentions `'T list`, and so on.

To bake a reference like `'T option` into a `SemType`, the extractor must know
the referent's **kind** — is `option` a union (`TyUnion`), a class (`TyClass`),
a record (`TyRecord`), a transparent abbreviation to expand, or an intrinsic
primitive that collapses to a short `TyConst`? That kind lives in the *defining*
package's extraction state, not the referencing one.

The design makes that cross-package kind information available at the moment a
type constructor is baked, by processing packages in dependency order and giving each
package read access to its dependencies' already-built type shapes.

## Data flow

```txt
 manifest.toml (roots)
        │
        ▼
┌───────────────────────────────────────────────────────────────────┐
│ ReferencedProject.buildClosureWithDeps                            │
│   • transitively load every manifest reachable via `depends-on`   │
│   • topo-sort (cycle / missing dep ⇒ hard error)                  │
│   • emit: ordered paths  +  per-package transitive dep closure    │
└───────────────────────────────────────────────────────────────────┘
        │  ordered = [ dep … dependent ]      transitiveDeps : path → paths
        ▼
┌───────────────────────────────────────────────────────────────────┐
│ SymbolProviders.composeProviders  (builds BOTTOM-UP)              │
│                                                                   │
│   for path in ordered:                                            │
│     ambientShapes = composite( deps' providers ++ BCL ).TryLookupType
│     provider      = ReferencedProject.buildProviderWith ambientShapes path
│     byPath[path]  = provider                                      │
│                                                                   │
│            ambientShapes ─────────────┐                           │
│                                       ▼                           │
│   ┌─────────────────────────────────────────────────────────────┐ │
│   │ buildProviderWith  →  ExtractCtx                            │ │
│   │   ctx.AmbientShapes <- ambientShapes                        │ │
│   │   walk .fsi files → extractSymbols → translateType          │ │
│   │                                                             │ │
│   │   nominal tyCtor:                                           │ │
│   │     resolveTypeName ──Error──▶ bake  TyUnknown name         │ │
│   │            │ Ok compiled                                    │ │
│   │            ▼                                                │ │
│   │     mkNominal: shapeOf ctx compiled                         │ │
│   │       own TypeShapes  ─┐                                    │ │
│   │       AmbientShapes ───┴▶ Union →TyUnion  Class →TyClass    │ │
│   │                          Record→TyRecord  Abbrev→expand     │ │
│   │                          Intrinsic→TyConst                  │ │
│   └─────────────────────────────────────────────────────────────┘ │
│                                                                   │
│   composite( built ++ BCL )  ◀── final layer-1 ++ layer-2 stack   │
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

- the **dependency-ordered** manifest paths (each package after everything it
  depends on), and
- a **transitive `depends-on` closure** lookup (normalised path → the paths it
  depends on, directly or transitively, itself excluded), each closure itself in
  dependency-first order.

`depends-on "X"` resolves to the sibling `src/X/manifest.toml` by the convention
that **a package's directory name is its identity**. `parseManifest` rejects an
explicit `[core] name` that diverges from the directory name, so the name a
`depends-on` resolves against and the name a manifest reports can't drift apart.

`buildClosure` is the simpler projection (ordered paths only); the core walk is
shared in the private `closeAndOrder`.

### Building bottom-up — `SymbolProviders.composeProviders`

[`composeProviders`](../../XParsec.FSharp.Codegen.Clr/SymbolProviders.fs) walks
the ordered manifests and builds each one's provider in turn, accumulating into a
`byPath` index. Before building a package it assembles that package's **ambient
shapes**: the composite `TryLookupType` of

- the providers of its **transitive `depends-on` closure** (resolved from
  `byPath` — dependency order guarantees they are already built), plus
- **layer 2** (`MetadataSymbols.provider`, the BCL via `MetadataLoadContext`).

Scoping the ambient to the *declared* closure rather than to all topological
predecessors is deliberate: a package that never declared a `depends-on` for
another can't silently kind a type constructor against it just because it happened to sort
earlier. An undeclared cross-package reference bakes `TyUnknown` and surfaces at
the use site.

Layer 2 is in the ambient so a contract naming a raw BCL nominal with no
`extern` alias in its own package (e.g. `System.Text.StringBuilder`) kinds
correctly at bake time, instead of baking a spurious `TyUnknown` for a type the
consumer would resolve through layer 2 anyway. The result: a package's ambient is
*exactly* "the `TryLookupType` the consumer sees, restricted to this package's
dependency closure".

The final return is `composite (built ++ [BCL])` — the whole layer-1 stack ahead
of layer 2. `buildContract` caches this composite (and its cross-package inline
bodies) per manifest set.

### Read access during extraction — `ExtractCtx`

[`ExtractCtx`](../VesperLib/TyparCapture.fs) gains one settable field,
`AmbientShapes : string -> ExternalTypeShape voption`. `buildProviderWith` sets it
*before* walking the `.fsi` files (signature translation runs inside the ctx, so
the seam must be live first). The default is `fun _ -> ValueNone` — the
dependency-free, no-metadata path used by single-package tests and
`buildProvider`.

`ExtractCtx.shapeOf` is the single in-scope lookup the kinding step consults:

```fsharp
let shapeOf (ctx: ExtractCtx) (compiled: string) : ExternalTypeShape voption =
    match ctx.TypeShapes.TryGetValue compiled with
    | true, s -> ValueSome s          // this package's own shapes win
    | _ -> ctx.AmbientShapes compiled  // then dependencies + BCL
```

Own shapes shadow a dependency's on a name clash, matching the consumer
composite's first-source-wins priority. A package's own shapes are registered as
its files are walked, so an intra-package *forward* reference resolves only when
it is legal — inside a `type … and …` group or a `rec` scope, whose
mutually-referential shapes register together before any body is kinded. Any
other same-package miss is a genuine `TyUnknown`, not an ordering artefact.

### Kinding a type constructor — `mkNominal` and `resolveTypeName`

[`translateType`](../VesperLib/TypeTranslate.fs) handles the three nominal forms
(`NamedType`, `GenericType`, `'T list` `SuffixedType`) in two steps:

1. **`resolveTypeName`** maps the *written* name to a *compiled* name. It checks
   this package's own qualified set and short-name index first, then consults
   `ctx.AmbientShapes` for a dotted cross-package reference (`Vesper.Option`
   directly; a short name via each open-prefix candidate). A miss returns
   `Error`, and that arm bakes `TyUnknown name` — retaining the `val` and
   surfacing the defect at the use site, rather than the old silent skip.

2. **`mkNominal`** runs *inside the deferred builder* (at `Instantiate` time, when
   `ctx.TypeShapes` is fully populated) and kinds the compiled name against
   `shapeOf`:

   | shape | baked `SemType` |
   | ------- | ----------------- |
   | `Union`     | `TyUnion(compiled, args)` |
   | `Class`     | `TyClass(compiled, args)` |
   | `Record`    | `TyRecord(compiled, args)` |
   | `Abbrev`    | expand `build args` (already kind-correct from the defining package) |
   | `Intrinsic` | `TyConst <shortName>` |
   | `Opaque`    | **`failwith`** — body-less, no kind to bake |
   | `ValueNone` | **`failwith`** — invariant: a resolved name always has a shape |

Both `failwith` arms are loud invariant assertions, not fall-throughs. They
encode two facts established elsewhere: every registered type declaration also
registers a *shape* (so `ValueNone` is unreachable for a resolved name), and an
`Opaque` residue is never named by a shipping contract.

### Shapes — `ExternalTypeShape`

[`ExternalTypeShape`](../ExternalSymbols.fs) is the kind vocabulary `shapeOf`
returns: `Abbrev`, `Record`, `Union`, `Class`, `Intrinsic`, and `Opaque of
arity`. `Opaque` is the residue: a type whose *name + arity* the extractor
registered but whose body it does not model — an `enum` / `delegate` /
type-extension (the body is deferred), or a union/record/abbreviation whose body
used an unsupported form. Coupling registration so that **every**
`registerTypeDecl` also writes a shape (an `Opaque` for the deferrals) is what
makes `shapeOf` total over resolvable names and `mkNominal`'s `ValueNone`
unreachable.

A type the extractor *does* model contributes a real shape and kinds normally.
The cons-list `Vesper.Collections.List` is the worked example: its `.fsi`
declares `| ([]): … | (::): …` in GADT syntax, but `extractUnionBody` extracts
those operator cases (taking fields from the `(::)` arg spec, ignoring the
explicit return type) and registers a genuine `Union` shape, so every `'T list`
reference kinds as `TyUnion`. Its cases keep compiled-op names (`op_Nil` /
`op_ColonColon`) rather than `Empty`/`Cons` so they can't collide with a user
union's own cases in ctor-name resolution.

### The unresolved leaf — `TyUnknown`

[`SemType.TyUnknown of name`](../SemanticInfo.fs) is the absorbing element for a
nominal reference that resolved to no in-scope shape. It is baked only at
`resolveTypeName`'s failure arms — never by `mkNominal`, which is reached only
*after* a name has resolved.

[`unify`](../Passes/Unification/Engine.fs) matches it first
(`TyUnknown name, _ | _, TyUnknown name`), emits a use-site diagnostic — *"Type
'name' could not be resolved during contract extraction — is a package
dependency missing?"* — and leaves the other side untouched (no `Link`), so one
broken type constructor can't cascade. `checkConstraint` defers on it (the real error already
fired at `unify`). Every other exhaustive `SemType` match carries an inert leaf
arm, and `ClrEncoder.encodeTypeCore` rejects it with a pointed internal-error
message: `TyUnknown` must never reach the backend, because the front end errors
on it first.

## What the consumer no longer does

Because val signatures are baked kind-correct at extraction, the consumer-side
reconciliation that an earlier design needed is gone:
`ExternalSymbols.normalizeNominal`, `UnificationTranslate.normalizeExternalValueTy`,
and `ClrRecipes.normalizeSig` were deleted; their callers use bare
`sym.Instantiate` directly. An abbreviation expands in place at both extraction
(`mkNominal`'s `Abbrev` arm) and the consumer (`tryResolveExternalType`), because
an abbreviation's `build` closure *is* the defining package's `translateType`
builder — its type constructors already kinded against that package's scope, so the wider
consumer scope can't improve them.

## Invariants worth preserving

- **`shapeOf` is total over resolvable names.** Every `registerTypeDecl` registers
  a shape; a body it can't model registers `Opaque`. Don't add a registration
  path that writes a name without a shape — `mkNominal`'s `ValueNone` `failwith`
  is the tripwire.
- **`TyUnknown` is baked at name-resolution failure, not at kinding.** The
  distinction matters: `mkNominal`'s `ValueNone` is *name resolved, shape
  missing* (an invariant violation), whereas a `resolveTypeName` `Error` is *name
  unresolved* (a real, expected contract defect).
- **Ambient = declared dependency closure + layer 2.** Widening it to all
  topological predecessors would re-admit undeclared cross-package references;
  narrowing it to exclude layer 2 would mis-bake raw BCL nominals.
- **Directory name is the package identity.** `parseManifest` enforces it so
  `depends-on` resolution and cycle/error reporting can't disagree.
- **Intrinsic short-name collapse** assumes two packages binding the same
  primitive name agree on the short form (they do via `IntrinsicReprTypes`); if
  that ever stops holding, the `Intrinsic` arm must key on the repr.
