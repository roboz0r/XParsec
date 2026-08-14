# XParsec.FSharp.SemanticAnalysis

Semantic analysis for the F# CST produced by `XParsec.FSharp`: name resolution,
type inference, escape analysis, validation. It consumes an immutable CST and
produces a **frozen, immutable Typed AST** that the code generators
(`XParsec.FSharp.Codegen.Clr`, `XParsec.FSharp.Codegen.Js`) lower to IL / JS.

This file is a **map, not a specification**. It tells you where things live and
which mental models you need; the code is the source of truth for what they do,
and the passes carry their own pre/post-condition contracts at the top of each
file. Nothing here enumerates supported syntax — that list rots faster than it
can be maintained. Ask the tests.

## The mental model

Almost every "why is it like this?" question bottoms out here:

> **immutable shared CST → mutable in-flight TAST → immutable shared frozen TAST**

Mutation isn't forbidden, it's **confined**. Both ends are immutable because they
are *shared* — the CST with the formatter/linter/IDE, the frozen TAST with the
backends. The middle is mutable because that's where mutation pays for itself
(in-place union-find, dictionary side tables) and because nothing there escapes:
it is scoped to a single `PassContext`, i.e. a single compilation. See
[`docs/architecture.md`](docs/architecture.md#the-governing-principle-immutable--mutable--immutable).

Two consequences you will meet immediately:

**The CST is never mutated; semantic facts live beside it.** Every CST node has a
64-bit [`NodeKey`](NodeKey.fs) (source offset + node kind). The resolved binding,
the type variable, the escape state all live in `Dictionary<NodeKey, _>` side
tables on the [`PassContext`](PassContext.fs). Passes write slots; the tree stays
put.

**There are two type domains, and exactly one cut between them.**

| Domain | Type | Mutable? | Lives where |
|---|---|---|---|
| Inference | `SemType` — `TyVar` metavars in a union-find graph | yes, in place | every pass up to and including `Elaborate` |
| Output | `FrozenType` — no metavars; an open typar is a self-describing `FTTypar` | no | codegen's input, the assembly's output |

[`Freeze.fs`](Freeze.fs) is the single `SemType → FrozenType` rebuild and the
**last** step of the pipeline — the gate back out to the shared column. A metavar
is unrepresentable in `FrozenType` by construction, so codegen cannot receive one.

Keep the two apart: [`Elaborate.fs`](Elaborate.fs) sits in the *middle* of the
pipeline. It builds the TAST (CST → `TastFileG<SemType>`, inline call sites
expanded, typars quantified) but stays in the `SemType` domain — its tree still
points into the live, still-mutating `TypeVar` graph. **`Elaborate` builds the
tree; `Freeze` changes the type domain.** (`Elaborate` was itself once *called*
`Freeze`; if you hit a stray `Freeze` in an old comment or commit that clearly
means "builds the TAST", that is why.)

## The pipeline

[`Pipeline.fs`](Pipeline.fs) is the authoritative order — read it first; the
table below just annotates it. Strictly forward: each pass runs once and reads
only what its predecessors wrote. See
[`docs/passes.md`](docs/passes.md#why-no-fixpoint-at-the-pass-level) for why
there is deliberately no feedback edge.

| # | Step | File | What it does |
|---|---|---|---|
| 1 | `Desugar` | [`Passes/Desugar.fs`](Passes/Desugar.fs) | Annotation-only; mints synthetic `NodeKey`s. Never changes CST shape. |
| 2 | `NameResolution` | [`Passes/NameResolution.fs`](Passes/NameResolution.fs) + [`Passes/NameResolution/`](Passes/NameResolution/) | Scopes, shadowing, `open`, qualified lookup; registers types and members. |
| 3 | `Unification` | [`Passes/Unification.fs`](Passes/Unification.fs) + [`Passes/Unification/`](Passes/Unification/) | Algorithm J + Rémy's levels. By far the largest pass — one file per inference concern (`InferApp`, `InferOverload`, `InferPat`, …). |
| 4 | `Validation` | [`Passes/Validation.fs`](Passes/Validation.fs) | Read-only diagnostics: assignment to an immutable binding, value restriction, exhaustiveness. |
| 5 | `Elaborate` | [`Elaborate.fs`](Elaborate.fs), [`ElaborateExpr.fs`](ElaborateExpr.fs), [`Elaborate/`](Elaborate/) | CST → `TastFileG<SemType>`. Expands `inline` call sites, quantifies open typars. Side tables are discardable after this. |
| 6 | `Regions` | [`Passes/Regions.fs`](Passes/Regions.fs) | Escape analysis, on the **post-inline** tree — inlining both removes and exposes closures, so this must run over the closures codegen actually emits. |
| 7 | `RefCellPromotion` | [`Passes/RefCellPromotion.fs`](Passes/RefCellPromotion.fs) | Rewrites a `let mutable` captured by an escaping closure into a heap `Vesper.Ref<'T>` cell. |
| 8 | `ResolvedTypes` | [`ResolvedTypes.fs`](ResolvedTypes.fs) | Guard: a leaked `TyVar` becomes a graceful diagnostic here rather than a hard error in `Freeze`. |
| 9 | `PlatformTypes` | [`PlatformTypes.fs`](PlatformTypes.fs) | Guard: a primitive with no representation on the compiling target. |
| 10 | `DynamicEscape` | [`DynamicEscape.fs`](DynamicEscape.fs) | Warns where a `d?foo` result was pinned to a concrete type by context. |
| 11 | `Freeze` | [`Freeze.fs`](Freeze.fs) | The `SemType → FrozenType` cut. Output tree. |

**Entry points** (all in `Pipeline.fs`): `analyse*` returns the frozen tree — the
production path. `analyseSem*` stops **before** the freeze and returns the
`SemType` tree plus the `PassContext`; that is what front-end tests assert on.

## Landmarks

Where to look when you're hunting for something:

| Concern | Files |
|---|---|
| Keys into the side tables | [`NodeKey.fs`](NodeKey.fs), [`CstKeys.fs`](CstKeys.fs) |
| Side tables + the context threaded through every pass | [`PassContext.fs`](PassContext.fs) |
| `SemType`, `TypeVar`, `EscapeState`, `RegionRepr` | [`SemanticInfo.fs`](SemanticInfo.fs) |
| Union-find (Algorithm J primitives) | [`UnionFind.fs`](UnionFind.fs) |
| The output TAST + its walkers / converters | [`Tast.fs`](Tast.fs), [`TastWalk.fs`](TastWalk.fs), [`TastConvert.fs`](TastConvert.fs) |
| Type-definition registry (records, unions, classes, enums) | [`TypeRegistry.fs`](TypeRegistry.fs), [`TypeInfos.fs`](TypeInfos.fs) |
| How we learn about symbols we didn't compile | [`ExternalSymbols.fs`](ExternalSymbols.fs), [`ExternalSymbolProviders.fs`](ExternalSymbolProviders.fs) |
| Extracting a package's symbols from its `.fsi` contract files | [`VesperLib.fs`](VesperLib.fs), [`VesperLib/`](VesperLib/) |
| Where `int` / `string` get their identity (contract-sourced, never hardcoded) | [`Intrinsics.fs`](Intrinsics.fs) |
| `inline` expansion | [`Inline.fs`](Inline.fs), [`Passes/InlineExpansion.fs`](Passes/InlineExpansion.fs) |
| `.fsi` ↔ `.fs` conformance for a package | [`Conformance.fs`](Conformance.fs), [`ConformancePass.fs`](ConformancePass.fs) |

## Docs

Start here, in order:

1. [`docs/architecture.md`](docs/architecture.md) — CST in, TAST out, side tables
   between; why there is no working-tree wrapper; why pass order is one-way.
2. [`docs/passes.md`](docs/passes.md) — the pass contracts and where `inline`
   fires.
3. [`docs/nodekey.md`](docs/nodekey.md) — the 64-bit key and its synthetic-node
   discriminator.
4. [`docs/typevar.md`](docs/typevar.md) — the three axes of a `TypeVar` (type,
   units, region) and how SRTP / IWSAM bounds attach.

Then, per subsystem, as you need them: `du-architecture.md`,
`records-architecture.md`, `printf-architecture.md`,
`package-type-extraction-architecture.md`, `core-lib-architecture.md`,
`dynamic-typing-design.md`.

The original spec is [`semantic-analysis.md`](../../semantic-analysis.md) at the
repo root. It states intent; where it and the code disagree, the code won.

Two conventions in `docs/` worth knowing:

- **`*-plan.md` files are ephemeral.** Each scopes one body of work and is
  deleted when that work lands. A plan doc is *not* a record of how the system
  works — if you want to know how something works, read the code, which carries
  the rationale as sited comments. Do not cite a plan doc from code.
- **`brainstorm-*.md` files are thinking, not decisions.** They may describe
  roads not taken.

## Tests

[`test/XParsec.FSharp.SemanticAnalysis.Tests`](../../test/XParsec.FSharp.SemanticAnalysis.Tests) —
one file per concern (`NameResolutionTests`, `GeneralisationTests`,
`RegionsTests`, `ValidationTests`, `ResolvedTypesTests`, …). They go through
`Pipeline.analyseSemWithContext` so they can assert against the side tables, not
just the output tree. The end-to-end behavioural gates for anything that reaches
codegen live in the `Codegen.Clr` / `Codegen.Js` test projects.
