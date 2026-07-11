# Architecture

This document captures the design decisions made *after* the original
`semantic-analysis.md` brainstorm — the ones that shape the project's physical
layout and pass contracts.

## The governing principle: immutable → mutable → immutable

Most of what follows is a consequence of one shape:

> **immutable shared CST → mutable in-flight TAST → immutable shared frozen TAST**

Mutation is not forbidden; it is **confined**. The two ends are immutable because
they are *shared* — with tooling, with other compilations, with the backends,
potentially across threads. The middle is mutable because that is where mutation
pays for itself (in-place union-find, dictionary side tables) and because nothing
there escapes: it is scoped to one `PassContext`, which is scoped to one
compilation.

| Stage | Representation | Mutable? | Who may hold it |
|---|---|---|---|
| **In** | CST + trivia | no | the parser's other consumers — formatter, linter, IDE — and every compilation at once |
| **Middle** | side tables + the `SemType` / `TypeVar` union-find graph, and the `TastFileG<SemType>` tree built over it | **yes** | exactly one `PassContext`. Nothing else, ever |
| **Out** | `TastFileG<FrozenType>` | no | codegen, caches, other assemblies |

Read that way, three decisions stop being independent choices and become the same
decision:

- **The CST is not mutated** — it is in the *shared* column. Attaching mutable
  semantic state to it would mean every other consumer has to know when that
  state is valid. So semantic facts go in side tables instead (§below).
- **Side tables and `TypeVar` may be mutated freely** — they are in the *confined*
  column. This is why we don't reach for persistent maps or path-copying: nothing
  outside the pipeline can observe the mutation.
- **`Freeze` exists at all** — it is the gate back out to the shared column. An
  elaborated tree still points into a live, mutating `TypeVar` graph (`Regions`
  writes `TypeVar.Region` *after* `Elaborate` has run), so it is not safe to hand
  to a backend or cache across a compilation boundary. `Freeze` severs that link
  by rebuilding into `FrozenType`, in which a metavar is unrepresentable **by
  construction**.

That last point is why `FrozenType` has no `TyVar` case, and why the
`ResolvedTypes` / `PlatformTypes` guards run immediately before the freeze rather
than after: they are the last chance to turn "this didn't resolve" into a decent
diagnostic while the mutable graph is still around to explain itself.

A caveat when reading the source: `Elaborate.fs` describes its output as
"sharable". That means *independent of the CST and side tables* — narrower than it
sounds. The elaborated tree is not yet shareable in the sense this section uses,
because it still carries `SemType`.

## CST in, TAST out, side tables in between

The `XParsec.FSharp` parser produces an **immutable** CST whose nodes preserve
source tokens (`LongIdent.Dots`, exact whitespace via attached trivia, etc.).
That fidelity is load-bearing for tooling (formatter, linter, syntax
highlighting) and we don't want to lose it.

So the rules are:

- The CST is **not mutated** by this project. No mutable fields are added to
  CST records. No "promise to only touch this during analysis" hatch.
- All in-flight semantic information lives in **side tables** keyed by
  [`NodeKey`](nodekey.md). One table per kind of information (resolved
  binding, `TypeVar`, `EscapeState`, …). They live on the `PassContext` — see
  [`PassContext.fs`](../PassContext.fs).
- [`Elaborate.fs`](../Elaborate.fs) projects the CST + side tables into a
  brand-new immutable [`Tast`](../Tast.fs) tree. It is the **one** CST → TAST
  projection, and the side tables are discardable once it returns.

This is the same shape Roslyn and FCS use: keep the syntax tree pure, attach
semantic info alongside, and produce a separate bound/typed tree only when
needed.

> **Don't confuse the two.** [`Freeze.fs`](../Freeze.fs) is a *different, later*
> step than `Elaborate`: the single `SemType → FrozenType` rebuild that ends the
> pipeline. **`Elaborate` builds the tree; `Freeze` changes the type domain.** So
> the CST → TAST projection is not the only tree-to-tree pass any more —
> `RefCellPromotion` rewrites the TAST, and `Freeze` maps it across type domains.
> It remains true that no pass rewrites the *CST*.
>
> `Elaborate` was itself once *called* `Freeze` (hence the historical confusion).
> A stray `Freeze` in an old commit that plainly means "builds the TAST" is that.

## Why not a "working tree" wrapper?

An alternative we considered: build a parallel `WorkingExpr { Cst; mutable
Desugared; mutable TypeVar; ... }` tree eagerly, mutate in place, freeze at the
end. Discarded because:

- It allocates a wrapper per CST node whether or not that node carries
  semantic info. Many CST nodes (literals, trivia-heavy wrappers) carry
  nothing.
- It muddies the "immutable at rest" claim — the wrapper *is* the working
  tree, and downstream code has to know which fields are safe to read when.
- Side tables compose better: adding a new pass means adding a new
  `Dictionary<NodeKey, NewInfo>`, not extending a wrapper record everywhere.

The cost is a hash lookup per semantic query. With the 64-bit `NodeKey` packed
into a value the JIT can hold in a register, that's noise.

## Annotation-only desugaring

Desugaring (`Passes/Desugar.fs`) **does not rewrite the CST shape**. When it
sees `x |> f`, it does *not* replace the `Pipeline` CST node with an
`Application` node. Instead it writes a `DesugaredForm` entry into a side
table, keyed by the `Pipeline` node's `NodeKey`. The constraint generator and
later passes read through that side table.

Consequences:

- The CST view stays queryable at every point during analysis (hover semantics
  on a real-source span work whether or not desugaring has happened yet).
- Pass contracts are clean: "this pass reads slot X, writes slot Y". No pass
  destroys information.
- Synthetic nodes (CE method calls, comprehension expansions, the body of an
  expanded `for-in-do`) have no source position, so they get **synthetic
  NodeKeys** — see [nodekey.md](nodekey.md).

## Pass order is strictly forward

`Pipeline.fs` is the authoritative order and [`passes.md`](passes.md#pipeline)
tabulates it — deliberately not duplicated here, because a second copy is a
second thing to rot. In shape: annotate (`Desugar`) → resolve
(`NameResolution`) → infer (`Unification`) → check (`Validation`) → build the
tree (`Elaborate`) → analyse and rewrite the tree (`Regions`,
`RefCellPromotion`) → guard (`ResolvedTypes`, `PlatformTypes`, `DynamicEscape`)
→ change type domain (`Freeze`).

Two facts about that order are load-bearing rather than incidental:

- **`Regions` runs *after* `Elaborate`, not before.** Escape analysis has to see
  the closures codegen will actually emit, and inlining both destroys closures
  and creates them. Running it on the CST would analyse a tree that no longer
  exists by the time anything is emitted.
- **The guards run before `Freeze`, in the `SemType` domain.** A leaked metavar
  is a graceful per-decl diagnostic from `ResolvedTypes`; if it reached `Freeze`
  it would be a hard error, because `FrozenType` cannot represent one.

Each pass's contract:

- **Precondition:** the side tables written by previous passes are populated.
- **Postcondition:** this pass's side table is populated.

No pass re-runs an earlier pass. The only "feedback" is *inside* `Unification`:
when a `TypeVar` is solved, deferred SRTP/IWSAM checks fire and may unify
further variables. That iteration is contained.

### Where `inline` lives

F#'s `inline` is semantically observable (it lets SRTPs dispatch against the
caller's concrete types), so it has to be handled inside `Unification`, not in
a downstream optimisation pass. We deliberately carve `inline` out as the
only "optimisation" that has to live in semantic analysis. Everything else
(CSE, closure conversion, lambda lifting, dead code elim) consumes the frozen
TAST and is out of scope here.

## Parallelism

**Design intent, not built.** The orchestration described here does not exist
yet; the multi-file story is still being planned. What follows is the shape the
design keeps open, and the reason nothing about `PassContext` forecloses it.

The side-table-per-`PassContext` design is what enables file-granularity
parallelism. Each file's analysis owns its own `PassContext` — its own
`TypeVar` pool, its own `SideTable`s, its own `Diagnostics` channel. None
of that state is shared with other files' pipelines, so spinning up N
`PassContext`s on N threads is safe by construction. The mutability
inside a single `PassContext` is load-bearing for performance (in-place
union-find, dictionary updates) and isn't worth trading away for
within-pass parallelism that Algorithm J wouldn't benefit from anyway.

The only object that crosses thread boundaries is
`IExternalSymbolProvider`, which is read-only by contract — see the doc
comment on the interface in `ExternalSymbols.fs`. Implementations that
cache lazily (a real `FSharp.Core.dll` reader, or a wrapper exposing
another file's post-analysis schemes) must guard their own mutation.

**Cross-file ordering** is a separate concern from threading. A file
that references names defined elsewhere has a true dependency, and its
analysis can't start until the upstream file's exported schemes are
queryable. The orchestration shape mirrors recent FCS: each file's
pipeline finishes by producing a signature (exported names + schemes),
which is then wrapped behind `IExternalSymbolProvider` for downstream
files. Files within the same topological level of the dependency graph
analyse in parallel; files across levels chain sequentially.

What's deliberately *not* parallelised:

- **Within a pass.** Algorithm J unifies in place against a shared
  union-find graph; persistent alternatives (path-copying, persistent
  maps) typically cost 5–10× on HM inference. No production type checker
  does in-pass parallelism for this reason.
- **Across passes within one file.** Each pass reads what its
  predecessors wrote — the dependency is the whole point of the side-
  table contract in [`docs/passes.md`](passes.md).

## Lowering split: universal vs target-specific

Everything downstream of Freeze is a lowering, and each one has to sit on
one side of a line: does it run once, here, for every backend — or once
per backend?

**Universal** lowerings traffic only in `Frozen.TExpr` / `FrozenType` /
`NodeKey` and live in this project, because the codegen projects must not
reference each other. `TastLower.fs` is the shared home; `InlineExpansion`
and `RefCellPromotion` are pre-freeze siblings that lower on the still
`TyVar`-carrying tree, where `zonk` and union-find are native.

**Target-specific** lowerings live in the backend: DU and tuple
representation, exception representation, generic instantiation strategy
(CLR generics vs JS monomorphisation), entry-point shape, and FSharp.Core
resolution. Overload *name mangling* is target-specific for the same
reason — JS has no overloading, so it needs a signature-derived name the
CLR gets free from metadata.

The line is not symmetric, and that asymmetry is the rule to decide by:

> Moving a lowering from universal into a backend is non-breaking. Moving
> one out of a backend, after discovering it silently encoded that
> backend's assumptions, is not — every other backend has by then been
> written against the leak. **When uncertain, make it target-specific.**

Keep the universal list conservative for that reason, not because
duplication across backends is cheap. Duplication is recoverable;
a CLR-ism baked into `TastLower` and inherited by JS is not. The
`compile` / `materialise` pair each backend exposes is a function
signature, not a shared interface — there is deliberately no
`IArtifactBuilder`, and no high-level IR between TAST and emission,
until two backends duplicate enough lowering work to make the shape
obvious rather than guessed.

## What's out of scope

- **Target-specific lowering.** Consumes the frozen TAST and lives in the
  backend projects (`XParsec.FSharp.Codegen.Clr`,
  `XParsec.FSharp.Codegen.Js`), not here. See §Lowering split above for
  which side of the line a given lowering belongs on.
- **Incremental recompilation.** The `NodeKey` design supports it (a CST edit
  invalidates a known range of keys), but no incremental machinery is built.
- **Generic constraint solver.** The `TypeVar` and union-find code here is
  F#-specific. If a second language ever wants the same solver, we'll
  extract a `src/XParsec.SemanticAnalysis` core then. Not before.
