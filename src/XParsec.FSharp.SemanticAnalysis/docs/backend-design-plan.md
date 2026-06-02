# Backend design plan

How the codegen layer slots in after Freeze. Captures the *posture*
the work should take and the load-bearing design decisions that
follow from it. Deliberately under-specified: the analysis side
taught us that contracts crystallise better from a working
implementation than from up-front interface design.

## Posture

Carry the same design principles from the analysis side into codegen:

- **No observable mutation of inputs.** The builder gets `TastFile`
  and `IExternalSymbolProvider`, both immutable from its perspective.
  It can mutate freely internally — assembly writers like
  `System.Reflection.Metadata`'s `MetadataBuilder` are deeply stateful
  and there's no point fighting that — but the mutation stays inside
  the seam, doesn't leak through whatever entry point the backend
  exposes.
- **Side tables over wrapper trees.** When codegen needs to attach
  per-node info during emission (local slot indices, generated method
  refs, captured-variable layouts), keep it in a `Dictionary<NodeKey, _>`
  keyed off the same `NodeKey`s the analysis side used.
  [architecture.md](architecture.md) §"Why not a working tree wrapper"
  applies here for the same reasons.
- **Don't speculate on a second backend.** The CLR backend lands
  first. JS, WASM, native, or "logging compiler that just prints"
  are hypothetical until proven otherwise. The first backend's seams
  become the abstraction surface for the second when the time
  arrives — that's how `IExternalSymbolProvider` factored out on
  the analysis side, and it's the cheapest way to get the right
  abstraction.

## The shape: a pair, not an interface

The minimum useful split is two functions:

```fsharp
// Compilation: TAST + symbol context → in-memory artifact.
val compile :
    symbols: IExternalSymbolProvider ->
    project: ProjectInfo ->
    tast: TastFile ->
    'TArtifact

// Materialisation: in-memory artifact → side effect.
val materialise : 'TArtifact -> unit
```

`'TArtifact` is target-specific. On CLR it's roughly an in-memory
`MetadataBuilder` + `BlobBuilder` pair (or whatever shape ends up
holding the assembled PE before flush). On a hypothetical
interpreter target it's the executable closure that runs in place.
On a JS target it's a string or AST.

The pair structure exists because the two halves have genuinely
different concerns:

- `compile` is pure-ish — same TAST + same provider should produce
  the same artifact, modulo any internal nondeterminism that's
  worth fixing.
- `materialise` is where side effects live — writing PE bytes to
  disk, printing to stdout, invoking the assembled code in process.

Separating them keeps tests honest (compile once, assert against
the artifact in memory, materialise only when end-to-end is
needed) and gives the in-memory and on-disk callers the same code
path up until the last step.

## Shared inputs

The builder sees the same `IExternalSymbolProvider` instance that
inference used. Rationales:

- `TExpr.External` carries the compiled name; resolving the call
  needs the symbol's shape (arity, type, constraints) which is
  exactly what the provider knows.
- One less surface to design. The hypothetical "target-resolution
  arm" sketched in the earlier discussion can wait until a backend
  needs binding information the provider doesn't carry. When it
  arrives, it lands as an additive extension to the provider, not
  a second parallel interface.

`ProjectInfo` covers the per-build configuration the backend needs
(target framework, output path, signing, resources). Extensible
record so targets can read different fields; design it as a record
of `option`-typed slots rather than a closed type.

## Lowering split

The design decision the *posture* doesn't decide for you: which
TAST-level lowerings run before the backend sees TAST, and which
live inside the backend.

**Universal canonicalisations** — run as TAST→TAST passes in this
project, after Freeze, before any backend runs:

- Closure / eta lowering to `Fun`-constraint form
  ([function-representation-plan](function-representation-plan.md)).
- Format-spec resolution into `PrintfFormat<_, _, _, _>` shapes
  ([front-end-gaps-plan](front-end-gaps-plan.md) §B).
- List literal → ctor calls ([front-end-gaps-plan](front-end-gaps-plan.md) §A).
- Fully resolved SemType at every node ([front-end-gaps-plan](front-end-gaps-plan.md) §D).

**Target-specific lowerings** — live inside the backend:

- DU representation (CLR closed-hierarchy classes; JS tagged
  payload objects; native tagged unions).
- Tuple representation (`ValueTuple` vs `Tuple` vs array).
- Exception representation.
- Generic instantiation strategy (CLR generics vs JS
  monomorphisation).
- Entry point shape (`[<EntryPoint>]` member vs `module.exports`
  vs WASM `_start`).
- FSharp.Core resolution (reference real DLL vs ship lib's `.fs`
  impls).

The line shifts as backends materialise. Keep the universal list
conservative — pushing a lowering into the backend later is
non-breaking, pulling one out and discovering it encoded a CLR-ism
is fatal. Default to "target-specific" when uncertain.

## CLR backend internals

Two layered concerns inside the CLR backend:

1. **The IL-emission DSL.** `System.Reflection.Metadata` is the
   chosen writer. The raw API is C#-shaped and offers no
   compile-time guarantees about correctness — stack-balance,
   token-validity, control-flow well-formedness are all runtime
   failures. The F#-idiomatic wrapper is a computation expression
   tracking those invariants at the type level, in the shape of
   [LicenseToCIL](https://github.com/rspeele/LicenseToCIL) — phantom
   type parameters for stack state, builder operations carrying
   pre-/post-stack types, and CE syntax for sequencing.

   This is a worthwhile abstraction *for the CLR backend
   specifically* because IL has unusually precise correctness
   constraints that a type system can capture cheaply. Don't
   generalise it to "all backends use a CE wrapper" — JS or WASM
   targets have different correctness models.

2. **The TAST walker.** Recursively translates `TDecl` / `TExpr`
   into emission-CE operations. Owns the target-specific lowerings
   listed above. Walks decls in source order; per-decl emits
   either a static member, a field initialiser, or a contribution
   to the entry-point body.

The walker writes against the CE's typed API, so structural
errors (unbalanced stack from a forgotten store, wrong-typed
argument, malformed control flow) are caught at compile time
rather than at IL verification.

## What this plan deliberately doesn't decide

- **`IArtifactBuilder<'TArtifact>` interface.** Earlier discussion
  sketched one; rejected as premature. The `compile` / `materialise`
  pair is a function signature, not a contract. If/when a second
  backend lands and shares enough structure with the first to
  warrant an interface, factor it out then.
- **High-level IR between TAST and emission.** Also rejected as
  premature. TAST is the contract until two backends are sharing
  enough lowering work that the duplication becomes obvious.
- **Per-decl vs per-node vs per-instruction granularity of the
  hypothetical interface.** N/A — no interface yet.
- **Target-resolution sub-provider.** Defer until the first
  backend hits the wall the symbol provider can't answer for it.
- **Multi-target build orchestration.** A single backend builds a
  single target. Multi-target builds run the same `compile` against
  different `ProjectInfo`s; the orchestration layer is above this
  plan.

## Order of work

Once the front-end gaps close ([front-end-gaps-plan](front-end-gaps-plan.md)):

1. **Universal canonicalisation passes.** TAST→TAST. Closure
   lowering, format-spec resolution, list desugaring, resolved-type
   validation. Each is a small pass with its own tests against the
   existing semantic-analysis corpus.
2. **CE wrapper around `System.Reflection.Metadata`.** Phantom-typed
   IL emission DSL. Standalone — no TAST dependency. Tested by
   hand-writing small assemblies (`add(int, int) : int`) and
   asserting on the emitted bytes plus runtime behaviour.
3. **TAST walker → CE.** The `compile` function proper. Built up
   per a thin-slice progression: `printfn "hi"` → arithmetic →
   `inline` → list literal → full sample.
4. **`materialise` to disk.** Writes the in-memory artifact to a
   PE file. Trivial once the in-memory shape is settled.

(1) and (2) are independent and can run in parallel. (3) needs both.
(4) is bookkeeping after (3).

## Cross-references

- [function-representation-plan](function-representation-plan.md) —
  the closure-lowering canonicalisation that runs before the
  backend sees TAST.
- [front-end-gaps-plan](front-end-gaps-plan.md) — the prerequisite
  TAST-cleaning work.
- [architecture.md](architecture.md) — the design principles this
  plan inherits (no observable mutation, side tables, freeze-only-
  once).
- [extract-symbols-plan](extract-symbols-plan.md) — the symbol
  provider the backend reuses.
