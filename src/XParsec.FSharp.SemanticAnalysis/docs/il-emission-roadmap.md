# IL emission roadmap

Forward-looking sketch of the path from today's `TastFile` to emitting
a runnable .NET assembly for the canonical sample:

```fsharp
let inline sum xs = List.fold (+) 0 xs
let nums = [1; 2; 3; 4; 5]
printfn "%d" (sum nums)
```

This is a roadmap, not a phased plan with status columns — the individual
plans it points at carry the per-phase detail. The point of this doc is
to make the *shape* of "what's left between us and IL" legible so the
sub-plans can be sequenced against each other.

## Starting point

`Pipeline.fs` runs Desugar → NameResolution → Unification → Regions →
Validation → Freeze and produces a `TastFile`. The lib-backed
`IExternalSymbolProvider` ([extract-symbols-plan](extract-symbols-plan.md))
resolves operators and combinators end-to-end through Phase 5b.2 —
`1 + 2 : int` and SRTP member dispatch both type-check through a
lib-only provider. There is currently no codegen layer.

## What the sample demands that we don't have yet

| Need | Owner |
|---|---|
| `[1; 2; 3; 4; 5]` lowered to nested `FSharpList.Cons` / `Nil` ctors. | [front-end-gaps-plan](front-end-gaps-plan.md) §A |
| `(+)` passed as a value to `List.fold` — function-representation choice and closure synthesis. | [function-representation-plan](function-representation-plan.md) |
| `let inline sum` body retained for per-call-site expansion. | [front-end-gaps-plan](front-end-gaps-plan.md) §C |
| `printfn "%d"` — `PrintfFormat<_,_,_,_>` typing from the literal's format spec. | [front-end-gaps-plan](front-end-gaps-plan.md) §B |
| Fully resolved `SemType` at every TAST node (no dangling `TypeVar`). | [front-end-gaps-plan](front-end-gaps-plan.md) §D |
| Top-level `printfn …` as an entry point with the right cctor / `Main` semantics. | this doc, §Entry-point lowering |

## Three layers

### 1. Front-end gap closure

In-repo, in this project. Closes the gaps above before IL emission can
sensibly start. Locked-in order is `A → D → C → B` — see
[front-end-gaps-plan](front-end-gaps-plan.md).

This is the only layer that needs to land before a thin-slice IL
backend can prove it works.

### 2. Backend scaffolding (new sibling project)

Once the TAST is codegen-clean for the sample, scaffolding lives in a
new sibling project (working name: `XParsec.FSharp.Codegen.Clr`):

- **Target codegen provider interface.** Symmetrical to
  `IExternalSymbolProvider` — for each compiled name the symbol
  provider knows about, the codegen provider knows how to emit a
  call to it. `op_Addition` → CIL `add`; `Microsoft.FSharp.Collections.ListModule.Fold`
  → `call FSharpList.Fold`; `Microsoft.FSharp.Core.Printf.printfn` →
  the BCL `Printf` entry point. Same per-target pluggability as the
  symbol side ([fsi-target-brainstorm](fsi-target-brainstorm.md)).
- **Assembly writer.** Pick `System.Reflection.Metadata` — deterministic
  output, no `S.R.Emit` dependency, lines up with how modern .NET
  toolchains write PE files. Mono.Cecil is the friendlier alternative
  if mutation is preferred over construction.
- **FSharp.Core resolution.** The lib's `.fsi` files are signatures
  only. v1 references a real `FSharp.Core.dll` at runtime, matching
  the existing `.NET` provider stack convention
  ([[project_dotnet_provider_stack]]). Long-term, the curated
  per-target `.fs` impl files in the lib ship their own implementations
  ([[project_fsharpcore_clr_needs_prim_types]]).
- **Closure / eta lowering.** Synthesises the function-value
  representation per the chosen scheme — see
  [function-representation-plan](function-representation-plan.md). v1
  emits `FSharpFunc<_,_>` subclasses; the `Fun`-constraint-driven
  struct-closure path is a follow-up.

### 3. .NET codegen pass

The `TastFile → assembly` work proper:

- **Top-level module shape.** Each module-level `let` becomes a static
  member on a `<ModuleName>` class. Top-level expressions go into a
  synthetic `[<EntryPoint>]` method (or `<StartupCode>$Main` for
  ordering-sensitive side effects) — see Entry-point lowering below.
- **TExpr → IL** node-by-node. Literals to `ldc.*`, `App` to call
  sequences, `Let` to local-slot store/load, `IfThenElse` to branch
  pairs, `Match` to a decision-tree compilation pass that produces
  switch / branch IL.
- **Generic methods.** Typars in scope at a `TDecl.Let` become
  `GenericParameter` entries on the emitted method. SRTP-resolved
  external symbols come through the codegen provider as already-resolved
  member refs — no SRTP dispatch left at IL time.
- **External-symbol calls.** Every `TExpr.External compiledName` goes
  through the codegen provider's `EmitCall` hook. The compiled-name
  table built in [extract-symbols-plan](extract-symbols-plan.md) keys
  the dispatch.

## Entry-point lowering

`TDecl.Expression` items appear in source order in the TAST. For the
sample, `printfn "%d" (sum nums)` is the third decl. The CLR has two
options:

1. Synthesise `[<EntryPoint>] static member Main(argv) : int = …; 0`
   containing the lowered top-level expressions in declaration order,
   with `let` decls preceding it as static-member assignments. This
   mirrors what F# does for top-level scripts.
2. Use the module class's static constructor (`.cctor`) to evaluate the
   side-effects. Simpler but harder to attach an exit code to.

(1) is the canonical F# shape. v1 picks (1); decls without effects
(`let nums = …`) initialise fields, decls with effects (`printfn …`)
emit calls in body order inside `Main`.

## Thin-slice ordering

Once the front-end gaps close, the recommended thin-slice progression
through the backend:

1. **`printfn "hi"` only.** No list, no fold, no inline. Validates
   assembly writer, FSharp.Core resolution, entry point, and the
   simplest external-symbol call. No closure machinery yet.
2. **`let x = 1 + 2 in printfn "%d" x`.** Adds primitive arithmetic
   (CIL `add`), local bindings, and the `%d` format spec end-to-end.
   Still no function values.
3. **`let inline succ x = x + 1 in printfn "%d" (succ 41)`.** Adds
   `inline` expansion at the call site. No closure, no first-class
   function value yet.
4. **`[1; 2; 3]` literal pretty-printed.** Adds list-literal
   desugaring → IL all the way through.
5. **The full sample.** Adds `List.fold` (cross-assembly generic
   call), `(+)` as a value (closure synthesis), and the polymorphic
   inline body. By this point every other piece exists; the only
   new thing is the closure path.

Each slice is independently testable end-to-end (run the emitted
assembly, check exit code / captured stdout) and bounded enough to
diagnose regressions.

## Out of scope

Anything not in the canonical sample: classes / inheritance
([inheritance-plan](inheritance-plan.md)), records and DUs beyond
what the lib uses internally, async, computation expressions, exception
handling beyond what `try`/`with` already types as, units of measure,
mutable refs / cells, debug info / PDBs, optimisations beyond what
`inline` + JIT devirt give for free. Each is its own follow-up after
the thin slice runs.

## Cross-references

- [codegen-clr-plan](codegen-clr-plan.md) — the concrete project plan for
  the first backend (`XParsec.FSharp.Codegen.Clr`): internal structure,
  the two emission layers, and the thin-slice-1 component checklist.
- [backend-design-plan](backend-design-plan.md) — the backend's posture
  and the universal-vs-target lowering split.
- [front-end-gaps-plan](front-end-gaps-plan.md) — the in-scope work
  for this project (TAST-level).
- [function-representation-plan](function-representation-plan.md) —
  how `'A -> 'B` lowers; affects every call site.
- [fsi-target-brainstorm](fsi-target-brainstorm.md) — the per-target
  lib split this leans on for FSharp.Core resolution.
- [extract-symbols-plan](extract-symbols-plan.md) — the symbol-side
  surface that the codegen provider mirrors.
- [passes.md](passes.md) — current pass pipeline; codegen attaches
  after Freeze.
