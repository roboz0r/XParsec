# XParsec.FSharp.Lib

A signature-only port of FSharp.Core, organised into buckets and consumed
directly by `XParsec.FSharp.SemanticAnalysis`. This tree is **not** built
by `dotnet`/`fsc`; there is no `.fsproj` and no DLL output. The XParsec.FSharp
parser reads the `.fsi` files listed in each bucket's `manifest.toml` and the
type checker turns them into external symbols.

## Why signature-only / not-a-.NET-project

Three earlier rounds of work tried to publish FSharp.Core as one or more
`.NET` assemblies under `--compiling-fslib`. The constraints made a clean
bucket split impossible — see [[feedback-fsharpcore-one-assembly]] for the
full story. Briefly:

- `--compiling-fslib` is all-or-nothing: it treats the current compilation
  as *the* FSharp.Core CCU and refuses to resolve primitives via
  `ProjectReference`. Multiple `--compiling-fslib` assemblies do not compose.
- Inline IL `(# ... #)` and static-optimisation conditionals
  (`when 'T = int`) are gated on `--compiling-fslib`, and dozens of
  FSharp.Core source files use them. They cannot be split out into a
  "regular" downstream project.
- Dense cross-references between files (e.g. `event.fs` uses
  `Microsoft.FSharp.Reflection`; `observable.fs` uses an extension from
  `async.fs`) make even non-IL files chain together.

The type-checker doesn't actually *need* a DLL — it needs signatures so it
can resolve names and infer types. The `.fsi` files have all the
declarations it cares about, without the IL bodies. XParsec.FSharp's parser
already handles inline IL and static-optimisation syntax in `.fsi` position,
so the type-checker can read these files directly.

If a future target needs a runnable FSharp.Core (e.g. compiling F# to
WebAssembly), that target supplies its own implementation. This source tree
is the type-level contract that any such implementation must satisfy.

## Layout

```
XParsec.FSharp.Lib/
├── manifest.toml              ← upstream pin + bucket dependency graph
├── compiler-clr-project.md    ← this file
├── Clr/                       ← platform primitives
│   ├── manifest.toml
│   ├── prim-types-prelude.{fsi,fs}
│   ├── prim-types.{fsi,fs}
│   └── nativeptr.{fsi,fs}
├── Common/                    ← target-agnostic core
│   ├── manifest.toml
│   ├── Random.{fsi,fs}, local.{fsi,fs}, array2.{fsi,fs},
│   │   option.{fsi,fs}, result.{fsi,fs}, collections.{fsi,fs},
│   │   seqcore.{fsi,fs}, seq.{fsi,fs}, string.{fsi,fs},
│   │   list.{fsi,fs}, array.{fsi,fs}, array3.{fsi,fs},
│   │   map.{fsi,fs}, set.{fsi,fs},
│   │   event.{fsi,fs}, eventmodule.{fsi,fs}, observable.{fsi,fs}, SI.fs
│   └── math/z.{fsi,fs}
├── Threading/                 ← async, tasks, resumable, mailbox
│   ├── manifest.toml
│   ├── resumable.{fsi,fs}, async.{fsi,fs},
│   └── tasks.{fsi,fs}, mailbox.{fsi,fs}
├── Reflection/                ← F#-aware reflection wrappers
│   ├── manifest.toml
│   └── reflect.{fsi,fs}
└── Printf/                    ← printf + sformat
    ├── manifest.toml
    ├── sformat.{fsi,fs}
    └── printf.{fsi,fs}
```

`.fsi` files are the contract; the type-checker only reads them.
`.fs` files are kept alongside as **transliteration reference** for upstream
re-syncs — they're not listed in any manifest and are never parsed by the
semantic analyser. Diff them against `dotnet/fsharp/src/FSharp.Core/<file>`
when upstream moves to catch semantic drift the signatures don't show.

`SI.fs` is the one exception: upstream provides no `.fsi` for it (it's
pure units-of-measure declarations), so the `.fs` is listed in
`Common/manifest.toml`.

## Manifest schema (TOML)

Parsed via the in-tree `XParsec.Toml` project (forked from Fidelity.Toml).
Any TOML parser will work; the schema is deliberately small.

### Root `manifest.toml`

```toml
[upstream]
repository  = "dotnet/fsharp"
commit      = "ff81858ea7eaa4631e1d1ad6aa4f61d7b8967139"
tag         = "v13.9.201-665-gff81858ea"
path        = "D:/roboz0r/fsharp/src/FSharp.Core"

[[bucket]]
name        = "Clr"
path        = "Clr"
description = "..."
depends-on  = []
# ... one [[bucket]] entry per bucket, listing direct deps only.
```

### Per-bucket `<bucket>/manifest.toml`

```toml
files = [
  "prim-types-prelude.fsi",
  "prim-types.fsi",
  ...
]
```

The list is in compile order (`.fsi` before any consumer; otherwise
mirrors `FSharp.Core.fsproj`). The semantic analyser feeds these to
XParsec.FSharp's parser one at a time, in order, accumulating declarations.

## Files we deliberately did not port

- **`fslib-extra-pervasives.{fsi,fs}`** — would define
  `Microsoft.FSharp.Core.ExtraTopLevelOperators` (`sprintf`, `failwithf`,
  `printf`, etc.). Deferred because it references `Microsoft.FSharp.Linq.*`
  and `Microsoft.FSharp.Quotations.*`, both of which are dropped. Port when
  the type-checker actually needs `sprintf` as a resolved symbol.
- **`quotations.{fsi,fs}`** — `<@ @>` modelling. Skipped intentionally.
- **Queries** (`Linq`, `Query`, `QueryExtensions`, `MutableTuple`,
  `Nullable`) — FCS-coupled query support, skipped intentionally.

## Upstream pin and re-sync workflow

The root `manifest.toml` records the SHA we transliterated against:

```toml
commit = "ff81858ea7eaa4631e1d1ad6aa4f61d7b8967139"
tag    = "v13.9.201-665-gff81858ea"
```

To re-sync against a newer upstream:

1. Fetch the new commit in `D:/roboz0r/fsharp`.
2. For each ported file, run `diff` against the corresponding upstream
   `.fsi` (and `.fs`, since the `.fs` files are kept as reference).
3. Apply changes verbatim; keep the per-file header comment up-to-date with
   the new SHA.
4. Update the root `manifest.toml`'s `commit`/`tag` fields.

The transliteration discipline — see [[feedback-fsharpcore-port-transliterate]]
— is what makes this workflow tractable. Don't reorganise or rewrite the
files in this tree.

## How `XParsec.FSharp.SemanticAnalysis` will consume it

Sketch (not yet implemented):

1. Read `XParsec.FSharp.Lib/manifest.toml` via XParsec.Toml (or any TOML
   reader); produce the bucket dependency graph.
2. Topologically sort buckets; for each bucket in order:
   - Read `<bucket>/manifest.toml`.
   - For each listed file, parse it via `XParsec.FSharp` to produce a
     signature-tree.
   - Walk the signature-tree to extract type, module, and val declarations,
     and feed them into an `IExternalSymbolProvider` implementation that
     replaces `MockBuiltins.provider` in `ExternalSymbols.fs`.
3. The provider's `TryLookup name` answers from the accumulated declaration
   table, instantiating fresh `TypeVar`s at the requested level for
   polymorphic symbols.

Files in `Clr/` are processed first, then `Common/`, etc. Within a bucket,
files are processed in the order listed in its manifest. Cross-bucket
references resolve through the declaration table built so far.

## References

- Memories worth re-reading before changing this tree:
  - `feedback-fsharpcore-port-transliterate` — the literal-transliteration discipline.
  - `feedback-fsharpcore-one-assembly` — why the multi-assembly DLL split doesn't work.
  - `project-fsharpcore-clr-needs-prim-types` — the `--compiling-fslib`
    delayed-thunk story (historical, no longer load-bearing since we
    abandoned `--compiling-fslib`).
  - `project-inline-il-target-specific` — per-target `(+)` story.
- Upstream FSharp.Core: `D:\roboz0r\fsharp\src\FSharp.Core\` pinned at
  `ff81858ea7eaa4631e1d1ad6aa4f61d7b8967139`.
- F# compiler internals worth knowing (for understanding what's in the
  `.fsi` files):
  - `Compiler/SyntaxTree/LexFilter.fs` — handling of inline IL tokens.
  - `Compiler/pars.fsy` — grammar for inline IL and static-optimisation
    constraints.
- Related plans:
  - `src/XParsec.FSharp.SemanticAnalysis/docs/architecture.md` — the
    semantic-analysis pipeline.
  - `src/XParsec.FSharp.SemanticAnalysis/ExternalSymbols.fs` — where the
    new manifest-driven `IExternalSymbolProvider` will plug in (currently
    `MockBuiltins.provider`).
