# XParsec Developer Guide

## Project Overview

**XParsec** is a parser combinator library for .NET. The core `Parser` type is a function `Reader -> ParseResult`.
**XParsec.FSharp** (`src/XParsec.FSharp/`) is an F# language parser/lexer built on XParsec. It lexes source into tokens, processes compiler directives, applies offside (indentation) rules, and parses the stream into an AST.

```text
Source ──> [Lexing] ──> [Preprocessing (#if)] ──> [Parsing (AST)] ──> [Debug Output]
```

The **xparsec-dev** skill is provided to ease everyday development.

## Architecture & Compilation Order

The parser forms a strict **Directed Acyclic Graph (DAG)**. There are no recursive namespaces (`namespace rec`). Files in `XParsec.FSharp.fsproj` MUST remain in exact compilation order to satisfy F#'s strict top-to-bottom type resolution. The architectural layers are strictly:

1. **Foundation & Lexing:** Utilities, Tokens, Lexer (`Lexing.fs`)
2. **AST Data Types:** `Expr.fs`, `Declarations.fs`, etc.
3. **Parsing State:** `ParsingTypes.fs`, `Preprocessing.fs`, `ParsingHelpers.fs`
4. **Bottom-up Combinators:** Leaf parsers -> Expressions -> Type Definitions -> Declarations

## Key Patterns & Conventions

### Breaking Cycles with `ParserRefs.fs`

F# is mutually recursive, but our files are a strict DAG. We use the **RefParser** pattern to break dependencies:

1. `ParserRefs.fs` creates empty references early: `let refExpr = RefParser<Expr<...>>()`
2. Lower-level parsers use `refExpr.Parser` before the actual expression parser exists.
3. Higher-level parsers implement the logic and bind it at the bottom of their file: `do refExpr.Set Expr.parse`

### ParsingHelpers & Token Consumption

Because fetching the next token is computationally expensive (due to trivia skipping and `#if` preprocessing), use the patterns in `ParsingHelpers.fs`:

- **Peek and Consume:** Use `peekNextSyntaxToken` to evaluate the upcoming token. If matching, use `consumePeeked` to advance the reader.
- **Fast Dispatch:** Use `dispatchNextSyntaxTokenL` instead of large `choice` combinators for O(1) branching on the next token.
- **Virtual Tokens:** F# offside rules allow omitted delimiters. Use `nextSyntaxTokenVirtualIfNot` (synthesizes missing structural tokens like `in`) or `nextSyntaxTokenVirtualWithDiagnostic` (synthesizes missing required tokens like `)` while emitting an error).
- **Context Management:** Use `withContext` to establish new indentation boundaries. It manages the `OffsideContext` stack safely.
- **Error Recovery:** Wrap fallible constructs in `recoverWith`. On failure, it backtracks, emits a diagnostic, and skips tokens until a safe synchronization point (e.g., `;;` or `end`).

### Prefer Statically-Allocated Parser Values

If a parser does not close over local variables, define it as a **value** rather than a function to allocate the closure once at startup.
**Prefer:** `let peekSyntaxIndent = lookAhead (fun r -> ...)`
**Avoid:** `let peekSyntaxIndent reader = lookAhead (fun r -> ...) reader`

### Operator Precedence Parsing (Pratt Parsing)

Used for expressions, patterns, measures, and types. `PrecedenceLevel` converts to `BindingPower`. Handled via the `Operators` interface.

Pratt's `parseRhsInternal` / `parseLhsInternal` (`src/XParsec/OperatorParsing.fs`) already save `reader.Position` before calling `ops.RhsParser` / `ops.LhsParser` and restore on failure. **RHS/LHS parsers passed to the Pratt engine should NOT be wrapped in `choiceL` / `attempt` purely for backtracking** — the wrapper is dead weight that allocates a closure, saves the position a second time, and iterates an array for no benefit. Removing one such wrapper around the expression-RHS dispatcher saved 3% (Medium) / 8% (Large) wall-time on its own.

### Automatic Backtracking

`XParsec` performs automatic backtracking on `choice` and `choiceL` after failed alternatives. There is typically no need for an `attempt` combinator.

### Token Enum Value Sharing

`Token` (`src/XParsec.FSharp/Token.fs`) packs `Kind ||| Precedence` (or `Kind ||| KW.X`). **Same-precedence operators share a single numeric enum value**, so F# pattern-matching on one case catches the whole family. Notable: all 15 operators at `KindOperator ||| Precedence.ComparisonAndBitwise` (`<`, `>`, `<=`, `>=`, `<>`, `&&&`, `|||`, `^^^`, `<<<`, `>>>`, `<|`, `<||`, `<|||`, `>>`, `<<`, `!=`) collapse to the same value — F# treats a second pattern in the same match as redundant/unreachable. Pattern-matching against operators is therefore "family-level"; disambiguate specific operators via `ParseState.tokenStringIs` or `Lexed.GetTokenSpan`. `KWLParen`, `KWLBracket`, `OpSemicolon`, `OpComma`, `OpBar`, `OpArrowRight` use `KindKeyword ||| KW.X` with distinct KW codes and ARE individually distinguishable.

### Peek-and-Dispatch Over `choiceL` Trial Loops

When a `choiceL` alternative list can be narrowed by the next peeked token, replacing it with a single-peek + token-switch dispatcher is a major CPU win. Each of the old 6-way expression-RHS `choiceL` branches called `peekNextSyntaxToken` internally, so a single RHS operator parse was peeking 3-6 times over the same token. The specialist dispatcher peeks once and routes to at most 2-3 sub-parsers — saving wall-time AND allocations (each `peekNextSyntaxToken` constructs a `SyntaxToken` record).

Template: `dispatchNextSyntaxTokenL` (`ParsingHelpers.fs:~888`) for pure token-value dispatch. For state-dependent routing, hand-roll a `FSReader -> ParseResult` following the same shape: peek once, cache `reader.State` into a local, match on `token.Token`, fall through sub-parsers in priority order. Cross-reference each arm against `Token.canStartExpression` (`Token.fs:~1891`) to decide whether `pSepVirt` belongs in the fallback chain — `(` and `[` ARE expression-starters and need it for same-indent sequence separation, even though they're primarily adjacency-dispatched.

## ParseState & Indentation (Offside Rule)

`ParseState` tracks crucial context:

- `ConditionalCompilationStack` / `DefinedSymbols`: For nested `#if` evaluation.
- `Context`: A stack of `OffsideContext` values tracking the current indentation context (e.g., inside a `Let`, `Match`, or `Type`).
- `IndentationMode`: `Syntax.Light` or `Syntax.Verbose`.

## Testing

- **Framework**: [Expecto](https://github.com/haf/expecto) (`test/XParsec.FSharp.Tests/`)
- **Golden File Testing**: Test data lives in `data/`. Source `.fs` files are lexed/parsed and asserted against `.fs.lexed` and `.fs.parsed` golden output files.
- **Trace Debugging**: Test "Trace Debugging Test" in `test/XParsec.FSharp.Tests/ParserTests.fs` should be set to `ftest` and used to understand challenging parsing issues using the `.fs.trace` and `.fs.stack` output.

## Benchmarking

[BenchmarkDotNet](https://benchmarkdotnet.org/) benchmarks live in `bench/XParsec.FSharp.Benchmarks/`. Each benchmark class compares XParsec against `FSharp.Compiler.Service` (FCS) on `fixtures/{small,medium,large}.fs` and reports mean time and managed allocations via `[<MemoryDiagnoser>]`.

**Benchmark classes:**

- `LexingBenchmarks` — tokenization only (`Lexing.lexString`)
- `ParsingBenchmarks` — parse from already-lexed input (isolates the parser layer)
- `EndToEndBenchmarks` — source-to-AST (full pipeline)

**Running:**

```bash
./claude_tools.cmd -Action Benchmark -Filter '*Lexing*'
```

The `-Filter` argument is **required** and accepts BDN's wildcard syntax (e.g. `*Lexing*`, `*Parsing*`, `*EndToEnd*`, or `*` for all). The script runs `dotnet run -c Release -- -i -j short --filter <pattern>` under the hood — `-j short` keeps iteration count low so a pass takes ~45 s per class. Full BDN output is streamed to stdout and also saved to `claude_tools_output.log`; a GitHub-flavoured markdown summary lands in `BenchmarkDotNet.Artifacts/results/`.

**Reading the results:** focus on the `Allocated` column (managed bytes per op) and `Alloc Ratio` (vs. FCS baseline). Mean-time columns also appear but BDN's short-run confidence intervals are wide — allocation numbers are far more stable than mean times. When changing hot paths, always run the relevant benchmark before and after and compare both.

**In-process toolchain:** `BenchConfig.fs` forces `InProcessEmitToolchain` because the default out-of-process runner generates a fresh csproj that inherits this repo's CPM pin (`FSharp.Core 8.0.300`) and trips `NU1109` against FCS's `10.1.202` transitive requirement. The benchmark fsproj pins `FSharp.Core` via `VersionOverride` for the same reason.

## Performance Analysis & Tracing

Three complementary signals feed performance work:

1. **BDN `[<MemoryDiagnoser>]` summary** — the `Allocated` / `Gen0` / `Gen1` columns are the headline numbers for regressions; tight and stable across runs.
2. **Allocation attribution** — per-frame bytes via `dotnet-trace gc-verbose` + `AllocAggregate` (details below).
3. **CPU attribution** — per-frame samples via BDN `--profiler ETW` + `AllocAggregate.aggregateCpu` (details below).

The `bench/XParsec.FSharp.Benchmarks` exe bundles a CLI that wraps both pipelines and re-analyzes existing traces in-process via the `AllocAggregate` module (`bench/XParsec.FSharp.Benchmarks/AllocAggregate.fs`). `Microsoft.Diagnostics.Tracing.TraceEvent` is pinned in `Directory.Packages.props` under the `IsBenchmarkProject` item group.

### Allocation Tracing (dotnet-trace)

`dotnet-trace` is installed as a local tool in `.config/dotnet-tools.json` and captures `GCAllocationTick` events with full call stacks (no elevation needed).

**Running (full pipeline — collect + analyze):**

```bash
dotnet run --project bench/XParsec.FSharp.Benchmarks/ -c Release -- --trace medium 300
```

Launches `dotnet-trace collect --profile gc-verbose --format Speedscope` wrapping a `--trace-child` invocation of the same assembly, then calls `AllocAggregate.aggregate` on the resulting `.nettrace`. Output lands in `./tmp/parse-alloc-<size>.nettrace` (and a `.speedscope.json` alongside); override the directory with the `TRACE_OUT` env var.

### CPU Profiling (BDN ETW profiler)

BDN's `EtwProfiler` captures CPU sampling with stacks (unelevated) and GC events (allocation stacks require admin for the kernel session). Unelevated runs still yield the CPU profile, which is what attributes *wall-time* to frames.

**Running:**

```powershell
pwsh ./claude_tools.ps1 -Action Benchmark -Filter '*Parsing*' -Profiler ETW
```

The script drops `-i` (in-process is incompatible with the out-of-process profiler) and adds `--profiler ETW`. Requires `BenchmarkDotNet` 0.15.x+ (`Directory.Packages.props`); `net10.0` runtime and matching FSharp.Core pin (see Benchmarking §). Artifacts land in `BenchmarkDotNet.Artifacts/` as `<name>-<Size>-timestamp.etl`.

### Re-analyzing traces

All CLI verbs work against both `.nettrace` and `.etl` (the aggregator auto-detects the format via extension):

```bash
# Full allocation self/total tables, filtered to frames containing the substring ("XParsec" default, "*" for all)
dotnet bench/XParsec.FSharp.Benchmarks/bin/Release/net10.0/XParsec.FSharp.Benchmarks.dll --aggregate tmp/parse-alloc-medium.nettrace
dotnet <bench.dll> --aggregate tmp/parse-alloc-medium.nettrace "*"

# Full CPU self/total tables from an ETW .etl or dotnet-trace .nettrace (sampler enabled)
dotnet <bench.dll> --aggregate-cpu BenchmarkDotNet.Artifacts/Parsing...ParsingBenchmarks.XParsec\(Size_\ 1\)-*.etl

# Top-N immediate callers of any frame whose name matches the substring — useful for pinpointing
# which source location is constructing a particular compiler-generated closure (e.g. `clo@1023-6`).
dotnet <bench.dll> --aggregate-callers tmp/parse-alloc-medium.nettrace "clo@1023"

# What types are allocated at a matching leaf (e.g. reveals "100% Message" — a hot fail path)
dotnet <bench.dll> --aggregate-types tmp/parse-alloc-medium.nettrace "pTypeApplication"

# First N full call stacks reaching a leaf — for disambiguating compiler-generated frames
dotnet <bench.dll> --dump-stacks tmp/parse-alloc-medium.nettrace "clo@1023" 3
```

### Reading the tables

- `SELF` = leaf-frame attribution (bytes or CPU samples) — where the work literally happens. Frames named `Foo@<line>` or `Foo+bar@<line>-N` are compiler-generated closures. Use `--aggregate-types` to see what's being allocated there and `--dump-stacks` to see the enclosing scope. CPU SELF samples are approximately 1 ms of on-CPU time each at the BDN default 1 kHz sampling rate.
- `TOTAL` = inclusive attribution across the stack; points at the hot call tree, not the literal alloc/CPU site.
- Default frame filter is `XParsec`; pass `*` to include BCL / JIT frames (useful for spotting `CastHelpers.IsInstanceOfClass` DU-dispatch overhead, JIT-tiering hot spots, etc.).
- For ETW ETL files without admin elevation: `GCAllocationTick` events fire but carry no stacks, so `--aggregate` on the ETL will show `events with stack: 0`. Use `--aggregate-cpu` on those ETLs and re-run `--trace` for allocation attribution.

### Common alloc diagnoses

- `Message[...]` at 100% of a leaf's bytes → `fail (Message "literal") reader` on a hot failure path. Hoist to a separately-bound module-level `let private err: ErrorType<_, _> = Message "..."` — a combined `let p = let err = ... in fun r -> ...` triggers F#'s value restriction and compiles as a thunk that re-allocates on each access.
- `Foo@<line>` closure class with self-bytes → an anonymous lambda allocated per invocation. Either inline the callee manually, or mark the enclosing function `inline` so `[<InlineIfLambda>]` propagates.
- Class-member parser `let` bindings re-evaluating per use → lift them to a module-level `let` or a sibling `module private Submodule` so initialization happens once at module load.
- Record field type changes that inflate layout → e.g. swapping `ParseState.Trace: TraceCallback` to `TraceCallback voption` added an 8-byte tag+padding per copy and bumped Medium allocations ~1% (ParseState is copied hundreds of times per parse). For "optional reference" hot fields, prefer `[<AllowNullLiteral>] type Foo() = …` + a nullable reference field — same pointer-width storage as the original, guarded by `if not (isNull state.Trace) then …`. An inline `[<InlineIfLambda>]` helper (`ParseState.ifTrace state (fun tc -> tc.Foo(args))`) keeps call sites idiomatic and compiles down to a plain null check + direct call with no closure.

### Common CPU diagnoses

- Hot function with a `match … with` / `when`-guard ladder where most inputs fall through to a default → reorder so the match dispatches on the distinguishing scalar first (F# emits a jumptable for enum matches). The `nextSyntaxTokenImpl` refactor collapsed five sequential `when token.Token = Token.IfDirective` / `ElseDirective` / `EndIfDirective` / `NoWarnDirective` / `WarnOnDirective` guards into a single `switch` for ~1% of parse time.
- Duplicate computation inside a hot function → hoist once. `getIndent` was called twice per token in `nextSyntaxTokenImpl` (once for the offside check, once for the trace callback). Binding `tokenCol` once saved ~1%.
- Repeated field reads through a property chain → cache into a local. `let state = reader.State` at the top of hot functions helps the JIT register-alias. Combined with the above two, this batch accounted for a 6-8% Medium wall-time win.
- Virtual no-op callbacks firing per token → see the `[<AllowNullLiteral>]` pattern above. Even a `default _.Foo() = ()` body pays a vtable dispatch per call; the null check skips it entirely.
- Wide `choiceL` on the hot path where the next peeked token would decide the winner → replace with a peek-and-dispatch (see "Peek-and-Dispatch Over `choiceL` Trial Loops" above). On the expression-RHS dispatcher this alone saved 10-20% Medium/Large and also dropped allocations because the eliminated peeks had been constructing `SyntaxToken` records each.
- `CastHelpers.IsInstanceOfClass` (BCL) appearing at >1% self → DU-case pattern matching compiles to sequential `isinst` checks for multi-case reference-DUs. Look for `match x with | CaseA _ | CaseB _ | … | CaseN _ ->` on hot paths. Some DUs tolerate conversion to tag-struct form; others regress — two prior attempts in this repo (`ExprAux` and `ErrorType`) reverted with notes in memory. Experiment behind a throwaway commit before integrating.

### Workflow

1. Run the relevant benchmark class under idle-system conditions (ShortRun CIs are too wide to resolve sub-5% changes; prefer `dotnet run -c Release -- --filter '*Parsing*'` without `-j short` when measuring). Capture allocated-bytes and mean-time per size as the baseline.
2. Identify hotspots with one of: `--trace <size> <iters>` for allocations, or `--profiler ETW` + `--aggregate-cpu` for CPU. The Medium fixture (functions-heavy, LET + Pratt-dominated) is the richest signal for parser work.
3. Apply the smallest mechanical change implied by the hotspot (hoisting, caching, dispatch collapse, null-callback short-circuit). Resist compound refactors — it's easier to attribute a win to one change than to three.
4. Verify correctness: `pwsh ./claude_tools.ps1 -Action Test -TestProject XParsec.FSharp.Tests` (1338 expected passing). Golden-file tests catch behavioral changes that pure benchmarks don't.
5. (Optional) `ilspycmd` spot-check the compiled IL to confirm inlining / switch compilation / closure elimination actually happened. Search for `clo@`, `@<line>-N` class definitions — if they appeared where you expected `[<InlineIfLambda>]` to erase them, the inlining isn't firing (often because the caller isn't itself `inline`).
6. Re-run the benchmark and diff against step 1's baseline. Allocations are stable across runs; mean-time needs tight CIs (long-run, idle system).
