---
name: xparsec-dev
description: Use this when you need to build the project, run tests, run benchmarks, or format F# code using Fantomas in the XParsec repository.
---

# XParsec Everyday Development Tooling

You are operating on a Windows machine in the `XParsec` repository. To perform development tasks safely, you MUST use the local wrapper script (`./claude_tools.cmd`), which is a thin proxy that forwards all arguments to `claude_tools.ps1`.

- **DO NOT** use raw `dotnet` commands. They will trigger permission blocks or flood your context window.
- **DO** use the **serena** MCP to search within the repository and navigate the codebase efficiently.

The script exposes four actions: `Build`, `Test`, `Format`, and `Benchmark`.

## Available Actions

### 1. Building the Project (`-Action Build`)
To compile the entire repository:
```bash
./claude_tools.cmd -Action Build
```

To compile a specific library project (path `src/<SourceProject>`) and isolate compilation errors, use the `-SourceProject` parameter:
```bash
./claude_tools.cmd -Action Build -SourceProject "XParsec.FSharp"
```

**Valid source projects are:**
- `XParsec`
- `XParsec.CLArgs`
- `XParsec.FSharp`
- `XParsec.FSharp.Codegen.Clr`
- `XParsec.FSharp.Codegen.Js`
- `XParsec.FSharp.SemanticAnalysis`
- `XParsec.Json`

The terminal shows only error/warning lines plus the build summary; the full build output is always written to the log file (see Logging below).

### 2. Running Tests (`-Action Test`)

To run a test suite, you must use the `Test` action and specify the exact test project (path `test/<TestProject>`).

```bash
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Tests"
```

**Valid test projects are:**
- `XParsec.C.Tests`
- `XParsec.CLArgs.Interactive`
- `XParsec.CLArgs.Tests`
- `XParsec.FSharp.Codegen.Clr.Tests`
- `XParsec.FSharp.Codegen.Js.Tests`
- `XParsec.FSharp.Lexer.Tests`
- `XParsec.FSharp.SemanticAnalysis.Tests`
- `XParsec.FSharp.Tests`
- `XParsec.Json.Tests`
- `XParsec.MessagePack.Tests`
- `XParsec.Tests`
- `XParsec.Toml.Tests`
- `Vesper.Tests`

**Focusing a specific test — two options:**

- **Preferred (no rebuild semantics to worry about):** use your file editing tools to change the test definition in the source from `test` to `ftest` (focused) before running.
- **Expecto `--filter`:** pass `-Filter` to target tests by name. This switches the runner to `dotnet run --no-build -- --filter <name>`, so the test project **must already be built** (run a `Build` first), otherwise it will fail:
  ```bash
  ./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Tests" -Filter "MyTestName"
  ```

**Fixing Failing Golden/Snapshot Tests:**
If a test fails because the AST or lexed output changed intentionally, force the runner to overwrite the golden files with the `-UpdateSnapshots` flag (sets `UPDATE_SNAPSHOTS=1` for the run):
```bash
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Tests" -UpdateSnapshots
```

**Controlling terminal output:** test output is truncated to the last 30 lines (the summary) by default. Override with `-SummaryLines <n>`:
```bash
./claude_tools.cmd -Action Test -TestProject "XParsec.Tests" -SummaryLines 60
```

### 3. Formatting F# Code (`-Action Format`)

To format all F# code across the repository using Fantomas:

```bash
./claude_tools.cmd -Action Format
```

### 4. Running Benchmarks (`-Action Benchmark`)

Runs BenchmarkDotNet against `bench/XParsec.FSharp.Benchmarks`. A `-Filter` is **mandatory** (wildcards allowed, e.g. `'*Lexing*'`):

```bash
./claude_tools.cmd -Action Benchmark -Filter "*Lexing*"
```

**Available benchmark names:** `EndToEndBenchmarks`, `LexingBenchmarks`, `ParsingBenchmarks`.

By default the fast in-process toolchain (`-i`) is used. To capture a profile, pass `-Profiler` with one of:
- `EP` — EventPipe sampling (cross-platform)
- `ETW` — Windows-only event tracing

```bash
./claude_tools.cmd -Action Benchmark -Filter "LexingBenchmarks" -Profiler EP
```

When a profiler is set, BDN runs out-of-process (drops `-i`) and writes `.speedscope.json` traces alongside the report. Profiling has extra restore prerequisites (BDN >= 0.15.x, FSharp.Core centrally pinned) — see the `reference_bdn_profiler_eventpipe` memory before running with `-Profiler`.

## Logging and Debugging

Every time you run `claude_tools.cmd`, the **complete, unfiltered output** of the underlying command is automatically written to `claude_tools_output.log` in the repository root (cleared at the start of each run).

- To save your context window, the terminal output is filtered/truncated (test output to the last `-SummaryLines` lines, build output to errors + summary).
- If tests fail or code fails to compile and the truncation hides the actual stack trace or compiler error, **DO NOT run the command again.**
- Instead, immediately use your native `Read` tool to open `claude_tools_output.log` to investigate the failure.
