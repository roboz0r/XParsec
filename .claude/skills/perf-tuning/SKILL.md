---
name: perf-tuning
description: Performance work on the XParsec parser and lexer: how to benchmark, where the remaining wins are, and the optimisations already tried and reverted. Use before proposing any allocation or hot-path optimisation, and when reading a CPU profile.
---

# Performance tuning

## The standing verdict on hot-frame work

The obvious optimisations in `XParsec.FSharp.Parser` are spent. Do not propose an isolated
hot-frame intervention as a standalone win.

Profile-flagged frames in this codebase are now hot because they are **frequently invoked**,
not because they hold local optimization slack. CPU-self percentage at this level is misleading:
a small-percentage frame does not have a small-percentage intervention hiding inside it.

The parser has already absorbed an allocation-reduction pass (~20-25% allocations, wall time to
60%), a lexer token refactor (~6-9% parser wall time), a Pratt RHS dispatch change (~8-20%), a
postfix peek dispatch (~3-6%), an operator span lookup (~10-12% lex allocations) and named ops
in the well-known-ops table (~2-4% lex).

**How to apply.** Summarise what a shared profile says, but propose a fix only where there is a
clear *structural* opportunity: a new dispatch shape, a different data structure, an algorithmic
change. "Hoist X out of the CE" and "convert tail recursion to a loop" are no longer enough on
their own. If the user asks for a specific hotspot fix, do it, since they know their code, but
flag this guidance and ask whether a structural alternative would be preferable first.

## Running a benchmark

`./claude_tools.cmd -Action Benchmark -Filter '*Lexing*'` against `bench/XParsec.FSharp.Benchmarks`.
A filter is mandatory; wildcards are allowed. The benchmark types are `EndToEndBenchmarks`,
`LexingBenchmarks` and `ParsingBenchmarks`.

The fast in-process toolchain is the default. `-Profiler EP` (EventPipe, cross-platform) or
`-Profiler ETW` (Windows) runs out of process and writes `.speedscope.json` traces beside the
report.

Measure on an idle machine. Two of the reverted spikes below built clean and passed the full
suite while regressing wall time.

### Profiling prerequisites

BDN's profilers only work with the out-of-process toolchain, so `-Profiler` auto-drops `-i`. In
process fails validation with "EventPipeProfiler supports only .NET Core 3.0+", which actually
means the profiler is incompatible with `InProcessEmitToolchain`.

`BenchmarkDotNet` and `BenchmarkDotNet.Diagnostics.Windows` are pinned at 0.15.8 in
`Directory.Packages.props`, which is new enough: 0.14.0 did not recognise `net10.0` as a runtime
and fell through to that same misleading validator.

**FSharp.Core still needs raising for a profiling session.** The central pin is 8.0.300 while
FCS pins 10.1.202 transitively. The bench project carries a `VersionOverride` that solves this
for in-process runs, but the out-of-process toolchain regenerates a csproj that re-inherits the
central pin and trips NU1109. Raise the central `<PackageVersion Include="FSharp.Core">` to
10.1.202 for the session and revert afterwards, because raising it permanently forces the whole
solution onto FSharp.Core 10.x.

`--profiler ETW` captures allocation stacks, the richer signal, but its kernel session requires
elevation. EP does not.

### Reading an EP trace

BDN's EP profiler emits `profile.type = "evented"`: method entry and exit events with one
`CPU_TIME` node, so **self time per frame is always 0** and only inclusive wall time is
actionable. EP therefore shows which parsers are on-stack for how long, not where allocations
happen. Allocation attribution needs ETW with elevation, or dotnet-trace with the GC-verbose
provider.

Artifacts land in `BenchmarkDotNet.Artifacts/` at the repo root rather than under `bench/`, as
`<name>-Size_<N>-timestamp.speedscope.json` and `.nettrace`.

## Tried and reverted: do not re-attempt

### `ErrorType<'T, 'State>` as a struct

`src/XParsec/Types.fs`. Two approaches, both failed.

A manual `[<Struct>] { Tag: byte; Token: 'T; Payload: obj }` with AutoOpened constructor
functions and active patterns is compile-blocked by ~25 **FS0064 "less generic than indicated"**
errors. A DU case constructor carries an implicit polymorphism a regular generic function lacks:
the case `Message : string -> ErrorType<'T,'State>` stays generic across call sites, while
`let inline Message<'T,'State>` forces inference of both parameters at each site, and unused
ones fall back to `obj`. It also trips the value restriction on every module-level
`Message "..."` binding.

`[<Struct>]` on the existing DU builds clean, but an F# struct DU emits **a field per case**
rather than a compact tag-and-payload layout. The struct's size becomes the sum of all nine
cases' fields and it is passed by value through every Pratt call, giving a **stack overflow** in
`OperatorParsing.Pratt.parseRhsInternal` after 274 of 1338 tests.

The preferred `{ Tag; Token; Payload }` shape cannot be had in F# with a DU's ergonomics.

### `ExprAux` as a tag-discriminated struct

`src/XParsec.FSharp/ExpressionParsing.fs`, converting from a reference DU to
`[<Struct>] {Tag; Token1; Token2; Payload: obj}` with `Get*()` accessors unboxing via `:?>`.

| input | allocations | time |
| --- | --- | --- |
| Small | 269.91 → 268.56 KB | 449.0 → 440.3 µs (noise) |
| Medium | 1594.09 → 1571.41 KB | 2823.1 → **2946.5 µs (+4.4%)** |
| Large | 25850.35 → 25609.78 KB | 55332.9 → **57760.2 µs (+4.4%)** |

The allocation saving is real but tiny, because `ExprAux` is not the dominant allocator. The
`:?>` unbox casts on the hot Pratt completion path cost more than the case-class allocation they
remove, and passing the ~48-byte struct by value through `Result<ExprAux, _>` pessimises
register usage. The regression came from the downcast rather than the struct layout, so this is
worth reconsidering only if `ExprAux` becomes radically simpler with no `obj` slot.

### `voption` on a `ParseState` field

Switching `ParseState.Trace: TraceCallback` to `TraceCallback voption` added ~1% allocated bytes
(Small +1.68 KB, Medium +18.2 KB, Large +267 KB) for no benefit.

`ValueOption<'T>` is a struct DU, so for a reference `T` the compiler still emits `_tag: int` and
`_value: T` with padding: 16 bytes on x64 where the field was an 8-byte pointer. `ParseState` is
copied hundreds to thousands of times per parse, so one 8-byte inflation is measurable.

For an optional reference field on a hot path, use `[<AllowNullLiteral>]` plus a nullable field
and `if not (isNull state.Foo)`. Same pointer width, same fast path, no layout change. Hide the
null check behind an `inline` helper with `[<InlineIfLambda>]`; F# lowers it to a plain null test
with no closure.

### Two hot-frame conversions

`nextSyntaxTokenImpl` as a while loop regressed Large by 7.0%. A `Type.parse` `opt`-hoist was
flat. Both built clean and passed 1338 of 1338.

## Where allocation wins actually remain

Push and pop offside closures, `Type` parser closures, `ParseState`
equality, and `ImArr` growth. On the error path specifically: hoist common `Message "..."` values
to module-level bindings, avoid `ParseError.createNested [a; b]` where the list allocation
dominates, and use a `ValueSome` soft error against `ValueNone` to skip error construction
entirely on success paths.

## Choosing a struct in the first place

`[<Struct>]` only where minting dominates passing. Count the hops a value takes after
construction: many hops means a plain record, while constructed-and-immediately-consumed (a
dictionary probe key, a loop-local) makes a struct defensible. A record created once per lookup
and then threaded through a provider chain is passed more than created, so the struct pays a
copy at every hop to save one allocation at the start.
