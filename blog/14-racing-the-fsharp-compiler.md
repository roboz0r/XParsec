# 14. Racing the F# Compiler

## Hook

(TBD — open with the headline number: at time of writing, `XParsec.FSharp` parses the F# compiler's own source ~2× faster than `FSharp.Compiler.Service` with substantially fewer allocations in the common case. The story of how that happened lives on the `fsharp-bench` branch.)

## What this post will cover

- **The starting point.** Where the parser sat when the corpus first closed (post 13) — correct, but unprofiled. Set out the baseline numbers from BDN: small / medium / large fixtures, wall time, allocations, vs FCS.
- **The profiling pipeline.** BenchmarkDotNet for steady-state numbers; `dotnet-trace` + the in-process `AllocAggregate` harness for allocation hotspots; the EventPipe profiler for CPU samples; the `--trace` mode of the bench runner that ties them together.
- **The hot-path catalogue.** What the profiler kept pointing at:
  - `nextSyntaxTokenImpl` and the lexical-filter dispatch
  - Pratt RHS dispatch — the 6-way `choiceL` was costing both stack and time
  - `pPostfixType` — type-adjacent peek dispatch
  - `pOperatorToken` — span-scan consumption + length-bucketed lookup
  - Allocations from parser-CE bodies that re-allocated combinator closures on every call
- **The mechanical wins.**
  - Hoisted CE allocations: lift module-level combinator calls out of `parser { }` bodies (`feedback_hoist_ce_combinators`)
  - Eta-expanded inlinable updates inside CEs (`pattern_ce_eta_expand_for_inline`)
  - Peek-once + span dispatch instead of per-arm `pstring` (`pattern_peek_once_startswith_dispatch`)
  - Length-bucketed flat tables for operator/keyword lookup (`pattern_length_bucketed_span_lookup`)
  - SIMD-aware ordering of `IndexOf` dispatch tables (`pattern_simd_indexof_chunk_alignment`)
  - Add fast-path / NoInlining slow-path for hot append-with-grow methods (`pattern_add_fastpath_slowpath`)
- **The shape of a perf session.** Profile → identify the top 1–3 hotspots → form a hypothesis → spike it → BDN compare → keep or revert. Most ideas don't pay off: see the cluster of "spike failed, don't retry" memories (`feedback_struct_union_aux_not_worth_it`, `feedback_errortype_spike_failed`, `feedback_voption_inflates_parsestate`, `feedback_dispatch_to_match_regresses`, `feedback_redundant_peek_guard`, `feedback_ascii_fastpath_unicode_category`). Negative results are data; the harness keeps you from chasing them twice.
- **Final numbers.** Updated BDN table. Allocations vs FCS, wall time vs FCS, on the same `prim-types.fs` / functions-corpus / types-corpus fixtures.

## Anchor commits / files

- `fsharp-bench` branch
- `src/XParsec.FSharp/Lexing.fs`, `ExpressionParsing.fs` — the hotspots
- `XParsec.FSharp.Benchmarks/` — BDN harness
- The April 2026 perf-session memories (`project_alloc_reduction_apr19`, `project_nexttoken_refactor_apr22`, `project_pratt_rhs_dispatch_apr22`, `project_ppostfix_peek_dispatch_apr22`, `project_poperator_span_lookup_apr24`)

## Takeaway

(TBD — provisional thesis: a parser combinator library written with mechanical sympathy from the start can compete with hand-written parsers. The wins came not from rewriting in C# or moving to a generated parser, but from observing what allocates and what dispatches, and methodically removing the unnecessary work. The `fsharp-bench` branch is the diary of that methodical work.)
