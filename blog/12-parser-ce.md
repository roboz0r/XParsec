# 12. Hardening the ParserCE

## Hook

A parser combinator library's computation expression is its most visible surface. The line `parser { let! x = pFoo in ... }` is what users will write thousands of times. Late in the project a batch of CE-level work landed: inlining, non-boxing `using`, and a subtle imperative-semantics fix for `while` and `for` inside `parser { }`. These commits are small and nuclear — they improved correctness *and* performance without a single API change.

## What this post covers

- **The CE reminder.** XParsec's `parser { }` compiles a monadic expression down to `Bind`/`Return`/`ReturnFrom`/`TryWith`/`Using`/`While`/`For`. Each is a method on the CE builder. The compiler generates code that boxes state through those methods unless they're written to avoid it.
- **Non-boxing `using`.** Commit `6e94e00 Non-boxing using semantics in ParserCE`. The default `Using` members for CEs take an `IDisposable`-bound resource and wrap in a closure that keeps the resource alive across the monadic bind. If you're careful you can write a `Using` that doesn't box the parser state at all — crucial in a combinator library where this runs in the hot loop.
- **Inlining primitives.** `aa03713 Inline implementation of ParserCE primitives`. `Bind`, `Return`, `Zero`, `Combine` get `[<MethodImpl(MethodImplOptions.AggressiveInlining)>]`. JIT then inlines the entire monadic chain back into a single straight-line function. The CE becomes a zero-cost syntactic convenience.
- **The `while`/`for` imperative-semantics fix.** Commit `e09f08d Fix imperative semantics in ParserCE's If While and For`. The bug: `while cond do mutate()` inside a `parser { }` was reading a stale snapshot of `cond` because the CE desugaring wrapped the whole loop in a delayed thunk. The fix: give `While` and `For` members that preserve plain F# imperative semantics, evaluating `cond` each iteration. This kept the CE's monadic behavior for `let!`/`do!` while letting plain statements inside the CE behave like the F# programmer expects. This is the fix documented in the memory as `feedback_parser_ce_while`.
- **Why this matters in the lexer.** The lexer uses `parser { while hasMore do ... }` heavily (interpolated string fragments, string content, block comments). Before the fix several of those loops silently misbehaved; after the fix, they're plain F# loops that happen to produce a parser result.
- **Parent escape bug.** `af9c4ff Fix parent escape bug and optimise Readables`. A subtle reader-identity bug where a child reader could "escape" back to a parent it had diverged from. Found by corpus stress-testing, fixed by tightening the `Readables` invariant. This is the kind of fix that looks like nothing in the diff and prevents a class of mysterious recovery failures.
- **Helper consolidation.** `67787e4 Merge nextNonTriviaTokenVirtualIfNot and nextNonTriviaTokenVirtualWithDiagnostic` and `83baaf0 Merge skipInactiveBranch and skipElseBranch`. Two pairs of near-duplicate helpers collapse to one each. Follows the same consolidation pattern as post 8 but at the parser-infrastructure level.
- **Before and after benchmarks.** If time allows: simple microbenchmarks showing the allocation rate and throughput on a representative file before and after the inlining commits. Even if you don't benchmark, the commit messages themselves tell the story.

## Anchor commits / files

- `src/XParsec/ParserCE.fs` (and wherever the CE methods live)
- `src/XParsec.FSharp/ParsingHelpers.fs` (consolidated helpers)
- `aa03713`, `6e94e00`, `e09f08d`, `af9c4ff`, `e5f861d Fix build for ParserCE fixes`
- `67787e4`, `83baaf0`

## Takeaway

You can spend years in a combinator library's surface and then one careful afternoon rewriting the CE — and the afternoon pays for itself an order of magnitude faster. If your CE is the user-visible API, budget time to make it zero-cost.
