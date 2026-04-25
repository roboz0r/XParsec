# 12. Taming the stack: tracing, probing, and a second operator rewrite

## Hook

For a stretch in the middle of the project I thought about stopping. Deep expression parsing — chains of binary operators, deeply nested patterns, recursive prefix operators — would blow the .NET default 1 MB stack without warning. The process died. No stack trace. No position. No recoverable diagnostic. Golden-file tests that had passed last week now crashed the test runner. The combinator style itself seemed to be the problem.

What got me unstuck was instrumentation. Once I could *see* the stack growing I could find where it wasn't supposed to. The result is now one of the two pillars of the testing infrastructure (the other is the corpus test, post 10), and the remediation included a second rewrite of the operator parser — this time driven not by missing features but by stack consumption.

## What this post covers

- **The crisis.** F# expressions nest freely. A list of a hundred items separated by `;` produces a hundred-deep Pratt frame. An expression like `a + b + c + ... + z` with enough operands hits the same ceiling. Pattern parsing has the same problem with nested tuples and records. The .NET default stack is 1 MB. Combinators that look innocent — `parser { let! x = pLeft in ... }` — can each contribute a handful of frames to the call depth. Do the math and the ceiling comes fast.
- **Why stack overflows are the worst bugs.** `StackOverflowException` on .NET is unrecoverable in-process. You can't catch it. You can't emit a diagnostic. You can't even finish logging what you were doing when it hit. The process exits. Every test in that process is lost. Unlike the heap-allocated infinite loops (which the combinator timeout catches), a stack overflow gives you nothing to work with.
- **First response: parser tracing.** Commit `a1f666f Add parser tracing`. The parser emits a `TraceEvent` at every context push/pop, every offside check, every virtual token synthesis, every permitted undentation. A pluggable `TraceCallback` consumes them. Under normal test runs it's a no-op; in debug tests it's written to `file.fs.trace`. Commit `fc0dfbc Add tracing to debug test` wires it in. Commit `bd9d2ab Add guidance to use trace debugging` captures the workflow: focus the Trace Debugging Test, drop a minimal reproduction into `data/manual/file.fs`, run, read the trace.
- **The `.fs.trace` file.** A flat log of every parser event, with source position and (where relevant) the offside rule that let a token through. Invaluable for "the parser stopped here — why?" questions. For a failing input, the last few lines before the failure are almost always diagnostic.
- **Second response: the stack probe.** This part the LLMs designed, and it's clever enough that I'm happy to give credit. The design:
  - Run the parse on a dedicated `Thread` with an *explicit* stack size (so the test doesn't depend on whatever stack the process started with).
  - At every `ContextPush` / `ContextPop` trace event, capture the current stack pointer by taking the address of a local variable (`NativePtr.toNativeInt &&marker`). This is a cheap SP proxy; it doesn't need to be accurate, only monotonic.
  - Track the deepest depth observed. Each time depth increases, snapshot the full `System.Diagnostics.StackTrace`.
  - Emit the report to `file.fs.stack`: the deepest trace, plus every recorded SP with its delta from the previous sample.
  - Enforce a timeout on the thread so infinite loops don't hang the whole test run.

  See `parseWithStackProbe` in `test/XParsec.FSharp.Tests/TestHelpers.fs`. The design is the kind of thing a seasoned engineer might also produce given half a day to think, but the LLM had it in one prompt. Both the probe and the format of the `.fs.stack` output were designed in collaboration with Claude.

- **The `.fs.stack` file.** Two sections. The deepest-ever stack trace (file/line for every frame). Then a full SP log: `SP=0x... delta=...  PUSH SeqBlock indent=12 depth=84`, in order. The `delta` column is the whole point — it tells you how many bytes each combinator is costing at the deepest call path. A healthy parser has small, bounded deltas. A sick parser has one combinator blowing through hundreds of KB per level.
- **Who reads the `.fs.stack` file?** Mostly Claude. Stack overflow debugging is tedious — it's reading forty frames of parser-combinator call chain and recognizing which one should have been iterative instead of recursive. LLMs are good at this specific pattern-recognition task, and the `.fs.stack` format was chosen (again: collaboratively) to be LLM-friendly. The harness that made that collaboration cheap is post 11.
- **The second operator parser rewrite.** Post 5 covers the XParsec 0.2 → 0.3 redesign driven by *missing* features. This was the 2.0 of the second operator parser: same features, rewritten to bound stack consumption. The Pratt loop had been naturally recursive: process LHS, find operator, recurse for RHS, combine. Every RHS call added frames. For a left-associative chain of 500 operators the recursion depth was 500. The rewrite made the loop iterate along the left spine, stacking operators on an explicit work list and folding them at decreasing precedence. Same parse result, constant stack depth.
- **Stack-probe-driven refactoring.** The rewrite landed in several commits, each guided by the `.fs.stack` output. Look for places where the deepest trace repeatedly hit the same combinator — that combinator was the target. `839e52d Refactoring Expr lhsParser`, `80ac363 Avoid eager consumption of tokens in expression parsing`, `11bf544 Parser fixes and optimizations`. The pattern each time: the `.fs.stack` pointed at the hot combinator; the fix replaced an open recursion with an explicit loop.
- **Infinite loop protection as a bonus.** Commit `97397c4 Add infinite loop protection to the debug parser test`, `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing`. The same thread-with-timeout wrapper that isolates stack overflows also isolates runaway loops. Two classes of "my parser is dead" bug collapse into one observable: "the thread didn't finish in 10 seconds."

- **Postscript: the pattern repeats, and the second time is cheap.** Commit `7786a60 Reduce stack consumption in AST traversal` (2026-04-18) fixed exactly the same class of bug in `AstTraversal.fs` — a 1,300-line `walkExpr` whose per-call frame was large enough that deep ASTs overflowed even after the parser itself had been fixed. The remedy was the now-familiar one: split the giant pattern-match into one small function per DU arm, each taking the arm's fields as arguments, so the enclosing frame stays small regardless of which branch fires. What took weeks the first time took a single prompt and seven minutes of Claude time. Post 13 quotes the prompt and walks through the diff — the second occurrence of a bug shape is almost free, given the harness from post 11.

## Anchor commits / files

- `test/XParsec.FSharp.Tests/TestHelpers.fs` (`parseWithStackProbe`, `writeStackProbe`)
- `test/XParsec.FSharp.Tests/ParserTests.fs` ("Trace Debugging Test")
- `src/XParsec.FSharp/Debug.fs` (`TraceEvent` + format)
- `a1f666f Add parser tracing`, `fc0dfbc Add tracing to debug test`, `bd9d2ab Add guidance to use trace debugging`
- `97397c4 Add infinite loop protection to the debug parser test`
- `839e52d Refactoring Expr lhsParser`, `80ac363 Avoid eager consumption of tokens in expression parsing`, `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing`
- `7786a60 Reduce stack consumption in AST traversal` (same pattern, second occurrence)

## Takeaway

When your debugger tells you nothing — because the process died before it could — the only way forward is to write the debugger into your own code. Tracing captured *what* was happening; the stack probe captured *how much stack* each step cost. With those two streams in text files I could see a class of bug that had been driving me toward quitting, and pick it off combinator by combinator.

The post-mortem line is: don't try to avoid stack overflows by being careful. Instrument the stack, make its depth visible, and let the visible depth drive the refactoring. And if an LLM hands you the design for that instrumentation in one shot, take it.
