# Partial application — a `Fun` value struct over a static parsed spec

**Status: deferred, not built.** The one unlanded piece of the printf design (for the
landed architecture see [printf-architecture](printf-architecture.md)). This plan
scopes zero-alloc partial application and is deleted once it lands.

## Current behaviour

`let p = printfn "%d"` already *works* — but via the FSharp.Core cold path, not the
Vesper handler. The happy-path gate (`Passes/Unification/InferApp.fs`,
`tryInferPrintfApp`) accepts only fully-applied literals
(`args.Length = specs.Length + 1`); an under-applied call is left unmarked, so
`FreezeExpr.fs` keeps the `App printfn` intact and it lowers to
`Microsoft.FSharp.Core.PrintfModule`. Correct output, but it allocates the
`PrintfFormat` object + closures the rest of `Vesper.Printf` is built to avoid.

## Target design

`let p = printfn "%d"` becomes a value struct `S : Fun<int, unit>`:

- Its only "state" is the format, a compile-time constant → a **`static readonly`
  parsed-spec field**. So `S` is **stateless ⇒ `default(S)`, zero heap allocation**,
  and `constrained.callvirt Fun::Invoke` devirtualises (rides the `Fun` work directly
  — see [function-representation-plan](function-representation-plan.md)).
- The static field holds the **parsed spec** (parsed once at type init), not the raw
  string. `Invoke` drives it through the same P1 handler (`formatter.fs`) — no
  reparse, no box. **This is the one place `PrintfFormat` survives** — demoted to the
  static-field / cold representation, never on the happy path.
- **Multi-hole currying:** `printfn "%d %s"` partially applied is a chain — an outer
  `Fun<int, Fun<string, unit>>` whose `Invoke(n)` yields an inner `Fun<string, unit>`
  **capturing `n`** plus the shared static spec; the final `Invoke` runs spec →
  handler. Intermediate stages capture bound args (not stateless), still value structs
  where they don't escape.
- **Escape caveat:** zero-alloc only while `p` flows into `Fun`-bounded generic
  positions or is invoked directly; an interface-typed `Fun<int, unit>` slot boxes it
  — it inherits whatever the `Fun` escape analysis decides.

## Where it plugs in

- **The gate.** `tryInferPrintfApp` gains a *lowerable partial application* case: a
  literal format, under-applied, with lowerable placeholders, diverted to the
  value-struct lowering instead of falling through to FSharp.Core.
- **The spec-runner.** `Invoke` calls a runtime spec-runner that walks the parsed
  spec and drives the P1 handler. This is the **same** engine the cold path
  (format-as-value / non-literal) needs, so building it here also gives the cold path
  a Vesper runner and removes another FSharp.Core dependency.

## Dependencies

- The `Fun` value struct + escape analysis
  ([function-representation-plan](function-representation-plan.md)) — the
  representation these structs ride on. This is the gating prerequisite.
- The parsed-spec representation already exists: `PrintfSpec.fs` produces the
  typed-hole sequence; the static field holds it (or a lowered form of it).

## Acceptance

- `let p = printfn "%d" in p 3` prints `3` with **no heap allocation** on the
  construct/invoke path where `p` does not escape.
- Multi-hole currying (`printfn "%d %s"`, applied one arg at a time) works.
- Byte-for-byte parity with FSharp.Core's output across the spec matrix.
- The cold-path spec-runner and the partial-application `Invoke` share one engine and
  one handler.
