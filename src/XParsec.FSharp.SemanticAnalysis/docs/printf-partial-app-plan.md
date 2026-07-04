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
- **Multi-hole: land flat, curry only on demand.** `printfn "%d %s"` fully unapplied
  is a *flat* value struct `Fun<int, string, unit>` (the flat arity-2 interface,
  overloading curried `Fun<_,_>` by generic arity; stateless ⇒ still just the shared
  static spec field). Applying one arg does **not** build a nested `Fun<int, Fun<string,
  unit>>` chain — it produces a `Curried<int, string, unit>` residual `Fun<string,
  unit>` **capturing `n`** plus the spec, reusing the exact `Curried`/flat-`Fun` adapter
  machinery already in `Vesper.Core/core-types`. The final saturated `Invoke` runs spec
  → handler. An *n*-hole spec needs the flat `Fun`*(n+1)* + its `Curried`*n*; today only
  the flat `Fun`3`/`Curried` (arity 2) exist, so this is the driver to introduce
  `Fun`4`/`Fun`5`… and
  their curried residuals — a mechanical extension of the landed arity-2 template, not a
  new design. Only the on-demand residual captures (is stateless-less); the unapplied
  and saturated forms stay stateless value structs where they don't escape.
- **Escape caveat:** zero-alloc only while `p` flows into `Fun`-bounded generic
  positions or is invoked directly; an interface-typed `Fun<int, unit>` slot boxes it
  — it inherits whatever the `Fun` escape analysis decides.

## Where it plugs in

- **The gate.** `tryInferPrintfApp` gains a *lowerable partial application* case: a
  literal format, under-applied, with lowerable placeholders, diverted to the
  value-struct lowering instead of falling through to FSharp.Core. **Scope: `printf`
  only for now** — same `idx = 0` restriction as the happy path. The rest of the family
  (`fprintf`/`sprintf`/`eprintf`, and the `idx ≠ 0` writer/builder sinks) stays on the
  FSharp.Core path; review the whole family for partial application once `printf` lands.
- **The spec-runner.** `Invoke` calls a runtime spec-runner that walks the parsed
  spec and drives the P1 handler. This is the **same** engine the cold path
  (format-as-value / non-literal) needs, so building it here also gives the cold path
  a Vesper runner and removes another FSharp.Core dependency. **The `Invoke` body is
  emitted through the same lowering as `EmitFormat.fs`**, so the `printfn` trailing
  `"\n"` and the `Flush`-vs-`ToStringAndClear` sink choice fall out of the generated
  closure for free — the spec-runner doesn't re-derive them, and byte-for-byte parity
  is the same guarantee the happy path already carries.

## Dependencies

- The `Fun` value-struct representation + escape analysis — what these structs ride
  on. `Vesper.Core/core-types` already ships the flat `Fun<'A,'B,'C>` interface (CLR
  `Fun`3`, overloading curried `Fun`2` by generic arity) and the `Curried`/`Flattened`
  adapters this plan builds on; the gating prerequisite is value-struct (stateless /
  captured) closures over those, plus the higher `Fun`4`/`Fun`5`… arities the
  multi-hole case needs. See [function-representation-plan](function-representation-plan.md)
  for the landed closure representation; the code (`core-types.fsi`,
  `prim-types-min.fsi`) is the source of truth.
- The parsed-spec representation already exists: `PrintfSpec.fs` produces the
  typed-hole sequence; the static field holds it (or a lowered form of it).

## Acceptance

- `let p = printfn "%d" in p 3` prints `3` with **no heap allocation** on the
  construct/invoke path where `p` does not escape.
- Multi-hole (`printfn "%d %s"`) lands flat as `Fun<_,_,_>`; applying one arg at a time works
  via the `Curried` residual, and the *n*-hole form works once `Fun`*n*/`Curried`*n*
  exist.
- Byte-for-byte parity with FSharp.Core's output across the spec matrix — including the
  `printfn` trailing newline, which comes from sharing the `EmitFormat` lowering.
- The cold-path spec-runner and the partial-application `Invoke` share one engine and
  one handler.
- **Scope: `printf` only.** The rest of the family is a follow-up review, not this
  landing.
