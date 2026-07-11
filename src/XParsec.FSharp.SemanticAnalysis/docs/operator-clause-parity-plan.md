# Backend conformance corpus — and operator clause parity as its first tenant

## Why this is now urgent

Deleting `Emit.BuiltinOps` changed what a missing operator clause *means*.

Before: every operator had a codegen fallback. A contract clause that was absent, or
wrong, degraded quietly to an opcode table that was at least *present* on both backends.
A clause set was a precision detail.

After: `ops-platform.fs` (CLR) and `ops-platform.js.fs` (JS) are the **sole** source of
operator semantics. Each operator is a static-optimization whose base is an SRTP trait
call, and `Passes.InlineExpansion` reports a receiver that cannot dispatch as
*"The type 'X' does not support the operator '+'"*. So the clause list is no longer a
detail — **it is the definition of which types support arithmetic** — and it is written
twice, once per backend, with nothing checking that the two agree.

Two failure modes follow, different in kind:

1. **Set drift.** A primitive with a clause on CLR and none on JS is a program that
   compiles for one target and is a *compile error* for the other. The sets happen to
   match today; nothing holds them there. Both file headers assert the match in prose.

2. **False precision.** `ops-platform.js.fs` enumerates 12 clauses per binary operator,
   and ~8 of them are knowingly-wrong bare JS operators. They *read* as coverage. Under
   the doctrine the CLR file adopted for `decimal` — no correct clause ⇒ diagnose, never
   emit garbage — those clauses should not exist at all.

Failure mode 2 is the more serious: it is the one the change's own principle forbids, and
the one no existing test can see.

## The current divergence, precisely

JS clauses correct today: `int` (`| 0`, `Math.imul`), `int64` (`BigInt.asIntN(64, …)`),
`float`, and `string` for `(+)`.

JS clauses knowingly wrong — the bare JS operator, which is what the old polymorphic base
gave these widths, now spelled out one clause at a time so it *looks* deliberate:

| width | what JS does | what F# says |
| --- | --- | --- |
| `byte` | `10uy - 20uy` → `-10` | `246` (`& 0xFF`) |
| `sbyte` | `100y + 100y` → `200` | `-56` (`<< 24 >> 24`) |
| `int16` | `30000s + 10000s` → `40000` | `-25536` (`<< 16 >> 16`) |
| `uint16` | `10us - 20us` → `-10` | `65526` (`& 0xFFFF`) |
| `uint32` | `4000000000u + 1u` → float | `>>> 0` |
| `uint64` | BigInt, unwrapped | `BigInt.asUintN(64, …)` |
| `nativeint` / `unativeint` | bare | no JS repr decided |

Division is worse than a missing mask: JS `/` is true division, so `10uy / 3uy` yields
`3.3333…` — not an integer at all. `(~-)` on the unsigned widths yields a negative
number. Every row is a wrong *answer*, silently, at runtime.

`float32` is a third category: `$0 * $1` computes in double precision with no
`Math.fround`, so results are *close* but not F#-faithful. That needs a decision, not
necessarily a fix.

## The shape: a conformance corpus, not a test harness

The instinct is a differential test — compile the same program with both backends, run
both, diff the outputs. **Don't.** Comparing A to B only says they disagree; it cannot
say which is wrong, and it goes green the moment both are wrong the same way.

The right shape is a **corpus of self-contained conformance programs** that any backend
can be pointed at:

```
test/Codegen.Conformance/            # data only — NOT compiled by any fsproj
    ops/arith-byte.fs
    ops/arith-int64.fs
    ops/arith-unsigned-div.fs
    ops/arith-nativeint.diag.fs      # must NOT compile (see below)
    manifest.toml
```

Each `.fs` is a real program in our dialect that computes and reports; conformance is
"compile it, run it, it agrees". Precedent exists: `test/XParsec.FSharp.Tests/data/*.fs`
are corpus files on disk that no fsproj compiles — which they must not, since they are
our F#, not the host's. Keep that property; a `<Compile>` item on any of these is a bug.

The shared piece is one small library project, `XParsec.FSharp.Codegen.Common.Tests`,
holding the corpus loader and the assertions, parameterized over the one thing that
differs:

```fsharp
type Backend =
    {
        Name: string
        /// Compile + run a conformance program. `None` ⇒ the runtime is unavailable
        /// (no Node on PATH) and the row SKIPS rather than fails.
        CompileAndRun: string -> string -> (int * string) option
        /// Compile only; the error-severity diagnostics.
        Diagnostics: string -> string list
    }

val conformanceTests : Backend -> Test
```

Both backends already expose exactly this seam — CLR has `compileSource` +
`runEntryPoint` (an `int * string`), JS has `runNodeFiles` (already an `option`, skipping
cleanly when Node is absent). So `Codegen.Clr.Tests` and `Codegen.Js.Tests` each add
*one* file: their `Backend` value and `yield conformanceTests clrBackend`. A future
`Codegen.Wasm` gets the whole corpus for free by writing one record.

## Two wrinkles worth deciding up front

**1. "Self-asserting" is circular if the assertion vocabulary is under test.**

A program that judges itself with `if x = y then …` is trusting `=` — and `=` is one of
the things this corpus exists to check. A broken structural `=` makes an assertion
vacuously pass. The existing arithmetic corpus already has both shapes, and one is
strictly better:

```fsharp
printfn "%d" (if 200uy + 100uy = 44uy then 1 else 0)   // trusts `=` to judge `+`
printfn "%d" (int (10uy - 20uy))                       // prints the value; nothing to trust
```

So: a conformance program **prints computed values and nothing else**, and its expected
stdout lives beside it as a golden — `arith-byte.fs` / `arith-byte.expected`. That is
still one expectation, still written once, still target-neutral (it is F# semantics, not
a backend's opinion), and the runner stays "compile, run, diff stdout, expect exit 0".
The judgement moves out of the program under test and into a file no backend can
influence.

Where a case genuinely needs `=` (structural equality of a DU, say), that is *that
program's subject* and using it is correct — the rule is only that a program must not
lean on a primitive it is not there to test.

**2. Not-compiling is also conformance.**

The whole point of the `BuiltinOps` deletion is that an unsupported operand is now a
*compile error* rather than silent garbage. That is a conformance property exactly as
much as an answer is, and it is where the two backends legitimately **differ**:
`nativeint + nativeint` should compile and run on CLR, and should *fail to compile* on
JS, because JS has no repr for it.

So the corpus has two kinds of file, and the manifest says which applies where:

```toml
[[program]]
path     = "ops/arith-byte.fs"
backends = ["clr", "js"]        # both must run it and match arith-byte.expected

[[program]]
path     = "ops/arith-nativeint.fs"
backends = ["clr"]              # runs here…
[program.diagnose]
js = "does not support the operator"   # …and must be REJECTED here, with this message
```

This is the load-bearing bit. It gives every (operator, width, backend) a small closed set
of legal states, with no state in which a clause exists but computes the wrong thing.
`decimal` on CLR already lives in the "rejected" state. The plan is to make every width
live in one of the legal states, the way `decimal` already does. Prose in a file header
("the narrow widths still await their masking templates") stops being how we track this;
the manifest is.

**3. There is a third legal state: runs and FAULTS.** (Discovered while scoping — the
"exactly two states" claim above was one short.) Integer division by zero must throw, and
that is a conformance property with the same standing as an answer or a rejection. It
cannot ride the "expect exit 0, diff stdout" runner, because the two harnesses surface a
fault in structurally *different* shapes: CLR invokes the entry point in-process by
reflection, so an uncaught exception never becomes an exit code at all — it becomes a
host-side failure carrying the exception's type name (`TestHelpers.runtimeThrows`, already
used by `LogicalOperatorTests` for exactly this). JS runs Node out-of-process and *does*
return a real non-zero exit code plus stderr. So the runner's result is not `int * string`
but an outcome — **completed** (exit code + stdout) or **faulted** (a description) — and
each backend maps its own surface into it.

The expectation stays target-neutral. The BCL message for `DivideByZeroException` is
"Attempted to divide by zero."; the JS helper throws an `Error` carrying that same message,
so one case-insensitive substring in the manifest matches both backends.

Note what is NOT available: **`try … with` is unemittable on both backends.** The
`TExprG.TryWith` node parses, infers and freezes, but neither `Codegen.Clr/EmitExpr.fs` nor
`Codegen.Js/EmitJs.fs` has an emit arm — it falls through to `failwithf "unsupported
expression"`. So a fault must be observed as an *uncaught* fault; a catch-and-print program
is not a shape the dialect can express today.

## Sequencing

The order matters, because step 1 is designed to go red.

1. **Stand up the corpus + runner, seeded from the existing CLR arithmetic corpus.** Lift
   the rows out of `ArithmeticOperatorTests` into conformance programs with goldens, and
   point both backends at them. Expect ~8 widths × several operators to fail on JS. That
   is the deliverable: converting invisible wrong answers into red tests. Fix nothing yet.

   (Note the narrow widths must route through `int (…)`: a negative `sbyte`/`int16`
   literal is not representable in `TConstValue`, so `100y + 100y = -56y` cannot be
   written, but `int (100y + 100y)` prints `-56` and reads the same truncation.)

2. **Triage each red width into "runs" or "diagnoses".** A real decision per width, not a
   mechanical fix:
   - `byte` / `sbyte` / `int16` / `uint16` / `uint32` — a JS repr exists and the mask is
     well understood. Write the template. `/` needs truncation *and* the mask, not just
     the mask.
   - `uint64` — BigInt exists; `BigInt.asUintN(64, …)` is the wrap. Straightforward.
   - `nativeint` / `unativeint` — no decided JS representation. **Delete the clauses.** Note
     these already *do* diagnose on JS, but not because of the clause list: `PlatformTypes`
     rejects any primitive with no JS repr ("no JS representation"), the same mechanism that
     catches `decimal`. The clauses are therefore dead surface — unreachable, because such a
     program never gets past type-checking. Removing them is wart-excision, not a behaviour
     change, and the manifest row should be green from the start.
   - `float32` — **decided: `Math.fround` per operation.** It is not a special case; it is
     the single-precision width mask, the exact analogue of `| 0` for `int32` and
     `BigInt.asIntN(64, …)` for `int64`. Documenting the double-precision approximation
     instead would be the false-precision failure mode this plan exists to end — a clause
     that reads as coverage and computes a subtly wrong answer.
   - **Integer division by zero — decided: JS must throw, matching CLR.** CIL `div` / `rem`
     fault on a zero divisor; JS `/` yields `Infinity`, and `Infinity | 0` is `0`. So the
     truncation templates this step writes would, left bare, ship a *new* wrong answer of
     exactly the kind being removed. Every integer `/` and `%` clause is therefore guarded.
     Three constraints on the mechanism. The operands must not be evaluated twice, which
     rules out an inline ternary guard on the `$N` holes. A bare JS-expression template
     cannot register a module import — the `=` / `hash` base arms already show the shape
     that can, delegating to a non-inline runtime `val` whose body lives in
     `Vesper.Core.mjs` and which the backend imports through the ordinary external-call
     path. And the throw cannot name `System.DivideByZeroException`: that roster lives in
     `Vesper.Exceptions`, which declares `depends-on = ["Vesper.Core"]`, so reaching for it
     from `ops-platform.js.fs` is a cycle. Vesper.Core throws with its own vocabulary —
     `exn` (`prim-types-exn`) carrying the CLR message.

     That last point costs nothing, and it is worth being precise about why. Every type in
     the `Vesper.Exceptions` roster **already erases to the `exn` root (`Error`) on JS**, so
     type-based catch is uniformly lossy there today — `:? InvalidOperationException` cannot
     be told from any other `Error`. A plain `exn` from the division guard is therefore
     exactly as faithful as every other JS exception and introduces no new gap; what
     conformance actually turns on is that the program *faults* instead of silently
     yielding `0`.

     Note the widths collapse: every non-64-bit width is a JS `number` and can share one
     checked helper, with the width mask applied around the call; `int64` / `uint64` are
     BigInt and need their own (`0n`).

3. **A cheap structural guard, once the sets are right.** Harvest each operator's
   `TExpr.StaticOptimization` clauses from the contract body, project each
   `TStaticOptConstraint.TyconEquals(_, required)` to its primitive name, and assert the
   set equals what the manifest says that backend supports. Both suites already have the
   harvest machinery (`OpsPlatformClrTests` / `OpsPlatformJsTests`,
   `ArithmeticOperatorTests.opcodesOf`); only the constraint projection is new. This
   catches drift without paying for Node or CIL, and it catches a clause *added* for a
   width the manifest says must diagnose — the false-precision regression, mechanically.

## Why this is worth more than operators

Operator clause parity is the forcing case, but it is not the interesting one. Once a
backend is a 3-field record and the corpus is data on disk, *every* semantic question the
two backends could answer differently gets a file: structural equality, `hash`, printf
specifiers, list/seq operations, exception semantics, integer division by zero, closure
capture. Each is one program plus one golden, and every backend — including ones not
written yet — either conforms or says why it cannot.

That is the actual deliverable. Operator arithmetic is just the tenant that proves the
building works.

**The named next tenant: exception type identity on JS.** Every type in the
`Vesper.Exceptions` roster erases to the `exn` root (`Error`) on JS, so a type-based catch
cannot tell `InvalidOperationException` from any other error there. Lowering each to a real
`class … extends Error` would restore genuine identity for exceptions we throw, and the
prototype chain would give subtype matching for free. Two things make it more than a
mechanical change, and are why it wants a conformance program rather than an argument:

- The *declared* hierarchy becomes observable. `exceptions.fsi` currently has every type
  inheriting `exn` directly — a flat roster, harmless only because it is never consulted.
  The BCL's is not flat (`ArgumentNullException` derives from `ArgumentException`), so on
  CLR `with :? ArgumentException` catches a thrown `ArgumentNullException` and a flat JS
  roster would not. The corpus program that pins this is the deliverable; the lowering is
  the fix.
- Identity is restored only for exceptions *we* throw. A host-originated `TypeError` has no
  Vesper identity, so `:? NullReferenceException` stays a CLR-only proposition.

Prerequisite: **`try … with` must be emittable first** (see wrinkle 3 — it is not, today),
since none of this is observable without a catch. And when it lands it creates an
obligation on the division guard: a bare `exn` for divide-by-zero, which is faithful *today*
precisely because everything erases, becomes observably wrong the moment catches are
nominal. Which exposes the real structural point — **the exception roots are in the wrong
package.** `prim-types-exn` defines `exn` inside `Vesper.Core`, and it is core operations
(integer division, array indexing, invalid casts) that need to throw BCL-named exceptions;
yet `Vesper.Exceptions` declares `depends-on = ["Vesper.Core"]`, so core is structurally
forbidden from naming the very exceptions it must raise. A roster core cannot reach is a
roster one layer too far downstream. If the nominal lowering happens, fold the roots into
`Vesper.Core` alongside `exn` rather than keeping a package core cannot use.

## Non-goals

- **Making the clause sets identical.** Portability is a property of the corpus, not of
  the tables. A width JS cannot represent should be absent from JS and diagnose there;
  the manifest states the asymmetry rather than smuggling it.
- **A shared clause generator** (one table emitting both `.fs` files). Tempting given ~73
  near-identical clauses per file, but it puts a code generator between the reader and the
  semantics of `+`, and the contract files are meant to be readable F# our own front end
  parses. The duplication is the price of that; the corpus is what makes it safe.
- **Fixing `decimal` on CLR.** It correctly diagnoses today. A clause calling
  `Decimal::op_Addition` is separate work.
