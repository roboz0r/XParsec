# The `Vesper.Printf` contracts: one to implement, one that is fiction

**Status (2026-08-15): new plan.** Independent of the other three — needs no `.fsi`
resolution work to start. Delete when it lands (`feedback_plan_docs_ephemeral`).

Architecture background is [printf-architecture](printf-architecture.md), which is **stale**:
its line citations have drifted past the ends of the files they name, and its
"falls back to FSharp.Core `PrintfModule`" row no longer describes the code — `PrintfModule`
has no hit anywhere under `src/`. Read it for intent, not for facts.

## `printf-format.fsi` — write the body

`Vesper.PrintfFormat<_,_,_,_>` (`printf-format.fsi:12`) is a real type: an annotation naming
it resolves like any ordinary external type. But at a call site the format's type is minted
unconditionally on the **FSharp.Core** key:

```fsharp
// PrintfSpec.fs:284-285
let formatType (printer: SemType) (fam: Family) : SemType =
    TyClass(RuntimeNames.printfFormatKey, ...)   // Microsoft.FSharp.Core.PrintfFormat`4
```

and `ClrEncoder.fs:88` rewrites both keys to `Microsoft.FSharp.Core.PrintfFormat`4`, calling
`markFSharpCoreDep`. Since the cold path was deleted, **that line is the only FSharp.Core
printf touchpoint left in the backend.**

The contract declares a ctor and a `Value` member and nothing else, so the body is small.
Writing `printf-format.fs`, then pointing `formatType` at `RuntimeNames.vesperPrintfFormatKey`
(`RuntimeNames.fs:71-72`), pairs the contract, removes its exemption, and drops the
dependency. Continues the direction of `27a8e103 Remove the FSharp.Core fallback for lists`.

## `printf.fsi` — decorative, and two-thirds wrong

Not an unimplemented contract. A bypassed one.

- **The front end never consults it.** `Scope.fs:129-131` suppresses `Unresolved identifier`
  for any name `PrintfSpec.tryFamily` admits, so `printf "%d" 1` compiles whether or not the
  file exists.
- **`PrintfSpec.families` (`PrintfSpec.fs:211-221`) knows eight names; the contract declares
  three.** `eprintf`, `eprintfn`, `fprintf`, `fprintfn` and `bprintf` work today with no
  contract at all — which is the proof the other three do not need theirs either.
- **Two of the three declarations disagree with the compiler, and nothing catches it.**
  `printf.fsi:7,10` declare `PrintfFormat<'Printer, unit, string, unit>` — `'State = unit`,
  `'Residue = string`. `writerFamily` (`PrintfSpec.fs:175-184`) types both as
  `'State = TextWriter`, `'Residue = unit`, and the table wins: `InferApp.fs:282-287` builds
  its own `fnTy` from the literal. `sprintf`'s declaration matches `stringFamily`.

The drift is invisible precisely because there is no `.fs`, so conformance has nothing to
check against — the `sig-only` key is what hides it.

Two ways out:

1. **Delete it from `files`.** `Scope.fs` already calls the family a front-end intrinsic and
   five-eighths of it already lives that way. Costs the doc comments and the consumer-visible
   surface.
2. **Make it load-bearing.** `tryInferPrintfApp` types off the declared scheme instead of the
   hardcoded table; fix the two wrong rows; declare all eight. The contract is then real and
   still bodiless, so it needs a declaration-site marker meaning "no definition is emitted" —
   the value-level analogue of `type X = extern`. `[<Global>]` (`Attributes.fs:214`, checked
   in both halves; see `prim-types-undefined.js.fsi`/`.fs`) is the precedent for the *shape*,
   but not reusable: it names a target global, and `printf` has none.

Only (2) kills the drift permanently. It is much the larger change, and it is the one that
would benefit from conformance comparing resolved signatures
([fsi-front-end-plan](fsi-front-end-plan.md)) rather than from a new marker alone.

**Recommendation: take `printf-format.fsi` first.** It is the only entry here where a body is
both writable and wanted, and it pays for itself.

## Not in scope

`EmitJs.fs:457` `failwithf`s on every sink but `ToStdOut true` / `ToStdErr true` / `ToString`,
so `printf`, `eprintf`, `fprintf` and `bprintf` crash the JS backend rather than diagnosing.
Real, and a different plan.
