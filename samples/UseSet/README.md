# UseSet — a standalone `Vesper.Set` consumer

A standalone program that consumes the built `Vesper.Set` package end-to-end.

[`Program.fs`](Program.fs) references nothing of the package but its `.fsi`
contract — `open Vesper.Collections` brings in `Set<'T>` and the `Set` module —
and exercises the operation surface the Phase-9 golden table proved: construction
(`ofArray`/`ofList`), cardinality/membership, set algebra in both function
(`union`/`intersect`/`difference`) and operator (`+`/`-`) forms, transforms
(`map`/`filter`/`fold`), predicates (`forall`/`exists`), `partition`, ordering
(`min`/`maxElement`), the `'T list` / `'T array` bridges, and `iter`.

## How it is built and run

Like the rest of the Vesper tree this sample is **not** built by `dotnet`/`fsc`
and carries no `.fsproj`. It is compiled by this repo's own backend, loaded
alongside the emitted `Vesper.Set.dll` and its eight transitive `Vesper.*` deps,
and run in-process.

That whole flow — and the assertion that the program's stdout matches the
expected output baked into its comments — is the regression gate
[`UseSetSampleTests.fs`](../../test/XParsec.FSharp.Codegen.Clr.Tests/UseSetSampleTests.fs),
which reads *this exact file* and drives it through the `runsSet` harness
(`test/XParsec.FSharp.Codegen.Clr.Tests/TestHelpers.fs`). Run it with the
`xparsec-dev` skill or:

```
dotnet test test/XParsec.FSharp.Codegen.Clr.Tests --filter "UseSetSample"
```

Because the gate reads the committed source rather than a copy, the sample
cannot rot into a program that no longer compiles or whose output has drifted.

## HOF posture

Arguments the module *folds* are written **curried** (`fun acc -> fun x -> …`),
matching the Freeze posture the `runsSet` harness documents; single-argument
predicates and projections stay uncurried.

## Cross-references

- [`../../src/Vesper.Set/manifest.toml`](../../src/Vesper.Set/manifest.toml) — the package this consumes.
- [`core-lib-architecture.md`](../../src/XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md) — how the Vesper packages fit together.
- [`SetModuleTests.fs`](../../test/XParsec.FSharp.Codegen.Clr.Tests/SetModuleTests.fs) — the per-operation golden table this demo composes into one program.
