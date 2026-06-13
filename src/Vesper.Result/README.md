# Vesper.Result

The `result` type and the `Result` module for **Vesper** (the language;
`XParsec.*` is the *compiler*), packaged standalone per
[`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md)
(PS1: one package per type). Carved out of `Vesper.Core`'s `core-types.fsi`.

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `result.fsi` | **contract** — the type + `Result` module signatures, in namespace `Vesper` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `result.fs` | **runtime impl** — the union + module bodies | this repo's own backend → `Vesper.Result.dll` (BCL-only) |

A normal signature/implementation pair, both ours. Today `result.fs` is a
partial, growing subset of the contract (the array/list/`option` conversions and
`contains` are contract-only — and in fact not even in the contract yet — for
now) — expected while the self-hosting ladder is climbed, the same stance as
`Vesper.Option` and `Vesper.Core`'s `List`.

## Two legs (package-split-plan PS3)

- **Type leg** — the `Result<'T, 'TError>` struct union (`Ok of 'T` /
  `Error of 'TError`). Does not need `Fun`, but it does need **struct-union emit**
  in the backend: the rung-2 union path currently emits reference classes, so
  value-typed unions are the backend follow-up this representation implies (shared
  with `Vesper.Option`).
- **Module leg** — `Result.map`/`mapError`/`bind`/`fold`/… are overwhelmingly
  higher-order, so they ride **R1** (the `Fun`-not-`FSharpFunc` cutover). Until R1
  the module leg can sit on a C# interim per PS3.

## Naming / shape decisions

- **Package name ≠ namespace** (PS5): the package is `Vesper.Result`, but it
  contributes type `Result` and module `Result` into namespace **`Vesper`** (not
  `Vesper.Result`).
- **`Result` is a struct** — like `Vesper.Option`'s `Option`, Vesper's `Result`
  is a value type, so neither `Ok` nor `Error` allocates on the heap. The price is
  the usual struct trade-off: `Result<'T, 'TError>` is copied by value and boxes
  if it crosses an `obj` boundary.
- **`Data`, not `State` — opt-in `[<StructuralComparison>]`** (operators-plan.md
  O10): `Result` is unconditionally structurally equatable and structurally
  comparable *iff its type arguments are* (the O7
  `Comparison.Structural ⇒ Equality.Structural` invariant holds). The recognised
  comparison attributes themselves live in `Vesper.Core` (PS4), since the DAG
  forbids Core depending on `Vesper.Comparison` yet Core/leaf types must be
  annotatable.
- The module is **`[<RequireQualifiedAccess>]`** + **`ModuleSuffix`** so it
  shares the `Result` name with the type (compiled name `ResultModule`), matching
  `Vesper.Option`'s `Option` and `Vesper.Core`'s `List`.
- Module functions are **not `inline`** (unlike FSharp.Core): the Vesper backend
  lowers each functional-argument application to `callvirt Fun::Invoke` rather
  than expanding at the call site, the same choice as `Option.map` / `List.fold`.

## Module surface

A focused starter set whose signatures touch only `Fun` / `bool` / `int` /
`result` itself: `isOk`, `isError`, `defaultValue`, `defaultWith`, `count`,
`fold`, `foldBack`, `exists`, `forall`, `iter`, `map`, `mapError`, `bind`.

Deliberately deferred (additive later, as the cross-package surface grows):

- `toArray` / `toList` / `toOption` — cross-package conversions
  (`Vesper.List` / `Vesper.Option`).
- `contains` — needs the `'T: equality` constraint surface.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.Option` / `Vesper.Printf` / `XParsec.FSharp.Lib`,
this tree is not built by `dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp`
and walked into an `IExternalSymbolProvider`; the `.fs` is compiled by our own
backend once the self-host ladder lands. Parser coverage is verified by golden
`.parsed` snapshots committed next to each source
(`test/XParsec.FSharp.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md) — the per-package split this realises (PS1/PS2/PS3/PS5).
- [`operators-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/operators-plan.md) — O10, the opt-in `[<StructuralComparison>]` on `Result`.
- [`../Vesper.Option/README.md`](../Vesper.Option/README.md) — the sibling option package this mirrors.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on; `Fun`, `unit`, the contract/impl mechanics reused here.
