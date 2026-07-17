# Vesper.Result

The `result` type and the `Result` module for **Vesper** (the language;
`XParsec.*` is the *compiler*), packaged standalone — one package per type, see
[`core-lib-architecture.md`](../XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md).
Carved out of `Vesper.Core`'s `core-types.fsi`.

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

## The type and the module are separate legs

- **The type** — the `Result<'T, 'TError>` struct union (`Ok of 'T` /
  `Error of 'TError`). Does not need `Fun`, but it does need **struct-union emit**
  in the backend: the union path emits reference classes, so value-typed unions
  are the backend follow-up this representation implies (shared with
  `Vesper.Option`).
- **The module** — `Result.map`/`mapError`/`bind`/`fold`/… are overwhelmingly
  higher-order, so they need the `Fun`-not-`FSharpFunc` function representation.

## Naming / shape decisions

- **Package name ≠ namespace**: the package is `Vesper.Result`, but it
  contributes type `Result` and module `Result` into namespace **`Vesper`** (not
  `Vesper.Result`).
- **`Result` is a struct** — like `Vesper.Option`'s `Option`, Vesper's `Result`
  is a value type, so neither `Ok` nor `Error` allocates on the heap. The price is
  the usual struct trade-off: `Result<'T, 'TError>` is copied by value and boxes
  if it crosses an `obj` boundary.
- **`Data`, not `State` — opt-in `[<StructuralComparison>]`**: `Result` is
  unconditionally structurally equatable and structurally comparable *iff its type
  arguments are* (the `Comparison.Structural ⇒ Equality.Structural` invariant
  holds). The recognised
  comparison attributes themselves live in `Vesper.Core`
  (`compiler-attributes.fsi`), since the DAG forbids Core depending on
  `Vesper.Comparison` yet Core/leaf types must be annotatable.
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

Like `Vesper.Core` / `Vesper.Option` / `Vesper.Printf`, this tree is not built by
`dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp` and walked into an
`IExternalSymbolProvider`; the `.fs` is compiled by our own backend. Parser
coverage is verified by golden `.parsed` snapshots committed next to each source
(`test/Vesper.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`core-lib-architecture.md`](../XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md) — the per-package split this realises, and the dependency graph it sits in.
- [`../Vesper.Option/README.md`](../Vesper.Option/README.md) — the sibling option package this mirrors.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on; `Fun`, `unit`, the contract/impl mechanics reused here.
