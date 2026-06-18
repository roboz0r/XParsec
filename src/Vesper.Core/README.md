# Vesper.Core

The minimal, self-hosted core library for **Vesper** (the language; `XParsec.*`
is the *compiler*). It replaces the FSharp.Core dependency in emitted programs
and the FSharp.Core `.fsi` port as the front-end symbol contract.

It is the minimal, self-hosted slice: a universal `.fsi` contract plus a
per-target `.fs` implementation, compiled by this repo's own backend (no
`dotnet`/`fsc`, no FSharp.Core). The design rationale — namespace, `Fun`,
`unit = ValueTuple`, the contract/impl split — is captured inline below.

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `*.fsi` | **contract** — signatures in the `Vesper.*` namespace | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `*.fs` | **runtime impl** — the Tier-0 types/members | this repo's own backend → `Vesper.Core.dll` (BCL-only) |

They are a normal signature/implementation pair, both ours. Drift is caught the
way F# catches it — the impl is checked against its signature (until that
conformance check exists, a reflection round-trip over `Vesper.Core.dll`
substitutes). Today the impl (`.fs`) is a partial, growing subset of the
contract (`.fsi`); that is expected while the self-hosting ladder is climbed.

## Layout — decomposed per concern

The contract is split one file per primitive group rather than one monolith, so
a **target includes exactly the primitives it supports**: a JS target omits
`prim-types-nativeint.fsi` and the `int64` half of `prim-types-int.fsi`; a CLR
target keeps them. The split also tracks the universal-vs-target axis
(`ops-platform` lowers to per-target CIL intrinsics; `ops-std` is target-
agnostic). Compile order is fixed in `manifest.toml` (parser accumulates
declarations file by file).

| `.fsi` file | Defines |
|---|---|
| `prim-types-min.fsi` | `int`, `bool`, array, **`Fun<'A,'B>`**, **`unit = ValueTuple`** — the Turing-complete minimum |
| `prim-types-int.fsi` | `sbyte`/`byte`/`int8`/`uint8`/`int16`/`uint16`/`int32`/`uint32`/`int64`/`uint64`/`uint` |
| `prim-types-float.fsi` | `float32`/`float`/`single`/`double` |
| `prim-types-string.fsi` | `char`, `string` |
| `prim-types-object.fsi` | `obj`, `objnull` |
| `prim-types-exn.fsi` | `exn` |
| `prim-types-decimal.fsi` | `decimal` |
| `prim-types-nativeint.fsi` | `nativeint`/`unativeint`/`nativeptr`/`voidptr`/`ilsigptr` (target-specific) |
| `prim-types-nd-array.fsi` | multi-dimensional array types |
| `prim-types-attr.fsi`, `compiler-attributes.fsi` | compiler-recognised attribute hooks (stubs for now) |
| `core-types.fsi` | `Ref`, `ValueOption`, `Result` (`Option` split out to `src/Vesper.Option/`; `Collections.List<'T>` + `List.fold` to `src/Vesper.List/`) |
| `ops-platform.fsi` | arithmetic / bitwise / comparison — lower to per-target CIL intrinsics |
| `ops-std.fsi` | logical / composition / pipe — target-agnostic, over `bool` / `Fun` |

Impl side (`.fs`, our-backend target source — **not** built by `dotnet`/`fsc`):
every `prim-types-*.fsi` now has a companion `prim-types-*.fs` that binds its
`extern` types to `(# "..." #)` intrinsics (`prim-types-min.fs`, `-int.fs`,
`-float.fs`, `-string.fs`, `-object.fs`, `-exn.fs`, `-decimal.fs`,
`-nativeint.fs`, `-nd-array.fs`, `-attr.fs`). Retargeting a primitive (e.g. `int`
to 64-bit) is a one-line edit in the relevant `.fs`. (The cons-list impl moved to
the standalone `src/Vesper.List/` package — see its README.)

## Naming / shape decisions

- **`Fun<'A,'B>`** is *the* function representation — a one-`Invoke` interface,
  **not** `IFunc` (Vesper is not .NET, so no Hungarian `I`-prefix). Closures
  implement it; application is `callvirt Fun::Invoke`. `'A -> 'B` is sugar for
  `Fun<'A,'B>`; curried arrows nest. This drops `FSharpFunc` from the native
  path (returns only under a future `--fsharp-compat`).
- Namespaces are **`Vesper`** and **`Vesper.Collections`** (no `.Core` segment).
- **`unit = System.ValueTuple`** — the BCL zero-arity value tuple, the nullary
  product; no type to ship (D7).
- Operator modules are **`[<AutoOpen>]`** so `(+)`, `|>`, `&&` resolve
  unqualified, as in FSharp.Core's `Operators`.

## No `.fsproj`

Like `XParsec.FSharp.Lib`, this tree is not built by `dotnet`/`fsc`. The
operators alone force it: a `let inline (+)` body needs inline IL /
`--compiling-fslib`, so the operator files are contract-only. The `.fsi` are
parsed by `XParsec.FSharp` and walked into an `IExternalSymbolProvider`; the
`.fs` are compiled by our own backend once the self-host ladder lands.

> **`ops-platform.fs` now carries the operator + `hash` implementations.** The live
> `=`/`<>`, arithmetic/bitwise/unary, and `hash` semantics in emitted programs come
> from this frozen `.fs` (F# static-optimization over inline IL), read across the
> package boundary by the codegen inline-body loader (`SymbolProviders.inlineBodies`)
> and spliced at each use site. The `Emit.BuiltinOps` op→opcode table remains only as
> the un-ground / nested / generic fallback (and `Emit.isHash` is gone). Both
> front-end gaps that once blocked this are closed — operator-named bindings freeze
> with their compiled name, and BCL generic-member resolution reaches the
> `EqualityComparer<'T>` fall-clauses.

Parser coverage is verified: every `.fsi`/`.fs` here parses with zero recovery
diagnostics — the same bar as the FSharp.Core corpus — by
`test/XParsec.FSharp.Tests/VesperCoreContractTests.fs` (golden `.parsed`
snapshots committed next to each source). Inline IL (`(# "!0[]" #)` etc.) parses
fine; it is the fsc *build* that needs `--compiling-fslib`, not the parser.

## Self-hosting status

The `.fs` files are the *target* source for the self-hosting ladder, climbed in
dependency order:

1. **Library output + nominal type / interface emission** — emits `Fun`
   (`unit` is BCL `ValueTuple`, so it drops out of this rung). **Landed** — see
   `test/XParsec.FSharp.Codegen.Clr.Tests/SelfHostTests.fs`.
2. **Union + recursion + `match`** — emits `List<'T>` and `List.fold` (the
   standalone `Vesper.List` package). **Landed.**
3. **Strings + chars + printf engine** — `Vesper.Printf` (separate library);
   still a C# interim pending a self-hosted rewrite.

Until a rung lands, that slice can stay on a C# interim.

## Cross-references

- [`fsi-target-brainstorm.md`](../XParsec.FSharp.SemanticAnalysis/docs/fsi-target-brainstorm.md) — universal-contract / per-target-impl architecture; `type int = Platform.Int32`.
- [`function-representation-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/function-representation-plan.md) — the function representation (`Fun`) and the closure devirtualisation endgame.
- `XParsec.FSharp.Lib/compiler-clr-project.md` — the full FSharp.Core port this supersedes as the contract, and the manifest/loader mechanics reused here.
