# TS symbol-provider MVP vertical slice

Scaffolds the `.d.ts → JSON manifest → F# provider → resolves` slice from
[`codegen-js-symbol-provider-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/codegen-js-symbol-provider-plan.md).
F#/Fable + **shared schema** (one type grammar, used natively by the .NET loader
and Fable-compiled to JS for the extractor, so producer/consumer can't drift).
JSON is handled by the repo's own `XParsec.Json` — no external JSON dependency.

## Projects

| Project | Role | Status |
|---|---|---|
| `Vesper.Ts.Manifest.Schema` | Shared neutral IR (`Schema.fs`) + JSON codec over `XParsec.Json` (`Codec.fs`) | **Done, builds clean (net8.0 + netstandard2.0)** |
| `XParsec.Json` (`JsonWriter.fs`) | `JsonValue → string` writer (inverse of the existing parser); retargeted to add `net8.0`/`netstandard2.0` | **Done** |
| `XParsec.FSharp.Codegen.Js/TsManifestProvider.fs` | Loader: manifest → `IExternalSymbolProvider` (mirrors `JsNativeSymbols`/`MetadataSymbols`) | **Done, builds clean; verified resolving the fixture** |
| `Vesper.Ts.Extractor` | Fable extractor driving the real `ts.TypeChecker` → manifest JSON | **Working (MVP)** — runs `tsc` over a `.d.ts` and emits the manifest |
| `test/Vesper.Ts.Extractor.Tests` | Golden harness (Expecto) + fixtures under `specs/<feature>/` | **Done; 4/4 pass** |

## Building / running the extractor

```
npm install                         # brings in the pinned `typescript`
npm run extractor:build             # dotnet fable → src/Vesper.Ts.Extractor/dist
node src/Vesper.Ts.Extractor/dist/Program.js <pkg.d.ts> <packageName> <out.json>
```

The extractor is wired against `vendor/TypeScript.fs` (Glutinum, MIT). MVP scope:
non-generic `Interface` members (primitive-typed) + free `Function`s; types via
`typeToString`. Broadening the type mapping (unions → `TyOr`, generics,
structurals, construct signatures, import-shape detection) is the next work.

## Golden tests

`test/Vesper.Ts.Extractor.Tests` adapts the XParsec golden ergonomics but stays
light: the **`.manifest.json` is the golden artifact** (no separate rendered
snapshot — that `.parsed`-style dump suits an AST parser, not this). Per fixture:
the **real golden** runs the compiled extractor on the `.d.ts` and asserts its
output equals the sibling `.manifest.json`; plus the committed JSON is in
canonical serialized form, the loader resolves every declared export, and an
orphan guard pairs each `.d.ts` with its `.manifest.json`. The extractor stage
**skips cleanly** when `dist/` isn't built or `node` is absent (CI-safe).
Regenerate goldens (extractor is source of truth) with
`UPDATE_SNAPSHOTS=1 dotnet run --project test/Vesper.Ts.Extractor.Tests` after
`npm run extractor:build`.

## Verified end-to-end

`greeter.d.ts` → extractor (real `tsc`) → `greeter.manifest.json` → loader →
provider resolves `shout` (function), `Greeter` (type), and its `greet`/`greeting`
members; codec round-trips; canonical form holds. Resolution is checkable against
the CLR pipeline today — nothing needs the JS backend to emit yet.

## To finish the extractor (one open item)

1. **Wire to the real bindings.** The full TS compiler API surface is vendored at
   `src/Vesper.Ts.Extractor/vendor/TypeScript.fs` (`module rec TypeScript`, from
   Glutinum, MIT — see the file header). Remaining work is to replace
   `Extractor.fs`'s stub `Ts*` interfaces + `loadContext` with the real
   `TypeScript.ts.*` calls (`createProgram`/`getTypeChecker`/`getExportsOfModule`/
   `getDeclaredTypeOfSymbol`/`getPropertiesOfType`/call+**construct** signatures/
   `resolveModuleName`). Optional ergonomic helpers (`SyntaxKind` name lookups
   etc.) live in Glutinum's `TypeScript.Extensions.fs` if wanted.

The earlier ImmutableArray-under-Fable concern is **retired**: XParsec core shims
the `ImmutableArray` surface the `JsonValue` path uses (`FableTypes.fs` —
`CreateRange`, indexing, `Length`, gated on `FABLE_COMPILER`). Mirror
`XParsec.Json`'s own opens so `Codec`'s references bind to the shim.

## Deferred (schema slots exist; mapping not wired)

`any`→`TyDynamic`; structural content-hash + SCC cycles; conditional/mapped
evaluation; union classifier (null/undefined → distinct JS intrinsics, literal→enum);
registering `null`/`undefined` JS intrinsics in `IntrinsicRepr`; `ImportShape`
threading; the `retype` override layer. See the plan's mapping table.
