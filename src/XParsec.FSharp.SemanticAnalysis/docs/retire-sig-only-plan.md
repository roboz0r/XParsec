# Retire `sig-only`

**Status (2026-08-15): new plan, gated.** The mechanical removal is last; each of the four
entries has to earn its way out first. Delete when it lands
(`feedback_plan_docs_ephemeral`).

**Question (user, 2026-08-15):** rather than keep `sig-only` as a better-spelled exemption,
remove the category by providing an appropriate implementation for the signatures.

Supersedes the "Why NOT also merge the rest" section of
[manifest-single-file-list-plan](manifest-single-file-list-plan.md), which argued `sig-only`
should stay because the intent "has no spelling in the file list". Correct as far as it went,
and beside the point: the intent should not need a spelling, because there should be no
exemption to declare.

## Why the key exists at all

`checkManifest` has four acceptance routes. Three are DERIVED from file content —
`Paired`, `Unrepresentable` (every declaration is `extern` or an abbreviation),
`RuntimeServed` (the committed asset exports every declared `val`). `sig-only` is the only
DECLARED one, and `ConformancePass.fs:133-135` exists to let the key outrank the content
check.

A second consequence, easy to miss: `PackageUnits.ofOutcome` iterates `manifest.Impl`
(`PackageUnits.fs:32`), so an exempted `.fsi` is **published to consumers via `files` but
never compiled in its own package**. Nothing beyond parsing checks it. That is how the
`printf.fsi` drift in [printf-contract-plan](printf-contract-plan.md) survived.

## The four entries and their gates

`ConformanceTests.fs:554-562` already splits them: the Core pair is target-asymmetric, the
printf pair is symmetric across both targets — a property of the contract rather than of a
backend.

| entry | answer | gated on |
|---|---|---|
| `Vesper.Core` js `compiler-attributes.fsi` | gets a `compiler-attributes.fs` on both targets and pairs | [attribute-representation-plan](attribute-representation-plan.md) |
| `Vesper.Core` js `exceptions.js.fsi` | already representable — the backend derives the repr by climbing `inherit` | [js-exception-identity-plan](js-exception-identity-plan.md) |
| `Vesper.Printf` ×2 `printf-format.fsi` | write the body | [printf-contract-plan](printf-contract-plan.md) |
| `Vesper.Printf` ×2 `printf.fsi` | delete from `files`, or make it load-bearing | [printf-contract-plan](printf-contract-plan.md) |

### `exceptions.js.fsi` needs no body and no key

`JsExternalMembers.exnReprOf` (`JsExternalMembers.fs:106-139`) walks `FrozenBaseType` until it
reaches a type carrying an intrinsic repr. Each roster entry `inherit exn`
(`exceptions.js.fsi:6-33`), and `exn = (# class "Error" #)`
(`prim-types-exn.js.fs:5`), so `new FormatException("x")` already emits `new Error("x")`
via `EmitJs.fs:217-232`. The implementation is not missing; conformance's check is syntactic
and one level deep (`ConformancePass.fs:132-165`) where the backend's is semantic and
transitive.

**Do not generalise this to "reaches an intrinsic repr ⇒ no body owed".** That is sound on JS
and unsound on the CLR, where `compiler-attributes.fsi`'s chain also reaches a repr
(`prim-types-attr.clr.fs`) but the `.fs` is genuinely required — you cannot `newobj` a
TypeDef you never emitted. It only stays safe today because `unpaired` fires solely when no
companion exists. Whatever rule replaces the key must be answered per target.

## The mechanical removal, once all four are clear

Follows `96837bff Remove impl-only as a category from source manifests` exactly.

- **`ReferencedProject`**: the `Manifest.SigOnly` field (`:85`), the `sig-only` key, its
  `coreKeys` entry (`:137`), its `parseManifest` arm (`:195`). Its `sourceInputs` term
  (`:132`) is already dead — `sig-only` is necessarily a subset of `files`, since `enforce`
  reports `UnknownSigOnly` otherwise. An unknown key becomes a parse error, which is what
  makes a stale manifest fail loudly instead of quietly losing its exemption.
- **`ConformancePass`**: `PackageOutcome.SigOnlyExemptions` (`:57`, `:201`) and the
  `declaredSigOnly` short-circuit (`:114`, `:133-135`). `PairOutcome.SigOnly` **stays** — it
  becomes purely the error state it always described, and `enforce:255-257` reports
  `SigWithoutImpl` unconditionally.
- **`ConformanceVerdict`**: `StaleSigOnly` / `UnknownSigOnly` (`Diagnostics.fs:127-129`), the
  V243 mapping (`:150-151`) and their messages (`:170-174`), with the
  `FrozenCodecDiagnostics` wire tags (`:79-82`, `:107-108`) renumbered densely and
  `Cache.CodeVersion` bumped 31 → 32 (`Cache.fs:52`).
- **Manifests**: `Vesper.Core/manifest.js.toml`, `Vesper.Printf/manifest.{clr,js}.toml`, and
  the fixtures under `tmp/buildClosure-tests/`.
- **Tests**: `ConformanceTests.fs` (the exemption arms, `mkOutcome`'s second parameter, and
  the target-asymmetry pin at `:554-597`, whose expected list goes to `[]`),
  `ReferencedProjectTests.fs:672,893`,
  `Codegen.Js.Tests/FrozenCodecRoundTripTests.fs:426-427`,
  `Codegen.Clr.Tests/TestHelpers.fs`.

## What replaces it

Nothing declarative. Three derived routes remain, plus one per-target question for the
attribute case that [attribute-representation-plan](attribute-representation-plan.md) answers
at emit rather than in the manifest.
