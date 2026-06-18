namespace Vesper

#nowarn "42"

// JS-target intrinsic bindings for the core primitives.
// The platform (codegen/runtime) face only — `canon` (the unifier's identity key) is the
// `.fsi` name itself (`int`/`bool`/`unit`), so each type keeps its distinct identity while
// projecting to a JS-native tag here. This `<base>.js.fs` override is harvested by
// `ReferencedProject.buildProviderWith` (the `targetOverrideFs (Some "js")` companion) into
// the `platform` face; the base `prim-types-min.fs` only marks these as intrinsics
// (`IntrinsicBaseReprs`). `unit` projects to JS `undefined` (the value a unit `Const` already
// emits, and the `typeof` tag — like `number`/`boolean`/`string`).
//
// A SCALAR primitive omitted from this override gets `platform = None` and is rejected by
// `SemanticAnalysis.PlatformTypes` as "no JS representation" (that is how `decimal`/`nativeint`
// are caught) — it does NOT silently fall back to the BCL base repr. The generic structural
// constructors (`'T []`, `byref`) are deliberately NOT repointed: they too get `platform =
// None`, but the back end lowers them structurally (`FreezeExpr` emits `FTConst("[]")`, a JS
// array — no repr string needed), and `PlatformTypes` skips them on `arity ≥ 1`, so they are
// representable by construction rather than via a tag here.

type int = (# "number" #)
type bool = (# "boolean" #)
type unit = (# "undefined" #)
