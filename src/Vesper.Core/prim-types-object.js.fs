namespace Vesper

#nowarn "42"

// JS-target intrinsic binding for `obj` (intrinsic-runtime-type-plan.md).
// Platform (codegen/runtime) face only — the `canon` identity is the `.fsi` name
// `obj`. The universal supertype erases to the JS `unknown` tag (boxing is a no-op on
// JS; in a `.d.ts` an `obj` value is `unknown`). Repointing it here keeps `obj` off the
// "no JS representation" diagnostic — without it `obj` would fall back to the base
// `System.Object` BCL repr, which has no JS analogue.
//
// `objnull` is an abbreviation (`obj | null`), not an `extern`, so it carries no repr.

type obj = (# "unknown" #)
