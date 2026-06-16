namespace Vesper

#nowarn "42"

// JS-target intrinsic binding for the exception root (codegen-js-steps.md Step 8).
// On JS there is no `System.Exception` — every exception erases to the native
// `Error`. Harvested in preference to `prim-types-exn.fs` by the target-aware
// repr harvest (`ReferencedProject.buildProviderWith (Some "js")`), the
// intrinsic-repr analogue of the manifest's `inline-bodies-js` override.

type exn = (# "Error" #)
