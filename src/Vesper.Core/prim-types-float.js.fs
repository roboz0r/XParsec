namespace Vesper

#nowarn "42"

// JS-target intrinsic bindings for the floating-point primitives
// (intrinsic-runtime-type-plan.md). Platform (codegen/runtime) face only — the `canon`
// identity stays on the base `prim-types-float.fs` (`System.Single`/`System.Double`),
// so `float32` and `float` keep distinct identities while both project to JS `number`.

type float32 = (# "number" #)
type float = (# "number" #)
