namespace Vesper

#nowarn "42"

// JS-target intrinsic bindings for the text primitives.
// Platform (codegen/runtime) face only — the `canon` identity stays on the base
// `prim-types-string.fs` (`System.Char`/`System.String`), so `char` and `string` keep
// distinct identities. JS has no character type, so `char` projects to the same
// `string` tag as `string` (a one-code-unit string).

type char = (# "string" #)
type string = (# "string" #)
