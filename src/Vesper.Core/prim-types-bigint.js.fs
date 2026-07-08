namespace Vesper

#nowarn "42"

// JS-target intrinsic binding for the arbitrary-precision integer primitive.
// Platform (codegen/runtime) face only — the `canon` identity stays on the base
// `prim-types-bigint.fs` (`System.Numerics.BigInteger`), so `bigint` keeps its
// identity while projecting to the JS `bigint` primitive. Discovered by the
// `<base>.<target>.fs` sibling convention (`ReferencedProject.targetOverrideFs`),
// so it needs no separate `manifest.toml` entry.

type bigint = (# "bigint" #)
