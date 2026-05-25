namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic bindings for the floating-point primitives.
// Retarget a primitive here in one line; aliases mirror the .fsi.

type float32 = (# "System.Single" #)
type float = (# "System.Double" #)
type single = float32
type double = float
