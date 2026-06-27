namespace Vesper

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

// Impl (`.fs`) side: per-target CLR identity for each language-capability anchor.
// This is the reconciliation source — `harvestIntrinsicReprsInto` collects these
// `(# "<repr>" #)` reprs into the `{ platform -> canon }` reverse map, so a type
// whose metadata surfaces `System.IDisposable` reconciles to the canonical
// `disposable` (the `exn === System.Exception` mechanism, generalized to
// interfaces). The repr strings must match what the metadata provider's
// `FullName` / `instantiateInterfaces` surfaces BYTE-FOR-BYTE — including the
// backtick arity suffix on the generic interfaces — or reconciliation silently
// misses. Declared generic to mirror the BCL reality; the harvest keys off the
// short name only (`equatable`), so the typar list does not affect the canon.
//
// Iteration (`seq<'T>` / `IEnumerable<'T>`) is resolved off the existing `seq`
// abbreviation, not anchored here — see `capabilities.fsi`.

type disposable = (# "System.IDisposable" #)
type equatable<'T> = (# "System.IEquatable`1" #)
type comparable<'T> = (# "System.IComparable`1" #)
