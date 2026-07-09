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
// Iteration (`seq` / `enumerator`) is anchored in the `Vesper.Collections` namespace
// below — its reprs are the BCL enumerable/enumerator interfaces the CLR backend
// reconciles + co-slot-synthesizes to (see `capabilities.fsi`).

type disposable = (# "System.IDisposable" #)
type equatable<'T> = (# "System.IEquatable`1" #)
type comparable<'T> = (# "System.IComparable`1" #)

namespace Vesper.Collections

#nowarn "42"

type enumerator<'T> = (# "System.Collections.Generic.IEnumerator`1" #)
type seq<'T> = (# "System.Collections.Generic.IEnumerable`1" #)
