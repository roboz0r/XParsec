namespace Vesper

// Language-capability anchors. These extern types name the per-target identities
// the front end resolves to lower / validate F#'s capability grammar for the
// capabilities that have NO existing language type: `use` (disposable),
// `[<CustomEquality>]` (equatable), `[<CustomComparison>]` (comparable). Declared
// `extern` (no repr) so the per-target `.fs` companion binds each to its platform
// identity and the provider surfaces an `Intrinsic` shape the semantic-analysis
// passes resolve (cf. `prim-types-exn.fsi`'s `type exn = extern`). The generic
// ones carry the arity the BCL interface name does (``IEquatable`1`` etc.);
// `disposable` is non-generic.
//
// Iteration (`for … in`) is NOT an `extern` anchor — it already has a language type,
// `seq<'T>` (a transparent abbreviation for `IEnumerable<'T>`), off whose resolved head
// the resolver reads the enumerable identity. `seq` is declared at the BOTTOM of this
// file (moved here from `Vesper.List/list.fsi`): reading it off the abbreviation while
// the abbreviation lived in `Vesper.List` was CIRCULAR — `Vesper.List` itself (whose
// `List` union implements `seq`) can't resolve `seq` from its own not-yet-built
// contract. `Vesper.Core` is always a dependency, so anchoring `seq` here resolves it
// even while building `List`. `seq` keeps its `Vesper.Collections` namespace, so the
// resolver lookup and every bare `seq<'T>` reference are unchanged.
//
// No `extern with` member surface yet — CLR identity resolution needs only the
// `Intrinsic` platform face (members come from metadata); the member surface is
// JS-only work, deferred with the Intrinsic-carrying-members migration.

/// <summary>The disposal capability — anchors `use` (and `for … in` finally). On
/// the CLI it is <see cref="T:System.IDisposable"/>.</summary>
///
/// <category>Language Capabilities</category>
type disposable = extern

/// <summary>The equality capability — anchors `[<CustomEquality>]` conformance.
/// On the CLI it is <see cref="T:System.IEquatable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
type equatable<'T> = extern

/// <summary>The comparison capability — anchors `[<CustomComparison>]`
/// conformance. On the CLI it is <see cref="T:System.IComparable`1"/>.</summary>
///
/// <category>Language Capabilities</category>
type comparable<'T> = extern

namespace Vesper.Collections

open System.Collections.Generic

/// <summary>An abbreviation for the CLI type <see cref="T:System.Collections.Generic.IEnumerable`1"/></summary>
///
/// <remarks>
///  The iteration capability — `for … in` resolves the enumerable identity off this
///  abbreviation's resolved head (`IEnumerable`1`). Declared in `Vesper.Core` (not
///  `Vesper.List`) so `Vesper.List` — whose `List` union implements `seq` — can resolve
///  it from a dependency rather than its own not-yet-built contract.
///</remarks>
type seq<'T> = IEnumerable<'T>
