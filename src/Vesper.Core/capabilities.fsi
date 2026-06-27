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
// Iteration (`for … in`) is intentionally ABSENT: it already has a language type,
// `seq<'T>` (`Vesper.List/list.fsi`, an abbreviation for `IEnumerable<'T>`), so the
// resolver reads the enumerable identity off that abbreviation rather than a
// redundant anchor here.
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
