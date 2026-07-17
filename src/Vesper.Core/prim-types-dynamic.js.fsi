namespace Vesper

/// <summary>The JS-only opaque <c>dynamic</c> type — the disciplined F# <c>any</c>.
/// It is the landing point for a TS <c>any</c>-typed member/param/return. It has no
/// statically-known members; the only way to project out of it is the dynamic-access
/// operator <c>?</c> (target-typed) or an explicit <c>retype</c> cast. It enters only
/// from a TS-<c>any</c>-typed value or the <c>dynamic</c> conversion function.</summary>
///
/// <category>Basic Types</category>
type dynamic = extern
