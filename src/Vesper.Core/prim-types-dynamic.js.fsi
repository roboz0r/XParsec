namespace Vesper

/// <summary>The JS-only opaque <c>dynamic</c> type — the landing point for a TS
/// <c>any</c>-typed member, param or return. It has no statically-known members: the only
/// ways out are the target-typed <c>?</c> operator and an explicit <c>retype</c>.</summary>
///
/// <category>Basic Types</category>
type dynamic = extern
