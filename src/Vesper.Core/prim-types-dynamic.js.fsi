namespace Vesper

// JS-only opaque dynamic intrinsic type — the F#-side landing point for TS `any`.
// Appended to the contract for the JS target ONLY (`files-js`): it has no CLR analog.
// Modelled exactly like the sibling `undefined` intrinsic: `dynamic` is `extern` —
// "the JS target provides this; there is no Vesper representation here" — paired with a
// `(# "any" #)` intrinsic in the `.fs` companion carrying its own JS runtime tag.
//
// `dynamic` carries NO special unifier behaviour: it unifies with itself by name (like
// `int`/`string`) and with nothing else, and has ZERO assignability edges — you cannot
// silently enter or leave it. You enter via the `dynamic` conversion (or a `retype`
// cast), and the only member access it admits is the `?` operator (`x?foo`), never
// dotted `.member`. See `ops-dynamic.js.fsi` and `docs/dynamic-typing-design.md`.

/// <summary>The JS-only opaque <c>dynamic</c> type — the disciplined F# <c>any</c>.
/// It is the landing point for a TS <c>any</c>-typed member/param/return. It has no
/// statically-known members; the only way to project out of it is the dynamic-access
/// operator <c>?</c> (target-typed) or an explicit <c>retype</c> cast. It enters only
/// from a TS-<c>any</c>-typed value or the <c>dynamic</c> conversion function.</summary>
///
/// <category>Basic Types</category>
type dynamic = extern
