namespace Vesper

// JS-only absence-sentinel intrinsic type. Appended to the contract for the JS
// target ONLY (`files-js`): it has no CLR analog. `undefined` is `extern` — "the JS
// target provides this; there is no Vesper representation here" — paired with a
// `(# "undefined" #)` intrinsic in the `.fs` companion carrying its own JS runtime tag.
//
// NOTE: the sibling absence sentinel `null` is NOT declared here — `null` is a reserved
// keyword (`Token.KWNull`), so `type null = extern` cannot parse. It needs no declaration
// anyway: nothing maps to the reverse-canon key `"null"`, so it is ALREADY a distinct
// identity from `unit` (its value keyword path `Type.Null`/`Expr.Null` is untouched).

/// <summary>The JS <c>undefined</c> absence sentinel — a distinct type identity
/// from <c>unit</c>. Both project to JS <c>undefined</c> at the value level, but
/// that is a backend repr coincidence, not type identity (<c>unit</c> is inhabited
/// by <c>()</c>; <c>undefined</c> is an absence sentinel). Enters only from TS
/// sources, as a member of a nullable union (<c>T | undefined</c>).</summary>
///
/// <category>Basic Types</category>
type undefined = extern

/// The value-level companion of the `undefined` TYPE — the single honest source of
/// the JS absence value. `[<AutoOpen>]` so it is usable unqualified exactly like the
/// type. Bound to the zero-operand `(# "undefined" #)` intrinsic in the `.js.fs`
/// companion; the JS backend inlines it (no lowered definition, each reference
/// splices bare `undefined`), so it never appears as an import or a runtime `const`.
[<AutoOpen>]
module Undefined =

    /// The JS <c>undefined</c> absence value — a runtime global with no CLR analog.
    val undefined: undefined
