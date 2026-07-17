namespace Vesper

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
