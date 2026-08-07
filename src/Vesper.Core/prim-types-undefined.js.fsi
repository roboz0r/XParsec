namespace Vesper

/// <summary>The JS <c>undefined</c> absence sentinel — a distinct type from <c>unit</c>,
/// though both project to JS <c>undefined</c> at the value level (<c>unit</c> is inhabited
/// by <c>()</c>). Enters from TS sources, as a member of <c>T | undefined</c>.</summary>
///
/// <category>Basic Types</category>
type undefined = extern

/// The value-level companion of the `undefined` TYPE. Each reference splices the bare
/// `undefined`, so it never appears as an import or a runtime `const`.
[<AutoOpen>]
module Undefined =

    /// The JS <c>undefined</c> absence value — a runtime global with no CLR analog.
    val undefined: undefined
