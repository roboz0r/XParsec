namespace Vesper

/// <summary>The JS <c>undefined</c> absence sentinel — a distinct type from <c>unit</c>,
/// though both project to JS <c>undefined</c> at the value level (<c>unit</c> is inhabited
/// by <c>()</c>). Enters from TS sources, as a member of <c>T | undefined</c>.</summary>
///
/// <category>Basic Types</category>
type undefined = extern

[<AutoOpen>]
module Undefined =

    /// The JS <c>undefined</c> absence value. Each reference splices the bare
    /// <c>undefined</c>: no import, and no emitted <c>const</c>.
    val undefined: undefined
