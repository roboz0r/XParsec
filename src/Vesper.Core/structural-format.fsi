namespace Vesper

// Front-end symbol contract for the `%A` structural-format interfaces — the
// signature peer of `structural-format.fs`. See that file's header and
// vesper-printf-percentA-plan.md (design-doc P3) for the rationale; the backend's
// `%A` rail binds these via `ClrEnv.eFormatSink` / `eStructuralFormattable`.

/// <summary>The declarative layout surface the synthesised
/// <c>IStructuralFormattable.Format</c> body drives.</summary>
type IFormatSink =
    /// <summary>A literal run that never breaks (labels, punctuation, brackets).</summary>
    abstract member Text: s: string -> unit
    /// <summary>A soft break: a space when its group is flat, a newline + indent when broken.</summary>
    abstract member Line: unit -> unit
    /// <summary>A soft break with no flat form: nothing flat, a newline + indent when broken.</summary>
    abstract member SoftBreak: unit -> unit
    /// <summary>Open a group: its soft breaks all flatten or all break together.</summary>
    abstract member BeginGroup: unit -> unit
    /// <summary>Close the current group.</summary>
    abstract member EndGroup: unit -> unit
    /// <summary>Open an indentation scope: broken lines inside hang at <c>+indent</c>.</summary>
    abstract member BeginNest: indent: int -> unit
    /// <summary>Close the current indentation scope.</summary>
    abstract member EndNest: unit -> unit
    /// <summary>Open a DU application (<c>Case payload</c>): parenthesised in argument position.</summary>
    abstract member BeginApplication: unit -> unit
    /// <summary>Close the current DU application.</summary>
    abstract member EndApplication: unit -> unit
    /// <summary>Recurse into a child in normal position (record field, element, component).</summary>
    abstract member FormatChild: value: obj -> unit
    /// <summary>Recurse into a child in DU-argument position (parenthesised if an application).</summary>
    abstract member FormatArg: value: obj -> unit

/// <summary>Implemented by every compiler-synthesised record / union.</summary>
type IStructuralFormattable =
    /// <summary>Declare this value's structure into <paramref name="sink"/>.</summary>
    abstract member Format: sink: IFormatSink -> unit
