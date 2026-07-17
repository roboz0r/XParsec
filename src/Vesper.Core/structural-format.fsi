namespace Vesper

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
    /// <summary>Open a record: the fields follow as <c>Field name</c> / <c>Child value</c> pairs.</summary>
    abstract member BeginRecord: unit -> unit
    /// <summary>Mark the next field's label; its value arrives in the following <c>Child</c>.</summary>
    abstract member Field: name: string -> unit
    /// <summary>Close the current record (renders <c>{ }</c> if it had no fields).</summary>
    abstract member EndRecord: unit -> unit
    /// <summary>Open a union case named <paramref name="name"/>; its payloads follow as <c>Child</c>s.</summary>
    abstract member BeginCase: name: string -> unit
    /// <summary>Close the current union case (arity decided from the observed <c>Child</c> count).</summary>
    abstract member EndCase: unit -> unit
    /// <summary>Recurse into a record field / union payload child; the enclosing frame fixes its position.</summary>
    abstract member Child: value: obj -> unit

/// <summary>Implemented by every compiler-synthesised record / union.</summary>
type IStructuralFormattable =
    /// <summary>Declare this value's structure into <paramref name="sink"/>.</summary>
    abstract member Format: sink: IFormatSink -> unit
