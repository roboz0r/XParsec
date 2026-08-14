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
    /// <summary>Open a record: the fields follow as <c>Field name</c> / <c>Child value</c> pairs.
    /// The layout policy (<c>{ F = ·; G = · }</c>, <c>+2</c> hang) lives in the sink, not the
    /// caller.</summary>
    abstract member BeginRecord: unit -> unit
    /// <summary>Mark the next field's label; its value arrives in the following <c>Child</c>.</summary>
    abstract member Field: name: string -> unit
    /// <summary>Close the current record (renders <c>{ }</c> if it had no fields).</summary>
    abstract member EndRecord: unit -> unit
    /// <summary>Open a union case named <paramref name="name"/>; its payloads follow as <c>Child</c>s.</summary>
    abstract member BeginCase: name: string -> unit
    /// <summary>Close the current union case; the arity arm follows from the observed
    /// <c>Child</c> count. A single payload is parenthesised iff it is itself an
    /// application-shaped case (<c>Some (Some 3)</c> but not <c>Some 3</c> / <c>Some [1; 2]</c>).</summary>
    abstract member EndCase: unit -> unit
    /// <summary>Render a sequence as <c>[a; b; c]</c> (empty: <c>[]</c>). Enumerated ONCE, and
    /// only as far as the <c>%.NA</c> node budget allows: truncation is the sink's call, so
    /// nothing past its cut is pulled.</summary>
    abstract member Sequence: items: seq<obj> -> unit
    /// <summary>Recurse into a record field / union payload child; the enclosing frame fixes its position.</summary>
    abstract member Child: value: obj -> unit

/// <summary>Implemented by every compiler-synthesised record / union.</summary>
type IStructuralFormattable =
    /// <summary>Declare this value's structure into <paramref name="sink"/>.</summary>
    abstract member Format: sink: IFormatSink -> unit
