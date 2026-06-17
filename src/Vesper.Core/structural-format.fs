namespace Vesper

// The `%A` structural-format interfaces. The compiler synthesises `IStructuralFormattable`
// on every record/union; the `IFormatSink` layout engine lives in `Vesper.Printf`.
// Both interfaces live in `Vesper.Core` so a record-bearing program links only Core.

/// <summary>The declarative layout surface the synthesised
/// <c>IStructuralFormattable.Format</c> body drives. Calls record tokens that the
/// engine lays out group-by-group; <c>FormatChild</c>/<c>FormatArg</c> take
/// <c>obj</c> (value children box once — fine on the <c>%A</c> heavy path).</summary>
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

/// <summary>Implemented by every compiler-synthesised record / union. The
/// synthesised <c>Format</c> body declares the type's structure into the
/// <paramref name="sink"/>.</summary>
type IStructuralFormattable =
    /// <summary>Declare this value's structure into <paramref name="sink"/>.</summary>
    abstract member Format: sink: IFormatSink -> unit
