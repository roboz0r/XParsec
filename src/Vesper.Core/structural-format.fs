namespace Vesper

type IFormatSink =
    abstract member Text: s: string -> unit
    abstract member Line: unit -> unit
    abstract member SoftBreak: unit -> unit
    abstract member BeginGroup: unit -> unit
    abstract member EndGroup: unit -> unit
    abstract member BeginNest: indent: int -> unit
    abstract member EndNest: unit -> unit
    abstract member BeginRecord: unit -> unit
    abstract member Field: name: string -> unit
    abstract member EndRecord: unit -> unit
    abstract member BeginCase: name: string -> unit
    abstract member EndCase: unit -> unit
    abstract member Child: value: obj -> unit

type IStructuralFormattable =
    abstract member Format: sink: IFormatSink -> unit
