open System
open System.Collections
open System.Globalization
open System.Runtime.CompilerServices
//#include _layout-core.fs
let rec hasDot (s: string) (i: int) : bool =
    if i >= s.Length then
        false
    else
        let c = s.[i]

        if c = '.' || c = 'e' || c = 'E' then
            true
        else
            hasDot s (i + 1)

let fixFloat (s: string) (finite: bool) (suffix: string) : string =
    if not finite then
        if s = "NaN" then "nan" + suffix
        elif s = "Infinity" then "infinity" + suffix
        elif s = "-Infinity" then "-infinity" + suffix
        else s
    elif hasDot s 0 then
        s + suffix
    else
        s + ".0" + suffix

let formatPrimitive (value: obj) : string =
    match value with
    | :? double as d -> fixFloat (d.ToString(null, CultureInfo.InvariantCulture)) (Double.IsFinite d) ""
    | :? single as f -> fixFloat (f.ToString(null, CultureInfo.InvariantCulture)) (Single.IsFinite f) "f"
    | _ ->
        let s =
            match value with
            | :? IFormattable as fmt -> fmt.ToString(null, CultureInfo.InvariantCulture)
            | _ -> value.ToString()

        let suffix =
            match value with
            | :? sbyte -> "y"
            | :? byte -> "uy"
            | :? int16 -> "s"
            | :? uint16 -> "us"
            | :? uint32 -> "u"
            | :? int64 -> "L"
            | :? uint64 -> "UL"
            | :? decimal -> "M"
            | _ -> ""

        s + suffix

let appendEscaped (acc: string) (c: char) (quote: char) : string =
    if c = '\\' then acc + "\\\\"
    elif c = '\n' then acc + "\\n"
    elif c = '\r' then acc + "\\r"
    elif c = '\t' then acc + "\\t"
    elif c = quote then acc + "\\" + c.ToString()
    else acc + c.ToString()

let rec escapeInto (acc: string) (s: string) (i: int) (quote: char) : string =
    if i >= s.Length then
        acc
    else
        escapeInto (appendEscaped acc s.[i] quote) s (i + 1) quote

let quoteString (s: string) : string = "\"" + escapeInto "" s 0 '\"' + "\""
let quoteChar (c: char) : string = "'" + appendEscaped "" c '\'' + "'"
//#include _frame-sem-types.fs
type LSink =
    val Width: int
    val mutable Size: int
    val mutable Depth: int
    val mutable Frames: Frame list
    val mutable SemFrames: SemFrame list
    val mutable LastAppShaped: bool

    new(width: int, size: int) =
        let root = Frame(Root, 0)

        {
            Width = width
            Size = size
            Depth = 0
            Frames = [ root ]
            SemFrames = []
            LastAppShaped = false
        }
    //#include _sink-frame-plumbing.fs
    member private this.FormatTuple(t: ITuple) =
        this.Push(Frame(Group, 0))
        this.Add(LText "(")
        this.Push(Frame(Nest, 1))

        for i in 0 .. t.Length - 1 do
            if i > 0 then
                this.Add(LText ",")
                this.Add(LLine " ")

            this.Dispatch(t.[i])

        this.PopWrap(Nest)
        this.Add(LText ")")
        this.PopWrap(Group)
        this.LastAppShaped <- false

    member private this.FormatEnumerable(xs: IEnumerable) =
        this.Push(Frame(Group, 0))
        this.Add(LText "[")
        this.Push(Frame(Nest, 2))
        this.Add(LLine "")
        let mutable i = 0
        let mutable truncated = false

        for item in xs do
            if not truncated then
                if i > 0 then
                    this.Add(LText ";")
                    this.Add(LLine " ")

                if i >= 100 || this.Size <= 0 then
                    this.Add(LText "...")
                    truncated <- true
                else
                    this.Dispatch(item)
                    i <- i + 1

        this.PopWrap(Nest)
        this.Add(LLine "")
        this.Add(LText "]")
        this.PopWrap(Group)
        this.LastAppShaped <- false

    member private this.Dispatch(value: obj) =
        this.LastAppShaped <- false

        match value with
        | null -> this.Add(LText "null")
        | _ ->
            if this.Depth >= 100 then
                this.Add(LText "...")
            elif this.Size <= 0 then
                this.Add(LText "...")
            else
                this.Depth <- this.Depth + 1
                this.DispatchInner(value)
                this.Depth <- this.Depth - 1

    member private this.DispatchInner(value: obj) =
        match value with
        | :? Vesper.IStructuralFormattable as structural -> structural.Format(this :> Vesper.IFormatSink)
        | :? string as s ->
            this.Size <- this.Size - 1
            this.Add(LText(quoteString s))
        | :? char as c ->
            this.Size <- this.Size - 1
            this.Add(LText(quoteChar c))
        | :? bool as b ->
            this.Size <- this.Size - 1
            this.Add(LText(if b then "true" else "false"))
        | :? ITuple as t -> this.FormatTuple t
        | :? IFormattable ->
            this.Size <- this.Size - 1
            this.Add(LText(formatPrimitive value))
        | :? IEnumerable as xs -> this.FormatEnumerable xs
        | _ ->
            this.Size <- this.Size - 1
            this.Add(LText(value.ToString()))
    //#include _sink-finish-protocol.fs
type MyOpt(payload: obj, isSome: bool) =
    interface Vesper.IStructuralFormattable with
        member this.Format(sink: Vesper.IFormatSink) =
            if isSome then
                sink.BeginCase "Some"
                sink.Child payload
                sink.EndCase()
            else
                sink.BeginCase "None"
                sink.EndCase()

let print (v: obj) (width: int) (size: int) : string =
    let sink = LSink(width, size)
    (sink :> Vesper.IFormatSink).Child(v)
    sink.Finish()

printfn "%s" (print (box 42) 80 10000)
printfn "%s" (print (box true) 80 10000)
printfn "%s" (print (box 3.0) 80 10000)
printfn "%s" (print (box "hi") 80 10000)
printfn "%s" (print (box 'c') 80 10000)
printfn "%s" (print (box 5L) 80 10000)
printfn "%s" (print (box 1.5M) 80 10000)
printfn "%s" (print (box (1, "a")) 80 10000)
printfn "%s" (print (box [| 1; 2; 3 |]) 80 10000)
printfn "%s" (print ((MyOpt((MyOpt(box 1, true) :> obj), true)) :> obj) 80 10000)
printfn "%s" (print (box [| 1; 2; 3; 4; 5 |]) 80 2)
