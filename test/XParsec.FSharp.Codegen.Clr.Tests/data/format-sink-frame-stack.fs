//#include _layout-core.fs
//#include _frame-sem-types.fs
type LSink =
    val Width: int
    val mutable Frames: Frame list
    val mutable SemFrames: SemFrame list
    val mutable LastAppShaped: bool

    new(width: int) =
        let root = Frame(Root, 0)

        {
            Width = width
            Frames = [ root ]
            SemFrames = []
            LastAppShaped = false
        }
    //#include _sink-frame-plumbing.fs
    member private this.Dispatch(value: obj) =
        this.LastAppShaped <- false

        match value with
        | :? Vesper.IStructuralFormattable as s -> s.Format(this :> Vesper.IFormatSink)
        | null -> this.Add(LText "null")
        | _ -> this.Add(LText(value.ToString()))
    //#include _sink-finish-protocol.fs
let rec emitElems (sink: Vesper.IFormatSink) (xs: int list) (first: bool) : unit =
    match xs with
    | [] -> ()
    | h :: t ->
        if not first then
            sink.Text ";"
            sink.Line()

        sink.Child(box h)
        emitElems sink t false

type MyList(xs: int list) =
    interface Vesper.IStructuralFormattable with
        member this.Format(sink: Vesper.IFormatSink) =
            sink.BeginGroup()
            sink.Text "["
            sink.BeginNest 2
            sink.SoftBreak()
            emitElems sink xs true
            sink.EndNest()
            sink.SoftBreak()
            sink.Text "]"
            sink.EndGroup()

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

let print (v: obj) (width: int) : string =
    let sink = LSink(width)
    (sink :> Vesper.IFormatSink).Child(v)
    sink.Finish()

printfn "%s" (print (MyList([ 1; 2; 3 ]) :> obj) 80)
printfn "%s" (print (MyList([ 1; 2; 3 ]) :> obj) 5)
printfn "%s" (print (MyOpt(box 1, true) :> obj) 80)
printfn "%s" (print (MyOpt((MyOpt(box 1, true) :> obj), true) :> obj) 80)
