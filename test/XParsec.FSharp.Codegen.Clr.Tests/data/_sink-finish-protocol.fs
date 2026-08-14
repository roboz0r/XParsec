member this.Finish() : string =
    match this.Frames with
    | [ root ] ->
        let kids = revOnto root.Kids []
        let docRoot =
            match kids with
            | [ single ] -> single
            | _ -> LCat kids
        layout docRoot this.Width
    | _ -> failwith "Vesper.LSink: unbalanced layout scopes at Finish."
interface Vesper.IFormatSink with
    member this.Text(s: string) = this.Add(LText s)
    member this.Line() = this.Add(LLine " ")
    member this.SoftBreak() = this.Add(LLine "")
    member this.BeginGroup() = this.Push(Frame(Group, 0))
    member this.EndGroup() = this.PopWrap(Group)
    member this.BeginNest(indent: int) = this.Push(Frame(Nest, indent))
    member this.EndNest() = this.PopWrap(Nest)
    member this.BeginRecord() = ()
    member this.Field(name: string) = ()
    member this.EndRecord() = ()
    member this.BeginCase(name: string) =
        this.SemFrames <- SemFrame(name) :: this.SemFrames
        this.Push(Frame(CaseCollect, 0))
    member this.Sequence(items: seq<obj>) =
        this.Push(Frame(Group, 0))
        this.Add(LText "[")
        for x in items do
            this.Dispatch(x)
        this.Add(LText "]")
        this.PopWrap(Group)
    member this.Child(value: obj) =
        this.Dispatch(value)
        match this.SemFrames with
        | sf :: _ ->
            sf.Count <- sf.Count + 1
            sf.ChildAppShaped <- this.LastAppShaped
        | [] -> ()
    member this.EndCase() =
        match this.SemFrames with
        | cf :: rest ->
            this.SemFrames <- rest
            let kids =
                match this.Frames with
                | f :: fr ->
                    this.Frames <- fr
                    revOnto f.Kids []
                | [] -> []
            let caseDoc =
                if cf.Count = 0 then LText cf.Name
                else
                    let child =
                        match kids with
                        | [ single ] -> single
                        | _ -> LCat kids
                    let payload =
                        if cf.ChildAppShaped then LGroup(child, true) else child
                    LGroup(LCat [ LText(cf.Name + " "); payload ], false)
            this.Add(caseDoc)
            this.LastAppShaped <- cf.Count >= 1
        | [] -> ()
