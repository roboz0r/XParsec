member private this.Add(d: LDoc) =
    match this.Frames with
    | top :: _ -> top.Kids <- d :: top.Kids
    | [] -> ()
member private this.Push(f: Frame) = this.Frames <- f :: this.Frames
member private this.PopWrap(expected: FrameKind) =
    match this.Frames with
    | f :: rest ->
        this.Frames <- rest
        let kids = revOnto f.Kids []
        let inner =
            match kids with
            | [ single ] -> single
            | _ -> LCat kids
        let wrapped =
            match f.Kind with
            | Group -> LGroup(inner, false)
            | Nest -> LNest(f.NestIndent, inner)
            | Root -> inner
            | CaseCollect -> inner
        this.Add(wrapped)
    | [] -> ()
