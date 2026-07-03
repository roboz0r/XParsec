namespace Vesper

open System
open System.Collections
open System.Globalization
open System.Runtime.CompilerServices

// StructuralFormatBaseline.fs — a FROZEN copy of the `%A` structural engine as it
// stood BEFORE the pooled-buffer rewrite (the string-concatenation render pass).
//
// This file is NOT fsc-compiled. It is read as TEXT and compiled through this repo's
// own Codegen.Clr backend (alongside the live `src/Vesper.Printf/structural-printer.fs`)
// by `StructuralFormatBenchmarks`, so the before/after of the pooled-buffer rewrite is
// measured on the *emitted IL* of both engines — not the fsc rendering. It defines the
// same `Vesper.StructuralPrinter` as the live engine and binds the same Core-owned
// `Vesper.IStructuralFormattable` / `Vesper.IFormatSink` interfaces; the two engines are
// compiled into separate assemblies / load contexts, so the duplicate type names never
// collide. Output is byte-identical to the live engine (the golden/differential suite
// guards that); only the render strategy (O(n^2) `string + string` here vs the O(n)
// pooled `char[]` buffer in the live engine) differs.

type Doc =
    | DocText of string
    | DocLine of string
    | DocCat of Doc list
    | DocNest of int * Doc
    | DocGroup of Doc * bool

type RenderResult = { Txt: string; Col: int }

module internal DocLayout =

    let rec flatWidth (d: Doc) : int =
        match d with
        | DocText s -> s.Length
        | DocLine flat -> flat.Length
        | DocCat kids -> catWidth kids
        | DocNest(_, inner) -> flatWidth inner
        | DocGroup(inner, parens) -> flatWidth inner + (if parens then 2 else 0)

    and catWidth (kids: Doc list) : int =
        match kids with
        | [] -> 0
        | k :: rest -> flatWidth k + catWidth rest

    let rec spaces (n: int) : string =
        if n <= 0 then "" else " " + spaces (n - 1)

    let rec render (d: Doc) (indent: int) (broken: bool) (col: int) (width: int) : RenderResult =
        match d with
        | DocText s -> { Txt = s; Col = col + s.Length }
        | DocLine flat ->
            if broken then
                {
                    Txt = "\n" + spaces indent
                    Col = indent
                }
            else
                { Txt = flat; Col = col + flat.Length }
        | DocNest(i, inner) -> render inner (indent + i) broken col width
        | DocCat kids -> renderCat kids indent broken col width
        | DocGroup(inner, parens) ->
            let openCol = if parens then col + 1 else col
            let groupBroken = width <> 0 && openCol + flatWidth inner > width
            let r = render inner indent groupBroken openCol width

            if parens then
                {
                    Txt = "(" + r.Txt + ")"
                    Col = r.Col + 1
                }
            else
                r

    and renderCat (kids: Doc list) (indent: int) (broken: bool) (col: int) (width: int) : RenderResult =
        match kids with
        | [] -> { Txt = ""; Col = col }
        | k :: rest ->
            let r1 = render k indent broken col width
            let r2 = renderCat rest indent broken r1.Col width
            { Txt = r1.Txt + r2.Txt; Col = r2.Col }

    let layout (d: Doc) (width: int) : string =
        let r = render (DocGroup(d, false)) 0 false 0 width
        r.Txt

    let rec revOnto (xs: Doc list) (acc: Doc list) : Doc list =
        match xs with
        | [] -> acc
        | h :: t -> revOnto t (h :: acc)

    let rec interleaveComponents (xs: Doc list) (firstDone: bool) : Doc list =
        match xs with
        | [] -> []
        | k :: rest ->
            if firstDone then
                DocText "," :: DocLine " " :: k :: interleaveComponents rest true
            else
                k :: interleaveComponents rest true

    let buildTupleDoc (kids: Doc list) : Doc =
        DocGroup(
            DocCat
                [
                    DocText "("
                    DocNest(1, DocCat(interleaveComponents kids false))
                    DocText ")"
                ],
            false
        )

    let rec containsRef (xs: obj list) (v: obj) : bool =
        match xs with
        | [] -> false
        | h :: t ->
            if Object.ReferenceEquals(h, v) then
                true
            else
                containsRef t v

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
        if value :? double then
            let d = value :?> double
            fixFloat (d.ToString(null, CultureInfo.InvariantCulture)) (Double.IsFinite d) ""
        elif value :? single then
            let f = value :?> single
            fixFloat (f.ToString(null, CultureInfo.InvariantCulture)) (Single.IsFinite f) "f"
        else
            let s =
                if value :? IFormattable then
                    (value :?> IFormattable).ToString(null, CultureInfo.InvariantCulture)
                else
                    value.ToString()

            let suffix =
                if value :? sbyte then "y"
                elif value :? byte then "uy"
                elif value :? int16 then "s"
                elif value :? uint16 then "us"
                elif value :? uint32 then "u"
                elif value :? int64 then "L"
                elif value :? uint64 then "UL"
                elif value :? IntPtr then "n"
                elif value :? UIntPtr then "un"
                elif value :? decimal then "M"
                else ""

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

    let quoteString (s: string) : string = "\"" + escapeInto "" s 0 '"' + "\""

    let quoteChar (c: char) : string = "'" + appendEscaped "" c '\'' + "'"

type FrameKind =
    | Root
    | Group
    | Nest
    | Collect

type Frame =
    {
        Kind: FrameKind
        NestIndent: int
        mutable Kids: Doc list
    }

type SemFrame =
    {
        IsCase: bool
        Name: string
        mutable Count: int
        mutable ChildAppShaped: bool
    }

type RuntimeFormatState =
    val Width: int
    val mutable Size: int
    val mutable Depth: int
    val mutable Frames: Frame list
    val mutable SemFrames: SemFrame list
    val mutable LastAppShaped: bool
    val mutable Visited: obj list

    new(width: int, printSize: int) =
        let root =
            {
                Kind = Root
                NestIndent = 0
                Kids = []
            }

        {
            Width = width
            Size = printSize
            Depth = 0
            Frames = [ root ]
            SemFrames = []
            LastAppShaped = false
            Visited = []
        }

    member private this.Add(d: Doc) =
        match this.Frames with
        | top :: _ -> top.Kids <- d :: top.Kids
        | [] -> ()

    member private this.Push(f: Frame) = this.Frames <- f :: this.Frames

    member private this.PushKind(kind: FrameKind, indent: int) =
        this.Push(
            {
                Kind = kind
                NestIndent = indent
                Kids = []
            }
        )

    member private this.PopWrap(expected: FrameKind) =
        match this.Frames with
        | f :: rest ->
            this.Frames <- rest
            let kids = DocLayout.revOnto f.Kids []

            let inner =
                match kids with
                | [ single ] -> single
                | _ -> DocCat kids

            let wrapped =
                match f.Kind with
                | Group -> DocGroup(inner, false)
                | Nest -> DocNest(f.NestIndent, inner)
                | Root
                | Collect -> inner

            this.Add(wrapped)
        | [] -> ()

    // ---- the semantic protocol (mirror of Vesper.Printf/structural-printer.fs) ----

    member private this.BeginRecordP() =
        this.SemFrames <-
            {
                IsCase = false
                Name = ""
                Count = 0
                ChildAppShaped = false
            }
            :: this.SemFrames

        this.PushKind(Group, 0)

    member private this.FieldP(name: string) =
        match this.SemFrames with
        | rf :: _ ->
            if rf.Count = 0 then
                this.Add(DocText("{ " + name + " = "))
                this.PushKind(Nest, 2)
            else
                this.Add(DocText ";")
                this.Add(DocLine " ")
                this.Add(DocText(name + " = "))

            rf.Count <- rf.Count + 1
        | [] -> ()

    member private this.EndRecordP() =
        match this.SemFrames with
        | rf :: rest ->
            this.SemFrames <- rest

            if rf.Count = 0 then
                this.Add(DocText "{ }")
                this.PopWrap(Group)
            else
                this.PopWrap(Nest)
                this.Add(DocText " }")
                this.PopWrap(Group)

            this.LastAppShaped <- false
        | [] -> ()

    member private this.BeginCaseP(name: string) =
        this.SemFrames <-
            {
                IsCase = true
                Name = name
                Count = 0
                ChildAppShaped = false
            }
            :: this.SemFrames

        this.PushKind(Collect, 0)

    member private this.ChildP(value: obj) =
        this.Dispatch(value)

        match this.SemFrames with
        | sf :: _ when sf.IsCase ->
            sf.Count <- sf.Count + 1
            sf.ChildAppShaped <- this.LastAppShaped
        | _ -> ()

    member private this.EndCaseP() =
        match this.SemFrames with
        | cf :: rest ->
            this.SemFrames <- rest

            let kids =
                match this.Frames with
                | f :: fr ->
                    this.Frames <- fr
                    DocLayout.revOnto f.Kids []
                | [] -> []

            let caseDoc =
                match cf.Count with
                | 0 -> DocText cf.Name
                | 1 ->
                    let child =
                        match kids with
                        | [ single ] -> single
                        | _ -> DocCat kids

                    let payload = if cf.ChildAppShaped then DocGroup(child, true) else child

                    DocGroup(DocCat [ DocText(cf.Name + " "); payload ], false)
                | _ -> DocGroup(DocCat [ DocText(cf.Name + " "); DocLayout.buildTupleDoc kids ], false)

            this.Add(caseDoc)
            this.LastAppShaped <- cf.Count >= 1
        | [] -> ()

    member private this.FormatTuple(t: ITuple) =
        this.PushKind(Collect, 0)

        for i in 0 .. t.Length - 1 do
            this.Dispatch(t.[i])

        let kids =
            match this.Frames with
            | f :: fr ->
                this.Frames <- fr
                DocLayout.revOnto f.Kids []
            | [] -> []

        this.Add(DocLayout.buildTupleDoc kids)
        this.LastAppShaped <- false

    member private this.FormatEnumerable(xs: IEnumerable) =
        this.PushKind(Group, 0)
        this.Add(DocText "[")
        this.PushKind(Nest, 2)
        this.Add(DocLine "")
        let mutable i = 0
        let mutable truncated = false

        for item in xs do
            if not truncated then
                if i > 0 then
                    this.Add(DocText ";")
                    this.Add(DocLine " ")

                if i >= 100 || this.Size <= 0 then
                    this.Add(DocText "...")
                    truncated <- true
                else
                    this.Dispatch(item)
                    i <- i + 1

        this.PopWrap(Nest)
        this.Add(DocLine "")
        this.Add(DocText "]")
        this.PopWrap(Group)
        this.LastAppShaped <- false

    member private this.Dispatch(value: obj) =
        this.LastAppShaped <- false

        match value with
        | null -> this.Add(DocText "null")
        | _ ->
            if this.Depth >= 100 then
                this.Add(DocText "...")
            elif this.Size <= 0 then
                this.Add(DocText "...")
            elif DocLayout.containsRef this.Visited value then
                this.Add(DocText "...")
            else
                this.Visited <- value :: this.Visited
                this.Depth <- this.Depth + 1
                this.DispatchInner(value)
                this.Depth <- this.Depth - 1

                match this.Visited with
                | _ :: rest -> this.Visited <- rest
                | [] -> ()

    member private this.DispatchInner(value: obj) =
        if value :? Vesper.IStructuralFormattable then
            (value :?> Vesper.IStructuralFormattable).Format(this :> Vesper.IFormatSink)
        elif value :? string then
            this.Size <- this.Size - 1
            this.Add(DocText(DocLayout.quoteString (value :?> string)))
        elif value :? char then
            this.Size <- this.Size - 1
            this.Add(DocText(DocLayout.quoteChar (value :?> char)))
        elif value :? bool then
            this.Size <- this.Size - 1
            this.Add(DocText(if (value :?> bool) then "true" else "false"))
        elif value :? ITuple then
            this.FormatTuple(value :?> ITuple)
        elif value :? IFormattable then
            this.Size <- this.Size - 1
            this.Add(DocText(DocLayout.formatPrimitive value))
        elif value :? IEnumerable then
            this.FormatEnumerable(value :?> IEnumerable)
        else
            this.Size <- this.Size - 1
            this.Add(DocText(value.ToString()))

    member this.Finish() : string =
        match this.Frames with
        | [ root ] ->
            let kids = DocLayout.revOnto root.Kids []

            let docRoot =
                match kids with
                | [ single ] -> single
                | _ -> DocCat kids

            DocLayout.layout docRoot this.Width
        | _ -> failwith "VesperBaseline.RuntimeFormatState: unbalanced layout scopes at Finish."

    interface Vesper.IFormatSink with
        member this.Text(s: string) = this.Add(DocText s)
        member this.Line() = this.Add(DocLine " ")
        member this.SoftBreak() = this.Add(DocLine "")
        member this.BeginGroup() = this.PushKind(Group, 0)
        member this.EndGroup() = this.PopWrap(Group)
        member this.BeginNest(indent: int) = this.PushKind(Nest, indent)
        member this.EndNest() = this.PopWrap(Nest)
        member this.BeginRecord() = this.BeginRecordP()
        member this.Field(name: string) = this.FieldP(name)
        member this.EndRecord() = this.EndRecordP()
        member this.BeginCase(name: string) = this.BeginCaseP(name)
        member this.EndCase() = this.EndCaseP()
        member this.Child(value: obj) = this.ChildP(value)

type StructuralPrinter =

    static member Print(value: obj, widthBudget: int, sizeBudget: int) : string =
        let state = RuntimeFormatState(widthBudget, sizeBudget)
        (state :> Vesper.IFormatSink).Child(value)
        state.Finish()

    static member Print(value: obj, widthBudget: int) : string =
        StructuralPrinter.Print(value, widthBudget, 10000)
