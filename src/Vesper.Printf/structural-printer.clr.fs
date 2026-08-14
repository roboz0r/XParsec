namespace Vesper

open System
open System.Buffers
open System.Collections
open System.Globalization
open System.Runtime.CompilerServices
open Vesper.IntComparison

/// The recorded layout document.
type Doc =
    /// A literal run that never breaks (labels, punctuation, brackets).
    | DocText of string
    /// A soft break: its `flat` form (" " or "") when its group is flat, a newline
    /// + indent when broken.
    | DocLine of string
    /// A concatenation of children.
    | DocCat of Doc list
    /// An indentation scope: broken lines inside hang at `+indent`.
    | DocNest of int * Doc
    /// Its soft breaks all flatten or all break together. `parens` wraps it in
    /// parentheses (a DU application in argument position), counted toward the flat width.
    | DocGroup of Doc * bool

/// `Doc` width, and the primitive → copy-pasteable-source atom renderers.
module internal DocLayout =

    /// F#'s PrintLength: elements past this many render as `...`, per collection.
    [<Literal>]
    let printLength = 100

    /// F#'s PrintDepth: a value nested deeper than this renders as `...`.
    [<Literal>]
    let printDepth = 100

    /// The flat (single-line) width of a `Doc`.
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

    let rec revOnto (xs: Doc list) (acc: Doc list) : Doc list =
        match xs with
        | [] -> acc
        | h :: t -> revOnto t (h :: acc)

    /// Interleave components with `,` + a soft `Line`: `a, b, c`. `firstDone` is false
    /// for the first component (no leading separator), true thereafter.
    let rec interleaveComponents (xs: Doc list) (firstDone: bool) : Doc list =
        match xs with
        | [] -> []
        | k :: rest ->
            if firstDone then
                DocText "," :: DocLine " " :: k :: interleaveComponents rest true
            else
                k :: interleaveComponents rest true

    /// The `(a, b, c)` tuple `Doc` from already-built component `Doc`s: hung at indent 1
    /// under the open paren, the whole a single breakable group. A multi-payload union
    /// case renders `Case (a, b)` through this too.
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

    /// True if `v` is reference-identical to any element of `xs`.
    /// Linear walk over the DFS-ancestor chain (bounded by PrintDepth = 100).
    let rec containsRef (xs: obj list) (v: obj) : bool =
        match xs with
        | [] -> false
        | h :: t ->
            if Object.ReferenceEquals(h, v) then
                true
            else
                containsRef t v

    // ---- atom rendering (copy-pasteable source forms) ----

    /// Does `s` contain a `.`, `e`, or `E` (so it already reads back as a float)?
    let rec hasDot (s: string) (i: int) : bool =
        if i >= s.Length then
            false
        else
            let c = s.[i]

            if c = '.' || c = 'e' || c = 'E' then
                true
            else
                hasDot s (i + 1)

    /// Make a float paste back as a float: `3.0` stringifies `"3"`, so append `".0"` when
    /// there is no `.`/`e`/`E`. Non-finite values take the F# spellings `nan` /
    /// `infinity` / `-infinity`; `suffix` is `"f"` for float32, so `nanf`, `3.0f`.
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

    /// Render a primitive as its copy-pasteable source atom. Each non-`int32`
    /// integral / decimal carries the type suffix the XParsec.FSharp lexer accepts
    /// (`5L`, `5uy`, `1.5M`) so it re-lexes at its source type.
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
                | :? IntPtr -> "n"
                | :? UIntPtr -> "un"
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

    let quoteString (s: string) : string = "\"" + escapeInto "" s 0 '"' + "\""

    let quoteChar (c: char) : string = "'" + appendEscaped "" c '\'' + "'"

/// The kind of an open layout scope. `Root` is the implicit outermost frame; a
/// `Collect` frame's children are popped by hand and assembled from their observed
/// count (`None`, `Some x`, `Case (a, b)`, `(a, b)`).
type FrameKind =
    | Root
    | Group
    | Nest
    | Collect

/// A Doc-building frame: it collects children; closing a group/nest pops and wraps.
type Frame =
    {
        Kind: FrameKind
        NestIndent: int
        /// Children, accumulated by consing (so reversed); flipped at `PopWrap`.
        mutable Kids: Doc list
    }

/// Bookkeeping for one open `BeginRecord` / `BeginCase` scope. `Count` is the record's
/// field count (field 0 opens `{ `, the rest prefix `;`) or the case's payload count
/// (0 ⇒ `None`, 1 ⇒ `Some x`, more ⇒ `Case (a, b)`).
type SemFrame =
    {
        IsCase: bool
        Name: string
        mutable Count: int
        mutable ChildAppShaped: bool
    }

/// A NON-generic BCL `IEnumerator` as the `seq<obj>` `Sequence` takes, so the `IEnumerable`
/// arm and a declared `Format` body reach the one renderer. `Vesper.Collections.BoxedItems`
/// is the generic counterpart every collection uses; this one cannot be it, because
/// `IEnumerator` is CLR-only and already hands back `obj`. One-shot: `GetEnumerator` is `this`.
type BclPrintItems =
    val inner: IEnumerator

    new(inner: IEnumerator) = { inner = inner }

    interface seq<obj> with
        member this.GetEnumerator() = (this :> enumerator<obj>)

    interface enumerator<obj> with
        member this.Current = this.inner.Current
        member this.MoveNext() = this.inner.MoveNext()

    // Non-generic `IEnumerator` carries no `Dispose`, so there is nothing to forward to.
    interface Vesper.disposable with
        member this.Dispose() = ()

type RuntimeFormatState =

    /// 0 ⇒ never break (always flat); else the column budget.
    val Width: int
    /// F#'s PrintSize node budget: each leaf spends one unit, composites none. At 0,
    /// further values render as `...`.
    val mutable Size: int
    val mutable Depth: int
    /// The frame stack (top = head). Seeded with the Root frame.
    val mutable Frames: Frame list
    /// The semantic-frame stack for the BeginRecord / BeginCase protocol (top = head).
    val mutable SemFrames: SemFrame list
    /// Whether the value `Dispatch` most recently completed was application-shaped (a
    /// union case with ≥1 payload). A single-payload case parenthesises its child iff
    /// so: `Some (Some 3)`, against a bare `Some 3` / `Some [1; 2]`.
    val mutable LastAppShaped: bool
    /// DFS-ancestor chain for cycle detection (top = head). A value reference-identical
    /// to an ancestor is a back-edge and renders `...`.
    val mutable Visited: obj list
    /// Pooled render buffer (`Span<char>` cannot be a field of a heap class; layout
    /// members build `Span<char>` locals over it).
    val mutable RenderBuf: char[]
    /// Count of characters written into `RenderBuf` so far.
    val mutable RenderPos: int

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
            RenderBuf = ArrayPool<char>.Shared.Rent(256)
            RenderPos = 0
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
                // `Collect` frames are popped by hand, in `EndCaseP` / `FormatTuple`.
                | Root
                | Collect -> inner

            this.Add(wrapped)
        | [] -> ()

    // ---- the semantic protocol (BeginRecord/Field/BeginCase/Child/…) ----
    // `Field` adds the label before `Child` recurses, so a nested record's first
    // `Field` cannot clobber it.

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
                // First field opens `{ name = ` (outside the hang), then the fields
                // hang at +2 when the group breaks.
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
                // Field-less record ⇒ bare `{ }`; F# records have ≥1 field.
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
        // A record field's label was already emitted at `Field`; only a case counts here.
        match this.SemFrames with
        | sf :: _ when sf.IsCase ->
            sf.Count <- sf.Count + 1
            sf.ChildAppShaped <- this.LastAppShaped
        | _ -> ()

    member private this.EndCaseP() =
        match this.SemFrames with
        | cf :: rest ->
            this.SemFrames <- rest

            // The case's payload `Doc`s, in source order, from its `Collect` frame.
            let kids =
                match this.Frames with
                | f :: fr ->
                    this.Frames <- fr
                    DocLayout.revOnto f.Kids []
                | [] -> []

            let caseDoc =
                match cf.Count with
                | 0 ->
                    // Nullary case: a bare identifier (`None`).
                    DocText cf.Name
                | 1 ->
                    let child =
                        match kids with
                        | [ single ] -> single
                        | _ -> DocCat kids

                    let payload = if cf.ChildAppShaped then DocGroup(child, true) else child

                    DocGroup(DocCat [ DocText(cf.Name + " "); payload ], false)
                | _ ->
                    // Multi-payload case renders as a tuple, `Case (a, b)`: its own
                    // parens disambiguate, so no child is wrapped.
                    DocGroup(DocCat [ DocText(cf.Name + " "); DocLayout.buildTupleDoc kids ], false)

            this.Add(caseDoc)
            // A case is application-shaped iff it carries a payload.
            this.LastAppShaped <- cf.Count >= 1
        | [] -> ()

    member private this.FormatTuple(t: ITuple) =
        // `(a, b)` flat; broken hangs the components under the open paren (indent 1).
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
        // A tuple is never application-shaped (`Some (1, 2)` gets no extra parens).
        this.LastAppShaped <- false

    /// `; ` before every element but the first.
    member private this.SeqSep(count: int) =
        if count > 0 then
            this.Add(DocText ";")
            this.Add(DocLine " ")

    /// The `;`-separated elements, then `...` iff the source outlasts the budget. Owns the
    /// cursor, because cutting the walk short is the normal case here and the source may be
    /// holding a file or stream behind it.
    member private this.SeqElements(items: seq<obj>) =
        use cursor = items.GetEnumerator()
        let mutable count = 0
        // One element AHEAD of the last rendered: `...` shows iff one remains, so a source
        // ending exactly at the cut is not elided. `Current` is never read for it.
        let mutable more = cursor.MoveNext()

        while more && count < DocLayout.printLength && this.Size > 0 do
            this.SeqSep count
            this.Dispatch(cursor.Current)
            count <- count + 1
            more <- cursor.MoveNext()

        if more then
            // The `;` precedes it, so the output ends `2; ...]`.
            this.SeqSep count
            this.Add(DocText "...")

    /// `[1; 2; 3]` flat; broken puts the brackets on their own lines with the elements
    /// nested (indent 2), `;`-separated. Pulls only what the budget allows, so an unbounded
    /// source is safe.
    member private this.SequenceP(items: seq<obj>) =
        this.PushKind(Group, 0)
        this.Add(DocText "[")
        this.PushKind(Nest, 2)
        this.Add(DocLine "")
        this.SeqElements(items)
        this.PopWrap(Nest)
        this.Add(DocLine "")
        this.Add(DocText "]")
        this.PopWrap(Group)
        // A list is never application-shaped (`Some [1; 2]` gets no extra parens).
        this.LastAppShaped <- false

    member private this.FormatEnumerable(xs: IEnumerable) =
        this.SequenceP(new BclPrintItems(xs.GetEnumerator()) :> seq<obj>)

    /// The depth / node-budget guard around every value.
    member private this.Dispatch(value: obj) =
        this.LastAppShaped <- false

        match value with
        | null -> this.Add(DocText "null")
        | _ ->
            // Depth guard / exhausted node budget: truncate before classifying the value.
            if this.Depth >= DocLayout.printDepth then
                this.Add(DocText "...")
            elif this.Size <= 0 then
                this.Add(DocText "...")
            elif DocLayout.containsRef this.Visited value then
                this.Add(DocText "...")
            else
                // An ancestor only for its own subtree: popped on exit, so two siblings
                // (e.g. the same interned string twice) do not read as a cycle.
                this.Visited <- value :: this.Visited
                this.Depth <- this.Depth + 1
                this.DispatchInner(value)
                this.Depth <- this.Depth - 1

                match this.Visited with
                | _ :: rest -> this.Visited <- rest
                | [] -> ()

    /// The type switch: every arm is a cast, so no arm reflects over the value's fields.
    member private this.DispatchInner(value: obj) =
        match value with
        | :? Vesper.IStructuralFormattable as structural ->
            // The synthesised `Format` body drives the semantic protocol.
            structural.Format(this :> Vesper.IFormatSink)
        | :? string as s ->
            this.Size <- this.Size - 1
            this.Add(DocText(DocLayout.quoteString s))
        | :? char as c ->
            this.Size <- this.Size - 1
            this.Add(DocText(DocLayout.quoteChar c))
        | :? bool as b ->
            this.Size <- this.Size - 1
            this.Add(DocText(if b then "true" else "false"))
        | :? ITuple as t -> this.FormatTuple t
        | :? IFormattable ->
            this.Size <- this.Size - 1
            this.Add(DocText(DocLayout.formatPrimitive value))
        | :? IEnumerable as xs -> this.FormatEnumerable xs
        | _ ->
            this.Size <- this.Size - 1
            this.Add(DocText(value.ToString()))

    // ---- the layout pass: append into the pooled `RenderBuf` ----

    /// Grow `RenderBuf` so at least `extra` more chars fit past `RenderPos`.
    member private this.EnsureRoom(extra: int) =
        let needed = this.RenderPos + extra

        if needed > this.RenderBuf.Length then
            let newLen = Math.Max(needed, this.RenderBuf.Length * 2)
            let bigger = ArrayPool<char>.Shared.Rent(newLen)
            Span<char>(this.RenderBuf).Slice(0, this.RenderPos).CopyTo(Span<char>(bigger))
            ArrayPool<char>.Shared.Return(this.RenderBuf)
            this.RenderBuf <- bigger

    member private this.Emit(s: string) =
        this.EnsureRoom(s.Length)
        s.CopyTo(Span<char>(this.RenderBuf).Slice(this.RenderPos, this.RenderBuf.Length - this.RenderPos))
        this.RenderPos <- this.RenderPos + s.Length

    /// Append `n` spaces — the broken-line indent.
    member private this.EmitSpaces(n: int) =
        if n > 0 then
            this.EnsureRoom(n)
            Span<char>(this.RenderBuf).Slice(this.RenderPos, n).Fill(' ')
            this.RenderPos <- this.RenderPos + n

    /// Lay `d` out into the buffer, threading the current indent / broken flag /
    /// column; returns the column it ends at.
    member private this.RenderDoc(d: Doc, indent: int, broken: bool, col: int, width: int) : int =
        match d with
        | DocText s ->
            this.Emit(s)
            col + s.Length
        | DocLine flat ->
            if broken then
                this.Emit("\n")
                this.EmitSpaces(indent)
                indent
            else
                this.Emit(flat)
                col + flat.Length
        | DocNest(i, inner) -> this.RenderDoc(inner, indent + i, broken, col, width)
        | DocCat kids -> this.RenderCat(kids, indent, broken, col, width)
        | DocGroup(inner, parens) ->
            let openCol = if parens then col + 1 else col
            // All-or-nothing: flat iff the group's whole flat rendering fits the budget
            // remaining from `openCol`.
            let groupBroken = width <> 0 && openCol + DocLayout.flatWidth inner > width

            if parens then
                this.Emit("(")

            let endCol = this.RenderDoc(inner, indent, groupBroken, openCol, width)

            if parens then
                this.Emit(")")
                endCol + 1
            else
                endCol

    member private this.RenderCat(kids: Doc list, indent: int, broken: bool, col: int, width: int) : int =
        match kids with
        | [] -> col
        | k :: rest ->
            let col1 = this.RenderDoc(k, indent, broken, col, width)
            this.RenderCat(rest, indent, broken, col1, width)

    member this.Finish() : string =
        match this.Frames with
        | [ root ] ->
            let kids = DocLayout.revOnto root.Kids []

            let docRoot =
                match kids with
                | [ single ] -> single
                | _ -> DocCat kids

            // An implicit top-level group, so the top level can break.
            let _ = this.RenderDoc(DocGroup(docRoot, false), 0, false, 0, this.Width)

            let result = Span<char>(this.RenderBuf).Slice(0, this.RenderPos).ToString()
            ArrayPool<char>.Shared.Return(this.RenderBuf)
            result
        | _ -> failwith "Vesper.RuntimeFormatState: unbalanced layout scopes at Finish."

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
        member this.Sequence(items: seq<obj>) = this.SequenceP(items)
        member this.Child(value: obj) = this.ChildP(value)

type StructuralPrinter =

    static member Print(value: obj, widthBudget: int, sizeBudget: int) : string =
        let state = RuntimeFormatState(widthBudget, sizeBudget)
        (state :> Vesper.IFormatSink).Child(value)
        state.Finish()

    static member Print(value: obj, widthBudget: int) : string =
        StructuralPrinter.Print(value, widthBudget, 10000)
