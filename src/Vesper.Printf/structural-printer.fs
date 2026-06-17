namespace Vesper

open System
open System.Buffers
open System.Collections
open System.Globalization
open System.Runtime.CompilerServices
// The primitive `int` ordering operators (`< > <= >=` on depth / size / index
// budgets). The polymorphic family is in Vesper.Comparison, which this package
// deliberately does not reference; see ../Vesper.Core/int-comparison.fsi.
open Vesper.IntComparison

// Vesper-compiled `%A` structural engine, ported from the C# `StructuralFormat.cs`.
// Reflection-free pretty-printer: group-based layout where every composite renders
// ALL-flat (if its flat form fits the width budget) or ALL-broken.
// `Formatter.AppendStructured` calls `StructuralPrinter.Print`. Output is
// copy-pasteable Vesper source (`5L`, `3.0`, `nan`, `[1; 2; 3]`).
//
// Deviations from `StructuralFormat.cs`:
//   * `Doc` tree recomputes `flatWidth` (the C# caches it per node); trees are small.
//   * Render pass appends into a single pooled `char[]` buffer (`RenderBuf`/`RenderPos`,
//     `Emit`/`EmitSpaces`); each `RenderDoc` returns only the end column (an `int`).
//     The C# `StringBuilder` analogue; O(n) vs the naive O(n²) `string + string` port.
//   * Frame stack is a cons-list (push = cons, pop = head/tail; `PopWrap` reverses
//     via `revOnto`) rather than a BCL mutable `List<Doc>`.
//   * Cycle detection uses a cons-list of DFS-ancestor values scanned by
//     `Object.ReferenceEquals` (`DocLayout.containsRef` + `RuntimeFormatState.Visited`)
//     rather than `HashSet<obj>` + `ReferenceEqualityComparer`. Bounded by the
//     `PrintDepth = 100` guard; path-set semantics identical to the C#.
//   * The `:?` chain is `if value :? T then … (value :?> T)` (test + downcast, no
//     `as` binder) — a large `match | :? T as x` in a member body drops a binder slot.

/// The recorded layout document. A group renders all-flat or all-broken; nesting
/// governs the indent broken lines hang at.
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
    /// A group (`Doc`, `parens`): its soft breaks all flatten or all break
    /// together; `parens` wraps it in parentheses (a DU application in argument
    /// position), counted toward the flat width.
    | DocGroup of Doc * bool

/// The `Doc` width + atom-rendering helpers (the copy-pasteable source forms).
/// Pure functions over the immutable `Doc` tree — no sink state. The layout pass
/// itself lives on `RuntimeFormatState` (it threads the pooled render buffer).
module internal DocLayout =

    /// The flat (single-line) width of a `Doc`. Recomputed rather than cached; the
    /// trees are small.
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

    /// Reverse `xs` onto `acc` — used to flip a frame's `Kids` accumulator (built by
    /// consing, so reversed) back into source order.
    let rec revOnto (xs: Doc list) (acc: Doc list) : Doc list =
        match xs with
        | [] -> acc
        | h :: t -> revOnto t (h :: acc)

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
    // `CultureInfo.InvariantCulture` is used inline (matching the C# engine, which
    // formats every primitive with the invariant culture).

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

    /// Make a float render back as a float: a finite value whose shortest
    /// round-trip has no `.`/`e`/`E` would paste as an int — append ".0". float32
    /// takes the `f` suffix; non-finite floats use the F# spellings.
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

    /// Append `c` to `acc`, escaping the backslash, the newline family, and `quote`.
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

    /// Quote + escape a string (`"a\nb"`).
    let quoteString (s: string) : string = "\"" + escapeInto "" s 0 '"' + "\""

    /// Quote + escape a char (`'c'`).
    let quoteChar (c: char) : string = "'" + appendEscaped "" c '\'' + "'"

/// The kind of an open layout scope. `Root` is the implicit outermost frame.
type FrameKind =
    | Root
    | Group
    | Nest
    | Application

/// A Doc-building frame: it collects children; closing a group/nest pops and wraps.
type Frame =
    {
        Kind: FrameKind
        NestIndent: int
        Parens: bool
        /// Children, accumulated by consing (so reversed); flipped at `PopWrap`.
        mutable Kids: Doc list
    }

/// The concrete <see cref="Vesper.IFormatSink"/>: builds a `Doc` via a frame stack,
/// runs the reflection-free dispatcher for children, and lays the document out to a
/// string. Carries the depth + size (PrintSize) counters.
type RuntimeFormatState =

    /// 0 ⇒ never break (always flat); else the column budget.
    val Width: int
    /// F#'s PrintSize: a global "node" budget. Each leaf spends one unit; composites
    /// don't (their children do). At 0, further values render as "...".
    val mutable Size: int
    val mutable Depth: int
    /// The frame stack (top = head). Seeded with the Root frame.
    val mutable Frames: Frame list
    /// Set by FormatArg for the immediately-dispatched value; consumed by the first
    /// BeginApplication it produces (so only a top-level application parenthesizes).
    val mutable ArgPending: bool
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
                Parens = false
                Kids = []
            }

        {
            Width = width
            Size = printSize
            Depth = 0
            Frames = [ root ]
            ArgPending = false
            Visited = []
            RenderBuf = ArrayPool<char>.Shared.Rent(256)
            RenderPos = 0
        }

    member private this.Add(d: Doc) =
        match this.Frames with
        | top :: _ -> top.Kids <- d :: top.Kids
        | [] -> ()

    member private this.Push(f: Frame) = this.Frames <- f :: this.Frames

    /// Push a fresh frame of the given kind onto the stack (its `Kids` start empty).
    member private this.PushKind(kind: FrameKind, indent: int, parens: bool) =
        this.Push(
            {
                Kind = kind
                NestIndent = indent
                Parens = parens
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
                | Application -> DocGroup(inner, f.Parens)
                | Nest -> DocNest(f.NestIndent, inner)
                | Root -> inner

            this.Add(wrapped)
        | [] -> ()

    member private this.FormatChildP(value: obj) =
        this.ArgPending <- false
        this.Dispatch(value)

    member private this.FormatArgP(value: obj) =
        this.ArgPending <- true
        this.Dispatch(value)
        this.ArgPending <- false

    member private this.FormatTuple(t: ITuple) =
        // `(a, b)` flat; broken hangs the components under the open paren (indent 1).
        this.PushKind(Group, 0, false)
        this.Add(DocText "(")
        this.PushKind(Nest, 1, false)

        for i in 0 .. t.Length - 1 do
            if i > 0 then
                this.Add(DocText ",")
                this.Add(DocLine " ")

            this.FormatChildP(t.[i])

        this.PopWrap(Nest)
        this.Add(DocText ")")
        this.PopWrap(Group)

    member private this.FormatEnumerable(xs: IEnumerable) =
        // `[1; 2; 3]` flat; broken puts the brackets on their own lines with the
        // elements nested (indent 2), `;`-separated.
        this.PushKind(Group, 0, false)
        this.Add(DocText "[")
        this.PushKind(Nest, 2, false)
        this.Add(DocLine "")
        let mutable i = 0
        let mutable truncated = false

        for item in xs do
            if not truncated then
                if i > 0 then
                    this.Add(DocText ";")
                    this.Add(DocLine " ")

                // Truncate on the per-collection length cap (PrintLength = 100) or
                // the exhausted global node budget (`%.NA`/PrintSize). Breaking here
                // keeps the trailing `...` single.
                if i >= 100 || this.Size <= 0 then
                    this.Add(DocText "...")
                    truncated <- true
                else
                    this.FormatChildP(item)
                    i <- i + 1

        this.PopWrap(Nest)
        this.Add(DocLine "")
        this.Add(DocText "]")
        this.PopWrap(Group)

    /// The depth/size budget guard; the type-switch lives in `DispatchInner`.
    member private this.Dispatch(value: obj) =
        match value with
        | null -> this.Add(DocText "null")
        | _ ->
            // Global node budget exhausted (`%.NA`) / depth guard: truncate before
            // the value is classified.
            if this.Depth >= 100 then
                this.Add(DocText "...")
            elif this.Size <= 0 then
                this.Add(DocText "...")
            elif DocLayout.containsRef this.Visited value then
                // A back-edge: `value` is reference-identical to an ancestor still on
                // the open path ⇒ a cycle. Render `...` rather than recurse forever.
                this.Add(DocText "...")
            else
                // Track `value` as an ancestor for the duration of its subtree, then
                // pop it (so siblings — e.g. two equal interned strings — don't see
                // each other as a cycle), mirroring the C# add-on-enter / remove-on-exit.
                this.Visited <- value :: this.Visited
                this.Depth <- this.Depth + 1
                this.DispatchInner(value)
                this.Depth <- this.Depth - 1

                match this.Visited with
                | _ :: rest -> this.Visited <- rest
                | [] -> ()

    /// Resolution order (reflection-free): our own structural types, then BCL
    /// shapes, then a `ToString` fallback. Leaf cases spend one unit of the node
    /// budget; composites don't (their leaf children do).
    member private this.DispatchInner(value: obj) =
        if value :? Vesper.IStructuralFormattable then
            // Propagate arg position so the structural value's own BeginApplication
            // can parenthesize (only DUs open one).
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

    // ---- the layout pass: append into the pooled `RenderBuf` ----
    // These mirror `formatter.fs`'s grow/copy surface (`ArrayPool<char>` + a
    // `Span<char>` local over the field). Each `RenderDoc` returns the end column
    // (an `int`); the rendered characters are pushed straight into `RenderBuf`.

    /// Grow `RenderBuf` so at least `extra` more chars fit past `RenderPos`.
    member private this.EnsureRoom(extra: int) =
        let needed = this.RenderPos + extra

        if needed > this.RenderBuf.Length then
            let newLen = Math.Max(needed, this.RenderBuf.Length * 2)
            let bigger = ArrayPool<char>.Shared.Rent(newLen)
            Span<char>(this.RenderBuf).Slice(0, this.RenderPos).CopyTo(Span<char>(bigger))
            ArrayPool<char>.Shared.Return(this.RenderBuf)
            this.RenderBuf <- bigger

    /// Append a literal run to the buffer.
    member private this.Emit(s: string) =
        this.EnsureRoom(s.Length)
        s.CopyTo(Span<char>(this.RenderBuf).Slice(this.RenderPos, this.RenderBuf.Length - this.RenderPos))
        this.RenderPos <- this.RenderPos + s.Length

    /// Append `n` spaces (the broken-line indent) to the buffer.
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
            // The opening paren advances the column the inner content lays out from.
            let openCol = if parens then col + 1 else col
            // All-or-nothing: the group is flat iff its entire flat rendering fits
            // the remaining budget from the current column. width 0 ⇒ an unbounded
            // budget ⇒ always flat (the `%0A` "never break" mode).
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

    /// Lay the recorded document out to a string.
    member this.Finish() : string =
        match this.Frames with
        | [ root ] ->
            let kids = DocLayout.revOnto root.Kids []

            let docRoot =
                match kids with
                | [ single ] -> single
                | _ -> DocCat kids

            // An implicit top-level group so the top level can break. The end column
            // is discarded (a wildcard bind, not `|> ignore`: the latter would leave
            // the `ignore` recipe as a bare value, which codegen can't eta-expand).
            let _ = this.RenderDoc(DocGroup(docRoot, false), 0, false, 0, this.Width)

            let result = Span<char>(this.RenderBuf).Slice(0, this.RenderPos).ToString()
            ArrayPool<char>.Shared.Return(this.RenderBuf)
            result
        | _ -> failwith "Vesper.RuntimeFormatState: unbalanced layout scopes at Finish."

    interface Vesper.IFormatSink with
        member this.Text(s: string) = this.Add(DocText s)
        member this.Line() = this.Add(DocLine " ")
        member this.SoftBreak() = this.Add(DocLine "")
        member this.BeginGroup() = this.PushKind(Group, 0, false)
        member this.EndGroup() = this.PopWrap(Group)
        member this.BeginNest(indent: int) = this.PushKind(Nest, indent, false)
        member this.EndNest() = this.PopWrap(Nest)

        member this.BeginApplication() =
            let parens = this.ArgPending
            this.ArgPending <- false
            this.PushKind(Application, 0, parens)

        member this.EndApplication() = this.PopWrap(Application)
        member this.FormatChild(value: obj) = this.FormatChildP(value)
        member this.FormatArg(value: obj) = this.FormatArgP(value)

/// Entry point for `%A`. `Print` renders a value as copy-pasteable Vesper source;
/// it is the standalone "render to string" used by tests and by the printf
/// handler's structural hole (`Formatter.AppendStructured`).
type StructuralPrinter =

    /// Render `value` within a column budget of `widthBudget` (0 ⇒ never break, the
    /// `%0A` mode) and a node budget of `sizeBudget` (F# PrintSize; nodes past it
    /// render as `...`, the `%.NA` mode).
    static member Print(value: obj, widthBudget: int, sizeBudget: int) : string =
        let state = RuntimeFormatState(widthBudget, sizeBudget)
        (state :> Vesper.IFormatSink).FormatChild(value)
        state.Finish()

    /// Render `value` with the default node budget (F#'s 10000 — plain `%A`).
    static member Print(value: obj, widthBudget: int) : string =
        StructuralPrinter.Print(value, widthBudget, 10000)
