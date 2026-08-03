namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// Synthesised `IStructuralFormattable.Format` body builders (`%A`). The
/// `Format(IFormatSink sink)` body is straight-line `callvirt`s on the `sink` arg
/// (`ldarg.1`) that speak the *semantic* sink protocol — `BeginRecord` /
/// `Field(name)` / `Child(box value)` / `EndRecord` for records, `BeginCase(name)`
/// / `Child(box payload)` / `EndCase` for union arms. The record/union layout
/// policy (`{ F = ·; G = · }`, `None`, `Some ·`, `Case (·, ·)`) lives entirely in
/// the runtime sink (`Vesper.Printf/structural-printer.clr.fs`), which lowers these
/// frames into its `Doc` builders; the emitted body no longer replays the layout
/// grammar, so the `%A` policy is patchable in the runtime rather than frozen into
/// every assembly. Every field/payload is `box`ed and handed to `Child(obj)`; `box`
/// on a reference type is a no-op (ECMA-335 III.4.1), so it is emitted uniformly —
/// value fields, reference fields, and generic typar fields (`Some of 'T`) all take
/// one `box`.
///
/// Lifted out of `Emit.fs` so all `%A`-body emission lives in one focused module;
/// `NominalEmit` is the sole caller.
module internal EmitStructuralFormat =

    /// `(label, field handle, field type)` per record field, declaration order.
    /// `MkString` mints the literal `UserStringHandle`s the body pushes (codegen owns
    /// the metadata context); `BoxToken` mints the `box` type token per field type.
    type RecordFormatSupport =
        {
            Sink: FormatSinkHandles
            MkString: string -> UserStringHandle
            BoxToken: FrozenType -> EntityHandle
            Fields: (string * EntityHandle * FrozenType) list
        }

    /// One union case for `Format` synthesis: its name and its `(field handle,
    /// field type)` payload in declaration order.
    type UnionFormatCase =
        {
            Name: string
            Fields: (EntityHandle * FrozenType) list
        }

    type UnionFormatSupport =
        {
            Sink: FormatSinkHandles
            MkString: string -> UserStringHandle
            BoxToken: FrozenType -> EntityHandle
            TagField: EntityHandle
            /// Cases in tag order (index = `_tag` value).
            Cases: UnionFormatCase list
        }

    /// `sink.Text(str)` — push the sink, the literal, `callvirt Text`. Only the
    /// total-safety empty-union fallback (`()`) still emits a raw literal; the
    /// semantic record/union bodies speak `Field`/`BeginCase`/`Child` instead.
    let private sinkText
        (b: IlBuilder)
        (sink: FormatSinkHandles)
        (mk: string -> UserStringHandle)
        (str: string)
        : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldstr(mk str))
        b.Add(ILInstr.Callvirt(sink.Text, 2, 0))

    /// A nullary sink call (`sink.BeginRecord()` / `sink.EndRecord()` /
    /// `sink.EndCase()`).
    let private sinkCall0 (b: IlBuilder) (h: EntityHandle) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Callvirt(h, 1, 0))

    /// `sink.<label>(name)` — a string-arg marker call (`Field(name)` /
    /// `BeginCase(name)`): push the sink, the literal name, `callvirt` the label
    /// entry.
    let private sinkLabel (b: IlBuilder) (mk: string -> UserStringHandle) (label: EntityHandle) (name: string) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldstr(mk name))
        b.Add(ILInstr.Callvirt(label, 2, 0))

    /// `sink.Child(box this.<field>)` — push the sink, load + box the field off
    /// `this` (`ldarg.0`), `callvirt Child`. The `box` is uniform (a no-op on
    /// reference types) so `Child` sees the runtime type behind the erased `obj`.
    let private sinkChild
        (b: IlBuilder)
        (sink: FormatSinkHandles)
        (boxToken: FrozenType -> EntityHandle)
        (fieldHandle: EntityHandle)
        (fty: FrozenType)
        : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld fieldHandle)
        b.Add(ILInstr.Box(boxToken fty))
        b.Add(ILInstr.Callvirt(sink.Child, 2, 0))

    /// `void Format(IFormatSink sink)` for a record. Emits the semantic frame
    /// `BeginRecord; (Field name; Child (box field))×n; EndRecord` — 2+2n calls in
    /// declaration order. The layout (`{ F = ·; G = · }`, the +2 hang, break policy)
    /// is the runtime sink's job, not the emitted body's.
    let buildRecordFormat (s: RecordFormatSupport) : ILBody =
        let b = IlBuilder()
        let sink = s.Sink

        sinkCall0 b sink.BeginRecord

        for (name, h, t) in s.Fields do
            sinkLabel b s.MkString sink.Field name
            sinkChild b sink s.BoxToken h t

        sinkCall0 b sink.EndRecord

        b.Add ILInstr.Ret
        b.Body

    /// `void Format(IFormatSink sink)` for a union: switch on `_tag`, render the
    /// active case. Each case arm emits `BeginCase(name); Child (box payload)×k;
    /// EndCase` (2+k calls; a nullary case is `BeginCase(name); EndCase()`). The
    /// runtime sink decides the rendered form from the observed child count and the
    /// application-shaped frame mark (`None` / `Some ·` / `Case (·, ·)`).
    let buildUnionFormat (s: UnionFormatSupport) : ILBody =
        let b = IlBuilder()
        let sink = s.Sink

        // The tag-switch dispatch (below) stays here; each case arm speaks the
        // semantic `BeginCase`/`Child`/`EndCase` protocol in declaration order.
        let emitCase (c: UnionFormatCase) : unit =
            sinkLabel b s.MkString sink.BeginCase c.Name

            for (h, t) in c.Fields do
                sinkChild b sink s.BoxToken h t

            sinkCall0 b sink.EndCase

        let cases = s.Cases
        let n = List.length cases

        match cases with
        | [] ->
            // F# unions always have ≥1 case; the dispatch below indexes
            // `cases.[n - 1]` as the fall-through, so guard the empty case here
            // (mirrors the record `[]` arm — both unreachable in practice, total
            // for safety).
            sinkText b sink s.MkString "()"
        | _ ->
            let endLabel = b.Label()
            // One label per non-last case; the last case is the dispatch fall-through.
            let caseLabels = [| for _ in 0 .. n - 2 -> b.Label() |]

            // Dispatch: `if _tag = k goto caseK` for every case but the last.
            cases
            |> List.iteri (fun k _ ->
                if k < n - 1 then
                    b.Add(ILInstr.Ldarg 0)
                    b.Add(ILInstr.Ldfld s.TagField)
                    b.Add(ILInstr.LdcI4 k)
                    b.Add(ILInstr.Beq caseLabels.[k])
            )

            // Fall-through ⇒ the last (highest-tag) case.
            emitCase cases.[n - 1]
            b.Add(ILInstr.Br endLabel)

            // The earlier cases, each branched to and exiting to `endLabel`.
            cases
            |> List.iteri (fun k c ->
                if k < n - 1 then
                    b.Add(ILInstr.Mark caseLabels.[k])
                    emitCase c
                    b.Add(ILInstr.Br endLabel)
            )

            b.Add(ILInstr.Mark endLabel)

        b.Add ILInstr.Ret
        b.Body
