namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// Synthesised `IStructuralFormattable.Format` bodies (`%A`): straight-line `callvirt`s
/// on the `sink` arg (`ldarg.1`) — `BeginRecord; (Field name; Child (box f))×n; EndRecord`
/// for a record. The rendered layout is the runtime sink's job, not the body's.
module internal EmitStructuralFormat =

    /// `Fields` is `(label, field handle, field type)` in declaration order.
    type RecordFormatSupport =
        {
            Sink: FormatSinkHandles
            MkString: string -> UserStringHandle
            BoxToken: FrozenType -> EntityHandle
            Fields: (string * EntityHandle * FrozenType) list
        }

    /// `Fields` is the case payload in declaration order.
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

    /// `sink.Text(str)` — push the sink, the literal, `callvirt Text`.
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
    /// `BeginCase(name)`).
    let private sinkLabel (b: IlBuilder) (mk: string -> UserStringHandle) (label: EntityHandle) (name: string) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldstr(mk name))
        b.Add(ILInstr.Callvirt(label, 2, 0))

    /// `sink.Child(box this.<field>)` — load the field off `this` (`ldarg.0`). The `box`
    /// is uniform, a no-op on reference types (ECMA-335 III.4.1), so `Child` sees the
    /// runtime type behind the erased `obj`.
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

    /// `void Format(IFormatSink sink)` for a record: `BeginRecord;
    /// (Field name; Child (box field))×n; EndRecord`, 2+2n calls in declaration order.
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

    /// `void Format(IFormatSink sink)` for a union: switch on `_tag`, then the active
    /// arm emits `BeginCase(name); Child (box payload)×k; EndCase`. The sink derives
    /// `None` / `Some ·` / `Case (·, ·)` from the observed child count.
    let buildUnionFormat (s: UnionFormatSupport) : ILBody =
        let b = IlBuilder()
        let sink = s.Sink

        let emitCase (c: UnionFormatCase) : unit =
            sinkLabel b s.MkString sink.BeginCase c.Name

            for (h, t) in c.Fields do
                sinkChild b sink s.BoxToken h t

            sinkCall0 b sink.EndCase

        let cases = s.Cases
        let n = List.length cases

        match cases with
        | [] ->
            // F# unions always have ≥1 case; the dispatch below indexes `cases.[n - 1]`.
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
