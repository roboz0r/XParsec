namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// Synthesised `IStructuralFormattable.Format` bodies (`%A`): straight-line `callvirt`s
/// on the `sink` arg (`ldarg.1`), `BeginRecord; (Field name; Child (box f))×n; EndRecord`
/// for a record. The rendered layout is the runtime sink's job, not the body's.
module internal EmitStructuralFormat =

    /// `Fields` is the case payload in declaration order.
    type UnionFormatCase =
        {
            Name: string
            Fields: (EntityHandle * FrozenType) list
        }

    /// `sink.Text(str)` — push the sink, the literal, `callvirt Text`.
    let private sinkText (b: IlBuilder) (h: IStructuralHandles) (str: string) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldstr(h.UserString str))
        b.Add(ILInstr.Callvirt(h.FormatSink.Text, 2, 0))

    /// A nullary sink call (`sink.BeginRecord()` / `sink.EndRecord()` /
    /// `sink.EndCase()`).
    let private sinkCall0 (b: IlBuilder) (handle: EntityHandle) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Callvirt(handle, 1, 0))

    /// `sink.<label>(name)` — a string-arg marker call (`Field(name)` /
    /// `BeginCase(name)`).
    let private sinkLabel (b: IlBuilder) (h: IStructuralHandles) (label: EntityHandle) (name: string) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldstr(h.UserString name))
        b.Add(ILInstr.Callvirt(label, 2, 0))

    /// `sink.Child(box this.<field>)` — load the field off `this` (`ldarg.0`). The `box`
    /// is uniform, a no-op on reference types (ECMA-335 III.4.1), so `Child` sees the
    /// runtime type behind the erased `obj`.
    let private sinkChild (b: IlBuilder) (h: IStructuralHandles) (fieldHandle: EntityHandle) (fty: FrozenType) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld fieldHandle)
        b.Add(ILInstr.Box(h.BoxToken fty))
        b.Add(ILInstr.Callvirt(h.FormatSink.Child, 2, 0))

    /// `void Format(IFormatSink sink)` for a record: `BeginRecord;
    /// (Field name; Child (box field))×n; EndRecord`, 2+2n calls in declaration order.
    /// `fields` is `(label, field handle, field type)`.
    let buildRecordFormat (h: IStructuralHandles) (fields: (string * EntityHandle * FrozenType) list) : ILBody =
        let b = IlBuilder()

        sinkCall0 b h.FormatSink.BeginRecord

        for (name, handle, t) in fields do
            sinkLabel b h h.FormatSink.Field name
            sinkChild b h handle t

        sinkCall0 b h.FormatSink.EndRecord

        b.Add ILInstr.Ret
        b.Body

    /// One case's `BeginCase(name); Child (box payload)×k; EndCase`. The sink derives
    /// `None` / `Some ·` / `Case (·, ·)` from the observed child count.
    let private emitFormatCase (b: IlBuilder) (h: IStructuralHandles) (c: UnionFormatCase) : unit =
        sinkLabel b h h.FormatSink.BeginCase c.Name

        for (handle, t) in c.Fields do
            sinkChild b h handle t

        sinkCall0 b h.FormatSink.EndCase

    /// `void Format(IFormatSink sink)` on a hierarchy union's case type: its own case,
    /// straight through, since the dispatch that reached this body settled which one.
    let buildUnionCaseFormat (h: IStructuralHandles) (c: UnionFormatCase) : ILBody =
        let b = IlBuilder()
        emitFormatCase b h c
        b.Add ILInstr.Ret
        b.Body

    /// `void Format(IFormatSink sink)` for a FLAT union: switch on `_tag`, then the active
    /// arm emits that case. `cases` is in tag order (index = `_tag` value).
    let buildUnionFormat (h: IStructuralHandles) (tagField: EntityHandle) (cases: UnionFormatCase list) : ILBody =
        let b = IlBuilder()
        let n = List.length cases

        match cases with
        | [] ->
            // F# unions always have ≥1 case; the dispatch below indexes `cases.[n - 1]`.
            sinkText b h "()"
        | _ ->
            let endLabel = b.Label()
            // One label per non-last case; the last case is the dispatch fall-through.
            let caseLabels = [| for _ in 0 .. n - 2 -> b.Label() |]

            // Dispatch: `if _tag = k goto caseK` for every case but the last.
            cases
            |> List.iteri (fun k _ ->
                if k < n - 1 then
                    b.Add(ILInstr.Ldarg 0)
                    b.Add(ILInstr.Ldfld tagField)
                    b.Add(ILInstr.LdcI4 k)
                    b.Add(ILInstr.Beq caseLabels.[k])
            )

            // Fall-through ⇒ the last (highest-tag) case.
            emitFormatCase b h cases.[n - 1]
            b.Add(ILInstr.Br endLabel)

            // The earlier cases, each branched to and exiting to `endLabel`.
            cases
            |> List.iteri (fun k c ->
                if k < n - 1 then
                    b.Add(ILInstr.Mark caseLabels.[k])
                    emitFormatCase b h c
                    b.Add(ILInstr.Br endLabel)
            )

            b.Add(ILInstr.Mark endLabel)

        b.Add ILInstr.Ret
        b.Body
