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
            Fields: EmitStructural.StructuralField list
        }

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

    /// `sink.Child(box this.<field>)`. An erased slot already holds `object` and is passed
    /// as read. Otherwise the `box` is uniform, a no-op on reference types (ECMA-335
    /// III.4.1), so `Child` sees the runtime type.
    let private sinkChild (b: IlBuilder) (h: IStructuralHandles) (f: EmitStructural.StructuralField) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldarg 0)
        EmitStructural.loadFieldPath b f

        match f.Cast with
        | ValueSome _ -> ()
        | ValueNone -> b.Add(ILInstr.Box(h.BoxToken f.Ty))

        b.Add(ILInstr.Callvirt(h.FormatSink.Child, 2, 0))

    /// `void Format(IFormatSink sink)` for a record: `BeginRecord;
    /// (Field name; Child (box field))×n; EndRecord`, 2+2n calls in declaration order.
    let buildRecordFormat (h: IStructuralHandles) (fields: (string * EmitStructural.StructuralField) list) : ILBody =
        let b = IlBuilder()

        sinkCall0 b h.FormatSink.BeginRecord

        for (name, f) in fields do
            sinkLabel b h h.FormatSink.Field name
            sinkChild b h f

        sinkCall0 b h.FormatSink.EndRecord

        b.Add ILInstr.Ret
        b.Body

    /// One case's `BeginCase(name); Child (box payload)×k; EndCase`. The sink derives
    /// `None` / `Some ·` / `Case (·, ·)` from the observed child count.
    let private emitFormatCase (b: IlBuilder) (h: IStructuralHandles) (c: UnionFormatCase) : unit =
        sinkLabel b h h.FormatSink.BeginCase c.Name

        for f in c.Fields do
            sinkChild b h f

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

        EmitStructural.switchOnTag
            b
            tagField
            [
                for (tag, c) in List.indexed cases ->
                    tag,
                    (fun () ->
                        emitFormatCase b h c
                        EmitStructural.WalkExit.Joins
                    )
            ]

        b.Add ILInstr.Ret
        b.Body
