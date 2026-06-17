namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// Synthesised `IStructuralFormattable.Format` body builders (`%A`). The
/// `Format(IFormatSink sink)` body is straight-line `callvirt`s on the `sink` arg
/// (`ldarg.1`), mirroring the hand-written `Point`/`Opt` impls in
/// `StructuralFormatTests.fs` (all the layout complexity lives in the C#
/// sink). Every field/payload is `box`ed and handed to `FormatChild`/`FormatArg`;
/// the reflection-free dispatcher classifies it at runtime. `box` on a reference
/// type is a no-op (ECMA-335 III.4.1), so it is emitted uniformly — value fields,
/// reference fields, and generic typar fields (`Some of 'T`) all take one `box`.
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

    /// A nullary sink call (`sink.BeginGroup()` / `sink.Line()` / …).
    let private sinkCall0 (b: IlBuilder) (h: EntityHandle) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Callvirt(h, 1, 0))

    /// `sink.BeginNest(n)`.
    let private sinkBeginNest (b: IlBuilder) (sink: FormatSinkHandles) (n: int) : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.LdcI4 n)
        b.Add(ILInstr.Callvirt(sink.BeginNest, 2, 0))

    /// `sink.<recurse>(box this.<field>)` — push the sink, load + box the field off
    /// `this` (`ldarg.0`), `callvirt` the recursion entry (`FormatChild`/`FormatArg`).
    let private sinkFormatField
        (b: IlBuilder)
        (boxToken: FrozenType -> EntityHandle)
        (recurse: EntityHandle)
        (fieldHandle: EntityHandle)
        (fty: FrozenType)
        : unit =
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld fieldHandle)
        b.Add(ILInstr.Box(boxToken fty))
        b.Add(ILInstr.Callvirt(recurse, 2, 0))

    /// `void Format(IFormatSink sink)` for a record. Flat ⇒ `{ X = 1; Y = "a" }`;
    /// broken (the C# sink decides) ⇒ the fields hang at +2 with the closer
    /// dedented. The first label opens with `{ `; the `;`/`Line` separator and the
    /// next label precede each subsequent field; `EndNest` then ` }` close.
    let buildRecordFormat (s: RecordFormatSupport) : ILBody =
        let b = IlBuilder()
        let sink = s.Sink
        let text = sinkText b sink s.MkString
        let child = sinkFormatField b s.BoxToken sink.FormatChild

        match s.Fields with
        | [] ->
            // F# records always have ≥1 field; keep this total for safety.
            text "{ }"
        | (name0, h0, t0) :: rest ->
            sinkCall0 b sink.BeginGroup
            text (sprintf "{ %s = " name0)
            sinkBeginNest b sink 2
            child h0 t0

            for (name, h, t) in rest do
                text ";"
                sinkCall0 b sink.Line
                text (sprintf "%s = " name)
                child h t

            sinkCall0 b sink.EndNest
            text " }"
            sinkCall0 b sink.EndGroup

        b.Add ILInstr.Ret
        b.Body

    /// `void Format(IFormatSink sink)` for a union: switch on `_tag`, render the
    /// active case. A nullary case is a bare identifier (`None`); a payload-bearing
    /// case opens a `BeginApplication` (so it parenthesises in argument position,
    /// `Some (Some 3)`). A single payload renders via `FormatArg`; a multi-field
    /// payload renders as a parenthesised tuple of `FormatChild`ren (`C (a, b)`).
    let buildUnionFormat (s: UnionFormatSupport) : ILBody =
        let b = IlBuilder()
        let sink = s.Sink
        let text = sinkText b sink s.MkString
        let child = sinkFormatField b s.BoxToken sink.FormatChild
        let arg = sinkFormatField b s.BoxToken sink.FormatArg

        let emitCase (c: UnionFormatCase) : unit =
            match c.Fields with
            | [] -> text c.Name
            | [ (h, t) ] ->
                sinkCall0 b sink.BeginApplication
                text (c.Name + " ")
                arg h t
                sinkCall0 b sink.EndApplication
            | many ->
                // `Case (a, b)` — the application wraps a parenthesised tuple; the
                // tuple parens already disambiguate, so the components are normal
                // `FormatChild`ren (not args).
                sinkCall0 b sink.BeginApplication
                text (c.Name + " ")
                sinkCall0 b sink.BeginGroup
                text "("
                sinkBeginNest b sink 1

                many
                |> List.iteri (fun i (h, t) ->
                    if i > 0 then
                        text ","
                        sinkCall0 b sink.Line

                    child h t
                )

                sinkCall0 b sink.EndNest
                text ")"
                sinkCall0 b sink.EndGroup
                sinkCall0 b sink.EndApplication

        let cases = s.Cases
        let n = List.length cases

        match cases with
        | [] ->
            // F# unions always have ≥1 case; the dispatch below indexes
            // `cases.[n - 1]` as the fall-through, so guard the empty case here
            // (mirrors the record `[]` arm — both unreachable in practice, total
            // for safety).
            text "()"
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
