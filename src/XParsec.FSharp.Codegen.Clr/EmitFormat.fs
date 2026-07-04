namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm
open EmitTypes
open EmitLower

/// `TExprG.Format` lowering, lifted out of `EmitExpr`. Self-contained apart from
/// recursing into the expression compiler, which is passed in as `buildExpr`
/// (the node lives behind a ref-struct local + sink, so it can't ride the
/// `CallRecipe` model the rest of the call sites use).
module EmitFormat =
    /// Lower a `TExprG.Format` to the `Vesper.Formatter` write-through handler: a
    /// ref-struct local constructed in place, then each segment folded
    /// left-to-right (`AppendLiteral` for a literal run, `AppendFormatted<T>`
    /// for a hole — its arg evaluated *here*, at its position), then a trailing
    /// newline (printfn-style sinks) and flush, or `ToStringAndClear` for the
    /// string sink. The node yields a value: the `unit` (zero-field
    /// `System.ValueTuple`) of the writing sinks, or the result string of
    /// `sprintf`. Not a `CallRecipe` — the recipe
    /// model can't interleave literals/args around a ref-struct local + sink.
    let buildFormat
        (buildExpr: EmitEnv -> IlBuilder -> Frozen.TExpr -> unit)
        (env: EmitEnv)
        (b: IlBuilder)
        (sink: Frozen.FormatSink)
        (segments: EqArray<Frozen.FormatSeg>)
        : unit =
        let fh = env.Provider.FormatHandles()
        let slot = b.Local fh.HandlerLocal

        // Capacity hints for the ctor; the handler grows past them as needed, so
        // they need not be exact.
        let mutable litLen = 0
        let mutable holeCount = 0

        for seg in segments do
            match seg with
            | FormatSegG.Lit s -> litLen <- litLen + s.Length
            | FormatSegG.Hole _
            | FormatSegG.StarWidthHole _ -> holeCount <- holeCount + 1

        // Construct in place: `ldloca h; ldc litLen; ldc holeCount; <sink?>; call .ctor`.
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.LdcI4 litLen)
        b.Add(ILInstr.LdcI4 holeCount)

        match sink with
        | FormatSinkG.ToString -> b.Add(ILInstr.Call(fh.CtorString, 3, 0))
        | FormatSinkG.ToStdOut _ ->
            b.Add(ILInstr.Call(fh.ConsoleOut, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSinkG.ToStdErr _ ->
            b.Add(ILInstr.Call(fh.ConsoleError, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSinkG.ToWriter(w, _) ->
            buildExpr env b w
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSinkG.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        // Emit one hole's handler call. `starWidthLocal = Some slot` for a star hole
        // (`%*d`/`%*A`): the runtime width has already been guarded (padding) or
        // clamped (`%A`) and spilled to that int local *before* the value expression
        // (curried evaluation order), so the alignment / width-budget operand loads
        // the local instead of a compile-time constant. `None` for an ordinary hole.
        let emitHole (hole: Frozen.HoleSpec) (arg: Frozen.TExpr) (starWidthLocal: int option) =
            // Push the alignment operand for an `Alignment`, returning whether one was
            // pushed. `Star` loads the pre-spilled width local; `Const` a literal;
            // `None` pushes `dflt` when the member always takes an operand (the
            // `AppendBool`/`AppendUnsigned`/`AppendOctal` members), else nothing.
            let pushAlign (align: Alignment) (dflt: int option) : bool =
                match align with
                | Alignment.Star _ ->
                    match starWidthLocal with
                    | Some s ->
                        b.Add(ILInstr.Ldloc s)
                        true
                    | None -> failwith "Emit: star alignment without a spilled width local (invariant broken)"
                | Alignment.Const a ->
                    b.Add(ILInstr.LdcI4 a)
                    true
                | Alignment.None ->
                    match dflt with
                    | Some d ->
                        b.Add(ILInstr.LdcI4 d)
                        true
                    | None -> false

            // `%A`: `AppendStructured<T>(value, widthBudget, sizeBudget)`. The width
            // budget resolves the `%0A`/`%NA`/plain default (80) statically, or — for
            // `%*A` (`PrintWidth.Star`) — loads the clamped runtime width local; the
            // size budget resolves F#'s `PrintSize` node count (`%.NA`, default 10000).
            // The generic member boxes the value C#-side, so no explicit box in the IL.
            let percentA (width: PrintWidth) (size: int option) =
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b arg

                match percentAWidth width with
                | ValueSome w -> b.Add(ILInstr.LdcI4 w)
                | ValueNone ->
                    match starWidthLocal with
                    | Some s -> b.Add(ILInstr.Ldloc s)
                    | None -> failwith "Emit: %*A without a spilled width local (invariant broken)"

                b.Add(ILInstr.LdcI4(percentASize size))
                b.Add(ILInstr.Call(fh.AppendStructured hole.Ty, 4, 0))

            // A non-`%A` hole, emitted from its projected
            // `(HoleKind, .NET-format, Alignment)` triple.
            let field (kind: PrintfSpec.HoleKind) (format: string option) (align: Alignment) =
                match kind with
                | PrintfSpec.HoleKind.Formatted ->
                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg

                    // Push optional args in the C# parameter order: alignment, then format.
                    let hasAlignment = pushAlign align None

                    match format with
                    | Some f -> b.Add(ILInstr.Ldstr(env.Ctx.UserString f))
                    | None -> ()

                    let handle = fh.AppendFormatted(hole.Ty, hasAlignment, format.IsSome)

                    let argc = 2 + (if hasAlignment then 1 else 0) + (if format.IsSome then 1 else 0)

                    b.Add(ILInstr.Call(handle, argc, 0))

                | PrintfSpec.HoleKind.BoolText
                | PrintfSpec.HoleKind.Octal
                | PrintfSpec.HoleKind.Unsigned ->
                    // A dedicated handler member `(value, int alignment)` — no
                    // .NET format string. The alignment is always pushed (0 ⇒ no
                    // padding), from the star width local when present; `%u`'s
                    // `int`→`uint` is a free CLI-stack reinterpret, so the arg is
                    // emitted unchanged.
                    let handle =
                        match kind with
                        | PrintfSpec.HoleKind.BoolText -> fh.AppendBool
                        | PrintfSpec.HoleKind.Octal -> fh.AppendOctal
                        | _ -> fh.AppendUnsigned

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    pushAlign align (Some 0) |> ignore
                    b.Add(ILInstr.Call(handle, 3, 0))

                | PrintfSpec.HoleKind.ZeroPaddedFloat ->
                    // `AppendZeroPaddedFloat(value, "F<prec>", width)` — the
                    // `"F<prec>"` body rides in `format`, the field width in the
                    // alignment slot as a `Const` (both guaranteed present by the
                    // projection; a star never reaches the zero-pad forms).
                    let fmt =
                        match format with
                        | Some f -> f
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its format string"

                    let width =
                        match align with
                        | Alignment.Const w -> w
                        | _ -> failwith "Emit: ZeroPaddedFloat hole missing its width"

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.Ldstr(env.Ctx.UserString fmt))
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(fh.AppendZeroPaddedFloat, 4, 0))

                | PrintfSpec.HoleKind.Structured ->
                    // `toDotNetFormat` only projects `FieldFormat`s, so `Structured`
                    // never reaches here — `%A` is emitted by `percentA` from
                    // `HoleForm.PercentA`.
                    failwith "Emit: %A reached the Field projection (unreachable)"

            match hole.Source with
            | HoleSpecSource.RawFormat fmt ->
                // A `{x:fmt}` interpolation custom-format clause: a verbatim CLR .NET
                // format string with no printf placeholder.
                field PrintfSpec.HoleKind.Formatted fmt Alignment.None
            | HoleSpecSource.Classified(HoleForm.PercentA(width, size)) -> percentA width size
            | HoleSpecSource.Classified(HoleForm.Field(fmt, alignment)) ->
                let kind, format, align = ClrHoleFormat.toDotNetFormat fmt alignment
                field kind format align

        for seg in segments do
            match seg with
            | FormatSegG.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSegG.Hole(hole, arg) -> emitHole hole arg None
            | FormatSegG.StarWidthHole(widthExpr, hole, valueArg) ->
                // Curried application evaluates the width arg *before* the value, but
                // the handler members take the width in the alignment slot *after* the
                // value — so spill the guarded/clamped width to a local first, then emit
                // the value expression. (Invariant: `hole.Source`'s star classification
                // and `widthExpr` were built from the same placeholder.)
                let wLocal = b.Local(FTConst("int", EqArray.empty))
                buildExpr env b widthExpr

                match hole.Source with
                | HoleSpecSource.Classified(HoleForm.Field(_, Alignment.Star leftJustify)) ->
                    // Padding forms: F# throws on a negative width. Guard, then negate
                    // *after* the guard for a `-`-flag left-justify (the members read a
                    // negative alignment as left-justify), so a negative width still
                    // throws rather than silently right-justifying.
                    b.Add(ILInstr.Call(fh.GuardTotalWidth, 1, 1))

                    if leftJustify then
                        b.Add(ILInstr.Un ILOpCode.Neg)
                | HoleSpecSource.Classified(HoleForm.PercentA(PrintWidth.Star, _)) ->
                    // `%*A`: a negative budget renders flat (F# does not throw), so clamp
                    // to 0 rather than guard-throwing.
                    b.Add(ILInstr.Call(fh.ClampWidth, 1, 1))
                | _ -> failwith "Emit: StarWidthHole without a star-carrying spec (invariant broken)"

                b.Add(ILInstr.Stloc wLocal)
                emitHole hole valueArg (Some wLocal)

        match sink with
        | FormatSinkG.ToString ->
            // Leaves the built string on the stack (the `sprintf` result).
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.ToStringAndClear, 1, 1))
        | FormatSinkG.ToStdOut nl
        | FormatSinkG.ToStdErr nl ->
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
        | FormatSinkG.ToWriter(_, nl) ->
            // `fprintfn` appends the trailing `\n` before flushing, exactly as the
            // `ToStdOut`/`ToStdErr nl` sinks do; `fprintf` (`nl = false`) does not.
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
        | FormatSinkG.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"
