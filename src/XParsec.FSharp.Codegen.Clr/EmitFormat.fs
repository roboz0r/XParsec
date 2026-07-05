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
            | FormatSegG.DynHole _ -> holeCount <- holeCount + 1

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
        | FormatSinkG.ToBuilder sb ->
            buildExpr env b sb
            b.Add(ILInstr.Call(fh.CtorBuilder, 4, 0))

        // A float field form whose precision is a runtime star (`%.*f`/`%.*e`/`%.*g`/
        // `%+.*f`): the source type letter + whether it is forced-sign (`Some space`).
        // Any other form (static precision, or non-float) returns `None` and lowers via
        // the static `toDotNetFormat` projection instead.
        let dynFloatOf (fmt: FieldFormat) : (char * bool option) option =
            match fmt with
            | FieldFormat.Fixed Prec.Star -> Some('f', None)
            | FieldFormat.Exponential(Prec.Star, upper) -> Some((if upper then 'E' else 'e'), None)
            | FieldFormat.Compact(Prec.Star, upper) -> Some((if upper then 'G' else 'g'), None)
            | FieldFormat.ForcedSign(space, Prec.Star, typeChar, _) -> Some(typeChar, Some space)
            | _ -> None

        // A forced-sign scientific / compact float at a *static* precision
        // (`%+e`/`% e`/`%+g`/`%+G`): `(typeChar, space, precision)`. Scientific / compact
        // notation can't ride a .NET section format, so it routes to the signed dynamic
        // handler with a constant precision (unlike the fixed `'d'`/`'f'` forms, which
        // project to a section format via `toDotNetFormat`).
        let constSignedExpCompact (fmt: FieldFormat) : (char * bool * int) option =
            match fmt with
            | FieldFormat.ForcedSign(space, Prec.Const n, typeChar, Option.None) when
                typeChar = 'e' || typeChar = 'E' || typeChar = 'g' || typeChar = 'G'
                ->
                Some(typeChar, space, n)
            | _ -> None

        // Emit one hole's handler call. `starWidthLocal`/`starPrecLocal = Some slot` for
        // a hole whose width (`%*d`/`%*A`) / precision (`%.*f`/`%.*A`) is a runtime star:
        // the value has already been guarded/clamped/normalized and spilled to that int
        // local *before* the value expression (curried evaluation order: width, then
        // precision, then value), so the alignment / width-budget / precision operand
        // loads the local instead of a compile-time constant. `None` for a dim that is
        // static (or a plain hole).
        let emitHole
            (hole: Frozen.HoleSpec)
            (arg: Frozen.TExpr)
            (starWidthLocal: int option)
            (starPrecLocal: int option)
            =
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
            let percentA (width: PrintWidth) (size: PrintSize) =
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b arg

                match percentAWidth width with
                | ValueSome w -> b.Add(ILInstr.LdcI4 w)
                | ValueNone ->
                    match starWidthLocal with
                    | Some s -> b.Add(ILInstr.Ldloc s)
                    | None -> failwith "Emit: %*A without a spilled width local (invariant broken)"

                match percentASize size with
                | ValueSome sz -> b.Add(ILInstr.LdcI4 sz)
                | ValueNone ->
                    match starPrecLocal with
                    | Some p -> b.Add(ILInstr.Ldloc p)
                    | None -> failwith "Emit: %.*A without a spilled precision local (invariant broken)"

                b.Add(ILInstr.Call(fh.AppendStructured hole.Ty, 4, 0))

            // A float field form routed through the dynamic-precision member: a *runtime*
            // precision (`%.*f`/`%.*e`/`%.*g`/`%+.*f` — `pushPrec` loads the spilled
            // precision local) or a forced-sign scientific / compact form at a *static*
            // precision (`%+e`/`%+g` — `pushPrec` a constant, since notation can't ride a
            // section format). Pushes (value, typeChar, precision, alignment[, space]).
            // `alignment` is the width slot — a `Star` loads the spilled width local, a
            // `Const` a literal, `None` 0.
            let dynamicFloat (typeChar: char) (signedSpace: bool option) (align: Alignment) (pushPrec: unit -> unit) =
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b arg
                b.Add(ILInstr.LdcI4(int typeChar))
                pushPrec ()
                pushAlign align (Some 0) |> ignore

                match signedSpace with
                | Option.None -> b.Add(ILInstr.Call(fh.AppendDynamicPrecisionFloat, 5, 0))
                | Option.Some space ->
                    b.Add(ILInstr.LdcI4(if space then 1 else 0))
                    b.Add(ILInstr.Call(fh.AppendDynamicPrecisionSignedFloat, 6, 0))

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

                | PrintfSpec.HoleKind.OctalZeroPad
                | PrintfSpec.HoleKind.UnsignedZeroPad ->
                    // `AppendZeroPadded{Octal,Unsigned}(value, width)` — the field
                    // width rides in the alignment slot as a `Const` (the projection
                    // guarantees it; a star never reaches the zero-pad forms). `%u`'s
                    // `int`→`uint` is a free CLI-stack reinterpret, so the arg is
                    // emitted unchanged, like `Unsigned`.
                    let handle =
                        match kind with
                        | PrintfSpec.HoleKind.OctalZeroPad -> fh.AppendZeroPaddedOctal
                        | _ -> fh.AppendZeroPaddedUnsigned

                    let width =
                        match align with
                        | Alignment.Const w -> w
                        | _ -> failwith "Emit: zero-pad octal/unsigned hole missing its width"

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(handle, 3, 0))

                | PrintfSpec.HoleKind.ZeroPaddedFloat
                | PrintfSpec.HoleKind.RightZeroPaddedFloat ->
                    // `AppendZeroPaddedFloat(value, body, width)` (`%0w.pf`, and the
                    // scientific / compact `%014e`/`%010g` over the `"e6"`/`"g6"` body)
                    // zero-pads *after any sign*; `AppendRightZeroPaddedFloat` (`%-0w.pf`)
                    // pads on the RIGHT instead. Both take the format body in `format` and
                    // the field width in the alignment slot as a `Const` (guaranteed by the
                    // projection; a star never reaches the zero-pad forms).
                    let fmt =
                        match format with
                        | Some f -> f
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its format string"

                    let width =
                        match align with
                        | Alignment.Const w -> w
                        | _ -> failwith "Emit: ZeroPaddedFloat hole missing its width"

                    let handle =
                        match kind with
                        | PrintfSpec.HoleKind.RightZeroPaddedFloat -> fh.AppendRightZeroPaddedFloat
                        | _ -> fh.AppendZeroPaddedFloat

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.Ldstr(env.Ctx.UserString fmt))
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(handle, 4, 0))

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
                match dynFloatOf fmt, starPrecLocal with
                | Some(typeChar, signedSpace), Some precLocal ->
                    dynamicFloat typeChar signedSpace alignment (fun () -> b.Add(ILInstr.Ldloc precLocal))
                | Some _, None ->
                    failwith "Emit: runtime-precision float field without a spilled precision local (invariant broken)"
                | Option.None, _ ->
                    match constSignedExpCompact fmt with
                    | Some(typeChar, space, prec) ->
                        dynamicFloat typeChar (Some space) alignment (fun () -> b.Add(ILInstr.LdcI4 prec))
                    | Option.None ->
                        let kind, format, align = ClrHoleFormat.toDotNetFormat fmt alignment
                        field kind format align

        for seg in segments do
            match seg with
            | FormatSegG.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSegG.Hole(hole, arg) -> emitHole hole arg None None
            | FormatSegG.DynHole d ->
                // Curried application evaluates the dimension args *before* the value,
                // but the handler members take them *after* the value — so spill each
                // present dim (width first, then precision) to a local, then emit the
                // value. (Invariant: `d.Spec.Source`'s star classification agrees with
                // which of `d.Width`/`d.Precision` are present — same placeholder.)
                let wLocal =
                    match d.Width with
                    | ValueNone -> None
                    | ValueSome widthExpr ->
                        let l = b.Local(FTConst("int", EqArray.empty))
                        buildExpr env b widthExpr

                        let form =
                            match d.Spec.Source with
                            | HoleSpecSource.Classified f -> f
                            | _ -> failwith "Emit: DynHole star width on an unclassified hole (invariant broken)"

                        match starWidthClamp form with
                        | ValueSome StarWidthClamp.Guard ->
                            // Padding forms: F# throws on a negative width. Guard, then
                            // negate *after* the guard for a `-`-flag left-justify (the
                            // members read a negative alignment as left-justify), so a
                            // negative width still throws rather than right-justifying.
                            b.Add(ILInstr.Call(fh.GuardTotalWidth, 1, 1))

                            match form with
                            | HoleForm.Field(_, Alignment.Star true) -> b.Add(ILInstr.Un ILOpCode.Neg)
                            | _ -> ()
                        | ValueSome StarWidthClamp.Clamp ->
                            // `%*A`: a negative budget renders flat (F# does not throw),
                            // so clamp to 0 rather than guard-throwing.
                            b.Add(ILInstr.Call(fh.ClampWidth, 1, 1))
                        | ValueNone ->
                            failwith "Emit: DynHole star width without a star-carrying spec (invariant broken)"

                        b.Add(ILInstr.Stloc l)
                        Some l

                let pLocal =
                    match d.Precision with
                    | ValueNone -> None
                    | ValueSome precExpr ->
                        let l = b.Local(FTConst("int", EqArray.empty))
                        buildExpr env b precExpr

                        // `normalizePrecision` (clamp 0..99) applies ONLY on the two-star
                        // float-field path (`printf.fs:632`); the prec-star-only paths keep
                        // the raw precision (`:649-657`) and `%A` sets `PrintSize` raw
                        // (`:1114`). The shared classifier owns that rule.
                        match d.Spec.Source with
                        | HoleSpecSource.Classified form when normalizesStarPrecision form ->
                            b.Add(ILInstr.Call(fh.NormalizePrecision, 1, 1))
                        | _ -> ()

                        b.Add(ILInstr.Stloc l)
                        Some l

                emitHole d.Spec d.Value wLocal pLocal

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
        | FormatSinkG.ToBuilder _ ->
            // `bprintf` has no newline variant, so no trailing `\n` — just flush the
            // buffered text to the `StringBuilder` sink and yield unit.
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
