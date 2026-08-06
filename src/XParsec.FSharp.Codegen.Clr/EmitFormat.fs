namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm
open EmitTypes
open EmitLower

module EmitFormat =
    /// Lower a `Format` node against the `Vesper.Formatter` ref-struct handler:
    /// construct it in place, fold the segments left-to-right (each hole's arg
    /// evaluated at its position), then flush — or `ToStringAndClear` for `sprintf`.
    let buildFormat
        (buildExpr: EmitEnv -> IlBuilder -> TastAccessor.ExprId -> unit)
        (env: EmitEnv)
        (b: IlBuilder)
        (sink: TastAccessor.FormatSink)
        (segments: TastAccessor.FormatSeg[])
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
            | FormatSegG.DynHole _
            | FormatSegG.CallbackHole _ -> holeCount <- holeCount + 1

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
        // `%+.*f`): its type letter, plus `Some space` when the form is forced-sign.
        let dynFloatOf (fmt: FieldFormat) : (char * bool option) option =
            match fmt with
            | FieldFormat.Fixed Prec.Star -> Some('f', None)
            | FieldFormat.Exponential(Prec.Star, upper) -> Some((if upper then 'E' else 'e'), None)
            | FieldFormat.Compact(Prec.Star, upper) -> Some((if upper then 'G' else 'g'), None)
            | FieldFormat.ForcedSign(space, Prec.Star, typeChar, _) -> Some(typeChar, Some space)
            | _ -> None

        // A forced-sign float at a *static* precision, no zero-pad (`%+.Nf`/`%+e`/`%+G`).
        // It still routes to the signed dynamic handler: that rounds half-to-even via a
        // `"F<prec>"` body, where a .NET *section* format would round half-away.
        let constSignedFloat (fmt: FieldFormat) : (char * bool * int) option =
            match fmt with
            | FieldFormat.ForcedSign(space, Prec.Const n, typeChar, Option.None) when
                typeChar = 'f'
                || typeChar = 'e'
                || typeChar = 'E'
                || typeChar = 'g'
                || typeChar = 'G'
                ->
                Some(typeChar, space, n)
            | _ -> None

        // A forced-sign fixed float that *also* zero-pads (`%+08.2f`/`% 08.2f`): the
        // sign is composed first, then the zero-padding fills after it to `width`.
        let constSignedZeroPadFloat (fmt: FieldFormat) : (bool * string * int) option =
            match fmt with
            | FieldFormat.ForcedSign(space, Prec.Const n, 'f', Option.Some w) -> Some(space, "F" + string n, w)
            | _ -> None

        // Emit one hole's handler call. A `Some slot` marks a runtime star (`%*d`,
        // `%.*f`) whose value was guarded and spilled to that int local BEFORE the value
        // expression, so the operand loads the local rather than a constant.
        let emitHole
            (hole: Pooled.HoleSpec)
            (arg: TastAccessor.ExprId)
            (starWidthLocal: int option)
            (starPrecLocal: int option)
            =
            // Push the alignment operand, returning whether one was pushed. `None`
            // pushes `dflt` for the members that always take an operand
            // (`AppendBool` / `AppendUnsigned` / `AppendOctal`), else nothing.
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

            // `%A`: `AppendStructured<T>(value, widthBudget, sizeBudget)`. Each budget
            // resolves statically (`%NA` / `%.NA`), or loads the runtime local spilled
            // for `%*A` / `%.*A`.
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

            // The dynamic-precision float member — either a runtime precision (`%.*f`,
            // `pushPrec` loads the spilled local) or a forced-sign `%+e`/`%+g` at a
            // static one. Pushes (value, typeChar, precision, alignment[, space]).
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

            // `%+08.2f`: format the `"F<prec>"` body, force the sign, then zero-pad after
            // it. The width rides inside the `FieldFormat`, not in an alignment slot.
            let forcedSignZeroPadFloat (space: bool) (body: string) (width: int) =
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b arg
                b.Add(ILInstr.Ldstr(env.Ctx.UserString body))
                b.Add(ILInstr.LdcI4 width)
                b.Add(ILInstr.LdcI4(if space then 1 else 0))
                b.Add(ILInstr.Call(fh.AppendForcedSignZeroPaddedFloat, 5, 0))

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
                    // A dedicated `(value, int alignment)` member, no .NET format string;
                    // alignment `0` means no padding. `%u`'s `int`→`uint` is a free
                    // CLI-stack reinterpret, so the arg is emitted unchanged.
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
                    // `AppendZeroPadded{Octal,Unsigned}(value, width)` — the field width
                    // rides in the alignment slot as a `Const`, since a star never
                    // reaches the zero-pad forms.
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
                    // `AppendZeroPaddedFloat(value, body, width)` (`%0w.pf`, `%014e`)
                    // zero-pads AFTER any sign; `%-0w.pf` pads on the right instead. The
                    // width rides in the alignment slot as a `Const`, never a star.
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

                | PrintfSpec.HoleKind.Structured -> failwith "Emit: %A reached the Field projection (unreachable)"

            match hole.Source with
            | HoleSpecSource.RawFormat fmt ->
                // A `{x:fmt}` interpolation custom-format clause: a verbatim CLR .NET
                // format string with no printf placeholder.
                field PrintfSpec.HoleKind.Formatted fmt Alignment.None
            | HoleSpecSource.Classified(HoleForm.Callback _) ->
                // `%a`/`%t` ride a `CallbackHole` segment whose residue string is spliced
                // directly; a callback spec's `HoleForm` is provenance only.
                failwith "Emit: callback hole reached the Field projection (unreachable)"
            | HoleSpecSource.Classified(HoleForm.PercentA(width, size)) -> percentA width size
            | HoleSpecSource.Classified(HoleForm.Field(fmt, alignment)) ->
                match dynFloatOf fmt, starPrecLocal with
                | Some(typeChar, signedSpace), Some precLocal ->
                    dynamicFloat typeChar signedSpace alignment (fun () -> b.Add(ILInstr.Ldloc precLocal))
                | Some _, None ->
                    failwith "Emit: runtime-precision float field without a spilled precision local (invariant broken)"
                | Option.None, _ ->
                    match constSignedZeroPadFloat fmt with
                    | Some(space, body, width) -> forcedSignZeroPadFloat space body width
                    | Option.None ->
                        match constSignedFloat fmt with
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
            | FormatSegG.CallbackHole(_, residue) ->
                // `%a`/`%t`: the callback (and any scratch sink) was already lowered to an
                // ordinary residue-*string* expr, so splice it exactly like a literal —
                // codegen has no sink knowledge.
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b residue
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSegG.DynHole d ->
                // Curried application evaluates the dimension args BEFORE the value, but
                // the handler members take them AFTER it — so spill each present dim
                // (width first, then precision) to a local, then emit the value.
                let wLocal =
                    match d.Width with
                    | ValueNone -> None
                    | ValueSome widthExpr ->
                        let l = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
                        buildExpr env b widthExpr

                        let form =
                            match d.Spec.Source with
                            | HoleSpecSource.Classified f -> f
                            | _ -> failwith "Emit: DynHole star width on an unclassified hole (invariant broken)"

                        match starWidthClamp form with
                        | ValueSome StarWidthClamp.Guard ->
                            // The `-`-flag left-justify negates AFTER the guard (the
                            // members read a negative alignment as left-justify), so a
                            // negative runtime width still throws.
                            b.Add(ILInstr.Call(fh.GuardTotalWidth, 1, 1))

                            match form with
                            | HoleForm.Field(_, Alignment.Star true) -> b.Add(ILInstr.Un ILOpCode.Neg)
                            | _ -> ()
                        | ValueSome StarWidthClamp.Clamp -> b.Add(ILInstr.Call(fh.ClampWidth, 1, 1))
                        | ValueNone ->
                            failwith "Emit: DynHole star width without a star-carrying spec (invariant broken)"

                        b.Add(ILInstr.Stloc l)
                        Some l

                let pLocal =
                    match d.Precision with
                    | ValueNone -> None
                    | ValueSome precExpr ->
                        let l = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
                        buildExpr env b precExpr

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
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
        | FormatSinkG.ToBuilder _ ->
            // `bprintf` has no newline variant — no trailing `\n`, just flush.
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
