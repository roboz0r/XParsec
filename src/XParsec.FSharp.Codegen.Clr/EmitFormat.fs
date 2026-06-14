namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
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
            | FormatSegG.Hole _ -> holeCount <- holeCount + 1

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
        | FormatSinkG.ToWriter w ->
            buildExpr env b w
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSinkG.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        for seg in segments do
            match seg with
            | FormatSegG.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSegG.Hole(hole, arg) ->
                match hole.Kind with
                | PrintfSpec.HoleKind.Formatted ->
                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg

                    // Push optional args in the C# parameter order: alignment, then format.
                    match hole.Alignment with
                    | Some a -> b.Add(ILInstr.LdcI4 a)
                    | None -> ()

                    match hole.Format with
                    | Some f -> b.Add(ILInstr.Ldstr(env.Ctx.UserString f))
                    | None -> ()

                    let handle = fh.AppendFormatted(hole.Ty, hole.Alignment.IsSome, hole.Format.IsSome)

                    let argc =
                        2
                        + (if hole.Alignment.IsSome then 1 else 0)
                        + (if hole.Format.IsSome then 1 else 0)

                    b.Add(ILInstr.Call(handle, argc, 0))

                | PrintfSpec.HoleKind.BoolText
                | PrintfSpec.HoleKind.Octal
                | PrintfSpec.HoleKind.Unsigned ->
                    // A dedicated handler member `(value, int alignment)` — no
                    // .NET format string. The alignment is always pushed (0 ⇒ no
                    // padding); `%u`'s `int`→`uint` is a free CLI-stack
                    // reinterpret, so the arg is emitted unchanged.
                    let handle =
                        match hole.Kind with
                        | PrintfSpec.HoleKind.BoolText -> fh.AppendBool
                        | PrintfSpec.HoleKind.Octal -> fh.AppendOctal
                        | _ -> fh.AppendUnsigned

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.LdcI4(defaultArg hole.Alignment 0))
                    b.Add(ILInstr.Call(handle, 3, 0))

                | PrintfSpec.HoleKind.ZeroPaddedFloat ->
                    // `AppendZeroPaddedFloat(value, "F<prec>", width)` — the
                    // `"F<prec>"` body rides in `Format`, the field width in
                    // `Alignment` (both guaranteed present by `tryHoleFormat`).
                    let fmt =
                        match hole.Format with
                        | Some f -> f
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its format string"

                    let width =
                        match hole.Alignment with
                        | Some w -> w
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its width"

                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.Ldstr(env.Ctx.UserString fmt))
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(fh.AppendZeroPaddedFloat, 4, 0))

                | PrintfSpec.HoleKind.Structured ->
                    // `AppendStructured<T>(value, widthBudget, sizeBudget)` — `%A`.
                    // The print-WIDTH budget rides in `PercentAWidthBudget` (the
                    // `Alignment` slot): `None` ⇒ default 80, `Some 0` ⇒ flat
                    // (`%0A`), `Some N` ⇒ N. The print-SIZE budget (F#'s `PrintSize`)
                    // rides in `PercentASizeBudget` (the `Format` slot): `None` ⇒
                    // default 10000, `Some N` ⇒ N (`%.NA`). The generic member boxes
                    // the value C#-side, so no explicit box in the IL.
                    b.Add(ILInstr.Ldloca slot)
                    buildExpr env b arg
                    b.Add(ILInstr.LdcI4(defaultArg hole.PercentAWidthBudget 80))
                    b.Add(ILInstr.LdcI4(defaultArg hole.PercentASizeBudget 10000))
                    b.Add(ILInstr.Call(fh.AppendStructured hole.Ty, 4, 0))

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
        | FormatSinkG.ToWriter _ ->
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
        | FormatSinkG.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"
