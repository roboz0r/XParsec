namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower

/// `TExpr.Format` lowering, lifted out of `EmitExpr`. Self-contained apart from
/// recursing into the expression compiler, which is passed in as `buildExpr`
/// (the node lives behind a ref-struct local + sink, so it can't ride the
/// `CallRecipe` model the rest of the call sites use).
module EmitFormat =
    /// Lower a `TExpr.Format` to the `Vesper.Formatter` write-through handler: a
    /// ref-struct local constructed in place, then each segment folded
    /// left-to-right (`AppendLiteral` for a literal run, `AppendFormatted<T>`
    /// for a hole — its arg evaluated *here*, at its position), then a trailing
    /// newline (printfn-style sinks) and flush, or `ToStringAndClear` for the
    /// string sink. The node yields a value: the `unit` (zero-field
    /// `System.ValueTuple`) of the writing sinks, or the result string of
    /// `sprintf`. Not a `CallRecipe` — the recipe
    /// model can't interleave literals/args around a ref-struct local + sink.
    let buildFormat
        (buildExpr: EmitEnv -> IlBuilder -> TExpr -> unit)
        (env: EmitEnv)
        (b: IlBuilder)
        (sink: FormatSink)
        (segments: EqArray<FormatSeg>)
        : unit =
        let fh = env.Provider.FormatHandles()
        let slot = b.Local fh.HandlerLocal

        // Capacity hints for the ctor; the handler grows past them as needed, so
        // they need not be exact.
        let mutable litLen = 0
        let mutable holeCount = 0

        for seg in segments do
            match seg with
            | FormatSeg.Lit s -> litLen <- litLen + s.Length
            | FormatSeg.Hole _ -> holeCount <- holeCount + 1

        // Construct in place: `ldloca h; ldc litLen; ldc holeCount; <sink?>; call .ctor`.
        b.Add(ILInstr.Ldloca slot)
        b.Add(ILInstr.LdcI4 litLen)
        b.Add(ILInstr.LdcI4 holeCount)

        match sink with
        | FormatSink.ToString -> b.Add(ILInstr.Call(fh.CtorString, 3, 0))
        | FormatSink.ToStdOut _ ->
            b.Add(ILInstr.Call(fh.ConsoleOut, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToStdErr _ ->
            b.Add(ILInstr.Call(fh.ConsoleError, 0, 1))
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToWriter w ->
            buildExpr env b w
            b.Add(ILInstr.Call(fh.CtorWriter, 4, 0))
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        for seg in segments do
            match seg with
            | FormatSeg.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSeg.Hole(hole, arg) ->
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

        match sink with
        | FormatSink.ToString ->
            // Leaves the built string on the stack (the `sprintf` result).
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.ToStringAndClear, 1, 1))
        | FormatSink.ToStdOut nl
        | FormatSink.ToStdErr nl ->
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
        | FormatSink.ToWriter _ ->
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            EmitTypes.buildUnitValue env b
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"
