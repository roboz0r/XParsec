namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm
open ClrHoleFormat
open EmitTypes
open EmitLower
open EmitDispatch

module EmitFormat =
    /// The opcode that truncates an `int32`-slot value to `k`'s own bits, `ValueNone` where
    /// the slot is already exactly the width. A narrower value's upper bits carry whatever
    /// the producing opcode left: `%u` of `~~~200uy` is `55`, not `4294967095`.
    let private truncateToOwnWidth (k: IntKind) : ILOpCode voption =
        match k with
        | IntKind.SByte
        | IntKind.Byte -> ValueSome ILOpCode.Conv_u1
        | IntKind.Int16
        | IntKind.UInt16 -> ValueSome ILOpCode.Conv_u2
        | IntKind.Int32
        | IntKind.UInt32
        | IntKind.Int64
        | IntKind.UInt64
        | IntKind.NativeInt
        | IntKind.UNativeInt -> ValueNone

    /// Widen the integer on the stack to its own-width bits zero-extended to 64, the normal
    /// form `%u` and `%o` render. `conv.u8` zero-extends from `int32` and from a native int.
    let private widenToUnsigned64 (b: IlBuilder) (ty: FrozenType) =
        let width =
            match ty with
            | FTConst(k, _) -> RuntimeNames.intKindOfKey k
            | _ -> ValueNone

        match width with
        | ValueNone -> failwithf "Emit: %%u/%%o hole at the non-integer type %A (invariant broken)" ty
        | ValueSome k ->
            match truncateToOwnWidth k with
            | ValueSome op -> b.Add(ILInstr.Un op)
            | ValueNone -> ()

        b.Add(ILInstr.Un ILOpCode.Conv_u8)

    /// Lower a `Format` node against the `Vesper.Formatter` ref-struct handler:
    /// construct it in place, fold the segments left-to-right (each hole's arg
    /// evaluated at its position), then flush, or `ToStringAndClear` for `sprintf`.
    let buildFormat
        (buildExpr: Recur)
        (pos: ExprPos)
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

        // Emit one hole's handler call. A `Some slot` marks a runtime star (`%*d`,
        // `%.*f`) whose value was guarded and spilled to that int local BEFORE the value
        // expression, so the operand loads the local rather than a constant.
        let emitHole
            (hole: Pooled.HoleSpec)
            (arg: TastAccessor.ExprId)
            (starWidthLocal: int option)
            (starPrecLocal: int option)
            =
            // Every append member takes the handler byref, then the value.
            let pushValue () =
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b arg

            // The alignment operand. `Alignment.None` pushes `dflt` for the members that
            // always take one (`0` meaning no padding), and nothing for the
            // `AppendFormatted` overload that omits the parameter.
            let pushAlign (align: Alignment) (dflt: int option) : unit =
                match align with
                | Alignment.Star _ ->
                    match starWidthLocal with
                    | Some s -> b.Add(ILInstr.Ldloc s)
                    | None -> failwith "Emit: star alignment without a spilled width local (invariant broken)"
                | Alignment.Const a -> b.Add(ILInstr.LdcI4 a)
                | Alignment.None ->
                    match dflt with
                    | Some d -> b.Add(ILInstr.LdcI4 d)
                    | None -> ()

            // The precision operand of a dynamic-precision float: a static digit count, or
            // the runtime `int` normalised and spilled before the value.
            let pushPrec (prec: Prec) : unit =
                match prec with
                | Prec.Const n -> b.Add(ILInstr.LdcI4 n)
                | Prec.Star ->
                    match starPrecLocal with
                    | Some p -> b.Add(ILInstr.Ldloc p)
                    | None ->
                        failwith "Emit: runtime-precision float without a spilled precision local (invariant broken)"

            // `AppendZeroPaddedFloat` / `AppendRightZeroPaddedFloat` share the shape
            // `(value, body, width)`.
            let floatZeroPad (m: GenericAppend) (body: string) (width: int) =
                pushValue ()
                b.Add(ILInstr.Ldstr(env.Ctx.UserString body))
                b.Add(ILInstr.LdcI4 width)
                b.Add(ILInstr.Call(fh.AppendGeneric(m, hole.Ty), 4, 0))

            // `%A`: `AppendStructured<T>(value, widthBudget, sizeBudget)`. Each budget
            // resolves statically (`%NA` / `%.NA`), or loads the runtime local spilled
            // for `%*A` / `%.*A`.
            let percentA (width: PrintWidth) (size: PrintSize) =
                pushValue ()

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

                b.Add(ILInstr.Call(fh.AppendGeneric(GenericAppend.Structured, hole.Ty), 4, 0))

            // A non-`%A` hole: push the operands its projected `HoleCall` carries, then call.
            let call (c: HoleCall) =
                match c with
                | HoleCall.Formatted(format, align) ->
                    // The overload is the one whose optional parameters are present, pushed
                    // in the C# declaration order: alignment, then format.
                    let hasAlignment = align <> Alignment.None
                    pushValue ()
                    pushAlign align Option.None

                    match format with
                    | Some f -> b.Add(ILInstr.Ldstr(env.Ctx.UserString f))
                    | Option.None -> ()

                    let handle =
                        fh.AppendGeneric(GenericAppend.Formatted(hasAlignment, format.IsSome), hole.Ty)

                    let argc = 2 + (if hasAlignment then 1 else 0) + (if format.IsSome then 1 else 0)

                    b.Add(ILInstr.Call(handle, argc, 0))

                | HoleCall.BoolText align ->
                    pushValue ()
                    pushAlign align (Some 0)
                    b.Add(ILInstr.Call(fh.AppendBool, 3, 0))

                | HoleCall.Octal align ->
                    pushValue ()
                    widenToUnsigned64 b hole.Ty
                    pushAlign align (Some 0)
                    b.Add(ILInstr.Call(fh.AppendOctal, 3, 0))

                | HoleCall.Unsigned align ->
                    pushValue ()
                    widenToUnsigned64 b hole.Ty
                    pushAlign align (Some 0)
                    b.Add(ILInstr.Call(fh.AppendUnsigned, 3, 0))

                | HoleCall.OctalZeroPad width ->
                    pushValue ()
                    widenToUnsigned64 b hole.Ty
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(fh.AppendZeroPaddedOctal, 3, 0))

                | HoleCall.UnsignedZeroPad width ->
                    pushValue ()
                    widenToUnsigned64 b hole.Ty
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.Call(fh.AppendZeroPaddedUnsigned, 3, 0))

                | HoleCall.ZeroPaddedFloat(body, width) -> floatZeroPad GenericAppend.ZeroPaddedFloat body width

                | HoleCall.RightZeroPaddedFloat(body, width) ->
                    floatZeroPad GenericAppend.RightZeroPaddedFloat body width

                | HoleCall.ForcedSignZeroPaddedFloat(body, width, space) ->
                    pushValue ()
                    b.Add(ILInstr.Ldstr(env.Ctx.UserString body))
                    b.Add(ILInstr.LdcI4 width)
                    b.Add(ILInstr.LdcI4(if space then 1 else 0))
                    b.Add(ILInstr.Call(fh.AppendGeneric(GenericAppend.ForcedSignZeroPaddedFloat, hole.Ty), 5, 0))

                | HoleCall.DynamicPrecisionFloat(typeChar, prec, align) ->
                    pushValue ()
                    b.Add(ILInstr.LdcI4(int typeChar))
                    pushPrec prec
                    pushAlign align (Some 0)
                    b.Add(ILInstr.Call(fh.AppendGeneric(GenericAppend.DynamicPrecisionFloat, hole.Ty), 5, 0))

                | HoleCall.DynamicPrecisionSignedFloat(typeChar, prec, align, space) ->
                    pushValue ()
                    b.Add(ILInstr.LdcI4(int typeChar))
                    pushPrec prec
                    pushAlign align (Some 0)
                    b.Add(ILInstr.LdcI4(if space then 1 else 0))
                    b.Add(ILInstr.Call(fh.AppendGeneric(GenericAppend.DynamicPrecisionSignedFloat, hole.Ty), 6, 0))

            match hole.Source with
            | HoleSpecSource.RawFormat fmt ->
                // A `{x:fmt}` interpolation custom-format clause: a verbatim CLR .NET
                // format string with no printf placeholder.
                call (HoleCall.Formatted(fmt, Alignment.None))
            | HoleSpecSource.Classified(HoleForm.Callback _) ->
                // `%a`/`%t` arrive as a `CallbackHole` segment whose residue string is
                // spliced directly; a callback spec's `HoleForm` is provenance only.
                failwith "Emit: callback hole reached the field projection (unreachable)"
            | HoleSpecSource.Classified(HoleForm.PercentA(width, size)) -> percentA width size
            | HoleSpecSource.Classified(HoleForm.Field(fmt, alignment)) -> call (holeCall fmt alignment)

        for seg in segments do
            match seg with
            | FormatSegG.Lit s ->
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString s))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSegG.Hole(hole, arg) -> emitHole hole arg None None
            | FormatSegG.CallbackHole(_, residue) ->
                // `%a`/`%t`: the callback (and any scratch sink) was already lowered to an
                // ordinary residue-*string* expr, so splice it exactly like a literal.
                b.Add(ILInstr.Ldloca slot)
                buildExpr env b residue
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))
            | FormatSegG.DynHole d ->
                // Curried application evaluates the dimension args BEFORE the value, but
                // the handler members take them AFTER it, so spill each present dim
                // (width first, then precision) to a local, then emit the value.
                let wLocal =
                    match d.Width with
                    | ValueNone -> None
                    | ValueSome widthExpr ->
                        let l = b.Local(RuntimeNames.intTy)
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
                        let l = b.Local(RuntimeNames.intTy)
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
            ExprPos.reifyUnit env b pos
        | FormatSinkG.ToWriter(_, nl) ->
            if nl then
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Ldstr(env.Ctx.UserString "\n"))
                b.Add(ILInstr.Call(fh.AppendLiteral, 2, 0))

            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            ExprPos.reifyUnit env b pos
        | FormatSinkG.ToBuilder _ ->
            // `bprintf` has no newline variant, so no trailing `\n`, just flush.
            b.Add(ILInstr.Ldloca slot)
            b.Add(ILInstr.Call(fh.Flush, 1, 0))
            ExprPos.reifyUnit env b pos
