namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm
open JsEmitHelpers
open EmitJsContext

/// Printf/format lowering: `%`-specifiers and `$N` templates expanded to inline `JsExpr`.
module EmitJsFormat =

    /// Build the JS expression a format hole's argument contributes: the specifier's
    /// formatting inlined from the hole's typed fields, so no format string reaches runtime.
    let buildHole
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (hole: Pooled.HoleSpec)
        (operand: TastAccessor.ExprId)
        (starWidth: TastAccessor.ExprId voption)
        (starPrecision: TastAccessor.ExprId voption)
        : JsExpr =
        let num (n: int) =
            JsExpr.Literal(JsLiteral.Number(string n), ValueNone)

        let str (s: string) =
            JsExpr.Literal(JsLiteral.String s, ValueNone)

        let id (s: string) = JsExpr.Identifier(s, ValueNone)
        let call callee args = JsExpr.Call(callee, args, ValueNone)
        // A method call `objArg.m(args…)`.
        let invoke (objArg: JsExpr) (m: string) (args: JsExpr list) =
            JsExpr.Call(JsExpr.Member(objArg, id m, false, ValueNone), args, ValueNone)
        // Parenthesise a bare numeric-literal `.method` object argument, because `5.toFixed(0)` is a
        // JS syntax error and `-3.14.toFixed(2)` mis-binds as `-(3.14.toFixed(2))`.
        let objArg (e: JsExpr) : JsExpr =
            match e with
            | JsExpr.Literal _ -> JsExpr.Sequence([ e ], ValueNone)
            | _ -> e

        // `Some w` ⇒ right-justify, `Some -w` ⇒ left. A zero-pad form carries its width
        // inside the `FieldFormat` and classifies as alignment `None`, so this is a no-op.
        let withAlign (alignment: int option) (e: JsExpr) : JsExpr =
            match alignment with
            | Some a when a >= 0 -> invoke e "padStart" [ num a ]
            | Some a -> invoke e "padEnd" [ num (-a) ]
            | None -> e

        // Splice the operand into `build value`, for forms that read it a single time.
        let direct (build: JsExpr -> JsExpr) : JsExpr = build (buildExpr ctx operand)

        // Bind the operand to `v` in an arrow IIFE, `((v) => build(v))(operand)`, so a form
        // reading it more than once still evaluates it once: `%05d (f ())` calls `f` once.
        let iife (build: JsExpr -> JsExpr) : JsExpr =
            call (JsExpr.Arrow([ "v" ], JsFnBody.Expr(build (id "v")), ValueNone)) [ buildExpr ctx operand ]

        // `((s) => build(s))(inner)` — binds the intermediate *string* so a sign-aware float
        // form can inspect it (`s.startsWith("-")`) without rebuilding it.
        let strBind (inner: JsExpr) (build: JsExpr -> JsExpr) : JsExpr =
            call (JsExpr.Arrow([ "s" ], JsFnBody.Expr(build (id "s")), ValueNone)) [ inner ]

        // `%*d`: pad an already-string expression to the runtime width bound to `w`. The
        // enclosing arrow has already thrown on a negative `w`, so no clamp is needed here.
        let padDyn (leftJustify: bool) (e: JsExpr) : JsExpr =
            if leftJustify then
                invoke e "padEnd" [ id "w" ]
            else
                invoke e "padStart" [ id "w" ]

        // `"-3.14".padStart(8, "0")` would give `"000-3.14"`, so a negative body pads its
        // digits and re-prefixes the sign: `%08.2f` -3.14 ⇒ `"-0003.14"`.
        let zeroPadAfterSign (width: int) (s: JsExpr) : JsExpr =
            let signed =
                JsExpr.Binary(
                    "+",
                    str "-",
                    invoke (invoke s "slice" [ num 1 ]) "padStart" [ num (width - 1); str "0" ],
                    ValueNone
                )

            JsExpr.Conditional(
                invoke s "startsWith" [ str "-" ],
                signed,
                invoke s "padStart" [ num width; str "0" ],
                ValueNone
            )

        // A float form's precision: a static literal, or `%.*f`'s runtime `p`, bound outside.
        let precJs (p: Prec) : JsExpr =
            match p with
            | Prec.Const n -> num n
            | Prec.Star -> id "p"

        // `toPrecision` requires ≥ 1 significant digit, so a `%.0g` would throw a RangeError.
        let precForToPrecision (p: Prec) : JsExpr =
            match p with
            | Prec.Const n -> num (max 1 n)
            | Prec.Star -> invoke (id "Math") "max" [ num 1; id "p" ]

        // A float32 is a JS double, so shortest-round-trip rendering needs a search loop. This
        // is the one specifier that calls the `Vesper.Printf` runtime instead of inlining.
        let float32FmtRef () =
            JsExpr.Identifier(JsImports.addRef ctx.Imports float32ToStringRef, ValueNone)

        // `wrap` is the field-width pad. Its absence only matters to `Verbatim`: a bare `%d`
        // passes the raw operand through un-stringified, for the surrounding concat to coerce.
        let emitField (fmt: FieldFormat) (wrap: (JsExpr -> JsExpr) option) : JsExpr =
            let wrapped = defaultArg wrap (fun e -> e)

            match fmt with
            // `%d`/`%s`/`%O`/`%c`/`%M`: stringify by the operand's STATIC F# type, not its JS
            // runtime type. An `int64` is a `bigint` (`console.log` prints `5n`) and a
            // `float32` a double, so those two render explicitly even with no field width.
            | FieldFormat.Verbatim ->
                match plainRenderOf ctx hole.Ty with
                | PlainRender.Native ->
                    match wrap with
                    | Option.Some w -> w (direct (fun v -> call (id "String") [ v ]))
                    | Option.None -> buildExpr ctx operand
                | PlainRender.BigInt -> wrapped (direct (fun v -> call (id "String") [ v ]))
                | PlainRender.Single -> wrapped (direct (fun v -> call (float32FmtRef ()) [ v ]))
            // `%0wd`: zeros pad *after* the sign, as .NET `"D5"` does. `%05d` of `-42` ⇒
            // `"-00042"`. Reads the value three times, hence the IIFE.
            | FieldFormat.DecimalZeroPad width ->
                iife (fun v ->
                    let signStr =
                        JsExpr.Conditional(JsExpr.Binary("<", v, num 0, ValueNone), str "-", str "", ValueNone)

                    let digits =
                        invoke
                            (invoke (invoke (id "Math") "abs" [ v ]) "toString" [])
                            "padStart"
                            [ num width; str "0" ]

                    JsExpr.Binary("+", signStr, digits, ValueNone)
                )
            // `%x`/`%X`/`%B`/`%o`: `(v >>> 0).toString(base)`. `>>> 0` is JS's 32-bit unsigned
            // coercion, so a negative `int` prints its two's-complement digits as on the CLR.
            | FieldFormat.IntRadix(radix, zeroPad) ->
                let baseN, upper =
                    match radix with
                    | Radix.Hex u -> 16, u
                    | Radix.Binary -> 2, false
                    | Radix.Octal -> 8, false

                wrapped (
                    direct (fun v ->
                        let digits =
                            invoke (JsExpr.Binary(">>>", v, num 0, ValueNone)) "toString" [ num baseN ]

                        let cased = if upper then invoke digits "toUpperCase" [] else digits

                        match zeroPad with
                        | Some w -> invoke cased "padStart" [ num w; str "0" ]
                        | None -> cased
                    )
                )
            // `%u`: the source `int`'s bits reinterpreted unsigned. `-1` prints `4294967295`.
            | FieldFormat.Unsigned zeroPad ->
                wrapped (
                    direct (fun v ->
                        let digits = invoke (JsExpr.Binary(">>>", v, num 0, ValueNone)) "toString" []

                        match zeroPad with
                        | Some w -> invoke digits "padStart" [ num w; str "0" ]
                        | None -> digits
                    )
                )
            // `%b`: lowercase `true`/`false`.
            | FieldFormat.Bool -> wrapped (direct (fun v -> JsExpr.Conditional(v, str "true", str "false", ValueNone)))
            // `%f`/`%.Nf`/`%.*f`: fixed-point, `precision` fraction digits.
            | FieldFormat.Fixed precision ->
                wrapped (direct (fun v -> invoke (objArg v) "toFixed" [ precJs precision ]))
            // `%0w.Nf`: fixed-point, then zeros after any sign to a total field of `width`.
            | FieldFormat.FixedZeroPad(precision, width) ->
                wrapped (
                    strBind (direct (fun v -> invoke (objArg v) "toFixed" [ num precision ])) (zeroPadAfterSign width)
                )
            // `%-0w.Nf`: fixed-point, then zeros on the RIGHT to a total field of `width`
            // (`%-05.2f` 3.14159 ⇒ `"3.140"`). `padEnd` never truncates, so an already-wider
            // body prints unpadded, matching F#.
            | FieldFormat.FixedRightZeroPad(precision, width) ->
                wrapped (
                    direct (fun v ->
                        invoke (invoke (objArg v) "toFixed" [ num precision ]) "padEnd" [ num width; str "0" ]
                    )
                )
            // `%+d`/`% d`/`%+05d`/`%+.Nf`/`%+e`/`%+g`: forced sign. A non-negative value takes
            // `+` (or a space with `% `), a negative keeps its `-`. `typeChar` picks the base
            // rendering; a `zeroPad` then zero-fills after the sign (`%+05d` 42 ⇒ `"+0042"`).
            | FieldFormat.ForcedSign(space, precision, typeChar, zeroPad) ->
                let sign = if space then " " else "+"

                let numStr =
                    direct (fun v ->
                        match typeChar with
                        | 'e'
                        | 'E' ->
                            let e = invoke (objArg v) "toExponential" [ precJs precision ]
                            if typeChar = 'E' then invoke e "toUpperCase" [] else e
                        | 'g'
                        | 'G' ->
                            let g = invoke (objArg v) "toPrecision" [ precForToPrecision precision ]
                            if typeChar = 'G' then invoke g "toUpperCase" [] else g
                        | _ -> invoke (objArg v) "toFixed" [ precJs precision ]
                    )

                let signed =
                    strBind
                        numStr
                        (fun s ->
                            JsExpr.Conditional(
                                invoke s "startsWith" [ str "-" ],
                                s,
                                JsExpr.Binary("+", str sign, s, ValueNone),
                                ValueNone
                            )
                        )

                let padded =
                    match zeroPad with
                    | Option.None -> signed
                    | Option.Some w ->
                        // The signed string always opens with a sign char, so slot 0 is kept
                        // and the zeros go after it, to a total field of `w`.
                        strBind
                            signed
                            (fun s ->
                                JsExpr.Binary(
                                    "+",
                                    invoke s "slice" [ num 0; num 1 ],
                                    invoke (invoke s "slice" [ num 1 ]) "padStart" [ num (w - 1); str "0" ],
                                    ValueNone
                                )
                            )

                wrapped padded
            // `%e`/`%E`: `toExponential`. JS writes a minimal exponent, .NET zero-pads it to
            // three digits (`1.234500e+4` vs `1.234500e+004`), an accepted approximation.
            | FieldFormat.Exponential(precision, upper) ->
                wrapped (
                    direct (fun v ->
                        let e = invoke (objArg v) "toExponential" [ precJs precision ]
                        if upper then invoke e "toUpperCase" [] else e
                    )
                )
            // `%g`/`%G`: `toPrecision`. JS keeps trailing zeros and switches to exponential at
            // different thresholds than .NET `G`, so again an approximation, not byte-exact.
            | FieldFormat.Compact(precision, upper) ->
                wrapped (
                    direct (fun v ->
                        let g = invoke (objArg v) "toPrecision" [ precForToPrecision precision ]
                        if upper then invoke g "toUpperCase" [] else g
                    )
                )
            // `%014e`/`%010g`: scientific / compact, then zeros after any sign to a total field
            // of `width`, reusing `%0w.Nf`'s padding over an `e`/`g` body, same approximation.
            | FieldFormat.ExpCompactZeroPad(precision, width, typeChar) ->
                wrapped (
                    strBind
                        (direct (fun v ->
                            match typeChar with
                            | 'e'
                            | 'E' ->
                                let e = invoke (objArg v) "toExponential" [ num precision ]
                                if typeChar = 'E' then invoke e "toUpperCase" [] else e
                            | _ ->
                                let g = invoke (objArg v) "toPrecision" [ num (max 1 precision) ]
                                if typeChar = 'G' then invoke g "toUpperCase" [] else g
                        ))
                        (zeroPadAfterSign width)
                )

        let structuralFmtRef () =
            JsExpr.Identifier(JsImports.addRef ctx.Imports structuralFormatRef, ValueNone)

        // Bind the runtime star width to `w`. A JS call argument evaluates before the arrow
        // body, so `printf "%*d" (f ()) (g ())` still calls `f` before `g`, as F# does.
        let bindStarWidth (widthExpr: TastAccessor.ExprId) (body: JsFnBody) : JsExpr =
            call (JsExpr.Arrow([ "w" ], body, ValueNone)) [ buildExpr ctx widthExpr ]

        // Bind the runtime star precision to `p`. `normalize` clamps it to `0..99`, which F#
        // does only when width and precision are BOTH `*` on a float form (`%*.*f`).
        let bindStarPrec (precExpr: TastAccessor.ExprId) (normalize: bool) (bodyExpr: JsExpr) : JsExpr =
            let arg =
                if normalize then
                    invoke (id "Math") "max" [ num 0; invoke (id "Math") "min" [ num 99; buildExpr ctx precExpr ] ]
                else
                    buildExpr ctx precExpr

            call (JsExpr.Arrow([ "p" ], JsFnBody.Expr bodyExpr, ValueNone)) [ arg ]

        let classified =
            match hole.Source with
            | HoleSpecSource.Classified f -> ValueSome f
            | HoleSpecSource.RawFormat _ -> ValueNone

        // Nest the precision binding inside the width binding, so the emitted
        // `((w) => ((p) => body)(prec))(width)` evaluates width, then precision, then the
        // value. A padding form throws `RangeError` on a negative `%*` width; `%*A` clamps.
        let wrapDims (bodyExpr: JsExpr) : JsExpr =
            let guardWidth =
                match classified with
                | ValueSome f -> starWidthClamp f = ValueSome StarWidthClamp.Guard
                | ValueNone -> false

            let normalizePrec =
                match classified with
                | ValueSome f -> normalizesStarPrecision f
                | ValueNone -> false

            let withPrec =
                match starPrecision with
                | ValueSome precExpr -> bindStarPrec precExpr normalizePrec bodyExpr
                | ValueNone -> bodyExpr

            match starWidth with
            | ValueNone -> withPrec
            | ValueSome widthExpr ->
                if guardWidth then
                    let body =
                        JsFnBody.Block
                            [
                                JsStatement.If(
                                    JsExpr.Binary("<", id "w", num 0, ValueNone),
                                    [
                                        JsStatement.Throw(JsExpr.New(id "RangeError", [ str "totalWidth" ], ValueNone))
                                    ],
                                    []
                                )
                                JsStatement.Return withPrec
                            ]

                    bindStarWidth widthExpr body
                else
                    bindStarWidth widthExpr (JsFnBody.Expr withPrec)

        match hole.Source with
        // An interpolation's `{x:X}` clause is a .NET custom-format string, not a printf
        // placeholder; JS cannot interpret it, so the raw operand stands.
        | HoleSpecSource.RawFormat _ -> buildExpr ctx operand
        // `%A`: `structuralFormat(value, width, size)`. A star width (`%*A`) binds `w` and
        // clamps to 0 inline, because F# renders a negative budget flat rather than throwing.
        | HoleSpecSource.Classified(HoleForm.PercentA(width, size)) ->
            let widthArg =
                match percentAWidth width with
                | ValueSome n -> num n
                | ValueNone ->
                    JsExpr.Conditional(JsExpr.Binary("<", id "w", num 0, ValueNone), num 0, id "w", ValueNone)

            let sizeArg =
                match percentASize size with
                | ValueSome n -> num n
                | ValueNone -> id "p"

            let bodyExpr =
                call (structuralFmtRef ()) [ buildExpr ctx operand; widthArg; sizeArg ]

            wrapDims bodyExpr
        | HoleSpecSource.Classified(HoleForm.Field(fmt, alignment)) ->
            let wrap =
                match alignment with
                | PrintfHoleForm.Alignment.None -> Option.None
                | PrintfHoleForm.Alignment.Const a -> Option.Some(withAlign (Some a))
                | PrintfHoleForm.Alignment.Star leftJustify -> Option.Some(padDyn leftJustify)

            wrapDims (emitField fmt wrap)
        // `%a`/`%t` arrive as `FormatSegG.CallbackHole` and are spliced whole; a callback's
        // `HoleForm` is provenance only and never reaches this per-hole projection.
        | HoleSpecSource.Classified(HoleForm.Callback _) ->
            failwith "EmitJs: callback hole reached buildHole (unreachable)"

    /// Build the single argument a `console.log`/`error` call prints from format segments.
    let buildFormatArg
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (segments: TastAccessor.FormatSeg[])
        : JsExpr =
        let buildHole = buildHole buildExpr ctx

        match segments with
        | [| FormatSegG.Lit s |] -> JsExpr.Literal(JsLiteral.String s, ValueNone)
        | [| FormatSegG.Hole(hole, operand) |] -> buildHole hole operand ValueNone ValueNone
        | [| FormatSegG.DynHole d |] -> buildHole d.Spec d.Value d.Width d.Precision
        // `%a`/`%t`: the callback was already lowered to a residue-`string` expression, so
        // the splice is just that expression.
        | [| FormatSegG.CallbackHole(_, residue) |] -> buildExpr ctx residue
        | _ ->
            let pieces = ResizeArray<JsRawSeg>()
            // Seed with `""` so the first `+` already concatenates strings, even when the
            // format opens with two adjacent holes (`%d%d` must not add two numbers).
            pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String "", ValueNone)))

            for seg in segments do
                pieces.Add(JsRawSeg.Verbatim " + ")

                match seg with
                | FormatSegG.Lit s -> pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String s, ValueNone)))
                | FormatSegG.Hole(hole, operand) ->
                    pieces.Add(JsRawSeg.Hole(buildHole hole operand ValueNone ValueNone))
                | FormatSegG.DynHole d -> pieces.Add(JsRawSeg.Hole(buildHole d.Spec d.Value d.Width d.Precision))
                | FormatSegG.CallbackHole(_, residue) -> pieces.Add(JsRawSeg.Hole(buildExpr ctx residue))

            JsExpr.Raw(List.ofSeq pieces, ValueNone)

    /// Expand a `$N` JS-expression template into `JsRawSeg`s: verbatim chunks interleaved
    /// with operand expressions. `$$` is a literal `$`.
    let expandTemplate
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (template: string)
        (args: TastAccessor.ExprId[])
        : JsRawSeg list =
        let segs = ResizeArray<JsRawSeg>()
        let buf = System.Text.StringBuilder()
        let mutable sawHole = false

        let flush () =
            if buf.Length > 0 then
                segs.Add(JsRawSeg.Verbatim(buf.ToString()))
                buf.Clear() |> ignore

        let mutable i = 0

        while i < template.Length do
            let c = template.[i]

            if c = '$' && i + 1 < template.Length && template.[i + 1] = '$' then
                buf.Append '$' |> ignore
                i <- i + 2
            elif c = '$' && i + 1 < template.Length && System.Char.IsDigit template.[i + 1] then
                flush ()
                let mutable j = i + 1

                while j < template.Length && System.Char.IsDigit template.[j] do
                    j <- j + 1

                let idx =
                    System.Int32.Parse(
                        template.Substring(i + 1, j - i - 1),
                        System.Globalization.CultureInfo.InvariantCulture
                    )

                if idx < 0 || idx >= args.Length then
                    failwithf
                        "EmitJs: template '%s' references operand $%d but only %d supplied"
                        template
                        idx
                        args.Length

                segs.Add(JsRawSeg.Hole(buildExpr ctx args.[idx]))
                sawHole <- true
                i <- j
            else
                buf.Append c |> ignore
                i <- i + 1

        flush ()

        // Operands present but no hole → bare CIL mnemonic escaped the CLR-only finish pass.
        if args.Length > 0 && not sawHole then
            failwithf "EmitJs: non-template ILIntrinsic opcode '%s' reached the JS backend" template

        List.ofSeq segs
