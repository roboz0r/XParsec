namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm
open JsEmitHelpers
open EmitJsContext

/// The printf/format lowering: `$N`-template expansion, the `console.log` argument
/// builder, and the per-hole `buildHole` projection that reproduces every
/// `%`-specifier as inline `JsExpr` (no runtime format-string re-parse). Each recurses
/// into expression emission only to build a hole's OPERAND, so it takes `buildExpr` as
/// a callback rather than joining the walker's recursion group.
module EmitJsFormat =

    /// Build the JS expression a format hole's argument contributes.
    ///
    /// Reads the hole's classified semantic model (`HoleForm` / `FieldFormat`, on
    /// `hole.Source`) and builds real `JsExpr` nodes from the typed fields — no `.NET`
    /// format string is reconstructed or re-parsed (that dialect is CLR-only). A
    /// `RawFormat` interpolation clause (`{x:X}`) is such a CLR dialect string, so
    /// JS doesn't interpret it — the raw operand stands (the concat coerces it).
    ///
    /// `%A` (`PercentA`) renders the value as copy-pasteable source through the
    /// `structuralFormat` runtime, a flat call `(value, width, size)`; the width /
    /// node-size budgets resolve through `percentAWidth` / `percentASize` (the
    /// `80` / `10000` defaults, shared with the CLR `AppendStructured`).
    ///
    /// Every `Field` hole is the specifier's per-hole formatting reproduced as an
    /// inline JS expression, byte-matching the CLR `Formatter` members — the sole
    /// exception being a `%O` on a `float32`, whose shortest-round-trip search is a
    /// loop and so rides the `Vesper.Printf` runtime (`float32ToString`). The operand
    /// is evaluated exactly once: single-reference forms splice it directly; the one
    /// form that reads it repeatedly (`DecimalZeroPad`) binds it in an arrow IIFE, so
    /// `%05d (f ())` still calls `f` once.
    ///
    /// Covered: `%d`/`%i`/`%s`/`%O`/`%c`/`%M` (`Verbatim`), width + alignment
    /// (`padStart` / `padEnd`), `%x`/`%X`/`%B`/`%o` (`IntRadix`), `%u` (`Unsigned`
    /// reinterpret), `%b` (`Bool`), `%f` (`Fixed`), `%0wd` (`DecimalZeroPad`),
    /// `%0w.pf` (`FixedZeroPad`), `%-0w.pf` (`FixedRightZeroPad`), and `%+`/`% `
    /// (`ForcedSign`, incl. `%+05d` zero-pad-through-sign). The scientific / compact
    /// forms — `%e`/`%E`/`%g`/`%G` (`Exponential`/`Compact`), their zero-pad
    /// (`ExpCompactZeroPad`) and forced-sign (`ForcedSign` with an `e`/`g` letter)
    /// variants — emit `toExponential`/`toPrecision`, an accepted JS *approximation*
    /// (minimal exponent width, `toPrecision` trailing zeros), not F# byte-parity.
    let buildHole
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (hole: Frozen.HoleSpec)
        (operand: Frozen.TExpr)
        (starWidth: Frozen.TExpr voption)
        (starPrecision: Frozen.TExpr voption)
        : JsExpr =
        let num (n: int) =
            JsExpr.Literal(JsLiteral.Number(string n), ValueNone)

        let str (s: string) =
            JsExpr.Literal(JsLiteral.String s, ValueNone)

        let id (s: string) = JsExpr.Identifier(s, ValueNone)
        let call callee args = JsExpr.Call(callee, args, ValueNone)
        // A method call `recv.m(args…)`.
        let invoke (recv: JsExpr) (m: string) (args: JsExpr list) =
            JsExpr.Call(JsExpr.Member(recv, id m, false, ValueNone), args, ValueNone)
        // Parenthesise a bare numeric-literal `.method` receiver — `5.toFixed(0)` is a
        // JS syntax error and `-3.14.toFixed(2)` mis-binds as `-(3.14.toFixed(2))`.
        // Self-parenthesising receivers (`Binary` shifts, `Call`s) need no wrap.
        let receiver (e: JsExpr) : JsExpr =
            match e with
            | JsExpr.Literal _ -> JsExpr.Sequence([ e ], ValueNone)
            | _ -> e

        // The hole's field alignment, applied to an already-built string expression:
        // `Some w` ⇒ right-justify (`padStart w`), `Some -w` ⇒ left (`padEnd w`). The
        // zero-pad forms carry their width inside the `FieldFormat` and set alignment
        // `None`, so this is a no-op there.
        let withAlign (alignment: int option) (e: JsExpr) : JsExpr =
            match alignment with
            | Some a when a >= 0 -> invoke e "padStart" [ num a ]
            | Some a -> invoke e "padEnd" [ num (-a) ]
            | None -> e

        // Splice the operand once into `build value`. Use for forms that read the
        // value a single time (a duplicated side-effecting operand would re-run).
        let direct (build: JsExpr -> JsExpr) : JsExpr = build (buildExpr ctx operand)

        // Bind the operand to `v` in an arrow IIFE — for forms that read it more than
        // once, so it evaluates exactly once: `((v) => build(v))(operand)`.
        let iife (build: JsExpr -> JsExpr) : JsExpr =
            call (JsExpr.Arrow([ "v" ], JsFnBody.Expr(build (id "v")), ValueNone)) [ buildExpr ctx operand ]

        // `((s) => build(s))(inner)` — name an intermediate *string* result `s` so the
        // sign-aware float forms can inspect it (`s.startsWith("-")`) without rebuilding
        // it; `inner` already references the operand once.
        let strBind (inner: JsExpr) (build: JsExpr -> JsExpr) : JsExpr =
            call (JsExpr.Arrow([ "s" ], JsFnBody.Expr(build (id "s")), ValueNone)) [ inner ]

        // Apply a hole's field width dynamically for `%*d`: pad an already-string
        // expression to the runtime width bound to `w` (`padEnd` for a `-`-flag
        // left-justify, else `padStart`). The negative-width guard is emitted by the
        // enclosing IIFE (`buildStar`), before the value.
        let padDyn (leftJustify: bool) (e: JsExpr) : JsExpr =
            if leftJustify then
                invoke e "padEnd" [ id "w" ]
            else
                invoke e "padStart" [ id "w" ]

        // A float form's precision as a JS expression: a static literal, or the runtime
        // `p` bound by `bindStarPrec` (`%.*f`/`%.*e`/`%.*g`).
        let precJs (p: Prec) : JsExpr =
            match p with
            | Prec.Const n -> num n
            | Prec.Star -> id "p"

        // `toPrecision` requires ≥ 1 significant digit (`%.0g` / a clamped star would
        // throw a RangeError), so clamp: static → `max 1 n`, runtime → `Math.max(1, p)`.
        let precForToPrecision (p: Prec) : JsExpr =
            match p with
            | Prec.Const n -> num (max 1 n)
            | Prec.Star -> invoke (id "Math") "max" [ num 1; id "p" ]

        // The `float32ToString` runtime export a `%O` on a float32 renders through — a
        // NAMED import from the same `Vesper.Printf.mjs` `%A` rides (`structuralFmtRef`).
        let float32FmtRef () =
            JsExpr.Identifier(
                JsImports.addRef ctx.Imports "float32ToString" float32ToStringKey ImportForm.Named,
                ValueNone
            )

        // `emitField` builds the value string then applies the field-width `wrap` (a
        // static `padStart`/`padEnd` or a dynamic `%*d` pad). `wrap = None` means no
        // field width: only `Verbatim` cares — a bare `%d` keeps the raw operand so the
        // surrounding concat coerces it; every other form ignores `None` (identity),
        // as `toFixed`/`toString`/… already yield a string.
        let emitField (fmt: FieldFormat) (wrap: (JsExpr -> JsExpr) option) : JsExpr =
            let wrapped = defaultArg wrap (fun e -> e)

            match fmt with
            // `%d`/`%s`/`%O`/`%c`/`%M`: plain stringification — by the operand's STATIC F#
            // type (`plainRenderOf`), never by its JS runtime type, which is not the same
            // width (an `int64` is a `bigint`, a `float32` a double-precision `number`).
            // `Native` ⇒ JS's own coercion is already .NET's: bare gives the raw operand
            // (the surrounding concat coerces it, a lone `%d` stays `console.log(x)`), a
            // width gives `String(v)` then pad. The two mis-rendering widths must always
            // render explicitly, bare or not, so `console.log` cannot inspect the value.
            | FieldFormat.Verbatim ->
                match plainRenderOf ctx hole.Ty with
                | PlainRender.Native ->
                    match wrap with
                    | Option.Some w -> w (direct (fun v -> call (id "String") [ v ]))
                    | Option.None -> buildExpr ctx operand
                | PlainRender.BigInt -> wrapped (direct (fun v -> call (id "String") [ v ]))
                | PlainRender.Single -> wrapped (direct (fun v -> call (float32FmtRef ()) [ v ]))
            // `%0wd`: sign-aware zero-pad — zeros pad to `width` *after* the sign
            // (`(-42).ToString("D5") = "-00042"`), so the value is read three times.
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
            // `%x`/`%X`/`%B`/`%o`: `(v >>> 0).toString(base)` — `>>> 0` is JS's 32-bit
            // unsigned coercion (the CLI-stack reinterpret on CLR), then optional
            // upper-casing and zero-pad (`%08o` zero-pads like the others).
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
            // `%u`/`%05u`: the source `int`'s bits reinterpreted unsigned (`>>> 0`),
            // then optional zero-pad (`padStart` never truncates on overflow).
            | FieldFormat.Unsigned zeroPad ->
                wrapped (
                    direct (fun v ->
                        let digits = invoke (JsExpr.Binary(">>>", v, num 0, ValueNone)) "toString" []

                        match zeroPad with
                        | Some w -> invoke digits "padStart" [ num w; str "0" ]
                        | None -> digits
                    )
                )
            // `%b`: lowercase `true`/`false` (explicit ternary keeps the alignment path uniform).
            | FieldFormat.Bool -> wrapped (direct (fun v -> JsExpr.Conditional(v, str "true", str "false", ValueNone)))
            // `%f` / `%.Nf` / `%.*f`: fixed-point with `precision` fraction digits
            // (runtime `p` for a star).
            | FieldFormat.Fixed precision ->
                wrapped (direct (fun v -> invoke (receiver v) "toFixed" [ precJs precision ]))
            // `%0w.Nf`: fixed-point, then zeros after any sign to a total field of `width`.
            | FieldFormat.FixedZeroPad(precision, width) ->
                wrapped (
                    strBind
                        (direct (fun v -> invoke (receiver v) "toFixed" [ num precision ]))
                        (fun s ->
                            let padTail =
                                JsExpr.Binary(
                                    "+",
                                    str "-",
                                    invoke (invoke s "slice" [ num 1 ]) "padStart" [ num (width - 1); str "0" ],
                                    ValueNone
                                )

                            JsExpr.Conditional(
                                invoke s "startsWith" [ str "-" ],
                                padTail,
                                invoke s "padStart" [ num width; str "0" ],
                                ValueNone
                            )
                        )
                )
            // `%-0w.Nf`: fixed-point, then zeros on the RIGHT (past the digits) to a
            // total field of `width` — F#'s left-align + zero-pad on a float. `padEnd`
            // never truncates, so an already-wider body prints unpadded (matching F#).
            | FieldFormat.FixedRightZeroPad(precision, width) ->
                wrapped (
                    direct (fun v ->
                        invoke (invoke (receiver v) "toFixed" [ num precision ]) "padEnd" [ num width; str "0" ]
                    )
                )
            // `%+d`/`% d`/`%+05d`/`%+.Nf`/`% .Nf`/`%+e`/`%+g`: forced sign — a
            // non-negative value takes the sign char (`+` or a space), a negative keeps
            // its `-`. `typeChar` selects the base rendering (`'f'` ⇒ `toFixed`, `'e'`/`'E'`
            // ⇒ `toExponential`, `'g'`/`'G'` ⇒ `toPrecision`; the scientific / compact
            // forms inherit the JS approximation caveat, not F# parity). `zeroPad = Some w`
            // (`%+05d`) then zero-fills *after* the sign to a total field of `w`.
            | FieldFormat.ForcedSign(space, precision, typeChar, zeroPad) ->
                let sign = if space then " " else "+"

                let numStr =
                    direct (fun v ->
                        match typeChar with
                        | 'e'
                        | 'E' ->
                            let e = invoke (receiver v) "toExponential" [ precJs precision ]
                            if typeChar = 'E' then invoke e "toUpperCase" [] else e
                        | 'g'
                        | 'G' ->
                            let g = invoke (receiver v) "toPrecision" [ precForToPrecision precision ]
                            if typeChar = 'G' then invoke g "toUpperCase" [] else g
                        | _ -> invoke (receiver v) "toFixed" [ precJs precision ]
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
                        // The signed string always opens with a sign char (`+`/` `/`-`);
                        // zero-fill after it to a total field of `w`.
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
            // `%e`/`%E`: scientific notation via `v.toExponential(precision)`. JS uses
            // a lowercase `e` and a minimal (1-2 digit) exponent, so this is NOT byte-
            // identical to F#/.NET (which zero-pads the exponent to 3 digits,
            // `1.234500e+004`); it's an accepted close approximation. `%E` upper-cases
            // the `e` (only letter in the string, so `toUpperCase` is safe).
            | FieldFormat.Exponential(precision, upper) ->
                wrapped (
                    direct (fun v ->
                        let e = invoke (receiver v) "toExponential" [ precJs precision ]
                        if upper then invoke e "toUpperCase" [] else e
                    )
                )
            // `%g`/`%G`: compact form via `v.toPrecision(significant)`. JS `toPrecision`
            // keeps trailing zeros and switches to exponential on different thresholds
            // than .NET `G`, so again an accepted approximation, not byte-exact.
            // `toPrecision` requires ≥ 1 significant digit, so clamp (a `%.0g` would
            // otherwise throw a RangeError at runtime).
            | FieldFormat.Compact(precision, upper) ->
                wrapped (
                    direct (fun v ->
                        let g = invoke (receiver v) "toPrecision" [ precForToPrecision precision ]
                        if upper then invoke g "toUpperCase" [] else g
                    )
                )
            // `%014e`/`%010g`: scientific / compact, then zeros after any sign to a total
            // field of `width` (mirror `FixedZeroPad`). Inherits the
            // `toExponential`/`toPrecision` approximation — an accepted JS divergence.
            | FieldFormat.ExpCompactZeroPad(precision, width, typeChar) ->
                wrapped (
                    strBind
                        (direct (fun v ->
                            match typeChar with
                            | 'e'
                            | 'E' ->
                                let e = invoke (receiver v) "toExponential" [ num precision ]
                                if typeChar = 'E' then invoke e "toUpperCase" [] else e
                            | _ ->
                                let g = invoke (receiver v) "toPrecision" [ num (max 1 precision) ]
                                if typeChar = 'G' then invoke g "toUpperCase" [] else g
                        ))
                        (fun s ->
                            let padTail =
                                JsExpr.Binary(
                                    "+",
                                    str "-",
                                    invoke (invoke s "slice" [ num 1 ]) "padStart" [ num (width - 1); str "0" ],
                                    ValueNone
                                )

                            JsExpr.Conditional(
                                invoke s "startsWith" [ str "-" ],
                                padTail,
                                invoke s "padStart" [ num width; str "0" ],
                                ValueNone
                            )
                        )
                )

        // The `structuralFormat` runtime export a `%A` hole renders through — always
        // a NAMED import (`Vesper.Printf.mjs`).
        let structuralFmtRef () =
            JsExpr.Identifier(
                JsImports.addRef ctx.Imports "structuralFormat" structuralFormatKey ImportForm.Named,
                ValueNone
            )

        // Bind the runtime star width to `w`, evaluated *before* the value: the arrow
        // argument (`widthExpr`) evaluates first in JS call order, mirroring F#'s
        // curried application order, then `body` (which references `w` and reads the
        // value) runs. `body` may be a guarded block (padding forms) or an expression
        // (`%*A` clamp). The value operand is read once inside `body`.
        let bindStarWidth (widthExpr: Frozen.TExpr) (body: JsFnBody) : JsExpr =
            call (JsExpr.Arrow([ "w" ], body, ValueNone)) [ buildExpr ctx widthExpr ]

        // Bind the runtime star precision to `p`, evaluated *after* any width but before
        // the value. `normalize` clamps to `0..99` (F#'s `normalizePrecision`) — applied
        // ONLY on the width=*+prec=* float path (`printf.fs:632`); the prec=*-only paths
        // and `%A` keep the raw precision (`:649-657`, `:1114`). The error TYPE of a
        // negative raw precision diverges from the CLR (JS `toFixed` throws `RangeError`),
        // an accepted divergence like `%e`/`%g`.
        let bindStarPrec (precExpr: Frozen.TExpr) (normalize: bool) (bodyExpr: JsExpr) : JsExpr =
            let arg =
                if normalize then
                    invoke (id "Math") "max" [ num 0; invoke (id "Math") "min" [ num 99; buildExpr ctx precExpr ] ]
                else
                    buildExpr ctx precExpr

            call (JsExpr.Arrow([ "p" ], JsFnBody.Expr bodyExpr, ValueNone)) [ arg ]

        // The hole's classified form (if any) — drives the shared `PrintfHoleForm`
        // runtime-dim policy in `wrapDims`. `RawFormat` (no form) returns before it.
        let classified =
            match hole.Source with
            | HoleSpecSource.Classified f -> ValueSome f
            | HoleSpecSource.RawFormat _ -> ValueNone

        // Wrap a built body with the precision binding (inner) then the width binding
        // (outer) so the emitted JS evaluates width, then precision, then the value —
        // curried application order. The shared `starWidthClamp` decides guard-vs-clamp
        // (a `Guard` form throws on a negative width; `%*A` clamps inline instead), and
        // `normalizesStarPrecision` whether a star precision clamps to `0..99` — the same
        // classification the CLR backend consumes, so the two can't drift.
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
        // A `{x:fmt}` interpolation custom-format clause: a CLR dialect string with no
        // printf placeholder, which JS does not interpret — the raw operand stands.
        | HoleSpecSource.RawFormat _ -> buildExpr ctx operand
        // `%A`: `structuralFormat(value, width, size)`. A star width (`%*A`/`%*.*A`)
        // binds `w`, clamped to 0 inline (F# renders a negative budget flat); a star
        // size (`%.*A`/`%*.*A`) binds `p`, raw. Statics resolve through the shared
        // `percentAWidth`/`percentASize` defaults.
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
        // Every `Field` form: a static width lands in `alignment` (`Const`/`None`); a
        // star width is `Alignment.Star` and pads by `w` under a negative-width guard.
        | HoleSpecSource.Classified(HoleForm.Field(fmt, alignment)) ->
            let wrap =
                match alignment with
                | PrintfHoleForm.Alignment.None -> Option.None
                | PrintfHoleForm.Alignment.Const a -> Option.Some(withAlign (Some a))
                | PrintfHoleForm.Alignment.Star leftJustify -> Option.Some(padDyn leftJustify)

            wrapDims (emitField fmt wrap)
        // `%a`/`%t` callback holes ride a `FormatSegG.CallbackHole` whose residue string
        // is spliced directly; a callback spec's `HoleForm` is provenance only and never
        // reaches this per-hole projection.
        | HoleSpecSource.Classified(HoleForm.Callback _) ->
            failwith "EmitJs: callback hole reached buildHole (unreachable)"

    /// Build the single argument a `console.log`/`error` call prints from format segments.
    /// Mixed formats are concatenations seeded with `""` so every `+` is string-valued.
    let buildFormatArg
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (segments: EqArray<Frozen.FormatSeg>)
        : JsExpr =
        let buildHole = buildHole buildExpr ctx

        match EqArray.toList segments with
        | [ FormatSegG.Lit s ] -> JsExpr.Literal(JsLiteral.String s, ValueNone)
        | [ FormatSegG.Hole(hole, operand) ] -> buildHole hole operand ValueNone ValueNone
        | [ FormatSegG.DynHole d ] -> buildHole d.Spec d.Value d.Width d.Precision
        // `%a`/`%t`: Freeze lowered the callback to an ordinary residue-string expr; the
        // splice is just that expr (on JS only `sprintf`'s `cb(undefined)[(v)]` reaches
        // here — writer/builder `%a` diagnoses at the capability gate before Freeze).
        | [ FormatSegG.CallbackHole(_, residue) ] -> buildExpr ctx residue
        | segs ->
            let pieces = ResizeArray<JsRawSeg>()
            // Seed with `""` so the first `+` already concatenates strings, even
            // when the format opens with two adjacent holes (`%d%d`).
            pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String "", ValueNone)))

            for seg in segs do
                pieces.Add(JsRawSeg.Verbatim " + ")

                match seg with
                | FormatSegG.Lit s -> pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String s, ValueNone)))
                | FormatSegG.Hole(hole, operand) ->
                    pieces.Add(JsRawSeg.Hole(buildHole hole operand ValueNone ValueNone))
                | FormatSegG.DynHole d -> pieces.Add(JsRawSeg.Hole(buildHole d.Spec d.Value d.Width d.Precision))
                | FormatSegG.CallbackHole(_, residue) -> pieces.Add(JsRawSeg.Hole(buildExpr ctx residue))

            JsExpr.Raw(List.ofSeq pieces, ValueNone)

    /// Expand a `$N` JS-expression template into `JsRawSeg`s: verbatim chunks
    /// interleaved with operand expressions. `$$` is a literal `$`. A bare CIL
    /// mnemonic with operands but no `$N` hole is a hard error.
    let expandTemplate
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (template: string)
        (args: Frozen.TExpr list)
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

                if idx < 0 || idx >= List.length args then
                    failwithf
                        "EmitJs: template '%s' references operand $%d but only %d supplied"
                        template
                        idx
                        (List.length args)

                segs.Add(JsRawSeg.Hole(buildExpr ctx (List.item idx args)))
                sawHole <- true
                i <- j
            else
                buf.Append c |> ignore
                i <- i + 1

        flush ()

        // Operands present but no hole → bare CIL mnemonic escaped the CLR-only finish pass.
        if not (List.isEmpty args) && not sawHole then
            failwithf "EmitJs: non-template ILIntrinsic opcode '%s' reached the JS backend" template

        List.ofSeq segs
