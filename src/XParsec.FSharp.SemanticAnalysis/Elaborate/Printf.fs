namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

// Printf lowering for the Elaborate pass: a marked happy-path call becomes a
// `TExpr.Format`, a marked fully-unapplied lowerable partial a synthesised
// closure over one. Both read the markers `Unification.tryInferPrintfApp`
// recorded; nothing here re-derives specifier classification per backend.

module internal ElaboratePrintf =

    /// Whether `%A` of an argument of this (zonked) type may lower to the structural
    /// engine. The runtime `%A` dispatcher is *total* and reflection-free: a
    /// Vesper-compiled record / DU renders via its synthesised `IStructuralFormattable`,
    /// a list / array / tuple via the `IEnumerable` / `ITuple` arm, a BCL scalar via
    /// `IFormattable`, and *anything else* — an `FSharpOption`, an arbitrary BCL class,
    /// a runtime-boxed polymorphic value — falls to the `value.ToString()` tail. So the
    /// engine can lower **every concrete nominal**; the maintainer decision is that a
    /// non-Vesper structural type degrades down the `%A` hierarchy to `.ToString()` on
    /// the engine rather than riding the FSharp.Core cold path, even where its bytes
    /// diverge from F#'s reflective `%A`. The only holes that stay off the engine are
    /// the ones the backend can't author an `AppendStructured<T>` type argument for: an
    /// unresolved nominal (`TyUnknown`), an anonymous union / type-level computation
    /// (external vocabulary that a real `%A` hole never carries). A hole the engine
    /// can't take forces the *whole* format cold (`translatePrintfFormat` returns
    /// `ValueNone`) — additive, no regression.
    let rec private structuredArgFaithful (t: SemType) : bool =
        match t with
        | TyConst(key, args) ->
            let name = SymbolKeyOps.intrinsicName key
            // The array intrinsic (`'T[]` ≡ `TyConst("[]", [elem])`) renders via
            // the `IEnumerable` arm — faithful iff its element type is.
            if name = "[]" then
                EqArray.forall structuredArgFaithful args
            // Numeric primitives carry the F# literal suffixes the engine reproduces
            // (`5L`, `1.5M`); `string` / `char` / `bool` are special-cased atoms. All
            // are leaf scalars, so any type argument means it isn't really one.
            elif
                RuntimeNames.numericTypeNames.Contains name
                || name = "string"
                || name = "char"
                || name = "bool"
            then
                args.Length = 0
            else
                false
        | TyTuple items -> EqArray.forall structuredArgFaithful items
        // The cons-list still renders via the `IEnumerable` arm (it carries no
        // synthesised `Format`), so it stays faithful-iff-its-element-is. It
        // surfaces as a `TyUnion` in the self-host (the Vesper cons-list DU) but as a
        // `TyRecord` against the FSharp.Core contract (`list`1`), so accept both
        // shapes of the list keys.
        | TyUnion(key, args)
        | TyRecord(key, args) when RuntimeNames.isVesperListKey key || RuntimeNames.isFsharpCoreListKey key ->
            EqArray.forall structuredArgFaithful args
        // Every nominal record / DU / class renders on the engine — a Vesper-compiled
        // type via its synthesised `IStructuralFormattable.Format` (step-3
        // `NominalEmit`), an `FSharpOption` / arbitrary BCL type via the dispatcher's
        // `IFormattable` / `IEnumerable` / `ToString` tail. We do NOT recurse into
        // fields: the gate is a cold-vs-engine switch, not a per-field renderer, and
        // the runtime dispatcher already routes each field (a Vesper field via its own
        // `IStructuralFormattable`, a BCL field via `ToString` / `IEnumerable`). The
        // backend authors `AppendStructured<T>` for any of these — a project-local
        // nominal off its emitted `TypeDef`, an external one off its `TypeRef` — so the
        // only reason to decline is a hole type the encoder can't author, handled by
        // the final arm. Notably the home ASSEMBLY plays no part: a nominal is faithful
        // wherever it lives.
        | TyUnion _
        | TyRecord _
        | TyClass _ -> true
        // A polymorphic hole (`let f x = printfn "%A" x`) zonks to a still-free `TyVar`
        // here; `freeze` generalises it to a method typar (`FTTypar(Method, i)`), which
        // the CLR encoder maps to `!!i` and `appendStructured` authors as the
        // `AppendStructured<!!i>` type argument (verified: `let f x = printfn "%A" x`
        // emits cleanly). The runtime dispatcher recovers the boxed runtime type, so the
        // engine renders the argument whatever it turns out to be.
        | TyVar _ -> true
        // The residual shapes (`TyUnknown`, `TyOr`, `TyKeyOf` / `TyIndexedAccess` /
        // `TyConditional`, `TyEnum`) are either unresolved-nominal errors the front end
        // rejects before the backend, or external-vocabulary type-level constructs a
        // real `%A` hole never carries — the encoder can't author a type argument for
        // them, so they stay cold.
        | _ -> false

    /// Lower a marked printf call (`Unification.tryInferPrintfApp` recorded a
    /// `PrintfApp` sink for it) into a `TExpr.Format`, pairing each specifier
    /// with the next argument in spec order (the format is arg 0). The happy
    /// path therefore never produces a `New PrintfFormat` / `App printfn`.
    ///
    /// Returns `ValueNone` to *decline* the lowering — when a `%A` (`Structured`)
    /// hole's argument type isn't faithful on the step-2 engine
    /// (`structuredArgFaithful`); the caller then falls back to the standard
    /// external-call (FSharp.Core cold) path for the whole format.
    let translatePrintfFormat
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr voption =
        let sink =
            match ctx.PrintfApp.TryGetValue key with
            | ValueSome s -> s
            | ValueNone -> failwithf "Elaborate.translatePrintfFormat: no PrintfApp marker at %O" key

        // The format argument's positional index, recovered from the sink kind:
        // `fprintf`/`fprintfn` (writer sink) put a `TextWriter` at arg 0 and the
        // format at arg 1; every other family has the format at arg 0. Kept in
        // lockstep with `PrintfSpec.Family.FormatArgIndex` (the gate's `idx`).
        let idx =
            match sink with
            | PrintfSpec.PrintfSink.Writer _
            | PrintfSpec.PrintfSink.Builder -> 1
            | _ -> 0

        // E1(b): the format slot may hold an `Ident` bound to a literal; recover it
        // (via the same `PrintfFormatLiterals` table the gate consulted) so the parts
        // walk sees the underlying `Expr.String`, exactly as for a syntactic literal.
        let formatArg =
            ValueOption.defaultValue args.[idx] (ctx.TryRecoverFormatLiteral args.[idx])

        let parts =
            match formatArg with
            | Expr.String(parts = parts) -> parts
            | other -> failwithf "Elaborate.translatePrintfFormat: format arg is not a string literal: %A" other

        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        // Holes consume the trailing args (the format is `args.[idx]`) in spec order.
        let mutable holeIdx = idx + 1
        // Set when a `%A` hole's argument type isn't faithful on the step-2 engine
        // (a record / DU / unknown). Forces the whole format onto the cold path.
        let mutable cold = false

        // Consume the args for a `%a`/`%t` callback hole and append its segment,
        // lowered capture-first to an ordinary residue-*string* expression. The
        // callback (a `Vesper.Fun`, often a closure) is applied FIRST to the sink,
        // then — for `%a` — to the value (curried order, see `PrintfSpec.argTypes`);
        // `%a`/`%t` carry no width/precision and never go cold. `sprintf` splices the
        // callback's returned string; writer/builder splice a block that runs the
        // callback into a fresh scratch and reads its buffer — every node of which is
        // ordinary TAST codegen already lowers, so no backend knows about sinks.
        let addCallbackSeg t holeForm hasValue =
            let callbackT = translateExpr ctx args.[holeIdx]
            holeIdx <- holeIdx + 1

            let valueT =
                if hasValue then
                    let v = translateExpr ctx args.[holeIdx]
                    holeIdx <- holeIdx + 1
                    ValueSome v
                else
                    ValueNone

            // The callback's OWN arrow type (`'State -> 'T -> 'Residue`, or `'State ->
            // 'Residue` for `%t`) drives each `App`'s result type — independent of the
            // concrete arg pushed (a writer family passes a `StringWriter` where the
            // callback's domain is the abstract `TextWriter`; a base-reference push is
            // implicitly compatible).
            let funcTy = Unification.zonk ctx.Store (TastWalk.exprTy callbackT)

            let applyCallback (stateArg: TExpr) : TExpr =
                match funcTy with
                | TyFun(_, afterState) ->
                    let appState = TExpr.App(callbackT, stateArg, afterState, t)

                    match valueT with
                    | ValueSome v ->
                        match afterState with
                        | TyFun(_, residueTy) -> TExpr.App(appState, v, residueTy, t)
                        | _ -> failwithf "Elaborate.addCallbackSeg: %%a callback lacks a value arrow: %A" funcTy
                    | ValueNone -> appState
                | _ -> failwithf "Elaborate.addCallbackSeg: callback is not a function type: %A" funcTy

            let residue =
                match ctx.PrintfCallbackScratch.TryGetValue key with
                | ValueNone ->
                    // `sprintf` (`'State = unit`): the callback returns the residue
                    // string directly.
                    applyCallback (TExpr.Const(TConstValue.Unit, ctx.Intrinsics.Unit, t))
                | ValueSome scratch ->
                    // Writer/builder: `{ let s = new Scratch() in (cb s [v]); s.ToString() }`.
                    // The callback writes into `s` (its `unit` residue discarded by the
                    // `Sequential`); the block yields `s`'s buffered text.
                    let sKey = NodeKey.ofSynthetic t.StartIndex NodeKind.SynthLambdaBody
                    let sVar () = TExpr.Var(sKey, scratch.ScratchTy, t)

                    let newScratch =
                        TExpr.New(scratch.ScratchClassName, ValueNone, EqArray.empty, scratch.ScratchTy, t)

                    let toStringCall =
                        TExpr.App(
                            TExpr.ExternalMember(
                                ValueSome(sVar ()),
                                scratch.ToStringKey,
                                "ToString",
                                MemberStorage.Method,
                                TyFun(ctx.Intrinsics.Unit, ctx.Intrinsics.String),
                                t
                            ),
                            TExpr.Const(TConstValue.Unit, ctx.Intrinsics.Unit, t),
                            ctx.Intrinsics.String,
                            t
                        )

                    let seq =
                        TExpr.Sequential(
                            EqArray.ofList [ applyCallback (sVar ()); toStringCall ],
                            ctx.Intrinsics.String,
                            t
                        )

                    TExpr.Let(TPat.NamedSimple(sKey, scratch.ScratchTy, t), newScratch, seq, ctx.Intrinsics.String, t)

            // `spec.Ty` records the `%a` value type (`unit` for `%t`) for provenance;
            // the residue is a `string` expr the backends splice like a `%s` hole.
            let specTy =
                match valueT with
                | ValueSome v -> Unification.zonk ctx.Store (TastWalk.exprTy v)
                | ValueNone -> ctx.Intrinsics.Unit

            let spec =
                {
                    Ty = specTy
                    Source = HoleSpecSource.Classified holeForm
                    Tok = t
                }

            segments.Add(FormatSeg.CallbackHole(spec, residue))

        // Consume the args for a plain value hole (optionally star-dimensioned) and
        // append its `Hole` / `DynHole` segment. A star *width* (`%*d`, `%*A`) then a
        // star *precision* (`%.*f`, `%.*e`, `%.*A`) each consume a leading `int` arg,
        // evaluated before the value in curried application order (width first, then
        // precision — the source arg order). The happy-path marker guarantees full
        // application (`totalArity`), so the indices line up. Walk args by per-hole arity.
        let addValueSeg t holeForm (placeholder: FormatPlaceholder) =
            let widthExpr =
                if placeholder.Width = FormatDim.Star then
                    let w = translateExpr ctx args.[holeIdx]
                    holeIdx <- holeIdx + 1
                    ValueSome w
                else
                    ValueNone

            let precisionExpr =
                if placeholder.Precision = FormatDim.Star then
                    let pr = translateExpr ctx args.[holeIdx]
                    holeIdx <- holeIdx + 1
                    ValueSome pr
                else
                    ValueNone

            let argExpr = args.[holeIdx]
            holeIdx <- holeIdx + 1
            let argT = translateExpr ctx argExpr
            // Zonk before the faithfulness check: a union-case application
            // (`S 3`) leaves a metavar that only resolves to `TyUnion` after
            // zonking (a record literal is concrete immediately), and an
            // unzonked `TyVar` would wrongly read as non-faithful (cold).
            let holeTy = Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr argExpr))

            // `%A` of a non-engine-faithful arg (a non-Vesper structural type —
            // FSharpOption / a BCL type — or an unknown) can't be rendered by the
            // structural engine, so the hole stays off the `Format` path and the
            // generic printf call stands; every Vesper-compiled record / DU (local
            // or external) is faithful now that step-3 synthesises their `Format`.
            if placeholder.Type = FormatType.Structured && not (structuredArgFaithful holeTy) then
                cold <- true

            let spec =
                {
                    Ty = holeTy
                    Source = HoleSpecSource.Classified holeForm
                    Tok = t
                }

            match widthExpr, precisionExpr with
            | ValueNone, ValueNone -> segments.Add(FormatSeg.Hole(spec, argT))
            | _ ->
                segments.Add(
                    FormatSeg.DynHole
                        {
                            Width = widthExpr
                            Precision = precisionExpr
                            Spec = spec
                            Value = argT
                        }
                )

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.VerbatimEscapeQuote t ->
                // Verbatim source text (escape unescaping is a pre-existing gap
                // shared with `translateString`). `%%` is the printf escape for a
                // literal `%`; the lexer folds it into a raw `Text` part, and
                // there's no runtime format pass to collapse it, so collapse here.
                // A real specifier is its own `FormatSpecifier` part, so every `%`
                // in a raw run is half of a `%%` pair.
                litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
            | StringPart.EscapePercent _ ->
                // `%%` denotes a literal `%`; no runtime format pass here, so
                // collapse now (the FSharp.Core path does it at runtime).
                litRun.Append('%') |> ignore
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Elaborate.translatePrintfFormat: unparsable specifier (marker invariant broken)"

                // Classify once here (also validating the marker invariant: the
                // specifier must be one a backend renders faithfully). The node carries
                // the classified `HoleForm`, so no consumer re-derives it.
                let holeForm =
                    match PrintfHoleForm.tryClassify placeholder with
                    | ValueSome hf -> hf
                    | ValueNone ->
                        failwith "Elaborate.translatePrintfFormat: unsupported specifier (marker invariant broken)"

                match holeForm with
                | PrintfHoleForm.HoleForm.Callback hasValue -> addCallbackSeg t holeForm hasValue
                | _ -> addValueSeg t holeForm placeholder
            | StringPart.Expr _
            | StringPart.OrphanFormatSpecifier _
            | StringPart.InvalidText _ ->
                failwith "Elaborate.translatePrintfFormat: non-literal format part (marker invariant broken)"

        flushLit ()

        if cold then
            ValueNone
        else
            let formatSink =
                match sink with
                | PrintfSpec.PrintfSink.StdOut nl -> FormatSink.ToStdOut nl
                | PrintfSpec.PrintfSink.StdErr nl -> FormatSink.ToStdErr nl
                | PrintfSpec.PrintfSink.StringResult -> FormatSink.ToString
                // The writer expression is the leading arg 0 (the format is arg 1);
                // `newline` threads `fprintfn`'s trailing `\n` into `EmitFormat`.
                | PrintfSpec.PrintfSink.Writer nl -> FormatSink.ToWriter(translateExpr ctx args.[0], nl)
                // `bprintf`: the `StringBuilder` is the leading arg 0 (the format is
                // arg 1). No `bprintfn`, so `ToBuilder` carries no trailing newline.
                | PrintfSpec.PrintfSink.Builder -> FormatSink.ToBuilder(translateExpr ctx args.[0])

            ValueSome(TExpr.Format(formatSink, EqArray.ofSeq segments, ty, tok))

    /// Lower a fully-unapplied lowerable printf partial (`ctx.PrintfPartial` marked
    /// it) to a synthesised Vesper closure `fun h1 … hn -> Format(sink, …)`. Each
    /// hole becomes a fresh lambda parameter that the `Format` node's segment reads
    /// as a `Var`; the format's literal runs and per-hole `HoleForm` are baked in
    /// exactly as the happy path bakes them, so the closure's `Invoke` — the same
    /// `EmitFormat` unroll — produces byte-identical output. `ty` is the App node's
    /// type: the curried printer arrow `h1 -> … -> hn -> tail`, whose domains supply
    /// the parameter types (in specifier order) and whose tail is the `Format`
    /// result. `%A`/`%O` (and `%a`/`%t`) are excluded at the gate, so every hole has
    /// a concrete argument type. Unlike `translatePrintfFormat` this path never
    /// declines: with no `%A` hole there is no faithfulness question, and the marker
    /// invariant guarantees each specifier parses and classifies.
    let translatePrintfPartial
        (ctx: PassContext)
        (key: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let sink =
            match ctx.PrintfPartial.TryGetValue key with
            | ValueSome s -> s
            | ValueNone -> failwithf "Elaborate.translatePrintfPartial: no PrintfPartial marker at %O" key

        let parts =
            match args.[0] with
            | Expr.String(parts = parts) -> parts
            | other -> failwithf "Elaborate.translatePrintfPartial: format arg is not a string literal: %A" other

        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()
        // The synthesised lambda parameters, one per hole, in specifier order.
        let parameters = ResizeArray<NodeKey * SemType * SyntaxToken>()

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        // Peel one printer-arrow domain per hole (specifier order matches the
        // curried arrow order — `PrintfSpec.printerType` folds the hole types onto
        // the tail left-to-right). The running codomain after the last hole is the
        // tail (the `Format` result).
        let mutable runningTy = Unification.zonk ctx.Store ty

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.VerbatimEscapeQuote t -> litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
            | StringPart.EscapePercent _ -> litRun.Append('%') |> ignore
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Elaborate.translatePrintfPartial: unparsable specifier (marker invariant broken)"

                let holeForm =
                    match PrintfHoleForm.tryClassify placeholder with
                    | ValueSome hf -> hf
                    | ValueNone ->
                        failwith "Elaborate.translatePrintfPartial: unsupported specifier (marker invariant broken)"

                let holeTy, restTy =
                    match runningTy with
                    | TyFun(dom, cod) -> dom, cod
                    | _ ->
                        failwithf
                            "Elaborate.translatePrintfPartial: printer type has fewer arrows than holes: %A"
                            (Unification.zonk ctx.Store ty)

                // A fresh parameter keyed off the specifier's own token offset —
                // distinct per hole (distinct source positions) and stable, so the
                // synthesised `Var` and `NamedSimple` binder agree.
                let paramKey = NodeKey.ofSynthetic t.StartIndex NodeKind.SynthLambdaBody
                parameters.Add(paramKey, holeTy, t)

                segments.Add(
                    FormatSeg.Hole(
                        {
                            Ty = holeTy
                            Source = HoleSpecSource.Classified holeForm
                            Tok = t
                        },
                        TExpr.Var(paramKey, holeTy, t)
                    )
                )

                runningTy <- Unification.zonk ctx.Store restTy
            | StringPart.Expr _
            | StringPart.OrphanFormatSpecifier _
            | StringPart.InvalidText _ ->
                failwith "Elaborate.translatePrintfPartial: non-literal format part (marker invariant broken)"

        flushLit ()

        let formatSink =
            match sink with
            | PrintfSpec.PrintfSink.StdOut nl -> FormatSink.ToStdOut nl
            | PrintfSpec.PrintfSink.StdErr nl -> FormatSink.ToStdErr nl
            | PrintfSpec.PrintfSink.StringResult -> FormatSink.ToString
            // The 4a partial gate is `idx = 0`, so a writer / builder sink
            // (`fprintf` / `bprintf` partial, `idx = 1`) never reaches this path —
            // those stay cold.
            | PrintfSpec.PrintfSink.Writer _
            | PrintfSpec.PrintfSink.Builder ->
                failwith
                    "Elaborate.translatePrintfPartial: writer/builder sink is not a partial-lowering shape (marker invariant broken)"

        // `runningTy` is now the tail; the `Format` node returns it.
        let mutable body = TExpr.Format(formatSink, EqArray.ofSeq segments, runningTy, tok)
        let mutable resultTy = runningTy

        // Wrap innermost-last so the outermost lambda's type is the whole printer
        // arrow (equal to `ty`), exactly as `translateFun` folds a source lambda.
        for i = parameters.Count - 1 downto 0 do
            let (pk, pty, ptok) = parameters.[i]
            let lamTy = TyFun(pty, resultTy)
            body <- TExpr.Lambda(TPat.NamedSimple(pk, pty, ptok), body, lamTy, ptok)
            resultTy <- lamTy

        body
