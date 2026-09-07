namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

// Printf lowering: a marked happy-path call becomes a `TExpr.Format`; a marked
// fully-unapplied lowerable partial becomes a synthesised closure over one.

module internal ElaboratePrintf =

    /// The scalars `%A` renders atomically: numerics carry the literal suffixes the engine
    /// reproduces (`5L`, `1.5M`); `string` / `char` / `bool` are special-cased atoms.
    let private isAtomScalar =
        RuntimeNames.isKeyIn (
            RuntimeNames.stringKey
            :: RuntimeNames.charKey
            :: RuntimeNames.boolKey
            :: RuntimeNames.numericKeys
        )

    /// Whether `%A` of an argument of this (zonked) type may lower to the structural engine.
    /// The runtime `%A` dispatcher ends in a `value.ToString()` fallback, so every concrete nominal
    /// qualifies; only a type the backend can't author an `AppendStructured<T>` argument for stays cold.
    let rec private structuredArgFaithful (t: SemType) : bool =
        match t with
        // The array intrinsic renders via the `IEnumerable` arm, so it is faithful iff
        // its element type is.
        | TyArray elem -> structuredArgFaithful elem
        | TyConst(key, args) ->
            // Matched by KEY, so a user type of the same name is not mistaken for one. All
            // are niladic, so any type argument means it isn't really the intrinsic.
            isAtomScalar key && args.Length = 0
        | TyTuple items -> Block.forall structuredArgFaithful items
        // The cons-list stays faithful-iff-its-element-is.
        | TyUnion(key, args) when RuntimeNames.isVesperListKey key -> Block.forall structuredArgFaithful args
        // Every nominal record / DU / class renders on the engine, wherever its assembly
        // lives. No recursion into fields: this is a cold-vs-engine switch, not a
        // per-field renderer, and the runtime dispatcher already routes each field.
        | TyUnion _
        | TyRecord _
        | TyClass _ -> true
        // A polymorphic hole (`let f x = printfn "%A" x`) zonks to a still-free `TyVar`;
        // freeze generalises it to `FTTypar(ModuleFunction _, i)`, which the CLR encoder
        // maps to `!!i` and authors as the `AppendStructured<!!i>` type argument.
        | TyVar _ -> true
        // The residual shapes (`TyUnknown`, `TyOr`, `TyKeyOf`, `TyIndexedAccess`,
        // `TyConditional`, `TyEnum`) have no type argument the encoder can author.
        | _ -> false

    /// Append one literal part of a format string to the running literal run. An escape
    /// denoting no character keeps its raw text and answers `ValueSome` with its diagnostic.
    /// There is no runtime format pass on this path, so `%%` collapses
    /// to `%` here; every other token decodes as `StringLiterals.decodeLiteralToken` gives it.
    let private appendFormatLiteral
        (ctx: PassContext)
        (litRun: System.Text.StringBuilder)
        (t: SyntaxToken)
        : Kind voption =
        match t.Token with
        | Token.EscapePercent ->
            litRun.Append '%' |> ignore
            ValueNone
        | _ ->
            match StringLiterals.decodeLiteralToken ctx.NameOf t with
            | DecodedLiteralToken.Text text ->
                litRun.Append text |> ignore
                ValueNone
            | DecodedLiteralToken.Invalid(kind, raw) ->
                litRun.Append raw |> ignore
                ValueSome kind

    /// Lower a `PrintfLowering.Full` call into a `TExpr.Format`, pairing each
    /// specifier with the next argument in spec order. `ValueNone` *declines* the lowering:
    /// one unfaithful `%A` hole sends the whole format down the FSharp.Core cold path.
    let translatePrintfFormat
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (sink: PrintfSpec.PrintfSink)
        (scratch: PrintfSpec.CallbackScratch voption)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr voption =
        // The format argument's positional index, from the sink kind: a writer or builder
        // sink occupies arg 0 and the format arg 1; every other family has it at arg 0.
        // Kept in lockstep with `PrintfSpec.Family.FormatArgIndex`.
        let idx =
            match sink with
            | PrintfSpec.PrintfSink.Writer _
            | PrintfSpec.PrintfSink.Builder -> 1
            | _ -> 0

        // The format slot may hold an `Ident` bound to a literal; recover it so the
        // parts walk sees the underlying `Expr.String`.
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
        // Set when a `%A` hole's argument isn't faithful on the structural engine,
        // which forces the whole format onto the cold path.
        let mutable cold = false

        // Consume the args for a `%a`/`%t` callback hole and append its segment as an
        // ordinary residue-*string* expression: the callback applies FIRST to the sink, then
        // (for `%a`) to the value. `%a`/`%t` carry no width/precision and never go cold.
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

            // The callback's OWN function type (`'State -> 'T -> 'Residue`, or `'State ->
            // 'Residue` for `%t`) drives each `App`'s result type, independently of the arg
            // pushed (a writer family passes a `StringWriter` for a `TextWriter` domain).
            let funcTy = Unification.zonk ctx.Store (TastWalk.exprTy callbackT)

            let applyCallback (stateArg: TExpr) : TExpr =
                match funcTy with
                | TyFun(_, afterState) ->
                    let appState = TExpr.App(callbackT, stateArg, afterState, t)

                    match valueT with
                    | ValueSome v ->
                        match afterState with
                        | TyFun(_, residueTy) -> TExpr.App(appState, v, residueTy, t)
                        | _ -> failwithf "Elaborate.addCallbackSeg: %%a callback lacks a value parameter: %A" funcTy
                    | ValueNone -> appState
                | _ -> failwithf "Elaborate.addCallbackSeg: callback is not a function type: %A" funcTy

            let residue =
                match scratch with
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
                        TExpr.New(scratch.ScratchClassName, ValueNone, Block.empty, scratch.ScratchTy, t)

                    let toStringCall =
                        TExpr.App(
                            TExpr.ExternalMember(
                                ValueSome(sVar ()),
                                scratch.ToStringKey,
                                "ToString",
                                MemberStorage.Method,
                                // `ToString()`: one group taking `unit`, so zero pushed arguments.
                                Block.singleton 0,
                                TyFun(ctx.Intrinsics.Unit, ctx.Intrinsics.String),
                                t
                            ),
                            TExpr.Const(TConstValue.Unit, ctx.Intrinsics.Unit, t),
                            ctx.Intrinsics.String,
                            t
                        )

                    let seq =
                        TExpr.Sequential(
                            Block.ofList [ applyCallback (sVar ()); toStringCall ],
                            ctx.Intrinsics.String,
                            t
                        )

                    TExpr.Let(
                        {
                            Pattern = TPat.NamedSimple(sKey, scratch.ScratchTy, t, false)
                            Value = newScratch
                            Tok = t
                        },
                        seq,
                        false,
                        ctx.Intrinsics.String
                    )

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

        // Consume the args for a plain value hole and append its `Hole` / `DynHole` segment.
        // A star *width* (`%*d`) then a star *precision* (`%.*f`) each consume a leading
        // `int` arg, in source order before the value.
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
            // Zonk before the faithfulness check: an unresolved `TyVar` reads as
            // faithful, so a metavar standing for an unauthorable type (a union-case
            // application `S 3` leaves one) would wrongly take the engine path.
            let holeTy = Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr argExpr))

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
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t ->
                // An escape denoting no character sends the whole call down the cold path,
                // where `stitchLiteralString` reports it once.
                if (appendFormatLiteral ctx litRun t).IsSome then
                    cold <- true
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Elaborate.translatePrintfFormat: unparsable specifier (marker invariant broken)"

                // Classified once: the node carries the `HoleForm`, so no consumer re-derives it.
                let holeForm =
                    match PrintfHoleForm.classify placeholder with
                    | PrintfHoleForm.HoleVerdict.Lowerable hf -> hf
                    | PrintfHoleForm.HoleVerdict.Residual
                    | PrintfHoleForm.HoleVerdict.SignLeftAlignZeroPad
                    | PrintfHoleForm.HoleVerdict.OversizedDimension ->
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
                // The writer expression is the leading arg 0; `nl` carries `fprintfn`'s
                // trailing newline through to the backend.
                | PrintfSpec.PrintfSink.Writer nl -> FormatSink.ToWriter(translateExpr ctx args.[0], nl)
                // `bprintf`: the `StringBuilder` is the leading arg 0. There is no
                // `bprintfn`, so `ToBuilder` carries no trailing newline.
                | PrintfSpec.PrintfSink.Builder -> FormatSink.ToBuilder(translateExpr ctx args.[0])

            ValueSome(TExpr.Format(formatSink, Block.ofSeq segments, ty, tok))

    /// Lower a `PrintfLowering.Partial` call (a fully-unapplied printf partial)
    /// to a synthesised closure `fun h1 … hn -> Format(sink, …)`. `ty` is the curried printer
    /// `h1 -> … -> hn -> codomain`: its domains are the parameter types in specifier order.
    let translatePrintfPartial
        (ctx: PassContext)
        (sink: PrintfSpec.PrintfSink)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
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

        // Peel one printer domain per hole, in specifier order. The codomain left
        // after the last hole is what the `Format` node returns.
        let mutable runningTy = Unification.zonk ctx.Store ty

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t ->
                // No cold path exists for a lowered partial, so the escape is reported here.
                match appendFormatLiteral ctx litRun t with
                | ValueSome kind -> ctx.Report(t, kind)
                | ValueNone -> ()
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Elaborate.translatePrintfPartial: unparsable specifier (marker invariant broken)"

                let holeForm =
                    match PrintfHoleForm.classify placeholder with
                    | PrintfHoleForm.HoleVerdict.Lowerable hf -> hf
                    | PrintfHoleForm.HoleVerdict.Residual
                    | PrintfHoleForm.HoleVerdict.SignLeftAlignZeroPad
                    | PrintfHoleForm.HoleVerdict.OversizedDimension ->
                        failwith "Elaborate.translatePrintfPartial: unsupported specifier (marker invariant broken)"

                let holeTy, restTy =
                    match runningTy with
                    | TyFun(dom, cod) -> dom, cod
                    | _ ->
                        failwithf
                            "Elaborate.translatePrintfPartial: printer type has fewer parameters than holes: %A"
                            (Unification.zonk ctx.Store ty)

                // A parameter key off the specifier's own token offset is distinct per hole
                // and stable, so the synthesised `Var` and its `NamedSimple` bound variable agree.
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
            // The partial marker requires the format at arg 0, so a writer / builder
            // sink never reaches this path, and those partials stay cold.
            | PrintfSpec.PrintfSink.Writer _
            | PrintfSpec.PrintfSink.Builder ->
                failwith
                    "Elaborate.translatePrintfPartial: writer/builder sink is not a partial-lowering shape (marker invariant broken)"

        // `runningTy` is now the codomain; the `Format` node returns it.
        let mutable body = TExpr.Format(formatSink, Block.ofSeq segments, runningTy, tok)
        let mutable resultTy = runningTy

        // Wrap innermost-last, so the outermost lambda's type is the whole printer type (`ty`).
        for i = parameters.Count - 1 downto 0 do
            let (pk, pty, ptok) = parameters.[i]
            let lamTy = TyFun(pty, resultTy)
            body <- TExpr.Lambda(TPat.NamedSimple(pk, pty, ptok, false), body, lamTy, ptok)
            resultTy <- lamTy

        body
