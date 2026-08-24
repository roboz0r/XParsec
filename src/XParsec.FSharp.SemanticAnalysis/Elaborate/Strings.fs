namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateLiterals
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

// String-literal and interpolation lowering for the Elaborate pass: a printf format literal
// becomes `new PrintfFormat(text)`, a faithfully-renderable interpolation a `TExpr.Format`,
// everything else a stitched `Const`.

module internal ElaborateStrings =

    /// Interpolation holes have no rendering on this path, so they surface as
    /// `{<expr>}` placeholders. Only reached for plain strings, printf format
    /// literals, and interpolations a hole kept off the `TExpr.Format` path.
    let private stitchLiteralString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        foldStringParts ctx (fun () -> "{<expr>}") parts

    /// Classify one interpolation hole into the `HoleSpecSource` a `FormatSeg.Hole` carries,
    /// or `None` if it can't be rendered faithfully. `%d{x}` becomes `Classified`, a plain
    /// `{x}` / `{x:fmt}` a `RawFormat`; alignment (`{x,n}`) is not representable at all.
    let private tryInterpHoleSpec
        (ctx: PassContext)
        (formatSpecifier: SyntaxToken voption)
        (formatClause: SyntaxToken voption)
        : HoleSpecSource option =
        match formatSpecifier with
        | ValueSome ft ->
            match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
            | ValueSome p ->
                // The same gate as the printf path: only specifiers representable as a
                // structured `Format` lower, and the classification is kept on the node
                // rather than re-derived per backend.
                match PrintfHoleForm.classify p with
                | PrintfHoleForm.HoleVerdict.Lowerable hf -> Some(HoleSpecSource.Classified hf)
                | PrintfHoleForm.HoleVerdict.Residual
                | PrintfHoleForm.HoleVerdict.SignLeftAlignZeroPad -> None
            | ValueNone -> None
        | ValueNone ->
            let fmt =
                match formatClause with
                | ValueSome fc ->
                    let raw = ctx.NameOf fc

                    let f =
                        if raw.StartsWith(":", System.StringComparison.Ordinal) then
                            raw.Substring 1
                        else
                            raw

                    if f.Length = 0 then None else Some f
                | ValueNone -> None

            Some(HoleSpecSource.RawFormat fmt)

    /// Lower an interpolated string (`$"…{x}…"`) to a `TExpr.Format`. `None`, which keeps the
    /// literal-stitch fallback, when the string has no holes, or any hole isn't faithfully
    /// renderable: a free hole type, an orphan `%spec`, or an uncovered specifier.
    let private tryTranslateInterpolation
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr option =
        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()
        let mutable hasHole = false
        let mutable lowerable = true

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        for part in parts do
            if lowerable then
                match part with
                // `%%` collapses to `%`, because an interpolated string rides the same
                // `PrintfFormat` machinery as printf. Escape sequences stay VERBATIM: the
                // literal-stitch path does not unescape them either.
                | StringPart.Text t
                | StringPart.EscapeSequence t
                | StringPart.VerbatimEscapeQuote t -> litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
                | StringPart.EscapePercent _ -> litRun.Append('%') |> ignore
                | StringPart.Expr(formatSpecifier = fs; lBrace = lBrace; expr = holeExpr; formatClause = fc) ->
                    hasHole <- true
                    let holeTy = typeOfKey ctx (CstKeys.ofExpr holeExpr)

                    match Unification.zonk ctx.Store holeTy with
                    // A free hole type can't pick an `AppendFormatted<T>`.
                    | TyVar _ -> lowerable <- false
                    | zHoleTy ->
                        match tryInterpHoleSpec ctx fs fc with
                        | Some source ->
                            flushLit ()

                            // Source-map token: the specifier (`%d`) or format clause
                            // (`:fmt`) when present, else the opening brace.
                            let specTok =
                                match fs with
                                | ValueSome t -> t
                                | ValueNone ->
                                    match fc with
                                    | ValueSome c -> c
                                    | ValueNone -> lBrace

                            segments.Add(
                                FormatSeg.Hole(
                                    {
                                        Ty = zHoleTy
                                        Source = source
                                        Tok = specTok
                                    },
                                    translateExpr ctx holeExpr
                                )
                            )
                        | None -> lowerable <- false
                // A standalone `%spec`, orphan specifier, or lexer-error part has
                // interpolation-specific semantics this does not model, so keep the whole
                // string on the literal-stitch fallback.
                | StringPart.FormatSpecifier _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> lowerable <- false

        if hasHole && lowerable then
            flushLit ()
            Some(TExpr.Format(FormatSink.ToString, EqArray.ofSeq segments, ty, tok))
        else
            None

    let translateString
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match e with
        | Expr.String(parts = parts) ->
            match Unification.zonk ctx.Store ty with
            | TyClass(key, _) when key = RuntimeNames.printfFormatKey ->
                // A format literal at a printf call site denotes `new PrintfFormat<…>(text)`.
                TExpr.New(
                    PrintfSpec.printfFormatName,
                    // There is a single `value: string` ctor, so codegen resolves it by arity.
                    ValueNone,
                    EqArray.singleton (
                        TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), ctx.Intrinsics.String, tok)
                    ),
                    ty,
                    tok
                )
            | _ ->
                // A plain string, or an interpolation with an unrenderable hole, stitches to
                // literal text keeping that hole's `{<expr>}` placeholder.
                match tryTranslateInterpolation translateExpr ctx parts ty tok with
                | Some node -> node
                | None -> TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), ty, tok)
        | _ -> failwithf "Elaborate.translateString: not a String expr: %A" e
