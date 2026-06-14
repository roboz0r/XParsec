namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// The `TAST → JsAst` walker — the JS analogue of `Emit*` in `Codegen.Clr`. Step
/// 0a covers exactly the slice a `printfn "hi"` program needs: a string `Const`,
/// and a top-level printf-family `Format` whose segments are all literals lowered
/// to `console.log` / `console.error`. Every other node is an explicit `failwithf`
/// so an unsupported arm surfaces loudly rather than dropping silently — the band
/// of arms grows one step at a time (codegen-js-steps.md), and `EmitExpr.buildExpr`
/// in the CLR backend is the master checklist each step works toward.
module EmitJs =

    let private console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier "console", JsExpr.Identifier method, false)

    /// Fold a printf-family format whose segments are all literals into a single
    /// string. Step 0a does not emit holes: a `%d`-style placeholder needs the
    /// Step 1 `ILIntrinsic` template path, so it fails loudly until then.
    let private literalFormat (segments: EqArray<Frozen.FormatSeg>) : string =
        let sb = System.Text.StringBuilder()

        for seg in segments do
            match seg with
            | FormatSegG.Lit s -> sb.Append s |> ignore
            | FormatSegG.Hole _ -> failwith "EmitJs (Step 0a): format holes are not yet supported"

        sb.ToString()

    let rec buildExpr (e: Frozen.TExpr) : JsExpr =
        match e with
        | TExprG.Const(TConstValue.String s, _) -> JsExpr.Literal(JsLiteral.String s)

        | TExprG.Format(sink, segments, _) ->
            let arg = JsExpr.Literal(JsLiteral.String(literalFormat segments))

            match sink with
            // `console.log` / `console.error` append the trailing newline
            // themselves, matching `printfn` / `eprintfn`. The no-newline
            // `printf` / `eprintf` sinks (a `process.stdout.write`) await a later
            // step, as do the `sprintf` (`ToString`) / `fprintf` (`ToWriter`) sinks.
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ])
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ])
            | other -> failwithf "EmitJs (Step 0a): unsupported format sink %A" other

        | other -> failwithf "EmitJs (Step 0a): unsupported expression %A" other

    /// An expression evaluated for effect (a top-level `do` such as `printfn`).
    let buildStatement (e: Frozen.TExpr) : JsStatement = JsStatement.Expression(buildExpr e)

    /// The whole frozen file → a `Program`. Step 0a handles only top-level
    /// expression declarations; `let` / `type` decls await Steps 1+.
    let buildProgram (tast: Frozen.TastFile) : JsProgram =
        let body =
            [
                for decl in tast.Decls do
                    match decl with
                    | TDeclG.Expression(e, _) -> buildStatement e
                    | other -> failwithf "EmitJs (Step 0a): unsupported declaration %A" other
            ]

        { Body = body }
