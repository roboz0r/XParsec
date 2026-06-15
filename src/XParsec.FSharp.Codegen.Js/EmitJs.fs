namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The `TAST → JsAst` walker — the JS analogue of `Emit*` in `Codegen.Clr`. Step
/// 0a covers exactly the slice a `printfn "hi"` program needs: a string `Const`,
/// and a top-level printf-family `Format` whose segments are all literals lowered
/// to `console.log` / `console.error`. Every other node is an explicit `failwithf`
/// so an unsupported arm surfaces loudly rather than dropping silently — the band
/// of arms grows one step at a time (codegen-js-steps.md), and `EmitExpr.buildExpr`
/// in the CLR backend is the master checklist each step works toward.
///
/// Step 0b adds source-map `loc`s: each emitted top-level `JsExpr` carries the
/// (line, col) of its originating `TExprG` node's `'tok` (`SyntaxToken`), resolved
/// against the raw source text via `LineIndex`.
module EmitJs =

    /// Maps a source char offset (a `SyntaxToken.StartIndex`) to 0-based
    /// (line, column) — V3 source-map coordinates. Built once per compile from
    /// the raw source text: `Starts.[n]` is the char offset at which line `n`
    /// begins. A trailing `\r` rides its line; columns count UTF-16 code units,
    /// which V3 maps require.
    type LineIndex = { Starts: int[]; Length: int }

    module LineIndex =
        let build (source: string) : LineIndex =
            let starts = ResizeArray<int>()
            starts.Add 0

            for i in 0 .. source.Length - 1 do
                if source.[i] = '\n' then
                    starts.Add(i + 1)

            {
                Starts = starts.ToArray()
                Length = source.Length
            }

        /// Resolve a char offset to a `JsLoc`. Offsets past end-of-source clamp
        /// to the last line (defensive — a virtual token can anchor at the spawn
        /// offset, which is in range, but synthetic ends-of-input may not be).
        let resolve (idx: LineIndex) (offset: int) : JsLoc =
            let offset = max 0 (min offset idx.Length)
            let starts = idx.Starts
            // Binary search for the greatest line start <= offset.
            let mutable lo = 0
            let mutable hi = starts.Length - 1

            while lo < hi do
                let mid = (lo + hi + 1) / 2

                if starts.[mid] <= offset then lo <- mid else hi <- mid - 1

            {
                Line = lo
                Column = offset - starts.[lo]
            }

    /// Resolve a node's `'tok` to a source `loc`. `ValueNone` disables maps
    /// (no source text supplied); `ValueSome` carries the line index.
    type Resolver = LineIndex voption

    let private locOf (resolver: Resolver) (tok: SyntaxToken) : JsLoc voption =
        match resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    let private console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("console", ValueNone), JsExpr.Identifier(method, ValueNone), false, ValueNone)

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

    let rec buildExpr (resolver: Resolver) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf resolver (TastWalk.exprTok e)

        match e with
        | TExprG.Const(TConstValue.String s, _, _) -> JsExpr.Literal(JsLiteral.String s, loc)

        | TExprG.Format(sink, segments, _, _) ->
            let arg = JsExpr.Literal(JsLiteral.String(literalFormat segments), ValueNone)

            match sink with
            // `console.log` / `console.error` append the trailing newline
            // themselves, matching `printfn` / `eprintfn`. The no-newline
            // `printf` / `eprintf` sinks (a `process.stdout.write`) await a later
            // step, as do the `sprintf` (`ToString`) / `fprintf` (`ToWriter`) sinks.
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            | other -> failwithf "EmitJs (Step 0a): unsupported format sink %A" other

        | other -> failwithf "EmitJs (Step 0a): unsupported expression %A" other

    /// An expression evaluated for effect (a top-level `do` such as `printfn`).
    let buildStatement (resolver: Resolver) (e: Frozen.TExpr) : JsStatement =
        JsStatement.Expression(buildExpr resolver e)

    /// The whole frozen file → a `Program`. Step 0a handles only top-level
    /// expression declarations; `let` / `type` decls await Steps 1+.
    let buildProgram (resolver: Resolver) (tast: Frozen.TastFile) : JsProgram =
        let body =
            [
                for decl in tast.Decls do
                    match decl with
                    | TDeclG.Expression(e, _) -> buildStatement resolver e
                    | other -> failwithf "EmitJs (Step 0a): unsupported declaration %A" other
            ]

        { Body = body }
