namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

module Pipeline =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// A parsed file: the token stream and tree every pass runs on, and the diagnostics
    /// RECOVERY raised producing them. A COMPLETE tree can still have had every delimiter
    /// inserted and every missing expression stubbed, so `Ok` routinely carries diagnostics.
    type ParsedFile =
        {
            Lexed: Lexed
            File: ImplementationFile<SyntaxToken>
            Diagnostics: Diagnostic list
        }

    /// A file no tree came out of. `Lexed` is present iff LEXING succeeded, so the recovery
    /// diagnostics raised before the parser gave up still have the token stream their
    /// positions resolve against.
    type ParseFailure =
        {
            Lexed: Lexed voption
            Diagnostics: Diagnostic list
        }

    /// The parser's diagnostics as the semantic layer sees them, in SOURCE order — the
    /// parser accumulates them reversed. The underlying `ParseError` does not cross: it is
    /// parser-internal.
    let private ofParseDiagnostics (diagnostics: XParsec.FSharp.Parser.Diagnostic list) : Diagnostic list =
        // Both delimiter diagnostics point back at the delimiter left open.
        let openedHere (openedAt: Site) : Label list =
            [
                {
                    Site = openedAt
                    Message = DiagnosticCode.openedHereLabel
                }
            ]

        [
            for d in List.rev diagnostics do
                let site, related =
                    match d.Code with
                    // The close was never written: the parser SYNTHESISED one, so the
                    // mistake is the hole it went into, not the token that exposed it.
                    | DiagnosticCode.UnclosedDelimiter(openedAt = openedAt) ->
                        Site.gapBefore (Site.ofToken d.Token), openedHere openedAt
                    // The close IS written, just the wrong one, and nothing was inserted —
                    // so that token is the mistake.
                    | DiagnosticCode.MismatchedDelimiter(openedAt = openedAt) ->
                        Site.ofToken d.Token, openedHere openedAt
                    | _ ->
                        match d.TokenEnd with
                        | Some last -> Site.spanning [ d.Token; last ], []
                        | None -> Site.ofToken d.Token, []

                Diagnostic.create (Kind.Parse d.Code) site related
        ]

    /// The front-end parse chain, lex → reader → AST: a bare-expression `ScriptFragment`
    /// wraps as an `AnonymousModule`, and lex/parse failures surface as `Diagnostic`s
    /// (never exceptions). The parser's recovery diagnostics ride out on BOTH arms.
    let parse (source: string) : Result<ParsedFile, ParseFailure> =
        match Lexing.lexString source with
        | Result.Error e ->
            Error
                {
                    Lexed = ValueNone
                    // A whole-file lex failure blames no place in the file.
                    Diagnostics = [ Diagnostic.nowhere (Kind.LexFailure(sprintf "%A" e)) ]
                }
        | Result.Ok lexed ->
            let reader = Reader.ofLexed lexed Set.empty

            let failed (kind: Kind) =
                Error
                    {
                        Lexed = ValueSome lexed
                        Diagnostics = Diagnostic.nowhere kind :: ofParseDiagnostics reader.State.Diagnostics
                    }

            let parsed (file: ImplementationFile<SyntaxToken>) =
                Ok
                    {
                        Lexed = lexed
                        File = file
                        Diagnostics = ofParseDiagnostics reader.State.Diagnostics
                    }

            match FSharpAst.parse reader with
            | Result.Error e -> failed (Kind.ParseFailure(sprintf "%A" e))
            | Result.Ok(FSharpAst.ImplementationFile f) -> parsed f
            | Result.Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems)) ->
                parsed (ImplementationFile.AnonymousModule elems)
            | Result.Ok other -> failed (Kind.ParseFailure(sprintf "unexpected AST: %A" other))

    /// `parse`, refusing a tree the parser had to PATCH: every inserted delimiter and every
    /// `Expr.Missing` is a hole the source did not fill, so a recovered parse is not a
    /// compilable one.
    let parseUnrecovered (source: string) : Result<ParsedFile, Diagnostic list> =
        match parse source with
        | Error f -> Error f.Diagnostics
        | Ok parsed ->
            match parsed.Diagnostics with
            | [] -> Ok parsed
            | recovered -> Error recovered

    /// Runs every pass through the `SemType` domain, returning the populated `PassContext`
    /// and the pre-freeze `TastFile`. `assemblyName` is the assembly this file emits into;
    /// `""` for the front-end-only paths that never emit.
    let analyseSemWithContextForCore
        (selfHostList: bool)
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        let ctx = PassContext(provider, source)
        ctx.AssemblyName <- assemblyName
        // A self-host (BCL-only) package build has no FSharp.Core, so an unpinned `[]`/`::`
        // must default to the Vesper cons-list.
        ctx.DefaultListIsVesper <- selfHostList
        Desugar.run ctx file
        NameResolution.run ctx file
        Unification.run ctx file
        Validation.run ctx file
        // Elaboration lowers the CST to a typar-quantified TAST with every inline call site
        // already expanded, so escape analysis below sees the closures codegen emits.
        let tast0 = Elaborate.run ctx file
        Regions.run ctx tast0.Decls tast0.Specializations
        // Codegen has no `PassContext`, so the closure verdicts decided in side tables are
        // snapshotted onto the TastFile here, now that both are populated.
        let tast0 =
            { tast0 with
                ClosureReprs = Regions.closureReprSnapshot ctx tast0.Decls
                FunVerdicts =
                    ctx.FunVerdicts.AsDictionary()
                    |> Seq.map (fun kv -> kv.Key, kv.Value)
                    |> Map.ofSeq
            }
        // Promotes `let mutable` cells captured by escaping closures. Running before the
        // guards below keeps their sweeps observing post-promotion types.
        let tast1 = RefCellPromotion.run ctx tast0
        // Three whole-tree guards over the settled `SemType` domain, each reporting a
        // per-decl diagnostic rather than letting a backend emitter `failwith` later.
        ResolvedTypes.run ctx tast1
        PlatformTypes.run ctx tast1
        DynamicEscape.run ctx
        // Re-snapshot `ctx.Diagnostics` so the three guards' findings reach the TastFile.
        let tast =
            { tast1 with
                Diagnostics = List.ofSeq ctx.Diagnostics
            }

        ctx, tast

    /// The default front end: a bare-program list literal defaults to FSharp.Core's
    /// `list`. Self-host package builds use `…ForSelfHost` below.
    let analyseSemWithContextFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextForCore false assemblyName provider source file

    /// Every pass plus the final `SemType → FrozenType` freeze: the frozen tree AS POOLS,
    /// which is what codegen consumes. `SemType` consumers use the `…Sem…` variants above.
    let analyseWithContextFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        let ctx, tast = analyseSemWithContextFor assemblyName provider source file
        ctx, Freeze.run ctx tast

    /// `analyseSemWithContextFor` with no home assembly, for the front-end-only entries
    /// (side-table inspection tests, contract scrapes).
    let analyseSemWithContext
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextFor "" provider source file

    /// `analyseWithContextFor` with no home assembly.
    let analyseWithContext
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        analyseWithContextFor "" provider source file

    /// The `SemType` (pre-freeze) production entry, discarding the `PassContext`.
    let analyseSemFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseSemWithContextFor assemblyName provider source file
        tast

    /// The production entry. `assemblyName` is the home assembly for local keys.
    let analyseFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        let _, tast = analyseWithContextFor assemblyName provider source file
        tast

    let analyseSem
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        analyseSemFor "" provider source file

    let analyse
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        analyseFor "" provider source file

    /// The self-host pre-freeze entry — like `analyseSem` but a bare-program list
    /// literal/pattern defaults to the Vesper cons-list, not FSharp.Core's `list`. The JS
    /// target has no FSharp.Core, so the cons-list is its only list representation.
    let analyseSemForSelfHost
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseSemWithContextForCore true "" provider source file
        tast

    /// `analyseSemForSelfHost`, keeping the `PassContext`. A caller that inspects the tree's
    /// diagnostics before freezing it needs both halves: the freeze reads each residual
    /// typar root's bound variable out of the context.
    let analyseSemForSelfHostWithContext
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextForCore true "" provider source file

    /// The self-host production entry: like `analyseFor` but a bare-program list
    /// literal/pattern defaults to the Vesper cons-list, so a BCL-only package with no
    /// FSharp.Core reference emits cons-list only.
    let analyseForSelfHost
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (source: OriginSource)
        (file: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        let ctx, tast = analyseSemWithContextForCore true assemblyName provider source file

        Freeze.run ctx tast
