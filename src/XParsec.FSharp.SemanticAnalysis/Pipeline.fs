namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

module Pipeline =

    // The alias binds `Diagnostic` to the SemanticAnalysis one throughout this module; see
    // that type's declaration for why the bare name would otherwise be the parser's.
    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// A parsed unit: the token stream and tree every pass runs on, and the diagnostics
    /// RECOVERY raised producing them. A file can parse to a COMPLETE tree and still have
    /// had every delimiter in it inserted and every missing expression stubbed, so a
    /// successful parse carries diagnostics as routinely as a failed one.
    type ParsedUnit =
        {
            Lexed: Lexed
            File: ImplementationFile<SyntaxToken>
            Diagnostics: Diagnostic list
        }

    /// A unit no tree came out of. `Lexed` is present whenever LEXING succeeded, so the
    /// recovery diagnostics raised before the parser gave up still have the token stream
    /// their positions resolve against; only a lex failure has none.
    type ParseFailure =
        {
            Lexed: Lexed voption
            Diagnostics: Diagnostic list
        }

    /// The parser's diagnostics as the semantic layer sees them, in SOURCE order —
    /// `ParseState.Diagnostics` accumulates reversed. The parser-side `Error`
    /// (the underlying `ParseError`) does NOT cross: it is parser-internal, and only
    /// `Debug.printDiagnostics` renders it.
    let private ofParseDiagnostics (diagnostics: XParsec.FSharp.Parser.Diagnostic list) : Diagnostic list =
        // Both delimiter diagnostics point back at the delimiter left open; only their
        // PRIMARY differs, because only one of them describes a hole.
        let openedHere (opened: SyntaxToken) : Label list =
            [
                {
                    Site = Site.ofToken opened
                    Message = DiagnosticCode.openedHereLabel
                }
            ]

        [
            for d in List.rev diagnostics do
                let site, related =
                    match d.Code with
                    // The close was never written: the parser SYNTHESISED one, so the
                    // mistake is the hole it went into, and the token that exposed the
                    // absence is innocent.
                    | DiagnosticCode.UnclosedDelimiter(opened, _) -> Site.gapBefore d.Token, openedHere opened
                    // The close IS written, just the wrong one, and the parser consumed it
                    // as the close. Nothing was inserted, so that token is the mistake.
                    | DiagnosticCode.MismatchedDelimiter(opened, _) -> Site.ofToken d.Token, openedHere opened
                    | _ ->
                        match d.TokenEnd with
                        | Some last -> Site.spanning [ d.Token; last ], []
                        | None -> Site.ofToken d.Token, []

                Diagnostic.create (Kind.Parse d.Code) site related
        ]

    /// The SHARED front-end parse chain (lex → `Reader.ofLexed` → `FSharpAst.parse`),
    /// the one home for every driver's parse: a bare-expression `ScriptFragment` wraps as
    /// an `AnonymousModule`, and lex/parse failures surface as `Diagnostic`s (never
    /// exceptions). WHICH driver ran is not a property of the failure, so nothing here is
    /// stamped with a caller-supplied code: the kind says what went wrong. The parser's own
    /// recovery diagnostics ride out on BOTH arms.
    let parse (source: string) : Result<ParsedUnit, ParseFailure> =
        match Lexing.lexString source with
        | Result.Error e ->
            Error
                {
                    Lexed = ValueNone
                    // A whole-file lex failure names no place in the file.
                    Diagnostics = [ Diagnostic.nowhere (Kind.LexFailure(sprintf "%A" e)) ]
                }
        | Result.Ok lexed ->
            let reader = Reader.ofLexed lexed source Set.empty

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

    /// `parse`, refusing a tree the parser had to PATCH. A recovered parse is not a
    /// compilable one — every inserted delimiter and every `Expr.Missing` is a hole the
    /// source did not fill — so the rule is a property of the product, stated once here
    /// rather than re-derived by each driver and each test harness.
    let parseUnrecovered (source: string) : Result<ParsedUnit, Diagnostic list> =
        match parse source with
        | Error f -> Error f.Diagnostics
        | Ok parsed ->
            match parsed.Diagnostics with
            | [] -> Ok parsed
            | recovered -> Error recovered

    /// Runs every pass through the `SemType` domain and returns the populated
    /// `PassContext` plus the **`SemType`** `TastFile` — the pre-freeze tree. This is
    /// the accessor for front-end consumers that assert on `SemType` shapes (tests,
    /// side-table inspection). `assemblyName` is the assembly this unit emits into
    /// (`PassContext.AssemblyName`); `""` for the front-end-only paths that never emit.
    let analyseSemWithContextForCore
        (selfHostList: bool)
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        let ctx = PassContext(provider, input, lexed)
        ctx.AssemblyName <- assemblyName
        // A self-host (BCL-only) package build has no FSharp.Core, so an unpinned
        // `[]`/`::` must default to the Vesper cons-list (`resolveListLiterals`).
        ctx.DefaultListIsVesper <- selfHostList
        Desugar.run ctx file
        NameResolution.run ctx file
        Unification.run ctx file
        Validation.run ctx file
        // `Elaborate.run` (renamed from `Freeze`): CST →
        // typar-quantified `TastFileG<SemType>`, inline call sites already expanded.
        let tast0 = Elaborate.run ctx file
        // Escape analysis on the post-inline `TExpr` tree: elaboration has already
        // expanded inline call sites, so the
        // region graph is built over the closures codegen actually emits. Populates
        // `ctx.Bindings.Escape` (keyed by binder `NodeKey`) for the next pass.
        Regions.run ctx tast0.Decls
        // Snapshot the closure stack/heap verdict (Axis 1 ∧ Axis 2) onto the
        // TastFile now that both escape side tables are populated — codegen has no
        // PassContext, so this is how the verdict reaches `discoverClosures`.
        let tast0 =
            { tast0 with
                // `tast0.Decls` is every module-level binding, `inline` ones included
                // (`TastFileG.Decls`), so their binders are in the snapshot's key space too.
                ClosureReprs = Regions.closureReprSnapshot ctx tast0.Decls
                // Snapshot the node-keyed value-struct closure
                // verdicts (decided in `inferApp`) onto the TastFile alongside
                // `ClosureReprs` — codegen has no PassContext, so this is how
                // `discoverClosures` / `ClosureVerdictRewrite` reach them.
                FunVerdicts =
                    ctx.FunVerdicts.AsDictionary()
                    |> Seq.map (fun kv -> kv.Key, kv.Value)
                    |> Map.ofSeq
            }
        // TAST→TAST promotion of `let mutable` cells captured by escaping closures.
        // Reads `ctx.Bindings.Escape` / `ctx.Bindings.Binding`;
        // running before ResolvedTypes keeps the validation sweep observing
        // post-promotion types.
        let tast1 = RefCellPromotion.run ctx tast0
        // The `TyVar`-leak guard runs LAST in the `SemType` domain, immediately
        // before the freeze, so a stray metavar surfaces as a graceful per-decl
        // diagnostic here rather than as a `toFrozen` hard error in `Freeze.run`.
        ResolvedTypes.run ctx tast1
        // Sibling type-invariant guard, same SemType domain / same diagnostic channel:
        // a primitive with no representation on the compiling target (the provider's
        // `Intrinsic(_, platform = None)`) is a type the back end cannot lower, so it
        // is surfaced here as a graceful per-decl diagnostic rather than a `failwith`
        // in a single backend's emitter. No-op on a target where every primitive has a
        // representation (CLR).
        PlatformTypes.run ctx tast1
        // Implicit `dynamic`-escape warnings: a `d?foo` whose `^TResult` was pinned to
        // a concrete type by context (the `default : dynamic` never fired) is an
        // unchecked assertion. Runs post-settle (the TypeVar graph is stable) over the
        // sites `inferDynamicLookup` recorded; needs no tree.
        DynamicEscape.run ctx
        // Snapshot ctx.Diagnostics again so ResolvedTypes findings are visible on
        // TastFile.Diagnostics.
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
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextForCore false assemblyName provider input lexed file

    /// The production entry: every pass **plus the final `SemType → FrozenType`
    /// freeze**. The SemanticAnalysis assembly's output is
    /// the frozen tree AS POOLS; codegen consumes them. `SemType` consumers use the
    /// `…Sem…` variants above.
    let analyseWithContextFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        let ctx, tast = analyseSemWithContextFor assemblyName provider input lexed file
        ctx, Freeze.run ctx tast

    /// `analyseSemWithContextFor` with no home assembly — the front-end-only entry
    /// (side-table inspection tests, contract scrapes). Local nominal keys mint
    /// with `asm = Some ""`, self-consistent within the one compilation.
    let analyseSemWithContext
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextFor "" provider input lexed file

    /// `analyseWithContextFor` with no home assembly.
    let analyseWithContext
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        analyseWithContextFor "" provider input lexed file

    /// The `SemType` (pre-freeze) production entry, discarding the `PassContext`.
    let analyseSemFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseSemWithContextFor assemblyName provider input lexed file
        tast

    /// The production entry: like `analyseWithContextFor` but discards the
    /// `PassContext`. `assemblyName` is the home assembly for local keys.
    let analyseFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        let _, tast = analyseWithContextFor assemblyName provider input lexed file
        tast

    let analyseSem
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        analyseSemFor "" provider input lexed file

    let analyse
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        analyseFor "" provider input lexed file

    /// The self-host **`SemType`** (pre-freeze) entry — like `analyseSem` but a
    /// bare-program list literal/pattern defaults to the Vesper cons-list, not
    /// FSharp.Core's `list`. The JS backend's front end always runs through this:
    /// the JS target has no FSharp.Core (and imports no Fable.Core), so the
    /// cons-list is the only list representation. Callers inspect `Diagnostics`
    /// before freezing.
    let analyseSemForSelfHost
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseSemWithContextForCore true "" provider input lexed file
        tast

    /// `analyseSemForSelfHost`, keeping the `PassContext`. A caller that inspects the
    /// `SemType` tree's diagnostics before freezing it needs both halves:
    /// `Freeze.run` reads the binder of each residual typar root out of
    /// `ctx.Bindings.Scheme`.
    let analyseSemForSelfHostWithContext
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextForCore true "" provider input lexed file

    /// The self-host production entry: like `analyseFor` but a bare-program list
    /// literal/pattern defaults to the Vesper cons-list, not FSharp.Core's `list`,
    /// so a BCL-only package (no FSharp.Core reference) emits `Vesper.List`-only.
    /// Used by the package build harness; the contract/codegen stack is otherwise
    /// identical.
    let analyseForSelfHost
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        let ctx, tast =
            analyseSemWithContextForCore true assemblyName provider input lexed file

        Freeze.run ctx tast
