namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

module Pipeline =

    /// Runs every pass and returns both the populated `PassContext` and
    /// the frozen `TastFile`. Tests that need to inspect side tables (e.g.
    /// `ctx.Bindings.Escape`) call this; `analyse` is the production entry point
    /// that discards `ctx`. `assemblyName` is the home assembly stamped onto
    /// locally-minted nominal keys (`PassContext.AssemblyName`); `""` for the front-end-only paths that never emit.
    let analyseWithContextFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        let ctx = PassContext(provider, input, lexed)
        ctx.AssemblyName <- assemblyName
        Desugar.run ctx file
        NameResolution.run ctx file
        Unification.run ctx file
        Validation.run ctx file
        let tast0 = Freeze.run ctx file
        // Escape analysis runs on the post-inline `TExpr` tree (frozen-type-plan
        // 3A-2 / decision 4): `Freeze.run` has already expanded inline call sites,
        // so the region graph is built over the closures codegen actually emits —
        // inlined-away closures don't count, inline-exposed ones do. It populates
        // `ctx.Bindings.Escape` (keyed by binder `NodeKey`) for the next pass.
        Regions.run ctx tast0.Decls
        // TAST→TAST promotion of `let mutable` cells captured by escaping
        // closures (records-plan §B7). The pass reads `ctx.Bindings.Escape` /
        // `ctx.Bindings.Binding`; running before ResolvedTypes keeps the validation
        // sweep observing post-promotion types.
        let tast1 = RefCellPromotion.run ctx tast0
        ResolvedTypes.run ctx tast1
        // Snapshot ctx.Diagnostics again so ResolvedTypes findings are
        // visible on TastFile.Diagnostics. Freeze took its snapshot before
        // we ran.
        let tast =
            { tast1 with
                Diagnostics = List.ofSeq ctx.Diagnostics
            }

        ctx, tast

    /// `analyseWithContextFor` with no home assembly — the front-end-only entry
    /// (side-table inspection tests, contract scrapes). Local nominal keys mint
    /// with `asm = Some ""`, self-consistent within the one compilation.
    let analyseWithContext
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseWithContextFor "" provider input lexed file

    /// The production entry point: like `analyseWithContextFor` but discards the
    /// `PassContext`. `assemblyName` is the home assembly for local keys.
    let analyseFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseWithContextFor assemblyName provider input lexed file
        tast

    let analyse
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        analyseFor "" provider input lexed file
