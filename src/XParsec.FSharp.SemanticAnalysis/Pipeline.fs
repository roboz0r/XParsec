namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

module Pipeline =

    /// Runs every pass through the `SemType` domain and returns the populated
    /// `PassContext` plus the **`SemType`** `TastFile` — the pre-freeze tree. This is
    /// the accessor for front-end consumers that assert on `SemType` shapes (tests,
    /// side-table inspection). `assemblyName` is the home assembly stamped onto
    /// locally-minted nominal keys (`PassContext.AssemblyName`); `""` for the
    /// front-end-only paths that never emit.
    let analyseSemWithContextFor
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
        // `Elaborate.run` (renamed from `Freeze`): CST →
        // typar-quantified `TastFileG<SemType>`, inline call sites already expanded.
        let tast0 = Elaborate.run ctx file
        // Escape analysis on the post-inline `TExpr` tree: elaboration has already
        // expanded inline call sites, so the
        // region graph is built over the closures codegen actually emits. Populates
        // `ctx.Bindings.Escape` (keyed by binder `NodeKey`) for the next pass.
        Regions.run ctx tast0.Decls
        // TAST→TAST promotion of `let mutable` cells captured by escaping closures.
        // Reads `ctx.Bindings.Escape` / `ctx.Bindings.Binding`;
        // running before ResolvedTypes keeps the validation sweep observing
        // post-promotion types.
        let tast1 = RefCellPromotion.run ctx tast0
        // The `TyVar`-leak guard runs LAST in the `SemType` domain, immediately
        // before the freeze, so a stray metavar surfaces as a graceful per-decl
        // diagnostic here rather than as a `toFrozen` hard error in `Freeze.run`.
        ResolvedTypes.run ctx tast1
        // Snapshot ctx.Diagnostics again so ResolvedTypes findings are visible on
        // TastFile.Diagnostics.
        let tast =
            { tast1 with
                Diagnostics = List.ofSeq ctx.Diagnostics
            }

        ctx, tast

    /// The production entry: every pass **plus the final `SemType → FrozenType`
    /// freeze**. The SemanticAnalysis assembly's output is
    /// the frozen tree; codegen consumes it. `SemType` consumers use the `…Sem…`
    /// variants above.
    let analyseWithContextFor
        (assemblyName: string)
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * Frozen.TastFile =
        let ctx, tast = analyseSemWithContextFor assemblyName provider input lexed file
        ctx, Freeze.run tast

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
        : PassContext * Frozen.TastFile =
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
        : Frozen.TastFile =
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
        : Frozen.TastFile =
        analyseFor "" provider input lexed file
