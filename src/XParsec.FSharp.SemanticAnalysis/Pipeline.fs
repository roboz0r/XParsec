namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

module Pipeline =

    /// Runs every pass and returns both the populated `PassContext` and
    /// the frozen `TastFile`. Tests that need to inspect side tables (e.g.
    /// `ctx.Escape`) call this; `analyse` is the production entry point
    /// that discards `ctx`.
    let analyseWithContext
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        let ctx = PassContext(provider, input, lexed)
        Desugar.run ctx file
        NameResolution.run ctx file
        Unification.run ctx file
        Regions.run ctx file
        Validation.run ctx file
        let tast0 = Freeze.run ctx file
        // TAST→TAST promotion of `let mutable` cells captured by escaping
        // closures (records-plan §B7). The pass reads `ctx.Escape` /
        // `ctx.Binding`; running before ResolvedTypes keeps the validation
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

    let analyse
        (provider: IExternalSymbolProvider)
        (input: string)
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseWithContext provider input lexed file
        tast
