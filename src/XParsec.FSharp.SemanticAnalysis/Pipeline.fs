namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// The pass chain over an already-parsed implementation file.
module Pipeline =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// Runs every pass through the `SemType` domain, returning the populated `PassContext`,
    /// what the region pass decided, and the pre-freeze `TastFile`. `assembly` is the
    /// assembly this file emits into and the compiling target; `CompilingAssembly.none` for
    /// the front-end-only paths that never emit.
    let analyseSemWithContextForCore
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * RegionVerdicts * TastFile =
        let ctx = PassContext(provider, file, assembly)
        Desugar.run ctx impl
        NameResolution.run ctx impl
        Unification.run ctx impl
        Validation.run ctx impl
        // Elaboration lowers the CST to a typar-quantified TAST with every inline call site
        // already expanded, so escape analysis below sees the closures codegen emits.
        let tast0 = Elaborate.run ctx impl
        let regions = Regions.run ctx tast0.Decls tast0.Specializations
        // Codegen has no `PassContext`, so the closure verdicts ride the TastFile.
        let tast0 =
            { tast0 with
                ClosureReprs = regions.ClosureReprs
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

        ctx, regions, tast

    /// Every pass, for a caller with no use for the region verdicts.
    let analyseSemWithContextFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        let ctx, _, tast = analyseSemWithContextForCore assembly provider file impl

        ctx, tast

    /// Every pass plus the final `SemType → FrozenType` freeze: the frozen tree AS POOLS,
    /// which is what codegen consumes. `SemType` consumers use the `…Sem…` variants above.
    let analyseWithContextFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        let ctx, tast = analyseSemWithContextFor assembly provider file impl
        ctx, Freeze.run ctx tast

    /// `analyseSemWithContextFor` with no home assembly, for the front-end-only entries
    /// (side-table inspection tests, contract scrapes).
    let analyseSemWithContext
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * TastFile =
        analyseSemWithContextFor CompilingAssembly.none provider file impl

    /// `analyseSemWithContext` keeping what the region pass returned. The escape axis lands on
    /// the context, but the representation axis has no side table to read it off — it is
    /// `Regions.run`'s return value, and this is its only route out of the pipeline.
    let analyseSemWithRegions
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * RegionVerdicts * TastFile =
        analyseSemWithContextForCore CompilingAssembly.none provider file impl

    /// `analyseWithContextFor` with no home assembly.
    let analyseWithContext
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        analyseWithContextFor CompilingAssembly.none provider file impl

    /// The `SemType` (pre-freeze) production entry, discarding the `PassContext`.
    let analyseSemFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseSemWithContextFor assembly provider file impl
        tast

    /// The production entry. `assembly` carries the home assembly for local keys and the
    /// compiling target.
    let analyseFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        let _, tast = analyseWithContextFor assembly provider file impl
        tast

    let analyseSem
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : TastFile =
        analyseSemFor CompilingAssembly.none provider file impl

    let analyse
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        analyseFor CompilingAssembly.none provider file impl
