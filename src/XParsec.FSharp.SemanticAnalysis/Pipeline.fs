namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// The pass chain over an already-parsed implementation file.
module Pipeline =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// Runs every pass through the `SemType` domain, returning the populated `PassContext`,
    /// what the region pass decided, and the pre-freeze `TastFile`. `assembly` is the
    /// assembly this file emits into and the compiling target.
    let analyseSemWithContextForCore
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * RegionVerdicts * TastFile =
        let ctx = PassContext(provider, file, assembly)
        NameResolution.run ctx impl
        Unification.run ctx impl
        Validation.run ctx impl
        // Elaboration lowers the CST to a typar-quantified TAST with every inline call site
        // already expanded, so escape analysis below sees the closures codegen emits.
        let tast0 = Elaborate.run ctx impl
        Attributes.run ctx
        let regions = Regions.run ctx tast0.Decls tast0.Specializations
        // Codegen has no `PassContext`, so the closure verdicts are carried on the TastFile.
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
    ///
    /// A file whose analysis reported an error freezes what elaboration produced, so the surface
    /// it publishes still carries the declarations that did resolve and a later file of the same
    /// assembly reports only its own faults.
    let analyseWithContextFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : PassContext * FrozenPools =
        let ctx, tast = analyseSemWithContextFor assembly provider file impl
        ctx, Freeze.run ctx tast

    /// The `SemType` (pre-freeze) production entry, discarding the `PassContext`.
    let analyseSemFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : TastFile =
        let _, tast = analyseSemWithContextFor assembly provider file impl
        tast

    /// The `AnalyseFile` production entry. `assembly` carries the home assembly for local
    /// keys and the compiling target.
    let analyseFileFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : FrozenPools * ImportObligation list =
        let ctx, pools = analyseWithContextFor assembly provider file impl
        pools, List.ofSeq ctx.Bindings.Imports

    /// `analyseFileFor`, keeping the pools alone.
    let analyseFor
        (assembly: CompilingAssembly)
        (provider: IExternalSymbolProvider)
        (file: LexedFile)
        (impl: ImplementationFile<SyntaxToken>)
        : FrozenPools =
        fst (analyseFileFor assembly provider file impl)
