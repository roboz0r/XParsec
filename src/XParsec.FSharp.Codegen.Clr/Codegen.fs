namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

// Entry points: the `compile` / `materialise` pair from
// [backend-design-plan](../XParsec.FSharp.SemanticAnalysis/docs/backend-design-plan.md).
// `compile` is deterministic given the same inputs; `materialise` (re-exported
// from `Materialise`) is the only side effect. The emission machinery lives in
// `Assembler` (the converged spine) + `NominalEmit` (the per-type bodies);
// `assemble` here only sequences the phases.

module Codegen =

    let private assemble
        (externalInlines: Map<string, TDecl>)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: TastFile)
        (emitEntryPoint: bool)
        : ClrArtifact =
        let asm = Assembler(externalInlines, symbols, project, tast)

        asm.EmitInterfaces()

        for (td, cases, members) in asm.UnionDecls do
            NominalEmit.emit asm (NominalEmissionInput.Union cases) td members asm.UnionTypes

        for (td, fields, members) in asm.RecordDecls do
            NominalEmit.emit asm (NominalEmissionInput.Record fields) td members asm.RecordTypes

        for (td, fields, ctorParams, members, baseType, isSealed, staticLets, secondaryCtors, baseCtorCall) in
            asm.ClassDecls do
            NominalEmit.emit
                asm
                (NominalEmissionInput.Class(
                    fields,
                    ctorParams,
                    baseType,
                    isSealed,
                    staticLets,
                    secondaryCtors,
                    baseCtorCall
                ))
                td
                members
                asm.ClassTypes

        asm.EmitClosures()
        asm.EmitStaticMethods()
        let mainDef = asm.EmitMain emitEntryPoint
        asm.Finalise(mainDef, emitEntryPoint)

    /// `compile` plus the cross-package inline bodies (milestone M): a referenced
    /// package's `val inline` whose `.fs` body is *spliced* at each use site
    /// rather than called as a compiled member. `compile` passes an empty map.
    let compileWithInlines
        (externalInlines: Map<string, TDecl>)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: TastFile)
        : ClrArtifact =
        match project.OutputKind with
        | Library -> assemble externalInlines symbols project tast false
        | Exe -> assemble externalInlines symbols project tast true

    /// TAST + symbol context → in-memory PE artifact. `ProjectInfo.OutputKind`
    /// routes to the executable (`Main` + `Program`) or library tail of the one
    /// converged assembler.
    let compile (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: TastFile) : ClrArtifact =
        compileWithInlines Map.empty symbols project tast

    /// Assemble a hand-written `Main` body that drives the untyped `Il` surface
    /// directly — the testable seam for hand-written bodies, independent of any
    /// TAST.
    let assembleMainEmit (project: ProjectInfo) (build: Il -> unit) : ClrArtifact =
        AssemblerScaffold.assembleWith project (fun _ _ -> build)

    /// Provider-aware variant: the build callback sees the wired
    /// `ICodegenProvider` so the test seam can reference BCL primitives without
    /// setting up a TAST.
    let assembleMainEmitWithProvider (project: ProjectInfo) (build: ICodegenProvider -> Il -> unit) : ClrArtifact =
        AssemblerScaffold.assembleWith project (fun _ provider il -> build (provider :> ICodegenProvider) il)

    /// The serialised PE bytes.
    let toBytes (artifact: ClrArtifact) : byte[] = Materialise.toBytes artifact

    /// The only side effect: write the PE to `OutputPath` when one is set.
    let materialise (artifact: ClrArtifact) : unit = Materialise.materialise artifact

    /// Materialise a *runnable* framework-dependent app (PE + `runtimeconfig.json`
    /// + the referenced assemblies the shared framework does not carry).
    let materialiseApp (project: ProjectInfo) (artifact: ClrArtifact) : unit =
        Materialise.materialiseApp project artifact
