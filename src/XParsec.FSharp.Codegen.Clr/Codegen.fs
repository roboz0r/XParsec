namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

// Entry points: the `compile` / `materialise` pair.
// `compile` is deterministic given the same inputs; `materialise` (re-exported
// from `Materialise`) is the only side effect. `Layout.build` enumerates every
// ranged-table row as data; the Bind phase pre-fills the registries from the
// layout; the Prepare phase builds every signature/body against resolved
// handles; the write/finalise tail walks the layout mechanically. `assemble`
// sequences the phases — no ordering exists for a caller to get wrong.

module Codegen =

    let private classInput (cd: ClassDecl) : NominalEmissionInput =
        NominalEmissionInput.Class(
            cd.Fields,
            cd.CtorParams,
            cd.BaseType,
            cd.IsSealed,
            cd.StaticLets,
            cd.SecondaryCtors,
            cd.BaseCtorCall,
            cd.Interfaces,
            cd.ValueKind <> ClassValueKind.RefType,
            cd.HasPrimaryCtor
        )

    let private assemble
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: Frozen.TastFile)
        : ClrArtifact =
        let asm = Assembler(symbols, project, tast, bclReferences)

        // Bind: pre-fill the registries with layout-derived handles, so any
        // prepared body can reference any type / member / factory / static fn
        // / closure ctor with no emission-order discipline.
        for ud in asm.UnionDecls do
            NominalEmit.register asm (NominalEmissionInput.Union(ud.Cases, ud.Interfaces)) ud.Decl ud.Members

        for rd in asm.RecordDecls do
            NominalEmit.register asm (NominalEmissionInput.Record(rd.Fields, rd.Interfaces)) rd.Decl rd.Members

        for cd in asm.ClassDecls do
            NominalEmit.register asm (classInput cd) cd.Decl cd.Members

        asm.BindClosures()

        // Prepare: build every signature + body against the resolved handles.
        asm.PrepareInterfaces()

        for ud in asm.UnionDecls do
            NominalEmit.prepare asm (NominalEmissionInput.Union(ud.Cases, ud.Interfaces)) ud.Decl ud.Members

        for rd in asm.RecordDecls do
            NominalEmit.prepare asm (NominalEmissionInput.Record(rd.Fields, rd.Interfaces)) rd.Decl rd.Members

        for cd in asm.ClassDecls do
            NominalEmit.prepare asm (classInput cd) cd.Decl cd.Members

        asm.PrepareStructEnums()
        asm.PrepareClosures()
        asm.PrepareStaticMethods()
        asm.PrepareMain()

        // Write the MethodDef table in layout order, then the TypeDef rows +
        // sorted GenericParams, and serialise.
        asm.WriteMethods()
        asm.Finalise()

    /// TAST + symbol context → in-memory PE artifact. `ProjectInfo.OutputKind`
    /// decides (via the layout) whether `Main` + the "Program" holder exist
    /// and whether the PE serialises with an entry point.
    ///
    /// Cross-package `val inline` bodies (milestone M) are no longer threaded here:
    /// they are spliced pre-freeze by `Passes.InlineExpansion`, reaching the front
    /// end through the `IInlineBodyProvider` channel of
    /// the same `symbols` provider, so codegen takes no separate inline-body map.
    let compile (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: Frozen.TastFile) : ClrArtifact =
        assemble [] symbols project tast

    /// `compile` with the compilation's own BCL surface (a TFM ref pack +
    /// `<Reference>`s) threaded into the emitted-`AssemblyRef` identity map, so the
    /// bootstrap `System.Runtime` / `System.Console` refs bind the reference set
    /// rather than the host's `System.Private.CoreLib`. `bclReferences` feeds
    /// identity ONLY — it is never `ProjectInfo.References`, so `materialiseApp` does
    /// not ship a (body-less) reference assembly. `compile` is this with `[]`.
    let compileWithBclReferences
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: Frozen.TastFile)
        : ClrArtifact =
        assemble bclReferences symbols project tast

    /// Assemble a hand-written `Main` body that drives the untyped `Il` surface
    /// directly — the testable seam for hand-written bodies, independent of any
    /// TAST.
    let assembleMainEmit (symbols: IExternalSymbolProvider) (project: ProjectInfo) (build: Il -> unit) : ClrArtifact =
        AssemblerScaffold.assembleWith symbols project (fun _ _ -> build)

    /// Provider-aware variant: the build callback sees the wired
    /// `ICodegenProvider` so the test seam can reference BCL primitives without
    /// setting up a TAST.
    let assembleMainEmitWithProvider
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (build: ICodegenProvider -> Il -> unit)
        : ClrArtifact =
        AssemblerScaffold.assembleWith symbols project (fun _ provider il -> build (provider :> ICodegenProvider) il)

    /// The serialised PE bytes.
    let toBytes (artifact: ClrArtifact) : byte[] = Materialise.toBytes artifact

    /// The only side effect: write the PE to `OutputPath` when one is set.
    let materialise (artifact: ClrArtifact) : unit = Materialise.materialise artifact

    /// Materialise a *runnable* framework-dependent app (PE + `runtimeconfig.json`
    /// + the referenced assemblies the shared framework does not carry).
    let materialiseApp (project: ProjectInfo) (artifact: ClrArtifact) : unit =
        Materialise.materialiseApp project artifact
