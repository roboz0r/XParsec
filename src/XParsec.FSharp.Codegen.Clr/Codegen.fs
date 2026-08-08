namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

module Codegen =

    let private assemble
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tasts: FrozenPools list)
        : ClrArtifact =
        let asm = Assembler(symbols, project, tasts, bclReferences)

        // Bind, per file: pre-fill the registries with layout-derived handles, so a prepared
        // body can reference any type / member / factory / closure ctor whatever the emission
        // order. `EmitContext` is file-local; the nominal registries are shared.
        for f in asm.Files do
            for ud in f.Layout.Partitioned.Unions do
                NominalEmit.register asm (NominalEmissionInput.Union(ud.Cases, ud.Interfaces)) ud.Decl ud.Members

            for rd in f.Layout.Partitioned.Records do
                NominalEmit.register
                    asm
                    (NominalEmissionInput.Record(rd.Fields, rd.Interfaces, rd.ValueKind <> ClassValueKind.RefType))
                    rd.Decl
                    rd.Members

            for cd in f.Layout.Partitioned.Classes do
                NominalEmit.register asm (NominalEmissionInput.Class cd) cd.Decl cd.Members

            asm.BindClosures f

        // Prepare, per file: build every signature + body against the resolved handles.
        // `PrepareMain` fires only for the file the layout gave the entry point.
        for f in asm.Files do
            asm.PrepareInterfaces f

            for ud in f.Layout.Partitioned.Unions do
                NominalEmit.prepare
                    asm
                    f.EmitCtx
                    (NominalEmissionInput.Union(ud.Cases, ud.Interfaces))
                    ud.Decl
                    ud.Members

            for rd in f.Layout.Partitioned.Records do
                NominalEmit.prepare
                    asm
                    f.EmitCtx
                    (NominalEmissionInput.Record(rd.Fields, rd.Interfaces, rd.ValueKind <> ClassValueKind.RefType))
                    rd.Decl
                    rd.Members

            for cd in f.Layout.Partitioned.Classes do
                NominalEmit.prepare asm f.EmitCtx (NominalEmissionInput.Class cd) cd.Decl cd.Members

            asm.PrepareStructEnums f
            asm.PrepareClosures f
            asm.PrepareStaticMethods f
            asm.PrepareMain f

        // Write the MethodDef table in layout order, then the TypeDef rows +
        // sorted GenericParams, and serialise.
        asm.WriteMethods()
        asm.Finalise()

    /// A SEQUENCE of frozen files → one in-memory PE artifact. `symbols` must already carry
    /// every cross-file surface the files reference; the caller composes that view. Only the
    /// LAST file may carry top-level expressions, so it alone owns `Main` and the entry point.
    let compileFiles (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tasts: FrozenPools list) : ClrArtifact =
        assemble [] symbols project tasts

    /// `compileFiles` with the compilation's own BCL surface threaded into the emitted
    /// `AssemblyRef` identity map. `compileFiles` is this with `[]`.
    let compileFilesWithBclReferences
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tasts: FrozenPools list)
        : ClrArtifact =
        assemble bclReferences symbols project tasts

    /// TAST + symbol context → in-memory PE artifact; the single-file case of `compileFiles`.
    /// `ProjectInfo.OutputKind` decides (via the layout) whether `Main` + the "Program"
    /// class exist and whether the PE serialises with an entry point.
    let compile (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: FrozenPools) : ClrArtifact =
        compileFiles symbols project [ tast ]

    /// `compile` with the compilation's own BCL surface (a TFM ref pack + `<Reference>`s) in
    /// the emitted-`AssemblyRef` identity map, so `System.Runtime` / `System.Console` bind the
    /// reference set, not the host's `System.Private.CoreLib`. Identity only, never shipped.
    let compileWithBclReferences
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tast: FrozenPools)
        : ClrArtifact =
        compileFilesWithBclReferences bclReferences symbols project [ tast ]

    /// Assemble a hand-written `Main` body that drives the untyped `Il` surface directly —
    /// the test seam for a body written with no TAST.
    let assembleMainEmit (symbols: IExternalSymbolProvider) (project: ProjectInfo) (build: Il -> unit) : ClrArtifact =
        AssemblerScaffold.assembleWith symbols project (fun _ _ -> build)

    /// Provider-aware variant: the build callback sees the wired `ICodegenProvider`, so the
    /// test seam can reference BCL primitives.
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
