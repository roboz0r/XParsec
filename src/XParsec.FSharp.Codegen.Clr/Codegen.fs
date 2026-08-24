namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

module Codegen =

    let private assemble
        (referenceAssemblies: string list)
        (symbols: ICodegenSymbols)
        (project: ProjectInfo)
        (tasts: FrozenPools list)
        : ClrArtifact =
        let asm = Assembler(symbols, project, tasts, referenceAssemblies)

        // Bind, per file: pre-fill the registries with layout-derived handles, so a prepared
        // body can reference any type / member / factory / closure ctor whatever the emission
        // order. `EmitContext` is file-local; the nominal registries are shared.
        for f in asm.Files do
            for ud in f.Layout.Partitioned.Unions do
                NominalEmit.register asm (NominalEmissionInput.Union(ud.Cases, ud.Interfaces)) ud.Decl ud.Members

            for rd in f.Layout.Partitioned.Records do
                NominalEmit.register
                    asm
                    (NominalEmissionInput.Record(rd.Fields, rd.Interfaces, rd.ValueKind <> RecordValueKind.RefType))
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
                    (NominalEmissionInput.Record(rd.Fields, rd.Interfaces, rd.ValueKind <> RecordValueKind.RefType))
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

    /// A gated assembly → one in-memory PE artifact, so a cross-file reference is re-homed to
    /// a local `MethodDef`. `referenceAssemblies` supplies the emitted `AssemblyRef` identities.
    /// Only the LAST file may carry top-level expressions, so it alone owns `Main`.
    let emitAssembly
        (referenceAssemblies: string list)
        (project: ProjectInfo)
        (assembly: EmittableAssembly)
        : ClrArtifact =
        assemble
            referenceAssemblies
            (CodegenSymbols.ofProvider assembly.Visibility)
            project
            [ for f in assembly.Files -> f.Frozen ]

    /// The test seam for a body written with no TAST: assembles a hand-written `Main` that
    /// drives the untyped `Il` surface directly.
    let assembleMainEmit (symbols: ICodegenSymbols) (project: ProjectInfo) (build: Il -> unit) : ClrArtifact =
        AssemblerScaffold.assembleWith symbols project (fun _ _ -> build)

    /// Provider-aware variant: the build callback sees the wired `ICodegenProvider`, so the
    /// test seam can reference BCL primitives.
    let assembleMainEmitWithProvider
        (symbols: ICodegenSymbols)
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
    let materialiseApp (artifact: ClrArtifact) : unit = Materialise.materialiseApp artifact
