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

    let private assemble
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tasts: FrozenPools list)
        : ClrArtifact =
        // A compilation is an ordered SEQUENCE of frozen files emitted into one assembly;
        // `Layout.buildMany` combines them and the Bind/Prepare loops below iterate every
        // file. A single-file compile is the length-1 case — byte-identical to before.
        let asm = Assembler(symbols, project, tasts, bclReferences)

        // Bind, per file: pre-fill the registries with layout-derived handles, so any
        // prepared body can reference any type / member / factory / static fn / closure
        // ctor with no emission-order discipline. Each file binds against its own
        // `EmitContext` (NodeKey-keyed tables are file-local); the nominal registries and
        // the combined row space are shared on the Assembler.
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

        // Prepare, per file: build every signature + body against the resolved handles,
        // using that file's `EmitContext`. `PrepareMain` gates itself on the file that
        // carries the entry point, so it fires for the entry file alone.
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

    /// A SEQUENCE of frozen files → one in-memory PE artifact. The `symbols` provider
    /// must already carry every cross-file surface the files reference — the caller
    /// composes `composite(each file's projected view ++ external)` so a call into a
    /// prior file's exported module function finds its open signature (which
    /// `ClrRecipes.emitExternalCall` then re-homes to the LOCAL `MethodDef` via
    /// `localModuleFns`); types / records / unions resolve through the shared nominal
    /// registries the Bind pass fills. Only the LAST file may carry top-level
    /// expressions, so it alone owns `Main` + the Program holder + the entry point.
    ///
    /// This is the general entry; `compile` is the length-1 case. Codegen stays agnostic
    /// of the front-end `AssemblyFiles.FrozenFile`: the caller owns view-composition and
    /// hands over the already-composed provider + the bare `FrozenPools` list.
    let compileFiles (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tasts: FrozenPools list) : ClrArtifact =
        assemble [] symbols project tasts

    /// `compileFiles` with the compilation's own BCL surface threaded into the emitted-
    /// `AssemblyRef` identity map (see `compileWithBclReferences`). `compileFiles` is
    /// this with `[]`.
    let compileFilesWithBclReferences
        (bclReferences: string list)
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (tasts: FrozenPools list)
        : ClrArtifact =
        assemble bclReferences symbols project tasts

    /// TAST + symbol context → in-memory PE artifact. `ProjectInfo.OutputKind`
    /// decides (via the layout) whether `Main` + the "Program" holder exist
    /// and whether the PE serialises with an entry point.
    ///
    /// Cross-package `val inline` bodies are not threaded here: they are spliced
    /// pre-freeze by `Passes.InlineExpansion`, reaching the front end on the resolved
    /// entries of the same `symbols` provider, so codegen takes no inline-body map. The
    /// published TEMPLATES are a separate root array (`FrozenPools.InlineTemplates`) that
    /// emission never walks; an `inline` binding's ordinary compiled function is in
    /// `Roots` like any other and IS emitted.
    /// The single-file case of `compileFiles`.
    let compile (symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: FrozenPools) : ClrArtifact =
        compileFiles symbols project [ tast ]

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
        (tast: FrozenPools)
        : ClrArtifact =
        compileFilesWithBclReferences bclReferences symbols project [ tast ]

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
