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
        (tast: Frozen.TastFile)
        : ClrArtifact =
        // One unit today: the Assembler takes a unit LIST (the N-unit mechanism), driven
        // here with a singleton so single-file emission stays byte-identical.
        let asm = Assembler(symbols, project, [ tast ], bclReferences)

        // Bind, per unit: pre-fill the registries with layout-derived handles, so any
        // prepared body can reference any type / member / factory / static fn / closure
        // ctor with no emission-order discipline. Each unit binds against its own
        // `EmitContext` (NodeKey-keyed tables are file-local); the nominal registries and
        // the combined row space are shared on the Assembler.
        for u in asm.Units do
            for ud in u.Layout.Partitioned.Unions do
                NominalEmit.register asm (NominalEmissionInput.Union(ud.Cases, ud.Interfaces)) ud.Decl ud.Members

            for rd in u.Layout.Partitioned.Records do
                NominalEmit.register
                    asm
                    (NominalEmissionInput.Record(rd.Fields, rd.Interfaces, rd.ValueKind <> ClassValueKind.RefType))
                    rd.Decl
                    rd.Members

            for cd in u.Layout.Partitioned.Classes do
                NominalEmit.register asm (NominalEmissionInput.Class cd) cd.Decl cd.Members

            asm.BindClosures u

        // Prepare, per unit: build every signature + body against the resolved handles,
        // using that unit's `EmitContext`. `PrepareMain` gates itself on the unit that
        // carries the entry point, so it fires for the entry unit alone.
        for u in asm.Units do
            asm.PrepareInterfaces u

            for ud in u.Layout.Partitioned.Unions do
                NominalEmit.prepare asm u.EmitCtx (NominalEmissionInput.Union(ud.Cases, ud.Interfaces)) ud.Decl ud.Members

            for rd in u.Layout.Partitioned.Records do
                NominalEmit.prepare
                    asm
                    u.EmitCtx
                    (NominalEmissionInput.Record(rd.Fields, rd.Interfaces, rd.ValueKind <> ClassValueKind.RefType))
                    rd.Decl
                    rd.Members

            for cd in u.Layout.Partitioned.Classes do
                NominalEmit.prepare asm u.EmitCtx (NominalEmissionInput.Class cd) cd.Decl cd.Members

            asm.PrepareStructEnums u
            asm.PrepareClosures u
            asm.PrepareStaticMethods u
            asm.PrepareMain u

        // Write the MethodDef table in layout order, then the TypeDef rows +
        // sorted GenericParams, and serialise.
        asm.WriteMethods()
        asm.Finalise()

    /// TAST + symbol context → in-memory PE artifact. `ProjectInfo.OutputKind`
    /// decides (via the layout) whether `Main` + the "Program" holder exist
    /// and whether the PE serialises with an entry point.
    ///
    /// Cross-package `val inline` bodies are not threaded here: they are spliced
    /// pre-freeze by `Passes.InlineExpansion`, reaching the front end on the resolved
    /// entries of the same `symbols` provider, so codegen takes no inline-body map — and
    /// no inline template ever reaches it (`Freeze` keeps them out of `Decls`).
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
