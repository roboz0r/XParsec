namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// A single CLR compilation's inputs, MSBuild-shaped. `BclReferences` is a DRIVER-level input
/// by design and must NOT accrete onto `ProjectInfo`; `SelfManifest` is the package this
/// compilation IS — a referenced package is RESOLVED against, the self package DEFINED.
type ClrCompilation =
    {
        Project: ProjectInfo
        Manifests: string list
        BclReferences: string list
        SelfManifest: string option
    }

module ClrCompilation =

    /// References packages, defines no primitives of its own — the shape to reach for unless
    /// compiling a package that declares `extern` types.
    let consumer (project: ProjectInfo) (manifests: string list) (bclReferences: string list) : ClrCompilation =
        {
            Project = project
            Manifests = manifests
            BclReferences = bclReferences
            SelfManifest = None
        }

/// The production CLR driver: parse → analyse → gate → emit against an EXPLICIT reference
/// set, returning front-end errors rather than throwing.
module ClrDriver =

    // Front-end failures are user errors and surface as `Diagnostic`s. Codegen exceptions
    // are NOT caught: a malformed emission is a compiler bug and must fail loudly.
    let private driverDiagnostic (message: string) : Diagnostic =
        Diagnostic.nowhere (Kind.Driver message)

    /// The ANALYSIS diagnostics that block emission. One definition, because `compile` and
    /// `compileCached` claim to gate identically and two copies is how that stops being true.
    let private blockingErrors (tast: FrozenPools) : Diagnostic list =
        Diagnostic.errors tast.Residue.Diagnostics

    /// ONE binding for the provider build and for the cache digest, which are computed in
    /// different functions: a digest naming a different target would alias two targets' trees.
    let private clrTarget: string = Target.Clr

    /// Compile `source` to an in-memory PE against the compilation's own reference set. A
    /// driver program is a package CONSUMER, so it runs the default (non-self-host) front end.
    let compile (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match Pipeline.parseUnrecovered source with
        | Error diagnostics -> Error diagnostics
        | Ok parsed ->
            let provider =
                ClrSymbolProviders.buildContractWithRefs
                    inputs.SelfManifest
                    inputs.BclReferences
                    clrTarget
                    inputs.Manifests

            let tast =
                Pipeline.analyseFor
                    inputs.Project.AssemblyName
                    provider
                    (Hashing.originSourceOfText parsed.Lexed)
                    parsed.File

            match blockingErrors tast with
            | [] -> Ok(Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project tast)
            | errors -> Error errors

    /// Everything a cached front end depends on EXCEPT one file's text. Fold it once per
    /// compilation: reading the reference closure costs what the front end a hit elides does.
    let compilationDigest (inputs: ClrCompilation) : Hashing.CompilationDigest =
        Hashing.compilationDigest
            {
                HomeAssembly = inputs.Project.AssemblyName
                Target = clrTarget
                ReferenceAssemblies = inputs.BclReferences
                Manifests = inputs.Manifests
                SelfManifest = inputs.SelfManifest
            }

    /// `compile`, routing the per-file front end through the frozen-compile cache against an
    /// already-folded `digest`. A HIT skips parse + analyse + freeze; an errored front end is
    /// returned as `Error` and NOT stored.
    ///
    /// The provider is built OUTSIDE the cache, on BOTH paths, because codegen consumes it
    /// even on a hit — the cache elides only the front end, never emission. That is sound
    /// only because the key covers every input the provider is built from as well as the
    /// source: `compilationDigest` fills `Hashing.CompilationInputs` from exactly the four
    /// arguments handed to `buildContractWithRefs`, plus the home assembly the minted keys
    /// are rooted at. A digest folded from OTHER inputs than `inputs` is the one way to
    /// misuse this, and is why `compilationDigest` takes the same `ClrCompilation` this does.
    let compileCachedWith
        (store: ICacheStore)
        (digest: Hashing.CompilationDigest)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        let provider =
            ClrSymbolProviders.buildContractWithRefs inputs.SelfManifest inputs.BclReferences clrTarget inputs.Manifests

        // The key covers the path the frozen tree's nodes name, so a hit cannot serve a tree
        // anchored elsewhere.
        let path = Hashing.textOriginPath source

        let key =
            {
                Query = QueryId.Freeze
                CodeVersion = Cache.CodeVersion
                Input = Hashing.fileInputHash path source digest
            }

        FrozenCache.freezeResult
            store
            key
            (fun () ->
                match Pipeline.parseUnrecovered source with
                | Error diagnostics -> Error diagnostics
                | Ok parsed ->
                    let tast =
                        Pipeline.analyseFor
                            inputs.Project.AssemblyName
                            provider
                            (Hashing.originSource path parsed.Lexed)
                            parsed.File

                    match blockingErrors tast with
                    | [] -> Ok tast
                    | errors -> Error errors
            )
        |> Result.map (fun frozen ->
            Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project frozen
        )

    /// `compileCachedWith` for a ONE-FILE compilation, folding the digest inline. Several
    /// files through this would re-read the whole dependency closure per file.
    let compileCached
        (store: ICacheStore)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        compileCachedWith store (compilationDigest inputs) inputs source

    /// THE shared multi-file glue seam: an ordered `(path, source)` list analysed as one
    /// assembly, emitted as ONE PE — so emission composes ALL the files' views at once and a
    /// cross-file reference is re-homed to a local `MethodDef`. Diagnostics come back
    /// anchored to their own file rather than thrown.
    let compileAssemblyWith
        (analyse: AssemblyFiles.AnalyseFile)
        (bclReferences: string list)
        (external: IExternalSymbolProvider)
        (project: ProjectInfo)
        (files: (string * string) list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        AssemblyFiles.analyseGated analyse project.AssemblyName external files
        |> Result.map (fun analysed ->
            // Each file's projected view (nearest-first) ahead of the external stack.
            let symbols =
                ExternalSymbolProviders.composite ([ for f in analysed.Files -> f.View ] @ [ external ])

            let tasts = [ for f in analysed.Files -> f.Frozen ]
            Codegen.compileFilesWithBclReferences bclReferences symbols project tasts
        )

    /// The multi-file counterpart of `compile`, MSBuild-shaped: the default front end, with
    /// `BclReferences` threaded into both the contract provider and `AssemblyRef` identity.
    let compileAssembly
        (inputs: ClrCompilation)
        (files: (string * string) list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        let provider =
            ClrSymbolProviders.buildContractWithRefs inputs.SelfManifest inputs.BclReferences clrTarget inputs.Manifests

        compileAssemblyWith Pipeline.analyseFor inputs.BclReferences provider inputs.Project files

    /// `compile`, then — when `Project.OutputPath` is set — a runnable framework-dependent
    /// bundle. An in-memory compilation returns the artifact unwritten.
    let compileApp (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        compile inputs source
        |> Result.map (fun artifact ->
            match inputs.Project.OutputPath with
            | Some _ -> Materialise.materialiseApp inputs.Project artifact
            | None -> ()

            artifact
        )

    /// `compile` with `BclReferences` resolved from `Project.TargetFramework` — the
    /// no-MSBuild CONVENIENCE. Explicit `BclReferences` remains the PRIMARY mechanism.
    let compileForTfm (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match inputs.Project.TargetFramework with
        | None ->
            Error
                [
                    driverDiagnostic "compileForTfm requires ProjectInfo.TargetFramework to be set"
                ]
        | Some tfm ->
            match RefPack.resolve tfm with
            | Result.Error msg -> Error [ driverDiagnostic msg ]
            | Result.Ok dlls -> compile { inputs with BclReferences = dlls } source
