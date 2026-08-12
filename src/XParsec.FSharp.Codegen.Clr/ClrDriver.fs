namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// A single CLR compilation's inputs, MSBuild-shaped. `Packages` are the package DIRECTORIES
/// this compilation resolves against; `SelfPackage` is the one it DEFINES. The driver resolves
/// each to `manifest.clr.toml` itself, so a caller cannot hand it another target's contracts.
type ClrCompilation =
    {
        Project: ProjectInfo
        Packages: string list
        BclReferences: string list
        SelfPackage: string option
    }

module ClrCompilation =

    /// References packages, defines no primitives of its own. The shape to reach for unless
    /// compiling a package that declares `extern` types.
    let consumer (project: ProjectInfo) (packages: string list) (bclReferences: string list) : ClrCompilation =
        {
            Project = project
            Packages = packages
            BclReferences = bclReferences
            SelfPackage = None
        }

/// The production CLR driver: parse → analyse → gate → emit against an EXPLICIT reference
/// set, returning front-end errors rather than throwing.
module ClrDriver =

    let private driverDiagnostic (message: string) : Diagnostic =
        Diagnostic.nowhere (Kind.Driver message)

    /// The ANALYSIS diagnostics that block emission.
    let private blockingErrors (tast: FrozenPools) : Diagnostic list =
        Diagnostic.errors tast.Residue.Diagnostics

    /// Compile `source` to an in-memory PE against the compilation's own reference set. A
    /// driver program is a package CONSUMER, so it runs the default (non-self-host) front end.
    let compile (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match Pipeline.parseUnrecovered source with
        | Error diagnostics -> Error diagnostics
        | Ok parsed ->
            let provider =
                ClrSymbolProviders.buildContractWithRefs inputs.SelfPackage inputs.BclReferences inputs.Packages

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
                Target = Target.Clr
                ReferenceAssemblies = inputs.BclReferences
                Packages = inputs.Packages
                SelfPackage = inputs.SelfPackage
            }

    /// `compile` through the frozen-compile cache: a HIT skips parse + analyse + freeze, and an
    /// errored front end comes back as `Error` and is NOT stored. Emission is never elided, so
    /// the provider is built on both paths, and `digest` must be folded from THESE `inputs`.
    let compileCachedWith
        (store: ICacheStore)
        (digest: Hashing.CompilationDigest)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        let provider =
            ClrSymbolProviders.buildContractWithRefs inputs.SelfPackage inputs.BclReferences inputs.Packages

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

    /// An ordered `(path, source)` list analysed as one assembly and emitted as ONE PE, so a
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
            ClrSymbolProviders.buildContractWithRefs inputs.SelfPackage inputs.BclReferences inputs.Packages

        compileAssemblyWith Pipeline.analyseFor inputs.BclReferences provider inputs.Project files

    /// `compile`, then a runnable framework-dependent bundle when `Project.OutputPath` is
    /// set. An in-memory compilation returns the artifact unwritten.
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
