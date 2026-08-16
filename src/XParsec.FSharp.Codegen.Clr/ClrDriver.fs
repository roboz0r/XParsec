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
        ReferenceAssemblies: string list
        SelfPackage: string option
        CompilationDefines: Set<string>
    }

/// A `ClrCompilation` resolved ONCE for reuse across its files: the digest half of every file's
/// cache key, paired with the provider built from the same inputs.
type PreparedCompilation =
    private
        {
            Inputs: ClrCompilation
            Digest: Hashing.CompilationDigest
            /// The reference set already GATED, so a compilation whose contracts failed to
            /// resolve is refused for the whole run and not once per file.
            Contract: Result<SymbolProviders.Contract, Diagnostic list>
        }

module ClrCompilation =

    /// References packages, defines no primitives of its own. The shape to reach for unless
    /// compiling a package that declares `extern` types.
    let consumer
        (project: ProjectInfo)
        (packages: string list)
        (referenceAssemblies: string list)
        compilationDefines
        : ClrCompilation =
        {
            Project = project
            Packages = packages
            ReferenceAssemblies = referenceAssemblies
            SelfPackage = None
            CompilationDefines = compilationDefines
        }

/// The production CLR driver: parse → analyse → gate → emit against an EXPLICIT reference
/// set, returning front-end errors rather than throwing.
module ClrDriver =

    let private driverDiagnostic (message: string) : Diagnostic =
        Diagnostic.nowhere (Kind.Driver message)

    /// The ANALYSIS diagnostics that block emission.
    let private blockingErrors (tast: FrozenPools) : Diagnostic list =
        Diagnostic.errors tast.Residue.Diagnostics

    /// The compilation's reference set resolved, GATED on what resolving it found.
    let private contractFor
        (inputs: ClrCompilation)
        : Result<SymbolProviders.Contract, AssemblyFiles.AnchoredDiagnostic list> =
        ClrSymbolProviders.compilationContract inputs.SelfPackage inputs.ReferenceAssemblies inputs.Packages
        |> SymbolProviders.Contract.gate

    /// A gate refusal as the FLAT diagnostics a single-file entry returns; the file each names
    /// is in its own message, there being no compiling file to anchor it to.
    let private unanchored (diagnostics: AssemblyFiles.AnchoredDiagnostic list) : Diagnostic list =
        diagnostics |> List.map (fun d -> d.Diagnostic)

    /// Compile `source` to an in-memory PE against the compilation's own reference set. A
    /// driver program is a package CONSUMER, so it runs the default (non-self-host) front end.
    let compile (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match contractFor inputs |> Result.mapError unanchored with
        | Error contractErrors -> Error contractErrors
        | Ok contract ->
            match ParseChain.parseUnrecovered inputs.CompilationDefines source with
            | Error diagnostics -> Error diagnostics
            | Ok parsed ->
                let provider = contract.Provider

                let tast =
                    Pipeline.analyseFor
                        {
                            Name = inputs.Project.AssemblyName
                            Target = Target.Clr
                        }
                        provider
                        (Hashing.originSourceOfText parsed.Lexed)
                        parsed.File

                match blockingErrors tast with
                | [] -> Ok(Codegen.compileWithReferences inputs.ReferenceAssemblies provider inputs.Project tast)
                | errors -> Error errors

    /// Everything a cached front end depends on EXCEPT one file's text. Fold it once per
    /// compilation: reading the reference closure costs what the front end a hit elides does.
    let compilationDigest (inputs: ClrCompilation) : Hashing.CompilationDigest =
        Hashing.compilationDigest
            {
                HomeAssembly = inputs.Project.AssemblyName
                Target = Target.Clr
                ReferenceAssemblies = inputs.ReferenceAssemblies
                Packages = inputs.Packages
                SelfPackage = inputs.SelfPackage
                CompilationDefines = inputs.CompilationDefines
            }

    /// Resolve a compilation's per-file-invariant work: the digest and the contract provider,
    /// which each read the dependency closure.
    let prepare (inputs: ClrCompilation) : PreparedCompilation =
        {
            Inputs = inputs
            Digest = compilationDigest inputs
            Contract = contractFor inputs |> Result.mapError unanchored
        }

    /// `compile` through the frozen-compile cache: a HIT skips parse + analyse + freeze, and an
    /// errored front end comes back as `Error` and is NOT stored. Emission is never elided, so
    /// the provider serves both paths.
    let compileCachedWith
        (store: ICacheStore)
        (prepared: PreparedCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        let inputs = prepared.Inputs

        match prepared.Contract with
        | Error contractErrors -> Error contractErrors
        | Ok contract ->

            // The key covers the path the frozen tree's nodes name, so a hit cannot serve a tree
            // anchored elsewhere.
            let path = Hashing.textOriginPath source

            let key =
                {
                    Query = QueryId.Freeze
                    CodeVersion = Cache.CodeVersion
                    Input = Hashing.fileInputHash path source prepared.Digest
                }

            FrozenCache.freezeResult
                store
                key
                (fun () ->
                    match ParseChain.parseUnrecovered inputs.CompilationDefines source with
                    | Error diagnostics -> Error diagnostics
                    | Ok parsed ->
                        let tast =
                            Pipeline.analyseFor
                                {
                                    Name = inputs.Project.AssemblyName
                                    Target = Target.Clr
                                }
                                contract.Provider
                                (Hashing.originSource path parsed.Lexed)
                                parsed.File

                        match blockingErrors tast with
                        | [] -> Ok tast
                        | errors -> Error errors
                )
            |> Result.map (fun frozen ->
                Codegen.compileWithReferences inputs.ReferenceAssemblies contract.Provider inputs.Project frozen
            )

    /// `compileCachedWith` for a ONE-FILE compilation, preparing inline. Several files through
    /// this would re-read the whole dependency closure per file.
    let compileCached
        (store: ICacheStore)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        compileCachedWith store (prepare inputs) source

    /// An ordered source-file list analysed as one assembly and emitted as ONE PE, so a
    /// cross-file reference is re-homed to a local `MethodDef`. Diagnostics come back
    /// anchored to their own file rather than thrown.
    let compileAssemblyWith
        (referenceAssemblies: string list)
        (external: IExternalSymbolProvider)
        (project: ProjectInfo)
        (units: Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        let assembly: CompilingAssembly =
            {
                Name = project.AssemblyName
                Target = Target.Clr
            }

        AssemblyFiles.analyseGatedParsed Pipeline.analyseFor assembly external units
        |> Result.map (fun analysed ->
            // The visibility stack analysis composed, rebuilt: `external` is the floor and
            // `Files` is in file order, so each view pushes on top of the ones it may shadow.
            let symbols =
                ExternalSymbolProviders.composite (
                    analysed.Files |> List.fold (fun stack f -> f.View :: stack) [ external ]
                )

            let tasts = [ for f in analysed.Files -> f.Frozen ]
            Codegen.compileFilesWithReferences referenceAssemblies symbols project tasts
        )

    /// The multi-file counterpart of `compile`, MSBuild-shaped: `ReferenceAssemblies` threaded
    /// into both the contract provider and `AssemblyRef` identity.
    let compileAssembly
        (inputs: ClrCompilation)
        (units: AssemblyFiles.SourceUnit list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        contractFor inputs
        |> Result.bind (fun contract ->
            compileAssemblyWith
                inputs.ReferenceAssemblies
                contract.Provider
                inputs.Project
                (List.map (AssemblyFiles.parseUnit inputs.CompilationDefines) units)
        )

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

    /// `compile` with `ReferenceAssemblies` resolved from `Project.TargetFramework`, the
    /// no-MSBuild CONVENIENCE. Passing them explicitly remains the PRIMARY mechanism.
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
            | Result.Ok dlls ->
                compile
                    { inputs with
                        ReferenceAssemblies = dlls
                    }
                    source
