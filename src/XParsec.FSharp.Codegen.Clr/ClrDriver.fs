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

    /// The compilation's reference set resolved, GATED on what resolving it found.
    let private contractFor
        (inputs: ClrCompilation)
        : Result<PackageProviders.AnalyzedManifest, AssemblyFiles.AnchoredDiagnostic list> =
        ClrSymbolProviders.compilationContract inputs.SelfPackage inputs.ReferenceAssemblies inputs.Packages
        |> PackageProviders.AnalyzedManifest.gate

    /// A gate refusal as the FLAT diagnostics a single-file entry returns. A positioned
    /// diagnostic keeps its file and position, rendered into the message; a whole-set fault
    /// has no file and passes through.
    let private unanchored (diagnostics: AssemblyFiles.AnchoredDiagnostic list) : Diagnostic list =
        [
            for d in diagnostics ->
                if d.Path = AssemblyFileId.nowhere then
                    d.Diagnostic
                else
                    Diagnostic.nowhere (
                        Kind.Driver(sprintf "%s(%d,%d): %s" d.Path.Name d.Line d.Col d.Diagnostic.Message)
                    )
        ]

    /// Codegen's own gate firing on a tree `analyseGated` already passed, which means the two
    /// disagree. There is no file left to anchor to, so each finding is re-filed as a driver
    /// refusal carrying the original message.
    let private reanchored (diagnostics: Diagnostic list) : AssemblyFiles.AnchoredDiagnostic list =
        AssemblyFiles.unpositionedDiagnostics
            AssemblyFileId.nowhere
            [ for d in diagnostics -> Diagnostic.nowhere (Kind.Driver d.Message) ]

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
                let symbols = CodegenSymbols.ofProvider provider

                let tast =
                    Pipeline.analyseFor
                        {
                            Name = inputs.Project.AssemblyName
                            Target = Target.Clr
                        }
                        provider
                        (Hashing.lexedFileOfText parsed.Lexed)
                        parsed.File

                Codegen.compileWithReferences inputs.ReferenceAssemblies symbols inputs.Project tast

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

        AssemblyFiles.analyseGated Pipeline.analyseFor assembly external units
        |> Result.bind (fun analysed ->
            // The visibility stack analysis composed, rebuilt: `external` is the floor and
            // `Files` is in file order, so each view pushes on top of the ones it may shadow.
            let symbols =
                ExternalSymbolProviders.composite (
                    analysed.Files |> List.fold (fun stack f -> f.View :: stack) [ external ]
                )
                |> CodegenSymbols.ofProvider

            let tasts = [ for f in analysed.Files -> f.Frozen ]

            Codegen.compileFilesWithReferences referenceAssemblies symbols project tasts
            |> Result.mapError reanchored
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
