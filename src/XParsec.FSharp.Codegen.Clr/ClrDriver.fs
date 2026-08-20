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
        : Result<PackageProviders.AnalysedManifest, AssemblyFiles.AnchoredDiagnostic list> =
        ClrSymbolProviders.compilationContract inputs.SelfPackage inputs.ReferenceAssemblies inputs.Packages
        |> PackageProviders.AnalysedManifest.gate

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

                let home = AssemblyName inputs.Project.AssemblyName

                let tast =
                    Pipeline.analyseFor
                        { Name = home; Target = Target.Clr }
                        provider
                        // No path was handed over, so the text names the file; the assembly IS
                        // known and is stamped rather than left blank.
                        (LexedFile.inAssembly home (AssemblyFileId.ofText parsed.Lexed.Input) parsed.Lexed)
                        parsed.Tree

                Codegen.compileWithReferences inputs.ReferenceAssemblies symbols inputs.Project tast

    /// An ordered source-file list analysed as one assembly under `assemblyName`, stopping
    /// before the gate. Every finding stays anchored to the file that produced it. Resolution
    /// comes from `external` alone.
    let analyseAssemblyWith
        (external: IExternalSymbolProvider)
        (assemblyName: string)
        (units: AssemblyFiles.AssemblyUnit list)
        : AnalysedAssembly =
        AssemblySources.ofUnits assemblyName Target.Clr units
        |> AnalysedAssembly.analyse Pipeline.analyseFor external

    /// An analysed assembly gated and emitted as ONE PE. `referenceAssemblies` and
    /// `project.References` together supply the emitted `AssemblyRef` identities.
    let emitAnalysed
        (referenceAssemblies: string list)
        (project: ProjectInfo)
        (analysed: AnalysedAssembly)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        AnalysedAssembly.gate analysed
        |> Result.map (Codegen.emitAssembly referenceAssemblies project)

    /// An ordered source-file list analysed as one assembly and emitted as ONE PE.
    /// Diagnostics come back anchored to their own file rather than thrown.
    let compileAssemblyWith
        (referenceAssemblies: string list)
        (external: IExternalSymbolProvider)
        (project: ProjectInfo)
        (units: AssemblyFiles.AssemblyUnit list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        analyseAssemblyWith external project.AssemblyName units
        |> emitAnalysed referenceAssemblies project

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
                (List.map (AssemblyFiles.AssemblyUnit.parse inputs.CompilationDefines) units)
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
