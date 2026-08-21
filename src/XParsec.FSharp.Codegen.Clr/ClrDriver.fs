namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.SemanticAnalysis

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

    /// `consumer` with the reference set resolved from `Project.TargetFramework`, so the TFM
    /// the emitted `runtimeconfig.json` names and the pack the compilation binds against are
    /// the same one. A project with no TFM, or a TFM with no installed pack, is refused.
    let forTfm
        (project: ProjectInfo)
        (packages: string list)
        (compilationDefines: Set<string>)
        : Result<ClrCompilation, AssemblyFiles.AnchoredDiagnostic list> =
        let refuse (message: string) =
            Error(
                AssemblyFiles.unpositionedDiagnostics
                    AssemblyFileId.nowhere
                    [ Diagnostic.nowhere (Kind.Driver message) ]
            )

        match project.TargetFramework with
        | None -> refuse (sprintf "'%s' sets no ProjectInfo.TargetFramework" project.AssemblyName)
        | Some tfm ->
            match RefPack.resolve tfm with
            | Error message -> refuse message
            | Ok referenceAssemblies -> Ok(consumer project packages referenceAssemblies compilationDefines)

/// The production CLR driver: analyse → gate → emit against an EXPLICIT reference set,
/// returning front-end errors rather than throwing.
module ClrDriver =

    /// The compilation's reference set resolved, GATED on what resolving it found.
    let private contractFor
        (inputs: ClrCompilation)
        : Result<PackageProviders.AnalysedManifest, AssemblyFiles.AnchoredDiagnostic list> =
        ClrSymbolProviders.compilationContract inputs.SelfPackage inputs.ReferenceAssemblies inputs.Packages
        |> PackageProviders.AnalysedManifest.gate

    /// Source text as one CLR assembly's inputs, under the emitted assembly's own name.
    let sourcesFor
        (project: ProjectInfo)
        (compilationDefines: Set<string>)
        (units: AssemblyFiles.SourceUnit list)
        : AssemblySources =
        AssemblySources.synthetic project.AssemblyName Target.Clr compilationDefines units

    /// An analysed assembly gated and emitted as ONE PE. `referenceAssemblies` and
    /// `project.References` together supply the emitted `AssemblyRef` identities.
    let emitAnalysed
        (referenceAssemblies: string list)
        (project: ProjectInfo)
        (analysed: AnalysedAssembly)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        Frontend.emitAnalysed (Codegen.emitAssembly referenceAssemblies project) analysed

    /// An assembly's sources analysed and emitted as ONE PE. Diagnostics come back anchored to
    /// their own file rather than thrown. Resolution comes from `external` alone.
    let compileWith
        (referenceAssemblies: string list)
        (external: IExternalSymbolProvider)
        (project: ProjectInfo)
        (sources: AssemblySources)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        Frontend.compile (Codegen.emitAssembly referenceAssemblies project) external sources

    /// `compileWith` over source text, MSBuild-shaped: the reference set resolves the contract
    /// the units are analysed against and supplies the emitted `AssemblyRef` identities.
    let compile
        (inputs: ClrCompilation)
        (units: AssemblyFiles.SourceUnit list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        contractFor inputs
        |> Result.bind (fun contract ->
            sourcesFor inputs.Project inputs.CompilationDefines units
            |> compileWith inputs.ReferenceAssemblies contract.Provider inputs.Project
        )
