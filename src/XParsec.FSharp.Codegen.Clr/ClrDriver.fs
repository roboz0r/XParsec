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

/// The production CLR driver: parse → analyse → gate → emit against an EXPLICIT reference
/// set, returning front-end errors rather than throwing.
module ClrDriver =

    /// The compilation's reference set resolved, GATED on what resolving it found.
    let private contractFor
        (inputs: ClrCompilation)
        : Result<PackageProviders.AnalysedManifest, AssemblyFiles.AnchoredDiagnostic list> =
        ClrSymbolProviders.compilationContract inputs.SelfPackage inputs.ReferenceAssemblies inputs.Packages
        |> PackageProviders.AnalysedManifest.gate

    /// An ordered source-file list analysed as one assembly under `assemblyName`, stopping
    /// before the gate. Every finding stays anchored to the file that produced it. Resolution
    /// comes from `external` alone.
    let analyseWith
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
    let compileWith
        (referenceAssemblies: string list)
        (external: IExternalSymbolProvider)
        (project: ProjectInfo)
        (units: AssemblyFiles.AssemblyUnit list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        analyseWith external project.AssemblyName units
        |> emitAnalysed referenceAssemblies project

    /// `compileWith`, MSBuild-shaped: the reference set resolves the contract the units are
    /// analysed against and supplies the emitted `AssemblyRef` identities.
    let compile
        (inputs: ClrCompilation)
        (units: AssemblyFiles.SourceUnit list)
        : Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list> =
        contractFor inputs
        |> Result.bind (fun contract ->
            compileWith
                inputs.ReferenceAssemblies
                contract.Provider
                inputs.Project
                (List.map (AssemblyFiles.AssemblyUnit.parse inputs.CompilationDefines) units)
        )
