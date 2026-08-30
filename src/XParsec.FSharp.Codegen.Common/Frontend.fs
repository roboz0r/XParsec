namespace XParsec.FSharp.Codegen.Common

open XParsec.FSharp.SemanticAnalysis

/// The analyse → gate → emit chain every backend drives, generic in the artifact its own
/// lowering produces. A backend supplies the emit step and its output packaging; the stages
/// above it are shared.
[<RequireQualifiedAccess>]
module Frontend =

    /// An assembly's units analysed in manifest order against `external`, stopping before the
    /// gate. Every finding stays anchored to the file that produced it.
    let analyse (external: IExternalSymbolProvider) (sources: AssemblySources) : AnalysedAssembly =
        AnalysedAssembly.analyse Pipeline.analyseFileFor external sources

    /// An analysed assembly admitted to emission and lowered by `emit`. Admission discharges
    /// the recorded `[<Import>]` obligations against `modules`; a refusal carries every
    /// error-severity finding in manifest order.
    let emitAnalysed
        (modules: IRuntimeModules)
        (emit: EmittableAssembly -> 'artifact)
        (analysed: AnalysedAssembly)
        : Result<'artifact, AssemblyFiles.AnchoredDiagnostic list> =
        AnalysedAssembly.gate modules analysed |> Result.map emit

    /// `analyse` then `emitAnalysed`.
    let compile
        (modules: IRuntimeModules)
        (emit: EmittableAssembly -> 'artifact)
        (external: IExternalSymbolProvider)
        (sources: AssemblySources)
        : Result<'artifact, AssemblyFiles.AnchoredDiagnostic list> =
        analyse external sources |> emitAnalysed modules emit
