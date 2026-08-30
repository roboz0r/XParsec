namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis

/// Every unit of an assembly, in manifest order. A parse failure and an error diagnostic are
/// members of the result.
[<NoEquality; NoComparison>]
type AnalysedAssembly =
    {
        Assembly: CompilingAssembly
        Units: UnitOutcome list
        /// The composed visibility the units were analysed under: their published views above
        /// the reference floor and the language prelude. A whole-assembly emission resolves
        /// through this; a single unit's own scope was narrower.
        Visibility: IExternalSymbolProvider
    }

    member this.Diagnostics: AnchoredDiagnostic list =
        List.collect UnitOutcome.surfaced this.Units

/// An assembly with every file analysed and no error-severity finding, so emission is defined
/// for all of it.
[<NoEquality; NoComparison>]
type EmittableAssembly =
    {
        Assembly: CompilingAssembly
        /// The assembly's files in manifest order.
        Files: FrozenFile list
        /// Every file's retained text as ONE anchor domain.
        Retained: LexedFiles
        /// The composed visibility the files were analysed under: the domain a whole-assembly
        /// emission resolves a cross-file reference through.
        Visibility: IExternalSymbolProvider
    }

[<RequireQualifiedAccess>]
module AnalysedAssembly =

    /// Analyse a multi-file assembly in manifest order through a chosen front end. Every unit
    /// is analysed, including the ones after a unit that yielded no tree.
    let analyse
        (analyseFile: AnalyseFile)
        (external: IExternalSymbolProvider)
        (sources: AssemblySources)
        : AnalysedAssembly =
        let analysed =
            analyseUnits analyseFile sources.Assembly external Publication.InAssembly sources.Units

        {
            Assembly = sources.Assembly
            Units = analysed.Units
            Visibility = visibility external analysed.Published
        }

    /// Every `[<Import>]` obligation the analysed units record, discharged against the
    /// target's module system; each finding is positioned at its binding in the declaring
    /// `.fs`.
    let private dischargeImports (modules: IRuntimeModules) (analysed: AnalysedAssembly) : AnchoredDiagnostic list =
        [
            for unit in analysed.Units do
                match unit with
                | UnitOutcome.Failed _ -> ()
                | UnitOutcome.Analysed u ->
                    for o in u.File.Imports do
                        match RuntimeModules.discharge modules o with
                        | ValueNone -> ()
                        | ValueSome e ->
                            yield!
                                anchorDiagnostics
                                    u.File.Retained
                                    [ Diagnostic.create (Kind.ConformanceFinding e) (Site.ofToken o.Site) [] ]
        ]

    /// Admit an assembly to emission: every unit must have parsed, no unit may surface an
    /// error-severity diagnostic, and every recorded `[<Import>]` obligation must discharge
    /// against `modules`, the target's module system. A refusal carries every such finding,
    /// unit findings first, each in manifest order.
    let gate
        (modules: IRuntimeModules)
        (analysed: AnalysedAssembly)
        : Result<EmittableAssembly, AnchoredDiagnostic list> =
        match
            AnchoredDiagnostic.errors analysed.Diagnostics
            @ dischargeImports modules analysed
        with
        | _ :: _ as errors -> Error errors
        | [] ->
            let files =
                analysed.Units
                |> List.map (
                    function
                    | UnitOutcome.Analysed u -> u.File
                    | UnitOutcome.Failed(leading, _) ->
                        failwithf "internal error: %s yielded no tree but reported no error" leading.Id.Name
                )

            Ok
                {
                    Assembly = analysed.Assembly
                    Files = files
                    Retained = retainedDomain files
                    Visibility = analysed.Visibility
                }
