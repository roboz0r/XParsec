namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis

/// A whole assembly that passed the gate: its files in manifest order.
type AnalysedAssembly =
    {
        Files: FrozenFile list
    }

    /// Every file's retained text as ONE domain. A backend resolves through it the anchors of
    /// a node spliced out of a prior file.
    member this.Retained: LexedFiles =
        LexedFiles.ofSeq [ for f in this.Files -> f.Retained ]

/// The driver entry for compiling an assembly: its units analysed under
/// `Publication.InAssembly` and turned into frozen files, gated or ungated.
module CompileAssembly =

    /// Analyse a multi-file assembly in manifest order through a chosen front end. A unit
    /// that never reached analysis comes back as every fault its halves carried.
    let analyseWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: AssemblyUnit list)
        : Result<FrozenFile, UnparsedFile list> list =
        analyseUnits analyse assembly external Publication.InAssembly units
        |> List.map (
            function
            | UnitOutcome.Analysed u -> Ok u.File
            | UnitOutcome.Failed(leading, rest) -> Error(leading :: rest)
        )

    /// `analyseWith` for a caller holding raw TEXT rather than parsed units.
    let analyseAssemblyWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        compilationDefines
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile list> list =
        analyseWith analyse assembly external (List.map (AssemblyUnit.parse compilationDefines) units)

    /// Analyse a multi-file assembly through the default package/FSharp.Core front end.
    /// The self-host one is reached by passing it to `analyseAssemblyWith` directly.
    let analyseAssembly
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        compilationDefines
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile list> list =
        analyseAssemblyWith Pipeline.analyseFor assembly external compilationDefines units

    /// Every analysed file's diagnostics, each anchored to ITS OWN file. Recovery's findings
    /// come first: they are what the tree the analysis ran on was patched up from.
    let consolidatedDiagnostics (files: FrozenFile list) : AnchoredDiagnostic list = List.collect fileDiagnostics files

    /// `analyseWith`, GATED: every file must have parsed, and no analysed file may carry
    /// an error-severity diagnostic. A parse failure is fatal for the whole assembly and is
    /// reported alone, because the files after it analysed against a truncated view.
    let analyseGated
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: AssemblyUnit list)
        : Result<AnalysedAssembly, AnchoredDiagnostic list> =
        let analysedUnits =
            analyseUnits analyse assembly external Publication.InAssembly units

        let parseFailures =
            analysedUnits
            |> List.collect (
                function
                | UnitOutcome.Failed(leading, rest) -> List.collect failureDiagnostics (leading :: rest)
                | UnitOutcome.Analysed _ -> []
            )

        match parseFailures with
        | _ :: _ -> Error parseFailures
        | [] ->
            let analysed =
                analysedUnits
                |> List.choose (
                    function
                    | UnitOutcome.Analysed u -> Some u
                    | UnitOutcome.Failed _ -> None
                )

            match
                analysed
                |> List.collect (fun u -> u.Surfaced)
                |> List.filter (fun a -> a.Diagnostic.Severity = Severity.Error)
            with
            | _ :: _ as errors -> Error errors
            | [] ->
                Ok
                    {
                        Files = [ for u in analysed -> u.File ]
                    }
