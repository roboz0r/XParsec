namespace XParsec.FSharp.SemanticAnalysis

// The units a PACKAGE compiles, as against the list a driver is handed: the manifest's `impl`
// order, each body under the contract the conformance pass married it to. The pairing is
// `checkManifest`'s, so `sig-only` and `impl-only` are honoured here without restating them.

module PackageUnits =

    /// `.fs` relative path → the `.fsi` that publishes it, for the contracts that paired.
    let private contractOfImpl (outcome: ConformancePass.PackageOutcome) : Map<string, string> =
        Map.ofList
            [
                for p in outcome.Pairs do
                    match p with
                    | ConformancePass.PairOutcome.Paired r -> yield r.ImplFile, r.SigFile
                    | ConformancePass.PairOutcome.SigOnly _
                    | ConformancePass.PairOutcome.Unrepresentable _
                    | ConformancePass.PairOutcome.RuntimeServed _
                    | ConformancePass.PairOutcome.ParseFailed _ -> ()
            ]

    /// Every `impl` body as a compilation unit, in manifest order, under its contract. A body
    /// with none publishes the surface it infers.
    let ofOutcome
        (manifest: ReferencedProject.Manifest)
        (outcome: ConformancePass.PackageOutcome)
        : AssemblyFiles.SourceUnit list =
        let dir = manifest.Dir
        let contracts = contractOfImpl outcome

        [
            for implRel in manifest.Impl do
                let implementation = AssemblyFiles.SourceFile.read dir implRel

                match Map.tryFind implRel contracts with
                | Some sigRel ->
                    AssemblyFiles.SourceUnit.paired (AssemblyFiles.SourceFile.read dir sigRel) implementation
                | None -> AssemblyFiles.SourceUnit.ofImplementation implementation
        ]

    /// `ofOutcome` for a caller holding only the path: it loads and conforms the package
    /// itself. The verdicts are dropped, since `ConformancePass.enforce` is the caller's gate.
    let ofManifest (mp: ReferencedProject.ManifestPath) : Result<AssemblyFiles.SourceUnit list, string> =
        ReferencedProject.loadManifest mp
        |> Result.bind (fun m -> ConformancePass.checkManifest mp |> Result.map (ofOutcome m))
