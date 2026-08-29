namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Manifest-driven `.fsi`↔`.fs` conformance: the pairs come from a package's manifest for one
// target, on its own pairing key, so the js manifest's `prim-types-int.js.fs` pairs with
// `prim-types-int.fsi`.

module ConformancePass =

    /// A signature file paired with its implementation file, plus the conformance verdict.
    [<NoEquality; NoComparison>]
    type PairResult =
        {
            /// `.fsi` relative path (as listed in the manifest).
            SigFile: string
            /// The companion `.fs` relative path (resolved from the pairing).
            ImplFile: string
            ModuleMismatch: Conformance.ModuleDeclMismatch voption
            /// Empty = the pair conforms.
            Errors: Conformance.ConformanceError list
        }

    /// The verdict for one signature file in a package.
    [<RequireQualifiedAccess>]
    type PairOutcome =
        /// `.fsi` with a companion `.fs`.
        | Paired of PairResult
        /// The `.fsi` or its companion `.fs` failed to parse, so the pair could not be
        /// conformed. Per signature file, so one malformed file does not abort the package.
        | ParseFailed of sigFile: string * detail: string

    /// The conformance outcome for a whole package, derived from its manifest.
    [<NoEquality; NoComparison>]
    type PackageOutcome =
        {
            /// Package / assembly simple name.
            Package: string
            /// One outcome per signature file, in manifest `files` order.
            Pairs: PairOutcome list
        }

    // A `[core] runtime` asset is a committed ESM module read as JS TEXT, never
    // parsed as F#. Only the presence of an exported NAME is read, never the body behind it.
    let private esmDeclaredExport =
        System.Text.RegularExpressions.Regex(
            @"\bexport\s+(?:default\s+)?(?:async\s+)?(?:const|let|var|function|class)\b[\s*]*([A-Za-z_$][A-Za-z0-9_$]*)",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    let private esmExportList =
        System.Text.RegularExpressions.Regex(
            @"\bexport\s*\{([^}]*)\}",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    let private exportedNames (source: string) : Set<string> =
        set
            [
                for m in esmDeclaredExport.Matches source -> m.Groups.[1].Value

                for m in esmExportList.Matches source do
                    for spec in m.Groups.[1].Value.Split(',') do
                        // `a as b` publishes `b`; a bare `a` publishes itself.
                        match spec.Split([| " as " |], System.StringSplitOptions.None) with
                        | [| _; alias |] -> yield alias.Trim()
                        | _ -> yield spec.Trim()
            ]
        |> Set.remove ""

    /// Why a manifest-named path yielded no tree, in the FAULT's own words.
    let private faultDetail (fault: FileFault) : string =
        FileFault.diagnostics fault
        |> List.map (fun d -> d.Message)
        |> String.concat "; "

    /// Conform every signature file a package manifest lists against its implementation
    /// companion, over the package as READ: no F# source is re-read and no pairing re-taken, so
    /// a file the read could not deliver becomes a `ParseFailed` verdict.
    let check (parsed: ParsedManifest) : PackageOutcome =
        let m = parsed.Manifest

        // One scrape per `[<Import>]`-named asset: `ValueNone` for a path outside the
        // manifest's `runtime` list or a listed file absent on disk.
        let assetExports =
            System.Collections.Generic.Dictionary<string, Set<string> voption>()

        let exportsOf (assetRel: string) : Set<string> voption =
            match assetExports.TryGetValue assetRel with
            | true, v -> v
            | _ ->
                let v =
                    if List.contains assetRel m.Runtime then
                        let abs = Path.Combine(m.Dir, assetRel)

                        if File.Exists abs then
                            ValueSome(exportedNames (File.ReadAllText abs))
                        else
                            ValueNone
                    else
                        ValueNone

                assetExports.[assetRel] <- v
                v

        // The manifest-held half of the `[<Import>]` check; the CST half is `checkUnit`'s.
        let checkImport (imp: Conformance.ImportBinding) : Conformance.ConformanceError list =
            let path = imp.Ref.Path

            if not (path.StartsWith("./", System.StringComparison.Ordinal)) then
                [ Conformance.ConformanceError.ImportUnknownAsset(imp.Name, path) ]
            else
                let assetRel = path.Substring 2

                match exportsOf assetRel with
                | ValueNone -> [ Conformance.ConformanceError.ImportUnknownAsset(imp.Name, path) ]
                | ValueSome exports when not (exports.Contains imp.Ref.Selector) ->
                    [
                        Conformance.ConformanceError.ImportMissingExport(imp.Name, imp.Ref.Selector, assetRel)
                    ]
                | ValueSome _ -> []

        let pair
            (signatureFile: ReadFile<ParseChain.ParsedSignature>)
            (implementationFile: ReadFile<ParseChain.ParsedImplementation>)
            : PairOutcome =
            let fsiRel = signatureFile.Relative

            match signatureFile.Outcome with
            | Error fault -> PairOutcome.ParseFailed(fsiRel, faultDetail fault)
            | Ok signature ->
                match implementationFile.Outcome with
                // Fails the PAIR: an unpaired-signature verdict would report the `.fs`'s
                // parse failure as an `.fsi` defect.
                | Error fault -> PairOutcome.ParseFailed(fsiRel, faultDetail fault)
                | Ok implementation ->
                    let verdict =
                        Conformance.checkUnit signature.Lexed signature.Tree implementation.Lexed implementation.Tree

                    PairOutcome.Paired
                        {
                            SigFile = fsiRel
                            ImplFile = implementationFile.Relative
                            ModuleMismatch = verdict.ModuleMismatch
                            Errors = verdict.Errors @ List.collect checkImport verdict.Imports
                        }

        {
            Package = m.Name
            Pairs =
                [
                    for unit in parsed.Units do
                        match unit.Signature with
                        | ValueSome signature -> pair signature unit.Implementation
                        // A `.fs` owes no `.fsi`.
                        | ValueNone -> ()
                ]
        }

    /// `check` for a caller holding only the path. `Error` ONLY when the package is wholly
    /// un-checkable, meaning a malformed or absent MANIFEST.
    let checkManifest (mp: ReferencedProject.ManifestPath) : Result<PackageOutcome, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (ParsedManifest.ofManifest >> check)

    // ---- Enforcement: conformance findings become hard errors ------------------

    /// One `ConformanceVerdict` per finding, every one a `Severity.Error`. Empty = the
    /// package conforms.
    let enforce (outcome: PackageOutcome) : XParsec.FSharp.SemanticAnalysis.Diagnostic list =
        // A package-level verdict is about a signature, not a place in any one file.
        let err (verdict: ConformanceVerdict) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
            Diagnostic.nowhere (Kind.Conformance(outcome.Package, verdict))

        [
            for p in outcome.Pairs do
                match p with
                | PairOutcome.Paired r ->
                    for e in r.Errors do
                        yield err (ConformanceVerdict.Unimplemented(r.SigFile, Conformance.describe e))

                    match r.ModuleMismatch with
                    | ValueSome mm ->
                        yield
                            err (
                                ConformanceVerdict.ModulePairingMismatch(r.SigFile, r.ImplFile, mm.SigDecl, mm.ImplDecl)
                            )
                    | ValueNone -> ()
                | PairOutcome.ParseFailed(sigFile, detail) ->
                    yield err (ConformanceVerdict.PairParseFailure(sigFile, detail))
        ]
