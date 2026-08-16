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
            /// The companion `.fs` relative path (resolved from the impl set).
            ImplFile: string
            ModuleMismatch: Conformance.ModuleDeclMismatch voption
            /// Empty = the pair conforms.
            Errors: Conformance.ConformanceError list
        }

    /// The verdict for one signature file in a package.
    [<RequireQualifiedAccess>]
    type PairOutcome =
        /// `.fsi` with a companion `.fs` in the impl set.
        | Paired of PairResult
        /// `.fsi` with NO companion `.fs` in the impl set.
        | SigOnly of sigFile: string
        /// `.fsi` with no companion `.fs` for this target, and NOTHING in it that a `.fs`
        /// could supply: every declaration is an `extern` or a transparent abbreviation.
        /// Derived from the file's CONTENT: the absent `.fs` states the target has no repr.
        | Unrepresentable of sigFile: string * types: string list
        /// `.fsi` with no companion `.fs` for this target, whose every `val` the target's
        /// committed RUNTIME ASSET exports (`Vesper.Core.mjs`'s `structuralEquals`), so no
        /// `.fs` is owed. Renaming an export drops the signature file back to `SigOnly`.
        | RuntimeServed of sigFile: string * asset: string * values: string list
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
            /// The `.fsi` files the manifest declares DELIBERATELY impl-free for this target
            /// (`[core] sig-only`). A `SigOnly` signature file in this set is an accepted
            /// exemption; one outside it is the FS0240-style hard error.
            SigOnlyExemptions: Set<string>
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
    let private faultDetail (package: string) (relative: string) (fault: PackageSource.FileFault) : string =
        (PackageSource.FileFault.toFailure package relative fault).Diagnostics
        |> List.map (fun d -> d.Message)
        |> String.concat "; "

    /// Conform every signature file a package manifest names against its implementation
    /// companion, over the package as READ: no F# source is re-read and no pairing re-taken, so
    /// a file the read could not deliver becomes a `ParseFailed` verdict.
    let check (pkg: PackageSource.ParsedPackage) : PackageOutcome =
        let m = pkg.Manifest
        let declaredSigOnly = m.SigOnly |> Set.ofList

        // The committed runtime asset and the names it publishes, needed because a signature
        // file may ship no `.fs` when its bodies live here. Only the FIRST asset counts.
        let runtimeAsset =
            match m.Runtime with
            | rel :: _ ->
                let abs = Path.Combine(m.Dir, rel)

                if File.Exists abs then
                    Some(Path.GetFileName rel, exportedNames (File.ReadAllText abs))
                else
                    None
            | [] -> None

        // A companion-less `.fsi`, split on its own CONTENT: it owes a `.fs` unless EVERY
        // declaration is satisfied without one, namely an `extern` or transparent abbreviation
        // always, and a `val` exactly when the committed runtime asset exports it.
        let unpaired (fsiRel: string) (signature: ParseChain.ParsedSignature) : PairOutcome =
            let decls = Conformance.summariseSig signature.Lexed signature.File
            let valNames = Conformance.summariseSigVals signature.Lexed signature.File

            let externs =
                decls
                |> List.choose (fun d -> if d.Shape.DemandsIntrinsic then Some d.Name else None)

            let bodiless =
                decls
                |> List.forall (fun d -> d.Shape.DemandsIntrinsic || d.Shape = Conformance.SigShape.Abbrev)

            if not bodiless then
                PairOutcome.SigOnly fsiRel
            elif List.isEmpty decls && List.isEmpty valNames then
                // A signature file that declares nothing states nothing.
                PairOutcome.SigOnly fsiRel
            elif List.isEmpty valNames then
                PairOutcome.Unrepresentable(fsiRel, externs)
            else
                match runtimeAsset with
                | Some(asset, exports) when valNames |> List.forall exports.Contains ->
                    PairOutcome.RuntimeServed(fsiRel, asset, valNames)
                | _ -> PairOutcome.SigOnly fsiRel

        let outcome (entry: PackageSource.SignatureEntry) : PairOutcome =
            let fsiRel = entry.Signature.Relative

            match entry.Signature.Outcome with
            | Error fault -> PairOutcome.ParseFailed(fsiRel, faultDetail m.Name fsiRel fault)
            | Ok signature ->
                match entry.Companion with
                | ValueSome companion ->
                    match companion.Outcome with
                    // Fails the PAIR: calling it a signature without an implementation would
                    // blame the `.fsi` for the `.fs`'s defect.
                    | Error fault -> PairOutcome.ParseFailed(fsiRel, faultDetail m.Name companion.Relative fault)
                    | Ok implementation ->
                        let verdict =
                            Conformance.checkUnit
                                signature.Lexed
                                signature.File
                                implementation.Lexed
                                implementation.File

                        PairOutcome.Paired
                            {
                                SigFile = fsiRel
                                ImplFile = companion.Relative
                                ModuleMismatch = verdict.ModuleMismatch
                                Errors = verdict.Errors
                            }
                // A manifest `sig-only` declaration outranks the content split.
                | ValueNone when declaredSigOnly.Contains fsiRel -> PairOutcome.SigOnly fsiRel
                | ValueNone -> unpaired fsiRel signature

        {
            Package = m.Name
            Pairs = pkg.Signatures |> List.map outcome
            SigOnlyExemptions = declaredSigOnly
        }

    /// `check` for a caller holding only the path. `Error` ONLY when the package is wholly
    /// un-checkable, meaning a malformed or absent MANIFEST.
    let checkManifest (mp: ReferencedProject.ManifestPath) : Result<PackageOutcome, string> =
        ReferencedProject.loadManifest mp
        |> Result.map (PackageSource.readPackage >> check)

    // ---- Enforcement: conformance findings become hard errors ------------------

    /// One `ConformanceVerdict` per finding, every one a `Severity.Error`. Empty = the
    /// package conforms.
    let enforce (outcome: PackageOutcome) : XParsec.FSharp.SemanticAnalysis.Diagnostic list =
        // A package-level verdict is about a signature, not a place in any one file.
        let err (verdict: ConformanceVerdict) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
            Diagnostic.nowhere (Kind.Conformance(outcome.Package, verdict))

        // The signature files that DID pair, the basis for catching a `sig-only`
        // exemption naming a file that in fact has a companion `.fs`.
        let pairedSigs =
            set
                [
                    for p in outcome.Pairs do
                        match p with
                        | PairOutcome.Paired r -> yield r.SigFile
                        | PairOutcome.SigOnly _
                        | PairOutcome.Unrepresentable _
                        | PairOutcome.RuntimeServed _
                        | PairOutcome.ParseFailed _ -> ()
                ]

        // `Unrepresentable` / `RuntimeServed` are only reached for a signature file the manifest
        // does NOT declare `sig-only`, so neither can be a declared exemption's file.
        let sigOnlySigs =
            set
                [
                    for p in outcome.Pairs do
                        match p with
                        | PairOutcome.SigOnly s -> yield s
                        | PairOutcome.Paired _
                        | PairOutcome.Unrepresentable _
                        | PairOutcome.RuntimeServed _
                        | PairOutcome.ParseFailed _ -> ()
                ]

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
                | PairOutcome.SigOnly s ->
                    if not (outcome.SigOnlyExemptions.Contains s) then
                        yield err (ConformanceVerdict.SigWithoutImpl s)
                // ACCEPTED: the absent `.fs` states that this target represents none of
                // these types.
                | PairOutcome.Unrepresentable _
                // ACCEPTED: the committed runtime asset exports every declared value.
                | PairOutcome.RuntimeServed _ -> ()
                | PairOutcome.ParseFailed(sigFile, detail) ->
                    yield err (ConformanceVerdict.PairParseFailure(sigFile, detail))

            for ex in outcome.SigOnlyExemptions do
                if pairedSigs.Contains ex then
                    yield err (ConformanceVerdict.StaleSigOnly ex)
                elif not (sigOnlySigs.Contains ex) then
                    yield err (ConformanceVerdict.UnknownSigOnly ex)
        ]
