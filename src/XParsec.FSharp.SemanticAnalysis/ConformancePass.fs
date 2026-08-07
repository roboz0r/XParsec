namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Manifest-driven `.fsi`↔`.fs` conformance: the pairs come from a package `manifest.toml`
// on its own pairing key, so `prim-types-int.js.fs` pairs with `prim-types-int.fsi`.

module ConformancePass =

    /// The leading `module`/`namespace` declarations of a paired `.fsi`/`.fs`
    /// disagree — the pairing rule paired two files F# would not consider a pair.
    [<Struct; NoEquality; NoComparison>]
    type ModuleDeclMismatch = { SigDecl: string; ImplDecl: string }

    /// A `.fsi` contract paired with its `.fs` implementation, plus the conformance verdict.
    [<NoEquality; NoComparison>]
    type PairResult =
        {
            /// `.fsi` relative path (as listed in the manifest).
            SigFile: string
            /// The companion `.fs` relative path (resolved from the impl set).
            ImplFile: string
            ModuleMismatch: ModuleDeclMismatch option
            /// Empty = the pair conforms.
            Errors: Conformance.ConformanceError list
        }

    /// The verdict for one `.fsi` contract in a package.
    [<RequireQualifiedAccess>]
    type PairOutcome =
        /// `.fsi` with a companion `.fs` in the impl set.
        | Paired of PairResult
        /// `.fsi` with NO companion `.fs` in the impl set — impl-free.
        | SigOnly of sigFile: string
        /// `.fsi` with no companion `.fs` for this target, and NOTHING in it that a `.fs`
        /// could supply — every declaration is an `extern` or a transparent abbreviation.
        /// Derived from the file's CONTENT: the absent `.fs` states the target has no repr.
        | Unrepresentable of sigFile: string * types: string list
        /// `.fsi` with no companion `.fs` for this target, whose every `val` the target's
        /// committed RUNTIME ASSET exports (`Vesper.Core.mjs`'s `structuralEquals`), so no
        /// `.fs` is owed. Renaming an export drops the contract back to `SigOnly`.
        | RuntimeServed of sigFile: string * asset: string * values: string list
        /// The `.fsi` or its companion `.fs` failed to parse, so the pair could not be
        /// conformed. Per-contract, so one malformed file does not abort the package.
        | ParseFailed of sigFile: string * detail: string

    /// The conformance outcome for a whole package, derived from its manifest.
    [<NoEquality; NoComparison>]
    type PackageOutcome =
        {
            /// Package / assembly simple name.
            Package: string
            /// One outcome per `.fsi` contract, in manifest `files` order.
            Pairs: PairOutcome list
            /// `.fs` files in the impl set whose pairing key has no `.fsi` contract.
            ImplOnly: string list
            /// The `.fs` bodies the manifest declares contract-less for this target
            /// (`impl-only`). One reported in `ImplOnly` is accepted; one that is not is a
            /// stale/typo'd declaration.
            ImplOnlyDeclarations: Set<string>
            /// The `.fsi` files the manifest declares DELIBERATELY impl-free for this target
            /// (`[core] sig-only`). A `SigOnly` contract in this set is an accepted
            /// exemption; one outside it is the FS0240-style hard error.
            SigOnlyExemptions: Set<string>
        }

    let private identText (lexed: Lexed) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT)
        | TokenIndex.Virtual -> ""

    let private longIdentText (lexed: Lexed) (li: LongIdent<SyntaxToken>) : string =
        li.Idents |> Seq.map (identText lexed) |> String.concat "."

    /// The dotted leading `module`/`namespace` path of a parsed file — the basis of
    /// F#'s `QualifiedNameOfFile` pairing key. `"global"` for an explicit
    /// `namespace global`; `""` for an anonymous module (no declaration).
    let private leadingDeclPath (lexed: Lexed) (ast: FSharpAst<SyntaxToken>) : string =
        match ast with
        | FSharpAst.SignatureFile sf ->
            match sf with
            | SignatureFile.Namespaces groups when groups.Length > 0 ->
                match groups.[0] with
                | NamespaceDeclGroupSignature.Named(longIdent = li) -> longIdentText lexed li
                | NamespaceDeclGroupSignature.Global _ -> "global"
            | SignatureFile.Namespaces _ -> ""
            | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(longIdent = li)) ->
                longIdentText lexed li
            | SignatureFile.AnonymousModule _ -> ""
        | FSharpAst.ImplementationFile f ->
            match f with
            | ImplementationFile.Namespaces groups when groups.Length > 0 ->
                match groups.[0] with
                | NamespaceDeclGroup.Named(longIdent = li) -> longIdentText lexed li
                | NamespaceDeclGroup.Global _ -> "global"
            | ImplementationFile.Namespaces _ -> ""
            | ImplementationFile.NamedModule(NamedModule.NamedModule(longIdent = li)) -> longIdentText lexed li
            | ImplementationFile.AnonymousModule _ -> ""
        | _ -> ""

    // A `[targets.<t>] runtime` asset is a committed ESM module read as JS TEXT, never
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

    let private parseRel (name: string) (dir: string) (rel: string) : Result<VesperLib.ParsedFile, string> =
        VesperLib.parseFileFull
            {
                Path = { BucketName = name; Relative = rel }
                Absolute = Path.Combine(dir, rel)
            }

    /// Conform every `.fsi` in a package manifest against its `.fs` companion for `target`.
    /// `Error` ONLY when the package is wholly un-checkable — a malformed/absent manifest;
    /// a per-file parse failure becomes a `ParseFailed` verdict instead.
    let checkManifest (target: string) (manifestPath: string) : Result<PackageOutcome, string> =
        match ReferencedProject.loadManifest manifestPath with
        | Error e -> Error e
        | Ok m ->
            let dir = Path.GetDirectoryName manifestPath

            // The impl candidate set: every `.fs` the manifest names for this target. A
            // `.fsi` pairs only with a `.fs` that is in it.
            let implFiles = ReferencedProject.resolveImpl target m |> List.distinct

            let sigFiles = ReferencedProject.resolveFiles target m

            let pairingKey = ReferencedProject.pairingKey m

            let declaredImplOnly = ReferencedProject.resolveImplOnly target m |> Set.ofList

            // A body declared contract-less publishes its own surface, so it is no pairing
            // candidate: it must not be married to a `.fsi` of the same key it does not implement.
            let pairCandidates = implFiles |> List.filter (declaredImplOnly.Contains >> not)

            // A later impl wins a key clash.
            let implByKey = pairCandidates |> List.map (fun f -> pairingKey f, f) |> Map.ofList

            let companionOf (fsiRel: string) : string option =
                Map.tryFind (pairingKey fsiRel) implByKey

            let declaredSigOnly = ReferencedProject.resolveSigOnly target m |> Set.ofList

            // The target's committed runtime asset and the names it publishes — a contract may
            // ship no `.fs` because its bodies live here. Only the FIRST asset counts.
            let runtimeAsset =
                match ReferencedProject.resolveRuntime target m with
                | rel :: _ ->
                    let abs = Path.Combine(dir, rel)

                    if File.Exists abs then
                        Some(Path.GetFileName rel, exportedNames (File.ReadAllText abs))
                    else
                        None
                | [] -> None

            // A companion-less `.fsi`, split on its own CONTENT: it owes a `.fs` unless EVERY
            // declaration is satisfied without one — an `extern` or transparent abbreviation
            // is, and a `val` is exactly when the committed runtime asset exports it.
            let unpaired (fsiRel: string) : PairOutcome =
                if declaredSigOnly.Contains fsiRel then
                    // A manifest `sig-only` declaration outranks the content split.
                    PairOutcome.SigOnly fsiRel
                else
                    match parseRel m.Name dir fsiRel with
                    | Error e -> PairOutcome.ParseFailed(fsiRel, e)
                    | Ok sigParsed ->
                        let decls =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf -> Conformance.summariseSig sigParsed.Lexed sf
                            | _ -> []

                        let vals =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf -> Conformance.summariseSigVals sigParsed.Lexed sf
                            | _ -> []

                        let externs =
                            decls
                            |> List.choose (fun d ->
                                match d.Shape with
                                | Conformance.SigShape.Extern
                                | Conformance.SigShape.ExternClass -> Some d.Name
                                | _ -> None
                            )

                        let bodiless =
                            decls
                            |> List.forall (fun d ->
                                match d.Shape with
                                | Conformance.SigShape.Extern
                                | Conformance.SigShape.ExternClass
                                | Conformance.SigShape.Abbrev -> true
                                | Conformance.SigShape.Enum
                                | Conformance.SigShape.Other _ -> false
                            )

                        let valNames = vals |> List.map (fun v -> v.Name)

                        if not bodiless then
                            PairOutcome.SigOnly fsiRel
                        elif List.isEmpty decls && List.isEmpty valNames then
                            // A contract that declares nothing states nothing.
                            PairOutcome.SigOnly fsiRel
                        elif List.isEmpty valNames then
                            PairOutcome.Unrepresentable(fsiRel, externs)
                        else
                            match runtimeAsset with
                            | Some(asset, exports) when valNames |> List.forall exports.Contains ->
                                PairOutcome.RuntimeServed(fsiRel, asset, valNames)
                            | _ -> PairOutcome.SigOnly fsiRel

            let outcome (fsiRel: string) : PairOutcome =
                match companionOf fsiRel with
                | None -> unpaired fsiRel
                | Some implRel ->
                    match parseRel m.Name dir fsiRel, parseRel m.Name dir implRel with
                    | Ok sigParsed, Ok implParsed ->
                        let sigDecls =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf -> Conformance.summariseSig sigParsed.Lexed sf
                            | _ -> []

                        let implDecls =
                            match implParsed.Ast with
                            | FSharpAst.ImplementationFile f -> Conformance.summariseImpl implParsed.Lexed f
                            | _ -> []

                        // Each empty for the wrong file kind.
                        let sigVals =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf -> Conformance.summariseSigVals sigParsed.Lexed sf
                            | _ -> []

                        let implVals =
                            match implParsed.Ast with
                            | FSharpAst.ImplementationFile f -> Conformance.summariseImplVals implParsed.Lexed f
                            | _ -> []

                        let sigPath = leadingDeclPath sigParsed.Lexed sigParsed.Ast
                        let implPath = leadingDeclPath implParsed.Lexed implParsed.Ast

                        let mismatch =
                            if sigPath = implPath then
                                None
                            else
                                Some
                                    {
                                        SigDecl = sigPath
                                        ImplDecl = implPath
                                    }

                        PairOutcome.Paired
                            {
                                SigFile = fsiRel
                                ImplFile = implRel
                                ModuleMismatch = mismatch
                                Errors =
                                    Conformance.check sigDecls implDecls
                                    @ Conformance.checkValuePresence sigVals implVals
                            }
                    | Error e, _
                    | _, Error e -> PairOutcome.ParseFailed(fsiRel, e)

            let pairs = sigFiles |> List.map outcome

            // `.fs` files with no `.fsi` contract — a body with no published surface.
            let contractKeys = sigFiles |> List.map pairingKey |> Set.ofList

            let implOnly =
                implFiles |> List.filter (fun f -> not (contractKeys.Contains(pairingKey f)))

            Ok
                {
                    Package = m.Name
                    Pairs = pairs
                    ImplOnly = implOnly
                    ImplOnlyDeclarations = declaredImplOnly
                    SigOnlyExemptions = declaredSigOnly
                }

    // ---- Enforcement: conformance findings become hard errors ------------------

    /// One `ConformanceVerdict` per finding, every one a `Severity.Error`. Empty = the
    /// package conforms.
    let enforce (outcome: PackageOutcome) : XParsec.FSharp.SemanticAnalysis.Diagnostic list =
        // A package-level verdict is about a signature, not a place in any one file.
        let err (verdict: ConformanceVerdict) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
            Diagnostic.nowhere (Kind.Conformance(outcome.Package, verdict))

        // The contract `.fsi` files that DID pair — the basis for catching a `sig-only`
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

        // `Unrepresentable` / `RuntimeServed` are only reached for a contract the manifest
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
                    | Some mm ->
                        yield
                            err (
                                ConformanceVerdict.ModulePairingMismatch(r.SigFile, r.ImplFile, mm.SigDecl, mm.ImplDecl)
                            )
                    | None -> ()
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

            for f in outcome.ImplOnly do
                if not (outcome.ImplOnlyDeclarations.Contains f) then
                    yield err (ConformanceVerdict.ImplWithoutContract f)

            // A declared contract-less body the impl set does not report as one: a name this
            // target does not compile, or one whose `.fsi` has since appeared.
            for d in outcome.ImplOnlyDeclarations do
                if not (List.contains d outcome.ImplOnly) then
                    yield err (ConformanceVerdict.UnknownImplOnly d)

            for ex in outcome.SigOnlyExemptions do
                if pairedSigs.Contains ex then
                    yield err (ConformanceVerdict.StaleSigOnly ex)
                elif not (sigOnlySigs.Contains ex) then
                    yield err (ConformanceVerdict.UnknownSigOnly ex)
        ]
