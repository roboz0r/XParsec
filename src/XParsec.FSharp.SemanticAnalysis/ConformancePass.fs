namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Manifest-driven `.fsi`↔`.fs` conformance (T8 Step 3).
//
// `Conformance.fs` is the pure CST-comparison KERNEL — given a parsed `.fsi`/`.fs`
// pair it reports presence + extern↔intrinsic drift. It knows nothing about where
// the files come from. This module is the PASS around it: it derives the pairs
// from a package `manifest.toml` (the single source of truth for which files a
// package ships), so a new `.fsi`/`.fs` is picked up automatically and can no
// longer be silently omitted from a hand-maintained list.
//
// Pairing follows the F#-faithful rules (see the T8 plan "Name resolution"):
//  - KEY: filename stem (`foo.fsi` ↔ `foo.fs`), with the per-target `.<t>.fs`
//    override preferred over the base `.fs` (mirrors
//    `ReferencedProject.targetOverrideFs`).
//  - The impl candidate set for target T is `resolveImpl T ∪ resolveInlineBodies T`
//    — a `.fsi` pairs only with a `.fs` the manifest actually compiles/splices for
//    that target. A `.fsi` with no such `.fs` is impl-free (an exemption candidate;
//    Step 5 turns an un-exempted one into an FS0240-style hard error).
//  - GUARD: a paired `.fsi`/`.fs` must agree on their leading `module`/`namespace`
//    declaration — what FS0240's message is really about (F# correlates files by
//    `QualifiedNameOfFile`). A disagreement means the stem rule paired two unrelated
//    files.
//
// This stays a source-level check (CST presence + pairing), so it runs the moment
// the files parse; value-level (member-signature / typar-order) conformance is
// T8 Step 4.

module ConformancePass =

    /// The leading `module`/`namespace` declarations of a paired `.fsi`/`.fs`
    /// disagree — the stem rule paired two files F# would not consider a pair.
    [<Struct; NoEquality; NoComparison>]
    type ModuleDeclMismatch = { SigDecl: string; ImplDecl: string }

    /// A `.fsi` contract paired with its `.fs` implementation (the stem rule found a
    /// companion in the manifest's impl set), plus the conformance verdict.
    [<NoEquality; NoComparison>]
    type PairResult =
        {
            /// `.fsi` relative path (as listed in the manifest).
            SigFile: string
            /// The companion `.fs` relative path (resolved from the impl set).
            ImplFile: string
            /// `Some` when the leading module/namespace declarations disagree.
            ModuleMismatch: ModuleDeclMismatch option
            /// Kernel conformance errors (empty = the pair conforms).
            Errors: Conformance.ConformanceError list
        }

    /// The verdict for one `.fsi` contract in a package.
    [<RequireQualifiedAccess>]
    type PairOutcome =
        /// `.fsi` with a companion `.fs` in the impl set.
        | Paired of PairResult
        /// `.fsi` with NO companion `.fs` in the impl set — impl-free.
        | SigOnly of sigFile: string

    /// The conformance outcome for a whole package, derived from its manifest.
    [<NoEquality; NoComparison>]
    type PackageOutcome =
        {
            /// Package / assembly simple name (`Manifest.Name`).
            Package: string
            /// One outcome per `.fsi` contract, in manifest `files` order.
            Pairs: PairOutcome list
            /// `.fs` files in the impl set whose stem has no `.fsi` contract — a
            /// body with no published surface (rare; usually empty).
            ImplOnly: string list
            /// The `.fsi` files the manifest declares DELIBERATELY impl-free for this
            /// target (`[core] sig-only`). `enforce` treats a `SigOnly` contract in
            /// this set as an accepted exemption; one outside it is the FS0240 hard
            /// error (T8 Step 5).
            SigOnlyExemptions: Set<string>
        }

    let private identText (lexed: Lexed) (input: string) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT, input)
        | TokenIndex.Virtual -> ""

    let private longIdentText (lexed: Lexed) (input: string) (li: LongIdent<SyntaxToken>) : string =
        li.Idents |> Seq.map (identText lexed input) |> String.concat "."

    /// The dotted leading `module`/`namespace` path of a parsed file — the basis of
    /// F#'s `QualifiedNameOfFile` pairing key. `"global"` for an explicit
    /// `namespace global`; `""` for an anonymous module (no declaration).
    let private leadingDeclPath (lexed: Lexed) (input: string) (ast: FSharpAst<SyntaxToken>) : string =
        match ast with
        | FSharpAst.SignatureFile sf ->
            match sf with
            | SignatureFile.Namespaces groups when groups.Length > 0 ->
                match groups.[0] with
                | NamespaceDeclGroupSignature.Named(longIdent = li) -> longIdentText lexed input li
                | NamespaceDeclGroupSignature.Global _ -> "global"
            | SignatureFile.Namespaces _ -> ""
            | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(longIdent = li)) ->
                longIdentText lexed input li
            | SignatureFile.AnonymousModule _ -> ""
        | FSharpAst.ImplementationFile f ->
            match f with
            | ImplementationFile.Namespaces groups when groups.Length > 0 ->
                match groups.[0] with
                | NamespaceDeclGroup.Named(longIdent = li) -> longIdentText lexed input li
                | NamespaceDeclGroup.Global _ -> "global"
            | ImplementationFile.Namespaces _ -> ""
            | ImplementationFile.NamedModule(NamedModule.NamedModule(longIdent = li)) -> longIdentText lexed input li
            | ImplementationFile.AnonymousModule _ -> ""
        | _ -> ""

    let private parseRel (name: string) (dir: string) (rel: string) : Result<VesperLib.ParsedFile, string> =
        VesperLib.parseFileFull
            {
                BucketName = name
                Relative = rel
                Absolute = Path.Combine(dir, rel)
            }

    /// Conform every `.fsi` in a package manifest against its `.fs` companion for
    /// `target` (the base/CLR pairing is `None`). Returns `Error` on a manifest or
    /// parse failure; otherwise the per-contract outcomes.
    let checkManifest (target: string option) (manifestPath: string) : Result<PackageOutcome, string> =
        match ReferencedProject.loadManifest manifestPath with
        | Error e -> Error e
        | Ok m ->
            let dir = Path.GetDirectoryName manifestPath

            // The impl candidate set: everything the manifest compiles or splices for
            // this target. A `.fsi` pairs only with a `.fs` that is actually in it.
            let implFiles =
                ReferencedProject.resolveImpl target m
                @ ReferencedProject.resolveInlineBodies target m
                |> List.distinct

            let implSet = Set.ofList implFiles

            let sigFiles = m.Files @ ReferencedProject.resolveExtraFiles target m

            // `foo.fsi` → its companion `.fs` in the impl set: prefer the per-target
            // `foo.<t>.fs` override, else the base `foo.fs`; `None` = impl-free.
            let companionOf (fsiRel: string) : string option =
                let stem = fsiRel.Substring(0, fsiRel.Length - 4) // strip ".fsi"

                let candidates =
                    match target with
                    | Some t -> [ stem + "." + t + ".fs"; stem + ".fs" ]
                    | None -> [ stem + ".fs" ]

                candidates |> List.tryFind implSet.Contains

            let mutable firstError = None

            let outcome (fsiRel: string) : PairOutcome =
                match companionOf fsiRel with
                | None -> PairOutcome.SigOnly fsiRel
                | Some implRel ->
                    match parseRel m.Name dir fsiRel, parseRel m.Name dir implRel with
                    | Ok sigParsed, Ok implParsed ->
                        let sigDecls =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf -> Conformance.summariseSig sigParsed.Lexed sigParsed.Input sf
                            | _ -> []

                        let implDecls =
                            match implParsed.Ast with
                            | FSharpAst.ImplementationFile f ->
                                Conformance.summariseImpl implParsed.Lexed implParsed.Input f
                            | _ -> []

                        // Value-binding presence (Step 4.1): the `.fsi` `val`s and the
                        // `.fs` `let`s, each empty for the wrong file kind.
                        let sigVals =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf ->
                                Conformance.summariseSigVals sigParsed.Lexed sigParsed.Input sf
                            | _ -> []

                        let implVals =
                            match implParsed.Ast with
                            | FSharpAst.ImplementationFile f ->
                                Conformance.summariseImplVals implParsed.Lexed implParsed.Input f
                            | _ -> []

                        let sigPath = leadingDeclPath sigParsed.Lexed sigParsed.Input sigParsed.Ast
                        let implPath = leadingDeclPath implParsed.Lexed implParsed.Input implParsed.Ast

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
                                // Type-presence/extern findings first, then value-presence.
                                Errors =
                                    Conformance.check sigDecls implDecls
                                    @ Conformance.checkValuePresence sigVals implVals
                            }
                    | Error e, _
                    | _, Error e ->
                        if firstError.IsNone then
                            firstError <- Some(sprintf "%s: %s" fsiRel e)
                        // A placeholder; the whole result is discarded once firstError fires.
                        PairOutcome.SigOnly fsiRel

            let pairs = sigFiles |> List.map outcome

            // `.fs` files with no `.fsi` contract. Strip `.fs`, then a trailing
            // `.<target>` segment (`ops-platform.js.fs` → `ops-platform`), and compare
            // against the contract stems.
            let fsiStems =
                sigFiles |> List.map (fun s -> s.Substring(0, s.Length - 4)) |> Set.ofList

            let implStem (implRel: string) =
                let noFs =
                    if implRel.EndsWith ".fs" then
                        implRel.Substring(0, implRel.Length - 3)
                    else
                        implRel

                match target with
                | Some t when noFs.EndsWith("." + t) -> noFs.Substring(0, noFs.Length - t.Length - 1)
                | _ -> noFs

            let implOnly =
                implFiles |> List.filter (fun f -> not (fsiStems.Contains(implStem f)))

            match firstError with
            | Some e -> Error e
            | None ->
                Ok
                    {
                        Package = m.Name
                        Pairs = pairs
                        ImplOnly = implOnly
                        SigOnlyExemptions = ReferencedProject.resolveSigOnly target m |> Set.ofList
                    }

    // ---- Enforcement: conformance findings become hard errors (T8 Step 5) -------
    //
    // The pass used to be diagnostic-only: a test compared `pairErrors` against an
    // `acceptedFindings` golden. `enforce` flips that — every discrepancy is now a
    // hard `Severity.Error` diagnostic (F#'s FS0240 family), so a package build that
    // runs it FAILS rather than silently shipping a degraded DLL with a codegen
    // substitution standing in for a missing `.fs`. The exemption list is no longer
    // a test-side constant: it is the manifest's `[core] sig-only`, so a `.fsi` whose
    // `.fs` was deleted (and which is not declared impl-free) is an FS0240 hard error
    // by construction.

    /// `V240` — the FS0240 family: a sig binding with no impl (`MissingInImpl` /
    /// `ValueMissingInImpl`), an extern/intrinsic drift, or an un-exempted `SigOnly`
    /// contract. `V241` — a leading-module/namespace pairing disagreement. `V242` — a
    /// compiled `.fs` with no `.fsi` contract. `V243` — a stale/unknown `sig-only`
    /// exemption (declared impl-free but a companion `.fs` exists, or the named `.fsi`
    /// is not a contract at all).
    ///
    /// Empty = the package conforms; a non-empty result must fail the build.
    // `Diagnostic` is qualified throughout: `open XParsec.FSharp.Parser` brings the
    // PARSER's `Diagnostic` (Token/DiagnosticCode/DiagnosticSeverity) into scope, which
    // shadows the SemanticAnalysis one this pass emits.
    let enforce (outcome: PackageOutcome) : XParsec.FSharp.SemanticAnalysis.Diagnostic list =
        let err (code: string) (message: string) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
            {
                Key = NodeKey(0UL)
                Code = code
                Message = sprintf "%s: %s" outcome.Package message
                Severity = Severity.Error
            }

        // The contract `.fsi` files actually present, split by pairing verdict — the
        // basis for catching a `sig-only` exemption that names a non-contract or a
        // file that in fact has a companion `.fs`.
        let pairedSigs =
            set
                [
                    for p in outcome.Pairs do
                        match p with
                        | PairOutcome.Paired r -> yield r.SigFile
                        | PairOutcome.SigOnly _ -> ()
                ]

        let sigOnlySigs =
            set
                [
                    for p in outcome.Pairs do
                        match p with
                        | PairOutcome.SigOnly s -> yield s
                        | PairOutcome.Paired _ -> ()
                ]

        [
            for p in outcome.Pairs do
                match p with
                | PairOutcome.Paired r ->
                    for e in r.Errors do
                        yield err "V240" (sprintf "%s: %s" r.SigFile (Conformance.describe e))

                    match r.ModuleMismatch with
                    | Some mm ->
                        yield
                            err
                                "V241"
                                (sprintf
                                    "%s ↔ %s: the paired files' leading module/namespace declarations disagree ('%s' vs '%s')"
                                    r.SigFile
                                    r.ImplFile
                                    mm.SigDecl
                                    mm.ImplDecl)
                    | None -> ()
                | PairOutcome.SigOnly s ->
                    if not (outcome.SigOnlyExemptions.Contains s) then
                        yield
                            err
                                "V240"
                                (sprintf
                                    "the signature file '%s' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                                    s)

            for f in outcome.ImplOnly do
                yield err "V242" (sprintf "the implementation file '%s' has no '.fsi' contract" f)

            // A declared exemption is stale if its `.fsi` actually pairs with a `.fs`,
            // and unknown if it names no contract in the package at all — both keep the
            // single exemption source honest.
            for ex in outcome.SigOnlyExemptions do
                if pairedSigs.Contains ex then
                    yield
                        err
                            "V243"
                            (sprintf
                                "'%s' is declared `sig-only` but a companion implementation exists — remove the stale exemption"
                                ex)
                elif not (sigOnlySigs.Contains ex) then
                    yield
                        err "V243" (sprintf "`sig-only` names '%s', which is not a contract `.fsi` in this package" ex)
        ]
