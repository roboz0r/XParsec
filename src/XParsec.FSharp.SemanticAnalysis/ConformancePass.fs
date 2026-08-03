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
//  - KEY: the manifest's own pairing stem (`ReferencedProject.pairingStem`), so
//    `prim-types-int.js.fs` pairs with `prim-types-int.fsi`.
//  - The impl candidate set for target T is `resolveImpl T` — a `.fsi` pairs only with a
//    `.fs` the manifest names for that target. A `.fsi` with no such `.fs` is impl-free
//    (an exemption candidate; Step 5 turns an un-exempted one into an FS0240-style hard
//    error).
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
        /// `.fsi` with no companion `.fs` for this target, and NOTHING in it that a `.fs`
        /// could supply: every declaration is an `extern` (whose whole body would be a repr
        /// binding) or a transparent abbreviation. The target represents these types
        /// nowhere, so the absent `.fs` is the STATEMENT of that — accepted here, and paid
        /// for at each use site instead (`nativeint is not supported on the js target`).
        ///
        /// Distinct from `SigOnly`, which is a manifest DECLARATION that a contract is
        /// impl-free on every target; this is derived from the file's own content, and it
        /// is what lets a target be conformance-checked with no hand-maintained exemption
        /// list. A contract carrying anything that needs a real body — a record, a union, a
        /// module-level `val` — is never this: a forgotten `.fs` must not read as "not
        /// supported".
        | Unrepresentable of sigFile: string * types: string list
        /// The `.fsi` or its companion `.fs` failed to parse, so the pair could not be
        /// conformed. Carried as a per-contract verdict (not an abort of the whole
        /// package) so one malformed file does not mask drift in the others; `enforce`
        /// turns it into a hard error.
        | ParseFailed of sigFile: string * detail: string

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
                Path = { BucketName = name; Relative = rel }
                Absolute = Path.Combine(dir, rel)
            }

    /// Conform every `.fsi` in a package manifest against its `.fs` companion for
    /// `target`. Returns `Error` ONLY when the package
    /// is wholly un-checkable — a malformed/absent manifest. A per-file parse failure is
    /// collected as a `PairOutcome.ParseFailed` verdict (so one bad file does not mask
    /// the others' drift); `enforce` promotes it to a hard error alongside the rest.
    let checkManifest (target: string) (manifestPath: string) : Result<PackageOutcome, string> =
        match ReferencedProject.loadManifest manifestPath with
        | Error e -> Error e
        | Ok m ->
            let dir = Path.GetDirectoryName manifestPath

            // The impl candidate set: every `.fs` the manifest names for this target. A
            // `.fsi` pairs only with a `.fs` that is in it.
            let implFiles = ReferencedProject.resolveImpl target m |> List.distinct

            let sigFiles = ReferencedProject.resolveFiles target m

            let stem = ReferencedProject.pairingStem m

            // The manifest's own pairing rule, so this pass checks the very pairs the
            // provider build extracts from. A later impl wins a stem clash.
            let implByStem = implFiles |> List.map (fun f -> stem f, f) |> Map.ofList

            let companionOf (fsiRel: string) : string option = Map.tryFind (stem fsiRel) implByStem

            let declaredSigOnly = ReferencedProject.resolveSigOnly target m |> Set.ofList

            // A companion-less `.fsi`, split on its own CONTENT. Everything an `extern`
            // promises is a repr binding the target does not make, and a transparent
            // abbreviation resolves through; anything else — a record, a union, a
            // module-level `val` — needs a real `.fs`, so its absence stays the FS0240
            // hard error rather than being read as "not supported here".
            let unpaired (fsiRel: string) : PairOutcome =
                if declaredSigOnly.Contains fsiRel then
                    // A manifest declaration of impl-free-on-every-target outranks the
                    // content split, so a declared exemption never silently re-labels
                    // itself and stops being checked as one.
                    PairOutcome.SigOnly fsiRel
                else
                    match parseRel m.Name dir fsiRel with
                    | Error e -> PairOutcome.ParseFailed(fsiRel, e)
                    | Ok sigParsed ->
                        let decls =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf -> Conformance.summariseSig sigParsed.Lexed sigParsed.Input sf
                            | _ -> []

                        let vals =
                            match sigParsed.Ast with
                            | FSharpAst.SignatureFile sf ->
                                Conformance.summariseSigVals sigParsed.Lexed sigParsed.Input sf
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

                        if List.isEmpty externs || not bodiless || not (List.isEmpty vals) then
                            PairOutcome.SigOnly fsiRel
                        else
                            PairOutcome.Unrepresentable(fsiRel, externs)

            let outcome (fsiRel: string) : PairOutcome =
                match companionOf fsiRel with
                | None -> unpaired fsiRel
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
                    | _, Error e -> PairOutcome.ParseFailed(fsiRel, e)

            let pairs = sigFiles |> List.map outcome

            // `.fs` files with no `.fsi` contract — a body with no published surface.
            let fsiStems = sigFiles |> List.map stem |> Set.ofList

            let implOnly = implFiles |> List.filter (fun f -> not (fsiStems.Contains(stem f)))

            // A per-file parse failure is a `PairOutcome.ParseFailed` verdict (surfaced
            // by `enforce`), NOT an `Error`: the pass still reports every other contract's
            // drift. `Error` is reserved for a failure that makes the whole package
            // un-checkable — a malformed/absent manifest (handled above).
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

    /// One `ConformanceVerdict` per finding; which `V24x` code each carries is
    /// `ConformanceVerdict.code`'s answer, not prose here.
    ///
    /// Empty = the package conforms; a non-empty result must fail the build.
    // `Diagnostic` is qualified throughout this pass rather than aliased; see the type's
    // declaration for why the bare name would otherwise be the parser's.
    let enforce (outcome: PackageOutcome) : XParsec.FSharp.SemanticAnalysis.Diagnostic list =
        // A package-level conformance verdict is about a signature, not a place in any one
        // file. The package rides on every one of them, so it is named here rather than at
        // the seven sites below.
        let err (verdict: ConformanceVerdict) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
            Diagnostic.nowhere (Kind.Conformance(outcome.Package, verdict))

        // The contract `.fsi` files actually present, split by pairing verdict — the
        // basis for catching a `sig-only` exemption that names a non-contract or a
        // file that in fact has a companion `.fs`.
        let pairedSigs =
            set
                [
                    for p in outcome.Pairs do
                        match p with
                        | PairOutcome.Paired r -> yield r.SigFile
                        | PairOutcome.SigOnly _
                        | PairOutcome.Unrepresentable _
                        | PairOutcome.ParseFailed _ -> ()
                ]

        // An `Unrepresentable` verdict is only ever reached for a contract the manifest
        // does NOT declare `sig-only`, so it can never be a declared exemption's file and
        // is excluded here rather than folded in.
        let sigOnlySigs =
            set
                [
                    for p in outcome.Pairs do
                        match p with
                        | PairOutcome.SigOnly s -> yield s
                        | PairOutcome.Paired _
                        | PairOutcome.Unrepresentable _
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
                // Declared, unrepresentable, ACCEPTED: the absent `.fs` is the statement
                // that this target represents none of these types, and the reject is owed
                // at the use site, not here.
                | PairOutcome.Unrepresentable _ -> ()
                | PairOutcome.ParseFailed(sigFile, detail) ->
                    yield err (ConformanceVerdict.PairParseFailure(sigFile, detail))

            for f in outcome.ImplOnly do
                yield err (ConformanceVerdict.ImplWithoutContract f)

            // A declared exemption is stale if its `.fsi` actually pairs with a `.fs`,
            // and unknown if it names no contract in the package at all — both keep the
            // single exemption source honest.
            for ex in outcome.SigOnlyExemptions do
                if pairedSigs.Contains ex then
                    yield err (ConformanceVerdict.StaleSigOnly ex)
                elif not (sigOnlySigs.Contains ex) then
                    yield err (ConformanceVerdict.UnknownSigOnly ex)
        ]
