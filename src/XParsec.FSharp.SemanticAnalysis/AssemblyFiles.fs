namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The FRONT-END multi-file assembly pipeline: an assembly is a LINEAR composition of
// per-file provider views, ahead of the external (package/BCL) provider. Each file is
// parsed and analysed ON ITS OWN — its own Input/Lexed/Ast/PassContext — so `NodeKey`
// offsets are per-file and never collide across files. That is the whole point: nothing
// `NodeKey`-keyed is ever merged across files.
//
// For file N (manifest order):
//   1. Parse it (its own `Lexed` + `ImplementationFile`).
//   2. Analyse+freeze it against `composite(prior file views (nearest-first) ++ [external])`.
//   3. Project its inferred signature to a provider view (`FrozenSignature.toProvider`)
//      and push it for the files that follow.
//
// Cross-file same-assembly resolution works because a prior file's view stamps
// `Origin = InAssembly assemblyName` (the compilation's OWN name), and
// `TypeRegistration.diagnoseExternalClaim` treats a claim whose asm = the home assembly
// as deliberately NOT a clash — so a file-N home-stamped symbol resolves in file N+1.
//
// This is the FRONT END only: no codegen. It proves cross-file NAME RESOLUTION.

module AssemblyFiles =

    // The alias binds `Diagnostic` to the SemanticAnalysis one throughout this module; see
    // that type's declaration for why the bare name would otherwise be the parser's.
    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// A driver's `(path, source)` pair as the file every anchor resolves against. The
    /// file's diagnostics resolve against it, and so does every anchor its analysis mints —
    /// one value, so a path and a text a caller could transpose without the compiler
    /// noticing cannot be paired wrongly.
    let fileSource (assemblyName: string) (path: string) (input: string) (lexed: Lexed) : OriginSource =
        Hashing.originSource
            {
                BucketName = assemblyName
                Relative = path
            }
            input
            lexed

    /// One successfully analysed file of a multi-file assembly: what its diagnostics
    /// resolve against, the frozen tree, and the provider view later files resolve its
    /// exports through.
    type FrozenFile =
        {
            Source: OriginSource
            /// What RECOVERY reported while parsing this file. Analysis runs regardless —
            /// a recovered tree is still a tree — so these ride alongside the analysis
            /// residue rather than short-circuiting the file.
            ParseDiagnostics: Diagnostic list
            Frozen: FrozenPools
            /// The provider this file WAS analysed against: the prior files' views
            /// nearest-first over the external surface, under the file's own declared
            /// namespaces. A per-file backend emits against it, so an `External` node
            /// resolves at emission to the symbol the front end resolved it to — carried
            /// rather than re-derived, because a re-derivation is a second answer.
            Scoped: IExternalSymbolProvider
            View: IExternalSymbolProvider
        }

    /// A file that never reached analysis: a lex/parse failure (`Pipeline.parse`), surfaced
    /// as a file-level error rather than thrown. Such a file contributes NO view, so later
    /// files simply compose over the ones that did parse. The failure is carried as the
    /// parser seam produced it, so the "`Lexed` present iff lexing succeeded" invariant is
    /// stated once, on `ParseFailure`, rather than restated here.
    type UnparsedFile =
        {
            Path: string
            Input: string
            Failure: Pipeline.ParseFailure
        }

    /// A diagnostic anchored to the file it came from: its source path plus a (line, col)
    /// resolved against THAT file's own text. Offsets are per-file, so resolution happens
    /// within each file — never by flattening bare diagnostics across files.
    type AnchoredDiagnostic =
        {
            Path: string
            Diagnostic: Diagnostic
            Line: int
            Col: int
        }

    /// The per-file front-end seam: analyse+freeze one parsed file against a composed
    /// provider. `Pipeline.analyseFor` (a package/FSharp.Core consumer) and
    /// `Pipeline.analyseForSelfHost` (a BCL-only self-host package) both have this exact
    /// shape, so a multi-file assembly can be driven through either front end.
    type AnalyseFile =
        string -> IExternalSymbolProvider -> OriginSource -> ImplementationFile<SyntaxToken> -> FrozenPools

    /// The namespaces a file DECLARES. F# implicitly opens a file's own `namespace N` over
    /// its body, and a PRIOR file's namespace-direct declarations are reachable through
    /// that — including by the provider-layer probes (intrinsic resolution) that never see
    /// the file's local scope.
    let private declaredNamespaces
        (lexed: Lexed)
        (input: string)
        (file: ImplementationFile<SyntaxToken>)
        : string list =
        let identText (tok: SyntaxToken) =
            match tok.Index with
            | TokenIndex.Regular iT -> lexed.GetTokenString(iT, input)
            | TokenIndex.Virtual -> ""

        match file with
        | ImplementationFile.Namespaces groups ->
            [
                for g in groups do
                    match g with
                    | NamespaceDeclGroup.Named(longIdent = li) ->
                        let path = li.Idents |> Seq.map identText |> String.concat "."

                        if path.Length > 0 then
                            yield path
                    | NamespaceDeclGroup.Global _ -> ()
            ]
            |> List.distinct
        | _ -> []

    /// Analyse a multi-file assembly in manifest order through a chosen front end. Each
    /// file resolves the ones BEFORE it — the prior file views composed nearest-first,
    /// then the external provider last — so a name a nearer file re-declares shadows a
    /// farther one's, and the external surface is the final fallback. Returns one `Result`
    /// per file, in order: `Ok` for an analysed file (carrying its view), `Error` for a
    /// parse failure. A failed file contributes no view; the files after it compose over
    /// the survivors.
    let analyseAssemblyWith
        (analyse: AnalyseFile)
        (assemblyName: string)
        (external: IExternalSymbolProvider)
        (files: (string * string) list)
        : Result<FrozenFile, UnparsedFile> list =
        // Prior file views in FILE ORDER (oldest first); the newest is at the head after
        // each push, so `List.rev` before composing puts the NEAREST file first.
        let mutable priorViews: IExternalSymbolProvider list = []
        let results = ResizeArray<Result<FrozenFile, UnparsedFile>>()

        for (path, raw) in files do
            // A driver hands over bytes it read; the manifest extractor reads the SAME files
            // for a self-host package's splice templates. One text per file, or the two
            // retentions of it disagree.
            let source = SourceText.normalise raw

            match Pipeline.parse source with
            | Error f ->
                results.Add(
                    Error
                        {
                            Path = path
                            Input = source
                            Failure = f
                        }
                )
            | Ok parsed ->
                // Nearest prior file first, external last.
                let composed =
                    ExternalSymbolProviders.composite ((List.rev priorViews) @ [ external ])

                // This file's own `namespace N` ahead of whatever prelude the external
                // surface already carries, so `N.bool` declared by a prior file answers a
                // bare `bool`. Without it a self-host package's operator bodies cannot
                // name a primitive an earlier file of the SAME package declares.
                let scoped =
                    match declaredNamespaces parsed.Lexed source parsed.File with
                    | [] -> composed
                    | ns ->
                        ExternalSymbolProviders.stack
                            ValueNone
                            (ns @ composed.AmbientOpenPrefixes |> List.distinct)
                            [ composed ]

                let origin = fileSource assemblyName path source parsed.Lexed
                let frozen = analyse assemblyName scoped origin parsed.File
                let view = FrozenSignature.toProvider origin frozen

                // Push this file's view so LATER files can resolve its exports. It rides
                // at the head, so it composes NEAREST for the immediately-following file.
                priorViews <- view :: priorViews

                results.Add(
                    Ok
                        {
                            Source = origin
                            ParseDiagnostics = parsed.Diagnostics
                            Frozen = frozen
                            Scoped = scoped
                            View = view
                        }
                )

        List.ofSeq results

    /// Analyse a multi-file assembly through the default (package/FSharp.Core consumer)
    /// front end, `Pipeline.analyseFor`. The self-host front end is reached by passing
    /// `Pipeline.analyseForSelfHost` to `analyseAssemblyWith` directly.
    let analyseAssembly
        (assemblyName: string)
        (external: IExternalSymbolProvider)
        (files: (string * string) list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseAssemblyWith Pipeline.analyseFor assemblyName external files

    /// Diagnostics from a file that never reached analysis: it has no `Lexed`, so no token
    /// index could be resolved against it — and a whole-file lex/parse failure names no
    /// place in the file anyway. They render at the file head.
    ///
    /// A POSITIONED diagnostic here is a contradiction, not a case to render at (1, 1):
    /// something resolved a token of a file whose token stream this function cannot see, so
    /// the position it carries is unverifiable. Fault rather than print a plausible line.
    let unpositionedDiagnostics (path: string) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
        [
            for d in diagnostics do
                match d.Site with
                | Site.Nowhere ->
                    {
                        Path = path
                        Diagnostic = d
                        Line = 1
                        Col = 1
                    }
                | positioned ->
                    failwithf
                        "AssemblyFiles.unpositionedDiagnostics: %s produced no `Lexed`, so a diagnostic cannot carry a position — got %A (%s)"
                        path
                        positioned
                        d.Message
        ]

    /// Anchor a file's bare diagnostics to a `path` + its own `source`: a `Site` names
    /// tokens of THIS file's `Lexed`, whose `StartIndex` is the char offset resolved
    /// against THAT text via `XParsec`'s canonical `LineIndex` (the same resolver
    /// `Debug.fs` uses), built ONCE. `Lexed.GetLineForToken` alone will not do — it yields
    /// a line, and a column still needs the offset. `Site.Nowhere` renders at the file
    /// head. Token indices are per-file, so this is only ever called with a diagnostic and
    /// the file it was produced in — bare diagnostics are never flattened across files and
    /// resolved later.
    let anchorDiagnostics (file: OriginSource) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
        let lexed = file.Lexed
        let source = file.Input
        let lineIndex = XParsec.LineIndex.OfString source

        // The gap after the LAST token is the end of the file; every other token's gap is
        // where the next one starts.
        let gapAfter (t: int<token>) =
            let next = t + 1<token>

            if int next < lexed.Tokens.Length then
                lexed.Tokens[next].StartIndex
            else
                source.Length

        [
            for d in diagnostics do
                let struct (line, col) =
                    match d.Site with
                    | Site.Nowhere -> struct (1, 1)
                    | Site.At t
                    | Site.Between(first = t) -> lineIndex.GetLineCol lexed.Tokens[t].StartIndex
                    | Site.After t -> lineIndex.GetLineCol(gapAfter t)

                {
                    Path = file.File.Path.Relative
                    Diagnostic = d
                    Line = line
                    Col = col
                }
        ]

    /// A failed file's diagnostics, anchored against its own token stream when the failure
    /// came AFTER lexing, and at the file head when there is no stream to anchor against.
    let failureDiagnostics (e: UnparsedFile) : AnchoredDiagnostic list =
        match e.Failure.Lexed with
        // No file was analysed, so no assembly claims this one; the source exists only to
        // resolve the positions the parser's own diagnostics carry.
        | ValueSome lexed -> anchorDiagnostics (fileSource "" e.Path e.Input lexed) e.Failure.Diagnostics
        | ValueNone -> unpositionedDiagnostics e.Path e.Failure.Diagnostics

    /// Every analysed file's diagnostics, each anchored to ITS OWN file (path + source).
    /// Recovery's findings come first: they are what the tree the analysis ran on was
    /// patched up from, so they precede anything the analysis then concluded about it.
    let consolidatedDiagnostics (files: FrozenFile list) : AnchoredDiagnostic list =
        [
            for f in files do
                yield! anchorDiagnostics f.Source (f.ParseDiagnostics @ f.Frozen.Residue.Diagnostics)
        ]

    /// A whole assembly that passed the gate: its files in manifest order, plus every one
    /// of their retained sources as one domain. The retention travels with the files
    /// because a backend needs it to read the anchors of a node spliced out of a prior
    /// file, and re-collecting it would give a second answer for what each file contains.
    type AnalysedAssembly =
        {
            Files: FrozenFile list
            Origins: OriginSources
        }

    /// `analyseAssemblyWith`, GATED: every file must parse, and no analysed file may carry
    /// an error-severity diagnostic. `Ok` is the analysed assembly; `Error` is every
    /// blocking diagnostic anchored to its own file (path + in-file line/col) rather than
    /// thrown. A parse failure is fatal for the whole assembly, and is reported alone —
    /// the files after it analysed against a truncated view, so their findings would be
    /// noise.
    ///
    /// THE gate both backends' drivers run, so "this package did not type-check" cannot
    /// come to mean two different things per target.
    let analyseGated
        (analyse: AnalyseFile)
        (assemblyName: string)
        (external: IExternalSymbolProvider)
        (files: (string * string) list)
        : Result<AnalysedAssembly, AnchoredDiagnostic list> =
        let results = analyseAssemblyWith analyse assemblyName external files

        let parseFailures =
            results
            |> List.collect (
                function
                | Error e -> failureDiagnostics e
                | Ok _ -> []
            )

        match parseFailures with
        | _ :: _ -> Error parseFailures
        | [] ->
            let analysed =
                results
                |> List.choose (
                    function
                    | Ok f -> Some f
                    | Error _ -> None
                )

            match
                consolidatedDiagnostics analysed
                |> List.filter (fun a -> a.Diagnostic.Severity = Severity.Error)
            with
            | _ :: _ as errors -> Error errors
            | [] ->
                Ok
                    {
                        Files = analysed
                        Origins = analysed |> List.map (fun f -> f.Source) |> OriginSources.ofSeq
                    }
