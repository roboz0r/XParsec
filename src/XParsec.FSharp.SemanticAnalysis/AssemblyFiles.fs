namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// An assembly is a LINEAR composition of per-file provider views, ahead of the external
// (package/BCL) provider. Each file is parsed and analysed on its OWN Lexed/PassContext,
// so `NodeKey` offsets are per-file and nothing `NodeKey`-keyed is merged across files.

module AssemblyFiles =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// The identity every anchor and diagnostic of one file resolves against: the assembly
    /// it is bucketed under, and the path it is named by.
    let fileSource (assemblyName: string) (path: string) (lexed: Lexed) : OriginSource =
        Hashing.originSource
            {
                BucketName = assemblyName
                Relative = path
            }
            lexed

    /// One successfully analysed file of a multi-file assembly, carrying the provider view
    /// later files resolve its exports through.
    type FrozenFile =
        {
            Source: OriginSource
            /// What RECOVERY reported while parsing. Analysis runs regardless, so these
            /// ride alongside the analysis residue rather than short-circuiting the file.
            ParseDiagnostics: Diagnostic list
            Frozen: FrozenPools
            /// The provider this file WAS analysed against: prior files' views nearest-first
            /// over the external surface, under the file's own declared namespaces. Carried
            /// rather than re-derived, so a backend resolves an `External` node the same way.
            Scoped: IExternalSymbolProvider
            View: IExternalSymbolProvider
        }

    /// A file that never reached analysis: a lex/parse failure, surfaced as a file-level
    /// error rather than thrown. It contributes NO view, so later files simply compose over
    /// the ones that did parse.
    type UnparsedFile =
        {
            Path: string
            Failure: Pipeline.ParseFailure
        }

    /// A diagnostic anchored to the file it came from: its source path plus a (line, col)
    /// resolved against THAT file's own text. Token offsets are per-file, so a bare
    /// diagnostic is only ever resolved inside the file it was produced in.
    type AnchoredDiagnostic =
        {
            Path: string
            Diagnostic: Diagnostic
            Line: int
            Col: int
        }

    /// The per-file front-end seam: analyse+freeze one parsed file against a composed
    /// provider. Both the package/FSharp.Core front end and the BCL-only self-host one have
    /// this shape, so a multi-file assembly can be driven through either.
    type AnalyseFile =
        string -> IExternalSymbolProvider -> OriginSource -> ImplementationFile<SyntaxToken> -> FrozenPools

    /// The namespaces a file DECLARES. F# implicitly opens a file's own `namespace N` over
    /// its body, and that is what reaches a PRIOR file's namespace-direct declarations —
    /// including from the provider-layer probes that never see the file's local scope.
    let private declaredNamespaces (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : string list =
        let identText (tok: SyntaxToken) =
            match tok.Index with
            | TokenIndex.Regular iT -> lexed.GetTokenString(iT)
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

    /// Analyse a multi-file assembly in manifest order through a chosen front end. Each file
    /// resolves the ones BEFORE it — prior file views nearest-first, external last — so a
    /// name a nearer file re-declares shadows a farther one's.
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

        for (path, source) in files do
            match Pipeline.parse source with
            | Error f -> results.Add(Error { Path = path; Failure = f })
            | Ok parsed ->
                let composed =
                    ExternalSymbolProviders.composite ((List.rev priorViews) @ [ external ])

                // This file's own `namespace N` ahead of whatever prelude the external
                // surface carries, so a bare `bool` finds the `N.bool` an earlier file of
                // the SAME package declared.
                let scoped =
                    match declaredNamespaces parsed.Lexed parsed.File with
                    | [] -> composed
                    | ns ->
                        ExternalSymbolProviders.stack
                            ValueNone
                            (ns @ composed.AmbientOpenPrefixes |> List.distinct)
                            [ composed ]

                let origin = fileSource assemblyName path parsed.Lexed
                let frozen = analyse assemblyName scoped origin parsed.File
                let view = FrozenSignature.toProvider origin frozen

                // Later files resolve this file's exports through its view.
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

    /// Analyse a multi-file assembly through the default package/FSharp.Core front end.
    /// The self-host one is reached by passing it to `analyseAssemblyWith` directly.
    let analyseAssembly
        (assemblyName: string)
        (external: IExternalSymbolProvider)
        (files: (string * string) list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseAssemblyWith Pipeline.analyseFor assemblyName external files

    /// Diagnostics from a file that never reached analysis: it has no `Lexed`, so nothing
    /// resolves a token index against it and they render at line 1, col 1. A POSITIONED
    /// diagnostic here is unverifiable, so it faults rather than printing a plausible line.
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

    /// Anchor a file's bare diagnostics to its path and text: a `Site` names tokens of THIS
    /// file's `Lexed`, whose `StartIndex` is a char offset into `file.Input`, turned into a
    /// (line, col) by one `LineIndex`. `Site.Nowhere` renders at line 1, col 1.
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
    /// came AFTER lexing, and at line 1, col 1 when there is no stream to anchor against.
    let failureDiagnostics (e: UnparsedFile) : AnchoredDiagnostic list =
        match e.Failure.Lexed with
        // The `""` bucket: no file was analysed, so no assembly claims this one — the
        // source exists only to resolve the positions the parser's diagnostics carry.
        | ValueSome lexed -> anchorDiagnostics (fileSource "" e.Path lexed) e.Failure.Diagnostics
        | ValueNone -> unpositionedDiagnostics e.Path e.Failure.Diagnostics

    /// Every analysed file's diagnostics, each anchored to ITS OWN file. Recovery's findings
    /// come first: they are what the tree the analysis ran on was patched up from.
    let consolidatedDiagnostics (files: FrozenFile list) : AnchoredDiagnostic list =
        [
            for f in files do
                yield! anchorDiagnostics f.Source (f.ParseDiagnostics @ f.Frozen.Residue.Diagnostics)
        ]

    /// A whole assembly that passed the gate: its files in manifest order, plus their
    /// retained sources as one domain — a backend needs those to read the anchors of a node
    /// spliced out of a prior file.
    type AnalysedAssembly =
        {
            Files: FrozenFile list
            Origins: OriginSources
        }

    /// `analyseAssemblyWith`, GATED: every file must parse, and no analysed file may carry
    /// an error-severity diagnostic. A parse failure is fatal for the whole assembly and is
    /// reported alone — the files after it analysed against a truncated view.
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
