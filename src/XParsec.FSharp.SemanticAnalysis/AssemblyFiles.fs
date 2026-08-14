namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// An assembly is a LINEAR composition of per-file provider views, ahead of the external
// (package/BCL) provider. Each file is parsed and analysed on its OWN Lexed/PassContext,
// so `NodeKey` offsets are per-file and nothing `NodeKey`-keyed is merged across files.
//
// A file publishes SIGNATURES and BODIES as two objects. Its `.fsi`, when it has one,
// replaces the signatures and leaves the bodies alone, so a declaration the signature does
// not make is invisible to every later file while a `member inline` beside it still splices.

module AssemblyFiles =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// What one run of files compiles into: the assembly its local keys are homed in, and the
    /// target whose platform reprs a signature's `type t = extern` resolves against.
    type CompilingAssembly = { Name: string; Target: string }

    /// The identity every anchor and diagnostic of one file resolves against: the assembly
    /// it is bucketed under, and the name it is known by within it.
    let fileSource (assemblyName: string) (id: AssemblyFileId) (lexed: Lexed) : OriginSource =
        Hashing.originSource
            {
                BucketName = assemblyName
                Relative = id
            }
            lexed

    /// One INPUT file of an assembly: its source text, and the name it is known by within that
    /// assembly, which anchors its diagnostics and keys the frozen cache. Nothing reopens `Id`.
    type SourceFile = { Id: AssemblyFileId; Text: string }

    [<RequireQualifiedAccess>]
    module SourceFile =

        /// Read `relative` from beneath `root`, named as the filesystem has it: `root` locates
        /// the bytes and is then discarded.
        let read (root: string) (relative: string) : SourceFile =
            {
                Id = AssemblyFileId.ofPathUnder root relative
                Text = System.IO.File.ReadAllText(System.IO.Path.Combine(root, relative))
            }

        /// A source handed over as TEXT, named by `id` alone: a driver given a string, a test.
        /// `id` is spelled like the relative path it stands in for, `math/z.fs`.
        let ofText (id: string) (text: string) : SourceFile =
            {
                Id = AssemblyFileId.ofRelative id
                Text = text
            }

    /// One compilation unit of an assembly: the implementation compiled into it, and the
    /// signature that publishes it. Without a signature the implementation publishes the
    /// surface it infers, which is the case every file is in until a `.fsi` is written.
    type SourceUnit =
        {
            Signature: SourceFile voption
            Implementation: SourceFile
        }

    [<RequireQualifiedAccess>]
    module SourceUnit =

        let ofImplementation (implementation: SourceFile) : SourceUnit =
            {
                Signature = ValueNone
                Implementation = implementation
            }

        let paired (signature: SourceFile) (implementation: SourceFile) : SourceUnit =
            {
                Signature = ValueSome signature
                Implementation = implementation
            }

    /// The `.fsi` half of an analysed unit. What it PUBLISHES has already been folded into the
    /// file's `View`; what is left here is everything anchored to the signature's own text.
    type FrozenSignatureFile =
        {
            Source: OriginSource
            /// What RECOVERY reported parsing the signature.
            ParseDiagnostics: Diagnostic list
            /// Where the implementation failed to answer the signature, and where the
            /// signature declared something extraction could not publish.
            Diagnostics: Diagnostic list
        }

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
            /// What LATER files resolve this file through: its signatures, replaced by the
            /// `.fsi`'s when it has one, with its own splice templates layered back on.
            View: IExternalSymbolProvider
            Signature: FrozenSignatureFile voption
        }

    /// A file that never reached analysis: a lex/parse failure, surfaced as a file-level
    /// error rather than thrown. It contributes NO view, so later files simply compose over
    /// the ones that did parse.
    type UnparsedFile =
        {
            Id: AssemblyFileId
            Failure: Pipeline.ParseFailure
        }

    /// A diagnostic anchored to the file it came from: its source path plus a (line, col)
    /// resolved against THAT file's own text. Token offsets are per-file, so a bare
    /// diagnostic is only ever resolved inside the file it was produced in.
    type AnchoredDiagnostic =
        {
            Path: AssemblyFileId
            Diagnostic: Diagnostic
            Line: int
            Col: int
        }

    /// The per-file front-end seam: analyse+freeze one parsed file against a composed
    /// provider. A seam so a probe can wrap it and time each file.
    type AnalyseFile =
        string -> IExternalSymbolProvider -> OriginSource -> ImplementationFile<SyntaxToken> -> FrozenPools

    /// The namespaces a file DECLARES. F# implicitly opens a file's own `namespace N` over
    /// its body, and that is what reaches a PRIOR file's namespace-direct declarations,
    /// including from the provider-layer probes that never see the file's local scope.
    let private declaredNamespaces (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : string list =
        let identText (tok: SyntaxToken) =
            match tok.Index with
            | TokenIndex.Regular iT -> lexed.GetTokenName(iT)
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

    /// One parsed half of a unit: its tree, and the name within the assembly its own
    /// diagnostics anchor to, which no tree carries.
    type private ParsedHalf<'tree> = { Id: AssemblyFileId; Parsed: 'tree }

    /// Both of a unit's trees, parsed before either is analysed.
    type private ParsedUnit =
        {
            Implementation: ParsedHalf<Pipeline.ParsedFile>
            Signature: ParsedHalf<Pipeline.ParsedSignature> voption
        }

    /// Parse both halves, each as leniently as the other: a tree the parser had to RECOVER is
    /// carried with its findings, which the gate refuses later. Only a half that yields NO tree
    /// fails the unit, and a signature failing that way fails it whole — it says nothing
    /// trustworthy about what its companion publishes, so falling back on the implementation's
    /// own inferred surface would publish more than the unit ever claimed.
    let private parseUnit (unit: SourceUnit) : Result<ParsedUnit, UnparsedFile> =
        match Pipeline.parse unit.Implementation.Text with
        | Error f ->
            Error
                {
                    Id = unit.Implementation.Id
                    Failure = f
                }
        | Ok parsed ->
            let implementation =
                {
                    Id = unit.Implementation.Id
                    Parsed = parsed
                }

            match unit.Signature with
            | ValueNone ->
                Ok
                    {
                        Implementation = implementation
                        Signature = ValueNone
                    }
            | ValueSome signature ->
                match Pipeline.parseSignature signature.Text with
                | Error f -> Error { Id = signature.Id; Failure = f }
                | Ok parsed ->
                    Ok
                        {
                            Implementation = implementation
                            Signature = ValueSome { Id = signature.Id; Parsed = parsed }
                        }

    /// What one `.fsi` is extracted AGAINST: the surface the files before it published, so it
    /// can name their types, and the sibling `.fs`, whose `(# … #)` bindings are where a
    /// `type t = extern` gets a repr the signature itself never states.
    type private SignatureScope =
        {
            Target: string
            Visible: IExternalSymbolProvider
            Implementation: VesperLibManifest.ParsedFile
        }

    /// Extract ONE in-assembly `.fsi` through the contract extractor, homed in the file rather
    /// than in an assembly, since a later file of the same assembly resolves it as a local.
    /// Publishes no ambient prefixes, matching what an implementation's own view publishes.
    ///
    /// Homed at the IMPLEMENTATION, not at the signature that declared it: a home names where
    /// a symbol physically lives, and what a backend emits for this unit is compiled from the
    /// `.fs`. `source` is still the signature's own, so extraction's losses anchor to the text
    /// that made the claim.
    let private signatureView
        (scope: SignatureScope)
        (source: OriginSource)
        (parsed: Pipeline.ParsedSignature)
        : IExternalSymbolProvider * Diagnostic list =
        let ctx = VesperLib.ExtractCtx.empty scope.Target
        ctx.AmbientShapes <- (fun name -> scope.Visible.TryLookupType name |> ExternalSymbols.typeShapeOf)
        ctx.DependencyAmbientPrefixes <- scope.Visible.AmbientOpenPrefixes

        // The `.fs` binds the reprs, so its pre-scan runs first and the `.fsi`'s
        // `type t = extern` picks a repr over `Unsupported`.
        VesperLib.extractIntrinsicReprsInto ctx.IntrinsicReprs scope.Implementation

        VesperLib.extractSymbols
            ctx
            {
                File = source.File.Path
                Lexed = parsed.Lexed
                Ast = FSharpAst.SignatureFile parsed.File
            }

        // A declaration extraction dropped is one the signature promised and no later file
        // can reach, so it is reported rather than left to surface as an unresolved name.
        // The two halves say DIFFERENT things: `Skipped` is a gap in what this compiler
        // models, `Diagnostics` a rule the signature broke.
        let dropped =
            [
                for (_, detail) in ctx.Skipped ->
                    Diagnostic.nowhere (
                        Kind.Conformance(source.File.Path.BucketName, ConformanceVerdict.SignatureNotExtracted detail)
                    )

                for (_, detail) in ctx.Diagnostics ->
                    Diagnostic.nowhere (
                        Kind.Conformance(source.File.Path.BucketName, ConformanceVerdict.SignatureRejected detail)
                    )
            ]

        ExternalSymbolProviders.stack
            (ValueSome(Origin.InFile scope.Implementation.File))
            []
            [ VesperLib.ExtractCtx.toProvider ctx ],
        dropped

    /// The implementation checked against what its signature publishes: `Conformance.checkUnit`,
    /// the CST rule set a manifest-paired unit is held to as well, then typar ORDER over the
    /// two frozen surfaces. The typar half is reachable only here — the package route conforms
    /// a manifest without freezing anything, so it has no inferred scheme to compare.
    /// A `let inline` is exempt from it by construction, on both routes.
    let private conformanceDiagnostics
        (assembly: string)
        (signature: ParsedHalf<Pipeline.ParsedSignature>)
        (implementation: ParsedHalf<Pipeline.ParsedFile>)
        (published: IExternalSymbolProvider)
        (frozen: FrozenPools)
        : Diagnostic list =
        let verdict (v: ConformanceVerdict) =
            Diagnostic.nowhere (Kind.Conformance(assembly, v))

        let unimplemented (detail: string) =
            verdict (ConformanceVerdict.Unimplemented(signature.Id.Name, detail))

        let pair =
            Conformance.checkUnit
                signature.Parsed.Lexed
                signature.Parsed.File
                implementation.Parsed.Lexed
                implementation.Parsed.File

        [
            match pair.ModuleMismatch with
            | ValueSome mm ->
                yield
                    verdict (
                        ConformanceVerdict.ModulePairingMismatch(
                            signature.Id.Name,
                            implementation.Id.Name,
                            mm.SigDecl,
                            mm.ImplDecl
                        )
                    )
            | ValueNone -> ()

            for e in pair.Errors do
                yield unimplemented (Conformance.describe e)

            for m in ConformanceTypars.checkFile published frozen do
                yield unimplemented (ConformanceTypars.describe m)

            for m in ConformanceTypars.checkMembers published frozen do
                yield unimplemented (ConformanceTypars.describeMember m)
        ]

    /// What a unit's `.fsi` half publishes, and everything anchored to the signature's own
    /// text: extraction's own losses, then the implementation's answer to it.
    let private analyseSignature
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (implementation: ParsedHalf<Pipeline.ParsedFile>)
        (implementationPath: OriginPath)
        (signature: ParsedHalf<Pipeline.ParsedSignature>)
        (frozen: FrozenPools)
        : IExternalSymbolProvider * FrozenSignatureFile =
        // Anchored to the signature's OWN token stream: its diagnostics index that text.
        let source = fileSource assembly.Name signature.Id signature.Parsed.Lexed

        let published, dropped =
            signatureView
                {
                    Target = assembly.Target
                    Visible = composed
                    Implementation =
                        {
                            File = implementationPath
                            Lexed = implementation.Parsed.Lexed
                            Ast = FSharpAst.ImplementationFile implementation.Parsed.File
                        }
                }
                source
                signature.Parsed

        published,
        {
            Source = source
            ParseDiagnostics = signature.Parsed.Diagnostics
            Diagnostics =
                dropped
                @ conformanceDiagnostics assembly.Name signature implementation published frozen
        }

    /// One unit analysed against `composed`, the surface every unit before it published.
    let private analyseUnit
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (unit: ParsedUnit)
        : FrozenFile =
        let parsed = unit.Implementation.Parsed

        // This file's own `namespace N` ahead of whatever prelude the external surface
        // carries, so a bare `bool` finds the `N.bool` an earlier file of the SAME package
        // declared.
        let scoped =
            match declaredNamespaces parsed.Lexed parsed.File with
            | [] -> composed
            | ns ->
                ExternalSymbolProviders.stack
                    ValueNone
                    (ns @ composed.AmbientOpenPrefixes |> List.distinct)
                    [ composed ]

        let origin = fileSource assembly.Name unit.Implementation.Id parsed.Lexed
        let frozen = analyse assembly.Name scoped origin parsed.File

        // The two objects the file publishes. The templates key off `SymbolKey` alone, so
        // replacing the signatures below leaves every one of them reachable.
        let bodies = InlineBodies.index (InlineBodies.collect origin frozen)

        // The `.fs`-derived signatures exist only to be checked against the published ones
        // and discarded; a `.fsi`'s are what survive.
        let signatures, signatureFile =
            match unit.Signature with
            | ValueNone -> FrozenSignature.toSignatures origin frozen, ValueNone
            | ValueSome signature ->
                let published, file =
                    analyseSignature assembly composed unit.Implementation origin.File.Path signature frozen

                published, ValueSome file

        {
            Source = origin
            ParseDiagnostics = parsed.Diagnostics
            Frozen = frozen
            Scoped = scoped
            View = ExternalSymbolProviders.withInlineBodies bodies signatures
            Signature = signatureFile
        }

    /// Analyse a multi-file assembly in manifest order through a chosen front end. Each file
    /// resolves the ones BEFORE it, composed nearest-first with the external surface last, so
    /// a name a nearer file re-declares shadows a farther one's.
    let analyseAssemblyWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile> list =
        // The visibility STACK, nearest first, with the external surface as its floor.
        let mutable visible: IExternalSymbolProvider list = [ external ]
        let results = ResizeArray<Result<FrozenFile, UnparsedFile>>()

        for unit in units do
            match parseUnit unit with
            | Error e -> results.Add(Error e)
            | Ok parsed ->
                let file =
                    analyseUnit analyse assembly (ExternalSymbolProviders.composite visible) parsed

                // Pushed on top of the files it may shadow; later files resolve through it.
                visible <- file.View :: visible
                results.Add(Ok file)

        List.ofSeq results

    /// Analyse a multi-file assembly through the default package/FSharp.Core front end.
    /// The self-host one is reached by passing it to `analyseAssemblyWith` directly.
    let analyseAssembly
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseAssemblyWith Pipeline.analyseFor assembly external units

    /// Diagnostics from a file that never reached analysis: it has no `Lexed`, so nothing
    /// resolves a token index against it and they render at line 1, col 1. A POSITIONED
    /// diagnostic here is unverifiable, so it faults rather than printing a plausible line.
    let unpositionedDiagnostics (path: AssemblyFileId) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
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
                        path.Name
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
        // The `""` bucket: no file was analysed, so no assembly claims this one. The
        // source exists only to resolve the positions the parser's diagnostics carry.
        | ValueSome lexed -> anchorDiagnostics (fileSource "" e.Id lexed) e.Failure.Diagnostics
        | ValueNone -> unpositionedDiagnostics e.Id e.Failure.Diagnostics

    /// Every analysed file's diagnostics, each anchored to ITS OWN file. Recovery's findings
    /// come first: they are what the tree the analysis ran on was patched up from.
    let consolidatedDiagnostics (files: FrozenFile list) : AnchoredDiagnostic list =
        [
            for f in files do
                match f.Signature with
                | ValueSome s -> yield! anchorDiagnostics s.Source (s.ParseDiagnostics @ s.Diagnostics)
                | ValueNone -> ()

                yield! anchorDiagnostics f.Source (f.ParseDiagnostics @ f.Frozen.Residue.Diagnostics)
        ]

    /// A whole assembly that passed the gate: its files in manifest order, plus their
    /// retained sources as one domain, because a backend needs those to read the anchors of a
    /// node spliced out of a prior file.
    type AnalysedAssembly =
        {
            Files: FrozenFile list
            Origins: OriginSources
        }

    /// `analyseAssemblyWith`, GATED: every file must parse, and no analysed file may carry
    /// an error-severity diagnostic. A parse failure is fatal for the whole assembly and is
    /// reported alone, because the files after it analysed against a truncated view.
    let analyseGated
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: SourceUnit list)
        : Result<AnalysedAssembly, AnchoredDiagnostic list> =
        let results = analyseAssemblyWith analyse assembly external units

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
