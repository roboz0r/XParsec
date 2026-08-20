namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// An assembly is a LINEAR composition of per-file provider views, ahead of the external
// (package/BCL) provider. Each file is parsed and analysed on its OWN Lexed/PassContext,
// so `NodeKey` offsets are per-file and nothing `NodeKey`-keyed is merged across files.
//
// A file publishes SIGNATURES and BODIES as two objects. Its `.fsi`, when it has one,
// replaces the signatures and leaves the bodies alone, so a declaration the signature does
// not make is invisible to every later file while a `member inline` beside it still splices.

module AssemblyFiles =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// One INPUT file of an assembly: its source text, and the name it is known by within that
    /// assembly, which anchors its diagnostics and identifies its frozen tree's nodes. Nothing
    /// reopens `Id`.
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
            Source: LexedFile
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
            Source: LexedFile
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
            Failure: ParseChain.ParseFailure
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
        CompilingAssembly -> IExternalSymbolProvider -> LexedFile -> ImplementationFile<SyntaxToken> -> FrozenPools

    /// The namespaces a file DECLARES. F# implicitly opens a file's own `namespace N` over
    /// its body, and that is what reaches a PRIOR file's namespace-direct declarations,
    /// including from the provider-layer probes that never see the file's local scope.
    let private declaredNamespaces (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : string list =
        match file with
        | ImplementationFile.Namespaces groups ->
            [
                for g in groups do
                    match g with
                    | NamespaceDeclGroup.Named(longIdent = li) ->
                        let path = li.Idents |> Seq.map (SyntaxToken.nameIn lexed) |> String.concat "."

                        if path.Length > 0 then
                            yield path
                    | NamespaceDeclGroup.Global _ -> ()
            ]
            |> List.distinct
        | _ -> []

    /// One parsed half of a unit: its tree, and the name within the assembly its own
    /// diagnostics anchor to, which no tree carries.
    type ParsedHalf<'tree> = { Id: AssemblyFileId; Parsed: 'tree }

    /// Both of a unit's trees, parsed before either is analysed. Public so a caller that already
    /// holds them, a package read once, can hand them over instead of text to parse again.
    type ParsedUnit =
        {
            Implementation: ParsedHalf<ParseChain.ParsedFile>
            Signature: ParsedHalf<ParseChain.ParsedSignature> voption
        }

    /// Parse both halves, each as leniently as the other: a tree the parser had to RECOVER is
    /// carried with its findings, which the gate refuses later. Only a half that yields NO tree
    /// fails the unit, and a signature failing that way fails it whole — it says nothing
    /// trustworthy about what its companion publishes, so falling back on the implementation's
    /// own inferred surface would publish more than the unit ever claimed.
    let parseUnit compilationDefines (unit: SourceUnit) : Result<ParsedUnit, UnparsedFile> =
        match ParseChain.parse compilationDefines unit.Implementation.Text with
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
                match ParseChain.parseSignature compilationDefines signature.Text with
                | Error f -> Error { Id = signature.Id; Failure = f }
                | Ok parsed ->
                    Ok
                        {
                            Implementation = implementation
                            Signature = ValueSome { Id = signature.Id; Parsed = parsed }
                        }

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
                        "internal error: %s produced no `Lexed`, so a diagnostic cannot carry a position, but got %A (%s)"
                        path.Name
                        positioned
                        d.Message
        ]

    /// A whole-set fault as one unpositioned diagnostic: it is about the package set a
    /// compilation was handed, so there is no file to anchor it to.
    let setFaultDiagnostics (fault: PackageSetFault) : AnchoredDiagnostic list =
        unpositionedDiagnostics AssemblyFileId.nowhere [ Diagnostic.nowhere (Kind.PackageSet fault) ]

    /// Anchor a file's bare diagnostics to its path and text: a `Site` points to tokens of THIS
    /// file's `Lexed`, whose `StartIndex` is a char offset into `file.Input`, turned into a
    /// (line, col) by one `LineIndex`. `Site.Nowhere` renders at line 1, col 1.
    let anchorDiagnostics (file: LexedFile) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
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
                    Path = file.Path.Relative
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
        | ValueSome lexed ->
            anchorDiagnostics (LexedFile.inFile { Assembly = ""; Relative = e.Id } lexed) e.Failure.Diagnostics
        | ValueNone -> unpositionedDiagnostics e.Id e.Failure.Diagnostics

    /// A `.fsi` half's findings, anchored in its own text: recovery's first, then
    /// resolution's and conformance's.
    let signatureFileDiagnostics (s: FrozenSignatureFile) : AnchoredDiagnostic list =
        anchorDiagnostics s.Source (s.ParseDiagnostics @ s.Diagnostics)

    /// An analysed implementation's findings, anchored in its own text: recovery's first,
    /// then the analysis residue.
    let implementationFileDiagnostics (f: FrozenFile) : AnchoredDiagnostic list =
        anchorDiagnostics f.Source (f.ParseDiagnostics @ f.Frozen.Residue.Diagnostics)

    /// Both halves' findings, the signature's first.
    let fileDiagnostics (f: FrozenFile) : AnchoredDiagnostic list =
        (match f.Signature with
         | ValueSome s -> signatureFileDiagnostics s
         | ValueNone -> [])
        @ implementationFileDiagnostics f

    /// Which side of the assembly boundary the fold publishes for.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type Publication =
        /// Compiling these units: each file homes in itself, the frozen `.fs` is kept for
        /// codegen, and a `.fsi` half is held to the conformance rules.
        | InAssembly
        /// Reading them as a reference: the published surfaces alone cross, and the caller
        /// stamps every symbol's home with the assembly. `bodyExternal`, given the intrinsic
        /// axis the units so far published, is what a BODY analyses over — platform metadata
        /// re-seeded with that axis, the way a package's own compile presents its primitives
        /// to itself.
        | AcrossAssemblies of bodyExternal: (IntrinsicTypeMap -> IExternalSymbolProvider)

    /// The language prelude as a SOURCE: it resolves nothing, and publishes the prefixes every
    /// file is written against. The FLOOR of the fold's visibility stack, so an in-assembly
    /// `.fsi` against an empty reference set and a package `.fsi` resolve `unit` identically.
    let private prelude =
        ExternalSymbolProviders.stack ValueNone RuntimeNames.preludeNamespaces []

    /// Short name ⇒ intrinsic repr, read off the unit's own implementation tree, so the
    /// `.fsi`'s `type t = extern` picks `Repr` over `Unsupported`.
    let private implementationReprs
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : Dictionary<string, string> =
        let reprs = Dictionary<string, string>(System.StringComparer.Ordinal)
        IntrinsicReprs.ofImplementationInto reprs (SyntaxToken.nameIn lexed) file
        reprs

    /// The implementation checked against its signature over the two ANALYSED halves: type and
    /// value presence, the `extern` ↔ repr pairing, and typar ORDER, each by resolved identity.
    /// Only here, because the package route freezes nothing to compare and keeps the CST rule
    /// set instead.
    ///
    /// The module-decl pairing stays SYNTACTIC. It asks whether the two files are a pair at
    /// all, and two halves resolved under different headers publish into different namespaces,
    /// which every finding below would then be about.
    let private conformanceDiagnostics
        (assembly: string)
        (signature: ParsedHalf<ParseChain.ParsedSignature>)
        (implementation: ParsedHalf<ParseChain.ParsedFile>)
        (surface: PublishedSurface)
        (published: IExternalSymbolProvider)
        (frozen: FrozenPools)
        : Diagnostic list =
        let verdict (v: ConformanceVerdict) =
            Diagnostic.nowhere (Kind.Conformance(assembly, v))

        let unimplemented (detail: string) =
            verdict (ConformanceVerdict.Unimplemented(signature.Id.Name, detail))

        let sigPath = Conformance.sigDeclPath signature.Parsed.Lexed signature.Parsed.File

        let implPath =
            Conformance.implDeclPath implementation.Parsed.Lexed implementation.Parsed.File

        [
            if sigPath <> implPath then
                yield
                    verdict (
                        ConformanceVerdict.ModulePairingMismatch(
                            signature.Id.Name,
                            implementation.Id.Name,
                            sigPath,
                            implPath
                        )
                    )

            for e in ConformanceSurface.checkTypes surface frozen do
                yield unimplemented (Conformance.describe e)

            for e in ConformanceSurface.checkValues surface frozen do
                yield unimplemented (Conformance.describe e)

            for m in ConformanceTypars.checkFile published frozen do
                yield unimplemented (ConformanceTypars.describe m)

            for m in ConformanceTypars.checkMembers published frozen do
                yield unimplemented (ConformanceTypars.describeMember m)
        ]

    /// One analysed unit of a fold: the frozen file, the surface it publishes across the
    /// assembly boundary — its `.fsi`'s when it has one, else the one its implementation
    /// infers — and its splice templates.
    [<NoEquality; NoComparison>]
    type AnalysedUnit =
        {
            File: FrozenFile
            Surface: PublishedSurface
            Bodies: InlineBodies.FileInlineBodies
            /// The provider this unit pushed onto the fold's stack: later units resolve
            /// through it, and a package build composes its outward provider from it. Under
            /// `Publication.InAssembly` it is the full view, splice templates included; under
            /// `Publication.AcrossAssemblies` it is the published surface alone.
            Published: IExternalSymbolProvider
            /// The diagnostics this unit surfaces to the fold's consumer, anchored in their
            /// own files. When compiling, everything both halves reported; when referencing,
            /// the `.fsi`'s findings — or, for a `.fs` without one, the analysis ERRORS,
            /// because the inferred surface IS the unit's contract.
            Surfaced: AnchoredDiagnostic list
        }

    /// A signature file resolved against the units before it, and its surface published.
    /// Every `FoldedUnit.SignatureOnly` payload is one of these; a paired unit's resolution
    /// passes through here before conformance joins its findings.
    [<NoEquality; NoComparison>]
    type ResolvedSignature =
        {
            /// The parsed half the surface was resolved from; its `Parsed.Diagnostics` are
            /// recovery's findings.
            Signature: ParsedHalf<ParseChain.ParsedSignature>
            Source: LexedFile
            Surface: PublishedSurface
            /// What resolving the signature found.
            Diagnostics: Diagnostic list
            /// `Surface` wrapped as a provider.
            Published: IExternalSymbolProvider
        }

    /// One unit's outcome under the fold.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type FoldedUnit =
        | Analysed of AnalysedUnit
        | SignatureOnly of ResolvedSignature
        /// A half that yielded no tree fails the unit whole; the implementation's fault
        /// leads when both halves failed.
        | Failed of leading: UnparsedFile * rest: UnparsedFile list

    [<RequireQualifiedAccess>]
    module FoldedUnit =

        /// The diagnostics a unit surfaces to the fold's consumer, anchored in their own
        /// files.
        let surfaced (unit: FoldedUnit) : AnchoredDiagnostic list =
            match unit with
            | FoldedUnit.Analysed u -> u.Surfaced
            | FoldedUnit.SignatureOnly r -> anchorDiagnostics r.Source (r.Signature.Parsed.Diagnostics @ r.Diagnostics)
            | FoldedUnit.Failed(leading, rest) -> List.collect failureDiagnostics (leading :: rest)

    /// One implementation file analysed and frozen against `composed`, with its splice
    /// templates collected.
    [<NoEquality; NoComparison>]
    type private ImplAnalysis =
        {
            Source: LexedFile
            Frozen: FrozenPools
            Scoped: IExternalSymbolProvider
            Bodies: InlineBodies.FileInlineBodies
        }

    let private analyseImplementation
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (implementation: ParsedHalf<ParseChain.ParsedFile>)
        : ImplAnalysis =
        let parsed = implementation.Parsed

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

        let source =
            LexedFile.inFile
                {
                    Assembly = assembly.Name
                    Relative = implementation.Id
                }
                parsed.Lexed

        let frozen = analyse assembly scoped source parsed.File

        {
            Source = source
            Frozen = frozen
            Scoped = scoped
            // The templates key off `SymbolKey` alone, so a `.fsi` replacing the file's
            // signatures leaves every one of them reachable.
            Bodies = InlineBodies.collect source frozen
        }

    let private resolveSignatureFile
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (reprs: Dictionary<string, string>)
        (signature: ParsedHalf<ParseChain.ParsedSignature>)
        : LexedFile * PublishedSurface * Diagnostic list =
        // Anchored to the signature's OWN token stream: its diagnostics index that text.
        let source =
            LexedFile.inFile
                {
                    Assembly = assembly.Name
                    Relative = signature.Id
                }
                signature.Parsed.Lexed

        let surface, diagnostics =
            SignatureResolution.resolveFile
                composed
                source
                {
                    Assembly = assembly.Name
                    Target = assembly.Target
                    Reprs = reprs
                }
                signature.Parsed.File

        source, surface, diagnostics

    let private noReprs () =
        Dictionary<string, string>(System.StringComparer.Ordinal)

    /// A unit's halves gated to trees: analysis runs only over a unit both halves of which
    /// parsed, and a faulted half fails the unit whole either way. The fold's input shape.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type ClassifiedUnit =
        | Unpaired of ParsedHalf<ParseChain.ParsedSignature>
        | Parsed of ParsedUnit
        /// The implementation's fault leads when both halves failed.
        | Faulted of leading: UnparsedFile * rest: UnparsedFile list

    [<RequireQualifiedAccess>]
    module ClassifiedUnit =

        /// Classify one package unit. `package` names the compilation in a faulted file's
        /// failure message.
        let ofPackageUnit (package: string) (unit: PackageSource.PackageUnit) : ClassifiedUnit =
            let unparsed (file: PackageSource.ReadFile<'Tree>) (fault: PackageSource.FileFault) : UnparsedFile =
                {
                    Id = file.Id
                    Failure = PackageSource.FileFault.toFailure package file.Relative fault
                }

            let half (file: PackageSource.ReadFile<'Tree>) (parsed: 'Tree) : ParsedHalf<'Tree> =
                { Id = file.Id; Parsed = parsed }

            match unit with
            | PackageSource.PackageUnit.UnpairedSignature signature ->
                match signature.Outcome with
                | Error fault -> ClassifiedUnit.Faulted(unparsed signature fault, [])
                | Ok parsed -> ClassifiedUnit.Unpaired(half signature parsed)
            | PackageSource.PackageUnit.Source src ->
                let sigFaults =
                    match src.Signature with
                    | ValueSome({ Outcome = Error fault } as signature) -> [ unparsed signature fault ]
                    | _ -> []

                match src.Implementation.Outcome with
                | Error fault -> ClassifiedUnit.Faulted(unparsed src.Implementation fault, sigFaults)
                | Ok parsedImplementation ->
                    match sigFaults with
                    | fault :: rest -> ClassifiedUnit.Faulted(fault, rest)
                    | [] ->
                        ClassifiedUnit.Parsed
                            {
                                Implementation = half src.Implementation parsedImplementation
                                Signature =
                                    match src.Signature with
                                    | ValueSome({ Outcome = Ok parsedSignature } as signature) ->
                                        ValueSome(half signature parsedSignature)
                                    | _ -> ValueNone
                            }

        /// A unit the caller parsed itself: `Ok` is a parsed unit, `Error` its one fault.
        let ofResult (unit: Result<ParsedUnit, UnparsedFile>) : ClassifiedUnit =
            match unit with
            | Ok u -> ClassifiedUnit.Parsed u
            | Error e -> ClassifiedUnit.Faulted(e, [])

    /// Fold a unit list in manifest order over `external`, with the language prelude at the
    /// visibility floor. Every implementation is analysed ONCE, and one with no `.fsi`
    /// publishes the surface it infers.
    ///
    /// Both arms are strict manifest order: each unit resolves only the units BEFORE it,
    /// nearest first, on both sides of the boundary. What remains of the axis is what a body
    /// analyses over, the home a `.fsi`'s surface is wrapped in, the conformance check, and
    /// which diagnostics a unit surfaces.
    let foldUnits
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (publication: Publication)
        (units: ClassifiedUnit list)
        : FoldedUnit list =
        // The units' published views so far, NEAREST first; `external` and the prelude are
        // the floor beneath them.
        let mutable own: IExternalSymbolProvider list = []

        let signatureFloor () =
            ExternalSymbolProviders.composite (own @ [ external; prelude ])

        let resolveSignature (reprs: Dictionary<string, string>) (signature: ParsedHalf<ParseChain.ParsedSignature>) =
            let source, surface, diagnostics =
                resolveSignatureFile assembly (signatureFloor ()) reprs signature

            {
                Signature = signature
                Source = source
                Surface = surface
                Diagnostics = diagnostics
                Published = PublishedSurface.toProvider surface
            }

        // What a BODY analyses over: the signature floor when compiling; when referencing,
        // platform metadata re-seeded with the axis published so far PLUS the intrinsics the
        // unit itself declares — the file's in-file knowledge, which BCL member
        // canonicalisation reads.
        let bodyProvider (sigPublished: IExternalSymbolProvider voption) : IExternalSymbolProvider =
            match publication with
            | Publication.InAssembly -> signatureFloor ()
            | Publication.AcrossAssemblies bodyExternal ->
                let axis =
                    ExternalSymbolProviders.mergeIntrinsics (
                        match sigPublished with
                        | ValueSome p -> p :: own
                        | ValueNone -> own
                    )

                ExternalSymbolProviders.composite (own @ [ bodyExternal axis; prelude ])

        [
            for unit in units ->
                match unit with
                | ClassifiedUnit.Faulted(leading, rest) -> FoldedUnit.Failed(leading, rest)
                | ClassifiedUnit.Unpaired signature ->
                    let resolved = resolveSignature (noReprs ()) signature

                    own <- resolved.Published :: own
                    FoldedUnit.SignatureOnly resolved
                | ClassifiedUnit.Parsed parsedUnit ->
                    // The unit's `.fsi` resolves against the units BEFORE it, and is not
                    // pushed until the body has analysed: neither half sees the other's
                    // names.
                    let resolvedSignature =
                        parsedUnit.Signature
                        |> ValueOption.map (
                            resolveSignature (
                                implementationReprs
                                    parsedUnit.Implementation.Parsed.Lexed
                                    parsedUnit.Implementation.Parsed.File
                            )
                        )

                    let impl =
                        analyseImplementation
                            analyse
                            assembly
                            (bodyProvider (resolvedSignature |> ValueOption.map (fun r -> r.Published)))
                            parsedUnit.Implementation

                    let surface, published, signatureFile =
                        match resolvedSignature with
                        | ValueSome r ->
                            // Homed in the IMPLEMENTATION file when compiling, so a later
                            // file of the same assembly resolves it as a local; bare when
                            // referencing, the caller stamping the assembly home once.
                            let published, conformance =
                                match publication with
                                | Publication.InAssembly ->
                                    let homed =
                                        ExternalSymbolProviders.stack
                                            (ValueSome(SymbolHome.InFile impl.Source.Path))
                                            []
                                            [ r.Published ]

                                    homed,
                                    conformanceDiagnostics
                                        assembly.Name
                                        r.Signature
                                        parsedUnit.Implementation
                                        r.Surface
                                        homed
                                        impl.Frozen
                                | Publication.AcrossAssemblies _ -> r.Published, []

                            r.Surface,
                            published,
                            ValueSome
                                {
                                    Source = r.Source
                                    ParseDiagnostics = r.Signature.Parsed.Diagnostics
                                    Diagnostics = r.Diagnostics @ conformance
                                }
                        | ValueNone ->
                            let surface = FrozenSignature.toSurface impl.Source impl.Frozen
                            surface, PublishedSurface.toProvider surface, ValueNone

                    let view =
                        ExternalSymbolProviders.withInlineBodies (InlineBodies.index impl.Bodies) published

                    // Pushed on top of the units it may shadow; later units resolve through
                    // it. The full view crosses only inside a compiling assembly — across
                    // the boundary the surface alone does, and the splice templates travel
                    // beside it.
                    let pushed =
                        match publication with
                        | Publication.InAssembly -> view
                        | Publication.AcrossAssemblies _ -> published

                    own <- pushed :: own

                    let file =
                        {
                            Source = impl.Source
                            ParseDiagnostics = parsedUnit.Implementation.Parsed.Diagnostics
                            Frozen = impl.Frozen
                            Scoped = impl.Scoped
                            View = view
                            Signature = signatureFile
                        }

                    let surfaced =
                        match publication with
                        | Publication.InAssembly -> fileDiagnostics file
                        | Publication.AcrossAssemblies _ ->
                            match signatureFile with
                            | ValueSome s -> signatureFileDiagnostics s
                            | ValueNone ->
                                // The unit's inferred surface IS its contract, so its
                                // analysis errors are findings about what it publishes. A
                                // unit with a `.fsi` keeps its tolerance: its analysis
                                // feeds splice templates alone.
                                implementationFileDiagnostics file
                                |> List.filter (fun d -> d.Diagnostic.Severity = Severity.Error)

                    FoldedUnit.Analysed
                        {
                            File = file
                            Surface = surface
                            Bodies = impl.Bodies
                            Published = pushed
                            Surfaced = surfaced
                        }
        ]

    /// Analyse a multi-file assembly in manifest order through a chosen front end: the
    /// compiling arm of `foldUnits`, for a caller holding its units as parse RESULTS.
    let analyseWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: Result<ParsedUnit, UnparsedFile> list)
        : Result<FrozenFile, UnparsedFile> list =
        foldUnits analyse assembly external Publication.InAssembly (List.map ClassifiedUnit.ofResult units)
        |> List.map (
            function
            | FoldedUnit.Analysed u -> Ok u.File
            | FoldedUnit.Failed(e, []) -> Error e
            | FoldedUnit.Failed _
            | FoldedUnit.SignatureOnly _ ->
                failwith
                    "internal error: `ClassifiedUnit.ofResult` yields one fault per failed unit and no unpaired signature"
        )

    /// `analyseWith` for a caller holding raw TEXT, namely a driver or a test. One that
    /// already parsed its units hands them over directly.
    let analyseAssemblyWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        compilationDefines
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseWith analyse assembly external (List.map (parseUnit compilationDefines) units)

    /// Analyse a multi-file assembly through the default package/FSharp.Core front end.
    /// The self-host one is reached by passing it to `analyseAssemblyWith` directly.
    let analyseAssembly
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        compilationDefines
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseAssemblyWith Pipeline.analyseFor assembly external compilationDefines units

    /// Every analysed file's diagnostics, each anchored to ITS OWN file. Recovery's findings
    /// come first: they are what the tree the analysis ran on was patched up from.
    let consolidatedDiagnostics (files: FrozenFile list) : AnchoredDiagnostic list = List.collect fileDiagnostics files

    /// A whole assembly that passed the gate: its files in manifest order, plus their
    /// retained sources as one domain, because a backend needs those to read the anchors of a
    /// node spliced out of a prior file.
    type AnalysedAssembly =
        {
            Files: FrozenFile list
            Sources: LexedFiles
        }

    /// `analyseWith`, GATED: every file must have parsed, and no analysed file may carry
    /// an error-severity diagnostic. A parse failure is fatal for the whole assembly and is
    /// reported alone, because the files after it analysed against a truncated view.
    let analyseGated
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: Result<ParsedUnit, UnparsedFile> list)
        : Result<AnalysedAssembly, AnchoredDiagnostic list> =
        let folded =
            foldUnits analyse assembly external Publication.InAssembly (List.map ClassifiedUnit.ofResult units)

        let parseFailures =
            folded
            |> List.collect (
                function
                | FoldedUnit.Failed(leading, rest) -> List.collect failureDiagnostics (leading :: rest)
                | _ -> []
            )

        match parseFailures with
        | _ :: _ -> Error parseFailures
        | [] ->
            let analysed =
                folded
                |> List.choose (
                    function
                    | FoldedUnit.Analysed u -> Some u
                    | _ -> None
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
                        Sources = analysed |> List.map (fun u -> u.File.Source) |> LexedFiles.ofSeq
                    }
