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
        CompilingAssembly -> IExternalSymbolProvider -> OriginSource -> ImplementationFile<SyntaxToken> -> FrozenPools

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

    /// The implementation checked against what its signature publishes: the CST rule set a
    /// manifest-paired unit is held to as well, then typar ORDER over the two frozen surfaces.
    /// The typar half runs only here, because the package route freezes nothing to compare.
    let private conformanceDiagnostics
        (assembly: string)
        (signature: ParsedHalf<ParseChain.ParsedSignature>)
        (implementation: ParsedHalf<ParseChain.ParsedFile>)
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

    /// One analysed unit of a fold: the frozen file, the surface it publishes across the
    /// assembly boundary — its `.fsi`'s when it has one, else the one its implementation
    /// infers — and its splice templates.
    [<NoEquality; NoComparison>]
    type AnalysedUnit =
        {
            File: FrozenFile
            Surface: PublishedSurface
            Bodies: InlineBodies.FileInlineBodies
            /// What LATER units of the fold resolved this one through.
            Published: IExternalSymbolProvider
        }

    /// A `.fsi` with no implementation on this target, resolved and published on its own.
    /// The conformance gate refuses one in a compiling assembly; the referencing route
    /// publishes it as-is.
    [<NoEquality; NoComparison>]
    type ResolvedSignature =
        {
            Source: OriginSource
            Surface: PublishedSurface
            /// What RECOVERY reported while parsing.
            ParseDiagnostics: Diagnostic list
            /// What resolving the signature found.
            Diagnostics: Diagnostic list
            /// What LATER units of the fold resolved this one through.
            Published: IExternalSymbolProvider
        }

    /// One unit's outcome under the fold.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type FoldedUnit =
        | Analysed of AnalysedUnit
        | SignatureOnly of ResolvedSignature
        /// A half that yielded no tree fails the unit whole; the implementation's fault
        /// leads when both halves failed.
        | Failed of faults: UnparsedFile list

    /// One implementation file analysed and frozen against `composed`, with its splice
    /// templates collected.
    [<NoEquality; NoComparison>]
    type private ImplAnalysis =
        {
            Origin: OriginSource
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

        let origin = fileSource assembly.Name implementation.Id parsed.Lexed
        let frozen = analyse assembly scoped origin parsed.File

        {
            Origin = origin
            Frozen = frozen
            Scoped = scoped
            // The templates key off `SymbolKey` alone, so a `.fsi` replacing the file's
            // signatures leaves every one of them reachable.
            Bodies = InlineBodies.collect origin frozen
        }

    let private resolveSignatureFile
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (reprs: Dictionary<string, string>)
        (signature: ParsedHalf<ParseChain.ParsedSignature>)
        : OriginSource * PublishedSurface * Diagnostic list =
        // Anchored to the signature's OWN token stream: its diagnostics index that text.
        let source = fileSource assembly.Name signature.Id signature.Parsed.Lexed

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

    /// One in-assembly unit analysed against `composed`, the surface every unit before it
    /// published. The `.fs`-derived surface is published where no `.fsi` narrows it; a
    /// `.fsi`'s replaces it, homed in the IMPLEMENTATION file so a later file of the same
    /// assembly resolves it as a local.
    let private analyseUnit
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (unit: ParsedUnit)
        : AnalysedUnit =
        let impl = analyseImplementation analyse assembly composed unit.Implementation

        let surface, signatures, signatureFile =
            match unit.Signature with
            | ValueNone ->
                let surface = FrozenSignature.toSurface impl.Origin impl.Frozen
                surface, PublishedSurface.toProvider surface, ValueNone
            | ValueSome signature ->
                let source, surface, resolutionDiagnostics =
                    resolveSignatureFile
                        assembly
                        composed
                        (implementationReprs unit.Implementation.Parsed.Lexed unit.Implementation.Parsed.File)
                        signature

                let published =
                    ExternalSymbolProviders.stack
                        (ValueSome(Origin.InFile impl.Origin.File.Path))
                        []
                        [ PublishedSurface.toProvider surface ]

                surface,
                published,
                ValueSome
                    {
                        Source = source
                        ParseDiagnostics = signature.Parsed.Diagnostics
                        Diagnostics =
                            resolutionDiagnostics
                            @ conformanceDiagnostics assembly.Name signature unit.Implementation published impl.Frozen
                    }

        let view =
            ExternalSymbolProviders.withInlineBodies (InlineBodies.index impl.Bodies) signatures

        {
            File =
                {
                    Source = impl.Origin
                    ParseDiagnostics = unit.Implementation.Parsed.Diagnostics
                    Frozen = impl.Frozen
                    Scoped = impl.Scoped
                    View = view
                    Signature = signatureFile
                }
            Surface = surface
            Bodies = impl.Bodies
            Published = view
        }

    /// A unit's halves gated to trees: analysis runs only over a unit both halves of which
    /// parsed, and a faulted half fails the unit whole either way.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type private ClassifiedUnit =
        | Unpaired of ParsedHalf<ParseChain.ParsedSignature>
        | Parsed of ParsedUnit
        | Faulted of UnparsedFile list

    /// Fold a unit list in manifest order over `external`, with the language prelude at the
    /// visibility floor. Every implementation is analysed ONCE, and one with no `.fsi`
    /// publishes the surface it infers.
    ///
    /// Both arms are strict manifest order: each unit resolves only the units BEFORE it,
    /// nearest first, on both sides of the boundary. What remains of the axis is the home,
    /// the ambient prefixes, the conformance check, and whether the frozen `.fs` is kept
    /// for codegen.
    let foldUnits
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (publication: Publication)
        (units: PackageSource.PackageUnit list)
        : FoldedUnit list =
        let unparsed (file: PackageSource.ReadFile<'Tree>) (fault: PackageSource.FileFault) : UnparsedFile =
            {
                Id = file.Id
                Failure = PackageSource.FileFault.toFailure assembly.Name file.Relative fault
            }

        let half (file: PackageSource.ReadFile<'Tree>) (parsed: 'Tree) : ParsedHalf<'Tree> =
            { Id = file.Id; Parsed = parsed }

        let classify (unit: PackageSource.PackageUnit) : ClassifiedUnit =
            match unit with
            | PackageSource.PackageUnit.UnpairedSignature signature ->
                match signature.Outcome with
                | Error fault -> ClassifiedUnit.Faulted [ unparsed signature fault ]
                | Ok parsed -> ClassifiedUnit.Unpaired(half signature parsed)
            | PackageSource.PackageUnit.Source src ->
                // The implementation's fault leads when both halves failed.
                let sigFaults =
                    match src.Signature with
                    | ValueSome({ Outcome = Error fault } as signature) -> [ unparsed signature fault ]
                    | _ -> []

                match src.Implementation.Outcome with
                | Error fault -> ClassifiedUnit.Faulted(unparsed src.Implementation fault :: sigFaults)
                | Ok parsedImplementation ->
                    match sigFaults with
                    | _ :: _ -> ClassifiedUnit.Faulted sigFaults
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

        let classified = List.map classify units

        match publication with
        | Publication.InAssembly ->
            // The visibility STACK, nearest first.
            let mutable visible: IExternalSymbolProvider list = [ external; prelude ]

            [
                for unit in classified ->
                    match unit with
                    | ClassifiedUnit.Faulted faults -> FoldedUnit.Failed faults
                    | ClassifiedUnit.Unpaired signature ->
                        let source, surface, resolutionDiagnostics =
                            resolveSignatureFile
                                assembly
                                (ExternalSymbolProviders.composite visible)
                                (noReprs ())
                                signature

                        let published = PublishedSurface.toProvider surface
                        visible <- published :: visible

                        FoldedUnit.SignatureOnly
                            {
                                Source = source
                                Surface = surface
                                ParseDiagnostics = signature.Parsed.Diagnostics
                                Diagnostics = resolutionDiagnostics
                                Published = published
                            }
                    | ClassifiedUnit.Parsed parsedUnit ->
                        let analysed =
                            analyseUnit analyse assembly (ExternalSymbolProviders.composite visible) parsedUnit

                        // Pushed on top of the units it may shadow; later units resolve
                        // through it.
                        visible <- analysed.Published :: visible
                        FoldedUnit.Analysed analysed
            ]
        | Publication.AcrossAssemblies bodyExternal ->
            // The visibility STACK, nearest first: what the units before this one published.
            let mutable visible: IExternalSymbolProvider list = []

            let signatureFloor () =
                ExternalSymbolProviders.composite (visible @ [ external; prelude ])

            let resolveBoundary
                (reprs: Dictionary<string, string>)
                (signature: ParsedHalf<ParseChain.ParsedSignature>)
                =
                let source, surface, diagnostics =
                    resolveSignatureFile assembly (signatureFloor ()) reprs signature

                {
                    Source = source
                    Surface = surface
                    ParseDiagnostics = signature.Parsed.Diagnostics
                    Diagnostics = diagnostics
                    Published = PublishedSurface.toProvider surface
                }

            [
                for unit in classified ->
                    match unit with
                    | ClassifiedUnit.Faulted faults -> FoldedUnit.Failed faults
                    | ClassifiedUnit.Unpaired signature ->
                        let resolved = resolveBoundary (noReprs ()) signature

                        visible <- resolved.Published :: visible
                        FoldedUnit.SignatureOnly resolved
                    | ClassifiedUnit.Parsed parsedUnit ->
                        // The unit's `.fsi` resolves against the units BEFORE it, and is not
                        // pushed until the body has analysed: neither half sees the other's
                        // names, exactly as in a compiling assembly.
                        let boundarySignature =
                            match parsedUnit.Signature with
                            | ValueSome signature ->
                                ValueSome(
                                    resolveBoundary
                                        (implementationReprs
                                            parsedUnit.Implementation.Parsed.Lexed
                                            parsedUnit.Implementation.Parsed.File)
                                        signature
                                )
                            | ValueNone -> ValueNone

                        // The body's platform metadata is seeded with the axis published so
                        // far PLUS the intrinsics this unit itself declares — the file's
                        // in-file knowledge, which BCL member canonicalisation reads.
                        let bodyAxis =
                            ExternalSymbolProviders.mergeIntrinsics (
                                match boundarySignature with
                                | ValueSome resolved -> resolved.Published :: visible
                                | ValueNone -> visible
                            )

                        let impl =
                            analyseImplementation
                                analyse
                                assembly
                                (ExternalSymbolProviders.composite (visible @ [ bodyExternal bodyAxis; prelude ]))
                                parsedUnit.Implementation

                        let surface, published, signatureFile =
                            match boundarySignature with
                            | ValueSome resolved ->
                                resolved.Surface,
                                resolved.Published,
                                ValueSome
                                    {
                                        Source = resolved.Source
                                        ParseDiagnostics = resolved.ParseDiagnostics
                                        Diagnostics = resolved.Diagnostics
                                    }
                            | ValueNone ->
                                let surface = FrozenSignature.toSurface impl.Origin impl.Frozen
                                surface, PublishedSurface.toProvider surface, ValueNone

                        visible <- published :: visible

                        FoldedUnit.Analysed
                            {
                                File =
                                    {
                                        Source = impl.Origin
                                        ParseDiagnostics = parsedUnit.Implementation.Parsed.Diagnostics
                                        Frozen = impl.Frozen
                                        Scoped = impl.Scoped
                                        View =
                                            ExternalSymbolProviders.withInlineBodies
                                                (InlineBodies.index impl.Bodies)
                                                published
                                        Signature = signatureFile
                                    }
                                Surface = surface
                                Bodies = impl.Bodies
                                Published = published
                            }
            ]

    /// Analyse a multi-file assembly in manifest order through a chosen front end: the
    /// compiling arm of `foldUnits`, for a caller holding its units as parse RESULTS.
    let analyseParsedWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: Result<ParsedUnit, UnparsedFile> list)
        : Result<FrozenFile, UnparsedFile> list =
        let toPackageUnit (unit: Result<ParsedUnit, UnparsedFile>) : PackageSource.PackageUnit =
            let readFile (h: ParsedHalf<'Tree>) : PackageSource.ReadFile<'Tree> =
                {
                    Relative = h.Id.Name
                    Id = h.Id
                    Outcome = Ok h.Parsed
                }

            match unit with
            | Ok u ->
                PackageSource.PackageUnit.Source
                    {
                        Signature = u.Signature |> ValueOption.map readFile
                        Implementation = readFile u.Implementation
                    }
            | Error e ->
                PackageSource.PackageUnit.Source
                    {
                        Signature = ValueNone
                        Implementation =
                            {
                                Relative = e.Id.Name
                                Id = e.Id
                                Outcome = Error(PackageSource.FileFault.Unparsed e.Failure)
                            }
                    }

        foldUnits analyse assembly external Publication.InAssembly (List.map toPackageUnit units)
        |> List.map (
            function
            | FoldedUnit.Analysed u -> Ok u.File
            | FoldedUnit.Failed(e :: _) -> Error e
            | FoldedUnit.Failed []
            | FoldedUnit.SignatureOnly _ ->
                // Every input unit above carries an implementation and at least a fault.
                failwith "internal error: a Result-shaped unit folded to a shape it cannot express"
        )

    /// `analyseParsedWith` for a caller holding raw TEXT, namely a driver or a test. One that
    /// already parsed its units hands them over directly.
    let analyseAssemblyWith
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        compilationDefines
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseParsedWith analyse assembly external (List.map (parseUnit compilationDefines) units)

    /// Analyse a multi-file assembly through the default package/FSharp.Core front end.
    /// The self-host one is reached by passing it to `analyseAssemblyWith` directly.
    let analyseAssembly
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        compilationDefines
        (units: SourceUnit list)
        : Result<FrozenFile, UnparsedFile> list =
        analyseAssemblyWith Pipeline.analyseFor assembly external compilationDefines units

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

    /// `analyseParsedWith`, GATED: every file must have parsed, and no analysed file may carry
    /// an error-severity diagnostic. A parse failure is fatal for the whole assembly and is
    /// reported alone, because the files after it analysed against a truncated view.
    let analyseGatedParsed
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (units: Result<ParsedUnit, UnparsedFile> list)
        : Result<AnalysedAssembly, AnchoredDiagnostic list> =
        let results = analyseParsedWith analyse assembly external units

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
