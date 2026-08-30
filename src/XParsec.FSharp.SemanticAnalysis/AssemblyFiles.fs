namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// The files an assembly is built from, and the anchoring every diagnostic about one goes
// through. Each file is parsed and analysed on its OWN Lexed/PassContext, so `NodeKey`
// offsets are per-file and a bare diagnostic resolves only inside the file that produced it.

module AssemblyFiles =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// One INPUT file of an assembly: its source text, and the name it is known by within that
    /// assembly, which anchors its diagnostics and identifies its frozen tree's nodes.
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

    /// One compilation unit of an assembly as TEXT: the implementation compiled into it, and
    /// the signature that publishes it.
    type SourceUnit = SourceUnit<SourceFile, SourceFile>

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
    /// file's `View`; the remainder is everything anchored to the signature's own text.
    type FrozenSignatureFile =
        {
            Retained: LexedFile
            /// What RECOVERY reported parsing the signature.
            ParseDiagnostics: Diagnostic list
            /// Where the implementation failed to satisfy the signature, and where the
            /// signature declared something extraction could not publish.
            Diagnostics: Diagnostic list
        }

    /// One successfully analysed file of a multi-file assembly, carrying the provider view
    /// later files resolve its exports through.
    type FrozenFile =
        {
            Retained: LexedFile
            /// What RECOVERY reported while parsing. Analysis runs regardless, and these are
            /// carried alongside the analysis residue.
            ParseDiagnostics: Diagnostic list
            Frozen: FrozenPools
            /// In declaration order.
            Imports: ImportObligation list
            /// The provider this file WAS analysed against: prior files' views nearest-first
            /// over the external surface, under the file's own declared namespaces. Carried
            /// rather than re-derived, so a backend resolves an `External` node the same way.
            Scoped: IExternalSymbolProvider
            /// What LATER files resolve this file through: its signatures, replaced by the
            /// `.fsi`'s when it has one, with its own splice templates layered back on.
            View: IExternalSymbolProvider
            Signature: FrozenSignatureFile voption
        }

    /// A file that never reached analysis, surfaced as a file-level error rather than thrown.
    /// Later files compose over the ones that did parse.
    type UnparsedFile =
        { Id: AssemblyFileId; Fault: FileFault }

    /// A diagnostic anchored to the file it came from: its source path plus a (line, col)
    /// resolved against THAT file's own text. `Path` is `ValueNone` for a finding about the
    /// package set or the driver's own inputs.
    type AnchoredDiagnostic =
        {
            Path: AssemblyFileId voption
            Diagnostic: Diagnostic
            Line: int
            Col: int
        }

    [<RequireQualifiedAccess>]
    module AnchoredDiagnostic =

        let errors (ds: AnchoredDiagnostic seq) : AnchoredDiagnostic list =
            ds |> Seq.filter (fun a -> Diagnostic.isError a.Diagnostic) |> List.ofSeq

        /// One finding as `file(line,col): message`.
        let render (a: AnchoredDiagnostic) : string =
            sprintf "%s(%d,%d): %s" (AssemblyFileId.toStored a.Path) a.Line a.Col a.Diagnostic.Message

        /// Findings as `render` per line, in the order given.
        let renderAll (ds: AnchoredDiagnostic seq) : string =
            ds |> Seq.map render |> String.concat "\n"

    /// The per-file front-end seam: analyse+freeze one parsed file against a composed
    /// provider, wrappable by a probe that times each file.
    type AnalyseFile =
        CompilingAssembly
            -> IExternalSymbolProvider
            -> LexedFile
            -> ImplementationFile<SyntaxToken>
            -> FrozenPools * ImportObligation list

    /// One parsed half of a unit: its tree, and the name within the assembly its own
    /// diagnostics anchor to, which the tree itself omits.
    type ParsedFile<'tree> = { Id: AssemblyFileId; Parsed: 'tree }

    /// Both of a unit's trees, parsed before either is analysed.
    type ParsedSourceUnit =
        SourceUnit<ParsedFile<ParseChain.ParsedSignature>, ParsedFile<ParseChain.ParsedImplementation>>

    /// One unit of an assembly, gated to trees: `Analysable` when both halves parsed, and
    /// `Faulted` carrying every half that yielded none. A signature that yielded no tree faults
    /// the unit whole, because the implementation's inferred surface publishes more.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type AssemblyUnit =
        | Analysable of ParsedSourceUnit
        /// The implementation's fault leads when both halves faulted.
        | Faulted of leading: UnparsedFile * rest: UnparsedFile list

    [<RequireQualifiedAccess>]
    module AssemblyUnit =

        /// Both halves gated to trees at once, so a unit reports what BOTH of them failed at.
        let private ofHalves
            (signature: Result<ParsedFile<ParseChain.ParsedSignature>, UnparsedFile> voption)
            (implementation: Result<ParsedFile<ParseChain.ParsedImplementation>, UnparsedFile>)
            : AssemblyUnit =
            let signatureFaults =
                match signature with
                | ValueSome(Error fault) -> [ fault ]
                | ValueSome(Ok _)
                | ValueNone -> []

            match implementation with
            | Error fault -> AssemblyUnit.Faulted(fault, signatureFaults)
            | Ok implementation ->
                match signatureFaults with
                | fault :: rest -> AssemblyUnit.Faulted(fault, rest)
                | [] ->
                    AssemblyUnit.Analysable
                        {
                            Implementation = implementation
                            Signature =
                                match signature with
                                | ValueSome(Ok parsed) -> ValueSome parsed
                                | ValueSome(Error _)
                                | ValueNone -> ValueNone
                        }

        /// Parse a unit held as TEXT, each half as leniently as the other: a tree the parser
        /// had to RECOVER is carried with its findings, which the gate refuses later.
        let parse (compilationDefines: Set<string>) (unit: SourceUnit) : AssemblyUnit =
            let half
                (parse: Set<string> -> string -> Result<'Tree, ParseChain.ParseFailure>)
                (file: SourceFile)
                : Result<ParsedFile<'Tree>, UnparsedFile> =
                match parse compilationDefines file.Text with
                | Ok parsed -> Ok { Id = file.Id; Parsed = parsed }
                | Error failure ->
                    Error
                        {
                            Id = file.Id
                            Fault = FileFault.Unparsed failure
                        }

            ofHalves
                (unit.Signature |> ValueOption.map (half ParseChain.parseSignature))
                (half ParseChain.parse unit.Implementation)

        /// A unit of a manifest read, whose halves were parsed when the manifest was read.
        let ofReadUnit (unit: ReadSourceUnit) : AssemblyUnit =
            let half (file: ReadFile<'Tree>) : Result<ParsedFile<'Tree>, UnparsedFile> =
                match file.Outcome with
                | Ok parsed -> Ok { Id = file.Id; Parsed = parsed }
                | Error fault -> Error { Id = file.Id; Fault = fault }

            ofHalves (ValueOption.map half unit.Signature) (half unit.Implementation)

    let private unpositioned (path: AssemblyFileId voption) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
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
                        "internal error: %s yielded no text, so a diagnostic cannot carry a position, but got %A (%s)"
                        (AssemblyFileId.toStored path)
                        positioned
                        d.Message
        ]

    /// Diagnostics from a path that yielded no text: nothing resolves a token index against
    /// it, so they render at line 1, col 1. A POSITIONED diagnostic here is unverifiable, so
    /// it faults rather than printing a plausible line.
    let unpositionedDiagnostics (path: AssemblyFileId) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
        unpositioned (ValueSome path) diagnostics

    /// Diagnostics about no file: the package set a compilation was handed, or the inputs a
    /// driver refused. They render at line 1, col 1 under a blank path.
    let unfiledDiagnostics (diagnostics: Diagnostic list) : AnchoredDiagnostic list = unpositioned ValueNone diagnostics

    /// A whole-set fault as one unfiled diagnostic.
    let setFaultDiagnostics (fault: PackageSetFault) : AnchoredDiagnostic list =
        unfiledDiagnostics [ Diagnostic.nowhere (Kind.PackageSet fault) ]

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

    /// A failed file's diagnostics, anchored against its own token stream, and at line 1,
    /// col 1 for a path that never yielded one.
    let failureDiagnostics (e: UnparsedFile) : AnchoredDiagnostic list =
        match e.Fault with
        // No file was analysed, so no assembly claims this one. The source exists only to
        // resolve the positions the parser's diagnostics carry.
        | FileFault.Unparsed f -> anchorDiagnostics (LexedFile.unclaimed e.Id f.Lexed) f.Diagnostics
        | FileFault.Missing d -> unpositionedDiagnostics e.Id [ d ]

    /// A `.fsi` half's findings, anchored in its own text: recovery's first, then
    /// resolution's and conformance's.
    let signatureFileDiagnostics (s: FrozenSignatureFile) : AnchoredDiagnostic list =
        anchorDiagnostics s.Retained (s.ParseDiagnostics @ s.Diagnostics)

    /// An analysed implementation's findings, anchored in its own text: recovery's first,
    /// then the analysis residue.
    let implementationFileDiagnostics (f: FrozenFile) : AnchoredDiagnostic list =
        anchorDiagnostics f.Retained (f.ParseDiagnostics @ f.Frozen.Residue.Diagnostics)

    /// Both halves' findings, the signature's first.
    let fileDiagnostics (f: FrozenFile) : AnchoredDiagnostic list =
        (match f.Signature with
         | ValueSome s -> signatureFileDiagnostics s
         | ValueNone -> [])
        @ implementationFileDiagnostics f

    /// Every analysed file's diagnostics, in manifest order, each anchored to ITS OWN file.
    let consolidatedDiagnostics (files: FrozenFile list) : AnchoredDiagnostic list = List.collect fileDiagnostics files

    /// The files' retained text as ONE anchor domain, so a node spliced out of one of them
    /// resolves its positions against the file that declared it.
    let retainedDomain (files: FrozenFile list) : LexedFiles =
        LexedFiles.ofSeq [ for f in files -> f.Retained ]
