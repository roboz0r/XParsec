namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The FRONT-END multi-file assembly pipeline: an assembly is a LINEAR compose of
// per-file provider views, ahead of the external (package/BCL) provider. Each file is
// parsed and analysed ON ITS OWN — its own Input/Lexed/Ast/PassContext — so `NodeKey`
// offsets are per-file and never collide across files. That is the whole point: nothing
// `NodeKey`-keyed is ever merged across units.
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

module AssemblyUnits =

    // The alias binds `Diagnostic` to the SemanticAnalysis one throughout this module; see
    // that type's declaration for why the bare name would otherwise be the parser's.
    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// What a unit's diagnostics resolve AGAINST: its path, its source text, and its own
    /// token stream. One record rather than three parameters, because `path`/`source` are
    /// adjacent strings a caller can transpose without the compiler noticing, and the
    /// result is every diagnostic in the unit silently attributed to the wrong file.
    type UnitSource =
        {
            Path: string
            Input: string
            Lexed: Lexed
        }

    /// One successfully analysed unit of a multi-file assembly: what its diagnostics
    /// resolve against, the frozen tree, and the provider view later files resolve its
    /// exports through.
    type FrozenUnit =
        {
            Source: UnitSource
            /// What RECOVERY reported while parsing this unit. Analysis runs regardless —
            /// a recovered tree is still a tree — so these ride alongside the analysis
            /// residue rather than short-circuiting the unit.
            ParseDiagnostics: Diagnostic list
            Frozen: FrozenPools
            View: IExternalSymbolProvider
        }

    /// A unit that never reached analysis: a lex/parse failure (`Pipeline.parse "ASM"`),
    /// surfaced as a unit-level error rather than thrown. Such a unit contributes NO view,
    /// so later files simply compose over the units that did parse. The failure is carried
    /// as the parser seam produced it, so the "`Lexed` present iff lexing succeeded"
    /// invariant is stated once, on `ParseFailure`, rather than restated here.
    type UnitError =
        {
            Path: string
            Input: string
            Failure: Pipeline.ParseFailure
        }

    /// A diagnostic anchored to the unit it came from: its source path plus a (line, col)
    /// resolved against THAT unit's own text. Offsets are per-unit, so resolution happens
    /// within each unit — never by flattening bare diagnostics across units.
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
    type AnalyseUnit =
        string -> IExternalSymbolProvider -> string -> Lexed -> ImplementationFile<SyntaxToken> -> FrozenPools

    /// Analyse a multi-file assembly in manifest order through a chosen front end. Each
    /// file resolves the ones BEFORE it — the prior file views composed nearest-first,
    /// then the external provider last — so a name a nearer file re-declares shadows a
    /// farther one's, and the external surface is the final fallback. Returns one `Result`
    /// per file, in order: `Ok` for an analysed unit (carrying its view), `Error` for a
    /// parse failure. A failed file contributes no view; the files after it compose over
    /// the survivors.
    let analyseAssemblyWith
        (analyse: AnalyseUnit)
        (assemblyName: string)
        (external: IExternalSymbolProvider)
        (files: (string * string) list)
        : Result<FrozenUnit, UnitError> list =
        // Prior file views in FILE ORDER (oldest first); the newest is at the head after
        // each push, so `List.rev` before composing puts the NEAREST file first.
        let mutable priorViews: IExternalSymbolProvider list = []
        let results = ResizeArray<Result<FrozenUnit, UnitError>>()

        for (path, source) in files do
            match Pipeline.parse "ASM" source with
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

                let frozen = analyse assemblyName composed source parsed.Lexed parsed.File
                let view = FrozenSignature.toProvider assemblyName frozen

                // Push this file's view so LATER files can resolve its exports. It rides
                // at the head, so it composes NEAREST for the immediately-following file.
                priorViews <- view :: priorViews

                results.Add(
                    Ok
                        {
                            Source =
                                {
                                    Path = path
                                    Input = source
                                    Lexed = parsed.Lexed
                                }
                            ParseDiagnostics = parsed.Diagnostics
                            Frozen = frozen
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
        : Result<FrozenUnit, UnitError> list =
        analyseAssemblyWith Pipeline.analyseFor assemblyName external files

    /// Diagnostics from a unit that never reached analysis: it has no `Lexed`, so no token
    /// index could be resolved against it — and a whole-file lex/parse failure names no
    /// place in the file anyway. They render at the file head.
    ///
    /// A POSITIONED diagnostic here is a contradiction, not a case to render at (1, 1):
    /// something resolved a token of a unit whose token stream this function cannot see, so
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
                        "AssemblyUnits.unpositionedDiagnostics: %s produced no `Lexed`, so a diagnostic cannot carry a position — got %A (%s)"
                        path
                        positioned
                        d.Message
        ]

    /// Anchor a unit's bare diagnostics to a `path` + its own `source`: a `Site` names
    /// tokens of THIS unit's `Lexed`, whose `StartIndex` is the char offset resolved
    /// against THAT text via `XParsec`'s canonical `LineIndex` (the same resolver
    /// `Debug.fs` uses), built ONCE. `Lexed.GetLineForToken` alone will not do — it yields
    /// a line, and a column still needs the offset. `Site.Nowhere` renders at the file
    /// head. Token indices are per-unit, so this is only ever called with a diagnostic and
    /// the unit it was produced in — bare diagnostics are never flattened across units and
    /// resolved later.
    let anchorDiagnostics (unit: UnitSource) (diagnostics: Diagnostic list) : AnchoredDiagnostic list =
        let lexed = unit.Lexed
        let source = unit.Input
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
                    Path = unit.Path
                    Diagnostic = d
                    Line = line
                    Col = col
                }
        ]

    /// A failed unit's diagnostics, anchored against its own token stream when the failure
    /// came AFTER lexing, and at the file head when there is no stream to anchor against.
    let failureDiagnostics (e: UnitError) : AnchoredDiagnostic list =
        match e.Failure.Lexed with
        | ValueSome lexed ->
            anchorDiagnostics
                {
                    Path = e.Path
                    Input = e.Input
                    Lexed = lexed
                }
                e.Failure.Diagnostics
        | ValueNone -> unpositionedDiagnostics e.Path e.Failure.Diagnostics

    /// Every analysed unit's diagnostics, each anchored to ITS OWN unit (path + source).
    /// Recovery's findings come first: they are what the tree the analysis ran on was
    /// patched up from, so they precede anything the analysis then concluded about it.
    let consolidatedDiagnostics (units: FrozenUnit list) : AnchoredDiagnostic list =
        [
            for u in units do
                yield! anchorDiagnostics u.Source (u.ParseDiagnostics @ u.Frozen.Residue.Diagnostics)
        ]
