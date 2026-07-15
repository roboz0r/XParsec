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

    /// One successfully analysed unit of a multi-file assembly: its own source text and
    /// `Lexed` (so its per-file diagnostics resolve to positions WITHIN it), the frozen
    /// tree, and the provider view later files resolve its exports through.
    type FrozenUnit =
        {
            Path: string
            Input: string
            Lexed: Lexed
            Frozen: Frozen.TastFile
            View: IExternalSymbolProvider
        }

    /// A unit that never reached analysis: a lex/parse failure (`Pipeline.parse "ASM"`),
    /// surfaced as a unit-level error rather than thrown. Such a unit contributes NO view,
    /// so later files simply compose over the units that did parse.
    // `open XParsec.FSharp.Parser` also declares a `Diagnostic`; the bare name binds to
    // the parser's, so the front-end diagnostic is named through this alias throughout.
    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    type UnitError =
        {
            Path: string
            Diagnostics: Diagnostic list
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

    /// Analyse a multi-file assembly in manifest order. Each file resolves the ones
    /// BEFORE it — the prior file views composed nearest-first, then the external
    /// provider last — so a name a nearer file re-declares shadows a farther one's, and
    /// the external surface is the final fallback. Returns one `Result` per file, in
    /// order: `Ok` for an analysed unit (carrying its view), `Error` for a parse failure.
    /// A failed file contributes no view; the files after it compose over the survivors.
    let analyseAssembly
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
            | Error ds -> results.Add(Error { Path = path; Diagnostics = ds })
            | Ok(lexed, file) ->
                // Nearest prior file first, external last.
                let composed =
                    ExternalSymbolProviders.composite ((List.rev priorViews) @ [ external ])

                let frozen = Pipeline.analyseFor assemblyName composed source lexed file
                let view = FrozenSignature.toProvider assemblyName frozen

                // Push this file's view so LATER files can resolve its exports. It rides
                // at the head, so it composes NEAREST for the immediately-following file.
                priorViews <- view :: priorViews

                results.Add(
                    Ok
                        {
                            Path = path
                            Input = source
                            Lexed = lexed
                            Frozen = frozen
                            View = view
                        }
                )

        List.ofSeq results

    /// Every unit's diagnostics, each anchored to ITS OWN unit: path + (line, col)
    /// resolved against that unit's `Input`. Offsets are per-unit, so a file-2 diagnostic
    /// resolves against file 2's text and carries file 2's path — bare diagnostics are
    /// never flattened across units. Position resolution is `XParsec`'s canonical
    /// `LineIndex` (the same resolver `Debug.fs` uses), built ONCE per unit; a
    /// counter-minted `NodeKey` (no source position) anchors at the file head `(1, 1)`.
    let consolidatedDiagnostics (units: FrozenUnit list) : AnchoredDiagnostic list =
        [
            for u in units do
                let lineIndex = XParsec.LineIndex.OfString u.Input

                for d in u.Frozen.Diagnostics do
                    let struct (line, col) =
                        if not d.Key.IsSourcePosition then
                            struct (1, 1)
                        else
                            lineIndex.GetLineCol(min d.Key.Offset u.Input.Length)

                    {
                        Path = u.Path
                        Diagnostic = d
                        Line = line
                        Col = col
                    }
        ]
