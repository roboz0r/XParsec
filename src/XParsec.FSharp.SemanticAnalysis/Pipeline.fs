namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Top-level entry point: runs every pass in order and returns the frozen
// TAST. See docs/passes.md for the pipeline contract.
//
// This module deliberately offers ONE function for the common case. If a
// caller wants to inspect intermediate side tables (e.g. for tooling),
// they can call PassContext() directly and invoke each pass module's `run`.

module Pipeline =

    /// Run the full semantic analysis pipeline on a parsed implementation file.
    let analyse (file: ImplementationFile<SyntaxToken>) : TastFile =
        let ctx = PassContext()
        Desugar.run ctx file
        NameResolution.run ctx file
        Unification.run ctx file
        Regions.run ctx file
        Validation.run ctx file
        Freeze.run ctx file
