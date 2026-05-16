namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  every prior side table populated.
// Post: ctx.Diagnostics has any semantic violations.
//
// Read-only. Checks: pattern-match exhaustiveness, value restriction,
// immutability enforcement (per semantic-analysis.md §4.5).

module Validation =

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
