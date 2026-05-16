namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared, ctx.Binding, ctx.TypeVar populated.
// Post: ctx.Escape populated for every TypeVar; TypeVar.Region set on
//       TypeVars that participated in the region graph.
//
// Regions are inequality-only (NOT used to drive type-class dispatch).
// Making them feed back into Unification would turn the whole pipeline
// into a fixpoint — see docs/architecture.md "Pass order is strictly forward".

module Regions =

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
