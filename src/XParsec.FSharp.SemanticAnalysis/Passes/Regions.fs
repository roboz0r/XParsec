namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pass 4 of the pipeline.
//
// Precondition:  ctx.Desugared, ctx.Binding, ctx.TypeVar populated.
// Postcondition: ctx.Escape contains an EscapeState for every TypeVar; the
//                TypeVar.Region field is set on TypeVars that participated
//                in the region graph.
//
// Solves the third axis from semantic-analysis.md §4.4. Unlike axes 1 and 2,
// regions are solved as a *partial order* (inequality: "A outlives B"), not
// equality. Algorithm:
//   1. Build a RegionGraph from AST dataflow:
//      - Every let-binding allocation is a fresh region.
//      - return-from-function adds edge (ReturnRegion >= local.Region).
//      - capture-into-closure adds edge (Closure.Region >= captured.Region).
//   2. Compute Least Upper Bound for every allocation.
//   3. Classify each TypeVar into LocalStack / CallerStack / HeapShared.
//
// Region info is NOT used during type-class dispatch (no Rust-style
// lifetime-dependent trait impl selection). This is a deliberate
// simplification — see docs/architecture.md "Pass order is strictly
// forward". If we ever want bidirectionality, the whole pipeline becomes a
// fixpoint.

module Regions =

    /// Build the region graph, solve LUBs, write EscapeState entries.
    /// Currently a no-op.
    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
