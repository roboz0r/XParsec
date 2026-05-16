namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pass 5 of the pipeline. The only pass that writes nothing to side tables —
// it only emits into ctx.Diagnostics.
//
// Precondition:  every prior side table populated.
// Postcondition: ctx.Diagnostics contains any semantic violations found.
//
// Checks (per semantic-analysis.md §4.5):
//
//   - Pattern match exhaustiveness. Build a decision tree per `match` /
//     `function`. Yield a diagnostic for each uncovered union case or
//     numeric range. The union-case set comes from ctx.TypeVar.Link on the
//     scrutinee.
//
//   - Value restriction. Walk generalised bindings; if a 'a appears in a
//     mutable ref position, yield a diagnostic. The generalisation info is
//     on the TypeVar after Unification.
//
//   - Immutability enforcement. For every `<-` site (CST: Expr.LongIdentSet
//     and friends), look up the target binding via ctx.Binding; if the
//     resolved binding's IsMutable is false, yield a diagnostic.
//
// Earlier passes also emit into ctx.Diagnostics (unresolved names,
// unification failures, etc.). Validation is just the "after the dust
// settles" sweep for things that are only checkable once everything else is
// known.

module Validation =

    /// Run all post-typing semantic checks. Currently a no-op.
    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
