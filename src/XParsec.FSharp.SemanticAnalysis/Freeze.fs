namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The freeze step. Pass 6, but conceptually separate: this is where the
// immutable TAST is built from the CST + every populated side table.
//
// Precondition:  every pass has run; ctx is fully populated.
// Postcondition: returns a TastFile that is safe to share / cache / consume
//                downstream. ctx's side tables can be discarded.
//
// This is the ONLY tree-to-tree transformation in the whole pipeline. Every
// previous pass annotated the CST in place (via side tables); freeze turns
// those annotations into a new tree shape.
//
// Why a separate tree rather than handing downstream consumers the CST +
// side tables? Two reasons:
//   - Downstream consumers (codegen, target plugins) don't care about
//     trivia, parens, or token-level layout. The TAST drops all of it.
//   - Side tables are scoped to one compilation. The TAST is sharable.

module Freeze =

    /// Build the TAST from the CST and populated side tables.
    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        ignore (ctx, file)
        // TODO: traverse the CST. For each expression node, look up its
        // entries in ctx.Desugared / ctx.Binding / ctx.TypeVar / ctx.Escape,
        // and build a corresponding TExpr.
        { Decls = []
          Diagnostics = List.ofSeq ctx.Diagnostics }
