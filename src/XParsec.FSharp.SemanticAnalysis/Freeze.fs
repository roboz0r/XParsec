namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The only tree-to-tree transformation in the pipeline. Every previous pass
// annotated via side tables; freeze projects those annotations into a fresh
// TAST.
//
// Side tables can be discarded after this returns. The TAST is sharable;
// the CST + side tables are scoped to one compilation.

module Freeze =

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        ignore (ctx, file)
        { Decls = []
          Diagnostics = List.ofSeq ctx.Diagnostics }
