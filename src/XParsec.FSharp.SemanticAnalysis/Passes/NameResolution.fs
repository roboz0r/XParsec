namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared populated.
// Post: ctx.Binding populated for every ident-use site.
//
// IsInline must be recorded on every binding — Unification reads it to
// decide whether SRTP bounds are resolvable.
// IsMutable must be recorded — Validation reads it for `<-` checks.

module NameResolution =

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
