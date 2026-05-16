namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pass 2 of the pipeline.
//
// Precondition:  ctx.Desugared populated by Desugar.
// Postcondition: ctx.Binding populated for every ident-use site, mapping it
//                to the NodeKey of its binding site (plus IsInline/IsMutable
//                flags from the binding's declaration).
//
// Responsibilities:
//   - Maintain a scope stack (immutable HashMap of Name -> BindingSite) per
//     §4.2 of semantic-analysis.md. Pushing a new scope allows shadowing.
//   - Resolve qualified lookups through `open` declarations.
//   - Record the IsInline flag on every binding — Unification reads this to
//     decide whether SRTP bounds are resolvable (see docs/passes.md).
//   - Record the IsMutable flag — Validation reads this for `<-` checks.
//
// External symbols (FSharp.Core types, .dll references, target-library
// symbols per §4.1) come from an IExternalSymbolProvider. Not designed yet;
// stub it as a parameter when the pass starts being implemented.

module NameResolution =

    /// Walk the file and populate ctx.Binding. Currently a no-op.
    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
