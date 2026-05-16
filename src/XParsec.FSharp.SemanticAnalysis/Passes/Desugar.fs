namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pass 1 of the pipeline.
//
// Precondition:  none (this pass runs first).
// Postcondition: ctx.Desugared is populated for every CST node whose
//                semantics differ from its surface form. Nodes without a
//                desugared form are absent from the table.
//
// Annotation-only: this pass NEVER rewrites the CST. It mints synthetic
// NodeKeys (via NodeKey.ofSynthetic) and stores a DesugaredForm describing
// how to interpret each construct. Later passes consult the table.
//
// What needs desugaring (rough list — flesh out as we implement):
//   - `x |> f` and `x ||> f y`        -> Application
//   - List/array/seq comprehensions   -> Sequential applications of yield/CE methods
//   - `for x in xs do ...`            -> IEnumerator pattern
//   - Computation expressions         -> Builder method chain
//   - Active pattern uses             -> Pattern-match plus discriminator calls
//   - Range expressions `[a..b..c]`   -> Seq.initInfinite + take, or a fast path
//   - Object expressions              -> Type instantiation + interface impl
//
// See docs/passes.md "Where `inline` fires" for the one cross-pass coupling
// that flows through Desugar's output.

module Desugar =

    /// Walk the implementation file and populate ctx.Desugared. Currently a no-op.
    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        // TODO: dispatch on file = Namespaces | NamedModule | AnonymousModule,
        // then traverse decls/exprs, calling helper functions per CST DU case.
        ()
