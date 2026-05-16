namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  none.
// Post: ctx.Desugared populated for every CST node whose semantics differ
//       from its surface form.
//
// Annotation-only: NEVER rewrites the CST. Mints synthetic NodeKeys via
// NodeKey.ofSynthetic and stores a DesugaredForm describing how to interpret
// each construct.
//
// TODO — constructs that will need desugaring:
//   - `x |> f`, `x ||> f y`             -> Application
//   - List / array / seq comprehensions -> yield + CE method chain
//   - `for x in xs do …`                -> IEnumerator pattern
//   - Computation expressions           -> Builder method chain
//   - Active pattern uses               -> Match + discriminator calls
//   - Range expressions                 -> Seq.initInfinite + take, or fast path
//   - Object expressions                -> Type instantiation + interface impl

module Desugar =

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
