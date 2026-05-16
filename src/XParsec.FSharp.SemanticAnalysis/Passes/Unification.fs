namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared and ctx.Binding populated.
// Post: ctx.TypeVar populated; unresolved variables at let boundaries are
//       generalised to polymorphic parameters.
//
// Algorithm J. On every successful unification, fire the on-unified
// callbacks for any SRTP / IWSAM bounds. Callbacks may unify further
// variables — iteration is contained to this pass.
//
// SRTP-bound resolution against primitives must consult both
// ctx.Provider AND the active target's capability table — see
// docs/typevar.md "SRTP resolution and target capabilities" and
// [[project_inline_il_target_specific]].

module Unification =

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
