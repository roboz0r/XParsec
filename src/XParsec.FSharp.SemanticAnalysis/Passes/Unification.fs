namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pass 3 of the pipeline. The heart of the type checker.
//
// Precondition:  ctx.Desugared and ctx.Binding populated.
// Postcondition: ctx.TypeVar contains a TypeVar for every expression node,
//                with Link/Units/IfaceBounds/SrtpBounds resolved as far as
//                possible. Unresolved variables at let boundaries get
//                generalised to polymorphic parameters.
//
// Algorithm: classic Algorithm J as sketched in semantic-analysis.md §4.3.
// One walk over the (desugared view of the) AST:
//   1. Fresh TypeVar minted per expression node.
//   2. Equality constraints generated per construct (App: arg = paramType,
//      etc.).
//   3. Constraints fed into UnionFind.union, with Link/Units compatibility
//      checked on each merge.
//   4. On every successful unification of a previously-unbound TypeVar,
//      fire the on-unified callbacks for any SRTP/IWSAM bounds attached.
//      Those callbacks may unify further variables — iteration is contained
//      to this pass (see docs/architecture.md "Pass order is strictly
//      forward").
//
// Unit-of-measure unification is the same algorithm with an abelian-group
// equality test instead of structural equality. Implementation TBD; see
// docs/typevar.md "Open questions".
//
// `inline` handling: when this pass encounters an SRTP bound on a TypeVar
// inside an `inline` binding (per ctx.Binding.IsInline), the bound is
// resolvable. Inside a non-inline binding, the bound is an error — defer
// the diagnostic emission to a fixpoint-end check rather than emitting eagerly,
// to avoid spurious errors for variables that DO get bound later.

module Unification =

    /// Walk the file, generate constraints, solve them. Currently a no-op.
    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        ignore (ctx, file)
        ()
