namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open EmitTypes

module EmitDispatch =

    /// `buildExpr` itself: the back-edge an arm module calls to emit a sub-expression in
    /// value position.
    type Recur = EmitEnv -> IlBuilder -> TastAccessor.ExprId -> unit

    /// `buildExprAt`: the back-edge for emitting a sub-expression at a chosen position.
    type RecurAt = ExprPos -> EmitEnv -> IlBuilder -> TastAccessor.ExprId -> unit
