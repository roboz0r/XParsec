namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open EmitTypes

module EmitDispatch =

    /// `buildExpr` itself: the back-edge an arm module calls to emit a sub-expression.
    type Recur = EmitEnv -> IlBuilder -> TastAccessor.ExprId -> unit
