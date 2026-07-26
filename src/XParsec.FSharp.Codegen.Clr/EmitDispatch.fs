namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open EmitTypes

module EmitDispatch =

    /// The back-edge into top-level expression emission. `buildExpr` and its ~45
    /// match arms once formed a single `let rec … and …` clique too large for one
    /// file (EmitExpr.fs, 1700+ lines). The arms now live in per-concern `Emit*.fs`
    /// modules; each receives `buildExpr` as this typed delegate. The clique's
    /// *only* edge that crosses a file boundary is the call back into `buildExpr`
    /// — every other cross-arm call is a forward reference resolved by compile
    /// order, so this single abbreviation is all the indirection the split needs
    /// (no mutable dispatch cell). Mirrors `UnificationInferDispatch.Infer`.
    type Recur = EmitEnv -> IlBuilder -> TastAccessor.ExprId -> unit
