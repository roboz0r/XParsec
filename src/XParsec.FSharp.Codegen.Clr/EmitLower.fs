namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// CLR-specific lowering: re-exports of the platform-neutral lowering utilities that
/// live in `SemanticAnalysis.TastLower` (so both codegen backends share them without
/// referencing each other). Existing CLR call sites keep using `EmitLower.*`.
///
/// Codegen owns NO per-operator dispatch. An operator is an ordinary external value
/// whose contract body `Passes.InlineExpansion` splices by `SymbolKey` at every
/// saturated use site — including the `App` its own pre-freeze eta mints for an
/// operator used as a VALUE (`List.fold (+) 0 xs`). Inline IL reaches codegen only as
/// the `ILIntrinsic` node a contract's per-primitive `when ^T : …` clause carries.
module EmitLower =

    let typeOfExpr = TastLower.typeOfExpr
    let typeOfPat = TastLower.typeOfPat
    // `inline` so the call sites keep `TastLower.receiverShape`'s inlining (a plain
    // re-export `let` would demote it to an allocated function value).
    let inline receiverShape ty = TastLower.receiverShape ty
    let matchInstantiation = TastLower.matchInstantiation
    let matchInstantiationPartial = TastLower.matchInstantiationPartial
    let iterChildren = TastLower.iterChildren
    let mintUnitParamKey = TastLower.mintUnitParamKey
    let mintTupleParamKey = TastLower.mintTupleParamKey
    let mintUseBinderKey = TastLower.mintUseBinderKey
    let peelLambda = TastLower.peelLambda

    /// The CLR backend's `lower`: the shared, platform-neutral `TastLower.lower`.
    let lower (decls: EqArray<Frozen.TDecl>) : Frozen.TDecl list = TastLower.lower decls
