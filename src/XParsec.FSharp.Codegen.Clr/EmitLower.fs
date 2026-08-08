namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

module EmitLower =

    let typeOfExpr = TastAccessor.exprTy
    let typeOfPat = TastAccessor.patTy
    // `inline` so call sites keep `TastLower.objArgShape`'s own inlining.
    let inline objArgShape ty = TastLower.objArgShape ty
    let matchInstantiation = TastLower.matchInstantiation
    let matchInstantiationPartial = TastLower.matchInstantiationPartial
    let iterChildren = TastAccessor.iterChildren
    let peelLambda = TastLower.peelLambda

    let lower (decls: TastAccessor.DeclId list) : TastAccessor.DeclId list = TastLower.lower decls
