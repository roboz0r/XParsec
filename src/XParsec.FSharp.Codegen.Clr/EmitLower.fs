namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

module EmitLower =

    let typeOfExpr = TastAccessor.exprTy
    let typeOfPat = TastAccessor.patTy
    let nominalOfExpr = TastAccessor.exprNominalTy
    let nominalOfPat = TastAccessor.patNominalTy
    let matchInstantiation = TastLower.matchInstantiation
    let matchInstantiationPartial = TastLower.matchInstantiationPartial
    let iterChildren = TastAccessor.iterChildren
    let peelLambda = TastLower.peelLambda

    let lower (decls: TastAccessor.DeclId list) : TastAccessor.DeclId list = TastLower.lower decls
