namespace XParsec.FSharp.Codegen.Clr

open Vesper
open XParsec.FSharp.Parser
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

    /// The args as a `list`: the shape the emitted-nominal tables and `ICodegenProvider` take.
    let keyAndTyArgs (n: FrozenNominal) : TypeKey * FrozenType list = n.Key, Block.toList n.Args

    let lower (decls: TastAccessor.DeclId list) : TastAccessor.DeclId list = TastLower.lower decls
