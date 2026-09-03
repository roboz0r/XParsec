namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationTranslate

// Resolving an `inherit` clause's PARENT, at registration time and in the scope the clause is
// written.

module NameResolutionInheritParent =

    /// The token an `inherit` rejection is reported at: the parent's name, or the `inherit`
    /// keyword for a shape with no name (`inherit (int * int)`).
    let rec private nameTokOf (inhTok: SyntaxToken) (t: Type<SyntaxToken>) : SyntaxToken =
        match t with
        | Type.ParenType(typ = inner) -> nameTokOf inhTok inner
        | Type.NamedType li
        | Type.GenericType(longIdent = li)
        | Type.SuffixedType(longIdent = li) -> li.Idents.[li.Idents.Length - 1]
        | _ -> inhTok

    /// Resolve an `inherit` clause's parent type to a nominal. The caller runs it under the
    /// derived class's typar scope, so a `'a` in the clause is the class's prototype TyVar.
    let resolveInheritParent (ctx: PassContext) (inhTok: SyntaxToken) (t: Type<SyntaxToken>) : BaseParent voption =
        BaseEligibility.classify
            (BaseEligibility.isInterfaceKey ctx)
            (BaseEligibility.isHeritableCanon ctx)
            (translateType ctx t)
        |> BaseEligibility.admit ctx (nameTokOf inhTok t)
