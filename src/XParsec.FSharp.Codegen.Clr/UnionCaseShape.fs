namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

[<RequireQualifiedAccess>]
module UnionCaseFields =

    /// Whether a case's payload fields land on a `TypeDef` of their own: a case type in a
    /// hierarchy regime, or the union itself when it declares a single case.
    let ownType (regime: UnionRegime) : bool =
        UnionRegime.isHierarchy regime || regime = UnionRegime.SingleCase

    /// The metadata names of one case's payload fields, in declaration order: FSC's
    /// spelling (`of radius: float` ⇒ `_radius`, a positional field `item` / `item<n>`)
    /// where the fields have a `TypeDef` to themselves (`ownType`), else `<Case>_<i>`.
    let names (regime: UnionRegime) (caseName: string) (declared: string voption list) : string list =
        if ownType regime then
            UnionCaseFieldName.ofCase declared
            |> List.map (fun n ->
                match n with
                | UnionCaseFieldName.Declared name -> "_" + name
                | UnionCaseFieldName.Lone -> "item"
                | UnionCaseFieldName.Positional i -> "item" + string i
            )
        else
            declared |> List.mapi (fun i _ -> sprintf "%s_%d" caseName i)

[<RequireQualifiedAccess>]
module UnionCaseType =

    /// The `TypeKey` a hierarchy union's case type is registered under: the
    /// `TypeContainer.InType` container spells the emitted `Ns.Union`1+Case`, and the
    /// arity is 0 because a case adds no typars of its own.
    let key (unionKey: TypeKey) (caseName: string) : TypeKey =
        SymbolKeyOps.typeKeyOfContainer (TypeContainer.InType unionKey) caseName 0

    /// The case type at the union's own type arguments. Registered in `UserTypes`, and as a
    /// generic shape when the union is generic, so it encodes like any nominal.
    let ty (unionKey: TypeKey) (caseName: string) (args: FrozenType list) : FrozenType =
        FTClass(key unionKey caseName, EqArray.ofList args)
