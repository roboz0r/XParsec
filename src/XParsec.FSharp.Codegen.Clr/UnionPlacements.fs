namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Identity of one physical field on a flat union's own `TypeDef`, scoped to the union.
[<RequireQualifiedAccess>]
type UnionSlotKey =
    /// A field owned by exactly one logical case field, stored at its declared type.
    | CaseField of case: string * index: int

/// One physical field on a flat union's `TypeDef`.
type UnionSlot =
    {
        Key: UnionSlotKey
        MetaName: string
        Ty: FrozenType
    }

/// The read path from a flat union value to one logical case field.
[<RequireQualifiedAccess>]
type UnionFieldAccess =
    /// `ldfld` the slot, whose stored type is the field's declared type.
    | Direct of UnionSlot

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionFieldAccess =

    /// The slot the read path ends on.
    let slot (access: UnionFieldAccess) : UnionSlot =
        match access with
        | UnionFieldAccess.Direct s -> s

/// The physical fields of a flat-regime union and the placement of every logical case field
/// among them. `Slots` is in `.ctor` parameter order, after `_tag` where the regime declares
/// one. Case factories, structural bodies and match arms read a logical field only through
/// its placement.
type FlatUnionPlacements =
    {
        Slots: UnionSlot list
        Fields: Map<string * int, UnionFieldAccess>
    }

    /// The read path to field `index` of case `caseName`.
    member this.Access(caseName: string, index: int) : UnionFieldAccess = this.Fields.[(caseName, index)]

    /// The read paths to one case's fields, in declaration order.
    member this.CaseAccess(c: Frozen.TUnionCase) : UnionFieldAccess list =
        [ for i in 0 .. c.Fields.Length - 1 -> this.Access(c.Name, i) ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FlatUnionPlacements =

    /// One slot per logical field, in case then field declaration order, named by
    /// `UnionCaseFields.names`. `regime` is a flat regime.
    let ofCases (regime: UnionRegime) (cases: Frozen.TUnionCase list) : FlatUnionPlacements =
        let slots =
            [
                for c in cases do
                    let names = UnionCaseFields.names regime c.Name [ for (n, _) in c.Fields -> n ]

                    for (fi, (name, (_, ty))) in List.indexed (List.zip names (EqArray.toList c.Fields)) ->
                        {
                            Key = UnionSlotKey.CaseField(c.Name, fi)
                            MetaName = name
                            Ty = ty
                        }
            ]

        {
            Slots = slots
            Fields =
                slots
                |> List.map (fun s ->
                    match s.Key with
                    | UnionSlotKey.CaseField(case, fi) -> (case, fi), UnionFieldAccess.Direct s
                )
                |> Map.ofList
        }
