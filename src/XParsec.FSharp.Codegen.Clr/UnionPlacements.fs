namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Identity of one physical field on a flat union's own `TypeDef`, scoped to the union.
[<RequireQualifiedAccess>]
type UnionSlotKey =
    /// A field owned by exactly one logical case field, stored at its declared type.
    | CaseField of case: string * index: int
    /// An `object` slot, holding whichever case's GC reference is active. A read
    /// `castclass`es to the field's declared type.
    | RefSlot of index: int
    /// A slot holding one stored type, shared across cases by every field of that exact
    /// type. The fallback for every non-reference field.
    | ExactSlot of index: int

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
    /// `ldfld` the `object` slot, then `castclass` to `declared`.
    | Erased of slot: UnionSlot * declared: FrozenType

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionFieldAccess =

    /// The slot the read path ends on.
    let slot (access: UnionFieldAccess) : UnionSlot =
        match access with
        | UnionFieldAccess.Direct s -> s
        | UnionFieldAccess.Erased(s, _) -> s

    /// The read path's result type.
    let declaredTy (access: UnionFieldAccess) : FrozenType =
        match access with
        | UnionFieldAccess.Direct s -> s.Ty
        | UnionFieldAccess.Erased(_, declared) -> declared

    /// The `castclass` target of an `Erased` read.
    let cast (access: UnionFieldAccess) : FrozenType voption =
        match access with
        | UnionFieldAccess.Direct _ -> ValueNone
        | UnionFieldAccess.Erased(_, declared) -> ValueSome declared

/// Where a flat union stores one logical case field.
[<RequireQualifiedAccess>]
type UnionStorage =
    /// A GC reference, erased into a shared `object` slot.
    | Reference
    /// Stored at its declared type, in a slot shared only with an identical type.
    | Exact

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionStorage =

    /// An `ILayoutOracle` over `ICodegenSymbols.IsValueType`, which already merges the
    /// target's layout over the declaration's request, so `Declared` adds nothing.
    let layoutOracle (symbols: ICodegenSymbols) : ILayoutOracle =
        { new ILayoutOracle with
            member _.Settled key =
                TypeLayout.ofSettled (symbols.IsValueType key)

            member _.Declared _ = TypeLayout.Unsettled
            member _.Platform = symbols.Platform
        }

    /// How a field of type `t` is stored. A typar, and any type whose layout `oracle`
    /// leaves unsettled, is `Exact`.
    let ofFrozen (oracle: ILayoutOracle) (t: FrozenType) : UnionStorage =
        match TypeLayout.resolve oracle (TypeLayout.shapeOfFrozen t) with
        | TypeLayout.Reference -> UnionStorage.Reference
        | TypeLayout.Value
        | TypeLayout.Unsettled -> UnionStorage.Exact

/// The physical fields of a flat-regime union and the placement of every logical case field
/// among them. `Slots` is in `.ctor` parameter order, after `_tag` where the regime declares
/// one.
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

/// One public `Get_<Case>_<i>` reader on a flat union: the logical case field it returns
/// and the read path its body is.
type UnionCaseGetter =
    {
        Case: string
        Index: int
        Access: UnionFieldAccess
    }

    member this.Name: string = UnionCaseFields.getterName this.Case this.Index

    /// The reader's return type.
    member this.FieldTy: FrozenType = UnionFieldAccess.declaredTy this.Access

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FlatUnionPlacements =

    let private objTy = FTConst(RuntimeNames.objKey, EqArray.empty)

    /// One slot per logical field, in case then field declaration order, named by
    /// `UnionCaseFields.names`.
    let private unshared (cases: Frozen.TUnionCase list) : FlatUnionPlacements =
        let slots, fields =
            [
                for c in cases do
                    let names = UnionCaseFields.names [ for (n, _) in c.Fields -> n ]

                    for (fi, (name, (_, ty))) in List.indexed (List.zip names (EqArray.toList c.Fields)) ->
                        let slot =
                            {
                                Key = UnionSlotKey.CaseField(c.Name, fi)
                                MetaName = name
                                Ty = ty
                            }

                        slot, ((c.Name, fi), UnionFieldAccess.Direct slot)
            ]
            |> List.unzip

        {
            Slots = slots
            Fields = Map.ofList fields
        }

    /// Cases overlap: a reference field is stored as `object`, any other field at its
    /// declared type, and a slot is shared by every field of its stored type. A case's own
    /// fields take distinct slots. Case then field declaration order fixes the assignment.
    let private shared (symbols: ICodegenSymbols) (cases: Frozen.TUnionCase list) : FlatUnionPlacements =
        let oracle = UnionStorage.layoutOracle symbols

        // In creation order, which is `.ctor` parameter order. `_ref<n>` and `_val<n>`
        // number independently.
        let slots = ResizeArray<UnionSlot>()
        let mutable refCount = 0
        let mutable valCount = 0

        let mint (stored: FrozenType) (erased: bool) : UnionSlot =
            let slot =
                if erased then
                    let i = refCount
                    refCount <- i + 1

                    {
                        Key = UnionSlotKey.RefSlot i
                        MetaName = sprintf "_ref%d" i
                        Ty = stored
                    }
                else
                    let i = valCount
                    valCount <- i + 1

                    {
                        Key = UnionSlotKey.ExactSlot i
                        MetaName = sprintf "_val%d" i
                        Ty = stored
                    }

            slots.Add slot
            slot

        let fields =
            [
                for c in cases do
                    let claimed = HashSet<UnionSlotKey>()

                    for (fi, (_, ty)) in List.indexed (EqArray.toList c.Fields) ->
                        let stored, cast =
                            match UnionStorage.ofFrozen oracle ty with
                            | UnionStorage.Reference -> objTy, ValueSome ty
                            | UnionStorage.Exact -> ty, ValueNone

                        let slot =
                            match slots |> Seq.tryFind (fun s -> s.Ty = stored && not (claimed.Contains s.Key)) with
                            | Some s -> s
                            | None -> mint stored cast.IsSome

                        claimed.Add slot.Key |> ignore

                        (c.Name, fi),
                        (match cast with
                         | ValueSome declared -> UnionFieldAccess.Erased(slot, declared)
                         | ValueNone -> UnionFieldAccess.Direct slot)
            ]

        {
            Slots = List.ofSeq slots
            Fields = Map.ofList fields
        }

    /// The placements of a flat regime. Cases overlap exactly where their fields have no
    /// `TypeDef` of their own (`UnionCaseFields.ownType`); a `SingleCase` union keeps one
    /// slot per field at its declared type.
    let ofCases (symbols: ICodegenSymbols) (regime: UnionRegime) (cases: Frozen.TUnionCase list) : FlatUnionPlacements =
        if UnionCaseFields.ownType regime then
            unshared cases
        else
            shared symbols cases

    /// One `Get_<Case>_<i>` reader per logical case field, in case then field declaration
    /// order.
    let caseGetters (p: FlatUnionPlacements) (cases: Frozen.TUnionCase list) : UnionCaseGetter list =
        [
            for c in cases do
                for fi in 0 .. c.Fields.Length - 1 ->
                    {
                        Case = c.Name
                        Index = fi
                        Access = p.Access(c.Name, fi)
                    }
        ]
