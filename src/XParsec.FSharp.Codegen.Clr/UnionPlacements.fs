namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Identity of one physical slot of a flat union, scoped to the union. A slot is a field of
/// the union's own `TypeDef` (`UnionSlotHome.Inline`) or of its nested `Payload` struct.
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
    /// `_data`, the `ExplicitLayout` overlay holding every case's unmanaged fields.
    | Data

/// One physical slot of a flat union.
type UnionSlot =
    {
        Key: UnionSlotKey
        MetaName: string
        Ty: FrozenType
    }

/// One unmanaged case field, stored on its case's data struct inside the overlay.
type UnionOverlaidField =
    {
        Case: string
        /// The field's index in the case's declaration.
        Index: int
        /// The field's name on the case data struct, in FSC's spelling.
        MetaName: string
        Ty: FrozenType
    }

/// The data struct of one case: its unmanaged fields in declaration order. Only a case with
/// at least one unmanaged field has one.
type UnionCaseData =
    {
        Case: string
        Fields: UnionOverlaidField list
    }

/// The read path from a flat union value to one logical case field.
[<RequireQualifiedAccess>]
type UnionFieldAccess =
    /// `ldfld` the slot, whose stored type is the field's declared type.
    | Direct of UnionSlot
    /// `ldfld` the `object` slot, then `castclass` to `declared`.
    | Erased of slot: UnionSlot * declared: FrozenType
    /// `ldfld _data`, `ldfld` the case's data struct, `ldfld` the field.
    | Overlaid of UnionOverlaidField

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionFieldAccess =

    /// The read path's result type.
    let declaredTy (access: UnionFieldAccess) : FrozenType =
        match access with
        | UnionFieldAccess.Direct s -> s.Ty
        | UnionFieldAccess.Erased(_, declared) -> declared
        | UnionFieldAccess.Overlaid f -> f.Ty

    /// The `castclass` target of an `Erased` read.
    let cast (access: UnionFieldAccess) : FrozenType voption =
        match access with
        | UnionFieldAccess.Direct _
        | UnionFieldAccess.Overlaid _ -> ValueNone
        | UnionFieldAccess.Erased(_, declared) -> ValueSome declared

/// Where a flat union stores one logical case field.
[<RequireQualifiedAccess>]
type UnionStorage =
    /// A GC reference, erased into a shared `object` slot.
    | Reference
    /// Stored at its declared type, in a slot shared only with an identical type.
    | Exact
    /// A value holding no GC reference, stored on its case's data struct in the overlay.
    | Unmanaged

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

    /// How a field of type `t` is stored.
    let ofFrozen (symbols: ICodegenSymbols) (oracle: ILayoutOracle) (t: FrozenType) : UnionStorage =
        match Unmanagedness.ofFrozen symbols t with
        | Unmanagedness.Unmanaged -> UnionStorage.Unmanaged
        | Unmanagedness.Managed
        | Unmanagedness.Undetermined _ ->
            match TypeLayout.resolve oracle (TypeLayout.shapeOfFrozen t) with
            | TypeLayout.Reference -> UnionStorage.Reference
            | TypeLayout.Value
            | TypeLayout.Unsettled -> UnionStorage.Exact

/// The `_data` overlay of a `Payload`: its slot on `Payload` and the case data structs at
/// offset 0 of it, in tag order.
type UnionOverlay =
    {
        Slot: UnionSlot
        Cases: UnionCaseData list
    }

/// The fields of a `StructTagged` union's `Payload` struct: the overlay where any case has
/// an unmanaged field, the `object` slots, then the exact slots, each pool in first-claim
/// order.
type UnionPayloadSlots =
    {
        Overlay: UnionOverlay voption
        Refs: UnionSlot list
        Exacts: UnionSlot list
    }

    /// The case data structs of the overlay; empty where there is none.
    member this.OverlaidCases: UnionCaseData list =
        match this.Overlay with
        | ValueSome o -> o.Cases
        | ValueNone -> []

    /// `Payload`'s fields in row order: `_data` where the overlay exists, then the pools.
    member this.Slots: UnionSlot list =
        [
            match this.Overlay with
            | ValueSome o -> yield o.Slot
            | ValueNone -> ()

            yield! this.Refs
            yield! this.Exacts
        ]

/// The `TypeDef` a flat union's slots are declared on, with the slots themselves.
[<RequireQualifiedAccess>]
type UnionSlotHome =
    /// The union's own `TypeDef`: one slot per field of a `SingleCase` union, and none for
    /// an `EnumLike` one.
    | Inline of slots: UnionSlot list
    /// The nested `Payload` struct behind the union's `_payload` field.
    | Payload of UnionPayloadSlots

    /// The slots in field-row order on the home `TypeDef`, which for `Inline` is also
    /// `.ctor` parameter order after `_tag` where the regime declares one.
    member this.Slots: UnionSlot list =
        match this with
        | UnionSlotHome.Inline slots -> slots
        | UnionSlotHome.Payload p -> p.Slots

/// One value type nested in a `StructTagged` union behind `_payload`.
[<RequireQualifiedAccess>]
type UnionNestedType =
    /// `Payload`, holding the slots in field-row order and redeclaring a generic union's
    /// typars.
    | Payload of slots: UnionSlot list
    /// `Data`, the `ExplicitLayout` overlay: one field per case data struct, at offset 0.
    | Overlay of cases: UnionCaseData list
    /// `Data_<Case>`, sequential, holding the case's unmanaged fields.
    | CaseData of UnionCaseData

    member this.Name: string =
        match this with
        | UnionNestedType.Payload _ -> UnionPayloadType.payloadName
        | UnionNestedType.Overlay _ -> UnionPayloadType.overlayName
        | UnionNestedType.CaseData c -> UnionPayloadType.caseDataName c.Case

    /// The key the type is registered under, nested in `unionKey`.
    member this.TypeKey(unionKey: TypeKey) : TypeKey =
        match this with
        | UnionNestedType.Payload _ -> UnionPayloadType.payloadKey unionKey
        | UnionNestedType.Overlay _ -> UnionPayloadType.overlayKey unionKey
        | UnionNestedType.CaseData c -> UnionPayloadType.caseDataKey unionKey c.Case

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

/// The physical slots of a flat-regime union, the placement of every logical case field
/// among them, and the public readers over them.
type FlatUnionPlacements =
    {
        Home: UnionSlotHome
        Fields: Map<string * int, UnionFieldAccess>
        /// One reader per logical case field, in case then field declaration order, where
        /// `UnionRegime.hasCaseGetters` holds; else empty.
        Getters: UnionCaseGetter list
    }

    member this.Slots: UnionSlot list = this.Home.Slots

    /// The read path to field `index` of case `caseName`.
    member this.Access(caseName: string, index: int) : UnionFieldAccess = this.Fields.[(caseName, index)]

    /// The read paths to one case's fields, in declaration order.
    member this.CaseAccess(c: Frozen.TUnionCase) : UnionFieldAccess list =
        [ for i in 0 .. c.Fields.Length - 1 -> this.Access(c.Name, i) ]

    /// The case data structs of the overlay; empty where there is none.
    member this.OverlaidCases: UnionCaseData list =
        match this.Home with
        | UnionSlotHome.Payload p -> p.OverlaidCases
        | UnionSlotHome.Inline _ -> []

    /// The value types nested behind `_payload`, in `TypeDef` row order: `Payload`, then,
    /// where the overlay exists, `Data` and one `Data_<Case>` per overlaid case. Empty for
    /// an `Inline` home.
    member this.NestedTypes: UnionNestedType list =
        match this.Home with
        | UnionSlotHome.Inline _ -> []
        | UnionSlotHome.Payload p ->
            [
                yield UnionNestedType.Payload p.Slots

                match p.OverlaidCases with
                | [] -> ()
                | cases ->
                    yield UnionNestedType.Overlay cases

                    for c in cases -> UnionNestedType.CaseData c
            ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FlatUnionPlacements =

    let private objTy = FTConst(RuntimeNames.objKey, EqArray.empty)

    /// A home and the placement of every logical case field in it.
    type private Placed =
        {
            Home: UnionSlotHome
            Fields: ((string * int) * UnionFieldAccess) list
        }

    /// One slot per logical field on the union itself, in case then field declaration
    /// order, named by `UnionCaseFields.names`.
    let private unshared (cases: Frozen.TUnionCase list) : Placed =
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
            Home = UnionSlotHome.Inline slots
            Fields = fields
        }

    /// One pool of `Payload` slots, keyed and named by position, growing in first-claim
    /// order.
    type private SlotPool(key: int -> UnionSlotKey, name: int -> string) =
        let slots = ResizeArray<UnionSlot>()

        member _.Slots: UnionSlot list = List.ofSeq slots

        /// The first slot storing `stored` outside `claimed`, else a fresh one; the result
        /// joins `claimed`.
        member _.Claim(stored: FrozenType, claimed: HashSet<UnionSlotKey>) : UnionSlot =
            let slot =
                match slots |> Seq.tryFind (fun s -> s.Ty = stored && not (claimed.Contains s.Key)) with
                | Some s -> s
                | None ->
                    let i = slots.Count

                    let s =
                        {
                            Key = key i
                            MetaName = name i
                            Ty = stored
                        }

                    slots.Add s
                    s

            claimed.Add slot.Key |> ignore
            slot

    /// Cases overlap on the `Payload` struct: an unmanaged field on its case's data struct
    /// in the `_data` overlay, a reference field in an `object` slot, any other field in a
    /// slot of its exact type. The fields of one case take distinct slots.
    let private shared (symbols: ICodegenSymbols) (unionKey: TypeKey) (cases: Frozen.TUnionCase list) : Placed =
        let oracle = UnionStorage.layoutOracle symbols
        let refs = SlotPool(UnionSlotKey.RefSlot, sprintf "_ref%d")
        let exacts = SlotPool(UnionSlotKey.ExactSlot, sprintf "_val%d")
        let overlay = ResizeArray<UnionCaseData>()

        let fields =
            [
                for c in cases do
                    let claimed = HashSet<UnionSlotKey>()
                    let names = UnionCaseFields.names [ for (n, _) in c.Fields -> n ]
                    let dataFields = ResizeArray<UnionOverlaidField>()

                    for (fi, (name, (_, ty))) in List.indexed (List.zip names (EqArray.toList c.Fields)) do
                        let access =
                            match UnionStorage.ofFrozen symbols oracle ty with
                            | UnionStorage.Unmanaged ->
                                let f =
                                    {
                                        Case = c.Name
                                        Index = fi
                                        MetaName = name
                                        Ty = ty
                                    }

                                dataFields.Add f
                                UnionFieldAccess.Overlaid f
                            | UnionStorage.Reference -> UnionFieldAccess.Erased(refs.Claim(objTy, claimed), ty)
                            | UnionStorage.Exact -> UnionFieldAccess.Direct(exacts.Claim(ty, claimed))

                        yield (c.Name, fi), access

                    if dataFields.Count > 0 then
                        overlay.Add
                            {
                                Case = c.Name
                                Fields = List.ofSeq dataFields
                            }
            ]

        let overlay =
            match List.ofSeq overlay with
            | [] -> ValueNone
            | cases ->
                ValueSome
                    {
                        Slot =
                            {
                                Key = UnionSlotKey.Data
                                MetaName = UnionPayloadType.overlayFieldName
                                Ty = UnionPayloadType.overlayTy unionKey
                            }
                        Cases = cases
                    }

        {
            Home =
                UnionSlotHome.Payload
                    {
                        Overlay = overlay
                        Refs = refs.Slots
                        Exacts = exacts.Slots
                    }
            Fields = fields
        }

    /// The placements of a flat regime, `ValueNone` for a hierarchy one.
    let ofCases
        (symbols: ICodegenSymbols)
        (unionKey: TypeKey)
        (regime: UnionRegime)
        (cases: Frozen.TUnionCase list)
        : FlatUnionPlacements voption =
        let placed =
            match regime with
            | UnionRegime.SingleCase -> ValueSome(unshared cases)
            | UnionRegime.StructTagged -> ValueSome(shared symbols unionKey cases)
            | UnionRegime.EnumLike ->
                ValueSome
                    {
                        Home = UnionSlotHome.Inline []
                        Fields = []
                    }
            | UnionRegime.TypeTested
            | UnionRegime.Tagged -> ValueNone

        placed
        |> ValueOption.map (fun placed ->
            {
                Home = placed.Home
                Fields = Map.ofList placed.Fields
                Getters =
                    [
                        if UnionRegime.hasCaseGetters regime then
                            for ((case, index), access) in placed.Fields ->
                                {
                                    Case = case
                                    Index = index
                                    Access = access
                                }
                    ]
            }
        )
