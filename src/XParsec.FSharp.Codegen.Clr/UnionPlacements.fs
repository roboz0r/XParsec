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

/// One logical case field of a flat union: its read path and both spellings of its name.
type UnionCaseField =
    {
        Case: string
        /// The field's index in the case's declaration.
        Index: int
        /// The field's property name on its `Payload_<Case>` view, in F#'s own spelling: a
        /// declared name verbatim, `Item` for a lone positional field, `Item<n>` otherwise.
        PropertyName: string
        Access: UnionFieldAccess
    }

    /// The name of the union's `Get_<Case>_<i>` reader of the field.
    member this.GetterName: string = UnionCaseFields.getterName this.Case this.Index

    /// The field's declared type, which an `Erased` slot stores as `object`.
    member this.FieldTy: FrozenType = UnionFieldAccess.declaredTy this.Access

/// One case's fields and their placements, in declaration order. A payload-bearing case of
/// a `Payload` home is also the shape of its `Payload_<Case>` view: one get-only property
/// per field, reading through the wrapped `Payload`.
type UnionCasePlacement =
    {
        Case: Frozen.TUnionCase
        Fields: UnionCaseField list
    }

/// One value type a `StructTagged` union owns: the storage behind `_payload`, or a public
/// per-case view over it. `Payload` and the views are nested in the union; the overlay is
/// the union's sibling, with the case data structs nested in the overlay.
[<RequireQualifiedAccess>]
type UnionNestedType =
    /// `Payload`, holding the slots in field-row order and redeclaring a generic union's
    /// typars.
    | Payload of slots: UnionSlot list
    /// `<Union>$Data`, the `ExplicitLayout` overlay: one field per case data struct, at
    /// offset 0.
    | Overlay of cases: UnionCaseData list
    /// `Data_<Case>`, sequential, holding the case's unmanaged fields.
    | CaseData of UnionCaseData
    /// `Payload_<Case>`, the public view over one case's fields, redeclaring a generic
    /// union's typars.
    | CaseView of UnionCasePlacement

    member this.TypeKey(unionKey: TypeKey) : TypeKey =
        match this with
        | UnionNestedType.Payload _ -> UnionPayloadType.payloadKey unionKey
        | UnionNestedType.Overlay _ -> UnionPayloadType.overlayKey unionKey
        | UnionNestedType.CaseData c -> UnionPayloadType.caseDataKey unionKey c.Case
        | UnionNestedType.CaseView v -> UnionPayloadType.viewKey unionKey v.Case.Name

    /// The `TypeDef` name: one segment, with no arity suffix.
    member this.MetaName(unionKey: TypeKey) : string = (this.TypeKey unionKey).Name

    /// Whether the type redeclares the union's typars.
    member this.IsGeneric: bool =
        match this with
        | UnionNestedType.Payload _
        | UnionNestedType.CaseView _ -> true
        | UnionNestedType.Overlay _
        | UnionNestedType.CaseData _ -> false

    /// Whether the type declares a `.ctor`, taking each of its fields in row order. Only a
    /// view does; the storage types are written field by field.
    member this.HasCtor: bool =
        match this with
        | UnionNestedType.CaseView _ -> true
        | UnionNestedType.Payload _
        | UnionNestedType.Overlay _
        | UnionNestedType.CaseData _ -> false

    /// The type's fields as `(name, type)` in row order, in the scope of the union's own
    /// `arity` typars.
    member this.Fields(unionKey: TypeKey, arity: int) : (string * FrozenType) list =
        match this with
        | UnionNestedType.Payload slots -> [ for s in slots -> s.MetaName, s.Ty ]
        | UnionNestedType.Overlay cases -> [ for c in cases -> c.Case, UnionPayloadType.caseDataTy unionKey c.Case ]
        | UnionNestedType.CaseData c -> [ for f in c.Fields -> f.MetaName, f.Ty ]
        | UnionNestedType.CaseView _ ->
            [
                UnionPayloadType.payloadFieldName, UnionPayloadType.payloadTyDeclaring unionKey arity
            ]

/// The physical slots of a flat-regime union and the placement of every logical case field
/// among them.
type FlatUnionPlacements =
    {
        Home: UnionSlotHome
        /// One placement per case, in declaration order.
        Cases: UnionCasePlacement list
    }

    member this.Slots: UnionSlot list = this.Home.Slots

    /// The fields behind the union's public `Get_<Case>_<i>` readers, in case then field
    /// declaration order: every field of a `Payload` home. An `Inline` home's slots are
    /// read directly, so it declares none.
    member this.Getters: UnionCaseField list =
        match this.Home with
        | UnionSlotHome.Inline _ -> []
        | UnionSlotHome.Payload _ ->
            [
                for p in this.Cases do
                    yield! p.Fields
            ]

    /// The cases with a `Payload_<Case>` view: every payload-bearing case of a `Payload`
    /// home, in declaration order. An `Inline` home has none.
    member this.Views: UnionCasePlacement list =
        match this.Home with
        | UnionSlotHome.Inline _ -> []
        | UnionSlotHome.Payload _ -> this.Cases |> List.filter (fun p -> not p.Fields.IsEmpty)

    /// The case data structs of the overlay; empty where there is none.
    member this.OverlaidCases: UnionCaseData list =
        match this.Home with
        | UnionSlotHome.Payload p -> p.OverlaidCases
        | UnionSlotHome.Inline _ -> []

    /// The value types nested in the union, in `TypeDef` row order: `Payload`, then one
    /// `Payload_<Case>` per view. Empty for an `Inline` home.
    member this.NestedTypes: UnionNestedType list =
        match this.Home with
        | UnionSlotHome.Inline _ -> []
        | UnionSlotHome.Payload p ->
            [
                yield UnionNestedType.Payload p.Slots

                for v in this.Views -> UnionNestedType.CaseView v
            ]

    /// The overlay's subtree, in `TypeDef` row order: the overlay, then one `Data_<Case>`
    /// per overlaid case nested in it. Empty when no case is overlaid.
    member this.OverlayTypes: UnionNestedType list =
        match this.OverlaidCases with
        | [] -> []
        | cases ->
            [
                yield UnionNestedType.Overlay cases

                for c in cases -> UnionNestedType.CaseData c
            ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FlatUnionPlacements =

    let private objTy = FTConst(RuntimeNames.objKey, EqArray.empty)

    /// One declared case field with both spellings of its name.
    type private DeclaredField =
        {
            /// FSC's backing-field spelling: `_radius`, `item`, `item<n>`.
            MetaName: string
            /// F#'s own spelling: `radius`, `Item`, `Item<n>`.
            PropertyName: string
            Ty: FrozenType
        }

    /// A case's fields in declaration order.
    let private declaredFields (c: Frozen.TUnionCase) : DeclaredField list =
        let declared = [ for (n, _) in c.Fields -> n ]

        List.zip3
            (UnionCaseFieldName.fscFieldNames declared)
            (UnionCaseFieldName.fsharpNames declared)
            [ for (_, ty) in c.Fields -> ty ]
        |> List.map (fun (metaName, propertyName, ty) ->
            {
                MetaName = metaName
                PropertyName = propertyName
                Ty = ty
            }
        )

    /// A case's placement over the read path of each of its fields, in declaration order.
    let private placement (c: Frozen.TUnionCase) (accesses: UnionFieldAccess list) : UnionCasePlacement =
        {
            Case = c
            Fields =
                [
                    for (fi, (f, access)) in List.indexed (List.zip (declaredFields c) accesses) ->
                        {
                            Case = c.Name
                            Index = fi
                            PropertyName = f.PropertyName
                            Access = access
                        }
                ]
        }

    /// One slot per logical field on the union itself, in case then field declaration
    /// order, named by `UnionCaseFieldName.fscFieldNames`.
    let private unshared (cases: Frozen.TUnionCase list) : UnionSlotHome * UnionCasePlacement list =
        let caseSlots =
            [
                for c in cases ->
                    c,
                    [
                        for (fi, f) in List.indexed (declaredFields c) ->
                            {
                                Key = UnionSlotKey.CaseField(c.Name, fi)
                                MetaName = f.MetaName
                                Ty = f.Ty
                            }
                    ]
            ]

        UnionSlotHome.Inline
            [
                for (_, slots) in caseSlots do
                    yield! slots
            ],
        [
            for (c, slots) in caseSlots -> placement c (List.map UnionFieldAccess.Direct slots)
        ]

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
    let private shared
        (symbols: ICodegenSymbols)
        (unionKey: TypeKey)
        (cases: Frozen.TUnionCase list)
        : UnionSlotHome * UnionCasePlacement list =
        let oracle = UnionStorage.layoutOracle symbols
        let refs = SlotPool(UnionSlotKey.RefSlot, sprintf "_ref%d")
        let exacts = SlotPool(UnionSlotKey.ExactSlot, sprintf "_val%d")
        let overlay = ResizeArray<UnionCaseData>()

        let placements =
            [
                for c in cases ->
                    let claimed = HashSet<UnionSlotKey>()
                    let dataFields = ResizeArray<UnionOverlaidField>()

                    let accesses =
                        [
                            for (fi, f) in List.indexed (declaredFields c) ->
                                match UnionStorage.ofFrozen symbols oracle f.Ty with
                                | UnionStorage.Unmanaged ->
                                    let overlaid =
                                        {
                                            Case = c.Name
                                            Index = fi
                                            MetaName = f.MetaName
                                            Ty = f.Ty
                                        }

                                    dataFields.Add overlaid
                                    UnionFieldAccess.Overlaid overlaid
                                | UnionStorage.Reference -> UnionFieldAccess.Erased(refs.Claim(objTy, claimed), f.Ty)
                                | UnionStorage.Exact -> UnionFieldAccess.Direct(exacts.Claim(f.Ty, claimed))
                        ]

                    if dataFields.Count > 0 then
                        overlay.Add
                            {
                                Case = c.Name
                                Fields = List.ofSeq dataFields
                            }

                    placement c accesses
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

        UnionSlotHome.Payload
            {
                Overlay = overlay
                Refs = refs.Slots
                Exacts = exacts.Slots
            },
        placements

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
            | UnionRegime.EnumLike -> ValueSome(UnionSlotHome.Inline [], [ for c in cases -> placement c [] ])
            | UnionRegime.TypeTested
            | UnionRegime.Tagged -> ValueNone

        placed
        |> ValueOption.map (fun (home, placements) -> { Home = home; Cases = placements })
