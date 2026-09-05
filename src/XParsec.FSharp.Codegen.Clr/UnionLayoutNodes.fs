namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open XParsec.FSharp.SemanticAnalysis
open LayoutNodes

/// The `TypeNode`s of a union: the union itself, a hierarchy regime's case types, and a
/// `StructTagged` regime's nested payload and view value types.
module internal UnionLayoutNodes =

    /// The row one structural slot takes on a hierarchy union's case type. The `Union`-typed
    /// entries and `GetHashCode` override the slots the base declares; the `Case`-typed pair
    /// declares no slot of its own and binds by `call`.
    let private unionCaseSlotRow (td: TastAccessor.TypeDecl) (caseName: string) (slot: UnionCaseSlot) : MethodRow =
        let attrs =
            match slot with
            | UnionCaseSlot.GetHashCode
            | UnionCaseSlot.EqualsUnion
            | UnionCaseSlot.CompareToUnion
            | UnionCaseSlot.Format -> overrideMethodAttrs
            | UnionCaseSlot.EqualsCase
            | UnionCaseSlot.CompareToCase -> instanceMethodAttrs

        {
            Key = MethodKey.UnionCaseStructural(td.Key, caseName, slot)
            Name = UnionCaseSlot.metaName slot
            Attrs = attrs
        }

    /// One case's nested `TypeDef` in a hierarchy union: its own payload fields, its
    /// `.ctor`, and the structural bodies the base declares abstract. A generic union's
    /// case redeclares the union's typars and adds none, so its name has no arity suffix.
    let private unionCaseNode (ud: UnionDecl) (structural: StructuralMembers) (c: Frozen.TUnionCase) : TypeNode =
        let td = ud.Decl

        let fields =
            List.zip (ud.FieldNames c) (EqArray.toList c.Fields)
            |> List.mapi (fun fi (name, (_, fty)) ->
                {
                    Key = FieldKey.UnionCaseField(td.Key, c.Name, fi)
                    Name = name
                    Attrs = instanceFieldAttrs FieldReach.Public FieldWrites.ByCtor
                    Ty = fty
                    ClosureScope = ValueNone
                }
            )

        let methodRows =
            [
                {
                    Key = MethodKey.UnionCaseCtor(td.Key, c.Name)
                    Name = ".ctor"
                    Attrs = ctorAttrs
                }
                yield! UnionCaseSlot.required structural |> List.map (unionCaseSlotRow td c.Name)
            ]

        {
            Slot =
                {
                    Key = TypeSlotKey.UnionCase(td.Key, c.Name)
                    Kind = TypeSlotKind.UnionCase
                    Namespace = ""
                    MetaName = c.Name
                    Typars = GenericParamRow.ofTypars (TTypeParam.names td.TypeParams) td.TyparConstraints
                }
            Enclosing = ValueSome(TypeSlotKey.Nominal td.Key)
            Fields = fields
            Methods = methodRows
            Properties = []
            Nested = []
        }

    /// The `FieldKey` of each field of `t`, in the order of `UnionNestedType.Fields`.
    let private nestedFieldKeys (td: TastAccessor.TypeDecl) (t: UnionNestedType) : FieldKey list =
        match t with
        | UnionNestedType.Payload(UnionPayloadStruct.Payload slots) ->
            [ for s in slots -> FieldKey.UnionSlot(td.Key, s.Key) ]
        | UnionNestedType.Payload(UnionPayloadStruct.Overlay cases) ->
            [ for c in cases -> FieldKey.UnionOverlayCase(td.Key, c.Case) ]
        | UnionNestedType.Payload(UnionPayloadStruct.CaseData c) ->
            [ for f in c.Fields -> FieldKey.UnionCaseDataField(td.Key, c.Case, f.Index) ]
        | UnionNestedType.View v -> [ FieldKey.UnionCaseViewPayload(td.Key, v.Case.Name) ]

    /// The attributes of every field of `t`: a view's wrapped `Payload` is reached only by the
    /// view's own members; a payload field is read directly by match arms in this assembly.
    let private nestedFieldAttrs (t: UnionNestedType) : FieldAttributes =
        match t with
        | UnionNestedType.View _ -> instanceFieldAttrs FieldReach.OwnType FieldWrites.ByCtor
        // A payload field stays writable pending an audit of its store sites, on the terms A1
        // set for the ctor-param and capture fields.
        | UnionNestedType.Payload _ -> instanceFieldAttrs FieldReach.Assembly FieldWrites.Anywhere

    /// Where an owned type's `TypeDef` row sits: `Payload` and the views under the union,
    /// the overlay beside the union in its container, a case data struct under the overlay.
    let private ownedPlacement (td: TastAccessor.TypeDecl) (t: UnionNestedType) : string * TypeSlotKey voption =
        match t with
        | UnionNestedType.Payload(UnionPayloadStruct.Payload _)
        | UnionNestedType.View _ -> "", ValueSome(TypeSlotKey.Nominal td.Key)
        | UnionNestedType.Payload(UnionPayloadStruct.Overlay _) -> containerPlacement td
        | UnionNestedType.Payload(UnionPayloadStruct.CaseData _) -> "", ValueSome(TypeSlotKey.UnionOverlay td.Key)

    /// The `TypeNode` of one value type a `StructTagged` union owns: its `Slot`, its
    /// fields, and the method and property rows it declares. Redeclares a generic union's
    /// typars where `IsGeneric` holds.
    let private ownedNode
        (td: TastAccessor.TypeDecl)
        (t: UnionNestedType)
        (methods: MethodRow list)
        (properties: PropertySlot list)
        : TypeNode =
        let ns, enclosing = ownedPlacement td t

        let fields =
            List.map2
                (fun key (name, ty) ->
                    {
                        Key = key
                        Name = name
                        Attrs = nestedFieldAttrs t
                        Ty = ty
                        ClosureScope = ValueNone
                    }
                )
                (nestedFieldKeys td t)
                (t.Fields(td.TypeKey, td.TypeParams.Length))

        {
            Slot =
                {
                    Key = UnionNestedType.slotKey td.Key t
                    Kind = UnionNestedType.slotKind t
                    Namespace = ns
                    MetaName = t.MetaName td.TypeKey
                    Typars =
                        if t.IsGeneric then
                            GenericParamRow.ofTypars (TTypeParam.names td.TypeParams) td.TyparConstraints
                        else
                            []
                }
            Enclosing = enclosing
            Fields = fields
            Methods = methods
            Properties = properties
            Nested = []
        }

    /// The `TypeNode` of one `Payload_<Case>` view: the `assembly .ctor` writing its
    /// wrapped `Payload`, and one get-only property per logical field.
    let private unionCaseViewNode (td: TastAccessor.TypeDecl) (v: UnionCasePlacement) : TypeNode =
        let caseName = v.Case.Name

        let methods =
            [
                yield
                    {
                        Key = MethodKey.UnionCaseViewCtor(td.Key, caseName)
                        Name = ".ctor"
                        Attrs = assemblyCtorAttrs
                    }

                for f in v.Fields ->
                    {
                        Key = MethodKey.UnionCaseViewGetter(td.Key, caseName, f.Index)
                        Name = AccessorNames.getterName f.PropertyName
                        Attrs = synthAccessorAttrs
                    }
            ]

        let properties =
            [
                for f in v.Fields ->
                    {
                        Key = PropertyKey.UnionCaseViewField(td.Key, caseName, f.Index)
                        Name = f.PropertyName
                        IsInstance = true
                        IndexTys = []
                        ValueTy = f.FieldTy
                        Getter = ValueSome(MethodKey.UnionCaseViewGetter(td.Key, caseName, f.Index))
                        Setter = ValueNone
                    }
            ]

        ownedNode td (UnionNestedType.View v) methods properties

    /// The `TypeNode` of one value type a `StructTagged` union owns: a view declares its
    /// `.ctor` and properties; a payload struct declares fields alone.
    let private unionOwnedNode (td: TastAccessor.TypeDecl) (t: UnionNestedType) : TypeNode =
        match t with
        | UnionNestedType.View v -> unionCaseViewNode td v
        | UnionNestedType.Payload _ -> ownedNode td t [] []

    /// The overlay `TypeNode` with one `Data_<Case>` node nested in it per overlaid case.
    let private unionOverlayNode (td: TastAccessor.TypeDecl) (cases: UnionCaseData list) : TypeNode =
        { unionOwnedNode td (UnionNestedType.Payload(UnionPayloadStruct.Overlay cases)) with
            Nested =
                [
                    for c in cases -> unionOwnedNode td (UnionNestedType.Payload(UnionPayloadStruct.CaseData c))
                ]
        }

    /// Per union: `_tag`, a singleton field per nullary case, a flat regime's payload
    /// slots or its `_payload`, `.ctor`, case factories, members and structural rows,
    /// then the nested `TypeDef`s its regime calls for. A union with an overlay is followed
    /// by the overlay's node.
    let buildUnionNodes (symbols: ICodegenSymbols) (unions: UnionDecl list) : TypeNode list =
        [
            for ud in unions do
                let td = ud.Decl
                let isHierarchy = ud.IsHierarchy
                let structural = StructuralMembers.ofUnion ud
                let singletonCases = ud.SingletonCases

                let selfTy =
                    FTUnion(td.TypeKey, EqArray.ofList (declaringMarkers td.TypeParams.Length))

                // A flat union's payload fields: `initonly` inline slots on its own `TypeDef`,
                // or the one `_payload` field with the value types nested behind it. A
                // hierarchy union declares each case's payload on the case's own nested `TypeDef`.
                let payloadFields, nested =
                    match ud.Placements with
                    | ValueSome p ->
                        match p.Home with
                        | UnionSlotHome.Inline slots ->
                            [
                                for s in slots ->
                                    {
                                        Key = FieldKey.UnionSlot(td.Key, s.Key)
                                        Name = s.MetaName
                                        Attrs = instanceFieldAttrs FieldReach.Public FieldWrites.ByCtor
                                        Ty = s.Ty
                                        ClosureScope = ValueNone
                                    }
                            ],
                            []
                        | UnionSlotHome.Payload _ ->
                            [
                                {
                                    Key = FieldKey.UnionPayload td.Key
                                    Name = UnionPayloadType.payloadFieldName
                                    Attrs = instanceFieldAttrs FieldReach.Assembly FieldWrites.ByCtor
                                    Ty = UnionPayloadType.payloadTyDeclaring td.TypeKey td.TypeParams.Length
                                    ClosureScope = ValueNone
                                }
                            ],
                            [ for t in p.NestedTypes -> unionOwnedNode td t ]
                    | ValueNone -> [], [ for c in ud.Cases -> unionCaseNode ud structural c ]

                let fields =
                    [
                        // A single-case union's sole case needs no discriminant, and its
                        // FSC-spelled payload may itself claim the name `_tag`
                        // (`C of tag: int`).
                        if ud.HasTag then
                            yield
                                {
                                    Key = FieldKey.UnionTag td.Key
                                    Name = "_tag"
                                    Attrs = instanceFieldAttrs FieldReach.OwnType FieldWrites.ByCtor
                                    Ty = RuntimeNames.intTy
                                    ClosureScope = ValueNone
                                }

                        // The `<Case>` factory is the singleton's public accessor.
                        for (_, c) in singletonCases ->
                            {
                                Key = FieldKey.UnionCaseSingleton(td.Key, c.Name)
                                Name = "_unique_" + c.Name
                                Attrs = staticFieldAttrs FieldReach.OwnType FieldWrites.ByCtor
                                Ty = selfTy
                                ClosureScope = ValueNone
                            }

                        yield! payloadFields
                    ]

                let methodRows =
                    [
                        yield
                            {
                                Key = MethodKey.NominalCtor td.Key
                                Name = ".ctor"
                                Attrs = assemblyCtorAttrs
                            }

                        // The `.cctor` constructs each nullary case's singleton once, so a
                        // nullary construction site stops allocating.
                        if not (List.isEmpty singletonCases) then
                            yield
                                {
                                    Key = MethodKey.NominalCctor td.Key
                                    Name = ".cctor"
                                    Attrs = cctorAttrs
                                }

                        if ud.HasTag then
                            yield
                                {
                                    Key = MethodKey.UnionGetTag td.Key
                                    Name = "get_Tag"
                                    Attrs = synthAccessorAttrs
                                }

                        for c in ud.Cases do
                            yield
                                {
                                    Key = MethodKey.UnionFactory(td.Key, c.Name)
                                    Name = c.Name
                                    Attrs = staticFactoryAttrs
                                }

                        // The two public read surfaces of a `Payload` home: the
                        // `Get_<Case>_<i>` field readers, then the `GetPayload_<Case>` view
                        // accessors.
                        match ud.Placements with
                        | ValueSome p ->
                            for g in p.Getters ->
                                {
                                    Key = MethodKey.UnionCaseGetter(td.Key, g.Case, g.Index)
                                    Name = g.GetterName
                                    Attrs = instanceMethodAttrs
                                }

                            for v in p.Views ->
                                {
                                    Key = MethodKey.UnionCaseViewAccessor(td.Key, v.Case.Name)
                                    Name = UnionCaseFields.viewGetterName v.Case.Name
                                    Attrs = instanceMethodAttrs
                                }
                        | ValueNone -> ()

                        yield! ownAndIfaceMemberRows td.Key ud.Members ud.Interfaces

                        let attrs =
                            if isHierarchy then
                                abstractStructuralAttrs
                            else
                                concreteStructuralAttrs

                        yield! structuralRows attrs structural td
                        yield! coSlotRows symbols td ud.Interfaces
                    ]

                let properties =
                    [
                        // The public reader of the union's private `_tag`, declared exactly
                        // where the discriminant is.
                        if ud.HasTag then
                            {
                                Key = PropertyKey.UnionTag td.Key
                                Name = "Tag"
                                IsInstance = true
                                IndexTys = []
                                ValueTy = RuntimeNames.intTy
                                Getter = ValueSome(MethodKey.UnionGetTag td.Key)
                                Setter = ValueNone
                            }

                        yield! ownAndIfaceProperties td.Key ud.Members ud.Interfaces
                    ]

                let node =
                    nominalNode (TypeSlotKind.Union(ud.ValueKind, ud.Regime)) td fields methodRows properties

                yield { node with Nested = nested }

                match ud.Placements with
                | ValueSome p ->
                    match p.OverlaidCases with
                    | [] -> ()
                    | cases -> yield unionOverlayNode td cases
                | ValueNone -> ()
        ]
