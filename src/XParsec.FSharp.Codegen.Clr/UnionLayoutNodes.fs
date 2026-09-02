namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open XParsec.FSharp.SemanticAnalysis
open LayoutNodes

/// The `TypeNode`s of a union: the union itself, a hierarchy regime's case types, and a
/// `StructTagged` regime's nested storage and view value types.
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
                    // Written only by the case's own `.ctor`, hence `initonly`.
                    Attrs = FieldAttributes.Public ||| FieldAttributes.InitOnly
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
                    Typars = typarNames td.TypeParams
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
        | UnionNestedType.Payload slots -> [ for s in slots -> FieldKey.UnionSlot(td.Key, s.Key) ]
        | UnionNestedType.Overlay cases -> [ for c in cases -> FieldKey.UnionOverlayCase(td.Key, c.Case) ]
        | UnionNestedType.CaseData c -> [ for f in c.Fields -> FieldKey.UnionCaseDataField(td.Key, c.Case, f.Index) ]
        | UnionNestedType.CaseView v -> [ FieldKey.UnionCaseViewPayload(td.Key, v.Case.Name) ]

    /// The attributes of every field of `t`: a view's wrapped `Payload` is
    /// `private initonly`, written only by the view's own `.ctor`; a storage field is
    /// `assembly`-visible, read directly by match arms in this assembly.
    let private nestedFieldAttrs (t: UnionNestedType) : FieldAttributes =
        match t with
        | UnionNestedType.CaseView _ -> FieldAttributes.Private ||| FieldAttributes.InitOnly
        | UnionNestedType.Payload _
        | UnionNestedType.Overlay _
        | UnionNestedType.CaseData _ -> compilerGeneratedStorage

    /// The `TypeNode` of one value type nested in a `StructTagged` union: its `Slot`, its
    /// fields, and the method and property rows it declares. Redeclares a generic union's
    /// typars where `IsGeneric` holds.
    let private nestedNode
        (td: TastAccessor.TypeDecl)
        (t: UnionNestedType)
        (methods: MethodRow list)
        (properties: PropertySlot list)
        : TypeNode =
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
                    Namespace = ""
                    MetaName = t.Name
                    Typars = if t.IsGeneric then typarNames td.TypeParams else []
                }
            Enclosing = ValueSome(TypeSlotKey.Nominal td.Key)
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
                        Attrs = getterAttrs
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

        nestedNode td (UnionNestedType.CaseView v) methods properties

    /// The `TypeNode` of one value type a `StructTagged` union nests: a view declares its
    /// `.ctor` and properties; a storage type declares fields alone.
    let private unionNestedNode (td: TastAccessor.TypeDecl) (t: UnionNestedType) : TypeNode =
        match t with
        | UnionNestedType.CaseView v -> unionCaseViewNode td v
        | UnionNestedType.Payload _
        | UnionNestedType.Overlay _
        | UnionNestedType.CaseData _ -> nestedNode td t [] []

    /// Per union: `_tag`, a singleton field per nullary case, a flat regime's payload
    /// slots or its `_payload`, `.ctor`, case factories, members and structural rows,
    /// then the nested `TypeDef`s its regime calls for.
    let buildUnionNodes (symbols: ICodegenSymbols) (unions: UnionDecl list) : TypeNode list =
        [
            for ud in unions ->
                let td = ud.Decl
                let isHierarchy = ud.IsHierarchy
                let structural = StructuralMembers.ofUnion ud
                let singletonCases = ud.SingletonCases

                let selfTy =
                    FTUnion(td.TypeKey, EqArray.ofList (declaringMarkers td.TypeParams.Length))

                // A flat union's storage: `initonly` inline slots on its own `TypeDef`, or the
                // one `_payload` field with the value types nested behind it. A hierarchy
                // union declares each case's payload on the case's own nested `TypeDef`.
                let storageFields, nested =
                    match ud.Placements with
                    | ValueSome p ->
                        match p.Home with
                        | UnionSlotHome.Inline slots ->
                            [
                                for s in slots ->
                                    {
                                        Key = FieldKey.UnionSlot(td.Key, s.Key)
                                        Name = s.MetaName
                                        Attrs = FieldAttributes.Public ||| FieldAttributes.InitOnly
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
                                    Attrs = compilerGeneratedStorage ||| FieldAttributes.InitOnly
                                    Ty = UnionPayloadType.payloadTyDeclaring td.TypeKey td.TypeParams.Length
                                    ClosureScope = ValueNone
                                }
                            ],
                            [ for t in p.NestedTypes -> unionNestedNode td t ]
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
                                    Attrs = FieldAttributes.Private ||| FieldAttributes.InitOnly
                                    Ty = FTConst(RuntimeNames.intKey, EqArray.empty)
                                    ClosureScope = ValueNone
                                }

                        // The `<Case>` factory is the singleton's public accessor.
                        for (_, c) in singletonCases ->
                            {
                                Key = FieldKey.UnionCaseSingleton(td.Key, c.Name)
                                Name = "_unique_" + c.Name
                                Attrs = FieldAttributes.Private ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                                Ty = selfTy
                                ClosureScope = ValueNone
                            }

                        yield! storageFields
                    ]

                let methodRows =
                    [
                        yield
                            {
                                Key = MethodKey.NominalCtor td.Key
                                Name = ".ctor"
                                Attrs = ctorAttrs
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
                                    Attrs = getterAttrs
                                }

                        for c in ud.Cases do
                            yield
                                {
                                    Key = MethodKey.UnionFactory(td.Key, c.Name)
                                    Name = c.Name
                                    Attrs = staticFactoryAttrs
                                }

                        // The two public read surfaces of a `Payload` home: the
                        // `Get_<Case>_<i>` field readers, then the `Get_<Case>` view
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
                                ValueTy = FTConst(RuntimeNames.intKey, EqArray.empty)
                                Getter = ValueSome(MethodKey.UnionGetTag td.Key)
                                Setter = ValueNone
                            }

                        yield! ownAndIfaceProperties td.Key ud.Members ud.Interfaces
                    ]

                let node =
                    nominalNode (TypeSlotKind.Union(ud.ValueKind, ud.Regime)) td fields methodRows properties

                { node with Nested = nested }
        ]
