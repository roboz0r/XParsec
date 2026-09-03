namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold
open NominalShared

/// Union emission: the `.ctor`, the case factories, and, in a hierarchy regime, each
/// case's own nested type, its `.ctor`, and the structural bodies the base declares
/// abstract.
module internal UnionEmit =


    /// The self-describing type of a case, in the scope of the union's own typars.
    let private caseTyOf (td: TastAccessor.TypeDecl) (caseName: string) : FrozenType =
        UnionCaseType.ty td.TypeKey caseName (typarMarkersOf td)

    /// The `Payload` struct in the union's own scope.
    let private payloadTyOf (td: TastAccessor.TypeDecl) : FrozenType =
        UnionPayloadType.payloadTyDeclaring td.TypeKey td.TypeParams.Length

    /// A case's `Payload_<Case>` view in the union's own scope.
    let private viewTyOf (td: TastAccessor.TypeDecl) (caseName: string) : FrozenType =
        UnionPayloadType.viewTy td.TypeKey caseName (typarMarkersOf td)

    // ---- Read paths ------------------------------------------------------------------

    /// One step of a read path, by the field's layout key.
    type private LayoutStep = EmitTypes.FieldStep<FieldKey>

    let private payloadStep (td: TastAccessor.TypeDecl) : LayoutStep =
        EmitTypes.FieldStep.Member(FieldKey.UnionPayload td.Key, UnionMember.Payload)

    let private slotStep (td: TastAccessor.TypeDecl) (key: UnionSlotKey) : LayoutStep =
        EmitTypes.FieldStep.Member(FieldKey.UnionSlot(td.Key, key), UnionMember.Slot key)

    /// A hierarchy case's field `fi`, on the case's own type.
    let private caseFieldStep (td: TastAccessor.TypeDecl) (c: Frozen.TUnionCase) (fi: int) : LayoutStep =
        EmitTypes.FieldStep.Member(FieldKey.UnionCaseField(td.Key, c.Name, fi), UnionMember.Field(c.Name, fi))

    /// The steps from a union value to the `TypeDef` its slots are declared on: `_payload`
    /// for a `Payload` home, none for an `Inline` one.
    let private homePathOf (td: TastAccessor.TypeDecl) (p: FlatUnionPlacements) : LayoutStep list =
        match p.Home with
        | UnionSlotHome.Inline _ -> []
        | UnionSlotHome.Payload _ -> [ payloadStep td ]

    /// The steps from the home `TypeDef`'s value to one logical field: the structs stepped
    /// through (`_data` and the case's data struct for an overlaid field) and the field.
    let private fieldPathOf (td: TastAccessor.TypeDecl) (access: UnionFieldAccess) : LayoutStep list * LayoutStep =
        match access with
        | UnionFieldAccess.Direct s
        | UnionFieldAccess.Erased(s, _) -> [], slotStep td s.Key
        | UnionFieldAccess.Overlaid f ->
            [
                slotStep td UnionSlotKey.Data
                EmitTypes.FieldStep.Def(FieldKey.UnionOverlayCase(td.Key, f.Case))
            ],
            EmitTypes.FieldStep.Def(FieldKey.UnionCaseDataField(td.Key, f.Case, f.Index))

    /// `fieldPathOf` as one chain: the steps from the home `TypeDef`'s value to one
    /// logical field.
    let private fieldStepsOf (td: TastAccessor.TypeDecl) (access: UnionFieldAccess) : LayoutStep list =
        let via, field = fieldPathOf td access
        [ yield! via; yield field ]

    /// The steps from a union value to one logical field.
    let private readPathOf
        (td: TastAccessor.TypeDecl)
        (p: FlatUnionPlacements)
        (access: UnionFieldAccess)
        : LayoutStep list =
        [ yield! homePathOf td p; yield! fieldStepsOf td access ]

    /// The handle of one step: a generic union's `MemberRef` on its own `TypeSpec` for a
    /// `Member` step, the `Def` token otherwise. A generic union mints a row per call.
    let private stepRef (asm: Assembler) (td: TastAccessor.TypeDecl) (step: LayoutStep) : EntityHandle =
        match step with
        | EmitTypes.FieldStep.Member(key, member') ->
            selfMemberRef asm td (UserMemberKind.UnionMember member') (toEntity (asm.FieldDef key))
        | EmitTypes.FieldStep.Def key -> toEntity (asm.FieldDef key)

    /// A flat union's placements and the handle of every step a read path can take, each
    /// minted once per pass.
    type private FlatPass =
        {
            Placements: FlatUnionPlacements
            Refs: IReadOnlyDictionary<FieldKey, EntityHandle>
        }

    /// Every step a read path over `p` can take: `_payload`, each slot, and each overlay
    /// case struct and its fields.
    let private stepsOf (td: TastAccessor.TypeDecl) (p: FlatUnionPlacements) : LayoutStep list =
        [
            yield! homePathOf td p

            for s in p.Slots -> slotStep td s.Key

            for c in p.OverlaidCases do
                yield EmitTypes.FieldStep.Def(FieldKey.UnionOverlayCase(td.Key, c.Case))

                for f in c.Fields -> EmitTypes.FieldStep.Def(FieldKey.UnionCaseDataField(td.Key, c.Case, f.Index))
        ]

    let private flatPassOf (asm: Assembler) (td: TastAccessor.TypeDecl) (p: FlatUnionPlacements) : FlatPass =
        {
            Placements = p
            Refs = readOnlyDict [ for s in stepsOf td p -> EmitTypes.FieldStep.field s, stepRef asm td s ]
        }

    let private handleOf (pass: FlatPass) (step: LayoutStep) : EntityHandle =
        pass.Refs.[EmitTypes.FieldStep.field step]

    let private handlesOf (pass: FlatPass) (steps: LayoutStep list) : EntityHandle list =
        [ for s in steps -> handleOf pass s ]

    /// A flat case's structural fields in declaration order: the walk every structural body
    /// over that case takes, `castclass`ing an erased slot back to the declared type.
    let private flatCaseFieldsOf
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (pass: FlatPass)
        (c: UnionCasePlacement)
        : EmitStructural.StructuralField list =
        [
            for f in c.Fields ->
                {
                    Path = handlesOf pass (readPathOf td pass.Placements f.Access)
                    Ty = f.FieldTy
                    Cast = UnionFieldAccess.cast f.Access |> ValueOption.map asm.Icodegen.TypeToken
                    Compare = asm.FieldCompareOf f.FieldTy
                }
        ]

    /// A hierarchy case's payload field refs in declaration order, parented on the case
    /// type.
    let private caseFieldRefsOf
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (c: Frozen.TUnionCase)
        : EntityHandle list =
        [ for fi in 0 .. c.Fields.Length - 1 -> stepRef asm td (caseFieldStep td c fi) ]

    /// A hierarchy case's structural fields in declaration order, each its own field on
    /// the case type.
    let private hierarchyCaseFieldsOf
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (c: Frozen.TUnionCase)
        : EmitStructural.StructuralField list =
        [
            for (h, (_, t)) in List.zip (caseFieldRefsOf asm td c) (EqArray.toList c.Fields) ->
                {
                    Path = [ h ]
                    Ty = t
                    Cast = ValueNone
                    Compare = asm.FieldCompareOf t
                }
        ]

    /// One `.ctor` parameter after `_tag`, stored into the field of the same name.
    type private CtorParam =
        {
            Name: string
            Ty: FrozenType
            Field: EntityHandle
        }

    // ---- The union's own rows --------------------------------------------------------

    /// The body of one field reader: the `ldfld` chain from `root` through the field's
    /// placement, `castclass`ing back where the slot stores `object`.
    let private fieldGetterIr
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (pass: FlatPass)
        (root: EntityHandle list)
        (access: UnionFieldAccess)
        : ILBody =
        Emit.buildFieldPathGetter
            [ yield! root; yield! handlesOf pass (fieldStepsOf td access) ]
            (UnionFieldAccess.cast access |> ValueOption.map asm.Icodegen.TypeToken)

    /// One `Payload_<Case>` view's bodies: its `.ctor` writing the wrapped `Payload`, one
    /// property getter per field rooted at that wrapped field, and the union's `GetPayload_<Case>`
    /// copying `_payload` into a fresh view.
    let private prepareCaseView
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (pass: FlatPass)
        (v: UnionCasePlacement)
        : unit =
        let provider = asm.Provider
        let caseName = v.Case.Name

        // A generic union's view is registered as a generic class over the union's typars,
        // so its `.ctor` and wrapped field are minted on the view's own `TypeSpec`.
        let viewMemberRef (which: ClassMember) (monoHandle: EntityHandle) : EntityHandle =
            memberRefOn
                asm
                td
                (UnionPayloadType.viewKey td.TypeKey caseName)
                (UserMemberKind.ClassMember which)
                monoHandle

        let viewPayloadField =
            viewMemberRef
                (ClassMember.Field UnionPayloadType.payloadFieldName)
                (toEntity (asm.FieldDef(FieldKey.UnionCaseViewPayload(td.Key, caseName))))

        asm.AddPrepared(
            MethodKey.UnionCaseViewCtor(td.Key, caseName),
            {
                Signature = provider.RecordCtorSignature [ payloadTyOf td ]
                Body = bodyOf asm (Emit.buildStructCtor [ viewPayloadField ])
                ParamNames = [ UnionPayloadType.payloadFieldName ]
                MethodTypars = []
            }
        )

        for f in v.Fields do
            asm.AddPrepared(
                MethodKey.UnionCaseViewGetter(td.Key, caseName, f.Index),
                {
                    Signature = provider.InstanceMethodSignature([], f.FieldTy)
                    Body = bodyOf asm (fieldGetterIr asm td pass [ viewPayloadField ] f.Access)
                    ParamNames = []
                    MethodTypars = []
                }
            )

        let viewCtor =
            viewMemberRef ClassMember.Ctor (toEntity (asm.MethodDef(MethodKey.UnionCaseViewCtor(td.Key, caseName))))

        asm.AddPrepared(
            MethodKey.UnionCaseViewAccessor(td.Key, caseName),
            {
                Signature = provider.InstanceMethodSignature([], viewTyOf td caseName)
                Body = bodyOf asm (Emit.buildUnionCaseViewGetter (handleOf pass (payloadStep td)) viewCtor)
                ParamNames = []
                MethodTypars = []
            }
        )

    /// The union `.ctor`, the `.cctor` a reference union's nullary singletons need, and one
    /// static factory per case.
    let prepareUnion (asm: Assembler) (ud: UnionDecl) : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let cases = ud.Cases
        let isStruct = ud.ValueKind.IsValueType
        let isHierarchy = ud.IsHierarchy
        let selfTy = FTUnion(td.TypeKey, EqArray.ofList (typarMarkersOf td))

        // The `_tag` ref, shared by the `.ctor` and `get_Tag`: a generic union mints a
        // `MemberRef` row per call.
        let tagRef: EntityHandle voption =
            if ud.HasTag then
                ValueSome(
                    selfMemberRef
                        asm
                        td
                        (UserMemberKind.UnionMember UnionMember.Tag)
                        (toEntity (asm.FieldDef(FieldKey.UnionTag td.Key)))
                )
            else
                ValueNone

        // A flat union's field refs, shared by the `.ctor`, the struct factories and the
        // `Get_<Case>_<i>` readers.
        let flat = ud.Placements |> ValueOption.map (flatPassOf asm td)

        // Each case beside its placement, in tag order; a hierarchy union yields `ValueNone`
        // throughout, since it declares each case's payload on the case type.
        let placedCases: (Frozen.TUnionCase * UnionCasePlacement voption) list =
            match flat with
            | ValueSome pass -> [ for p in pass.Placements.Cases -> p.Case, ValueSome p ]
            | ValueNone -> [ for c in cases -> c, ValueNone ]

        let flatCaseOf (placement: UnionCasePlacement voption) : FlatPass * UnionCasePlacement =
            match flat, placement with
            | ValueSome pass, ValueSome p -> pass, p
            | _ -> failwithf "Emit: flat union '%s' has no placements" td.Name

        // The `.ctor`'s parameters after `_tag`: every inline slot in placement order, or
        // the one `_payload`.
        let ctorParams: CtorParam list =
            match flat with
            | ValueSome pass ->
                match pass.Placements.Home with
                | UnionSlotHome.Payload _ ->
                    [
                        {
                            Name = UnionPayloadType.payloadFieldName
                            Ty = payloadTyOf td
                            Field = handleOf pass (payloadStep td)
                        }
                    ]
                | UnionSlotHome.Inline slots ->
                    [
                        for s in slots ->
                            {
                                Name = s.MetaName
                                Ty = s.Ty
                                Field = handleOf pass (slotStep td s.Key)
                            }
                    ]
            | ValueNone -> []

        // The `.ctor` stores `_tag` where the regime declares one, then its payload
        // parameters. A value type chains no base `.ctor`.
        let ctorFields =
            [ yield! ValueOption.toList tagRef; for p in ctorParams -> p.Field ]

        asm.AddPrepared(
            MethodKey.NominalCtor td.Key,
            {
                Signature =
                    provider.RecordCtorSignature
                        [
                            if ud.HasTag then
                                yield RuntimeNames.intTy
                            for p in ctorParams -> p.Ty
                        ]
                Body =
                    bodyOf
                        asm
                        (if isStruct then
                             Emit.buildStructCtor ctorFields
                         else
                             Emit.buildChainedCtor provider.ObjectCtorRef [] ctorFields)
                ParamNames =
                    [
                        if ud.HasTag then
                            yield "_tag"
                        for p in ctorParams -> p.Name
                    ]
                MethodTypars = []
            }
        )

        match tagRef with
        | ValueSome t ->
            asm.AddPrepared(
                MethodKey.UnionGetTag td.Key,
                {
                    Signature = provider.InstanceMethodSignature([], RuntimeNames.intTy)
                    Body = bodyOf asm (Emit.buildFieldGetter t)
                    ParamNames = []
                    MethodTypars = []
                }
            )
        | ValueNone -> ()

        let ctorRef =
            selfMemberRef
                asm
                td
                (UserMemberKind.UnionMember UnionMember.Ctor)
                (toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key)))

        // A case's own `.ctor` ref, and the singleton field a nullary case's factory loads.
        let caseCtorRef (c: Frozen.TUnionCase) =
            selfMemberRef
                asm
                td
                (UserMemberKind.UnionMember(UnionMember.CaseCtor c.Name))
                (toEntity (asm.MethodDef(MethodKey.UnionCaseCtor(td.Key, c.Name))))

        let singletonRef (c: Frozen.TUnionCase) =
            selfMemberRef
                asm
                td
                (UserMemberKind.UnionMember(UnionMember.CaseSingleton c.Name))
                (toEntity (asm.FieldDef(FieldKey.UnionCaseSingleton(td.Key, c.Name))))

        // The arguments one case passes to the union's own `.ctor`: `TagOnly` stamps the
        // discriminant.
        let ctorTagArgs (tag: int) =
            match ud.CtorShape with
            | UnionCtorShape.TagOnly -> [ ILInstr.LdcI4 tag ]
            | UnionCtorShape.Nullary
            | UnionCtorShape.Flat
            | UnionCtorShape.FlatTagged -> []

        if isHierarchy then
            // Each case's `.ctor(payload…)` chains the union's own `.ctor`, passing its tag
            // where the base declares one.
            cases
            |> List.iteri (fun tag c ->
                asm.AddPrepared(
                    MethodKey.UnionCaseCtor(td.Key, c.Name),
                    {
                        Signature = provider.RecordCtorSignature [ for (_, t) in c.Fields -> t ]
                        Body = bodyOf asm (Emit.buildChainedCtor ctorRef (ctorTagArgs tag) (caseFieldRefsOf asm td c))
                        ParamNames = ud.FieldNames c
                        MethodTypars = []
                    }
                )
            )

        match ud.SingletonCases with
        | [] -> ()
        | singletons ->
            let entries =
                [
                    for (tag, c) in singletons ->
                        // A hierarchy case constructs its own type over no arguments; a
                        // flat one IS the union, stamped with its tag where the `.ctor`
                        // declares one.
                        let ctor, ctorArgs =
                            if isHierarchy then
                                caseCtorRef c, []
                            else
                                ctorRef, ctorTagArgs tag

                        ctorArgs @ [ ILInstr.Newobj(ctor, List.length ctorArgs) ], singletonRef c
                ]

            asm.AddPrepared(
                MethodKey.NominalCctor td.Key,
                {
                    Signature = provider.CctorSignature()
                    Body = bodyOf asm (Emit.buildCachedFieldCctor entries)
                    ParamNames = []
                    MethodTypars = []
                }
            )

        placedCases
        |> List.iteri (fun tag (c, placement) ->
            let arity = c.Fields.Length

            let factoryIr =
                match UnionFactoryShape.ofCase ud.ValueKind ud.Regime arity with
                | UnionFactoryShape.StructTagged ->
                    let pass, placed = flatCaseOf placement

                    // Each parameter lands in this case's own placement; every other slot
                    // keeps the `initobj` zero.
                    let stores =
                        [
                            for f in placed.Fields ->
                                let via, field = fieldPathOf td f.Access

                                ({
                                    Arg = f.Index
                                    Via = handlesOf pass via
                                    Field = handleOf pass field
                                }
                                : Emit.PayloadStore)
                        ]

                    let payloadTy = payloadTyOf td

                    Emit.buildStructUnionPayloadFactory ctorRef tag payloadTy (asm.Icodegen.TypeToken payloadTy) stores
                | UnionFactoryShape.StructTag -> Emit.buildStructUnionTagFactory ctorRef tag
                | UnionFactoryShape.Cached -> Emit.buildUnionSingletonFactory (singletonRef c)
                | UnionFactoryShape.CaseCtor -> Emit.buildUnionCaseFactory (caseCtorRef c) arity
                | UnionFactoryShape.UnionCtor -> Emit.buildUnionCaseFactory ctorRef arity

            let factoryBody = bodyOf asm factoryIr
            let paramTys = [ for (_, t) in c.Fields -> t ]

            asm.AddPrepared(
                MethodKey.UnionFactory(td.Key, c.Name),
                {
                    Signature = provider.StaticMethodSignature(paramTys, selfTy)
                    Body = factoryBody
                    ParamNames = argNames arity
                    MethodTypars = []
                }
            )
        )

        // The two read surfaces of a `Payload` home: the `Get_<Case>_<i>` readers on the
        // union, then the `Payload_<Case>` views.
        match flat with
        | ValueNone -> ()
        | ValueSome pass ->
            // A union reader's chain hangs off `this`, stepping through `_payload`.
            let unionRoot = handlesOf pass (homePathOf td pass.Placements)

            for g in pass.Placements.Getters do
                asm.AddPrepared(
                    MethodKey.UnionCaseGetter(td.Key, g.Case, g.Index),
                    {
                        Signature = provider.InstanceMethodSignature([], g.FieldTy)
                        Body = bodyOf asm (fieldGetterIr asm td pass unionRoot g.Access)
                        ParamNames = []
                        MethodTypars = []
                    }
                )

            for v in pass.Placements.Views do
                prepareCaseView asm td pass v

    // ---- Structural bodies -----------------------------------------------------------

    /// A case type's own structural bodies, one per slot `UnionCaseSlot.required` lists.
    /// `caseType` is this case's own token, minted once by the caller (the `TypeSpec`
    /// table is append-only); `otherOrdinal` is forced only for a comparison slot.
    let private prepareCaseStructural
        (asm: Assembler)
        (ud: UnionDecl)
        (self: StructuralSelf)
        (tag: int)
        (c: Frozen.TUnionCase)
        (caseType: EntityHandle)
        (otherOrdinal: unit -> EmitStructural.OtherOrdinal)
        : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let handles = asm.Structural
        let caseName = c.Name
        let caseTy = caseTyOf td caseName

        let fields = hierarchyCaseFieldsOf asm td c
        let walk = EmitStructural.StructuralWalk.Flat(ValueSome tag, fields)

        let prepared slot signature paramNames ir =
            asm.AddPrepared(
                MethodKey.UnionCaseStructural(td.Key, caseName, slot),
                {
                    Signature = signature
                    Body = bodyOf asm ir
                    ParamNames = paramNames
                    MethodTypars = []
                }
            )

        // A case type's own synthesised method, minted on the case's `TypeSpec` for a
        // generic union.
        let caseSlotRef paramTys retTy slot =
            memberRefOn
                asm
                td
                (UnionCaseType.key td.TypeKey caseName)
                (UserMemberKind.Member(UnionCaseSlot.metaName slot, false, 0, paramTys, retTy))
                (toEntity (asm.MethodDef(MethodKey.UnionCaseStructural(td.Key, caseName, slot))))

        for slot in UnionCaseSlot.required self.Members do
            match slot with
            | UnionCaseSlot.EqualsCase ->
                prepared
                    slot
                    (provider.EqualsTypedSignature caseTy)
                    [ "other" ]
                    (EmitStructural.buildEqualsTyped handles false walk)

            | UnionCaseSlot.EqualsUnion ->
                let equalsCase = caseSlotRef [ caseTy ] RuntimeNames.boolTy UnionCaseSlot.EqualsCase

                prepared
                    slot
                    (provider.EqualsTypedSignature self.SelfTy)
                    [ "other" ]
                    (EmitStructural.buildUnionCaseEqualsUnion caseType equalsCase)

            | UnionCaseSlot.GetHashCode ->
                prepared
                    slot
                    (provider.GetHashCodeOverrideSignature())
                    []
                    (EmitStructural.buildGetHashCode handles walk)

            | UnionCaseSlot.CompareToCase ->
                prepared
                    slot
                    (provider.CompareToTypedSignature caseTy)
                    [ "other" ]
                    (EmitStructural.buildCompareTo handles false walk)

            | UnionCaseSlot.CompareToUnion ->
                let compareToCase =
                    caseSlotRef [ caseTy ] RuntimeNames.intTy UnionCaseSlot.CompareToCase

                prepared
                    slot
                    (provider.CompareToTypedSignature self.SelfTy)
                    [ "other" ]
                    (EmitStructural.buildUnionCaseCompareToUnion caseType caseTy compareToCase tag (otherOrdinal ()))

            | UnionCaseSlot.Format ->
                prepared
                    slot
                    (provider.StructuralFormatSignature())
                    [ "sink" ]
                    (EmitStructuralFormat.buildUnionCaseFormat handles { Name = caseName; Fields = fields })

    /// A hierarchy base's own structural bodies: `Equals(object)` and `CompareTo(object)`
    /// cast and dispatch to the abstract typed slots, and every other entry is a slot the
    /// case types implement.
    let private prepareHierarchyBase (asm: Assembler) (ud: UnionDecl) (self: StructuralSelf) : unit =
        let td = ud.Decl
        let provider = asm.Provider

        if self.Members.Equality then
            prepareEqualityTriple
                asm
                td
                self.SelfTy
                ValueNone
                (EmitStructural.buildUnionBaseEqualsObj self.SelfType (equalsTyped asm td self))
                ValueNone

        if self.Members.Comparison then
            let typedCompareTo = compareToTyped asm td self

            prepareComparisonPair
                asm
                td
                self.SelfTy
                ValueNone
                (EmitStructural.buildCompareToObj
                    asm.Structural
                    false
                    self.SelfType
                    self.SelfTy
                    (EmitStructural.TypedEntry.Virtual typedCompareTo))

        if self.Members.Format then
            asm.AddPrepared(
                MethodKey.FmtFormat td.Key,
                {
                    Signature = provider.StructuralFormatSignature()
                    Body = PreparedBody.Abstract
                    ParamNames = [ "sink" ]
                    MethodTypars = []
                }
            )

    /// A flat union's own structural bodies. One pass mints the field and tag refs that
    /// equality, comparison and `%A` share. Once the tags agree, `_tag` selects the active
    /// case's own field walk, because cases share slots.
    let private prepareFlatStructural
        (asm: Assembler)
        (ud: UnionDecl)
        (p: FlatUnionPlacements)
        (self: StructuralSelf)
        : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let handles = asm.Structural
        let isStruct = ud.ValueKind.IsValueType

        // Minted on first use and shared from there.
        let perCase =
            lazy
                (let pass = flatPassOf asm td p
                 [ for c in p.Cases -> c.Case.Name, flatCaseFieldsOf asm td pass c ])

        let tagField = lazy (tagFieldRefOf asm td)

        let walk: Lazy<EmitStructural.StructuralWalk> =
            lazy
                (if ud.HasTag then
                     EmitStructural.StructuralWalk.Tagged(tagField.Value, [ for (_, fs) in perCase.Value -> fs ])
                 else
                     EmitStructural.StructuralWalk.Flat(ValueNone, perCase.Value |> List.collect snd))

        if self.Members.Equality then
            prepareEqualityTriple
                asm
                td
                self.SelfTy
                (ValueSome(EmitStructural.buildGetHashCode handles walk.Value))
                (EmitStructural.buildEqualsObj
                    isStruct
                    self.SelfType
                    self.SelfTy
                    (EmitStructural.TypedEntry.Direct(equalsTyped asm td self)))
                (ValueSome(EmitStructural.buildEqualsTyped handles isStruct walk.Value))

        if self.Members.Comparison then

            prepareComparisonPair
                asm
                td
                self.SelfTy
                (ValueSome(EmitStructural.buildCompareTo handles isStruct walk.Value))
                (EmitStructural.buildCompareToObj
                    handles
                    isStruct
                    self.SelfType
                    self.SelfTy
                    (EmitStructural.TypedEntry.Direct(compareToTyped asm td self)))

        if self.Members.Format then
            let cases =
                [
                    for (name, fields) in perCase.Value ->
                        {
                            EmitStructuralFormat.UnionFormatCase.Name = name
                            EmitStructuralFormat.UnionFormatCase.Fields = fields
                        }
                ]

            let formatIr =
                match ud.Regime, cases with
                // A single-case union renders its sole case straight through: the same
                // body a hierarchy case type carries.
                | UnionRegime.SingleCase, [ sole ] -> EmitStructuralFormat.buildUnionCaseFormat handles sole
                | _ -> EmitStructuralFormat.buildUnionFormat handles tagField.Value cases

            asm.AddPrepared(
                MethodKey.FmtFormat td.Key,
                {
                    Signature = provider.StructuralFormatSignature()
                    Body = bodyOf asm formatIr
                    ParamNames = [ "sink" ]
                    MethodTypars = []
                }
            )

    /// Every body behind the union's synthesised structural rows; in a hierarchy regime,
    /// also each case type's rows and the `TypeRowExtras` its `TypeDefinition` needs.
    let prepareStructural (asm: Assembler) (ud: UnionDecl) (self: StructuralSelf) : unit =
        match ud.Placements with
        | ValueSome p -> prepareFlatStructural asm ud p self
        | ValueNone ->
            let td = ud.Decl
            prepareHierarchyBase asm ud self

            // Every case's own token, in tag order, minted once here: each case's bodies
            // use its own, and `TypeTested`'s `CompareTo(U)` walks the others'.
            let caseTokens = [ for c in ud.Cases -> asm.Icodegen.TypeToken(caseTyOf td c.Name) ]

            // `other`'s ordinal for one case's `CompareTo(U)`, the one discriminant read
            // a hierarchy body still takes: `Tagged` loads `_tag`, `TypeTested` walks the
            // other cases' types.
            let otherOrdinal tag =
                match ud.Regime with
                | UnionRegime.Tagged -> EmitStructural.OtherOrdinal.TagField(tagFieldRefOf asm td)
                | UnionRegime.TypeTested ->
                    EmitStructural.OtherOrdinal.TypeTests(
                        caseTokens
                        |> List.mapi (fun otherTag token -> token, otherTag)
                        |> List.filter (fun (_, otherTag) -> otherTag <> tag)
                    )
                | UnionRegime.SingleCase
                | UnionRegime.EnumLike
                | UnionRegime.StructTagged ->
                    failwithf "Emit: flat union '%s' declares no hierarchy case bodies" td.Name

            List.zip ud.Cases caseTokens
            |> List.iteri (fun tag (c, caseType) ->
                prepareCaseStructural asm ud self tag c caseType (fun () -> otherOrdinal tag)

                asm.AddTypeRowExtras(
                    TypeSlotKey.UnionCase(td.Key, c.Name),
                    {
                        Interfaces = []
                        // Every case extends the one base handle, minted once: a generic
                        // union's is a `TypeSpec` row, and that table is append-only.
                        BaseType = self.SelfType
                    }
                )
            )

    /// A step by its `Def` token, for a match arm to resolve at its own instantiation.
    let private emittedStep (asm: Assembler) (step: LayoutStep) : EmitTypes.FieldStep<EntityHandle> =
        EmitTypes.FieldStep.map (asm.FieldDef >> toEntity) step

    /// The read path a match arm takes to one case field.
    let private emittedCaseField (asm: Assembler) (steps: LayoutStep list) (erased: bool) : EmitTypes.EmittedCaseField =
        {
            Steps = [ for s in steps -> emittedStep asm s ]
            Erased = erased
        }

    /// The registry entry every construction, match and member call site resolves through.
    let register
        (asm: Assembler)
        (ud: UnionDecl)
        (emittedMembers: Dictionary<string, EqArray<Emit.EmittedMember>>)
        : unit =
        let td = ud.Decl
        let emittedCases = Dictionary<string, Emit.EmittedCase>()

        // Each case's field read paths, in tag order: through its placement for a flat
        // union, or its own fields on the case type for a hierarchy one.
        let caseFields: (Frozen.TUnionCase * EmitTypes.EmittedCaseField list) list =
            match ud.Placements with
            | ValueSome p ->
                [
                    for c in p.Cases ->
                        c.Case,
                        [
                            for f in c.Fields ->
                                emittedCaseField asm (readPathOf td p f.Access) (UnionFieldAccess.cast f.Access).IsSome
                        ]
                ]
            | ValueNone ->
                [
                    for c in ud.Cases ->
                        c,
                        [
                            for fi in 0 .. c.Fields.Length - 1 -> emittedCaseField asm [ caseFieldStep td c fi ] false
                        ]
                ]

        caseFields
        |> List.iteri (fun tag (c, fields) ->
            emittedCases.[c.Name] <-
                {
                    Tag = tag
                    Factory = toEntity (asm.MethodDef(MethodKey.UnionFactory(td.Key, c.Name)))
                    Fields = fields
                    CaseType =
                        if ud.IsHierarchy then
                            ValueSome(UnionCaseType.key td.TypeKey c.Name)
                        else
                            ValueNone
                }
        )

        asm.Unions.[td.TypeKey] <-
            {
                Name = td.Name
                Typars = EqArray.toList (TTypeParam.names td.TypeParams)
                Tag =
                    if ud.HasTag then
                        ValueSome
                            {
                                Field = toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))
                                Getter = toEntity (asm.MethodDef(MethodKey.UnionGetTag td.Key))
                            }
                    else
                        ValueNone
                ValueKind = ud.ValueKind
                Cases = emittedCases
                Members = emittedMembers
            }
