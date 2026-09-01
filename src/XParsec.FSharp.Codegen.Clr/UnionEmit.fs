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

    let private intTy = FTConst(RuntimeNames.intKey, EqArray.empty)
    let private boolTy = FTConst(RuntimeNames.boolKey, EqArray.empty)

    /// A member ref to one of a CASE type's own synthesised methods: a generic union
    /// parents it on the case's own `TypeSpec` over the union's typar markers, a
    /// monomorphic one uses the resolved `Def` token.
    let private caseMethodRef
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (caseName: string)
        (metaName: string)
        (paramTys: FrozenType list)
        (retTy: FrozenType)
        (monoHandle: EntityHandle)
        : EntityHandle =
        if td.TypeParams.IsEmpty then
            monoHandle
        else
            asm.Icodegen.UserGenericMemberRef(
                UnionCaseType.key td.TypeKey caseName,
                typarMarkersOf td,
                UserMemberKind.Member(metaName, false, 0, paramTys, retTy)
            )

    /// The self-describing type of a case, in the scope of the union's own typars.
    let private caseTyOf (td: TastAccessor.TypeDecl) (caseName: string) : FrozenType =
        UnionCaseType.ty td.TypeKey caseName (typarMarkersOf td)

    /// The `FieldDef` key one logical case field is read from: its placement slot on a
    /// flat union, or the case type's own field in a hierarchy regime.
    let private caseFieldKey (ud: UnionDecl) (c: Frozen.TUnionCase) (fi: int) : FieldKey =
        match ud.Placements with
        | ValueSome p -> FieldKey.UnionSlot(ud.Decl.Key, (UnionFieldAccess.slot (p.Access(c.Name, fi))).Key)
        | ValueNone -> FieldKey.UnionCaseField(ud.Decl.Key, c.Name, fi)

    /// A case's payload field refs in declaration order, via the `UnionMember.Field`
    /// spelling, which reparents onto the case type in a hierarchy regime.
    let private fieldRefsOf (asm: Assembler) (ud: UnionDecl) (c: Frozen.TUnionCase) : EntityHandle list =
        [
            for fi in 0 .. c.Fields.Length - 1 ->
                selfMemberRef
                    asm
                    ud.Decl
                    (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi)))
                    (toEntity (asm.FieldDef(caseFieldKey ud c fi)))
        ]

    /// A flat union's physical slots with their refs, in `.ctor` parameter order.
    let private slotRefsOf (asm: Assembler) (ud: UnionDecl) (p: FlatUnionPlacements) : (UnionSlot * EntityHandle) list =
        [
            for s in p.Slots ->
                let member' =
                    match s.Key with
                    | UnionSlotKey.CaseField(case, fi) -> UnionMember.Field(case, fi)

                s,
                selfMemberRef
                    asm
                    ud.Decl
                    (UserMemberKind.UnionMember member')
                    (toEntity (asm.FieldDef(FieldKey.UnionSlot(ud.Decl.Key, s.Key))))
        ]

    /// A case's `(field ref, declared type)` pairs in declaration order: the walk every
    /// structural body over that case takes. Each call mints a generic union's `MemberRef`
    /// rows afresh; call once and share the result across bodies.
    let private caseFieldsOf
        (asm: Assembler)
        (ud: UnionDecl)
        (c: Frozen.TUnionCase)
        : (EntityHandle * FrozenType) list =
        List.zip (fieldRefsOf asm ud c) [ for (_, t) in c.Fields -> t ]

    // ---- The union's own rows --------------------------------------------------------

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

        // A flat union's slots with their refs, in `.ctor` order, shared by the `.ctor`,
        // the struct factories and the `Get_<Case>_<i>` readers.
        let slotRefs: (UnionSlot * EntityHandle) list =
            match ud.Placements with
            | ValueSome p -> slotRefsOf asm ud p
            | ValueNone -> []

        // The `.ctor` stores `_tag` where the regime declares one, then every slot in
        // placement order. A value type chains no base `.ctor`.
        let ctorFields = [ yield! ValueOption.toList tagRef; for (_, h) in slotRefs -> h ]

        asm.AddPrepared(
            MethodKey.NominalCtor td.Key,
            {
                Signature =
                    provider.RecordCtorSignature
                        [
                            if ud.HasTag then
                                yield intTy
                            for (s, _) in slotRefs -> s.Ty
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
                        for (s, _) in slotRefs -> s.MetaName
                    ]
                MethodTypars = []
            }
        )

        match tagRef with
        | ValueSome t ->
            asm.AddPrepared(
                MethodKey.UnionGetTag td.Key,
                {
                    Signature = provider.InstanceMethodSignature([], intTy)
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
                        Body = bodyOf asm (Emit.buildChainedCtor ctorRef (ctorTagArgs tag) (fieldRefsOf asm ud c))
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

        cases
        |> List.iteri (fun tag c ->
            let arity = c.Fields.Length

            let factoryIr =
                match UnionFactoryShape.ofCase ud.ValueKind ud.Regime arity with
                | UnionFactoryShape.StructTagged ->
                    // This case's field index by the key of the slot storing it.
                    let ownedSlots =
                        match ud.Placements with
                        | ValueSome p ->
                            p.CaseAccess c
                            |> List.mapi (fun fi a -> (UnionFieldAccess.slot a).Key, fi)
                            |> Map.ofList
                        | ValueNone -> failwithf "Emit: struct union '%s' has no placements" td.Name

                    let args =
                        [
                            for (s, _) in slotRefs ->
                                match Map.tryFind s.Key ownedSlots with
                                | Some fi -> Emit.StructUnionCtorArg.Param fi
                                | None -> Emit.StructUnionCtorArg.Default(s.Ty, asm.Icodegen.TypeToken s.Ty)
                        ]

                    Emit.buildStructUnionFactory ctorRef tag args
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

        // The `Get_<Case>_<i>` readers, each `ldfld`ing its slot in place on `this`.
        let refOfSlot = slotRefs |> List.map (fun (s, h) -> s.Key, h) |> Map.ofList

        for g in ud.CaseGetters do
            asm.AddPrepared(
                MethodKey.UnionCaseGetter(td.Key, g.Case, g.Index),
                {
                    Signature = provider.InstanceMethodSignature([], g.FieldTy)
                    Body = bodyOf asm (Emit.buildFieldGetter refOfSlot.[g.Slot.Key])
                    ParamNames = []
                    MethodTypars = []
                }
            )

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

        let walk: EmitStructural.StructuralWalk =
            {
                Fields = caseFieldsOf asm ud c
                Discriminant = EmitStructural.Discriminant.CaseTag tag
            }

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

        let caseSlotRef paramTys retTy slot =
            caseMethodRef
                asm
                td
                caseName
                (UnionCaseSlot.metaName slot)
                paramTys
                retTy
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
                let equalsCase = caseSlotRef [ caseTy ] boolTy UnionCaseSlot.EqualsCase

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
                let compareToCase = caseSlotRef [ caseTy ] intTy UnionCaseSlot.CompareToCase

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
                    (EmitStructuralFormat.buildUnionCaseFormat
                        handles
                        {
                            Name = caseName
                            Fields = walk.Fields
                        })

    /// A hierarchy base's own structural bodies: `Equals(object)` and `CompareTo(object)`
    /// cast and dispatch to the abstract typed slots, and every other entry is a slot the
    /// case types implement.
    let private prepareHierarchyBase (asm: Assembler) (ud: UnionDecl) (self: StructuralSelf) : unit =
        let td = ud.Decl
        let provider = asm.Provider

        if self.Members.Equality then
            let equalsUnion =
                selfMemberRef
                    asm
                    td
                    (UserMemberKind.Member("Equals", false, 0, [ self.SelfTy ], boolTy))
                    (toEntity (asm.MethodDef(MethodKey.EqEqualsTyped td.Key)))

            prepareEqualityTriple
                asm
                td
                self.SelfTy
                ValueNone
                (EmitStructural.buildUnionBaseEqualsObj self.SelfType equalsUnion)
                ValueNone

        if self.Members.Comparison then
            let typedCompareTo =
                selfMemberRef
                    asm
                    td
                    (UserMemberKind.Member("CompareTo", false, 0, [ self.SelfTy ], intTy))
                    (toEntity (asm.MethodDef(MethodKey.CmpCompareToTyped td.Key)))

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
    /// equality, comparison and `%A` share. Once the tags agree the walk crosses every
    /// case's fields; an inactive case's fields hold their default.
    let private prepareFlatStructural (asm: Assembler) (ud: UnionDecl) (self: StructuralSelf) : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let handles = asm.Structural
        let isStruct = ud.ValueKind.IsValueType

        // Minted on first use and shared from there.
        let perCase = lazy [ for c in ud.Cases -> c.Name, caseFieldsOf asm ud c ]
        let tagField = lazy (tagFieldRefOf asm td)

        let walk: Lazy<EmitStructural.StructuralWalk> =
            lazy
                {
                    Fields = perCase.Value |> List.collect snd
                    Discriminant =
                        if ud.HasTag then
                            EmitStructural.Discriminant.TagField tagField.Value
                        else
                            EmitStructural.Discriminant.None
                }

        if self.Members.Equality then
            prepareEqualityTriple
                asm
                td
                self.SelfTy
                (ValueSome(EmitStructural.buildGetHashCode handles walk.Value))
                (EmitStructural.buildEqualsObj handles isStruct self.SelfType self.SelfTy walk.Value)
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
                    (EmitStructural.TypedEntry.Direct(toEntity (asm.MethodDef(MethodKey.CmpCompareToTyped td.Key)))))

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
        if ud.IsHierarchy then
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
        else
            prepareFlatStructural asm ud self

    /// The registry entry every construction, match and member call site resolves through.
    let register
        (asm: Assembler)
        (ud: UnionDecl)
        (emittedMembers: Dictionary<string, EqArray<Emit.EmittedMember>>)
        : unit =
        let td = ud.Decl
        let emittedCases = Dictionary<string, Emit.EmittedCase>()

        ud.Cases
        |> List.iteri (fun tag c ->
            emittedCases.[c.Name] <-
                {
                    Tag = tag
                    Factory = toEntity (asm.MethodDef(MethodKey.UnionFactory(td.Key, c.Name)))
                    Fields =
                        [
                            for fi in 0 .. c.Fields.Length - 1 -> toEntity (asm.FieldDef(caseFieldKey ud c fi))
                        ]
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
