namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold
open NominalShared

/// Union emission: the `.ctor`, the case factories, and — in a hierarchy regime — each
/// case's own nested type, its `.ctor`, and the structural bodies the base declares
/// abstract.
module internal UnionEmit =

    let private intTy = FTConst(RuntimeNames.intKey, EqArray.empty)
    let private boolTy = FTConst(RuntimeNames.boolKey, EqArray.empty)

    /// A member ref to one of a CASE type's own synthesised methods: a generic union parents
    /// it on the case's own `TypeSpec` over the union's typar markers, a monomorphic one
    /// uses the resolved `Def` token.
    ///
    /// A case's `.ctor` and payload fields go through `selfMemberRef` and the union's own
    /// `UnionMember` spelling instead, which reparents them onto the case behind that.
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

    /// A case's payload field refs in declaration order. `UnionMember.Field` is the one
    /// spelling every site writes; it reparents onto the case type behind that.
    let private fieldRefsOf (asm: Assembler) (td: TastAccessor.TypeDecl) (c: Frozen.TUnionCase) : EntityHandle list =
        [
            for fi in 0 .. c.Fields.Length - 1 ->
                selfMemberRef
                    asm
                    td
                    (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi)))
                    (toEntity (asm.FieldDef(FieldKey.UnionCaseField(td.Key, c.Name, fi))))
        ]

    /// A case's `(field ref, declared type)` pairs in declaration order — the walk every
    /// structural body over that case takes.
    ///
    /// A generic union mints a `MemberRef` row per field per call, so a caller that feeds
    /// several bodies calls this once and shares the result.
    let private caseFieldsOf
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (c: Frozen.TUnionCase)
        : (EntityHandle * FrozenType) list =
        List.zip (fieldRefsOf asm td c) [ for (_, t) in c.Fields -> t ]

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

        let ctorShape = ud.CtorShape

        // `_tag` is reachable only inside this binding, which holds the whole of its
        // lifecycle: the `.ctor` writes it and `get_Tag` fronts it for every other reader.
        // No factory or structural body below can name the field to store through.
        let ctorPrepared, getTagPrepared =
            // A generic union mints a `MemberRef` row per call, so the `.ctor` and `get_Tag`
            // share one.
            let tagRef =
                lazy
                    (selfMemberRef
                        asm
                        td
                        (UserMemberKind.UnionMember UnionMember.Tag)
                        (toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))))

            // The flat forms take EVERY case's fields in declaration order, so a factory
            // `newobj`s the whole value at once. A value type chains no base `.ctor`.
            let flatCtor (tagged: bool) =
                let fields =
                    [
                        if tagged then
                            yield tagRef.Value
                        yield! List.collect (fieldRefsOf asm td) cases
                    ]

                {
                    Signature =
                        provider.RecordCtorSignature
                            [
                                if tagged then
                                    yield intTy
                                for c in cases do
                                    for (_, t) in c.Fields -> t
                            ]
                    Body =
                        bodyOf
                            asm
                            (if isStruct then
                                 Emit.buildStructCtor fields
                             else
                                 Emit.buildChainedCtor provider.ObjectCtorRef [] fields)
                    ParamNames =
                        [
                            if tagged then
                                yield "_tag"
                            for c in cases do
                                yield! ud.FieldNames c
                        ]
                    MethodTypars = []
                }

            let ctor =
                match ctorShape with
                | UnionCtorShape.FlatTagged -> flatCtor true
                | UnionCtorShape.Flat -> flatCtor false
                | UnionCtorShape.TagOnly ->
                    {
                        Signature = provider.RecordCtorSignature [ intTy ]
                        Body = bodyOf asm (Emit.buildChainedCtor provider.ObjectCtorRef [] [ tagRef.Value ])
                        ParamNames = [ "_tag" ]
                        MethodTypars = []
                    }
                | UnionCtorShape.Nullary ->
                    {
                        Signature = provider.RecordCtorSignature []
                        Body = bodyOf asm (Emit.buildChainedCtor provider.ObjectCtorRef [] [])
                        ParamNames = []
                        MethodTypars = []
                    }

            let getTag =
                if ud.HasTag then
                    ValueSome
                        {
                            Signature = provider.InstanceMethodSignature([], intTy)
                            Body = bodyOf asm (Emit.buildFieldGetter tagRef.Value)
                            ParamNames = []
                            MethodTypars = []
                        }
                else
                    ValueNone

            ctor, getTag

        asm.AddPrepared(MethodKey.NominalCtor td.Key, ctorPrepared)

        match getTagPrepared with
        | ValueSome p -> asm.AddPrepared(MethodKey.UnionGetTag td.Key, p)
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

        // The arguments one case passes to the union's own `.ctor`. `TagOnly` stamps the
        // discriminant; the shapes a case reaches otherwise take no argument a case alone
        // supplies.
        let ctorTagArgs (tag: int) =
            match ctorShape with
            | UnionCtorShape.TagOnly -> [ ILInstr.LdcI4 tag ]
            | UnionCtorShape.Nullary
            | UnionCtorShape.Flat
            | UnionCtorShape.FlatTagged -> []

        if isHierarchy then
            // Each case's `.ctor(payload…)` chains the union's own `.ctor`, passing its tag
            // where the base declares one, so `_tag` is written once, by the base, and stays
            // `initonly`. A `TypeTested` base takes no argument and has no field to write.
            cases
            |> List.iteri (fun tag c ->
                asm.AddPrepared(
                    MethodKey.UnionCaseCtor(td.Key, c.Name),
                    {
                        Signature = provider.RecordCtorSignature [ for (_, t) in c.Fields -> t ]
                        Body = bodyOf asm (Emit.buildChainedCtor ctorRef (ctorTagArgs tag) (fieldRefsOf asm td c))
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
                    let args =
                        [
                            for c' in cases do
                                if c'.Name = c.Name then
                                    for fi in 0 .. c'.Fields.Length - 1 do
                                        yield Emit.StructUnionCtorArg.Param fi
                                else
                                    for (_, t) in c'.Fields do
                                        yield Emit.StructUnionCtorArg.Default(t, asm.Icodegen.TypeToken t)
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

    // ---- Structural bodies -----------------------------------------------------------

    /// A case type's own structural bodies, one per slot `UnionCaseSlot.required` names.
    /// The typed `Equals(<Case>)` / `CompareTo(<Case>)` hold the field walk and the
    /// `U`-typed overrides are the guards that reach them.
    ///
    /// `caseType` is this case's own token, minted once by the caller: on a generic union
    /// it is a `TypeSpec` row, and that table is appended to rather than deduplicated.
    /// `otherOrdinal` is forced only when a comparison slot is required.
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

        // The case's own fields, seeded by its tag: the case is settled by the dispatch
        // that reaches these bodies, so nothing compares a discriminant.
        let walk: EmitStructural.StructuralWalk =
            {
                Fields = caseFieldsOf asm td c
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

    /// A flat union's own structural bodies. Equality, comparison and `%A` all walk the
    /// same fields, so one pass mints the field and tag refs they share: on a generic union
    /// a second pass would mint a second `MemberRef` row per field.
    ///
    /// Once the tags agree the walk crosses every case's fields, not just the active
    /// case's. An inactive case's fields hold their default, so the two agree.
    let private prepareFlatStructural (asm: Assembler) (ud: UnionDecl) (self: StructuralSelf) : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let handles = asm.Structural
        let isStruct = ud.ValueKind.IsValueType

        // Minted on first use and shared from there: a type that synthesises no structural
        // body at all adds no `MemberRef` row, and one that synthesises several adds one
        // row per field rather than one per body.
        let perCase = lazy [ for c in ud.Cases -> c.Name, caseFieldsOf asm td c ]
        let tagField = lazy (tagFieldRefOf asm td)

        let walk: Lazy<EmitStructural.StructuralWalk> =
            lazy
                {
                    Fields = perCase.Value |> List.collect snd
                    Discriminant =
                        // A single-case union has one shape, so its walk is the record's;
                        // the other flat regimes compare `_tag` ahead of the co-resident
                        // fields.
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
                // A single-case union renders its sole case straight through — the same
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

    /// Every body behind the union's synthesised structural rows. In a hierarchy regime
    /// that also covers each case type's rows and the `TypeRowExtras` its `TypeDefinition`
    /// needs: it `extends` the union, instantiated over its own typars when the union is
    /// generic, and declares no interface of its own.
    let prepareStructural (asm: Assembler) (ud: UnionDecl) (self: StructuralSelf) : unit =
        if ud.IsHierarchy then
            let td = ud.Decl
            prepareHierarchyBase asm ud self

            // Every case's own token, in tag order, minted once here: each case's bodies
            // use its own, and `TypeTested`'s `CompareTo(U)` walks the others'.
            let caseTokens = [ for c in ud.Cases -> asm.Icodegen.TypeToken(caseTyOf td c.Name) ]

            // `other`'s ordinal for one case's `CompareTo(U)` — the one discriminant read
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
                        // Every case extends the one base handle. A generic union's is a
                        // `TypeSpec`, and that table is appended to rather than
                        // deduplicated, so re-deriving it per case would add a row per case.
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
                            for fi in 0 .. c.Fields.Length - 1 ->
                                toEntity (asm.FieldDef(FieldKey.UnionCaseField(td.Key, c.Name, fi)))
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
                Typars = EqArray.toList td.TypeParams
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
