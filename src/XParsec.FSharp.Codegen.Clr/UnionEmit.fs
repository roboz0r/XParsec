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

    /// The union `.ctor`, the `.cctor` a hierarchy union's nullary singletons need, and one
    /// static factory per case.
    let prepareUnion (asm: Assembler) (ud: UnionDecl) : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let cases = ud.Cases
        let isStruct = ud.ValueKind.IsValueType
        let isHierarchy = ud.IsHierarchy
        let selfTy = FTUnion(td.TypeKey, EqArray.ofList (typarMarkersOf td))

        let tagRef =
            selfMemberRef
                asm
                td
                (UserMemberKind.UnionMember UnionMember.Tag)
                (toEntity (asm.FieldDef(FieldKey.UnionTag td.Key)))

        // A struct union's `.ctor` takes the tag and EVERY case's fields in flat
        // declaration order, and each factory `newobj`s it. A hierarchy base takes the tag
        // alone, stamped by whichever case `.ctor` chains it. A flat reference union's is
        // nullary and its factories `stfld` after the `newobj`.
        let ctorPrepared =
            if isStruct then
                {
                    Signature =
                        provider.RecordCtorSignature(
                            intTy
                            :: [
                                for c in cases do
                                    for (_, t) in c.Fields -> t
                            ]
                        )
                    Body = bodyOf asm (Emit.buildStructCtor (tagRef :: List.collect (fieldRefsOf asm td) cases))
                    ParamNames =
                        "_tag"
                        :: [
                            for c in cases do
                                yield! ud.FieldNames c
                        ]
                    MethodTypars = []
                }
            elif isHierarchy then
                {
                    Signature = provider.RecordCtorSignature [ intTy ]
                    Body = bodyOf asm (Emit.buildClosureCtor provider.ObjectCtorRef [ tagRef ])
                    ParamNames = [ "_tag" ]
                    MethodTypars = []
                }
            else
                {
                    Signature = provider.NullaryCtorSignature()
                    Body = bodyOf asm (Emit.buildClosureCtor provider.ObjectCtorRef [])
                    ParamNames = []
                    MethodTypars = []
                }

        asm.AddPrepared(MethodKey.NominalCtor td.Key, ctorPrepared)

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

        if isHierarchy then
            // Each case's `.ctor(payload…)` chains the union's `.ctor(int32)` with the
            // case's tag, so `_tag` is written once, by the base, and stays `initonly`.
            cases
            |> List.iteri (fun tag c ->
                asm.AddPrepared(
                    MethodKey.UnionCaseCtor(td.Key, c.Name),
                    {
                        Signature = provider.RecordCtorSignature [ for (_, t) in c.Fields -> t ]
                        Body = bodyOf asm (Emit.buildUnionCaseCtor ctorRef tag (fieldRefsOf asm td c))
                        ParamNames = ud.FieldNames c
                        MethodTypars = []
                    }
                )
            )

            match ud.SingletonCases with
            | [] -> ()
            | singletons ->
                let pairs = [ for c in singletons -> caseCtorRef c, singletonRef c ]

                asm.AddPrepared(
                    MethodKey.NominalCctor td.Key,
                    {
                        Signature = provider.CctorSignature()
                        Body = bodyOf asm (Emit.buildUnionSingletonCctor pairs)
                        ParamNames = []
                        MethodTypars = []
                    }
                )

        cases
        |> List.iteri (fun tag c ->
            let arity = c.Fields.Length

            let factoryIr =
                if isStruct then
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
                elif isHierarchy then
                    if arity = 0 then
                        Emit.buildUnionSingletonFactory (singletonRef c)
                    else
                        Emit.buildUnionCaseFactory (caseCtorRef c) arity
                else
                    Emit.buildUnionFactory ctorRef tag tagRef (fieldRefsOf asm td c)

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
    let private prepareCaseStructural
        (asm: Assembler)
        (ud: UnionDecl)
        (self: StructuralSelf)
        (tag: int)
        (c: Frozen.TUnionCase)
        : unit =
        let td = ud.Decl
        let provider = asm.Provider
        let handles = asm.Structural
        let caseName = c.Name
        let caseTy = caseTyOf td caseName
        let caseType = asm.Icodegen.TypeToken caseTy

        // The case's own fields, seeded by its tag: the case is settled by the dispatch
        // that reaches these bodies, so nothing compares a discriminant.
        let walk: EmitStructural.StructuralWalk =
            {
                SelfType = caseType
                SelfTy = caseTy
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

        let caseSlotRef metaName paramTys retTy slot =
            caseMethodRef
                asm
                td
                caseName
                metaName
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
                let equalsCase = caseSlotRef "Equals" [ caseTy ] boolTy UnionCaseSlot.EqualsCase

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
                    caseSlotRef "CompareTo" [ caseTy ] intTy UnionCaseSlot.CompareToCase

                prepared
                    slot
                    (provider.CompareToTypedSignature self.SelfTy)
                    [ "other" ]
                    (EmitStructural.buildUnionCaseCompareToUnion
                        caseType
                        caseTy
                        compareToCase
                        tag
                        (tagFieldRefOf asm td))

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
                    SelfType = self.SelfType
                    SelfTy = self.SelfTy
                    Fields = perCase.Value |> List.collect snd
                    Discriminant = EmitStructural.Discriminant.TagField tagField.Value
                }

        if self.Members.Equality then
            prepareEqualityTriple
                asm
                td
                self.SelfTy
                (ValueSome(EmitStructural.buildGetHashCode handles walk.Value))
                (EmitStructural.buildEqualsObj handles isStruct walk.Value)
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

            asm.AddPrepared(
                MethodKey.FmtFormat td.Key,
                {
                    Signature = provider.StructuralFormatSignature()
                    Body = bodyOf asm (EmitStructuralFormat.buildUnionFormat handles tagField.Value cases)
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

            ud.Cases
            |> List.iteri (fun tag c ->
                prepareCaseStructural asm ud self tag c

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
                TagField = toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))
                ValueKind = ud.ValueKind
                Cases = emittedCases
                Members = emittedMembers
            }
