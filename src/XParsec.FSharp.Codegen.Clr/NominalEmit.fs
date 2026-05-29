namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The per-type body emitter shared by unions, records, and classes: fields +
/// ctor (+ per-case factories for a union) -> augmentation members -> the
/// optional structural-equality triple -> the optional structural-comparison
/// pair -> the pre-minted `InterfaceImpl` entity handles -> a row appended to
/// `rows`. Operates on the converged `Assembler`'s shared field/method counters
/// and registries. Field/method rows land before the closures' so each type's
/// `TypeDefinition` range stays contiguous.
module internal NominalEmit =

    let emit
        (asm: Assembler)
        (input: NominalEmissionInput)
        (td: TTypeDecl)
        (members: TTypeMember list)
        (rows: ResizeArray<EmittedTypeRow>)
        : unit =
        let provider = asm.Provider
        let icodegen = asm.Icodegen
        let ctx = asm.Ctx
        let bodyStream = asm.BodyStream
        let encodeLocals = asm.EncodeLocals
        let emitCtx = asm.EmitCtx
        let externalInlines = asm.ExternalInlines
        let ctorAttrs = asm.CtorAttrs
        let cctorAttrs = asm.CctorAttrs
        let staticFactoryAttrs = asm.StaticFactoryAttrs
        let staticMethodAttrs = asm.StaticMethodAttrs
        let instanceMethodAttrs = asm.InstanceMethodAttrs
        let overrideMethodAttrs = asm.OverrideMethodAttrs
        let ifaceEqualsAttrs = asm.IfaceEqualsAttrs
        let unions = asm.Unions
        let records = asm.Records
        let classes = asm.Classes
        let addParams (names: string list) = asm.AddParams(names)
        let claimFirstMethod (h: MethodDefinitionHandle) = asm.ClaimFirstMethod(h)

        let firstField = MetadataTokens.FieldDefinitionHandle(asm.FieldCount + 1)

        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = [ for n in td.TypeParams -> TyConst n ]

        // Local-signature encoder honouring the declaring type's typars: a member
        // (or secondary-ctor) body local of a generic type needs the generic
        // signature. Defined here so both the class arm (secondary ctors) and the
        // member loop below share it.
        let memberEncodeLocals =
            if isGeneric then
                fun locals -> provider.EncodeGenericLocalSignature(EqArray.toList td.TypeParams, locals)
            else
                encodeLocals

        let firstMember, postCtorInit =
            match input with
            | NominalEmissionInput.Union cases ->
                let tagField =
                    toEntity (ctx.AddField(FieldAttributes.Public, "_tag", provider.FieldSignature(TyConst "int")))

                asm.FieldCount <- asm.FieldCount + 1

                let caseFields =
                    [
                        for c in cases ->
                            let handles =
                                c.Fields
                                |> EqArray.toList
                                |> List.mapi (fun fi (_, fty) ->
                                    let sigBlob =
                                        if isGeneric then
                                            provider.GenericFieldSignature(EqArray.toList td.TypeParams, fty)
                                        else
                                            provider.FieldSignature fty

                                    let h = ctx.AddField(FieldAttributes.Public, sprintf "%s_%d" c.Name fi, sigBlob)

                                    asm.FieldCount <- asm.FieldCount + 1
                                    toEntity h
                                )

                            c.Name, handles
                    ]

                let ctorBodyOffset =
                    Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef []))

                let unionCtor =
                    ctx.AddMethodWithParamList(
                        ctorAttrs,
                        ".ctor",
                        provider.NullaryCtorSignature(),
                        ctorBodyOffset,
                        addParams []
                    )

                claimFirstMethod unionCtor
                asm.MethodCount <- asm.MethodCount + 1 // the `.ctor` row

                let emittedCases = Dictionary<string, Emit.EmittedCase>()

                cases
                |> List.iteri (fun tag c ->
                    let fieldHandles = caseFields |> List.find (fun (n, _) -> n = c.Name) |> snd

                    let ctorRef, tagRef, fieldRefs =
                        if isGeneric then
                            icodegen.UserGenericMemberRef(
                                td.Name,
                                typarMarkers,
                                UserMemberKind.UnionMember UnionMember.Ctor
                            ),
                            icodegen.UserGenericMemberRef(
                                td.Name,
                                typarMarkers,
                                UserMemberKind.UnionMember UnionMember.Tag
                            ),
                            [
                                for fi in 0 .. List.length fieldHandles - 1 ->
                                    icodegen.UserGenericMemberRef(
                                        td.Name,
                                        typarMarkers,
                                        UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))
                                    )
                            ]
                        else
                            toEntity unionCtor, tagField, fieldHandles

                    let factoryBody =
                        Cil.buildBody
                            encodeLocals
                            bodyStream
                            (IlIr.lower (Emit.buildUnionFactory ctorRef tag tagRef fieldRefs))

                    let paramTys = [ for (_, t) in c.Fields -> t ]

                    let factorySig =
                        if isGeneric then
                            provider.GenericStaticMethodSignature(
                                EqArray.toList td.TypeParams,
                                paramTys,
                                TyUnion(td.Name, EqArray.ofList typarMarkers)
                            )
                        else
                            provider.StaticMethodSignature(paramTys, TyUnion(td.Name, EqArray.empty))

                    let factory =
                        ctx.AddMethodWithParamList(
                            staticFactoryAttrs,
                            c.Name,
                            factorySig,
                            factoryBody,
                            addParams (argNames (List.length paramTys))
                        )

                    asm.MethodCount <- asm.MethodCount + 1 // each case factory row

                    emittedCases.[c.Name] <-
                        {
                            Tag = tag
                            Factory = toEntity factory
                            Fields = fieldHandles
                        }
                )

                let registerUnion (emittedMembers: Dictionary<string, Emit.EmittedMember>) =
                    unions.[td.Name] <-
                        {
                            Name = td.Name
                            Typars = EqArray.toList td.TypeParams
                            TagField = tagField
                            Cases = emittedCases
                            Members = emittedMembers
                        }

                unionCtor, registerUnion

            | NominalEmissionInput.Record fields ->
                let fieldHandles =
                    fields
                    |> List.map (fun f ->
                        let sigBlob =
                            if isGeneric then
                                provider.GenericFieldSignature(EqArray.toList td.TypeParams, f.Type)
                            else
                                provider.FieldSignature f.Type

                        let h = ctx.AddField(FieldAttributes.Public, f.Name, sigBlob)
                        asm.FieldCount <- asm.FieldCount + 1
                        f.Name, toEntity h, f.Type
                    )

                // Generic records share the §1.11 / B-1 ctor-store hazard with
                // classes: a raw `FieldDefinition` token in `stfld` resolves to the
                // wrong slot for a field at index >= 1 of a generic type. Route each
                // generic store through the field's `MemberRef` on the open self-
                // `TypeSpec` (`R\`1<!0>::Y`), mirroring the equality-triple field
                // handles below; monomorphic records keep the `Def` token.
                let ctorFieldRefs =
                    if isGeneric then
                        [
                            for (name, _, _) in fieldHandles ->
                                icodegen.UserGenericMemberRef(
                                    td.Name,
                                    typarMarkers,
                                    UserMemberKind.RecordMember(RecordMember.Field name)
                                )
                        ]
                    else
                        [ for (_, h, _) in fieldHandles -> h ]

                let ctorBodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs))

                let ctorSig =
                    if isGeneric then
                        provider.GenericRecordCtorSignature(EqArray.toList td.TypeParams, [ for f in fields -> f.Type ])
                    else
                        provider.ClosureCtorSignature [ for f in fields -> f.Type ]

                let recordCtor =
                    ctx.AddMethodWithParamList(
                        ctorAttrs,
                        ".ctor",
                        ctorSig,
                        ctorBodyOffset,
                        addParams [ for f in fields -> f.Name ]
                    )

                claimFirstMethod recordCtor
                asm.MethodCount <- asm.MethodCount + 1

                let registerRecord (_: Dictionary<string, Emit.EmittedMember>) =
                    records.[td.Name] <-
                        {
                            Name = td.Name
                            Typars = EqArray.toList td.TypeParams
                            Fields = fieldHandles
                            Ctor = toEntity recordCtor
                        }

                recordCtor, registerRecord

            | NominalEmissionInput.Class(_instanceFields, ctorParams, _baseType, _isSealed, staticLets, secondaryCtors) ->
                let fieldHandles =
                    ctorParams
                    |> List.map (fun p ->
                        let sigBlob =
                            if isGeneric then
                                provider.GenericFieldSignature(EqArray.toList td.TypeParams, p.Type)
                            else
                                provider.FieldSignature p.Type

                        let h = ctx.AddField(FieldAttributes.Public, p.Name, sigBlob)
                        asm.FieldCount <- asm.FieldCount + 1
                        p.Name, toEntity h, p.Type
                    )

                let staticFieldHandles =
                    staticLets
                    |> List.map (fun sl ->
                        let h =
                            ctx.AddField(
                                FieldAttributes.Private ||| FieldAttributes.Static,
                                sl.Name,
                                provider.FieldSignature sl.Type
                            )

                        asm.FieldCount <- asm.FieldCount + 1
                        sl.Name, toEntity h
                    )

                let staticFieldsDict = Dictionary<string, EntityHandle>()

                for (n, h) in staticFieldHandles do
                    staticFieldsDict.[n] <- h

                // A *generic* class's ctor `stfld` sequence must reference each
                // field through a `MemberRef` on the open self-`TypeSpec`
                // (`Box\`1<!0>::n`), not the raw `FieldDefinition` token — the
                // generic-union/record field paths already do this. The single-
                // field case happened to work with the raw `Def` token because
                // field 0 aliases the type's first slot, but a field at index >= 1
                // resolves to the wrong slot at runtime (vesper-set-sprint-plan
                // §1.11 / B-1). Member-body `FieldGet`/`FieldSet` already route
                // through `EmitResolve.resolveRecordField`'s `MemberRef`; this
                // closes the matching gap on the ctor stores.
                let ctorFieldRefs =
                    if isGeneric then
                        [
                            for (name, _, _) in fieldHandles ->
                                icodegen.UserGenericMemberRef(
                                    td.Name,
                                    typarMarkers,
                                    UserMemberKind.ClassMember(ClassMember.Field name)
                                )
                        ]
                    else
                        [ for (_, h, _) in fieldHandles -> h ]

                let ctorBodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs))

                let ctorSig =
                    if isGeneric then
                        provider.GenericRecordCtorSignature(
                            EqArray.toList td.TypeParams,
                            [ for p in ctorParams -> p.Type ]
                        )
                    else
                        provider.ClosureCtorSignature [ for p in ctorParams -> p.Type ]

                let classCtor =
                    ctx.AddMethodWithParamList(
                        ctorAttrs,
                        ".ctor",
                        ctorSig,
                        ctorBodyOffset,
                        addParams [ for p in ctorParams -> p.Name ]
                    )

                claimFirstMethod classCtor
                asm.MethodCount <- asm.MethodCount + 1

                // Register the class with its static-field handles *before*
                // building the `.cctor` body, so an initialiser referencing an
                // earlier `static let` (lowered to `StaticFieldGet`) resolves
                // through `resolveStaticField`.
                if not (List.isEmpty staticLets) then
                    classes.[td.Name] <-
                        {
                            Name = td.Name
                            Typars = EqArray.toList td.TypeParams
                            Fields = fieldHandles
                            Ctor = toEntity classCtor
                            Members = Dictionary()
                            StaticFields = staticFieldsDict
                            SecondaryCtors = []
                        }

                    let cctorInits =
                        List.map2
                            (fun (_, h) (sl: TStaticLet) ->
                                h,
                                (sl.Init
                                 |> Emit.spliceExternalInlinesInExpr externalInlines
                                 |> Emit.expandBuiltinOps)
                            )
                            staticFieldHandles
                            staticLets

                    let cctorBody =
                        Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx cctorInits))

                    ctx.AddMethodWithParamList(cctorAttrs, ".cctor", provider.CctorSignature(), cctorBody, addParams [])
                    |> ignore

                    asm.MethodCount <- asm.MethodCount + 1

                // Secondary constructors (B-11). Each is a `.ctor` overload whose
                // body runs its `let`-preamble then chains to the primary `.ctor`.
                // Emitted *before* the member-handle prediction below so their rows
                // are counted; the chain target is the primary ctor's `Def` token
                // (monomorphic) or a `MemberRef` on the open self-`TypeSpec` (generic).
                // The `(arity, handle)` list lets a `TExpr.New` call site pick the
                // matching overload.
                let secondaryCtorHandles =
                    if List.isEmpty secondaryCtors then
                        []
                    else
                        let primaryCtorRef =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.Name,
                                    typarMarkers,
                                    UserMemberKind.ClassMember ClassMember.Ctor
                                )
                            else
                                toEntity classCtor

                        [
                            for sc in secondaryCtors do
                                let paramTys = [ for (_, t) in sc.Params -> t ]

                                let scSig =
                                    if isGeneric then
                                        provider.GenericRecordCtorSignature(EqArray.toList td.TypeParams, paramTys)
                                    else
                                        provider.ClosureCtorSignature paramTys

                                let prep (e: TExpr) =
                                    e |> Emit.spliceExternalInlinesInExpr externalInlines |> Emit.expandBuiltinOps

                                let lets = [ for l in sc.Lets -> { l with Init = prep l.Init } ]
                                let primaryArgs = [ for a in sc.PrimaryArgs -> prep a ]

                                let scBody =
                                    Cil.buildBody
                                        memberEncodeLocals
                                        bodyStream
                                        (IlIr.lower (
                                            Emit.buildSecondaryCtor emitCtx sc.Params lets primaryCtorRef primaryArgs
                                        ))

                                let h =
                                    ctx.AddMethodWithParamList(
                                        ctorAttrs,
                                        ".ctor",
                                        scSig,
                                        scBody,
                                        addParams (argNames sc.Params.Length)
                                    )

                                asm.MethodCount <- asm.MethodCount + 1
                                yield (sc.Params.Length, toEntity h)
                        ]

                let registerClass (emittedMembers: Dictionary<string, Emit.EmittedMember>) =
                    classes.[td.Name] <-
                        {
                            Name = td.Name
                            Typars = EqArray.toList td.TypeParams
                            Fields = fieldHandles
                            Ctor = toEntity classCtor
                            Members = emittedMembers
                            StaticFields = staticFieldsDict
                            SecondaryCtors = secondaryCtorHandles
                        }

                classCtor, registerClass

        // Predict each member's handle from the running row count *before*
        // building any body, so a member body can reference a sibling
        // (`this.Length`) or a case factory (`Empty = Nil`). `postCtorInit`
        // then registers the union/record/class so those resolve.
        let emittedMembers = Dictionary<string, Emit.EmittedMember>()

        members
        |> List.iteri (fun i (mem: TTypeMember) ->
            let handle = MetadataTokens.MethodDefinitionHandle(asm.MethodCount + 1 + i)

            emittedMembers.[mem.Name] <-
                {
                    Handle = toEntity handle
                    IsStatic = mem.IsStatic
                    Arity = mem.Params.Length
                    MetaName = memberMetaName mem
                    ParamTys = [ for (_, t) in mem.Params -> t ]
                    RetTy = mem.ReturnTy
                }
        )

        postCtorInit emittedMembers

        for mem in members do
            // A *generic* member (B-12) carries its own typars as union-find roots.
            // Install them as the ambient `!!i` context for the duration of this
            // member's body / locals / signature encoding, so a `TyVar` leaf naming
            // one of them encodes to `GenericMethodParameter`; the declaring type's
            // typars stay `TyConst` markers that the signature's local typar map
            // resolves to `GenericTypeParameter`. Cleared after the row is added.
            let methodTypars = mem.MethodTypeParams
            let isGenericMethod = not methodTypars.IsEmpty

            if isGenericMethod then
                provider.SetMethodTypars [ for (_, r) in methodTypars -> r ]

            let bodyOffset =
                Cil.buildBody
                    memberEncodeLocals
                    bodyStream
                    (IlIr.lower (
                        Emit.buildMember
                            emitCtx
                            mem.ThisKey
                            mem.Params
                            (mem.Body
                             |> Emit.spliceExternalInlinesInExpr externalInlines
                             |> Emit.expandBuiltinOps)
                    ))

            let methodName = memberMetaName mem
            let paramTys = [ for (_, t) in mem.Params -> t ]

            let signature =
                if isGenericMethod then
                    provider.GenericMethodOnTypeSignature(
                        EqArray.toList td.TypeParams,
                        methodTypars.Length,
                        paramTys,
                        mem.ReturnTy,
                        not mem.IsStatic
                    )
                else
                    match isGeneric, mem.IsStatic with
                    | true, true ->
                        provider.GenericStaticMethodSignature(EqArray.toList td.TypeParams, paramTys, mem.ReturnTy)
                    | true, false ->
                        provider.GenericInstanceMethodSignature(EqArray.toList td.TypeParams, paramTys, mem.ReturnTy)
                    | false, true -> provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                    | false, false -> provider.InstanceMethodSignature(paramTys, mem.ReturnTy)

            let attrs =
                if mem.IsStatic then
                    staticMethodAttrs
                else
                    instanceMethodAttrs

            let memHandle =
                ctx.AddMethodWithParamList(
                    attrs,
                    methodName,
                    signature,
                    bodyOffset,
                    addParams (argNames mem.Params.Length)
                )

            if isGenericMethod then
                // The method's own typars are owned by this `MethodDef` (the
                // metadata name drops the F# leading quote, like every other
                // generic-param row).
                methodTypars
                |> EqArray.iteri (fun i (n, _) -> asm.AddMethodGenericParam(toEntity memHandle, i, n.TrimStart('\'')))

                provider.ClearMethodTypars()

            asm.MethodCount <- asm.MethodCount + 1 // each member-method row

        let emitsEqualityTriple = td.EqualitySupport = EqualityVerdict.Structural

        if emitsEqualityTriple then
            provider.SetTypeTypars(EqArray.toList td.TypeParams)

            match input with
            | NominalEmissionInput.Union cases ->
                let emitted = unions.[td.Name]
                let tagField = emitted.TagField

                // Flat walk across every case's fields, declaration order: sound
                // because inactive-case fields are always default (see
                // `Emit.UnionEqualitySupport`).
                let allFields =
                    [
                        for c in cases do
                            let caseFields = emitted.Cases.[c.Name].Fields

                            for fi in 0 .. c.Fields.Length - 1 ->
                                let fieldHandle =
                                    if isGeneric then
                                        icodegen.UserGenericMemberRef(
                                            td.Name,
                                            typarMarkers,
                                            UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))
                                        )
                                    else
                                        caseFields.[fi]

                                fieldHandle, Emit.zonk (snd c.Fields.[fi])
                    ]

                let support: Emit.UnionEqualitySupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericUnionSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyUnion(td.Name, EqArray.ofList typarMarkers)
                        TagField =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.Name,
                                    typarMarkers,
                                    UserMemberKind.UnionMember UnionMember.Tag
                                )
                            else
                                tagField
                        Fields = allFields
                        IntType = TyConst "int"
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                let getHashCodeBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionGetHashCode support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "GetHashCode",
                    provider.GetHashCodeOverrideSignature(),
                    getHashCodeBody,
                    addParams []
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

                let equalsBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionEquals support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "Equals",
                    provider.EqualsOverrideSignature(),
                    equalsBody,
                    addParams [ "obj" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

                let equalsTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionEqualsTyped support))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "Equals",
                    provider.EqualsTypedSignature(TyUnion(td.Name, EqArray.ofList typarMarkers)),
                    equalsTypedBody,
                    addParams [ "other" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

            | NominalEmissionInput.Record _ ->
                let emitted = records.[td.Name]

                let allFields =
                    [
                        for (name, h, fty) in emitted.Fields ->
                            let fieldHandle =
                                if isGeneric then
                                    icodegen.UserGenericMemberRef(
                                        td.Name,
                                        typarMarkers,
                                        UserMemberKind.RecordMember(RecordMember.Field name)
                                    )
                                else
                                    h

                            fieldHandle, Emit.zonk fty
                    ]

                let support: Emit.RecordEqualitySupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericRecordSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyRecord(td.Name, EqArray.ofList typarMarkers)
                        Fields = allFields
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                let getHashCodeBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordGetHashCode support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "GetHashCode",
                    provider.GetHashCodeOverrideSignature(),
                    getHashCodeBody,
                    addParams []
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

                let equalsBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordEquals support))

                ctx.AddMethodWithParamList(
                    overrideMethodAttrs,
                    "Equals",
                    provider.EqualsOverrideSignature(),
                    equalsBody,
                    addParams [ "obj" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

                let equalsTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordEqualsTyped support))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "Equals",
                    provider.EqualsTypedSignature(TyRecord(td.Name, EqArray.ofList typarMarkers)),
                    equalsTypedBody,
                    addParams [ "other" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

            | NominalEmissionInput.Class _ -> ()

            provider.ClearTypeTypars()

        let emitsComparisonPair = td.ComparisonSupport = ComparisonVerdict.Structural

        if emitsComparisonPair then
            provider.SetTypeTypars(EqArray.toList td.TypeParams)

            match input with
            | NominalEmissionInput.Union cases ->
                let emitted = unions.[td.Name]
                let tagField = emitted.TagField

                let allFieldsForCmp =
                    [
                        for c in cases do
                            let caseFields = emitted.Cases.[c.Name].Fields

                            for fi in 0 .. c.Fields.Length - 1 ->
                                let fieldHandle =
                                    if isGeneric then
                                        icodegen.UserGenericMemberRef(
                                            td.Name,
                                            typarMarkers,
                                            UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))
                                        )
                                    else
                                        caseFields.[fi]

                                fieldHandle, Emit.zonk (snd c.Fields.[fi])
                    ]

                let cmpSupport: Emit.UnionComparisonSupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericUnionSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyUnion(td.Name, EqArray.ofList typarMarkers)
                        TagField =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.Name,
                                    typarMarkers,
                                    UserMemberKind.UnionMember UnionMember.Tag
                                )
                            else
                                tagField
                        Fields = allFieldsForCmp
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                let compareToTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildUnionCompareTo cmpSupport))

                let typedCompareTo =
                    ctx.AddMethodWithParamList(
                        ifaceEqualsAttrs,
                        "CompareTo",
                        provider.CompareToTypedSignature(TyUnion(td.Name, EqArray.ofList typarMarkers)),
                        compareToTypedBody,
                        addParams [ "other" ]
                    )

                asm.MethodCount <- asm.MethodCount + 1

                let compareToObjBody =
                    Cil.buildBody
                        memberEncodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildUnionCompareToObj cmpSupport (toEntity typedCompareTo)))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "CompareTo",
                    provider.CompareToOverrideSignature(),
                    compareToObjBody,
                    addParams [ "obj" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

            | NominalEmissionInput.Record _ ->
                let emitted = records.[td.Name]

                let allFieldsForCmp =
                    [
                        for (name, h, fty) in emitted.Fields ->
                            let fieldHandle =
                                if isGeneric then
                                    icodegen.UserGenericMemberRef(
                                        td.Name,
                                        typarMarkers,
                                        UserMemberKind.RecordMember(RecordMember.Field name)
                                    )
                                else
                                    h

                            fieldHandle, Emit.zonk fty
                    ]

                let cmpSupport: Emit.RecordComparisonSupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericRecordSelfSpec td.Name
                            else
                                provider.UserTypeHandle td.Name
                        SelfSemType = TyRecord(td.Name, EqArray.ofList typarMarkers)
                        Fields = allFieldsForCmp
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                let compareToTypedBody =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower (Emit.buildRecordCompareTo cmpSupport))

                let typedCompareTo =
                    ctx.AddMethodWithParamList(
                        ifaceEqualsAttrs,
                        "CompareTo",
                        provider.CompareToTypedSignature(TyRecord(td.Name, EqArray.ofList typarMarkers)),
                        compareToTypedBody,
                        addParams [ "other" ]
                    )

                asm.MethodCount <- asm.MethodCount + 1

                let compareToObjBody =
                    Cil.buildBody
                        memberEncodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildRecordCompareToObj cmpSupport (toEntity typedCompareTo)))

                ctx.AddMethodWithParamList(
                    ifaceEqualsAttrs,
                    "CompareTo",
                    provider.CompareToOverrideSignature(),
                    compareToObjBody,
                    addParams [ "obj" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

            | NominalEmissionInput.Class _ -> ()

            provider.ClearTypeTypars()

        let selfTy =
            match input with
            | NominalEmissionInput.Union _ -> fun (ts: SemType list) -> TyUnion(td.Name, EqArray.ofList ts)
            | NominalEmissionInput.Record _ -> fun (ts: SemType list) -> TyRecord(td.Name, EqArray.ofList ts)
            | NominalEmissionInput.Class _ -> fun (ts: SemType list) -> TyClass(td.Name, EqArray.ofList ts)

        let interfaces =
            if emitsEqualityTriple || emitsComparisonPair then
                provider.SetTypeTypars(EqArray.toList td.TypeParams)
                let selfMarkers = [ for t in td.TypeParams -> TyConst t ]

                let acc =
                    [
                        if emitsEqualityTriple then
                            provider.EquatableInterfaceSpec(selfTy selfMarkers)
                        if emitsComparisonPair then
                            provider.ComparableInterfaceSpec(selfTy selfMarkers)
                            provider.IComparableType
                    ]

                provider.ClearTypeTypars()
                acc
            else
                []

        let rowIsSealed =
            match input with
            | NominalEmissionInput.Union _
            | NominalEmissionInput.Record _ -> true
            | NominalEmissionInput.Class(_, _, _, isSealed, _, _) -> isSealed

        rows.Add(
            {
                Name = td.Name
                Namespace = defaultArg td.Namespace ""
                Typars = EqArray.toList td.TypeParams
                FirstField = firstField
                FirstMethod = firstMember
                Interfaces = interfaces
                IsSealed = rowIsSealed
            }
        )
