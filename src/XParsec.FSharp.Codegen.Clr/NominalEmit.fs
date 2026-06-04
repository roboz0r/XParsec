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
        (td: Frozen.TTypeDecl)
        (members: Frozen.TTypeMember list)
        (rows: ResizeArray<EmittedTypeRow>)
        : unit =
        let provider = asm.Provider
        let icodegen = asm.Icodegen
        let ctx = asm.Ctx
        let bodyStream = asm.BodyStream
        let encodeLocals = asm.EncodeLocals
        let emitCtx = asm.EmitCtx
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
        // The declaring type's own typars as self-describing open-typar nodes
        // (`!i`), `i` = position in `TypeParams`. The codegen encoders resolve a
        // `FTTypar(Declaring, i)` straight off the node (frozen-type-plan keystone),
        // so these need no ambient typar window.
        let typarMarkers =
            [ for i in 0 .. td.TypeParams.Length - 1 -> FTTypar(TyparAxis.Declaring, i) ]

        // Local-signature encoder. A member (or secondary-ctor) body local of a
        // generic type carries its declaring typars as `FTTypar(Declaring, i)` nodes
        // the encoder resolves to `!i` directly, so the generic and monomorphic paths
        // are identical — one shared `encodeLocals` covers both.
        let memberEncodeLocals = encodeLocals

        // The IL base type for this `TypeDefinition`. Defaults to `Object`; the
        // class arm overwrites it with the parent's `TypeSpec` for an `inherit`
        // clause (B-4 Step 2.5). Resolved here (not in `Finalise`) so a generic
        // parent encodes against this class's typars while they are ambient.
        let mutable baseTypeHandle = provider.ObjectType

        let firstMember, postCtorInit =
            match input with
            | NominalEmissionInput.Union cases ->
                let tagField =
                    toEntity (
                        ctx.AddField(
                            FieldAttributes.Public,
                            "_tag",
                            provider.FieldSignature(FTConst("int", EqArray.empty))
                        )
                    )

                asm.FieldCount <- asm.FieldCount + 1

                let caseFields =
                    [
                        for c in cases ->
                            let handles =
                                c.Fields
                                |> EqArray.toList
                                |> List.mapi (fun fi (_, fty) ->
                                    let sigBlob = provider.FieldSignature fty

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
                                td.Key,
                                typarMarkers,
                                UserMemberKind.UnionMember UnionMember.Ctor
                            ),
                            icodegen.UserGenericMemberRef(
                                td.Key,
                                typarMarkers,
                                UserMemberKind.UnionMember UnionMember.Tag
                            ),
                            [
                                for fi in 0 .. List.length fieldHandles - 1 ->
                                    icodegen.UserGenericMemberRef(
                                        td.Key,
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

                    // A mono union's `typarMarkers` is empty, so the self return type is
                    // `FTUnion(td.Key, [])` — one path covers both.
                    let factorySig =
                        provider.StaticMethodSignature(paramTys, FTUnion(td.Key, EqArray.ofList typarMarkers))

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
                    unions.[td.Key] <-
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
                        let sigBlob = provider.FieldSignature f.Type

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
                                    td.Key,
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

                let ctorSig = provider.RecordCtorSignature [ for f in fields -> f.Type ]

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
                    records.[td.Key] <-
                        {
                            Name = td.Name
                            Typars = EqArray.toList td.TypeParams
                            Fields = fieldHandles
                            Ctor = toEntity recordCtor
                        }

                recordCtor, registerRecord

            | NominalEmissionInput.Class(_instanceFields,
                                         ctorParams,
                                         baseType,
                                         _isSealed,
                                         staticLets,
                                         secondaryCtors,
                                         baseCtorCall,
                                         _interfaces) ->
                // Resolve the parent handle for the IL `TypeDefinition.BaseType`
                // (B-4 Step 2.5). A non-generic parent (`Shape`) is the parent's
                // `TypeDefinition` token directly — the base-type column rejects a
                // `TypeSpec` that merely wraps a plain class. An instantiated generic
                // parent (`Box<int>` / `SetTree\`1<!0>`) needs a `GENERICINST`
                // `TypeSpec`, encoded with this class's typars ambient so an open
                // parent arg resolves to `!i`. Parent-less ⇒ `Object` (the default).
                match baseType with
                | ValueSome(FTClass(baseKey, baseArgs)) when baseArgs.IsEmpty ->
                    baseTypeHandle <- provider.UserTypeHandle baseKey
                | ValueSome bt ->
                    // A generic parent's open args ride `FTTypar(Declaring, i)` nodes
                    // (Freeze remaps `info.BaseType`), encoded `!i` directly — no window.
                    baseTypeHandle <- icodegen.TypeToken bt
                | ValueNone -> ()

                let fieldHandles =
                    ctorParams
                    |> List.map (fun p ->
                        let sigBlob = provider.FieldSignature p.Type

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
                                    td.Key,
                                    typarMarkers,
                                    UserMemberKind.ClassMember(ClassMember.Field name)
                                )
                        ]
                    else
                        [ for (_, h, _) in fieldHandles -> h ]

                // The primary `.ctor` body. Without an `inherit` clause it chains to
                // `Object` (the record/closure recipe). With one (B-4 Step 2.5) it
                // chains to the parent's `.ctor` with the `inherit Base(args)` args
                // before the field stores; the parent's primary `.ctor` is its `Def`
                // token (mono parent) or a `MemberRef` on the parent's `TypeSpec`
                // (generic parent). v1 inherits only from a project-local class, so
                // the parent must already be registered (declaration order, base first).
                let ctorBody =
                    match baseCtorCall with
                    | ValueSome bcc ->
                        let baseKey, baseArgs =
                            match baseType with
                            | ValueSome(FTClass(n, xs)) -> n, EqArray.toList xs
                            | _ -> failwithf "Emit: class '%s' has a base-ctor call but no class base type" td.Name

                        let baseCtorHandle =
                            match classes.TryGetValue baseKey with
                            | true, bc when List.isEmpty bc.Typars -> bc.Ctor
                            | true, _ ->
                                icodegen.UserGenericMemberRef(
                                    baseKey,
                                    baseArgs,
                                    UserMemberKind.ClassMember ClassMember.Ctor
                                )
                            | false, _ ->
                                failwithf
                                    "Emit: base class '%A' of '%s' is not an emitted project-local class"
                                    baseKey
                                    td.Name

                        Emit.buildClassBaseCtor
                            emitCtx
                            baseCtorHandle
                            (EqArray.toList bcc.Args)
                            (EqArray.toList bcc.CtorParams)
                            ctorFieldRefs
                    | ValueNone -> Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs

                let ctorBodyOffset =
                    Cil.buildBody memberEncodeLocals bodyStream (IlIr.lower ctorBody)

                let ctorSig = provider.RecordCtorSignature [ for p in ctorParams -> p.Type ]

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
                    classes.[td.Key] <-
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
                            (fun (_, h) (sl: Frozen.TStaticLet) ->
                                // Inline splicing ran pre-freeze (Passes.InlineExpansion);
                                // codegen only collapses the residual saturated built-in ops.
                                h, Emit.expandBuiltinOps sl.Init
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
                // The `(arity, handle)` list lets a `TExprG.New` call site pick the
                // matching overload.
                let secondaryCtorHandles =
                    if List.isEmpty secondaryCtors then
                        []
                    else
                        let primaryCtorRef =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.Key,
                                    typarMarkers,
                                    UserMemberKind.ClassMember ClassMember.Ctor
                                )
                            else
                                toEntity classCtor

                        [
                            for sc in secondaryCtors do
                                let paramTys = [ for (_, t) in sc.Params -> t ]

                                let scSig = provider.RecordCtorSignature paramTys

                                // Inline splicing ran pre-freeze (Passes.InlineExpansion);
                                // codegen only collapses the residual saturated built-in ops.
                                let prep (e: Frozen.TExpr) = Emit.expandBuiltinOps e

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
                    classes.[td.Key] <-
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

        // Interface implementations (B-2, §5.3): each `(ifaceTy, members)` entry's
        // member bodies are already-typed `Frozen.TTypeMember`s, flattened here. They
        // emit as virtual methods (`ifaceEqualsAttrs` — a new slot, `Final` since
        // classes are sealed) that the runtime binds to the `InterfaceImpl` row by
        // name + signature. Emitted *after* the class's own members so prediction
        // order matches emission order below.
        let classInterfaces =
            match input with
            | NominalEmissionInput.Class(_, _, _, _, _, _, _, interfaces) -> interfaces
            | _ -> []

        let ifaceMembers =
            [
                for (_, ms) in classInterfaces do
                    yield! ms
            ]

        // Predict each member's handle from the running row count *before*
        // building any body, so a member body can reference a sibling
        // (`this.Length`) or a case factory (`Empty = Nil`). `postCtorInit`
        // then registers the union/record/class so those resolve. The class's own
        // members lead, interface-impl members trail.
        let emittedMembers = Dictionary<string, Emit.EmittedMember>()

        (members @ ifaceMembers)
        |> List.iteri (fun i (mem: Frozen.TTypeMember) ->
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

        // `isIfaceImpl` forces `ifaceEqualsAttrs` (virtual / new-slot / final) so
        // the runtime maps the method to the implemented interface; the class's
        // own members keep their natural static/instance attrs.
        let emitMember (isIfaceImpl: bool) (mem: Frozen.TTypeMember) =
            // A *generic* member (B-12). Both its declaring-type typars and its own
            // method typars now ride self-describing `TempTypar` nodes in the signature /
            // locals / body (Freeze.remapMemberTypes remaps both axes — frozen-type-plan
            // 2E-1), so no ambient typar window is installed; the encoder resolves them by
            // index. `methodTypars` still feeds the `GENERIC` header arity and the
            // `GenericParam` rows below.
            let methodTypars = mem.MethodTypeParams
            let isGenericMethod = not methodTypars.IsEmpty

            let bodyOffset =
                Cil.buildBody
                    memberEncodeLocals
                    bodyStream
                    (IlIr.lower (
                        Emit.buildMember
                            emitCtx
                            mem.ThisKey
                            mem.BaseKey
                            mem.Params
                            // Inline splicing ran pre-freeze (Passes.InlineExpansion);
                            // codegen only collapses the residual saturated built-in ops.
                            (Emit.expandBuiltinOps mem.Body)
                    ))

            let methodName = memberMetaName mem
            let paramTys = [ for (_, t) in mem.Params -> t ]

            // The declaring type's typars (if any) ride `FTTypar(Declaring, i)` nodes
            // the encoder resolves to `!i` directly, so a generic and a monomorphic
            // type share one signature builder. A generic *method* (B-12) additionally
            // needs the `GENERIC` calling-convention header count; its own typars ride
            // `FTTypar(Method, i)` nodes the encoder resolves to `!!i` (no window).
            let signature =
                if isGenericMethod then
                    provider.GenericMethodOnTypeSignature(methodTypars.Length, paramTys, mem.ReturnTy, not mem.IsStatic)
                elif mem.IsStatic then
                    provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                else
                    provider.InstanceMethodSignature(paramTys, mem.ReturnTy)

            let attrs =
                if isIfaceImpl then ifaceEqualsAttrs
                elif mem.IsStatic then staticMethodAttrs
                else instanceMethodAttrs

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

            asm.MethodCount <- asm.MethodCount + 1 // each member-method row

        for mem in members do
            emitMember false mem

        for mem in ifaceMembers do
            emitMember true mem

        let emitsEqualityTriple = td.EqualitySupport = EqualityVerdict.Structural

        if emitsEqualityTriple then
            match input with
            | NominalEmissionInput.Union cases ->
                let emitted = unions.[td.Key]
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
                                            td.Key,
                                            typarMarkers,
                                            UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))
                                        )
                                    else
                                        caseFields.[fi]

                                fieldHandle, (snd c.Fields.[fi])
                    ]

                let support: Emit.UnionEqualitySupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericUnionSelfSpec td.Key
                            else
                                provider.UserTypeHandle td.Key
                        SelfTy = FTUnion(td.Key, EqArray.ofList typarMarkers)
                        TagField =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.Key,
                                    typarMarkers,
                                    UserMemberKind.UnionMember UnionMember.Tag
                                )
                            else
                                tagField
                        Fields = allFields
                        IntType = FTConst("int", EqArray.empty)
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
                    provider.EqualsTypedSignature(FTUnion(td.Key, EqArray.ofList typarMarkers)),
                    equalsTypedBody,
                    addParams [ "other" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

            | NominalEmissionInput.Record _ ->
                let emitted = records.[td.Key]

                let allFields =
                    [
                        for (name, h, fty) in emitted.Fields ->
                            let fieldHandle =
                                if isGeneric then
                                    icodegen.UserGenericMemberRef(
                                        td.Key,
                                        typarMarkers,
                                        UserMemberKind.RecordMember(RecordMember.Field name)
                                    )
                                else
                                    h

                            fieldHandle, fty
                    ]

                let support: Emit.RecordEqualitySupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericRecordSelfSpec td.Key
                            else
                                provider.UserTypeHandle td.Key
                        SelfTy = FTRecord(td.Key, EqArray.ofList typarMarkers)
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
                    provider.EqualsTypedSignature(FTRecord(td.Key, EqArray.ofList typarMarkers)),
                    equalsTypedBody,
                    addParams [ "other" ]
                )
                |> ignore

                asm.MethodCount <- asm.MethodCount + 1

            | NominalEmissionInput.Class _ -> ()

        let emitsComparisonPair = td.ComparisonSupport = ComparisonVerdict.Structural

        if emitsComparisonPair then
            match input with
            | NominalEmissionInput.Union cases ->
                let emitted = unions.[td.Key]
                let tagField = emitted.TagField

                let allFieldsForCmp =
                    [
                        for c in cases do
                            let caseFields = emitted.Cases.[c.Name].Fields

                            for fi in 0 .. c.Fields.Length - 1 ->
                                let fieldHandle =
                                    if isGeneric then
                                        icodegen.UserGenericMemberRef(
                                            td.Key,
                                            typarMarkers,
                                            UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))
                                        )
                                    else
                                        caseFields.[fi]

                                fieldHandle, (snd c.Fields.[fi])
                    ]

                let cmpSupport: Emit.UnionComparisonSupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericUnionSelfSpec td.Key
                            else
                                provider.UserTypeHandle td.Key
                        SelfTy = FTUnion(td.Key, EqArray.ofList typarMarkers)
                        TagField =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.Key,
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
                        provider.CompareToTypedSignature(FTUnion(td.Key, EqArray.ofList typarMarkers)),
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
                let emitted = records.[td.Key]

                let allFieldsForCmp =
                    [
                        for (name, h, fty) in emitted.Fields ->
                            let fieldHandle =
                                if isGeneric then
                                    icodegen.UserGenericMemberRef(
                                        td.Key,
                                        typarMarkers,
                                        UserMemberKind.RecordMember(RecordMember.Field name)
                                    )
                                else
                                    h

                            fieldHandle, fty
                    ]

                let cmpSupport: Emit.RecordComparisonSupport =
                    {
                        SelfType =
                            if isGeneric then
                                provider.GenericRecordSelfSpec td.Key
                            else
                                provider.UserTypeHandle td.Key
                        SelfTy = FTRecord(td.Key, EqArray.ofList typarMarkers)
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
                        provider.CompareToTypedSignature(FTRecord(td.Key, EqArray.ofList typarMarkers)),
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

        let selfTy =
            match input with
            | NominalEmissionInput.Union _ -> fun (ts: FrozenType list) -> FTUnion(td.Key, EqArray.ofList ts)
            | NominalEmissionInput.Record _ -> fun (ts: FrozenType list) -> FTRecord(td.Key, EqArray.ofList ts)
            | NominalEmissionInput.Class _ -> fun (ts: FrozenType list) -> FTClass(td.Key, EqArray.ofList ts)

        // One `InterfaceImpl` entity handle per implemented interface — the
        // synthesised structural-equality / comparison interfaces (unions /
        // records) and the user-declared `interface … with` impls (B-2, §5.3,
        // classes). A generic interface arg (`IEnumerable<'T>`) carries its `'T`
        // as a `FTTypar(Declaring, i)` (emitted by Freeze; `selfMarkers` for the
        // synthesised interfaces), encoded `!i` straight off the node — no ambient
        // window. `TypeSpecOf` mints the user interfaces' handles.
        let interfaces =
            if emitsEqualityTriple || emitsComparisonPair || not (List.isEmpty classInterfaces) then
                let selfMarkers =
                    [ for i in 0 .. td.TypeParams.Length - 1 -> FTTypar(TyparAxis.Declaring, i) ]

                [
                    if emitsEqualityTriple then
                        provider.EquatableInterfaceSpec(selfTy selfMarkers)
                    if emitsComparisonPair then
                        provider.ComparableInterfaceSpec(selfTy selfMarkers)
                        provider.IComparableType
                    for (ifaceTy, _) in classInterfaces do
                        provider.InterfaceHandleOf ifaceTy
                ]
            else
                []

        let rowIsSealed =
            match input with
            | NominalEmissionInput.Union _
            | NominalEmissionInput.Record _ -> true
            | NominalEmissionInput.Class(_, _, _, isSealed, _, _, _, _) -> isSealed

        rows.Add(
            {
                Name = td.Name
                Namespace = defaultArg td.Namespace ""
                Typars = EqArray.toList td.TypeParams
                FirstField = firstField
                FirstMethod = firstMember
                Interfaces = interfaces
                IsSealed = rowIsSealed
                BaseType = baseTypeHandle
            }
        )
