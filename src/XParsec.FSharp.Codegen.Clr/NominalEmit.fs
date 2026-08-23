namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The per-type bound variable/preparer shared by unions, records, and classes. `register`
/// fills the `EmitContext` registries with layout-derived handles only, so any prepared
/// body can reference any type's ctor, factory, field, or member.
module internal NominalEmit =

    /// The declaring type's own typars as self-describing nodes: position `i` in
    /// `TypeParams` encodes as `!i`.
    let private typarMarkersOf (td: TastAccessor.TypeDecl) : FrozenType list =
        [ for i in 0 .. td.TypeParams.Length - 1 -> FTTypar(TyparAxis.Declaring, i) ]

    /// The resolved `inherit` parent of a nominal class: the `extends` column and the
    /// primary-ctor chain target both read off it.
    type private BaseShape =
        /// No `inherit` clause: `extends Object`, or `System.ValueType` for a struct.
        | NoBase
        /// A non-generic FOREIGN base (`inherit exn` → `System.Exception`): `tref` is the
        /// `extends` token, and `key` is the PLATFORM key the chained base `.ctor` is minted
        /// against.
        | ExternalBase of key: TypeKey * tref: EntityHandle
        /// A non-generic base this compilation emits: `handle` is its `TypeDefinition` token,
        /// and `key` reaches its ctor through the emitted `classes` registry.
        | LocalMono of key: TypeKey * handle: EntityHandle
        /// A generic parent (`Box<int>`), or any non-`FTClass` base: `extends` a
        /// `GENERICINST` `TypeSpec` encoded against this class's typars.
        | Generic of ft: FrozenType

    /// The user `interface … with` impls (interface type + member bodies) a nominal
    /// carries.
    let private userInterfacesOf (input: NominalEmissionInput) : (FrozenNominal * TastAccessor.TypeMember list) list =
        match input with
        | NominalEmissionInput.Class cd -> cd.Interfaces
        | NominalEmissionInput.Union(_, interfaces) -> interfaces
        | NominalEmissionInput.Record(_, interfaces, _) -> interfaces

    let register
        (asm: Assembler)
        (input: NominalEmissionInput)
        (td: TastAccessor.TypeDecl)
        (members: TastAccessor.TypeMember list)
        : unit =
        let icodegen = asm.Icodegen
        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = typarMarkersOf td

        // Every member's handle is its layout row, resolvable before any body
        // is built, so a member body can reference a sibling (`this.Length`) or
        // a case factory (`static member Empty = []`).
        let emittedMembers = Dictionary<string, Emit.EmittedMember list>()

        // Name → its overloads in declaration order. Own members lead and interface-impl
        // members trail, so a same-signature pair (`Set.Add : Set<'T>` vs
        // `ICollection<'T>.Add : unit`) resolves to the class's own member on a tie.
        (members @ NominalMembers.flattenIfaceMembers (userInterfacesOf input))
        |> List.iteri (fun i (mem: TastAccessor.TypeMember) ->
            let em: Emit.EmittedMember =
                {
                    Handle = toEntity (asm.MethodDef(MethodKey.Member(td.Key, i)))
                    IsStatic = mem.IsStatic
                    ParamArity = mem.Params.Length
                    MetaName = memberMetaName mem
                    ParamTys = [ for (_, t) in mem.Params -> t ]
                    RetTy = mem.ReturnTy
                    MethodTyparCount = mem.MethodTypeParams.Length
                }

            let prior =
                match emittedMembers.TryGetValue mem.Name with
                | true, ms -> ms
                | false, _ -> []

            emittedMembers.[mem.Name] <- prior @ [ em ]
        )

        match input with
        | NominalEmissionInput.Union(cases, _) ->
            let emittedCases = Dictionary<string, Emit.EmittedCase>()

            cases
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
                    }
            )

            asm.Unions.[td.TypeKey] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    TagField = toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))
                    Cases = emittedCases
                    Members = emittedMembers
                }

        | NominalEmissionInput.Record(fields, _, isStruct) ->
            asm.Records.[td.TypeKey] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    Fields =
                        [
                            for f in fields ->
                                f.Name, toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name))), f.Type
                        ]
                    IsValueType = isStruct
                    Ctor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                    Members = emittedMembers
                }

        | NominalEmissionInput.Class cd ->
            let instanceFields = cd.Fields
            let ctorParams = cd.CtorParams
            let staticLets = TPreambleEntryG.lets cd.StaticPreamble
            let instanceLets = TPreambleEntryG.lets cd.InstancePreamble
            let secondaryCtors = cd.SecondaryCtors
            let isStruct = cd.ValueKind <> ClassValueKind.RefType

            // The handle every `ldsfld`/`stsfld` references: a generic class reaches
            // its own `static let` field through a `MemberRef` on the open
            // self-`TypeSpec` (`Set\`1<!0>::empty`), a mono class through the `Def` token.
            let staticFieldsDict = Dictionary<string, EntityHandle>()

            for sl in staticLets do
                staticFieldsDict.[sl.Name] <-
                    if isGeneric then
                        icodegen.UserGenericMemberRef(
                            td.TypeKey,
                            typarMarkers,
                            UserMemberKind.ClassMember(ClassMember.Field sl.Name)
                        )
                    else
                        toEntity (asm.FieldDef(FieldKey.ClassStaticField(td.Key, sl.Name)))

            // `(arity, paramTys, handle)` lets a `New` call site pick the matching
            // overload; the param types carry declaring-typar markers so a generic
            // site can mint a `MemberRef` on the instantiated `TypeSpec`.
            let secondaryCtorHandles =
                secondaryCtors
                |> List.mapi (fun i (sc: TastAccessor.SecondaryCtor) ->
                    sc.Params.Length,
                    [ for (_, t) in sc.Params -> t ],
                    toEntity (asm.MethodDef(MethodKey.SecondaryCtor(td.Key, i)))
                )

            // The val-field reference form (no primary ctor) declares no `NominalCtor`
            // row, so don't reserve its handle. `Ctor` aliases the first secondary.
            // Structs and the no-secondary fallback keep the synthesised primary.
            let emitPrimaryCtor =
                isStruct || cd.HasPrimaryCtor || List.isEmpty secondaryCtorHandles

            let ctorHandle =
                if emitPrimaryCtor then
                    toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                else
                    let (_, _, h) = List.head secondaryCtorHandles
                    h

            asm.Classes.[td.TypeKey] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    Fields =
                        [
                            for p in ctorParams ->
                                p.Name, toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))), p.Type
                        ]
                    // A declared `val` field and an instance-`let` backing field are one
                    // thing at a use site: `this.x` resolves by NAME against this list.
                    InstanceFields =
                        [
                            for f in instanceFields ->
                                f.Name, toEntity (asm.FieldDef(FieldKey.ClassInstanceField(td.Key, f.Name))), f.Type
                            for l in instanceLets ->
                                l.Name, toEntity (asm.FieldDef(FieldKey.ClassLetField(td.Key, l.Name))), l.Type
                        ]
                    IsValueType = isStruct
                    Ctor = ctorHandle
                    HasPrimaryCtor = emitPrimaryCtor
                    Members = emittedMembers
                    StaticFields = staticFieldsDict
                    SecondaryCtors = secondaryCtorHandles
                    // The implemented interfaces over this class's declaring typars; the impl
                    // member bodies are not needed here.
                    Interfaces = [ for (iface, _) in cd.Interfaces -> iface ]
                }

    let prepare
        (asm: Assembler)
        (emitCtx: Emit.EmitContext)
        (input: NominalEmissionInput)
        (td: TastAccessor.TypeDecl)
        (members: TastAccessor.TypeMember list)
        : unit =
        let provider = asm.Provider
        let icodegen = asm.Icodegen
        let ctx = asm.Ctx
        let bodyStream = asm.BodyStream
        let encodeLocals = asm.EncodeLocals
        let unions = asm.Unions
        let records = asm.Records
        let classes = asm.Classes

        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = typarMarkersOf td

        // A `[<Struct>]` record: `this` (`ldarg.0`) is a managed pointer, so the
        // synthesised equality/comparison bodies unbox the `object` arg and drop the
        // null guard on the by-value typed arg. A struct class emits no such triple.
        let recordIsStruct =
            match input with
            | NominalEmissionInput.Record(_, _, isStruct) -> isStruct
            | _ -> false

        // A reference to one of this type's own members (field / tag / ctor): a generic
        // type reaches it through a `MemberRef` on the open self-`TypeSpec`
        // (`Box\`1<!0>::n`), a monomorphic type through the resolved `Def` token.
        let selfMemberRef (kind: UserMemberKind) (monoHandle: EntityHandle) : EntityHandle =
            if isGeneric then
                icodegen.UserGenericMemberRef(td.TypeKey, typarMarkers, kind)
            else
                monoHandle

        // The `extends` column for this `TypeDefinition`. Defaults to `Object`; the
        // struct-record and class arms overwrite it.
        let mutable baseTypeHandle = provider.ObjectType

        match input with
        | NominalEmissionInput.Union(cases, _) ->
            let tagField = toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))
            let unionCtor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))

            let ctorBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef []))

            asm.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.NullaryCtorSignature()
                    BodyOffset = ctorBodyOffset
                    ParamNames = []
                    MethodTypars = []
                }
            )

            cases
            |> List.iteri (fun tag c ->
                let fieldHandles =
                    [
                        for fi in 0 .. c.Fields.Length - 1 ->
                            toEntity (asm.FieldDef(FieldKey.UnionCaseField(td.Key, c.Name, fi)))
                    ]

                let ctorRef = selfMemberRef (UserMemberKind.UnionMember UnionMember.Ctor) unionCtor
                let tagRef = selfMemberRef (UserMemberKind.UnionMember UnionMember.Tag) tagField

                let fieldRefs =
                    [
                        for fi in 0 .. List.length fieldHandles - 1 ->
                            selfMemberRef (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))) fieldHandles.[fi]
                    ]

                let factoryBody =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildUnionFactory ctorRef tag tagRef fieldRefs))

                let paramTys = [ for (_, t) in c.Fields -> t ]

                let factorySig =
                    provider.StaticMethodSignature(paramTys, FTUnion(td.TypeKey, EqArray.ofList typarMarkers))

                asm.AddPrepared(
                    MethodKey.UnionFactory(td.Key, c.Name),
                    {
                        Signature = factorySig
                        BodyOffset = factoryBody
                        ParamNames = argNames (List.length paramTys)
                        MethodTypars = []
                    }
                )
            )

        | NominalEmissionInput.Record(fields, _, _) ->
            // A `[<Struct>]` record extends `System.ValueType`; a reference record
            // keeps the `Object` default.
            if recordIsStruct then
                baseTypeHandle <- provider.ValueTypeBase

            let fieldHandles =
                [
                    for f in fields -> toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name)))
                ]

            // A raw `FieldDefinition` token in `stfld` resolves to the wrong slot for a
            // field at index >= 1 of a generic type, so each generic store routes
            // through the field's `MemberRef` on the open self-`TypeSpec` (`R\`1<!0>::Y`).
            let ctorFieldRefs =
                [
                    for i, f in List.indexed fields ->
                        selfMemberRef (UserMemberKind.RecordMember(RecordMember.Field f.Name)) fieldHandles.[i]
                ]

            // `System.ValueType` has no accessible ctor and value types do not chain,
            // so a struct record's `.ctor` only stores fields; a reference record
            // chains `Object::.ctor`.
            let ctorBody =
                if recordIsStruct then
                    Emit.buildStructCtor ctorFieldRefs
                else
                    Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs

            let ctorBodyOffset = Cil.buildBody encodeLocals bodyStream (IlIr.lower ctorBody)

            asm.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ for f in fields -> f.Type ]
                    BodyOffset = ctorBodyOffset
                    ParamNames = [ for f in fields -> f.Name ]
                    MethodTypars = []
                }
            )

        | NominalEmissionInput.Class cd ->
            let instanceFields = cd.Fields
            let ctorParams = cd.CtorParams
            let baseType = cd.BaseType
            let staticLets = TPreambleEntryG.lets cd.StaticPreamble
            let secondaryCtors = cd.SecondaryCtors
            let baseCtorCall = cd.BaseCtorCall
            let isStruct = cd.ValueKind <> ClassValueKind.RefType

            let baseShape =
                match baseType with
                | ValueNone -> BaseShape.NoBase
                // A parent carrying type ARGUMENTS resolves through a `TypeSpec` whatever its
                // flavour, so only an argless one is worth classifying further.
                | ValueSome b when not b.Args.IsEmpty -> BaseShape.Generic b.Frozen
                | ValueSome b ->
                    match b.Frozen with
                    | FTClass _ ->
                        match icodegen.ClassOrigin b.Key with
                        | ClassOrigin.Foreign tref -> BaseShape.ExternalBase(b.Key, tref)
                        | ClassOrigin.Local handle -> BaseShape.LocalMono(b.Key, handle)
                        | ClassOrigin.Unresolved ->
                            failwithf "Emit: class '%s' inherits %A, which resolves to no class" td.Name b.Key
                    // An intrinsic-class parent (`inherit exn`) arrives as the canon
                    // `FTConst`, not an `FTClass`, so resolve it to its platform class
                    // (`System.Exception`).
                    | FTConst _ ->
                        match icodegen.IntrinsicClassBase b.Key with
                        | ValueSome(platformKey, tref) -> BaseShape.ExternalBase(platformKey, tref)
                        | ValueNone -> BaseShape.Generic b.Frozen
                    | _ -> BaseShape.Generic b.Frozen

            // A non-generic parent is its token directly because the `extends` column
            // rejects a `TypeSpec` that merely wraps a plain class.
            match baseShape with
            | BaseShape.NoBase ->
                if isStruct then
                    baseTypeHandle <- provider.ValueTypeBase
            | BaseShape.ExternalBase(_, handle)
            | BaseShape.LocalMono(_, handle) -> baseTypeHandle <- handle
            | BaseShape.Generic bt -> baseTypeHandle <- icodegen.TypeToken bt

            let emitPrimaryCtor = isStruct || cd.HasPrimaryCtor || List.isEmpty secondaryCtors

            // The chain target for a secondary that chains to the primary. In the
            // suppressed val-field form it aliases the first secondary, so
            // `primaryCtorRef` resolves to a real token, not an absent `NominalCtor`.
            let classCtor =
                if emitPrimaryCtor then
                    toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                else
                    toEntity (asm.MethodDef(MethodKey.SecondaryCtor(td.Key, 0)))

            // A generic class's ctor `stfld` sequence reaches each field through a
            // `MemberRef` on the open self-`TypeSpec` (`Box\`1<!0>::n`): the raw
            // `FieldDefinition` token resolves to the wrong slot at index >= 1.
            let ctorFieldRefs =
                [
                    for p in ctorParams ->
                        selfMemberRef
                            (UserMemberKind.ClassMember(ClassMember.Field p.Name))
                            (toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))))
                ]

            // An external base's `.ctor` is minted BY KEY from the `ChosenCtor` identity
            // the front end recorded, falling back to arity; the parameterless one is
            // minted off the `TypeRef`, since a protected ctor is not in the member set.
            let ctorChain =
                match baseShape, baseCtorCall with
                | BaseShape.ExternalBase(baseKey, _), ValueSome bcc when not bcc.Args.IsEmpty ->
                    let argTypes = [ for a in bcc.Args -> TastAccessor.exprTy a ]

                    match icodegen.TryEmitCtor(baseKey, bcc.ChosenCtor, [], argTypes) with
                    | ValueSome recipe -> Emit.CtorChain.Base(recipe.Handle, EqArray.toList bcc.Args)
                    | ValueNone ->
                        failwithf
                            "Emit: class '%s' inherits external base %A but no '.ctor' overload matches its %d base-ctor argument(s)"
                            td.Name
                            baseKey
                            bcc.Args.Length
                | BaseShape.ExternalBase(baseKey, _), _ ->
                    match icodegen.ExternalParameterlessBaseCtor baseKey with
                    | ValueSome extCtor -> Emit.CtorChain.Base(extCtor, [])
                    | ValueNone ->
                        failwithf
                            "Emit: class '%s' inherits external base %A but its parameterless '.ctor()' could not be minted"
                            td.Name
                            baseKey
                | _, ValueSome bcc ->
                    let baseKey, baseArgs =
                        match baseShape with
                        | BaseShape.LocalMono(k, _) -> k, []
                        | BaseShape.Generic(FTClass(n, xs)) -> n, EqArray.toList xs
                        | _ -> failwithf "Emit: class '%s' has a base-ctor call but no class base type" td.Name

                    // `inherit Base(args)` reaches any of the base's ctors, so the chain target
                    // is picked on the same two axes a `TExpr.New` is.
                    let baseCtorHandle =
                        match classes.TryGetValue baseKey with
                        | true, bc ->
                            let argTypes = [ for a in bcc.Args -> TastAccessor.exprTy a ]
                            let kind, handle = EmitResolve.pickLocalCtor td.Name bc baseArgs argTypes

                            if List.isEmpty bc.Typars then
                                handle
                            else
                                icodegen.UserGenericMemberRef(baseKey, baseArgs, kind)
                        | false, _ ->
                            failwithf
                                "Emit: base class '%A' of '%s' is not an emitted project-local class"
                                baseKey
                                td.Name

                    Emit.CtorChain.Base(baseCtorHandle, EqArray.toList bcc.Args)
                | _, ValueNone when isStruct -> Emit.CtorChain.None
                | _, ValueNone -> Emit.CtorChain.Base(provider.ObjectCtorRef, [])

            // Base args are the only ctor expressions that reference a primary-ctor param
            // directly (`this` does not exist yet; a preamble entry reaches one through its
            // backing field), so this is empty for every other chain shape.
            let ctorParamArgs =
                match baseCtorCall with
                | ValueSome bcc -> EqArray.toList bcc.CtorParams
                | ValueNone -> []

            // The instance preamble, resolved through the same self-`MemberRef` shape as
            // the ctor-param stores.
            let instanceSteps =
                [
                    for entry in cd.InstancePreamble ->
                        match entry with
                        | TPreambleEntryG.Let l ->
                            Emit.PreambleStep.Store(
                                selfMemberRef
                                    (UserMemberKind.ClassMember(ClassMember.Field l.Name))
                                    (toEntity (asm.FieldDef(FieldKey.ClassLetField(td.Key, l.Name)))),
                                l.Init
                            )
                        | TPreambleEntryG.Do e -> Emit.PreambleStep.Run e
                ]

            let ctorBody =
                Emit.buildClassPrimaryCtor emitCtx ctorChain cd.ThisKey ctorParamArgs ctorFieldRefs instanceSteps

            if emitPrimaryCtor then
                let ctorBodyOffset = Cil.buildBody encodeLocals bodyStream (IlIr.lower ctorBody)

                asm.AddPrepared(
                    MethodKey.NominalCtor td.Key,
                    {
                        Signature = provider.RecordCtorSignature [ for p in ctorParams -> p.Type ]
                        BodyOffset = ctorBodyOffset
                        ParamNames = [ for p in ctorParams -> p.Name ]
                        MethodTypars = []
                    }
                )

            // The `.cctor` runs stores and effects interleaved, in declaration order,
            // which is load-bearing:
            // `static let a = f()` / `static do g a` / `static let b = h()`.
            if not (List.isEmpty cd.StaticPreamble) then
                let staticFields = classes.[td.TypeKey].StaticFields

                let cctorSteps =
                    [
                        for entry in cd.StaticPreamble ->
                            match entry with
                            | TPreambleEntryG.Let sl -> Emit.PreambleStep.Store(staticFields.[sl.Name], sl.Init)
                            | TPreambleEntryG.Do e -> Emit.PreambleStep.Run e
                    ]

                let cctorBody =
                    Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx cctorSteps))

                asm.AddPrepared(
                    MethodKey.NominalCctor td.Key,
                    {
                        Signature = provider.CctorSignature()
                        BodyOffset = cctorBody
                        ParamNames = []
                        MethodTypars = []
                    }
                )

            // Each secondary is a `.ctor` overload whose body runs its `let`-preamble,
            // then either chains the primary `.ctor` or stores explicit field inits.
            if not (List.isEmpty secondaryCtors) then
                let primaryCtorRef =
                    selfMemberRef (UserMemberKind.ClassMember ClassMember.Ctor) classCtor

                secondaryCtors
                |> List.iteri (fun i sc ->
                    let paramTys = [ for (_, t) in sc.Params -> t ]

                    let lets = EqArray.toList sc.Lets

                    let ctorIr =
                        if not sc.FieldInits.IsEmpty then
                            // Both ctor-param backing fields and explicit `val` fields
                            // are eligible.
                            let fieldHandleOf name =
                                if isGeneric then
                                    icodegen.UserGenericMemberRef(
                                        td.TypeKey,
                                        typarMarkers,
                                        UserMemberKind.ClassMember(ClassMember.Field name)
                                    )
                                elif ctorParams |> List.exists (fun (p: Frozen.TRecordField) -> p.Name = name) then
                                    toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, name)))
                                elif instanceFields |> List.exists (fun (f: Frozen.TRecordField) -> f.Name = name) then
                                    toEntity (asm.FieldDef(FieldKey.ClassInstanceField(td.Key, name)))
                                else
                                    failwithf "Emit: class '%s' secondary ctor inits unknown field '%s'" td.Name name

                            let fieldInits = [ for fi in sc.FieldInits -> fieldHandleOf fi.Field, fi.Init ]

                            Emit.buildSecondaryCtorFieldInit emitCtx sc.Params lets fieldInits
                        else
                            let primaryArgs = EqArray.toList sc.PrimaryArgs
                            Emit.buildSecondaryCtor emitCtx sc.Params lets primaryCtorRef primaryArgs

                    let scBody = Cil.buildBody encodeLocals bodyStream (IlIr.lower ctorIr)

                    asm.AddPrepared(
                        MethodKey.SecondaryCtor(td.Key, i),
                        {
                            Signature = provider.RecordCtorSignature paramTys
                            BodyOffset = scBody
                            ParamNames = argNames sc.Params.Length
                            MethodTypars = []
                        }
                    )
                )

        // Interface-impl member bodies emit as virtual methods the runtime binds to the
        // `InterfaceImpl` row by name + signature. The synthesised eq/comparison/format
        // impls use disjoint `MethodKey`s, so the two never collide on a method row.
        let userInterfaces = userInterfacesOf input

        let prepareMember (index: int) (isIfaceImpl: bool) (mem: TastAccessor.TypeMember) =
            // `(name, ty)[]` in ABI order — position IS the typar index. Feeds the
            // `GENERIC` header arity and the `GenericParam` rows.
            let methodTypars = mem.MethodTypeParams
            let isGenericMethod = methodTypars.Length > 0

            // A `unit`-returning member, static or instance, encodes as genuine CLR
            // `void`. Emitting the `unit`-as-`ValueTuple` return instead breaks
            // cross-assembly binding: a consumer's void member-ref misses it.
            let returnsVoid =
                match mem.ReturnTy with
                | FTUnit -> true
                | _ -> false

            let bodyOffset =
                try
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildMember emitCtx mem.ThisKey mem.BaseKey mem.Params returnsVoid mem.Body))
                with ex ->
                    raise (
                        System.Exception(
                            sprintf "While lowering body of member '%A.%s'\n%s" td.Key mem.Name ex.Message,
                            ex
                        )
                    )

            let paramTys = [ for (_, t) in mem.Params -> t ]

            // A generic method needs the `GENERIC` calling-convention header count; its
            // own typars ride `FTTypar(Method, i)` nodes, encoded `!!i`.
            let signature =
                try
                    if returnsVoid && isGenericMethod then
                        provider.GenericMethodOnTypeSignatureVoid(methodTypars.Length, paramTys, not mem.IsStatic)
                    elif returnsVoid && mem.IsStatic then
                        provider.StaticMethodSignatureVoid paramTys
                    elif returnsVoid then
                        provider.InstanceMethodSignatureVoid paramTys
                    elif isGenericMethod then
                        provider.GenericMethodOnTypeSignature(
                            methodTypars.Length,
                            paramTys,
                            mem.ReturnTy,
                            not mem.IsStatic
                        )
                    elif mem.IsStatic then
                        provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                    else
                        provider.InstanceMethodSignature(paramTys, mem.ReturnTy)
                with ex ->
                    // A leaked metavar / unresolved type constructor surfaces here as an anonymous
                    // encoder failure; identify the member and keep the original as
                    // `InnerException`, whose stack pinpoints the encode site.
                    raise (System.Exception(sprintf "While encoding signature of member '%A.%s'" td.Key mem.Name, ex))

            asm.AddPrepared(
                MethodKey.Member(td.Key, index),
                {
                    Signature = signature
                    BodyOffset = bodyOffset
                    ParamNames = argNames mem.Params.Length
                    // The metadata name drops the F# leading quote: `'T` → `T`.
                    MethodTypars = [ for (n, _) in methodTypars -> n.TrimStart('\'') ]
                }
            )

        for (index, isIfaceImpl, mem) in NominalMembers.indexed members userInterfaces do
            prepareMember index isIfaceImpl mem

        let selfTy (ts: FrozenType list) : FrozenType =
            match input with
            | NominalEmissionInput.Union _ -> FTUnion(td.TypeKey, EqArray.ofList ts)
            | NominalEmissionInput.Record _ -> FTRecord(td.TypeKey, EqArray.ofList ts)
            | NominalEmissionInput.Class _ -> FTClass(td.TypeKey, EqArray.ofList ts)

        let selfTyMarkers = selfTy typarMarkers

        // The handle the equality/comparison bodies `isinst`/`unbox.any` against: a
        // generic type's open self-`TypeSpec`, a mono type's `TypeDef`.
        let selfTypeHandle =
            if not isGeneric then
                provider.UserTypeHandle td.TypeKey
            else
                match input with
                | NominalEmissionInput.Union _ -> provider.GenericUnionSelfSpec td.TypeKey
                | NominalEmissionInput.Record _ -> provider.GenericRecordSelfSpec td.TypeKey
                | NominalEmissionInput.Class _ -> provider.UserTypeHandle td.TypeKey

        // `(handle, type)` flat across a union's cases in declaration order. Sound
        // because inactive-case fields are always default. Equality and comparison
        // consume the identical set.
        let structuralFields () : (EntityHandle * FrozenType) list =
            match input with
            | NominalEmissionInput.Union(cases, _) ->
                let emitted = unions.[td.TypeKey]

                [
                    for c in cases do
                        let caseFields = emitted.Cases.[c.Name].Fields

                        for fi in 0 .. c.Fields.Length - 1 ->
                            selfMemberRef (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))) caseFields.[fi],
                            snd c.Fields.[fi]
                ]
            | NominalEmissionInput.Record _ ->
                [
                    for (name, h, fty) in records.[td.TypeKey].Fields ->
                        selfMemberRef (UserMemberKind.RecordMember(RecordMember.Field name)) h, fty
                ]
            | NominalEmissionInput.Class _ -> []

        let tagFieldRef () =
            selfMemberRef (UserMemberKind.UnionMember UnionMember.Tag) unions.[td.TypeKey].TagField

        let bodyOf ir =
            Cil.buildBody encodeLocals bodyStream (IlIr.lower ir)

        // `GetHashCode` + `Equals(object)` override + typed `Equals(Self)`. Union and
        // record differ only in the body builders; the row signatures are identical.
        let prepareEqualityTriple getHashCodeIr equalsObjIr equalsTypedIr =
            asm.AddPrepared(
                MethodKey.EqGetHashCode td.Key,
                {
                    Signature = provider.GetHashCodeOverrideSignature()
                    BodyOffset = bodyOf getHashCodeIr
                    ParamNames = []
                    MethodTypars = []
                }
            )

            asm.AddPrepared(
                MethodKey.EqEqualsObj td.Key,
                {
                    Signature = provider.EqualsOverrideSignature()
                    BodyOffset = bodyOf equalsObjIr
                    ParamNames = [ "obj" ]
                    MethodTypars = []
                }
            )

            asm.AddPrepared(
                MethodKey.EqEqualsTyped td.Key,
                {
                    Signature = provider.EqualsTypedSignature selfTyMarkers
                    BodyOffset = bodyOf equalsTypedIr
                    ParamNames = [ "other" ]
                    MethodTypars = []
                }
            )

        // The comparison pair: typed `CompareTo(Self)` first (its handle feeds
        // `CompareTo(object)`'s body), then the `CompareTo(object)` override.
        let prepareComparisonPair compareToTypedIr compareToObjIr =
            asm.AddPrepared(
                MethodKey.CmpCompareToTyped td.Key,
                {
                    Signature = provider.CompareToTypedSignature selfTyMarkers
                    BodyOffset = bodyOf compareToTypedIr
                    ParamNames = [ "other" ]
                    MethodTypars = []
                }
            )

            asm.AddPrepared(
                MethodKey.CmpCompareToObj td.Key,
                {
                    Signature = provider.CompareToOverrideSignature()
                    BodyOffset = bodyOf compareToObjIr
                    ParamNames = [ "obj" ]
                    MethodTypars = []
                }
            )

        let emitsEqualityTriple = td.EqualitySupport = EqualityVerdict.Structural

        if emitsEqualityTriple then
            match input with
            | NominalEmissionInput.Union _ ->
                let support: Emit.UnionEqualitySupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        TagField = tagFieldRef ()
                        Fields = structuralFields ()
                        IntType = FTConst(RuntimeNames.intKey, EqArray.empty)
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                prepareEqualityTriple
                    (Emit.buildUnionGetHashCode support)
                    (Emit.buildUnionEquals support)
                    (Emit.buildUnionEqualsTyped support)
            | NominalEmissionInput.Record _ ->
                let support: Emit.RecordEqualitySupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        Fields = structuralFields ()
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                prepareEqualityTriple
                    (Emit.buildRecordGetHashCode support)
                    (Emit.buildRecordEquals recordIsStruct support)
                    (Emit.buildRecordEqualsTyped recordIsStruct support)
            | NominalEmissionInput.Class _ -> ()

        let emitsComparisonPair = td.ComparisonSupport = ComparisonVerdict.Structural

        if emitsComparisonPair then
            let typedCompareTo = toEntity (asm.MethodDef(MethodKey.CmpCompareToTyped td.Key))

            match input with
            | NominalEmissionInput.Union _ ->
                let cmpSupport: Emit.UnionComparisonSupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        TagField = tagFieldRef ()
                        Fields = structuralFields ()
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                prepareComparisonPair
                    (Emit.buildUnionCompareTo cmpSupport)
                    (Emit.buildUnionCompareToObj cmpSupport typedCompareTo)
            | NominalEmissionInput.Record _ ->
                let cmpSupport: Emit.RecordComparisonSupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        Fields = structuralFields ()
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                prepareComparisonPair
                    (Emit.buildRecordCompareTo recordIsStruct cmpSupport)
                    (Emit.buildRecordCompareToObj recordIsStruct cmpSupport typedCompareTo)
            | NominalEmissionInput.Class _ -> ()

        // The synthesised `IStructuralFormattable.Format(IFormatSink)` (`%A`), emitted for
        // every record and union independently of the equality / comparison verdicts.
        let emitsStructuralFormat =
            match input with
            | NominalEmissionInput.Union _
            | NominalEmissionInput.Record _ -> not (NominalMembers.declaresStructuralFormat userInterfaces)
            | NominalEmissionInput.Class _ -> false

        if emitsStructuralFormat then
            let formatIr =
                match input with
                | NominalEmissionInput.Union(cases, _) ->
                    let emitted = unions.[td.TypeKey]

                    let formatCases =
                        [
                            for c in cases ->
                                let caseFields = emitted.Cases.[c.Name].Fields

                                {
                                    EmitStructuralFormat.UnionFormatCase.Name = c.Name
                                    EmitStructuralFormat.UnionFormatCase.Fields =
                                        [
                                            for fi in 0 .. c.Fields.Length - 1 ->
                                                selfMemberRef
                                                    (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi)))
                                                    caseFields.[fi],
                                                snd c.Fields.[fi]
                                        ]
                                }
                        ]

                    let support: EmitStructuralFormat.UnionFormatSupport =
                        {
                            Sink = provider.FormatSinkHandles
                            MkString = ctx.UserString
                            BoxToken = icodegen.TypeToken
                            TagField = tagFieldRef ()
                            Cases = formatCases
                        }

                    EmitStructuralFormat.buildUnionFormat support
                | NominalEmissionInput.Record _ ->
                    let support: EmitStructuralFormat.RecordFormatSupport =
                        {
                            Sink = provider.FormatSinkHandles
                            MkString = ctx.UserString
                            BoxToken = icodegen.TypeToken
                            Fields =
                                [
                                    for (name, h, fty) in records.[td.TypeKey].Fields ->
                                        name,
                                        selfMemberRef (UserMemberKind.RecordMember(RecordMember.Field name)) h,
                                        fty
                                ]
                        }

                    EmitStructuralFormat.buildRecordFormat support
                | NominalEmissionInput.Class _ -> failwith "unreachable: class has no structural Format"

            asm.AddPrepared(
                MethodKey.FmtFormat td.Key,
                {
                    Signature = provider.StructuralFormatSignature()
                    BodyOffset = bodyOf formatIr
                    ParamNames = [ "sink" ]
                    MethodTypars = []
                }
            )

        // The BCL members a capability's platform interface INHERITS but never declared:
        // unsynthesised, the CLR refuses to load the type. Only the non-generic slots
        // need it, because a generic slot binds implicitly by the authored member's signature.
        let coSlots =
            CapabilityCoSlots.required asm.Symbols [ for (iface, _) in userInterfaces -> iface ]

        // The authored capability member a shim forwards to, as a handle callable from
        // inside this type. Scoped to the impl block of the capability that DEMANDED the
        // slot, so a like-named member of another interface can never be picked up.
        let capabilityMember (ifaceTy: FrozenNominal) (slot: CoSlot) : EntityHandle * FrozenType =
            let name =
                match CapabilityCoSlots.forwardsTo slot with
                | ValueSome n -> n
                | ValueNone -> failwithf "Emit: co-slot '%A' forwards to no capability member" slot

            let hit =
                NominalMembers.ofInterface members userInterfaces ifaceTy
                |> List.tryFind (fun (_, m) -> m.Name = name)

            match hit with
            | None ->
                failwithf
                    "Emit: type '%A' implements capability '%A', whose co-slot forwards to member '%s', but that impl block declares no such member"
                    td.Key
                    ifaceTy.Key
                    name
            | Some(i, mem) ->
                let kind =
                    UserMemberKind.Member(memberMetaName mem, false, 0, [ for (_, t) in mem.Params -> t ], mem.ReturnTy)

                selfMemberRef kind (toEntity (asm.MethodDef(MethodKey.Member(td.Key, i)))), mem.ReturnTy

        for (ifaceTy, slot) in coSlots do
            let signature, body =
                match slot with
                | CoSlot.EnumerableGetEnumerator ->
                    let getEnumerator, _ = capabilityMember ifaceTy slot

                    provider.InstanceMethodSignature(
                        [],
                        FTClass(SymbolKeyOps.typeKeyOf "System.Collections" "IEnumerator", EqArray.empty)
                    ),
                    Emit.buildEnumerableGetEnumeratorCoSlot getEnumerator
                | CoSlot.EnumeratorCurrent ->
                    let current, elemTy = capabilityMember ifaceTy slot

                    provider.InstanceMethodSignature([], FTConst(RuntimeNames.objKey, EqArray.empty)),
                    Emit.buildEnumeratorCurrentCoSlot current (icodegen.TypeToken elemTy)
                | CoSlot.EnumeratorReset ->
                    provider.InstanceMethodSignatureVoid [],
                    Emit.buildEnumeratorResetCoSlot provider.NotSupportedExceptionCtor

            asm.AddPrepared(
                MethodKey.CapCoSlot(td.Key, slot),
                {
                    Signature = signature
                    BodyOffset = bodyOf body
                    ParamNames = []
                    MethodTypars = []
                }
            )

        // One `InterfaceImpl` handle per implemented interface. A generic interface arg
        // (`IEnumerable<'T>`) carries its `'T` as `FTTypar(Declaring, i)`, encoded `!i`.
        let interfaces =
            if
                emitsEqualityTriple
                || emitsComparisonPair
                || emitsStructuralFormat
                || not (List.isEmpty userInterfaces)
            then
                [
                    if emitsEqualityTriple then
                        provider.EquatableInterfaceSpec selfTyMarkers
                    if emitsComparisonPair then
                        provider.ComparableInterfaceSpec selfTyMarkers
                        provider.IComparableType
                    if emitsStructuralFormat then
                        provider.StructuralFormattableInterface
                    for (iface, _) in userInterfaces do
                        provider.InterfaceHandleOf iface.Frozen
                ]
            else
                []

        asm.AddTypeRowExtras(
            TypeSlotKey.Nominal td.Key,
            {
                Interfaces = interfaces
                BaseType = baseTypeHandle
            }
        )
