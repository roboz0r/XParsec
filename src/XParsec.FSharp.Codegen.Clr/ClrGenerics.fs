namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// `TypeSpec` + `MemberRef` minting for generic user types emitted into this assembly: unions,
/// records, classes and closures, whose declaring typars arrive as `FTTypar(Declaring, i)` nodes
/// and encode as `!i` against the instantiated parent spec.
type internal ClrGenerics(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let userTypes = env.UserTypes
    let genericUnions = env.GenericUnions
    let genericRecords = env.GenericRecords
    let genericClasses = env.GenericClasses
    let genericClosures = env.GenericClosures
    let userValueTypes = env.UserValueTypes
    let encodeType te t = enc.EncodeType(te, t)

    let genericClassTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        let shape = genericClasses.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        // A `[<Struct>]` value type must carry the `VALUETYPE` tag, else the GENERICINST
        // encodes as `CLASS` and the loader faults "value type mismatch" when
        // constructing or dispatching on a generic struct.
        let isVt = userValueTypes.Contains key
        let g = te.GenericInstantiation(userTypes.[key], shape.Typars.Length, isVt)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClassMemberRef (key: TypeKey) (args: FrozenType list) (which: ClassMember) : EntityHandle =
        let shape = genericClasses.[key]
        let parent = genericClassTypeSpec key args

        match which with
        | ClassMember.Ctor ->
            // Only the primary ctor's own parameters (the leading `CtorParamCount`
            // entries), not the trailing `val`/`static let` backing fields that also
            // live in `Fields` for name-based `ClassMember.Field` resolution.
            let paramTys =
                shape.Fields |> EqArray.truncate shape.CtorParamCount |> EqArray.map snd

            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    paramTys.Length,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | ClassMember.SecondaryCtor paramTys ->
            // Same `.ctor` MemberRef shape as the primary, over the secondary ctor's
            // own parameters.
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | ClassMember.Field fieldName ->
            match shape.Fields |> EqArray.tryFind (fun (n, _) -> n = fieldName) with
            | ValueSome(_, declTy) ->
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | ValueNone -> failwithf "ClrProvider: generic class '%A' has no field '%s'" key fieldName

    let genericUnionTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        let shape = genericUnions.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()

        let g =
            te.GenericInstantiation(userTypes.[key], shape.Typars.Length, userValueTypes.Contains key)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericUnionMemberRef (key: TypeKey) (args: FrozenType list) (which: UnionMember) : EntityHandle =
        let shape = genericUnions.[key]
        let parent = genericUnionTypeSpec key args

        // The union named from inside its own bodies: the type a factory returns and a
        // `_unique_<Case>` singleton is typed at.
        let selfTy = FTUnion(key, EqArray.ofList (declaringMarkers shape.Typars.Length))

        // A flat regime's slots, and the `Payload` struct a `StructTagged` union nests, in
        // the same scope.
        let slots =
            match shape.Home with
            | ValueSome home -> home.Slots
            | ValueNone -> []

        let payloadTy = UnionPayloadType.payloadTyDeclaring key shape.Typars.Length

        let caseFields cn : EqArray<string * FrozenType> =
            match shape.Cases |> EqArray.tryFind (fun c -> c.Name = cn) with
            | ValueSome c -> c.Fields
            | ValueNone -> failwithf "ClrProvider: generic union '%A' has no case '%s'" key cn

        match which with
        | UnionMember.Ctor ->
            let s = BlobBuilder()

            let paramTys =
                match UnionCtorShape.ofRegime shape.Regime with
                | UnionCtorShape.FlatTagged -> [ RuntimeNames.intTy; payloadTy ]
                | UnionCtorShape.Flat -> [ for s in slots -> s.Ty ]
                | UnionCtorShape.TagOnly -> [ RuntimeNames.intTy ]
                | UnionCtorShape.Nullary -> []

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | UnionMember.Tag ->
            let s = BlobBuilder()
            BlobEncoder(s).FieldSignature().Int32()
            toEntity (ctx.MemberRef(parent, "_tag", s))
        | UnionMember.GetTag ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()), (fun (_: ParametersEncoder) -> ()))

            toEntity (ctx.MemberRef(parent, "get_Tag", s))
        | UnionMember.Field(caseName, idx) ->
            // The case's own type is registered as a generic class over the union's typars,
            // so its field refs come from that family rather than off `parent`.
            let metaName, _ = (caseFields caseName).[idx]
            genericClassMemberRef (UnionCaseType.key key caseName) args (ClassMember.Field metaName)
        | UnionMember.Slot slotKey ->
            match slots |> List.tryFind (fun s -> s.Key = slotKey), shape.Home with
            | Some slot, ValueSome(UnionSlotHome.Inline _) ->
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) slot.Ty
                toEntity (ctx.MemberRef(parent, slot.MetaName, s))
            // `Payload` is registered as a generic class over the union's typars, so its
            // slot refs come from that family.
            | Some slot, ValueSome(UnionSlotHome.Payload _) ->
                genericClassMemberRef (UnionPayloadType.payloadKey key) args (ClassMember.Field slot.MetaName)
            | Some _, ValueNone
            | None, _ -> failwithf "ClrProvider: generic union '%A' has no slot '%A'" key slotKey
        | UnionMember.Payload ->
            let s = BlobBuilder()
            encodeType (BlobEncoder(s).FieldSignature()) payloadTy
            toEntity (ctx.MemberRef(parent, UnionPayloadType.payloadFieldName, s))
        | UnionMember.CaseCtor caseName -> genericClassMemberRef (UnionCaseType.key key caseName) args ClassMember.Ctor
        | UnionMember.CaseSingleton caseName ->
            let s = BlobBuilder()
            encodeType (BlobEncoder(s).FieldSignature()) selfTy
            toEntity (ctx.MemberRef(parent, "_unique_" + caseName, s))
        | UnionMember.Factory caseName ->
            let paramTys = caseFields caseName |> EqArray.map snd
            let retTy = selfTy

            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    paramTys.Length,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, caseName, s))

    let genericRecordTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        let shape = genericRecords.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()

        let g =
            te.GenericInstantiation(userTypes.[key], shape.Typars.Length, userValueTypes.Contains key)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericRecordMemberRef (key: TypeKey) (args: FrozenType list) (which: RecordMember) : EntityHandle =
        let fields = genericRecords.[key].Fields
        let parent = genericRecordTypeSpec key args

        match which with
        | RecordMember.Ctor ->
            let paramTys = fields |> EqArray.map snd
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    paramTys.Length,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | RecordMember.Field fieldName ->
            match fields |> EqArray.tryFind (fun (n, _) -> n = fieldName) with
            | ValueSome(_, declTy) ->
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | ValueNone -> failwithf "ClrProvider: generic record '%A' has no field '%s'" key fieldName

    /// The parent `TypeSpec` of a generic user type, whichever family declares it. Every
    /// family's registry is keyed by the nominal `TypeKey`, so the key alone picks the arm.
    let genericTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        if genericUnions.ContainsKey key then
            genericUnionTypeSpec key args
        elif genericRecords.ContainsKey key then
            genericRecordTypeSpec key args
        elif genericClasses.ContainsKey key then
            genericClassTypeSpec key args
        else
            failwithf "ClrProvider: '%A' is not a registered generic union / record / class" key

    /// An augmentation member of ANY generic user type (`UserMemberKind.Member`). One
    /// encoding serves all, because the member ref does not depend on what declares it: the
    /// `key` picks the parent `TypeSpec`, and the rest is the member's own signature.
    let genericMemberRef
        (key: TypeKey)
        (args: FrozenType list)
        (metaName: string)
        (isStatic: bool)
        (methodTyparCount: int)
        (paramTys: FrozenType list)
        (retTy: FrozenType)
        : EntityHandle =
        let parent = genericTypeSpec key args
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = methodTyparCount, isInstanceMethod = not isStatic)
            .Parameters(
                List.length paramTys,
                // A `unit`-returning member Def is emitted `void`, static and instance alike,
                // so this MemberRef must encode `void` too or it misses the `MethodDef`
                // (`MissingMethodException`).
                (fun (ret: ReturnTypeEncoder) ->
                    match retTy with
                    | FTUnit -> ret.Void()
                    | _ -> encodeType (ret.Type()) retTy
                ),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        encodeType (pars.AddParameter().Type()) p
                )
            )

        toEntity (ctx.MemberRef(parent, metaName, s))

    let genericClosureTypeSpec (name: string) (args: FrozenType list) : EntityHandle =
        let shape = genericClosures.[name]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()

        let g = te.GenericInstantiation(shape.DefHandle, shape.TyparCount, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClosureMemberRef (name: string) (args: FrozenType list) (which: ClosureMember) : EntityHandle =
        let shape = genericClosures.[name]
        let parent = genericClosureTypeSpec name args

        let mint () =
            match which with
            | ClosureMember.Ctor ->
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        List.length shape.CaptureSigs,
                        (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                        (fun (pars: ParametersEncoder) ->
                            for c in shape.CaptureSigs do
                                encodeType (pars.AddParameter().Type()) c
                        )
                    )

                toEntity (ctx.MemberRef(parent, ".ctor", s))
            | ClosureMember.CaptureField idx ->
                if idx < 0 || idx >= List.length shape.CaptureSigs then
                    failwithf
                        "ClrProvider: generic closure '%s' has %d capture fields, asked for index %d"
                        name
                        (List.length shape.CaptureSigs)
                        idx

                let captureTy = shape.CaptureSigs.[idx]
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) captureTy
                toEntity (ctx.MemberRef(parent, sprintf "capture%d" idx, s))
            | ClosureMember.Invoke ->
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        1,
                        (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) shape.ResultTy),
                        (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) shape.ParamTy)
                    )

                toEntity (ctx.MemberRef(parent, "Invoke", s))

        // `parent` is minted under the caller's ambient closure scope; `mint` encodes against
        // THIS closure's typars, so force its scope on: under it `FTTypar(Declaring, i)`
        // encodes `!i` and `FTTypar(Method, j)` encodes `!(d + j)`.
        env.WithClosureTyparScope(shape.DeclaringTypars, mint)

    member _.GenericUnionMemberRef(key, args, which) = genericUnionMemberRef key args which
    member _.GenericRecordMemberRef(key, args, which) = genericRecordMemberRef key args which
    member _.GenericClassMemberRef(key, args, which) = genericClassMemberRef key args which

    member _.GenericMemberRef(key, args, metaName, isStatic, methodTyparCount, paramTys, retTy) =
        genericMemberRef key args metaName isStatic methodTyparCount paramTys retTy

    member _.GenericClosureTypeSpec(name, args) = genericClosureTypeSpec name args
    member _.GenericClosureMemberRef(name, args, which) = genericClosureMemberRef name args which

    /// A generic union's own instantiation `TypeSpec` over its declaring typars (`List`1<!0>`): the
    /// `isinst` target / `other`-local / typed-`Equals` self for its synthesised equality triple.
    /// The nominal `TypeKey` embeds the arity, so `Choice\`2`…`Choice\`7` stay distinct.
    member _.GenericUnionSelfSpec(key: TypeKey) : EntityHandle =
        let typars = genericUnions.[key].Typars
        genericUnionTypeSpec key (declaringMarkers typars.Length)

    member _.GenericRecordSelfSpec(key: TypeKey) : EntityHandle =
        let typars = genericRecords.[key].Typars
        genericRecordTypeSpec key (declaringMarkers typars.Length)
