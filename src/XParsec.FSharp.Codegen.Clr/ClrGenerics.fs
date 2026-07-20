namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// `TypeSpec` + `MemberRef` minting for generic user types emitted into *this* assembly — unions,
/// records, classes (whose declaring typars ride `FTTypar(Declaring, i)` nodes) and closures (whose
/// typars re-project onto the closure class's `!i` under `ClrEnv.ClosureTyparMode`).
type internal ClrGenerics(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let userTypes = env.UserTypes
    let genericUnions = env.GenericUnions
    let genericRecords = env.GenericRecords
    let genericClasses = env.GenericClasses
    let genericClosures = env.GenericClosures
    let userValueTypes = env.UserValueTypes
    let encodeType te t = enc.EncodeType(te, t)

    let genericUnionTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        let shape = genericUnions.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()

        let g = te.GenericInstantiation(userTypes.[key], shape.Typars.Length, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericUnionMemberRef (key: TypeKey) (args: FrozenType list) (which: UnionMember) : EntityHandle =
        let shape = genericUnions.[key]
        let parent = genericUnionTypeSpec key args

        let caseFields cn : EqArray<string * FrozenType> =
            match shape.Cases |> EqArray.tryFind (fun c -> c.Name = cn) with
            | ValueSome c -> c.Fields
            | ValueNone -> failwithf "ClrProvider: generic union '%A' has no case '%s'" key cn

        match which with
        | UnionMember.Ctor ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | UnionMember.Tag ->
            let s = BlobBuilder()
            BlobEncoder(s).FieldSignature().Int32()
            toEntity (ctx.MemberRef(parent, "_tag", s))
        | UnionMember.Field(caseName, idx) ->
            let metaName, declTy = (caseFields caseName).[idx]
            let s = BlobBuilder()
            encodeType (BlobEncoder(s).FieldSignature()) declTy
            toEntity (ctx.MemberRef(parent, metaName, s))
        | UnionMember.Factory caseName ->
            let paramTys = caseFields caseName |> EqArray.map snd

            let retTy =
                FTUnion(key, EqArray.init shape.Typars.Length (fun i -> FTTypar(TyparAxis.Declaring, i)))

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

        let g = te.GenericInstantiation(userTypes.[key], shape.Typars.Length, false)

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

    let genericClassTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        let shape = genericClasses.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        // A `[<Struct>]` value type's generic self-`TypeSpec` (the MemberRef parent
        // for its ctor / fields / members) must carry the `VALUETYPE` tag, else the
        // GENERICINST encodes as `CLASS` and the loader faults "value type mismatch"
        // when constructing or dispatching on a generic struct.
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
            // entries) — not the trailing `val`/`static let` backing fields that also
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
            // Same `.ctor` MemberRef shape as the primary, but the parameter
            // signature is the secondary ctor's own (the `paramTys` are in the
            // type's declaring-typar markers, so they encode as `!i` against the
            // instantiated parent `TypeSpec`).
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

    /// The parent `TypeSpec` of a generic user type, whichever family declares it. Every
    /// family's registry is keyed by the nominal `TypeKey`, so the key alone picks the
    /// arm — the caller never has to say which family it is. (The arms are NOT one shared
    /// encoder: only the class arm tags a `[<Struct>]` value type's instantiation
    /// `VALUETYPE`, and collapsing them would silently change how a struct record or a
    /// struct-declared union encodes.)
    let genericTypeSpec (key: TypeKey) (args: FrozenType list) : EntityHandle =
        if genericUnions.ContainsKey key then
            genericUnionTypeSpec key args
        elif genericRecords.ContainsKey key then
            genericRecordTypeSpec key args
        elif genericClasses.ContainsKey key then
            genericClassTypeSpec key args
        else
            failwithf "ClrProvider: '%A' is not a registered generic union / record / class" key

    /// An augmentation member of ANY generic user type (`UserMemberKind.Member`) — the one
    /// encoding, because the member ref does not depend on what declares the member: the
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
                // A `unit`-returning INSTANCE method is emitted `void` by the producer
                // (`NominalEmit`'s `returnsVoid`) and by the external-ref path — so this
                // MemberRef must encode `void` too, or it misses the void `MethodDef`
                // (`MissingMethodException`). Static `unit` methods keep the
                // `unit`-as-`ValueTuple` convention.
                (fun (ret: ReturnTypeEncoder) ->
                    match retTy with
                    | FTUnit when not isStatic -> ret.Void()
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

        // The `parent` TypeSpec above was minted under the caller's ambient closure
        // scope (off at a construction site inside a static method → `!!i`; on inside
        // an enclosing closure's body → the enclosing closure's slots); the
        // member-ref signature below speaks *this* closure's own typars, so force
        // its closure scope on: a `FTTypar(Declaring, i)` encodes `!i` and a
        // `FTTypar(Method, j)` the closure class's `!(d + j)`.
        let savedMode = env.ClosureTyparScope
        env.ClosureTyparScope <- ValueSome shape.DeclaringTypars

        let handle =
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

        env.ClosureTyparScope <- savedMode
        handle

    member _.GenericUnionMemberRef(key, args, which) = genericUnionMemberRef key args which
    member _.GenericRecordMemberRef(key, args, which) = genericRecordMemberRef key args which
    member _.GenericClassMemberRef(key, args, which) = genericClassMemberRef key args which

    member _.GenericMemberRef(key, args, metaName, isStatic, methodTyparCount, paramTys, retTy) =
        genericMemberRef key args metaName isStatic methodTyparCount paramTys retTy

    member _.GenericClosureTypeSpec(name, args) = genericClosureTypeSpec name args
    member _.GenericClosureMemberRef(name, args, which) = genericClosureMemberRef name args which

    /// A generic union's own instantiation `TypeSpec` over its declaring typars (`List`1<!0>`) — the
    /// `isinst` target / `other`-local / typed-`Equals` self for its synthesised equality triple.
    /// Keyed by the union's nominal `TypeKey` (which embeds the arity, disambiguating
    /// same-named overloads `Choice\`2`…`Choice\`7`) in `genericUnions`.
    member _.GenericUnionSelfSpec(key: TypeKey) : EntityHandle =
        let typars = genericUnions.[key].Typars
        genericUnionTypeSpec key [ for i in 0 .. typars.Length - 1 -> FTTypar(TyparAxis.Declaring, i) ]

    member _.GenericRecordSelfSpec(key: TypeKey) : EntityHandle =
        let typars = genericRecords.[key].Typars
        genericRecordTypeSpec key [ for i in 0 .. typars.Length - 1 -> FTTypar(TyparAxis.Declaring, i) ]
