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

    // Unions are keyed by their nominal `SymbolKey` (which embeds the arity, so
    // same-named overloads `Choice\`2`…`Choice\`7` don't collide) in `genericUnions`
    // / `userTypes`.
    let genericUnionTypeSpec (key: SymbolKey) (args: FrozenType list) : EntityHandle =
        let typars, _ = genericUnions.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericUnionMemberRef (key: SymbolKey) (args: FrozenType list) (which: UnionMember) : EntityHandle =
        let typars, cases = genericUnions.[key]
        let parent = genericUnionTypeSpec key args

        let caseFields cn =
            cases |> List.find (fun (n, _) -> n = cn) |> snd

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
            let paramTys = caseFields caseName |> List.map snd

            let retTy =
                FTUnion(
                    key,
                    EqArray.ofSeq (seq { for i in 0 .. List.length typars - 1 -> FTTypar(TyparAxis.Declaring, i) })
                )

            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, caseName, s))
        | UnionMember.Member(metaName, isStatic, paramTys, retTy) ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeType (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, metaName, s))

    let genericRecordTypeSpec (key: SymbolKey) (args: FrozenType list) : EntityHandle =
        let typars, _ = genericRecords.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericRecordMemberRef (key: SymbolKey) (args: FrozenType list) (which: RecordMember) : EntityHandle =
        let _, fields = genericRecords.[key]
        let parent = genericRecordTypeSpec key args

        match which with
        | RecordMember.Ctor ->
            let paramTys = fields |> List.map snd
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
        | RecordMember.Field fieldName ->
            match fields |> List.tryFind (fun (n, _) -> n = fieldName) with
            | Some(_, declTy) ->
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | None -> failwithf "ClrProvider: generic record '%A' has no field '%s'" key fieldName

    let genericClassTypeSpec (key: SymbolKey) (args: FrozenType list) : EntityHandle =
        let typars, _ = genericClasses.[key]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        // A `[<Struct>]` value type's generic self-`TypeSpec` (the MemberRef parent
        // for its ctor / fields / members) must carry the `VALUETYPE` tag, else the
        // GENERICINST encodes as `CLASS` and the loader faults "value type mismatch"
        // when constructing or dispatching on a generic struct.
        let isVt = userValueTypes.Contains key
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, isVt)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClassMemberRef (key: SymbolKey) (args: FrozenType list) (which: ClassMember) : EntityHandle =
        let _, fields = genericClasses.[key]
        let parent = genericClassTypeSpec key args

        match which with
        | ClassMember.Ctor ->
            let paramTys = fields |> List.map snd
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
            match fields |> List.tryFind (fun (n, _) -> n = fieldName) with
            | Some(_, declTy) ->
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | None -> failwithf "ClrProvider: generic class '%A' has no field '%s'" key fieldName
        | ClassMember.Member(metaName, isStatic, paramTys, retTy) ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retTy),
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
        // mode (off at a construction site inside a static method → `!!i`; on inside
        // an enclosing closure's body → `!i`); the member-ref signature below speaks
        // the closure's own typars, so force closure mode on:
        // the embedded `FTTypar(Method, i)` encode to the closure class's `!i`.
        let savedMode = env.ClosureTyparMode
        env.ClosureTyparMode <- true

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

        env.ClosureTyparMode <- savedMode
        handle

    member _.GenericUnionMemberRef(key, args, which) = genericUnionMemberRef key args which
    member _.GenericRecordMemberRef(key, args, which) = genericRecordMemberRef key args which
    member _.GenericClassMemberRef(key, args, which) = genericClassMemberRef key args which
    member _.GenericClosureTypeSpec(name, args) = genericClosureTypeSpec name args
    member _.GenericClosureMemberRef(name, args, which) = genericClosureMemberRef name args which

    /// A generic union's own instantiation `TypeSpec` over its declaring typars (`List`1<!0>`) — the
    /// `isinst` target / `other`-local / typed-`Equals` self for its synthesised equality triple.
    /// Keyed by the union's nominal `SymbolKey` (which embeds the arity, disambiguating
    /// same-named overloads `Choice\`2`…`Choice\`7`) in `genericUnions`.
    member _.GenericUnionSelfSpec(key: SymbolKey) : EntityHandle =
        let typars, _ = genericUnions.[key]
        genericUnionTypeSpec key [ for i in 0 .. List.length typars - 1 -> FTTypar(TyparAxis.Declaring, i) ]

    member _.GenericRecordSelfSpec(key: SymbolKey) : EntityHandle =
        let typars, _ = genericRecords.[key]
        genericRecordTypeSpec key [ for i in 0 .. List.length typars - 1 -> FTTypar(TyparAxis.Declaring, i) ]
