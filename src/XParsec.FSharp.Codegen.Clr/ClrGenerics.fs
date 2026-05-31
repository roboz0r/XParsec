namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// `TypeSpec` + `MemberRef` minting for generic user types emitted into *this* assembly — unions,
/// records, classes (all sharing `encodeUnionType`'s `!0`-marker convention) and closures (whose
/// typars are the enclosing static method's roots, encoded `!i` via the ambient `closureTyparLeaf`).
type internal ClrGenerics(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let userTypes = env.UserTypes
    let genericUnions = env.GenericUnions
    let genericRecords = env.GenericRecords
    let genericClasses = env.GenericClasses
    let genericClosures = env.GenericClosures
    let typarIx typars = env.TyparIx typars
    let encodeType te t = enc.EncodeType(te, t)
    let encodeUnionType typeIx te t = enc.EncodeUnionType(typeIx, te, t)

    // Unions are keyed by arity (`Choice\`2`), so the use-site arg count selects the
    // right same-named overload in `genericUnions` / `userTypes`.
    let genericUnionTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let key = TypeRegistry.keyFor name (List.length args)
        let typars, _ = genericUnions.[key]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericUnionMemberRef (name: string) (args: SemType list) (which: UnionMember) : EntityHandle =
        let typars, cases = genericUnions.[TypeRegistry.keyFor name (List.length args)]
        let typeIx = typarIx typars
        let parent = genericUnionTypeSpec name args

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
            encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
            toEntity (ctx.MemberRef(parent, metaName, s))
        | UnionMember.Factory caseName ->
            let paramTys = caseFields caseName |> List.map snd
            let retTy = TyUnion(name, EqArray.ofSeq (seq { for t in typars -> TyConst t }))
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, caseName, s))
        | UnionMember.Member(metaName, isStatic, paramTys, retTy) ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, metaName, s))

    let genericRecordTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let typars, _ = genericRecords.[name]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[name], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericRecordMemberRef (name: string) (args: SemType list) (which: RecordMember) : EntityHandle =
        let typars, fields = genericRecords.[name]
        let typeIx = typarIx typars
        let parent = genericRecordTypeSpec name args

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
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | RecordMember.Field fieldName ->
            match fields |> List.tryFind (fun (n, _) -> n = fieldName) with
            | Some(_, declTy) ->
                let s = BlobBuilder()
                encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | None -> failwithf "ClrProvider: generic record '%s' has no field '%s'" name fieldName

    let genericClassTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let typars, _ = genericClasses.[name]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[name], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClassMemberRef (name: string) (args: SemType list) (which: ClassMember) : EntityHandle =
        let typars, fields = genericClasses.[name]
        let typeIx = typarIx typars
        let parent = genericClassTypeSpec name args

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
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, ".ctor", s))
        | ClassMember.Field fieldName ->
            match fields |> List.tryFind (fun (n, _) -> n = fieldName) with
            | Some(_, declTy) ->
                let s = BlobBuilder()
                encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
                toEntity (ctx.MemberRef(parent, fieldName, s))
            | None -> failwithf "ClrProvider: generic class '%s' has no field '%s'" name fieldName
        | ClassMember.Member(metaName, isStatic, paramTys, retTy) ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    (fun (ret: ReturnTypeEncoder) -> encodeUnionType typeIx (ret.Type()) retTy),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            encodeUnionType typeIx (pars.AddParameter().Type()) p
                    )
                )

            toEntity (ctx.MemberRef(parent, metaName, s))

    let genericClosureTypeSpec (name: string) (args: SemType list) : EntityHandle =
        let shape = genericClosures.[name]
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()

        let g =
            te.GenericInstantiation(shape.DefHandle, List.length shape.TyparRoots, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClosureMemberRef (name: string) (args: SemType list) (which: ClosureMember) : EntityHandle =
        let shape = genericClosures.[name]
        let parent = genericClosureTypeSpec name args

        // The `parent` TypeSpec above was minted under the caller's ambient (methodTyparRoots at a
        // construction site, or another closureTyparRoots for an inner-closure self-construction); the
        // member-ref signature below speaks the closure's own typars.
        let savedClosure = env.ClosureTyparRoots
        let savedMethod = env.MethodTyparRoots
        env.ClosureTyparRoots <- shape.TyparRoots
        env.MethodTyparRoots <- []

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

        env.ClosureTyparRoots <- savedClosure
        env.MethodTyparRoots <- savedMethod
        handle

    member _.GenericUnionMemberRef(name, args, which) = genericUnionMemberRef name args which
    member _.GenericRecordMemberRef(name, args, which) = genericRecordMemberRef name args which
    member _.GenericClassMemberRef(name, args, which) = genericClassMemberRef name args which
    member _.GenericClosureTypeSpec(name, args) = genericClosureTypeSpec name args
    member _.GenericClosureMemberRef(name, args, which) = genericClosureMemberRef name args which

    /// A generic union's own instantiation `TypeSpec` over its declaring typars (`List`1<!0>`) — the
    /// `isinst` target / `other`-local / typed-`Equals` self for its synthesised equality triple.
    /// `arity` disambiguates same-named overloads (`Choice\`2`…`Choice\`7`) in `genericUnions`.
    member _.GenericUnionSelfSpec(name: string, arity: int) : EntityHandle =
        let typars, _ = genericUnions.[TypeRegistry.keyFor name arity]
        genericUnionTypeSpec name [ for t in typars -> TyConst t ]

    member _.GenericRecordSelfSpec(name: string) : EntityHandle =
        let typars, _ = genericRecords.[name]
        genericRecordTypeSpec name [ for t in typars -> TyConst t ]

    /// A capture-field signature for a *generic* closure, encoded in the closure's own typars (`!i`).
    /// `closureTypars` are installed for the duration of one `encodeType` call so a free `TyVar` whose
    /// root is one of them resolves to that closure type's `GenericTypeParameter`.
    member _.GenericCaptureFieldSignature(closureTypars: TypeVar list, ty: SemType) : BlobBuilder =
        let savedClosure = env.ClosureTyparRoots
        let savedMethod = env.MethodTyparRoots
        env.ClosureTyparRoots <- closureTypars |> List.map UnionFind.find
        env.MethodTyparRoots <- []
        let blob = BlobBuilder()
        encodeType (BlobEncoder(blob).FieldSignature()) ty
        env.ClosureTyparRoots <- savedClosure
        env.MethodTyparRoots <- savedMethod
        blob
