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

    // Unions are keyed by their nominal `SymbolKey` (which embeds the arity, so
    // same-named overloads `Choice\`2`…`Choice\`7` don't collide) in `genericUnions`
    // / `userTypes`.
    let genericUnionTypeSpec (key: SymbolKey) (args: SemType list) : EntityHandle =
        let typars, _ = genericUnions.[key]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericUnionMemberRef (key: SymbolKey) (args: SemType list) (which: UnionMember) : EntityHandle =
        let typars, cases = genericUnions.[key]
        let typeIx = typarIx typars
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
            encodeUnionType typeIx (BlobEncoder(s).FieldSignature()) declTy
            toEntity (ctx.MemberRef(parent, metaName, s))
        | UnionMember.Factory caseName ->
            let paramTys = caseFields caseName |> List.map snd

            let retTy =
                TyUnion(key, EqArray.ofSeq (seq { for t in typars -> TyConst(t, EqArray.empty) }))

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

    let genericRecordTypeSpec (key: SymbolKey) (args: SemType list) : EntityHandle =
        let typars, _ = genericRecords.[key]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericRecordMemberRef (key: SymbolKey) (args: SemType list) (which: RecordMember) : EntityHandle =
        let typars, fields = genericRecords.[key]
        let typeIx = typarIx typars
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
            | None -> failwithf "ClrProvider: generic record '%A' has no field '%s'" key fieldName

    let genericClassTypeSpec (key: SymbolKey) (args: SemType list) : EntityHandle =
        let typars, _ = genericClasses.[key]
        let typeIx = typarIx typars
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(userTypes.[key], List.length typars, false)

        for a in args do
            encodeUnionType typeIx (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClassMemberRef (key: SymbolKey) (args: SemType list) (which: ClassMember) : EntityHandle =
        let typars, fields = genericClasses.[key]
        let typeIx = typarIx typars
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
            | None -> failwithf "ClrProvider: generic class '%A' has no field '%s'" key fieldName
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

        let g = te.GenericInstantiation(shape.DefHandle, shape.TyparCount, false)

        for a in args do
            encodeType (g.AddArgument()) a

        toEntity (ctx.TypeSpec tsB)

    let genericClosureMemberRef (name: string) (args: SemType list) (which: ClosureMember) : EntityHandle =
        let shape = genericClosures.[name]
        let parent = genericClosureTypeSpec name args

        // The `parent` TypeSpec above was minted under the caller's ambient closure
        // mode (off at a construction site inside a static method → `!!i`; on inside
        // an enclosing closure's body → `!i`); the member-ref signature below speaks
        // the closure's own typars, so force closure mode on (frozen-type-plan 2B):
        // the embedded `TempTypar(Method, i)` encode to the closure class's `!i`.
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
        genericUnionTypeSpec key [ for t in typars -> TyConst(t, EqArray.empty) ]

    member _.GenericRecordSelfSpec(key: SymbolKey) : EntityHandle =
        let typars, _ = genericRecords.[key]
        genericRecordTypeSpec key [ for t in typars -> TyConst(t, EqArray.empty) ]

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
