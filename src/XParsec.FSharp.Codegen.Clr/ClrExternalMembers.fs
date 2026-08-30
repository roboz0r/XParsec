namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// The instantiation is part of the identity: `IEnumerable<!!0>.GetEnumerator` and
/// `IEnumerable<!!1>.GetEnumerator` must mint distinct `MemberRef`s.
[<RequireQualifiedAccess>]
type internal ExternalMemberCacheKey =
    | Ref of key: SymbolKey * isProperty: bool * isStatic: bool * memberTy: FrozenType
    | On of key: SymbolKey * declTy: FrozenType * isProperty: bool * isStatic: bool * memberTy: FrozenType
    | Field of key: SymbolKey * declTy: FrozenType voption * memberTy: FrozenType

/// One external member's CLR calling shape, over `FTTypar(Declaring, i)` /
/// `FTTypar(Method, j)` markers.
type internal OpenMemberSignature =
    {
        TupledParameters: FrozenType
        ParameterCount: int
        Return: FrozenType
        /// A property is minted as its `get_<name>` accessor, which takes no parameters.
        IsProperty: bool
        IsStatic: bool
        MethodTyparArity: int
    }

    static member OfMember(m: ExternalMember) : OpenMemberSignature =
        {
            TupledParameters = ExternalSignature.tupledParameters m.Signature
            ParameterCount = m.Key.ArgSig.Length
            Return = m.Signature.Return
            IsProperty = (m.Storage = MemberStorage.Property)
            IsStatic = m.IsStatic
            MethodTyparArity = m.Signature.MethodTyparArity
        }

/// A resolved external symbol's `SymbolKey` (+ instantiation) → an
/// `AssemblyRef`/`TypeRef`/`TypeSpec`/`MemberRef` against a referenced-assembly type.
type internal ClrExternalMembers(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let symbols = env.Symbols
    let arityOfMetaName n = env.ArityOfMetaName n
    let externalClassRef key = env.ExternalClassRef key
    let externalRecordRef key arity = env.ExternalRecordRef(key, arity)
    let externalUnionRef key arity = env.ExternalUnionRef(key, arity)
    let encodeType te t = enc.EncodeType(te, t)
    let methodSpec handle args = enc.MethodSpec(handle, args)

    let recoverOpenTypars declTyparArity methodTyparArity (openT: FrozenType) (instT: FrozenType) =
        enc.RecoverOpenTypars(declTyparArity, methodTyparArity, openT, instT)

    let externalTypeSpec (key: TypeKey) tref instArgs =
        enc.ExternalTypeSpec(tref, env.ExternalIsValueType key, instArgs)

    let typeSpecOf ty = enc.TypeSpecOf ty

    let externalMemberCache = Dictionary<ExternalMemberCacheKey, EntityHandle>()

    /// The .NET-tupled argument slot opened back to one `FrozenType` per declared parameter.
    /// A length mismatch is a malformed provider entry, not a user-writable call.
    let openParams (what: string) (argSigLen: int) (paramsT: FrozenType) : FrozenType list =
        match argSigLen, paramsT with
        | 0, _ -> []
        | 1, _ -> [ paramsT ]
        | n, FTTuple elems when elems.Length = n -> EqArray.toList elems
        | _ -> failwithf "ClrProvider: %s declares %d parameters but its signature slot is %A" what argSigLen paramsT

    /// Mint the `MemberRef` for `sig_` on `parent`, taking the getter shape for a property.
    let mintMemberRef (parent: EntityHandle) (sig_: OpenMemberSignature) (memberName: string) : EntityHandle =
        let retT = sig_.Return
        let isStatic = sig_.IsStatic

        let metaName = if sig_.IsProperty then "get_" + memberName else memberName

        let s = BlobBuilder()

        if sig_.IsProperty then
            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retT),
                    (fun (_: ParametersEncoder) -> ())
                )
        else
            let paramTys =
                openParams (sprintf "external member '%s'" memberName) sig_.ParameterCount sig_.TupledParameters

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = sig_.MethodTyparArity, isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    // A `System.Void` return surfaces as `FTUnit`, but encoding it as
                    // `FSharp.Core.Unit` mints a signature no external void method binds
                    // (`MissingMethodException`), so emit `void` (`IDisposable.Dispose`).
                    (fun (ret: ReturnTypeEncoder) ->
                        match retT with
                        | FTUnit -> ret.Void()
                        // A by-ref return (`Span<T>.get_Item : T&`) is carried on the return
                        // encoder's `isByRef` flag; byref is not a standalone
                        // `SignatureTypeEncoder` shape.
                        | FTByref elem -> encodeType (ret.Type(true)) elem
                        | _ -> encodeType (ret.Type()) retT
                    ),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            match p with
                            // A by-ref / `out` param (`Int32.TryParse(string, int&)`) is carried on
                            // the parameter encoder's `isByRef` flag; the caller pushes the
                            // argument's address (`ldloca`).
                            | FTByref elem -> encodeType (pars.AddParameter().Type(true)) elem
                            | _ -> encodeType (pars.AddParameter().Type()) p
                    )
                )

        toEntity (ctx.MemberRef(parent, metaName, s))

    let lookupChosen (declFullName: string) (memberName: string) (key: MemberKey) : ExternalMember =
        match symbols.TryLookupMemberByKey key with
        | ValueSome m -> m
        | ValueNone -> failwithf "ClrProvider: external member '%s.%s' did not resolve at emit" declFullName memberName

    /// Mint the `MemberRef` for a `TExpr.ExternalMember`. Both typar axes' use-site
    /// instantiations are recovered by matching the open signature against `memberTy`: the
    /// declaring args parameterise the parent `TypeSpec`, the method args the `MethodSpec`.
    let externalMemberRef (key: SymbolKey) (isProperty: bool) (isStatic: bool) (memberTy: FrozenType) : EntityHandle =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external member ref" key
        let declKey, memberName = mk.Decl, mk.Name

        let name = SymbolKeyOps.typeNestedName declKey

        let memoKey = ExternalMemberCacheKey.Ref(key, isProperty, isStatic, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let declTyparArity = arityOfMetaName name

            let chosen = lookupChosen declFullName memberName mk
            let sig_ = OpenMemberSignature.OfMember chosen

            let declArgs, methodArgs =
                recoverOpenTypars
                    declTyparArity
                    sig_.MethodTyparArity
                    (ExternalSignature.openTemplate chosen.Signature)
                    memberTy

            let tref =
                match env.ClassOrigin declKey with
                | ClassOrigin.Local t
                | ClassOrigin.Foreign t -> t
                | ClassOrigin.Unresolved ->
                    failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

            let parent = externalTypeSpec declKey tref (declArgs)

            let handle = methodSpec (mintMemberRef parent sig_ memberName) methodArgs

            externalMemberCache.[memoKey] <- handle
            handle

    /// `externalMemberRef` for a T-free member (`MoveNext(): bool` on a generic enumerator) whose
    /// declaring instantiation no signature match can recover: it is read off `declTy`
    /// (`List`1+Enumerator<int>`) and encoded through `TypeSpecOf`, so a struct parent lands `VALUETYPE`.
    let externalMemberRefOn
        (key: SymbolKey)
        (declTy: FrozenType)
        (isProperty: bool)
        (isStatic: bool)
        (memberTy: FrozenType)
        : EntityHandle =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external member ref" key
        let declKey, memberName = mk.Decl, mk.Name

        let memoKey = ExternalMemberCacheKey.On(key, declTy, isProperty, isStatic, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let chosen = lookupChosen declFullName memberName mk
            let sig_ = OpenMemberSignature.OfMember chosen

            // Only the method axis is recovered here; declaring arity 0 leaves the template's
            // `FTTypar(Declaring, i)` slots to encode as `!i` when the blob is minted.
            let _, methodArgs =
                recoverOpenTypars 0 sig_.MethodTyparArity (ExternalSignature.openTemplate chosen.Signature) memberTy

            let parent = typeSpecOf declTy

            let handle = methodSpec (mintMemberRef parent sig_ memberName) methodArgs

            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint a field `MemberRef` for an external public field (`String.Empty`), read via
    /// `ldfld`/`ldsfld`, not a `get_X` accessor. The parent instantiation comes from `declTy`
    /// when the access has an object argument, else it is recovered from the use-site `memberTy`.
    let externalFieldRef (key: SymbolKey) (declTy: FrozenType voption) (memberTy: FrozenType) : EntityHandle =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external field ref" key
        let declKey, fieldName = mk.Decl, mk.Name

        let name = SymbolKeyOps.typeNestedName declKey

        let memoKey = ExternalMemberCacheKey.Field(key, declTy, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let chosen = lookupChosen declFullName fieldName mk
            let openFieldTy = chosen.Signature.Return

            let parent =
                match declTy with
                | ValueSome dt -> typeSpecOf dt
                | ValueNone ->
                    let declTyparArity = arityOfMetaName name
                    let declArgs, _ = recoverOpenTypars declTyparArity 0 openFieldTy memberTy

                    let tref =
                        match env.ClassOrigin declKey with
                        | ClassOrigin.Local t
                        | ClassOrigin.Foreign t -> t
                        | ClassOrigin.Unresolved ->
                            failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

                    externalTypeSpec declKey tref declArgs

            let s = BlobBuilder()
            encodeType (BlobEncoder(s).FieldSignature()) openFieldTy

            let handle = toEntity (ctx.MemberRef(parent, fieldName, s))
            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint the `MemberRef` for a referenced-assembly record's `.ctor`, instantiated at `args`.
    /// Parameter types are the declared fields in their *open* typar form.
    let externalRecordCtor (key: TypeKey) (args: FrozenType list) : EntityHandle voption =
        let arity = List.length args

        match externalRecordRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            let parent = externalTypeSpec key tref args

            let paramTys = [ for f in fields -> f.Frozen ]

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

            ValueSome(toEntity (ctx.MemberRef(parent, ".ctor", s)))

    /// Mint the `MemberRef` for a referenced-assembly union's case factory, the static
    /// `<caseName>(fields…) : Union<…>` the union emitter writes, over open markers so the
    /// signature matches. Returns handle + field count; `ValueNone` ⇒ unknown union or case.
    let externalUnionFactory (key: TypeKey) (caseName: string) (args: FrozenType list) : (EntityHandle * int) voption =
        let fullName = SymbolKeyOps.typeMetaName key
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> EqArray.tryFind (fun c -> c.Name = caseName) with
            | ValueNone -> ValueNone
            | ValueSome case ->
                let parent = externalTypeSpec key tref args

                let paramTys = EqArray.toList case.FrozenFieldTypes

                let retTy =
                    FTUnion(
                        SymbolKeyOps.qualifiedTypeKeyOf fullName arity,
                        EqArray.ofArray [| for i in 0 .. arity - 1 -> FTTypar(TyparAxis.Declaring, i) |]
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

                ValueSome(toEntity (ctx.MemberRef(parent, caseName, s)), List.length paramTys)

    /// Mint the `_tag : int` field `MemberRef` on a referenced-package union at `args`, with
    /// `caseName`'s discriminator, its zero-based index in declaration order, as the union
    /// emitter assigns them. `ValueNone` ⇒ unknown union or case.
    let externalUnionTag (key: TypeKey) (args: FrozenType list) (caseName: string) : (EntityHandle * int) voption =
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> EqArray.tryFindIndex (fun c -> c.Name = caseName) with
            | ValueNone -> ValueNone
            | ValueSome tag ->
                let parent = externalTypeSpec key tref args
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) (FTConst(RuntimeNames.intKey, EqArray.empty))
                ValueSome(toEntity (ctx.MemberRef(parent, "_tag", s)), tag)

    /// Mint the `MemberRef` for the `<caseName>_<i>` public field of one case on a
    /// referenced-package union at `args` (the slot a cross-package `match … Some x` reads),
    /// with that field's type after the use-site substitution. `ValueNone` ⇒ unknown case/index.
    let externalUnionCaseField
        (key: TypeKey)
        (args: FrozenType list)
        (caseName: string)
        (fieldIndex: int)
        : (EntityHandle * FrozenType) voption =
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> EqArray.tryFind (fun c -> c.Name = caseName) with
            | ValueSome case when fieldIndex >= 0 && fieldIndex < case.FrozenFieldTypes.Length ->
                let parent = externalTypeSpec key tref args

                let openFieldTy = case.FrozenFieldTypes.[fieldIndex]

                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) openFieldTy

                let handle =
                    toEntity (ctx.MemberRef(parent, sprintf "%s_%d" caseName fieldIndex, s))

                let substitutedTy = substituteDeclaring (List.toArray args) openFieldTy
                ValueSome(handle, substitutedTy)
            | _ -> ValueNone

    /// Mint the `MemberRef` for a referenced-assembly class's ctor, instantiated at `tyArgs`. The
    /// overload is the `MemberKey` the front end recorded on `TExpr.New`, so
    /// `ArgumentException(string, string)` and `(string, Exception)` are told apart.
    let externalCtor
        (key: TypeKey)
        (chosen: SymbolKey voption)
        (tyArgs: FrozenType list)
        (argTypes: FrozenType list)
        : CtorRecipe voption =
        match symbols.TryLookupCtor(key, chosen, List.length argTypes) with
        | ValueNone -> ValueNone
        | ValueSome chosenCtor ->
            // FOREIGN only: a class this compilation emits reaches its ctor through the emitted
            // `classes` registry instead.
            match env.ClassOrigin key with
            | ClassOrigin.Local _
            | ClassOrigin.Unresolved -> ValueNone
            | ClassOrigin.Foreign tref ->
                let parent = externalTypeSpec key tref tyArgs

                let paramTys =
                    openParams
                        (sprintf "ctor of '%s'" (SymbolKeyOps.typeMetaName chosenCtor.Key.Decl))
                        chosenCtor.Key.ArgSig.Length
                        (ExternalSignature.tupledParameters chosenCtor.Signature)

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

                let handle = toEntity (ctx.MemberRef(parent, ".ctor", s))

                ValueSome
                    {
                        Handle = handle
                        ArgCount = List.length paramTys
                    }

    /// Mint the `MemberRef` for a referenced-assembly attribute class's `.ctor`, chosen by
    /// positional-argument count. An attribute class is non-generic, so the parent is the bare
    /// `TypeRef`. `ValueNone` ⇒ the type or an arity-matching ctor did not resolve; the caller
    /// emits no `CustomAttribute` row.
    let externalAttributeCtor (key: TypeKey) (argCount: int) : EntityHandle voption =
        match symbols.TryLookupCtor(key, ValueNone, argCount) with
        | ValueNone -> ValueNone
        | ValueSome chosenCtor ->
            match env.ClassOrigin key with
            | ClassOrigin.Local _
            | ClassOrigin.Unresolved -> ValueNone
            | ClassOrigin.Foreign tref ->
                let paramTys =
                    openParams
                        (sprintf "attribute ctor of '%s'" (SymbolKeyOps.typeMetaName key))
                        chosenCtor.Key.ArgSig.Length
                        (ExternalSignature.tupledParameters chosenCtor.Signature)

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

                ValueSome(toEntity (ctx.MemberRef(tref, ".ctor", s)))

    /// Mint the `MemberRef` for one named field on a referenced-assembly record at `args`, with
    /// its declared type after the use-site substitution, which a `FieldGet` encodes next.
    let externalRecordField
        (key: TypeKey)
        (args: FrozenType list)
        (fieldName: string)
        : (EntityHandle * FrozenType) voption =
        let arity = List.length args

        match externalRecordRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            match fields |> EqArray.tryFind (fun f -> f.Name = fieldName) with
            | ValueNone -> ValueNone
            | ValueSome field ->
                let parent = externalTypeSpec key tref args

                let openFieldTy = field.Frozen

                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) openFieldTy
                let handle = toEntity (ctx.MemberRef(parent, fieldName, s))

                let substitutedTy = substituteDeclaring (List.toArray args) openFieldTy
                ValueSome(handle, substitutedTy)

    /// The parameterless `.ctor()` `MemberRef` of a heritable FOREIGN base class
    /// (`System.Attribute`), minted directly off the `TypeRef`, because a base ctor is often
    /// `protected` and unsurfaced yet legal to `call` from a subclass ctor.
    let externalParameterlessBaseCtor (key: TypeKey) : EntityHandle voption =
        match env.ClassOrigin key with
        | ClassOrigin.Local _
        | ClassOrigin.Unresolved -> ValueNone
        | ClassOrigin.Foreign tref ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            ValueSome(toEntity (ctx.MemberRef(tref, ".ctor", s)))

    member _.ExternalParameterlessBaseCtor(key) = externalParameterlessBaseCtor key

    /// An intrinsic-CLASS `inherit` parent (`exn`) → its platform key (`System.Exception`)
    /// plus the raw `TypeRef` the derived type's `extends` points to. Only a `(# class "…" #)`
    /// primitive is heritable, so a value-repr intrinsic (`int`) never matches, even though
    /// it carries a surface of its own once it declares an `interface`.
    member _.IntrinsicClassBase(canon: TypeKey) : struct (TypeKey * EntityHandle) voption =
        match env.LookupTypeByKey canon |> ValueOption.bind ExternalSymbols.intrinsicClassOf with
        | ValueSome(struct ({
                                Platform = IntrinsicPlatform.Bound typeId
                            }, _)) ->
            let platformKey = SymbolKeyOps.qualifiedTypeKeyOf typeId.Value 0

            match externalClassRef platformKey with
            | ValueSome tref -> ValueSome(struct (platformKey, tref))
            | ValueNone -> ValueNone
        | _ -> ValueNone

    member _.ClassOrigin(key) = env.ClassOrigin key

    member _.ExternalMemberReturnsVoid(key: SymbolKey) : bool =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external member return" key

        match (lookupChosen (SymbolKeyOps.typeMetaName mk.Decl) mk.Name mk).Signature.Return with
        | FTUnit -> true
        | _ -> false

    member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
        externalMemberRef key isProperty isStatic memberTy

    member _.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy) =
        externalMemberRefOn key declTy isProperty isStatic memberTy

    member _.ExternalFieldRef(key, declTy, memberTy) = externalFieldRef key declTy memberTy

    member _.ExternalRecordCtor(key, args) = externalRecordCtor key args

    member _.ExternalUnionFactory(key, caseName, args) = externalUnionFactory key caseName args

    member _.ExternalUnionTag(key, args, caseName) = externalUnionTag key args caseName

    member _.ExternalUnionCaseField(key, args, caseName, fieldIndex) =
        externalUnionCaseField key args caseName fieldIndex

    member _.ExternalCtor(key, chosen, tyArgs, argTypes) = externalCtor key chosen tyArgs argTypes

    member _.ExternalAttributeCtor(key, argCount) = externalAttributeCtor key argCount

    member _.ExternalRecordField(key, args, fieldName) = externalRecordField key args fieldName

    /// `MethodSpec` instantiating a generic static method: `fold<int,int>` at a call site,
    /// `fold<!!0,!!1>` for a recursive self-call.
    member _.StaticFnMethodSpec(handle, instTypes) = methodSpec handle instTypes
