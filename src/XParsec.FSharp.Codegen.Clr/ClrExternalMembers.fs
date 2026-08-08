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

    let externalTypeSpec (key: SymbolKey) tref instArgs =
        enc.ExternalTypeSpec(tref, env.ExternalIsValueType key, instArgs)

    let typeSpecOf ty = enc.TypeSpecOf ty

    let externalMemberCache = Dictionary<ExternalMemberCacheKey, EntityHandle>()

    /// The .NET-tupled `Parameters` slot opened back to one `FrozenType` per declared
    /// parameter. A length mismatch is a malformed provider entry, not a user-writable call.
    let openParams (what: string) (argSigLen: int) (paramsT: FrozenType) : FrozenType list =
        let asTuple t =
            match t with
            | FTTuple elems -> ValueSome(EqArray.toList elems)
            | _ -> ValueNone

        match SymbolKeyOps.openTupledArg asTuple argSigLen paramsT with
        | ValueSome ps -> ps
        | ValueNone ->
            failwithf "ClrProvider: %s declares %d parameters but its signature slot is %A" what argSigLen paramsT

    /// The member-ref signature blob from the member's open template: `paramsT` the .NET-tupled
    /// argument slot, `retT` the return, both over `FTTypar(Declaring, i)` / `FTTypar(Method, j)`
    /// markers. `methodTyparArity > 0` sets the `GENERIC` calling-convention header count.
    let mintMemberRef
        (parent: EntityHandle)
        (methodTyparArity: int)
        (paramsT: FrozenType)
        (retT: FrozenType)
        (isProperty: bool)
        (isStatic: bool)
        (argSigLen: int)
        (memberName: string)
        : EntityHandle =
        let metaName = if isProperty then "get_" + memberName else memberName
        let s = BlobBuilder()

        if isProperty then
            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = not isStatic)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) retT),
                    (fun (_: ParametersEncoder) -> ())
                )
        else
            let paramTys =
                openParams (sprintf "external member '%s'" memberName) argSigLen paramsT

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = methodTyparArity, isInstanceMethod = not isStatic)
                .Parameters(
                    List.length paramTys,
                    // A `System.Void` return surfaces as `FTUnit`, but encoding it as
                    // `FSharp.Core.Unit` mints a signature no external void method binds
                    // (`MissingMethodException`) — emit `void` (`IDisposable.Dispose`).
                    (fun (ret: ReturnTypeEncoder) ->
                        match retT with
                        | FTUnit -> ret.Void()
                        // A by-ref return (`Span<T>.get_Item : T&`) rides the return
                        // encoder's `isByRef` flag; byref is not a standalone
                        // `SignatureTypeEncoder` shape.
                        | FTByref elem -> encodeType (ret.Type(true)) elem
                        | _ -> encodeType (ret.Type()) retT
                    ),
                    (fun (pars: ParametersEncoder) ->
                        for p in paramTys do
                            match p with
                            // A by-ref / `out` param (`Int32.TryParse(string, int&)`) rides the
                            // parameter encoder's `isByRef` flag; the caller pushes the argument's
                            // address (`ldloca`).
                            | FTByref elem -> encodeType (pars.AddParameter().Type(true)) elem
                            | _ -> encodeType (pars.AddParameter().Type()) p
                    )
                )

        toEntity (ctx.MemberRef(parent, metaName, s))

    let lookupChosen (declFullName: string) (memberName: string) (key: MemberKey) : ExternalMember =
        match symbols.TryLookupMemberByKey key with
        | ValueSome m -> m
        | ValueNone -> failwithf "ClrProvider: external member '%s.%s' did not resolve at emit" declFullName memberName

    /// The member's open signature as one template: the bare value type for a property, else
    /// the .NET-tupled `FTFun(params, ret)` — the form matched against the use-site type.
    let openTemplate (chosen: ExternalMember) (isProperty: bool) : FrozenType =
        let s = chosen.Signature

        if isProperty then
            s.Return
        else
            FTFun(s.Parameters, s.Return)

    /// Mint the `MemberRef` for a `TExpr.ExternalMember`. Both typar axes' use-site
    /// instantiations are recovered by matching the open signature against `memberTy`: the
    /// declaring args parameterise the parent `TypeSpec`, the method args the `MethodSpec`.
    let externalMemberRef (key: SymbolKey) (isProperty: bool) (isStatic: bool) (memberTy: FrozenType) : EntityHandle =
        let mk = SymbolKeyOps.asMemberKey "ClrProvider: external member ref" key
        let declKey, memberName, argSig = mk.Decl, mk.Name, mk.ArgSig

        let name = SymbolKeyOps.typeNestedName declKey

        let memoKey = ExternalMemberCacheKey.Ref(key, isProperty, isStatic, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let declTyparArity = arityOfMetaName name

            let chosen = lookupChosen declFullName memberName mk
            let methodTyparArity = chosen.MethodTyparArity
            let sig_ = chosen.Signature

            let declArgs, methodArgs =
                recoverOpenTypars declTyparArity methodTyparArity (openTemplate chosen isProperty) memberTy

            // A cross-file member whose declaring type is emitted INTO this assembly parents on
            // that type's local `TypeDef`; reaching `externalClassRef` here would instead emit
            // an `AssemblyRef`-scoped `TypeRef` back to our own assembly.
            let tref =
                match env.UserTypes.TryGetValue declKey with
                | true, localHandle -> localHandle
                | _ ->
                    match externalClassRef (SymbolKey.Type declKey) with
                    | ValueSome t -> t
                    | ValueNone ->
                        failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

            let parent = externalTypeSpec (SymbolKey.Type declKey) tref (declArgs)

            let handle =
                methodSpec
                    (mintMemberRef
                        parent
                        methodTyparArity
                        sig_.Parameters
                        sig_.Return
                        isProperty
                        isStatic
                        argSig.Length
                        memberName)
                    methodArgs

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
        let declKey, memberName, argSig = mk.Decl, mk.Name, mk.ArgSig

        let memoKey = ExternalMemberCacheKey.On(key, declTy, isProperty, isStatic, memberTy)

        match externalMemberCache.TryGetValue memoKey with
        | true, h -> h
        | _ ->
            let declFullName = SymbolKeyOps.typeMetaName declKey

            let chosen = lookupChosen declFullName memberName mk
            let methodTyparArity = chosen.MethodTyparArity
            let sig_ = chosen.Signature

            // Only the method axis is recovered here; declaring arity 0 leaves the template's
            // `FTTypar(Declaring, i)` slots to encode as `!i` when the blob is minted.
            let _, methodArgs =
                recoverOpenTypars 0 methodTyparArity (openTemplate chosen isProperty) memberTy

            let parent = typeSpecOf declTy

            let handle =
                methodSpec
                    (mintMemberRef
                        parent
                        methodTyparArity
                        sig_.Parameters
                        sig_.Return
                        isProperty
                        isStatic
                        argSig.Length
                        memberName)
                    methodArgs

            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint a field `MemberRef` for an external public field (`String.Empty`) — read via
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
                        match externalClassRef (SymbolKey.Type declKey) with
                        | ValueSome t -> t
                        | ValueNone ->
                            failwithf "ClrProvider: external declaring type '%s' did not resolve at emit" declFullName

                    externalTypeSpec (SymbolKey.Type declKey) tref declArgs

            let s = BlobBuilder()
            encodeType (BlobEncoder(s).FieldSignature()) openFieldTy

            let handle = toEntity (ctx.MemberRef(parent, fieldName, s))
            externalMemberCache.[memoKey] <- handle
            handle

    /// Mint the `MemberRef` for a referenced-assembly record's `.ctor`, instantiated at `args`.
    /// Parameter types are the declared fields in their *open* typar form.
    let externalRecordCtor (key: SymbolKey) (args: FrozenType list) : EntityHandle voption =
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

    /// Mint the `MemberRef` for a referenced-assembly union's case factory — the static
    /// `<caseName>(fields…) : Union<…>` the union emitter writes, over open markers so the
    /// signature matches. Returns handle + field count; `ValueNone` ⇒ unknown union or case.
    let externalUnionFactory
        (key: SymbolKey)
        (caseName: string)
        (args: FrozenType list)
        : (EntityHandle * int) voption =
        let fullName = SymbolKeyOps.qualifiedName key
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | None -> ValueNone
            | Some case ->
                let parent = externalTypeSpec key tref args

                let paramTys = List.ofArray case.FrozenFieldTypes

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
    /// `caseName`'s discriminator — its zero-based index in declaration order, as the union
    /// emitter assigns them. `ValueNone` ⇒ unknown union or case.
    let externalUnionTag (key: SymbolKey) (args: FrozenType list) (caseName: string) : (EntityHandle * int) voption =
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFindIndex (fun c -> c.Name = caseName) with
            | None -> ValueNone
            | Some tag ->
                let parent = externalTypeSpec key tref args
                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) (FTConst(RuntimeNames.intKey, EqArray.empty))
                ValueSome(toEntity (ctx.MemberRef(parent, "_tag", s)), tag)

    /// Mint the `MemberRef` for the `<caseName>_<i>` public field of one case on a
    /// referenced-package union at `args` — the slot a cross-package `match … Some x` reads —
    /// with that field's type after the use-site substitution. `ValueNone` ⇒ unknown case/index.
    let externalUnionCaseField
        (key: SymbolKey)
        (args: FrozenType list)
        (caseName: string)
        (fieldIndex: int)
        : (EntityHandle * FrozenType) voption =
        let arity = List.length args

        match externalUnionRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, cases) ->
            match cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some case when fieldIndex >= 0 && fieldIndex < case.FrozenFieldTypes.Length ->
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
        (key: SymbolKey)
        (chosen: SymbolKey voption)
        (tyArgs: FrozenType list)
        (argTypes: FrozenType list)
        : CtorRecipe voption =
        match symbols.TryLookupCtor(key, chosen, List.length argTypes) with
        | ValueNone -> ValueNone
        | ValueSome chosenCtor ->
            match externalClassRef key with
            | ValueNone -> ValueNone
            | ValueSome tref ->
                let parent = externalTypeSpec key tref tyArgs

                let paramTys =
                    openParams
                        (sprintf "ctor of '%s'" (SymbolKeyOps.typeMetaName chosenCtor.Key.Decl))
                        chosenCtor.Key.ArgSig.Length
                        chosenCtor.Signature.Parameters

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

    /// Mint the `MemberRef` for one named field on a referenced-assembly record at `args`, with
    /// its declared type after the use-site substitution — what a `FieldGet` encodes next.
    let externalRecordField
        (key: SymbolKey)
        (args: FrozenType list)
        (fieldName: string)
        : (EntityHandle * FrozenType) voption =
        let arity = List.length args

        match externalRecordRef key arity with
        | ValueNone -> ValueNone
        | ValueSome(tref, fields) ->
            match fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | None -> ValueNone
            | Some field ->
                let parent = externalTypeSpec key tref args

                let openFieldTy = field.Frozen

                let s = BlobBuilder()
                encodeType (BlobEncoder(s).FieldSignature()) openFieldTy
                let handle = toEntity (ctx.MemberRef(parent, fieldName, s))

                let substitutedTy = substituteDeclaring (List.toArray args) openFieldTy
                ValueSome(handle, substitutedTy)

    /// Mint the parameterless `.ctor()` `MemberRef` of a heritable external base class
    /// (`System.Attribute`), directly off the `TypeRef`: a base ctor is often `protected` and
    /// not surfaced, yet `call`ing it from a subclass ctor is legal.
    let externalParameterlessBaseCtor (key: SymbolKey) : EntityHandle voption =
        match externalClassRef key with
        | ValueNone -> ValueNone
        | ValueSome tref ->
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            ValueSome(toEntity (ctx.MemberRef(tref, ".ctor", s)))

    member _.ExternalParameterlessBaseCtor(key) = externalParameterlessBaseCtor key

    /// An intrinsic-CLASS `inherit` parent (`exn`) → its platform key (`System.Exception`)
    /// plus the raw `TypeRef` the derived type's `extends` names. Only a `(# class "…" #)`
    /// primitive carries a class surface, so a value-repr intrinsic (`int`) never matches.
    member _.IntrinsicClassBase(canon: SymbolKey) : struct (SymbolKey * EntityHandle) voption =
        match env.LookupTypeByKey canon with
        | ValueSome(ExternalTypeShape.Intrinsic {
                                                    Id = {
                                                             Platform = IntrinsicPlatform.Repr repr
                                                         }
                                                    Class = ValueSome _
                                                }) ->
            let platformKey = SymbolKeyOps.qualifiedTypeKey repr 0

            match externalClassRef platformKey with
            | ValueSome tref -> ValueSome(struct (platformKey, tref))
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// The raw external `TypeRef` for `key` — the Extends column of a derived type wants the
    /// bare ref for a non-generic external base, not a `TypeSpec`. `ValueNone` ⇒ not a class.
    member _.ExternalClassTypeRef(key) = externalClassRef key

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

    member _.ExternalRecordField(key, args, fieldName) = externalRecordField key args fieldName

    /// `MethodSpec` instantiating a generic static method: `fold<int,int>` at a call site,
    /// `fold<!!0,!!1>` for a recursive self-call.
    member _.StaticFnMethodSpec(handle, instTypes) = methodSpec handle instTypes
