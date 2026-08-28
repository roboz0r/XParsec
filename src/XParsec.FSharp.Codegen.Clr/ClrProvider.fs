namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// `ICodegenProvider` over the BCL + the referenced assemblies — a thin shell forwarding to the
/// collaborators constructed below. `reprs` maps an intrinsic canon key to its IL representation;
/// `references` maps an assembly's simple name to the identity read off its own file.
type ClrProvider
    (
        ctx: MetadataContext,
        reprs: System.Collections.Generic.IReadOnlyDictionary<TypeKey, string>,
        references: Map<string, System.Reflection.AssemblyName>,
        symbols: ICodegenSymbols
    ) =

    let env = ClrEnv(ctx, reprs, references, symbols)

    let enc = ClrEncoder(env)
    let generics = ClrGenerics(env, enc)
    let ext = ClrExternalMembers(env, enc)
    let recipes = ClrRecipes(env, enc)

    member _.ObjectType: EntityHandle = env.EObject.Value

    /// `System.ValueType` — the IL base type of a `[<Struct>]` value type.
    member _.ValueTypeBase: EntityHandle = env.EValueType.Value

    /// `System.Enum` — the IL base type of a numeric enum's `TypeDefinition`.
    member _.EnumBase: EntityHandle = env.EEnum.Value

    /// `System.Runtime.CompilerServices.IsByRefLikeAttribute::.ctor()` — the
    /// `CustomAttribute` constructor stamped on a `[<IsByRefLike>]` value type.
    member _.IsByRefLikeAttrCtor: EntityHandle = env.EIsByRefLikeAttrCtor.Value

    /// `System.AttributeUsageAttribute::.ctor(System.AttributeTargets)` — the CLR spelling
    /// a `[<AttributeUsage>]` row is written against.
    member _.AttributeUsageAttrCtor: EntityHandle = env.EAttributeUsageAttrCtor.Value

    /// A referenced-assembly attribute class's `.ctor` `MemberRef`, chosen by
    /// positional-argument count.
    member _.TryExternalAttributeCtor(key: TypeKey, argCount: int) : EntityHandle voption =
        ext.ExternalAttributeCtor(key, argCount)

    /// Member ref to `System.Object::.ctor()` for a union's base-ctor chain.
    member _.ObjectCtorRef: EntityHandle = env.EObjectCtor.Value

    /// Register a user type emitted into this assembly so `encodeType` can reference it (by its
    /// predicted `TypeDefinition` handle) before its row is added.
    member _.RegisterUserType(key: TypeKey, handle: EntityHandle) : unit = env.UserTypes.[key] <- handle

    /// Register a module-level function emitted into this assembly (by its `ValueKey`) so a
    /// cross-file call to it resolves to its local `MethodDef` rather than an external member ref.
    member _.RegisterLocalModuleFn(key: SymbolKey, handle: EntityHandle) : unit = env.LocalModuleFns.[key] <- handle

    /// Register a module-level value's static field emitted into this assembly (by its
    /// `ValueKey`) so a cross-file read of it loads the local `FieldDef`.
    member _.RegisterLocalModuleValue(key: SymbolKey, handle: EntityHandle) : unit =
        env.LocalModuleValues.[key] <- handle

    /// Record a project-local `[<Struct>]` value type so `encodeType` emits it as
    /// `ELEMENT_TYPE_VALUETYPE`.
    member _.RegisterUserValueType(key: TypeKey) : unit = env.UserValueTypes.Add key |> ignore

    /// Register a *generic* union's shape (typar names + cases) so member refs can be minted on its
    /// `TypeSpec`. A monomorphic union uses its `Def` tokens instead.
    member _.RegisterGenericUnion
        (key: TypeKey, typars: EqArray<string>, cases: (string * (string * FrozenType) list) list)
        : unit =
        env.GenericUnions.[key] <-
            {
                Typars = typars
                Cases =
                    cases
                    |> EqArray.ofSeq
                    |> EqArray.map (fun (name, fields) ->
                        {
                            Name = name
                            Fields = EqArray.ofList fields
                        }
                    )
            }

    member _.RegisterGenericRecord(key: TypeKey, typars: EqArray<string>, fields: (string * FrozenType) list) : unit =
        env.GenericRecords.[key] <-
            {
                Typars = typars
                Fields = EqArray.ofList fields
            }

    member _.RegisterGenericClass
        (key: TypeKey, typars: EqArray<string>, ctorParamCount: int, fields: (string * FrozenType) list)
        : unit =
        env.GenericClasses.[key] <-
            {
                Typars = typars
                CtorParamCount = ctorParamCount
                Fields = EqArray.ofList fields
            }

    member _.RecordCtorSignature(paramTys: FrozenType list) : BlobBuilder = enc.RecordCtorSignature(paramTys)

    member _.GenericMethodOnTypeSignature
        (methodTyparCount: int, paramTys: FrozenType list, retTy: FrozenType, isInstanceMethod: bool)
        : BlobBuilder =
        enc.GenericMethodOnTypeSignature(methodTyparCount, paramTys, retTy, isInstanceMethod)

    member _.NullaryCtorSignature() : BlobBuilder = enc.NullaryCtorSignature()

    member _.CctorSignature() : BlobBuilder = enc.CctorSignature()

    member _.GenericUnionSelfSpec(key: TypeKey) : EntityHandle = generics.GenericUnionSelfSpec key

    member _.GenericRecordSelfSpec(key: TypeKey) : EntityHandle = generics.GenericRecordSelfSpec key

    member _.GenericStaticFnSignature(typarCount: int, paramTys: FrozenType list, retTy: FrozenType) : BlobBuilder =
        enc.GenericStaticFnSignature(typarCount, paramTys, retTy)

    member _.StaticMethodSignature(paramTys: FrozenType list, retTy: FrozenType) : BlobBuilder =
        enc.StaticMethodSignature(paramTys, retTy)

    member _.InstanceMethodSignature(paramTys: FrozenType list, retTy: FrozenType) : BlobBuilder =
        enc.InstanceMethodSignature(paramTys, retTy)

    member _.InstanceMethodSignatureVoid(paramTys: FrozenType list) : BlobBuilder =
        enc.InstanceMethodSignatureVoid(paramTys)

    member _.StaticMethodSignatureVoid(paramTys: FrozenType list) : BlobBuilder =
        enc.StaticMethodSignatureVoid(paramTys)

    member _.GenericMethodOnTypeSignatureVoid
        (methodTyparCount: int, paramTys: FrozenType list, isInstanceMethod: bool)
        : BlobBuilder =
        enc.GenericMethodOnTypeSignatureVoid(methodTyparCount, paramTys, isInstanceMethod)

    member _.FunInterfaceSpec(a: FrozenType, b: FrozenType) : EntityHandle = recipes.FunInterfaceSpec(a, b)

    member _.FlatFunInterfaceSpecN(tys: FrozenType list) : EntityHandle = recipes.FlatFunInterfaceSpecN(tys)

    /// A `TypeSpec`/`TypeRef` handle for an arbitrary external type. A user class's
    /// `interface IEnumerable<'T>` carries its `'T` as `FTTypar(Declaring, i)`, which the
    /// encoder resolves to the declaring type's `!i`.
    member _.TypeSpecOf(ty: FrozenType) : EntityHandle = enc.TypeSpecOf ty

    /// The `InterfaceImpl.Interface` handle for a user class's implemented interface. A generic
    /// interface (`IEnumerable<int>`) needs a `TypeSpec` carrying its instantiation; a non-generic
    /// one (`IEnumerable`, `IComparable`) must reference its `TypeRef`/`TypeDef` directly.
    member _.InterfaceHandleOf(ty: FrozenType) : EntityHandle =
        match ty with
        | FTClass(key, args) when args.IsEmpty ->
            match env.ClassOrigin key with
            | ClassOrigin.Local handle
            | ClassOrigin.Foreign handle -> handle
            | ClassOrigin.Unresolved -> enc.TypeSpecOf ty
        | _ -> enc.TypeSpecOf ty

    /// The flat `instance resultTy Invoke(paramTys…)` signature of a `Fun`(N+1)` closure.
    member _.InvokeSignatureN(paramTys: FrozenType list, resultTy: FrozenType) : BlobBuilder =
        enc.InvokeSignatureN(paramTys, resultTy)

    member _.ClosureCtorSignature(captures: FrozenType list) : BlobBuilder = enc.ClosureCtorSignature captures

    member _.FieldSignature(ty: FrozenType) : BlobBuilder = enc.FieldSignature ty

    member _.ClosureSelfFieldSignature(closureTypeHandle: EntityHandle) : BlobBuilder =
        enc.ClosureSelfFieldSignature closureTypeHandle

    /// Register a *generic* closure's shape so its member refs can be minted on its `TypeSpec`.
    /// A monomorphic closure (`typarCount = 0`) uses its `Def` tokens instead.
    member _.RegisterClosure
        (
            name: string,
            typarCount: int,
            declaringTypars: int,
            captureSigs: FrozenType list,
            paramTy: FrozenType,
            resultTy: FrozenType,
            defHandle: EntityHandle
        ) : unit =
        env.GenericClosures.[name] <-
            {
                TyparCount = typarCount
                DeclaringTypars = declaringTypars
                CaptureSigs = captureSigs
                ParamTy = paramTy
                ResultTy = resultTy
                DefHandle = defHandle
            }

    /// Give a captureless value-struct closure an *encodable* `FrozenType`: it is keyed
    /// `TypeSlotKey.Closure name`, which no signature can encode, yet its by-value local, `initobj`
    /// and `MethodSpec` argument all need one. Registering here is what makes it a local `TypeDef`.
    member _.RegisterStackClosureValueType(name: string, defHandle: EntityHandle) : FrozenType =
        let typeKey = SymbolKeyOps.typeKeyOf "<closure>" name

        if env.UserTypes.ContainsKey typeKey then
            failwithf "Emit: synthetic value-struct closure key '%s' collides with a registered type" name

        env.UserTypes.[typeKey] <- defHandle
        env.UserValueTypes.Add typeKey |> ignore
        FTClass(typeKey, EqArray.empty)

    member _.GenericClosureTypeSpec(name: string, args: FrozenType list) : EntityHandle =
        generics.GenericClosureTypeSpec(name, args)

    member _.GenericClosureMemberRef(name: string, args: FrozenType list, which: ClosureMember) : EntityHandle =
        generics.GenericClosureMemberRef(name, args, which)

    /// Wrap a generic closure's own ctor / `Invoke` / field / locals / member-ref emission: the
    /// enclosing method's `FTTypar(Method, i)`, which the closure body embeds, re-projects onto
    /// the closure *class*'s `!(declaringTypars + i)` rather than `!!i`.
    member _.EnterClosureTyparScope(declaringTypars: int) : unit =
        env.ClosureTyparScope <- ValueSome declaringTypars

    /// INVARIANT: Exit resets to `ValueNone` rather than restoring a saved value, so Enter/Exit
    /// is only safe from an unscoped context. Code flipping the scope *mid-encoding of another
    /// signature* must save and restore `env.ClosureTyparScope` instead of calling Exit.
    member _.ExitClosureTyparScope() : unit = env.ClosureTyparScope <- ValueNone

    member _.EncodeAbstractType(te: SignatureTypeEncoder, t: FrozenType) : unit = enc.EncodeAbstractType(te, t)

    /// Resolve the `System.ValueTuple`n` family (parent `TypeSpec` + `.ctor` +
    /// `Item1…Itemn` field refs) for an N-tuple with the given element types.
    /// Arity 2–7; ≥8 throws.
    member _.ValueTupleRefs(elemTys: FrozenType list) : ValueTupleHandles = enc.ValueTupleRefs elemTys

    /// The `System.HashCode` accumulator local type for a union's `GetHashCode`.
    member _.HashCodeType: FrozenType = FTConst(ClrSinkKeys.hashCode, EqArray.empty)

    member _.EqualityComparerDefault(elem: FrozenType) : EntityHandle = recipes.EqualityComparerDefault elem

    member _.EqualityComparerEquals(elem: FrozenType) : EntityHandle = recipes.EqualityComparerEquals elem

    member _.EqualityComparerGetHashCode(elem: FrozenType) : EntityHandle =
        recipes.EqualityComparerGetHashCode elem

    member _.HashCodeAdd(elem: FrozenType) : EntityHandle = recipes.HashCodeAdd elem

    member _.HashCodeToHashCode: EntityHandle = env.EHashCodeToHashCode.Value

    member _.UserTypeHandle(key: TypeKey) : EntityHandle = env.UserTypes.[key]

    member _.EqualsOverrideSignature() : BlobBuilder = enc.EqualsOverrideSignature()

    member _.GetHashCodeOverrideSignature() : BlobBuilder = enc.GetHashCodeOverrideSignature()

    member _.EquatableInterfaceSpec(selfTy: FrozenType) : EntityHandle = recipes.EquatableInterfaceSpec selfTy

    member _.EqualsTypedSignature(selfTy: FrozenType) : BlobBuilder = enc.EqualsTypedSignature selfTy

    member _.ComparerDefault(elem: FrozenType) : EntityHandle = recipes.ComparerDefault elem

    member _.ComparerCompare(elem: FrozenType) : EntityHandle = recipes.ComparerCompare elem

    member _.ComparableInterfaceSpec(selfTy: FrozenType) : EntityHandle = recipes.ComparableInterfaceSpec selfTy

    member _.IComparableType: EntityHandle = env.EComparable.Value

    member _.ArgumentExceptionCtor: EntityHandle = env.EArgumentExceptionCtor.Value

    /// `System.NotSupportedException::.ctor()` — the synthesised `IEnumerator.Reset`
    /// capability co-slot throws it (the pull protocol has no rewind).
    member _.NotSupportedExceptionCtor: EntityHandle = env.ENotSupportedExceptionCtor.Value

    member _.CompareToOverrideSignature() : BlobBuilder = enc.CompareToOverrideSignature()

    member _.CompareToTypedSignature(selfTy: FrozenType) : BlobBuilder = enc.CompareToTypedSignature selfTy

    /// The `IStructuralFormattable` `InterfaceImpl` a `%A`-formattable synthesised record/DU declares.
    member _.StructuralFormattableInterface: EntityHandle =
        recipes.StructuralFormattableInterface

    member _.FormatSinkHandles: FormatSinkHandles = recipes.FormatSinkHandles

    member _.StructuralFormatSignature() : BlobBuilder = recipes.StructuralFormatSignature()

    member _.ExternalParameterlessBaseCtor(key: TypeKey) : EntityHandle voption = ext.ExternalParameterlessBaseCtor(key)

    interface ICodegenProvider with
        member _.ObjectType = env.EObject.Value
        member _.ExternalParameterlessBaseCtor(key) = ext.ExternalParameterlessBaseCtor(key)
        member _.ClassOrigin(key) = ext.ClassOrigin(key)
        member _.IntrinsicClassBase(canon) = ext.IntrinsicClassBase(canon)
        member _.TypeToken(ty) = recipes.TypeToken(ty)
        member _.ValueTupleRefs(elemTys) = enc.ValueTupleRefs elemTys
        member _.ExternalLayout(key) = env.ExternalLayout key
        member _.Platform = env.Symbols.Platform
        member _.DecimalCtor = env.EDecimalCtor.Value
        member _.ExceptionCtor = env.EExceptionCtor.Value

        member _.EqualityComparerDefault(elem) = recipes.EqualityComparerDefault elem

        member _.EqualityComparerEquals(elem) = recipes.EqualityComparerEquals elem

        member _.EqualityComparerGetHashCode(elem) =
            recipes.EqualityComparerGetHashCode elem

        member _.ExternalMemberReturnsVoid(key) = ext.ExternalMemberReturnsVoid key

        member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
            ext.ExternalMemberRef(key, isProperty, isStatic, memberTy)

        member _.TryCapabilityBaseMemberKey(key) =
            env.Symbols.TryRebaseCapabilityMember key

        member _.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy) =
            ext.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy)

        member _.ExternalFieldRef(key, declTy, memberTy) =
            ext.ExternalFieldRef(key, declTy, memberTy)

        member _.TryEmitCall(compiledName, key, fnTy) =
            if compiledName = "List.fold" then
                ValueSome(recipes.EmitFold(fnTy))
            else
                // Only a binding key identifies a module function; an operator-as-value does not,
                // and operators are expanded to `TExpr.ILIntrinsic` before emission anyway.
                match key with
                | ValueSome(SymbolKey.Binding binding as valueKey) ->
                    // A module VALUE of this assembly is a static field: load it, and let the
                    // caller `Invoke` any arguments a function-typed value takes.
                    match env.LocalModuleValues.TryGetValue valueKey with
                    | true, field ->
                        ValueSome
                            {
                                Emit =
                                    fun il ->
                                        il.Encoder.OpCode ILOpCode.Ldsfld
                                        il.Encoder.Token field
                                Arity = CallArity.Flat 0
                                Pushes = 1
                            }
                    | _ -> recipes.EmitExternalCall(binding, fnTy)
                | _ -> ValueNone

        member _.TryEmitCtor(key, chosen, tyArgs, argTypes) =
            ext.ExternalCtor(key, chosen, tyArgs, argTypes)

        member _.TryEmitUnionCons(key, caseName, tyArgs) =
            let elem () =
                match tyArgs with
                | [ e ] -> e
                | other -> failwithf "ClrProvider: list type expects one type argument, got %A" other

            // The cons-list, recognised by key identity rather than by string name.
            if RuntimeNames.isVesperListKey key then
                match caseName with
                | "Cons" -> ValueSome(recipes.EmitVesperListCons(elem ()))
                | "Empty" -> ValueSome(recipes.EmitVesperListEmpty(elem ()))
                | _ -> ValueNone
            else
                // A referenced-package union case (`Some` / `None`): `call` the emitted static
                // case factory `<caseName>(fields…) : Union<…>` on the instantiated `TypeSpec`,
                // its fields already on the stack in declaration order.
                match ext.ExternalUnionFactory(key, caseName, tyArgs) with
                | ValueSome(handle, argCount) ->
                    ValueSome
                        {
                            Emit = fun il -> il.Encoder.Call handle
                            Arity = CallArity.Flat argCount
                            Pushes = 1
                        }
                | ValueNone -> ValueNone

        member _.UserGenericMemberRef(key, args, kind) =
            let zonkedArgs = args

            match kind with
            | UserMemberKind.UnionMember which -> generics.GenericUnionMemberRef(key, zonkedArgs, which)
            | UserMemberKind.RecordMember which -> generics.GenericRecordMemberRef(key, zonkedArgs, which)
            | UserMemberKind.ClassMember which -> generics.GenericClassMemberRef(key, zonkedArgs, which)
            | UserMemberKind.Member(metaName, isStatic, methodTyparCount, paramTys, retTy) ->
                generics.GenericMemberRef(key, zonkedArgs, metaName, isStatic, methodTyparCount, paramTys, retTy)

        member _.UserClosureMemberRef(name, args, which) =
            generics.GenericClosureMemberRef(name, args, which)

        member _.TryEmitRecordCons(key, tyArgs, _fieldNames) =
            let zonkedArgs = tyArgs

            match ext.ExternalRecordCtor(key, zonkedArgs) with
            | ValueNone -> ValueNone
            | ValueSome handle ->
                let argCount =
                    match env.ExternalRecordShape(key, List.length zonkedArgs) with
                    | ValueSome(fields, _) -> fields.Length
                    | ValueNone -> 0

                ValueSome { Handle = handle; ArgCount = argCount }

        member _.TryResolveExternalRecordField(key, tyArgs, fieldName) =
            ext.ExternalRecordField(key, tyArgs, fieldName)

        member _.ExternalUnionTag(key, tyArgs, caseName) =
            // The cons-list is invisible to the generic external-union path, so match it
            // against its known emitted layout: `Empty` is tag 0, `Cons` tag 1.
            if RuntimeNames.isVesperListKey key then
                let elem =
                    match tyArgs with
                    | [ e ] -> e
                    | other -> failwithf "ClrProvider: cons-list match expects one type argument, got %A" other

                match caseName with
                | "Empty" -> ValueSome(recipes.EmitVesperListTagField elem, 0)
                | "Cons" -> ValueSome(recipes.EmitVesperListTagField elem, 1)
                | _ -> ValueNone
            else
                ext.ExternalUnionTag(key, tyArgs, caseName)

        member _.ExternalUnionCaseField(key, tyArgs, caseName, fieldIndex) =
            if RuntimeNames.isVesperListKey key then
                let elem =
                    match tyArgs with
                    | [ e ] -> e
                    | other -> failwithf "ClrProvider: cons-list match expects one type argument, got %A" other

                // Only `Cons` carries fields: `Cons_0` is the head (`elem`), `Cons_1` the tail
                // (`List<elem>`).
                match caseName, fieldIndex with
                | "Cons", 0 -> ValueSome(recipes.EmitVesperListConsField(elem, 0), elem)
                | "Cons", 1 -> ValueSome(recipes.EmitVesperListConsField(elem, 1), FTUnion(key, EqArray.ofList tyArgs))
                | _ -> ValueNone
            else
                ext.ExternalUnionCaseField(key, tyArgs, caseName, fieldIndex)

        member _.StaticFnMethodSpec(handle, instTypes) =
            ext.StaticFnMethodSpec(handle, instTypes)

        member _.RecoverOpenTypars(declTyparArity, methodTyparArity, openT, instT) =
            enc.RecoverOpenTypars(declTyparArity, methodTyparArity, openT, instT)

        member _.TryEmitInvoke(funcTy) =
            match funcTy with
            | FTFun _ as ft -> ValueSome(recipes.EmitInvoke ft)
            | _ -> ValueNone

        member _.FormatHandles() = recipes.BuildFormatHandles()

        member _.EncodeLocalSignature(locals) = enc.EncodeLocalSignature locals
