namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Vesper
open XParsec.FSharp.SemanticAnalysis

/// `ICodegenProvider` over the BCL + the referenced assemblies — a thin shell forwarding to the
/// collaborators constructed below. `bindings` maps an intrinsic canon key to its platform type
/// id; `references` maps an assembly's simple name to the identity read off its own file.
type ClrProvider
    (
        ctx: MetadataContext,
        bindings: System.Collections.Generic.IReadOnlyDictionary<TypeKey, PlatformTypeId>,
        references: Map<string, System.Reflection.AssemblyName>,
        symbols: ICodegenSymbols
    ) =

    let env = ClrEnv(ctx, bindings, references, symbols)

    let enc = ClrEncoder(env)
    let generics = ClrGenerics(env, enc)
    let ext = ClrExternalMembers(env, enc)
    let recipes = ClrRecipes(env, enc)

    /// The element type of a Vesper cons-list nominal, matched by key identity because
    /// FSharp.Core's `list` and the Vesper cons-list are both written `list`. A cons-list
    /// carries exactly one type argument, so any other arity throws.
    let (|VesperList|_|) (key: TypeKey) (tyArgs: Block<FrozenType>) : FrozenType voption =
        if RuntimeNames.isVesperListKey key then
            match tyArgs with
            | BlockOne elem -> ValueSome elem
            | other -> failwithf "ClrProvider: the cons-list expects one type argument, got %A" other
        else
            ValueNone

    member _.ObjectType: EntityHandle = env.EObject.Value

    /// `System.ValueType` — the IL base type of a `[<Struct>]` value type.
    member _.ValueTypeBase: EntityHandle = env.EValueType.Value

    /// `System.Enum` — the IL base type of a numeric enum's `TypeDefinition`.
    member _.EnumBase: EntityHandle = env.EEnum.Value

    /// The `.ctor` `MemberRef` a synthetic attribute's `CustomAttribute` row is written
    /// against.
    member _.SyntheticAttributeCtor(attr: SyntheticAttribute) : EntityHandle =
        match attr with
        | SyntheticAttribute.IsReadOnly -> env.EIsReadOnlyAttrCtor.Value
        | SyntheticAttribute.IsByRefLike -> env.EIsByRefLikeAttrCtor.Value
        | SyntheticAttribute.EditorBrowsableNever -> env.EEditorBrowsableAttrCtor.Value

    /// `System.AttributeUsageAttribute::.ctor(System.AttributeTargets)` — the CLR spelling
    /// a `[<AttributeUsage>]` row is written against.
    member _.AttributeUsageAttrCtor: EntityHandle = env.EAttributeUsageAttrCtor.Value

    /// The `MemberRef` of the attribute constructor the front end selected.
    member _.TryExternalAttributeCtor(ctor: MemberKey) : EntityHandle voption = ext.ExternalAttributeCtor ctor

    /// Member ref to `System.Object::.ctor()` for a union's base-ctor chain.
    member _.ObjectCtorRef: EntityHandle = env.EObjectCtor.Value

    /// A Vesper canon's platform type id on this target: `int` → `System.Int32`.
    member _.TryPrimitiveTypeId(key: TypeKey) : PlatformTypeId voption = env.TryPrimitiveTypeId key

    /// The metadata path of a referenced nominal; `ValueNone` for a type this compilation
    /// emits.
    member internal _.TryExternalTypePath(key: TypeKey) : ExternalTypePath voption = env.TryExternalTypePath key

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
        (
            key: TypeKey,
            typars: BlockM<string, typeSlot>,
            cases: (string * (string * FrozenType) list) list,
            valueKind: NominalValueKind,
            home: UnionSlotHome voption
        ) : unit =
        env.GenericUnions.[key] <-
            {
                Typars = typars
                Cases =
                    cases
                    |> Block.ofSeq
                    |> Block.map (fun (name, fields) ->
                        {
                            Name = name
                            Fields = Block.ofList fields
                        }
                    )
                ValueKind = valueKind
                Home = home
            }

    member internal _.RegisterGenericRecord
        (key: TypeKey, typars: BlockM<string, typeSlot>, fields: GenericRecordField list)
        : unit =
        env.GenericRecords.[key] <-
            {
                Typars = typars
                Fields = Block.ofList fields
            }

    member _.RegisterGenericClass
        (
            key: TypeKey,
            typars: BlockM<string, typeSlot>,
            ctorParamTys: FrozenType list,
            fields: (string * FrozenType) list
        ) : unit =
        env.GenericClasses.[key] <-
            {
                Typars = typars
                CtorParamTys = Block.ofList ctorParamTys
                Fields = Block.ofList fields
            }

    member _.RecordCtorSignature(paramTys: Block<FrozenType>) : BlobBuilder = enc.RecordCtorSignature(paramTys)

    member _.GenericMethodOnTypeSignature
        (methodTyparCount: int<typeSlot>, paramTys: Block<FrozenType>, retTy: FrozenType, isInstanceMethod: bool)
        : BlobBuilder =
        enc.GenericMethodOnTypeSignature(methodTyparCount, paramTys, retTy, isInstanceMethod)

    member _.CctorSignature() : BlobBuilder = enc.CctorSignature()

    member _.GenericUnionSelfSpec(key: TypeKey) : EntityHandle = generics.GenericUnionSelfSpec key

    member _.GenericRecordSelfSpec(key: TypeKey) : EntityHandle = generics.GenericRecordSelfSpec key

    member _.GenericStaticFnSignature
        (typarCount: int<typeSlot>, paramTys: Block<FrozenType>, retTy: FrozenType)
        : BlobBuilder =
        enc.GenericStaticFnSignature(typarCount, paramTys, retTy)

    member _.StaticMethodSignature(paramTys: Block<FrozenType>, retTy: FrozenType) : BlobBuilder =
        enc.StaticMethodSignature(paramTys, retTy)

    member _.InstanceMethodSignature(paramTys: Block<FrozenType>, retTy: FrozenType) : BlobBuilder =
        enc.InstanceMethodSignature(paramTys, retTy)

    member _.InstanceMethodSignatureVoid(paramTys: Block<FrozenType>) : BlobBuilder =
        enc.InstanceMethodSignatureVoid(paramTys)

    member _.RecordAccessorSignature(role: TAccessorRole, fieldTy: FrozenType) : BlobBuilder =
        enc.RecordAccessorSignature(role, fieldTy)

    member _.PropertySignature(isInstance: bool, indexTys: Block<FrozenType>, valueTy: FrozenType) : BlobBuilder =
        enc.PropertySignature(isInstance, indexTys, valueTy)

    member _.StaticMethodSignatureVoid(paramTys: Block<FrozenType>) : BlobBuilder =
        enc.StaticMethodSignatureVoid(paramTys)

    member _.GenericMethodOnTypeSignatureVoid
        (methodTyparCount: int<typeSlot>, paramTys: Block<FrozenType>, isInstanceMethod: bool)
        : BlobBuilder =
        enc.GenericMethodOnTypeSignatureVoid(methodTyparCount, paramTys, isInstanceMethod)

    member _.FunInterfaceSpec(a: FrozenType, b: FrozenType) : EntityHandle = recipes.FunInterfaceSpec(a, b)

    member _.FlatFunInterfaceSpecN(tys: Block<FrozenType>) : EntityHandle = recipes.FlatFunInterfaceSpecN(tys)

    /// A `TypeSpec`/`TypeRef` handle for an arbitrary external type. A user class's
    /// `interface IEnumerable<'T>` carries its `'T` as `FTTypar(Type _, i)`, which the
    /// encoder resolves to the declaring type's `!i`.
    member _.TypeSpecOf(ty: FrozenType) : EntityHandle = enc.TypeSpecOf ty

    /// The `TypeDefOrRef` handle of a declared type position: an `InterfaceImpl.Interface`
    /// or a `GenericParamConstraint` target. A bare nominal (`IComparable`) is its
    /// `TypeDef` / `TypeRef`; every other type (`IEnumerable<int>`) is a `TypeSpec`.
    member _.TypeDefOrRefOf(ty: FrozenType) : EntityHandle = enc.TypeDefOrRefOf ty

    /// The flat `instance resultTy Invoke(paramTys…)` signature of a `Fun`(N+1)` closure.
    member _.InvokeSignatureN(paramTys: Block<FrozenType>, resultTy: FrozenType) : BlobBuilder =
        enc.InvokeSignatureN(paramTys, resultTy)

    member _.ClosureCtorSignature(captures: Block<FrozenType>) : BlobBuilder = enc.ClosureCtorSignature captures

    member _.FieldSignature(ty: FrozenType) : BlobBuilder = enc.FieldSignature ty

    member _.ClosureSelfFieldSignature(closureTypeHandle: EntityHandle) : BlobBuilder =
        enc.ClosureSelfFieldSignature closureTypeHandle

    /// Register a *generic* closure's shape so its member refs can be minted on its `TypeSpec`.
    /// A monomorphic closure (`typarCount = 0`) uses its `Def` tokens instead.
    member _.RegisterClosure
        (
            name: string,
            frame: TyparFrame,
            captureSigs: Block<FrozenType>,
            paramTy: FrozenType,
            resultTy: FrozenType,
            defHandle: EntityHandle
        ) : unit =
        env.GenericClosures.[name] <-
            {
                Frame = frame
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
        FTClass(typeKey, Block.empty)

    member _.GenericClosureTypeSpec(name: string, args: Block<FrozenType>) : EntityHandle =
        generics.GenericClosureTypeSpec(name, args)

    member _.GenericClosureMemberRef(name: string, args: Block<FrozenType>, which: ClosureMember) : EntityHandle =
        generics.GenericClosureMemberRef(name, args, which)

    /// Run `f`, a synthesised owner's own signature / body / member-ref emission, with every
    /// `FTTypar` leaf resolved through `slots`.
    member _.WithTyparSlots(slots: TyparSlots, f: unit -> 'T) : 'T = env.WithTyparSlots(slots, f)

    member _.EncodeAbstractType(te: SignatureTypeEncoder, t: FrozenType) : unit = enc.EncodeAbstractType(te, t)

    /// Resolve the `System.ValueTuple`n` family (parent `TypeSpec` + `.ctor` +
    /// `Item1…Itemn` field refs) for an N-tuple with the given element types.
    /// Arity 2–7; ≥8 throws.
    member _.ValueTupleRefs(elemTys: Block<FrozenType>) : ValueTupleHandles = enc.ValueTupleRefs elemTys

    /// The `System.HashCode` accumulator local type for a union's `GetHashCode`.
    member _.HashCodeType: FrozenType = FTConst(ClrSinkKeys.hashCode, Block.empty)

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

    interface IStructuralHandles with
        member _.EqualityComparerDefault elem = recipes.EqualityComparerDefault elem
        member _.EqualityComparerEquals elem = recipes.EqualityComparerEquals elem
        member _.HashCodeType = FTConst(ClrSinkKeys.hashCode, Block.empty)
        member _.HashCodeAdd elem = recipes.HashCodeAdd elem
        member _.HashCodeToHashCode = env.EHashCodeToHashCode.Value
        member _.ComparerDefault elem = recipes.ComparerDefault elem
        member _.ComparerCompare elem = recipes.ComparerCompare elem
        member _.StringEquals = env.EStringEquals.Value
        member _.StringCompareOrdinal = env.EStringCompareOrdinal.Value
        member _.ArgumentExceptionCtor = env.EArgumentExceptionCtor.Value
        member _.FormatSink = recipes.FormatSinkHandles
        member _.BoxToken elem = recipes.TypeToken elem
        member _.UserString s = ctx.UserString s

    interface ICodegenProvider with
        member _.ObjectType = env.EObject.Value
        member _.ExternalParameterlessBaseCtor(key) = ext.ExternalParameterlessBaseCtor(key)
        member _.ClassOrigin(key) = ext.ClassOrigin(key)
        member _.IntrinsicClassBase(canon) = ext.IntrinsicClassBase(canon)
        member _.TypeToken(ty) = recipes.TypeToken(ty)
        member _.TypeFromHandle = env.ETypeGetTypeFromHandle.Value
        member _.TypeIsGenericType = env.ETypeIsGenericType.Value
        member _.TypeGetGenericTypeDefinition = env.ETypeGetGenericTypeDefinition.Value
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

        member _.TryEmitCall(key, fnTy) =
            // A module VALUE of this assembly is a static field: load it, and let the
            // caller `Invoke` any arguments a function-typed value takes.
            match env.LocalModuleValues.TryGetValue(SymbolKey.Binding key) with
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
            | _ -> recipes.EmitExternalCall(key, fnTy)

        member _.TryEmitCtor(key, chosen, tyArgs, argTypes) =
            ext.ExternalCtor(key, chosen, tyArgs, argTypes)

        member _.TryEmitUnionCons(key, caseName, tyArgs) =
            match tyArgs with
            | VesperList key elem ->
                match caseName with
                | "Cons" -> ValueSome(recipes.EmitVesperListCons elem)
                | "Empty" -> ValueSome(recipes.EmitVesperListEmpty elem)
                | _ -> ValueNone
            | _ ->
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
            match kind with
            | UserMemberKind.UnionMember which -> generics.GenericUnionMemberRef(key, args, which)
            | UserMemberKind.RecordMember which -> generics.GenericRecordMemberRef(key, args, which)
            | UserMemberKind.ClassMember which -> generics.GenericClassMemberRef(key, args, which)
            | UserMemberKind.Member(metaName, isStatic, methodTyparCount, paramTys, retTy) ->
                generics.GenericMemberRef(key, args, metaName, isStatic, methodTyparCount, paramTys, retTy)

        member _.UserClosureMemberRef(name, args, which) =
            generics.GenericClosureMemberRef(name, args, which)

        member _.UserClosureTypeSpec(name, args) =
            generics.GenericClosureTypeSpec(name, args)

        member _.TryEmitRecordCons(key, tyArgs, _fieldNames) =
            match ext.ExternalRecordCtor(key, tyArgs) with
            | ValueNone -> ValueNone
            | ValueSome handle ->
                let argCount =
                    match env.ExternalRecordShape(key, tyArgs.Length) with
                    | ValueSome(fields, _) -> fields.Length
                    | ValueNone -> 0

                ValueSome { Handle = handle; ArgCount = argCount }

        member _.TryResolveExternalRecordField(key, tyArgs, fieldName, role) =
            ext.ExternalRecordField(key, tyArgs, fieldName, role)

        member _.ExternalUnionCaseTest(key, tyArgs, caseName) =
            // The cons-list is invisible to the generic external-union path, so match it
            // against its known emitted layout: two cases with a payload ⇒ type-tested.
            match tyArgs with
            | VesperList key elem ->
                match caseName with
                | "Empty"
                | "Cons" -> ValueSome(UnionCaseTest.IsInst(recipes.VesperListCaseTypeSpec(elem, caseName)))
                | _ -> ValueNone
            | _ -> ext.ExternalUnionCaseTest(key, tyArgs, caseName)

        member _.ExternalUnionCaseField(key, tyArgs, caseName, fieldIndex) =
            match tyArgs with
            | VesperList key elem ->
                // Only `Cons` carries fields: field 0 is the head, field 1 the tail, both on
                // the `Cons` case type.
                match caseName, fieldIndex with
                | "Cons", 0 -> ValueSome(UnionCaseAccess.Field [ recipes.EmitVesperListConsField(elem, 0) ])
                | "Cons", 1 -> ValueSome(UnionCaseAccess.Field [ recipes.EmitVesperListConsField(elem, 1) ])
                | _ -> ValueNone
            | _ -> ext.ExternalUnionCaseField(key, tyArgs, caseName, fieldIndex)

        member _.ExternalUnionCaseType(key, tyArgs, caseName) =
            match tyArgs with
            | VesperList key elem -> ValueSome(recipes.VesperListCaseTypeSpec(elem, caseName))
            | _ -> ext.ExternalUnionCaseType(key, tyArgs, caseName)

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
