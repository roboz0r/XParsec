namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// `ICodegenProvider` over the BCL + the referenced assemblies. A thin shell over the collaborators
/// that hold the real implementation:
///   `ClrEnv`            — metadata context, reference identities, registries, ambient typar state;
///   `ClrEncoder`        — `FrozenType` → signature encoding + the signature-blob builders;
///   `ClrGenerics`       — `TypeSpec`/`MemberRef` minting for generic user unions/records/classes/closures;
///   `ClrExternalMembers`— external member/ctor/field refs and generic-static-method specs;
///   `ClrRecipes`        — call/ctor/format recipes and the structural equality/comparison member refs.
///
/// `reprs` is this unit's own `{ intrinsic canon key -> IL representation }` map
/// (`TastFile.IntrinsicReprKeys`); `references` maps an assembly's
/// simple name to the identity read off its file, so an emitted `AssemblyRef` matches that exact
/// artifact; `symbols` is the front end's resolution provider — pass
/// `ExternalSymbolProviders.nullProvider` on paths that emit no external member access.
type ClrProvider
    (
        ctx: MetadataContext,
        reprs: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, string>,
        references: Map<string, System.Reflection.AssemblyName>,
        symbols: IExternalSymbolProvider
    ) =

    let env = ClrEnv(ctx, reprs, references, CodegenSymbols.ofProvider symbols)

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

    /// Member ref to `System.Object::.ctor()` for a union's base-ctor chain.
    member _.ObjectCtorRef: EntityHandle = env.EObjectCtor.Value

    /// Register a user type emitted into this assembly so `encodeType` can reference it (by its
    /// predicted `TypeDefinition` handle) before its row is added.
    member _.RegisterUserType(key: TypeKey, handle: EntityHandle) : unit = env.UserTypes.[key] <- handle

    /// Register a module-level function emitted into this assembly (by its `ValueKey`) so a
    /// cross-file call to it resolves to its local `MethodDef` rather than an external member ref.
    member _.RegisterLocalModuleFn(key: SymbolKey, handle: EntityHandle) : unit = env.LocalModuleFns.[key] <- handle

    /// Record a project-local `[<Struct>]` value type so `encodeType` emits it as
    /// `ELEMENT_TYPE_VALUETYPE`.
    member _.RegisterUserValueType(key: TypeKey) : unit = env.UserValueTypes.Add key |> ignore

    /// Register a *generic* union's shape (typar names + cases) so member refs can be minted on its
    /// `TypeSpec`. A no-op for a monomorphic union (its `Def` tokens are used).
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

    /// The flat `Vesper.Fun`(len)<tys…>` interface `TypeSpec` a flat value-struct
    /// closure of param-arity `len-1` implements (`tys` = flat params ++ result).
    member _.FlatFunInterfaceSpecN(tys: FrozenType list) : EntityHandle = recipes.FlatFunInterfaceSpecN(tys)

    /// A `TypeSpec`/`TypeRef` handle for an arbitrary external type. A user class's
    /// `interface IEnumerable<'T>` carries its `'T` arg as a `TyTypar(Declaring, i)`
    /// the encoder resolves to the declaring type's `!i` directly. Drives each class
    /// `InterfaceImpl` row's interface handle.
    member _.TypeSpecOf(ty: FrozenType) : EntityHandle = enc.TypeSpecOf ty

    /// The `InterfaceImpl.Interface` handle for a user class's implemented
    /// interface. A *generic* interface (`IEnumerable<int>`) needs a
    /// `TypeSpec` carrying its instantiation; a *non-generic* one (`IEnumerable`,
    /// `IComparable`) references its `TypeRef` directly — the runtime rejects a
    /// `TypeSpec` that merely wraps a plain class in the interface-impl table (the
    /// structural-equality path uses the bare `IComparable` `TypeRef` for the same
    /// reason). A generic interface arg encodes off its `TyTypar(Declaring, i)` node.
    member _.InterfaceHandleOf(ty: FrozenType) : EntityHandle =
        match ty with
        | FTClass(key, args) when args.IsEmpty ->
            // LOCAL-FIRST, like every other nominal reference (`encodeType`'s project-local
            // arms, the `INVARIANT` at `ClrEncoder`'s `FTClass` arm): a project-local
            // interface — INCLUDING a same-assembly CROSS-FILE one, which a later unit
            // resolved as `External` (home-stamped to our OWN assembly) — has an emitted
            // `TypeDef` registered via `RegisterUserType`, and its `InterfaceImpl` row must
            // name that `TypeDef`, not an `AssemblyRef`-scoped `TypeRef` back to ourselves.
            // A non-generic local type must be the bare `TypeDef` (the runtime can't load a
            // `TypeSpec` for a non-generic type — "Could not load TypeSpec"). The external
            // table is consulted only on a `userTypes` MISS (a genuinely referenced-package
            // interface). Was external-first, which self-`AssemblyRef`'d a cross-file
            // interface impl (`externalClassRef` succeeds for it — it is in the projected
            // view — so the local fallback was never reached).
            match env.UserTypes.TryGetValue key with
            | true, h -> h
            | false, _ ->
                match env.ExternalClassRef(SymbolKey.Type key) with
                | ValueSome tref -> tref
                | ValueNone -> enc.TypeSpecOf ty
        | _ -> enc.TypeSpecOf ty

    /// The flat `instance resultTy Invoke(paramTys…)` signature of a `Fun`(N+1)` closure.
    member _.InvokeSignatureN(paramTys: FrozenType list, resultTy: FrozenType) : BlobBuilder =
        enc.InvokeSignatureN(paramTys, resultTy)

    member _.ClosureCtorSignature(captures: FrozenType list) : BlobBuilder = enc.ClosureCtorSignature captures

    member _.FieldSignature(ty: FrozenType) : BlobBuilder = enc.FieldSignature ty

    member _.ClosureSelfFieldSignature(closureTypeHandle: EntityHandle) : BlobBuilder =
        enc.ClosureSelfFieldSignature closureTypeHandle

    /// Register a *generic* closure's shape so its member refs can be minted on its `TypeSpec`. A
    /// monomorphic closure (`Closure.Typars = []`) is *not* registered — its `Def` tokens are used.
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

    /// Register a captureless `Stack` (value-struct) closure
    /// under a synthetic project-local `TypeKey` and return the `FrozenType` that
    /// names it. A closure has no `FrozenType` of its own (it is keyed by
    /// `TypeSlotKey.Closure name`, codegen-only), but a value-struct closure must be
    /// *encodable* — its by-value local, its `initobj`, and the constrained-slot
    /// `MethodSpec` type-argument all reference it. Minting an `FTClass(synthKey, [])` and
    /// registering `synthKey → defHandle` in `userTypes` + `userValueTypes` makes the SHARED
    /// `encodeType` value-type arm (`ELEMENT_TYPE_VALUETYPE`) emit it — no new
    /// encoder/MethodSpec path needed. The registration is ALSO what makes the key local:
    /// `encodeType`'s project-local arms are `userTypes` membership, so the synthetic type
    /// encodes as a `TypeDef` exactly because it is in the table.
    /// The synthetic key's `name` is never used for emission (only the handle is),
    /// so the closure name suffices. The key is placed in the reserved `<closure>`
    /// namespace — a sigil no source-declared type can produce — so it provably
    /// cannot collide with a real `userTypes` key; the guard below fails fast if
    /// that invariant is ever broken.
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

    /// Enter / exit closure-typar mode around a generic closure's own ctor / Invoke /
    /// field-signature / locals / member-ref emission: the enclosing method's
    /// `TyTypar(Method, i)` (which the closure body embeds) re-project onto the
    /// closure *class*'s `GenericTypeParameter i` rather than `!!i`. This is the
    /// only ambient typar mode that survives.
    member _.EnterClosureTyparScope(declaringTypars: int) : unit =
        env.ClosureTyparScope <- ValueSome declaringTypars

    /// INVARIANT: Enter/Exit is only used from an *unscoped* context — the
    /// `Assembler` field and closure passes are flat loops, never nested — so Exit
    /// resets to `ValueNone` rather than restoring a saved value. Code that flips
    /// the scope *mid-encoding of another signature* (`ClrGenerics`) must instead
    /// save `env.ClosureTyparScope` and restore it, not call Exit, or it would
    /// clobber the outer scope.
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

    /// `%A` structural-format synthesis. The `IStructuralFormattable`
    /// `InterfaceImpl` a synthesised record/DU declares; the `IFormatSink` member
    /// refs its `Format` body calls; and the `Format(IFormatSink) : void` signature.
    member _.StructuralFormattableInterface: EntityHandle =
        recipes.StructuralFormattableInterface

    member _.FormatSinkHandles: FormatSinkHandles = recipes.FormatSinkHandles

    member _.StructuralFormatSignature() : BlobBuilder = recipes.StructuralFormatSignature()

    member _.ExternalParameterlessBaseCtor(key: SymbolKey) : EntityHandle voption =
        ext.ExternalParameterlessBaseCtor(key)

    interface ICodegenProvider with
        member _.ObjectType = env.EObject.Value
        member _.ExternalParameterlessBaseCtor(key) = ext.ExternalParameterlessBaseCtor(key)
        member _.ExternalClassTypeRef(key) = ext.ExternalClassTypeRef(key)
        member _.IntrinsicClassBase(canon) = ext.IntrinsicClassBase(canon)
        member _.TypeToken(ty) = recipes.TypeToken(ty)
        member _.ValueTupleRefs(elemTys) = enc.ValueTupleRefs elemTys
        member _.IsExternalValueType(key) = env.ExternalIsValueType key
        member _.DecimalCtor = env.EDecimalCtor.Value
        member _.ExceptionCtor = env.EExceptionCtor.Value

        member _.EqualityComparerDefault(elem) = recipes.EqualityComparerDefault elem

        member _.EqualityComparerEquals(elem) = recipes.EqualityComparerEquals elem

        member _.EqualityComparerGetHashCode(elem) =
            recipes.EqualityComparerGetHashCode elem

        member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
            ext.ExternalMemberRef(key, isProperty, isStatic, memberTy)

        member _.TryCapabilityBaseMemberKey(key) =
            env.Symbols.TryRebaseCapabilityMember key

        member _.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy) =
            ext.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy)

        member _.ExternalFieldRef(key, declTy, memberTy) =
            ext.ExternalFieldRef(key, declTy, memberTy)

        member _.FSharpCoreDependencies() = env.FSharpCoreDependencies()

        member _.TryEmitCall(compiledName, key, fnTy) =
            if compiledName = "List.fold" then
                ValueSome(recipes.EmitFold(fnTy))
            else
                // General external module-function call: route by the Elaborate-stamped key to the
                // declaring module (`ns`) + method (`name`), and mint
                // a `call` (+ `MethodSpec` when generic) to the static method our backend emitted into
                // the referenced package. Only a module-qualified value key (`ns <> ""`) is a module
                // function; a bare key (an operator-as-value) is not, and operators are expanded to
                // `TExpr.ILIntrinsic` by `Emit.lower` before emission anyway. `EmitExternalCall`
                // returns `ValueNone` when the symbol is unknown to the provider, falling through to
                // the caller's hard error. (Every lowerable printf call is now a `TExpr.Format`
                // lowered in Elaborate, so no `printfn` App reaches here — the cold recipe is gone.)
                match key with
                | ValueSome(SymbolKey.Binding {
                                                  Decl = ModuleHolder.InModule m
                                                  Name = name
                                              }) -> recipes.EmitExternalCall(m, name, fnTy)
                | _ -> ValueNone

        member _.TryEmitCtor(key, chosen, tyArgs, argTypes) =
            ext.ExternalCtor(key, chosen, tyArgs, argTypes)

        member _.TryEmitUnionCons(key, caseName, tyArgs) =
            let elem () =
                match tyArgs with
                | [ e ] -> e
                | other -> failwithf "ClrProvider: list type expects one type argument, got %A" other

            // The cons recipe is selected by the receiver's nominal `SymbolKey`:
            // FSharp.Core's `list` vs the Vesper cons-list, recognised by
            // key identity rather than by string name.
            if RuntimeNames.isFsharpCoreListKey key then
                match caseName with
                | "Cons" -> ValueSome(recipes.EmitListCons(elem ()))
                | "Nil" -> ValueSome(recipes.EmitListNil(elem ()))
                | _ -> ValueNone
            elif RuntimeNames.isVesperListKey key then
                // `Empty` (the `[]` operator case) is the empty terminator post-`list.fs`
                // cutover; `Cons` the binary case.
                match caseName with
                | "Cons" -> ValueSome(recipes.EmitVesperListCons(elem ()))
                | "Empty" -> ValueSome(recipes.EmitVesperListEmpty(elem ()))
                | _ -> ValueNone
            else
                // A referenced-package union case (`Some` / `None`): `call` the
                // emitted static case factory `<caseName>(fields…) : Union<…>` on
                // the instantiated `TypeSpec`. The fields are already on the stack
                // in declaration order, so the
                // recipe is a static `call` pushing the one union value back.
                match ext.ExternalUnionFactory(SymbolKey.Type key, caseName, tyArgs) with
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

            match ext.ExternalRecordCtor(SymbolKey.Type key, zonkedArgs) with
            | ValueNone -> ValueNone
            | ValueSome handle ->
                let argCount =
                    match env.ExternalRecordShape(SymbolKey.Type key, List.length zonkedArgs) with
                    | ValueSome(fields, _) -> fields.Length
                    | ValueNone -> 0

                ValueSome { Handle = handle; ArgCount = argCount }

        member _.TryResolveExternalRecordField(key, tyArgs, fieldName) =
            ext.ExternalRecordField(key, tyArgs, fieldName)

        member _.ExternalUnionTag(key, tyArgs, caseName) =
            // The cons-list keeps op-form case names (`op_Nil` / `op_ColonColon`) in
            // its extracted contract, so it never resolves through the generic
            // external-union path; mirror construction (`TryEmitUnionCons`) and
            // special-case the cross-package `match` against its known emitted layout
            // (`Empty` tag 0, `Cons` tag 1).
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
                ext.ExternalUnionTag(SymbolKey.Type key, tyArgs, caseName)

        member _.ExternalUnionCaseField(key, tyArgs, caseName, fieldIndex) =
            if RuntimeNames.isVesperListKey key then
                let elem =
                    match tyArgs with
                    | [ e ] -> e
                    | other -> failwithf "ClrProvider: cons-list match expects one type argument, got %A" other

                // Only `Cons` carries fields: `Cons_0` is the head (`elem`), `Cons_1`
                // the tail (`List<elem>`). The returned type is informational (the
                // match compiler binds the field local off the sub-pattern's own type).
                match caseName, fieldIndex with
                | "Cons", 0 -> ValueSome(recipes.EmitVesperListConsField(elem, 0), elem)
                | "Cons", 1 -> ValueSome(recipes.EmitVesperListConsField(elem, 1), FTUnion(key, EqArray.ofList tyArgs))
                | _ -> ValueNone
            else
                ext.ExternalUnionCaseField(SymbolKey.Type key, tyArgs, caseName, fieldIndex)

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
