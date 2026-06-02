namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// `ICodegenProvider` over the BCL + the referenced assemblies. A thin shell over the collaborators
/// that hold the real implementation:
///   `ClrEnv`            — metadata context, reference identities, registries, ambient typar state;
///   `ClrEncoder`        — `SemType` → signature encoding + the signature-blob builders;
///   `ClrGenerics`       — `TypeSpec`/`MemberRef` minting for generic user unions/records/classes/closures;
///   `ClrExternalMembers`— external member/ctor/field refs and generic-static-method specs;
///   `ClrRecipes`        — call/ctor/format recipes and the structural equality/comparison member refs.
///
/// `reprs` is the Vesper-primitive-name → IL-representation map; `references` maps an assembly's
/// simple name to the identity read off its file, so an emitted `AssemblyRef` matches that exact
/// artifact; `symbols` is the front end's resolution provider — pass
/// `ExternalSymbols.nullProvider` on paths that emit no external member access.
type ClrProvider
    (
        ctx: MetadataContext,
        reprs: Map<string, string>,
        references: Map<string, AssemblyName>,
        symbols: IExternalSymbolProvider,
        assemblyName: string
    ) =

    let env = ClrEnv(ctx, reprs, references, symbols, assemblyName)
    let enc = ClrEncoder(env)
    let generics = ClrGenerics(env, enc)
    let ext = ClrExternalMembers(env, enc)
    let recipes = ClrRecipes(env, enc)

    member _.ObjectType: EntityHandle = env.EObject.Value

    /// Member ref to `System.Object::.ctor()` for a union's base-ctor chain.
    member _.ObjectCtorRef: EntityHandle = env.EObjectCtor.Value

    /// Register a user type emitted into this assembly so `encodeType` can reference it (by its
    /// predicted `TypeDefinition` handle) before its row is added.
    member _.RegisterUserType(key: SymbolKey, handle: EntityHandle) : unit = env.UserTypes.[key] <- handle

    /// Register a *generic* union's shape (typar names + cases) so member refs can be minted on its
    /// `TypeSpec`. A no-op for a monomorphic union (its `Def` tokens are used).
    member _.RegisterGenericUnion
        (key: SymbolKey, typars: string list, cases: (string * (string * SemType) list) list)
        : unit =
        env.GenericUnions.[key] <- (typars, cases)

    member _.RegisterGenericRecord(key: SymbolKey, typars: string list, fields: (string * SemType) list) : unit =
        env.GenericRecords.[key] <- (typars, fields)

    member _.RegisterGenericClass(key: SymbolKey, typars: string list, fields: (string * SemType) list) : unit =
        env.GenericClasses.[key] <- (typars, fields)

    member _.GenericFieldSignature(typars: string list, declTy: SemType) : BlobBuilder =
        enc.GenericFieldSignature(typars, declTy)

    member _.GenericStaticMethodSignature(typars: string list, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        enc.GenericStaticMethodSignature(typars, paramTys, retTy)

    member _.GenericRecordCtorSignature(typars: string list, paramTys: SemType list) : BlobBuilder =
        enc.GenericRecordCtorSignature(typars, paramTys)

    member _.GenericInstanceMethodSignature(typars: string list, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        enc.GenericInstanceMethodSignature(typars, paramTys, retTy)

    member _.GenericMethodOnTypeSignature
        (typeTypars: string list, methodTyparCount: int, paramTys: SemType list, retTy: SemType, isInstanceMethod: bool)
        : BlobBuilder =
        enc.GenericMethodOnTypeSignature(typeTypars, methodTyparCount, paramTys, retTy, isInstanceMethod)

    member _.EncodeGenericLocalSignature(typars: string list, locals: SemType list) : StandaloneSignatureHandle =
        enc.EncodeGenericLocalSignature(typars, locals)

    member _.NullaryCtorSignature() : BlobBuilder = enc.NullaryCtorSignature()

    member _.CctorSignature() : BlobBuilder = enc.CctorSignature()

    /// Install the ambient generic-method-typar set (by union-find root) for the generic static method
    /// about to be emitted, so `encodeType` maps those `TypeVar`s to `!!i`.
    member _.SetMethodTypars(typars: TypeVar list) : unit =
        env.MethodTyparRoots <- typars |> List.map UnionFind.find

    member _.ClearMethodTypars() : unit = env.MethodTyparRoots <- []

    /// Install the ambient *type*-typar set for a generic union's equality triple, so `encodeType` maps
    /// a declaring-typar marker (`'T`) to that type's `GenericTypeParameter` (`!0`). The names carry the
    /// F# leading quote.
    member _.SetTypeTypars(typars: string list) : unit =
        env.TypeTyparIx <- typars |> List.mapi (fun i n -> n, i) |> Map.ofList

    member _.ClearTypeTypars() : unit = env.TypeTyparIx <- Map.empty

    member _.GenericUnionSelfSpec(key: SymbolKey) : EntityHandle = generics.GenericUnionSelfSpec key

    member _.GenericRecordSelfSpec(key: SymbolKey) : EntityHandle = generics.GenericRecordSelfSpec key

    member _.GenericStaticFnSignature(typarCount: int, paramTys: SemType list, retTy: SemType) : BlobBuilder =
        enc.GenericStaticFnSignature(typarCount, paramTys, retTy)

    member _.StaticMethodSignature(paramTys: SemType list, retTy: SemType) : BlobBuilder =
        enc.StaticMethodSignature(paramTys, retTy)

    member _.InstanceMethodSignature(paramTys: SemType list, retTy: SemType) : BlobBuilder =
        enc.InstanceMethodSignature(paramTys, retTy)

    member _.FunInterfaceSpec(a: SemType, b: SemType) : EntityHandle = recipes.FunInterfaceSpec(a, b)

    /// A `TypeSpec`/`TypeRef` handle for an arbitrary external type, honouring the
    /// ambient `SetTypeTypars` set — so a user class's `interface IEnumerable<'T>`
    /// (B-2, §5.3) encodes its `'T` arg against the declaring type's generic
    /// parameters. Drives each class `InterfaceImpl` row's interface handle.
    member _.TypeSpecOf(ty: SemType) : EntityHandle = enc.TypeSpecOf ty

    /// The `InterfaceImpl.Interface` handle for a user class's implemented
    /// interface (B-2, §5.3). A *generic* interface (`IEnumerable<int>`) needs a
    /// `TypeSpec` carrying its instantiation; a *non-generic* one (`IEnumerable`,
    /// `IComparable`) references its `TypeRef` directly — the runtime rejects a
    /// `TypeSpec` that merely wraps a plain class in the interface-impl table (the
    /// structural-equality path uses the bare `IComparable` `TypeRef` for the same
    /// reason). The generic case rides the ambient `SetTypeTypars` window.
    member _.InterfaceHandleOf(ty: SemType) : EntityHandle =
        match env.Zonk ty with
        | TyClass(key, args) when args.IsEmpty ->
            match env.ExternalClassRef(ExternalSymbols.qualifiedName key) with
            | ValueSome tref -> tref
            | ValueNone -> enc.TypeSpecOf ty
        | _ -> enc.TypeSpecOf ty

    member _.InvokeSignature(a: SemType, b: SemType) : BlobBuilder = enc.InvokeSignature(a, b)

    member _.ClosureCtorSignature(captures: SemType list) : BlobBuilder = enc.ClosureCtorSignature captures

    member _.FieldSignature(ty: SemType) : BlobBuilder = enc.FieldSignature ty

    /// Register a *generic* closure's shape so its member refs can be minted on its `TypeSpec`. A
    /// monomorphic closure (`Closure.Typars = []`) is *not* registered — its `Def` tokens are used.
    member _.RegisterClosure
        (
            name: string,
            typarCount: int,
            captureSigs: SemType list,
            paramTy: SemType,
            resultTy: SemType,
            defHandle: EntityHandle
        ) : unit =
        env.GenericClosures.[name] <-
            {
                TyparCount = typarCount
                CaptureSigs = captureSigs
                ParamTy = paramTy
                ResultTy = resultTy
                DefHandle = defHandle
            }

    member _.GenericClosureTypeSpec(name: string, args: SemType list) : EntityHandle =
        generics.GenericClosureTypeSpec(name, List.map env.Zonk args)

    member _.GenericClosureMemberRef(name: string, args: SemType list, which: ClosureMember) : EntityHandle =
        generics.GenericClosureMemberRef(name, List.map env.Zonk args, which)

    member _.GenericCaptureFieldSignature(closureTypars: TypeVar list, ty: SemType) : BlobBuilder =
        generics.GenericCaptureFieldSignature(closureTypars, ty)

    /// Enter / exit closure-typar mode around a generic closure's own ctor / Invoke /
    /// field-signature / locals / member-ref emission: the enclosing method's
    /// `TempTypar(Method, i)` (which the closure body embeds) re-project onto the
    /// closure *class*'s `GenericTypeParameter i` rather than `!!i` (frozen-type-plan
    /// 2B). Invariant: at most one of `SetMethodTypars` / `SetTypeTypars` /
    /// `EnterClosureTyparScope` is active at a time.
    member _.EnterClosureTyparScope() : unit = env.ClosureTyparMode <- true

    member _.ExitClosureTyparScope() : unit = env.ClosureTyparMode <- false

    member _.EncodeAbstractType
        (typeIx: Map<string, int>, methodIx: Map<string, int>, te: SignatureTypeEncoder, t: SemType)
        : unit =
        enc.EncodeAbstractType(typeIx, methodIx, te, t)

    /// The `System.HashCode` accumulator local type for a union's `GetHashCode`.
    member _.HashCodeType: SemType = TyConst("System.HashCode", EqArray.empty)

    member _.EqualityComparerDefault(elem: SemType) : EntityHandle = recipes.EqualityComparerDefault elem

    member _.EqualityComparerEquals(elem: SemType) : EntityHandle = recipes.EqualityComparerEquals elem

    member _.EqualityComparerGetHashCode(elem: SemType) : EntityHandle =
        recipes.EqualityComparerGetHashCode elem

    member _.HashCodeAdd(elem: SemType) : EntityHandle = recipes.HashCodeAdd elem

    member _.HashCodeToHashCode: EntityHandle = env.EHashCodeToHashCode.Value

    member _.UserTypeHandle(key: SymbolKey) : EntityHandle = env.UserTypes.[key]

    member _.EqualsOverrideSignature() : BlobBuilder = enc.EqualsOverrideSignature()

    member _.GetHashCodeOverrideSignature() : BlobBuilder = enc.GetHashCodeOverrideSignature()

    member _.EquatableInterfaceSpec(selfTy: SemType) : EntityHandle = recipes.EquatableInterfaceSpec selfTy

    member _.EqualsTypedSignature(selfTy: SemType) : BlobBuilder = enc.EqualsTypedSignature selfTy

    member _.ComparerDefault(elem: SemType) : EntityHandle = recipes.ComparerDefault elem

    member _.ComparerCompare(elem: SemType) : EntityHandle = recipes.ComparerCompare elem

    member _.ComparableInterfaceSpec(selfTy: SemType) : EntityHandle = recipes.ComparableInterfaceSpec selfTy

    member _.IComparableType: EntityHandle = env.EComparable.Value

    member _.ArgumentExceptionCtor: EntityHandle = env.EArgumentExceptionCtor.Value

    member _.CompareToOverrideSignature() : BlobBuilder = enc.CompareToOverrideSignature()

    member _.CompareToTypedSignature(selfTy: SemType) : BlobBuilder = enc.CompareToTypedSignature selfTy

    interface ICodegenProvider with
        member _.ObjectType = env.EObject.Value
        member _.TypeToken(ty) = recipes.TypeToken(env.Zonk ty)
        member _.DecimalCtor = env.EDecimalCtor.Value
        member _.ExceptionCtor = env.EExceptionCtor.Value

        member _.EqualityComparerDefault(elem) = recipes.EqualityComparerDefault elem

        member _.EqualityComparerGetHashCode(elem) =
            recipes.EqualityComparerGetHashCode elem

        member _.ExternalMemberRef(key, isProperty, isStatic, memberTy) =
            ext.ExternalMemberRef(key, isProperty, isStatic, memberTy)

        member _.ExternalMemberRefOn(key, declTy, isProperty, isStatic, memberTy) =
            ext.ExternalMemberRefOn(key, env.Zonk declTy, isProperty, isStatic, memberTy)

        member _.FSharpCoreDependencies() = env.FSharpCoreDependencies()

        member _.TryEmitCall(compiledName, key, fnTy) =
            if compiledName = "List.fold" then
                ValueSome(recipes.EmitFold(env.Zonk fnTy))
            else
                // Dispatch by SymbolKey identity when Freeze stamped one (M1): only the canonical
                // `Vesper.Printf.printfn` trips the cold-printf recipe, so a user `MyMod.printfn` falls
                // through to the normal external-call path. The name-based fallback only fires on bare
                // `"printfn"` from unkeyed call sites (test mocks).
                let isCanonicalPrintfn =
                    match key with
                    | ValueSome k when PrintfSpec.isCanonicalPrintfn k -> true
                    | _ -> compiledName = "printfn"

                if isCanonicalPrintfn then
                    ValueSome(recipes.EmitPrintfn(env.Zonk fnTy))
                else
                    // General external module-function call (vesper-lib-test-plan Gap 2 Layer D): route
                    // by the Freeze-stamped key to the declaring module (`ns`) + method (`name`), and mint
                    // a `call` (+ `MethodSpec` when generic) to the static method our backend emitted into
                    // the referenced package. Only a module-qualified value key (`ns <> ""`) is a module
                    // function; a bare key (an operator-as-value) is not, and operators are expanded to
                    // `TExpr.ILIntrinsic` by `Emit.lower` before emission anyway. `EmitExternalCall`
                    // returns `ValueNone` when the symbol is unknown to the provider, falling through to
                    // the caller's hard error.
                    match key with
                    | ValueSome(SymbolKey.ValueKey(_, ns, name)) when ns <> "" ->
                        recipes.EmitExternalCall(ns, name, env.Zonk fnTy)
                    | _ -> ValueNone

        member _.TryEmitCtor(className, tyArgs, argTypes) =
            if className = PrintfSpec.printfFormatName then
                ValueSome(recipes.EmitPrintfFormatCtor(List.map env.Zonk tyArgs))
            else
                ext.ExternalCtor(className, List.map env.Zonk tyArgs, List.map env.Zonk argTypes)

        member _.TryEmitUnionCons(key, caseName, tyArgs) =
            let elem () =
                match List.map env.Zonk tyArgs with
                | [ e ] -> e
                | other -> failwithf "ClrProvider: list type expects one type argument, got %A" other

            // The cons recipe is selected by the receiver's nominal `SymbolKey`:
            // FSharp.Core's `list` vs the Vesper cons-list, recognised by key
            // identity rather than by string name.
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
                // the instantiated `TypeSpec` (vesper-lib-test-plan Gap 2 Layer B).
                // The fields are already on the stack in declaration order, so the
                // recipe is a static `call` pushing the one union value back.
                let typeName = ExternalSymbols.qualifiedName key

                match ext.ExternalUnionFactory(typeName, caseName, List.map env.Zonk tyArgs) with
                | ValueSome(handle, argCount) ->
                    ValueSome
                        {
                            Emit = fun il -> il.Encoder.Call handle
                            ArgCount = argCount
                            Pushes = 1
                        }
                | ValueNone -> ValueNone

        member _.UserGenericMemberRef(key, args, kind) =
            let zonkedArgs = List.map env.Zonk args

            match kind with
            | UserMemberKind.UnionMember which -> generics.GenericUnionMemberRef(key, zonkedArgs, which)
            | UserMemberKind.RecordMember which -> generics.GenericRecordMemberRef(key, zonkedArgs, which)
            | UserMemberKind.ClassMember which -> generics.GenericClassMemberRef(key, zonkedArgs, which)

        member _.UserClosureMemberRef(name, args, which) =
            generics.GenericClosureMemberRef(name, List.map env.Zonk args, which)

        member _.TryEmitRecordCons(typeName, tyArgs, _fieldNames) =
            let zonkedArgs = List.map env.Zonk tyArgs

            match ext.ExternalRecordCtor(typeName, zonkedArgs) with
            | ValueNone -> ValueNone
            | ValueSome handle ->
                let argCount =
                    match env.ExternalRecordShape(typeName, List.length zonkedArgs) with
                    | ValueSome(fields, _) -> fields.Length
                    | ValueNone -> 0

                ValueSome { Handle = handle; ArgCount = argCount }

        member _.TryResolveExternalRecordField(typeName, tyArgs, fieldName) =
            ext.ExternalRecordField(typeName, List.map env.Zonk tyArgs, fieldName)

        member _.ExternalUnionTag(unionName, tyArgs, caseName) =
            ext.ExternalUnionTag(unionName, List.map env.Zonk tyArgs, caseName)

        member _.ExternalUnionCaseField(unionName, tyArgs, caseName, fieldIndex) =
            ext.ExternalUnionCaseField(unionName, List.map env.Zonk tyArgs, caseName, fieldIndex)

        member _.StaticFnMethodSpec(handle, instTypes) =
            ext.StaticFnMethodSpec(handle, instTypes)

        member _.TryEmitInvoke(funcTy) =
            match env.Zonk funcTy with
            | TyFun _ as ft -> ValueSome(recipes.EmitInvoke ft)
            | _ -> ValueNone

        member _.TryEmitFSharpFuncInvoke(funcTy) =
            match env.Zonk funcTy with
            | TyFun _ as ft -> ValueSome(recipes.EmitFSharpFuncInvoke ft)
            | _ -> ValueNone

        member _.FormatHandles() = recipes.BuildFormatHandles()

        member _.EncodeLocalSignature(locals) = enc.EncodeLocalSignature locals
