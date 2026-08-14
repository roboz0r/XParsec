namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// A generic closure's registry entry. Every `FrozenType` field embeds the ENCLOSING method's
/// `FTTypar(Method, i)`; encoding under a closure-typar scope re-projects those onto the
/// closure class's own `!i`.
type internal GenericClosureShape =
    {
        TyparCount: int
        /// Offset of the closure's own typars: the enclosing class's occupy the first
        /// `DeclaringTypars` slots. `0` for a static-fn closure.
        DeclaringTypars: int
        CaptureSigs: FrozenType list
        ParamTy: FrozenType
        ResultTy: FrozenType
        DefHandle: EntityHandle
    }

/// One case of a generic user union: `Fields` in declaration order, each a
/// `(metadata field name, declared type)` pair whose typars are `FTTypar(Declaring, i)`.
type internal GenericUnionCase =
    {
        Name: string
        Fields: EqArray<string * FrozenType>
    }

/// A *generic* user union, keyed by its nominal `TypeKey`, which embeds the arity, so the
/// same-named `Choice`2`…`Choice`7` don't collide. Monomorphic unions are not registered;
/// their `Def` tokens suffice.
type internal GenericUnionShape =
    {
        Typars: EqArray<string>
        Cases: EqArray<GenericUnionCase>
    }

/// A *generic* user record: typar names + `(field name, declared type)` pairs.
type internal GenericRecordShape =
    {
        Typars: EqArray<string>
        Fields: EqArray<string * FrozenType>
    }

/// A *generic* user class. `Fields` is the FULL field shape in order: ctor-param backing
/// fields, then `val`s, then instance-`let` / `static let` backing. The first
/// `CtorParamCount` of them are the primary ctor's parameters; the rest are not ctor args.
type internal GenericClassShape =
    {
        Typars: EqArray<string>
        CtorParamCount: int
        Fields: EqArray<string * FrozenType>
    }

/// The CLR-only nominals the backend names DIRECTLY. They reach it as an `FTConst` over a bare
/// platform name (what the front end mints for a name the TARGET owns) and are recognised by
/// key identity, so a Vesper type of the same short name cannot false-match.
[<RequireQualifiedAccess>]
module internal ClrSinkKeys =

    /// The printf writer sink (`fprintf`).
    let textWriter: SymbolKey = RuntimeNames.opaqueKey RuntimeNames.textWriterTypeName

    /// The `Vesper.Printf` write-through format handler, a printf recipe's handler local.
    let formatter: SymbolKey = RuntimeNames.opaqueKey RuntimeNames.formatterTypeName

    /// The `System.HashCode` accumulator local of a synthesised `GetHashCode`.
    let hashCode: SymbolKey = RuntimeNames.opaqueKey "System.HashCode"

/// Reference identities resolve by SIMPLE NAME: `references` wins; `FSharp.Core` / `System.Runtime`
/// / `System.Console` fall back to the host-loaded copy; the `Vesper.*` are required. Every ref is
/// `lazy`, so a PE whose IL never touches an assembly carries no `AssemblyRef` for it.
type internal ClrEnv
    (
        ctx: MetadataContext,
        reprs: IReadOnlyDictionary<SymbolKey, string>,
        references: Map<string, System.Reflection.AssemblyName>,
        symbols: ICodegenSymbols
    ) =

    let refOrHost
        (simpleName: string)
        (hostFallback: unit -> System.Reflection.AssemblyName)
        : System.Reflection.AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None -> hostFallback ()

    let refRequired (simpleName: string) (need: string) : System.Reflection.AssemblyName =
        match references.TryFind simpleName with
        | Some an -> an
        | None ->
            failwithf
                "ClrProvider: %s, but no %s assembly is referenced (add its path to ProjectInfo.References)."
                need
                simpleName

    let fsCoreRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(
                    refOrHost "FSharp.Core" (fun () -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.GetName())
                )
            ))

    // The BCL identity every emitted assembly needs, sourced from the referenced `System.Runtime`
    // ref pack so the `AssemblyRef` is the REFERENCE identity, not the host
    // `System.Private.CoreLib`, to which the ref assembly type-forwards at run time.
    let coreRef =
        lazy
            (toEntity (ctx.AssemblyRef(refOrHost "System.Runtime" (fun () -> typeof<System.Object>.Assembly.GetName()))))

    // `lazy`, so only a program that forces `eFormatter` (`Vesper.Formatter`) needs a
    // `Vesper.Printf` reference; a printf-free program adds no `AssemblyRef` for it.
    let vesperRef =
        lazy
            (toEntity (ctx.AssemblyRef(refRequired "Vesper.Printf" "a printf / %A format call needs Vesper.Formatter")))

    let consoleRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refOrHost "System.Console" (fun () -> typeof<System.Console>.Assembly.GetName()))
            ))

    // `unit` — the zero-field `System.ValueTuple` struct that `prim-types-min.clr.fs` binds it
    // to. No `FSharp.Core.Unit` is referenced.
    let eValueTuple =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ValueTuple")))

    // The open generic tuple structs `System.ValueTuple`1..`8`, cached by arity as bare
    // `TypeRef`s. `1` is in the family because `ValueTuple`1` arises as the `TRest` of
    // a ≥ 8 nesting, never as a user-level 1-tuple.
    let valueTupleEntities = Dictionary<int, EntityHandle>()

    let eValueTupleN (arity: int) : EntityHandle =
        if arity < 1 || arity > 8 then
            failwithf
                "ClrProvider: ValueTuple arity %d is out of range — only the generic family `ValueTuple`1..`8` exists (≥9 nests via `ValueTuple`8`'s `TRest`)."
                arity

        match valueTupleEntities.TryGetValue arity with
        | true, h -> h
        | _ ->
            let h =
                toEntity (ctx.TypeRef(coreRef.Value, "System", sprintf "ValueTuple`%d" arity))

            valueTupleEntities.[arity] <- h
            h

    /// A `TypeRef` row for a well-known nominal, spelled from its own `TypeKey`: the
    /// namespace and the arity-suffixed segment name are the key's, so the row the backend
    /// emits and the identity the front end matched cannot name different types.
    let typeRefOfKey (scope: EntityHandle) (key: TypeKey) : EntityHandle =
        toEntity (ctx.TypeRef(scope, key.Namespace.Dotted, SymbolKeyOps.typeSegmentName key))

    let ePrintfFormat4 =
        lazy (typeRefOfKey fsCoreRef.Value RuntimeNames.printfFormatKey)

    // The function interfaces and the `%A` sinks live in `Vesper.Core`, not FSharp.Core.
    let vesperCoreRef =
        lazy (toEntity (ctx.AssemblyRef(refRequired "Vesper.Core" "a function value needs Vesper.Fun")))

    // No fallback to `vesperCoreRef`: that would mint a wrong `Vesper.Core::List`1`.
    let vesperListRef =
        lazy
            (toEntity (
                ctx.AssemblyRef(refRequired "Vesper.List" "a list literal / List.fold needs Vesper.Collections.List")
            ))

    let eFSharpList1 =
        lazy (toEntity (ctx.TypeRef(fsCoreRef.Value, "Microsoft.FSharp.Collections", "FSharpList`1")))

    let eVesperList1 =
        lazy (typeRefOfKey vesperListRef.Value RuntimeNames.vesperListKey)

    let eListModule =
        lazy (toEntity (ctx.TypeRef(vesperListRef.Value, "Vesper.Collections", "ListModule")))

    let eObject = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Object")))

    // `System.ValueType` — the IL base type of every `[<Struct>]` value type
    let eValueType = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ValueType")))

    // `System.Enum` — the IL base type of a numeric enum's `TypeDefinition`. The CLR reads the
    // base chain to mark the type `IsEnum`, and value-type via `Enum`'s own `System.ValueType`.
    let eEnum = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Enum")))

    // `System.Runtime.CompilerServices.IsByRefLikeAttribute` — stamped on a
    // `[<IsByRefLike>]` value type so the CLR confines it to the stack.
    let eIsByRefLikeAttr =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Runtime.CompilerServices", "IsByRefLikeAttribute")))

    // Its parameterless `.ctor`, the constructor a `CustomAttribute` row names.
    let eIsByRefLikeAttrCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eIsByRefLikeAttr.Value, ".ctor", s)))

    let eTextWriter =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.IO", "TextWriter")))

    // `System.Text.StringBuilder` — the `bprintf` write-through sink. In the ref pack it lives
    // in the same core assembly as `TextWriter` (`System.Runtime`), hence `coreRef`.
    let eStringBuilder =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Text", "StringBuilder")))

    let eConsole = lazy (toEntity (ctx.TypeRef(consoleRef.Value, "System", "Console")))

    let eFormatter =
        lazy (toEntity (ctx.TypeRef(vesperRef.Value, "Vesper", "Formatter")))

    let eDecimal = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Decimal")))

    let eException = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Exception")))

    let eEqualityComparer1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Collections.Generic", "EqualityComparer`1")))

    let eHashCode = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "HashCode")))

    let eEquatable1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IEquatable`1")))

    let eComparer1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Collections.Generic", "Comparer`1")))

    let eComparable1 =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IComparable`1")))

    let eComparable =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "IComparable")))

    let eArgumentException =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "ArgumentException")))

    let eObjectCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eObject.Value, ".ctor", s)))

    let eExceptionCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                 )

             toEntity (ctx.MemberRef(eException.Value, ".ctor", s)))

    let eArgumentExceptionCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                 )

             toEntity (ctx.MemberRef(eArgumentException.Value, ".ctor", s)))

    // `System.NotSupportedException` — thrown by the synthesised `IEnumerator.Reset` co-slot,
    // on the parameterless ctor so the BCL supplies the message.
    let eNotSupportedException =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "NotSupportedException")))

    let eNotSupportedExceptionCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eNotSupportedException.Value, ".ctor", s)))

    let eDecimalCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     5,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) ->
                         pars.AddParameter().Type().Int32()
                         pars.AddParameter().Type().Int32()
                         pars.AddParameter().Type().Int32()
                         pars.AddParameter().Type().Boolean()
                         pars.AddParameter().Type().Byte()
                     )
                 )

             toEntity (ctx.MemberRef(eDecimal.Value, ".ctor", s)))

    let eHashCodeToHashCode =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     0,
                     (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                     (fun (_: ParametersEncoder) -> ())
                 )

             toEntity (ctx.MemberRef(eHashCode.Value, "ToHashCode", s)))

    /// Each distinct FSharp.Core construct the emission references, so a build can tell
    /// *positively* whether the PE depends on `FSharp.Core.dll`, and what pins it.
    let fsharpCoreDeps = HashSet<string>()
    let markFSharpCoreDep (construct: string) : unit = fsharpCoreDeps.Add construct |> ignore

    /// User types emitted into *this* assembly, by their nominal `TypeKey` →
    /// predicted `TypeDefinition` handle, so a field / factory / local signature
    /// can reference the type before its row is added.
    let userTypes = Dictionary<TypeKey, EntityHandle>()

    // A `Vesper.Core`-owned interface, LOCAL-FIRST: when THIS compilation IS `Vesper.Core` the
    // interface is one of its own `TypeDef`s, and an `AssemblyRef` to itself is rejected. A
    // function, not a `lazy`, because `userTypes` fills per file and a forced value would go stale.
    let coreInterfaceEntity (key: TypeKey) : EntityHandle =
        match userTypes.TryGetValue key with
        | true, h -> h
        | _ -> toEntity (ctx.TypeRef(vesperCoreRef.Value, key.Namespace.Dotted, SymbolKeyOps.typeSegmentName key))

    // The `%A` structural-format interfaces, owned by `Vesper.Core`, so a record-bearing program
    // links only `Vesper.Core` and never `Vesper.Printf`. `IStructuralFormattable` is the
    // `InterfaceImpl` a synthesised record/DU declares; `IFormatSink` is its `Format` param type.
    let eStructuralFormattable () =
        coreInterfaceEntity RuntimeNames.structuralFormattableKey

    let eFormatSink () =
        coreInterfaceEntity RuntimeNames.formatSinkKey

    // `Vesper.Fun`2<a,b>` — the CURRIED function interface: the type of every function
    // value, and the interface a synthesised closure implements.
    let eFun2 () =
        coreInterfaceEntity (RuntimeNames.vesperFunKey 2)

    // The FLAT function interfaces: one `Invoke(a,…)` with no intermediate `Fun`2`. Same
    // `Vesper.Fun` name, overloaded by GENERIC arity (type args = flat param count + 1), so a
    // flat closure of param-arity N implements `Fun`(N+1)`; `Fun`2` above is the curried one.
    let flatFunEntity (genericArity: int) : EntityHandle =
        match genericArity with
        | 3
        | 4
        | 5 -> coreInterfaceEntity (RuntimeNames.vesperFunKey genericArity)
        | n -> failwithf "ClrEnv: no flat Fun interface for generic arity %d" n

    /// Module-level functions homed in *this* compilation's own assembly, by `ValueKey` → local
    /// `MethodDef`. A cross-file module-function call freezes to `External`, which would otherwise
    /// mint an `AssemblyRef`-scoped `MemberRef` back into the very assembly being emitted.
    let localModuleFns = Dictionary<SymbolKey, EntityHandle>()

    /// Project-local `[<Struct>]` value-type keys, so a user struct emits as
    /// `ELEMENT_TYPE_VALUETYPE` rather than `ELEMENT_TYPE_CLASS` in every signature.
    let userValueTypes = System.Collections.Generic.HashSet<TypeKey>()

    let genericUnions = Dictionary<TypeKey, GenericUnionShape>()

    let genericRecords = Dictionary<TypeKey, GenericRecordShape>()

    let genericClasses = Dictionary<TypeKey, GenericClassShape>()
    // Closures have no `SymbolKey` (synthetic names), so they stay string-keyed.
    let genericClosures = Dictionary<string, GenericClosureShape>()

    let arityOfMetaName (name: string) : int =
        match name.LastIndexOf '`' with
        | i when i >= 0 ->
            match System.Int32.TryParse(name.Substring(i + 1)) with
            | true, n -> n
            | _ -> 0
        | _ -> 0

    let externalAsmRef (origin: Origin) : EntityHandle =
        // The CLR emits ONE PE per assembly, so a home refined to its declaring file
        // scopes to the same `AssemblyRef`, and the assembly is all this reads.
        match origin.AssemblyOption with
        | ValueNone ->
            failwith
                "ClrProvider: an external symbol carries no home assembly (project-local symbols are resolved before the provider)."
        | ValueSome simpleName ->
            let an =
                match references.TryFind simpleName with
                | Some an -> an
                | None ->
                    match
                        System.AppDomain.CurrentDomain.GetAssemblies()
                        |> Array.tryFind (fun a -> a.GetName().Name = simpleName)
                    with
                    | Some a -> a.GetName()
                    | None -> System.Reflection.AssemblyName(simpleName)

            toEntity (ctx.AssemblyRef an)

    // A provider may key a generic type bare (`Vesper.Option`, contract layer) or arity-suffixed
    // (`Vesper.Option`1`, metadata layer); this reconciles the two registration conventions.
    let lookupTypeByKey (key: SymbolKey) : ExternalTypeShape voption =
        CodegenSymbols.lookupTypeByKey symbols key

    let lookupClassShape (key: SymbolKey) : ExternalClassShape voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Class info) -> ValueSome info
        | _ -> ValueNone

    /// The `TypeRef` for an external module's compiled module class (an F# module compiles to a
    /// static class): a nested module chains through its parent's `TypeRef` with the bare name +
    /// empty namespace; only the chain's root carries one. A key never says WHERE, hence `origin`.
    let rec externalModuleRef (origin: SymbolOrigin) (m: ModuleKey) : EntityHandle =
        match m.Container with
        | ModuleContainer.InModule parent -> toEntity (ctx.TypeRef(externalModuleRef origin parent, "", m.Name))
        | ModuleContainer.InNamespace ns -> toEntity (ctx.TypeRef(externalAsmRef origin.Home, ns.Dotted, m.Name))

    let rec externalClassRef (key: SymbolKey) : EntityHandle voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
            // A canonically-authored capability interface (`interface disposable`) has no emitted
            // type of its own, so re-resolve through its platform interface and the `InterfaceImpl`
            // binds the real BCL one (`System.IDisposable`). That shape is a plain `Class`: one hop.
            externalClassRef (SymbolKeyOps.qualifiedTypeKey platform 0)
        | _ ->

            match key, lookupClassShape key with
            | SymbolKey.Type t, ValueSome info ->
                let asm = externalAsmRef info.Origin.Home

                // A nested type (`List`1+Enumerator`) chains through the enclosing type's `TypeRef`
                // as ResolutionScope, with its OWN bare name + `` `N `` and no namespace. A flat
                // `Outer+Inner` under the `AssemblyRef` scope throws `TypeLoadException`.
                let rec typeRefOf (t: TypeKey) : EntityHandle =
                    match t.Container with
                    | TypeContainer.InType outer ->
                        toEntity (ctx.TypeRef(typeRefOf outer, "", SymbolKeyOps.typeSegmentName t))
                    | TypeContainer.InNamespace ns ->
                        toEntity (ctx.TypeRef(asm, ns.Dotted, SymbolKeyOps.typeSegmentName t))
                    | TypeContainer.InModule m ->
                        // A module-held type compiles NESTED in the module's class, so a
                        // bare namespace-scoped ref would drop `m` and fail to bind.
                        toEntity (ctx.TypeRef(externalModuleRef info.Origin m, "", SymbolKeyOps.typeSegmentName t))

                ValueSome(typeRefOf t)
            | _ -> ValueNone

    /// Referenced-assembly record shape by `SymbolKey` + arity.
    let externalRecordShape (key: SymbolKey) (arity: int) : (EqArray<ExternalFieldShape> * SymbolOrigin) voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Record(a, fields, origin, _)) when a = arity && origin.Home <> Origin.Unstamped ->
            ValueSome(fields, origin)
        | _ -> ValueNone

    let externalRecordRef (key: SymbolKey) (arity: int) : (EntityHandle * EqArray<ExternalFieldShape>) voption =
        // Any non-type key gives `ValueNone`, never a fabricated
        // `(ns = "", name = <whole dotted name>)` ref that only fails at load.
        match key, externalRecordShape key arity with
        | SymbolKey.Type t, ValueSome(fields, origin) ->
            // A record is never a CLR nested type, so the namespace + `` `n ``-suffixed name come
            // straight off the key's own containment chain.
            let simple = SymbolKeyOps.typeSegmentName t

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Home, t.Namespace.Dotted, simple)), fields)
        | _ -> ValueNone

    /// Referenced-assembly union shape by `SymbolKey` + arity, for cross-package case
    /// construction (`Some` / `None`).
    let externalUnionShape (key: SymbolKey) (arity: int) : (EqArray<ExternalCaseShape> * SymbolOrigin) voption =
        match lookupTypeByKey key with
        | ValueSome(ExternalTypeShape.Union(a, cases, _, origin)) when a = arity && origin.Home <> Origin.Unstamped ->
            ValueSome(cases, origin)
        | _ -> ValueNone

    let externalUnionRef (key: SymbolKey) (arity: int) : (EntityHandle * EqArray<ExternalCaseShape>) voption =
        match key, externalUnionShape key arity with
        | SymbolKey.Type t, ValueSome(cases, origin) ->
            let simple = SymbolKeyOps.typeSegmentName t

            ValueSome(toEntity (ctx.TypeRef(externalAsmRef origin.Home, t.Namespace.Dotted, simple)), cases)
        | _ -> ValueNone

    // `ValueNone` ⇒ off: a `FTTypar(Method, i)` encodes to the method's own `!!i`. `ValueSome d`
    // ⇒ inside a closure's own emission, where the enclosing class's typars hold the closure's
    // first `d` slots, so `FTTypar(Method, j)` lands at `!(d + j)` (a static-fn closure has d = 0).
    let mutable closureTyparScope: int voption = ValueNone

    let rec uncurryTy (t: FrozenType) : FrozenType list * FrozenType =
        match t with
        | FTFun(a, b) ->
            let ps, r = uncurryTy b
            a :: ps, r
        | other -> [], other

    member _.Ctx = ctx
    member _.References = references
    member _.Symbols: ICodegenSymbols = symbols

    /// A Vesper primitive's canon `SymbolKey` → its IL representation string, single-sourced
    /// from the `.fs` `(# … #)`: this file's OWN intrinsics first, then the dependency
    /// closure's. Keyed by the canon KEY, never by short name.
    member _.TryPrimitiveRepr(key: SymbolKey) : string option =
        match reprs.TryGetValue key with
        | true, repr -> Some repr
        | _ ->
            match symbols.TryPlatformRepr key with
            | ValueSome repr -> Some repr
            | ValueNone -> None

    member _.FsCoreRef = fsCoreRef
    member _.CoreRef = coreRef
    member _.VesperRef = vesperRef
    member _.ConsoleRef = consoleRef
    member _.EValueTuple = eValueTuple
    member _.EValueTupleN arity = eValueTupleN arity
    member _.EPrintfFormat4 = ePrintfFormat4
    member _.EFun2() = eFun2 ()
    member _.FlatFunEntity(genericArity: int) = flatFunEntity genericArity
    member _.VesperListRef = vesperListRef
    member _.EFSharpList1 = eFSharpList1
    member _.EVesperList1 = eVesperList1
    member _.EListModule = eListModule
    member _.EObject = eObject
    member _.EValueType = eValueType
    member _.EEnum = eEnum
    member _.EIsByRefLikeAttrCtor = eIsByRefLikeAttrCtor
    member _.ETextWriter = eTextWriter
    member _.EStringBuilder = eStringBuilder
    member _.EConsole = eConsole
    member _.EFormatter = eFormatter
    member _.EStructuralFormattable() = eStructuralFormattable ()
    member _.EFormatSink() = eFormatSink ()
    member _.EException = eException
    member _.EEqualityComparer1 = eEqualityComparer1
    member _.EHashCode = eHashCode
    member _.EEquatable1 = eEquatable1
    member _.EComparer1 = eComparer1
    member _.EComparable1 = eComparable1
    member _.EComparable = eComparable
    member _.EArgumentException = eArgumentException
    member _.EObjectCtor = eObjectCtor
    member _.EExceptionCtor = eExceptionCtor
    member _.EArgumentExceptionCtor = eArgumentExceptionCtor
    member _.ENotSupportedExceptionCtor = eNotSupportedExceptionCtor
    member _.EDecimalCtor = eDecimalCtor
    member _.EHashCodeToHashCode = eHashCodeToHashCode

    member _.UserTypes = userTypes
    member _.LocalModuleFns = localModuleFns
    member _.UserValueTypes = userValueTypes
    member _.GenericUnions = genericUnions
    member _.GenericRecords = genericRecords
    member _.GenericClasses = genericClasses
    member _.GenericClosures = genericClosures

    member _.MarkFSharpCoreDep construct = markFSharpCoreDep construct

    member _.FSharpCoreDependencies() =
        fsharpCoreDeps |> List.ofSeq |> List.sort

    member _.ClosureTyparScope
        with get () = closureTyparScope
        and set v = closureTyparScope <- v

    member _.ArityOfMetaName name = arityOfMetaName name
    member _.UncurryTy t = uncurryTy t

    member _.ExternalAsmRef asm = externalAsmRef asm
    member _.ExternalModuleRef(origin: SymbolOrigin, m: ModuleKey) = externalModuleRef origin m
    member _.ExternalClassRef key = externalClassRef key
    member _.LookupTypeByKey key = lookupTypeByKey key
    /// Drives the `VALUETYPE` vs `CLASS` element tag an encoded type spec carries.
    member _.ExternalIsValueType key = CodegenSymbols.isValueType symbols key

    member _.ExternalLayout key =
        CodegenSymbols.externalLayout symbols key

    member _.ExternalRecordShape(key, arity) = externalRecordShape key arity
    member _.ExternalRecordRef(key, arity) = externalRecordRef key arity
    member _.ExternalUnionShape(key, arity) = externalUnionShape key arity
    member _.ExternalUnionRef(key, arity) = externalUnionRef key arity
