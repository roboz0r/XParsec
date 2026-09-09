namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Vesper
open XParsec.FSharp.SemanticAnalysis

/// A generic closure's registry entry. Every `FrozenType` field embeds the ENCLOSING
/// scopes' `FTTypar(scope, i)`; encoding under `TyparSlots.ClosureClass Frame` re-projects
/// those onto the closure class's own `!slot`.
type internal GenericClosureShape =
    {
        /// The closure class's typars, one slot per enclosing typar.
        Frame: TyparFrame
        CaptureSigs: Block<FrozenType>
        ParamTy: FrozenType
        ResultTy: FrozenType
        DefHandle: EntityHandle
    }

/// One case of a generic user union: `Fields` in declaration order, each a
/// `(metadata field name, declared type)` pair whose typars are `FTTypar(Type _, i)`.
type internal GenericUnionCase =
    {
        Name: string
        Fields: Block<string * FrozenType>
    }

/// A *generic* user union, keyed by its nominal `TypeKey`, which embeds the arity, so the
/// same-named `Choice`2`…`Choice`7` don't collide. Monomorphic unions are not registered;
/// their `Def` tokens suffice.
type internal GenericUnionShape =
    {
        Typars: BlockM<string, typeSlot>
        /// Per case, each logical field's `(FSC-spelled name, declared type)`: a hierarchy
        /// case type's own field rows, and every case factory's parameter types.
        Cases: Block<GenericUnionCase>
        ValueKind: NominalValueKind
        /// A flat regime's physical slots and the `TypeDef` declaring them. `ValueNone` for
        /// a hierarchy regime.
        Home: UnionSlotHome voption
    }

    /// The metadata shape this union is emitted in.
    member this.Regime: UnionRegime =
        UnionRegime.classify
            this.ValueKind
            this.Cases.Length
            (this.Cases |> Block.exists (fun c -> not c.Fields.IsEmpty))

/// One field of a generic user record.
type internal GenericRecordField =
    {
        /// The source name, carried by the property and its accessors.
        Name: string
        /// The `Field` row name of the private backing field.
        MetaName: string
        Ty: FrozenType
    }

/// A *generic* user record: typar names + fields in declaration order.
type internal GenericRecordShape =
    {
        Typars: BlockM<string, typeSlot>
        Fields: Block<GenericRecordField>
    }

/// A *generic* user class. `Fields` is the FULL field shape in order: ctor-param backing
/// fields, then `val`s, then instance-`let` / `static let` backing. The first
/// `CtorParamCount` of them are the primary ctor's parameters; the rest are not ctor args.
type internal GenericClassShape =
    {
        Typars: BlockM<string, typeSlot>
        CtorParamCount: int
        Fields: Block<string * FrozenType>
    }

/// The CLR-only nominals the backend references DIRECTLY. They reach it as an `FTConst` over a bare
/// platform name (what the front end mints for a name the TARGET owns) and are recognised by
/// key identity, so a Vesper type of the same short name cannot false-match.
[<RequireQualifiedAccess>]
module internal ClrSinkKeys =

    /// The printf writer sink (`fprintf`).
    let textWriter: TypeKey = RuntimeNames.platformKey RuntimeNames.textWriterTypeId

    /// The `Vesper.Printf` write-through format handler, a printf recipe's handler local.
    let formatter: TypeKey = RuntimeNames.platformKey RuntimeNames.formatterTypeId

    /// The `System.HashCode` accumulator local of a synthesised `GetHashCode`.
    let hashCode: TypeKey = RuntimeNames.opaqueKey "System.HashCode"

/// A referenced type's placement in its assembly's metadata: the `TypeRef` chain from the
/// namespace-level root type to the type itself. Each segment is a metadata name, arity
/// suffix included; a module segment is the module class's compiled name.
type internal ExternalTypePath =
    {
        Home: SymbolHome
        /// Dotted; the root segment's `TypeRef` namespace column.
        Namespace: string
        Root: string
        /// Outermost first; each is a `TypeRef` scoped by the segment before it.
        Nested: string list
    }

    /// The reflection spelling: `Ns.Root+Inner`.
    member this.FullName: string =
        let chain = String.concat "+" (this.Root :: this.Nested)

        match this.Namespace with
        | "" -> chain
        | ns -> ns + "." + chain

/// Reference identities resolve by SIMPLE NAME: `references` wins; `FSharp.Core` / `System.Runtime`
/// / `System.Console` fall back to the host-loaded copy; the `Vesper.*` are required. Every ref is
/// `lazy`, so a PE whose IL never touches an assembly carries no `AssemblyRef` for it.
type internal ClrEnv
    (
        ctx: MetadataContext,
        bindings: IReadOnlyDictionary<TypeKey, PlatformTypeId>,
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
        lazy (toEntity (ctx.TypeRef(coreRef.Value, ClrTuples.Namespace, ClrTuples.Name)))

    // The open generic tuple structs `System.ValueTuple`1..`8`, cached by arity as bare
    // `TypeRef`s. `1` is in the family because `ValueTuple`1` arises as the `TRest` of
    // a ≥ 8 nesting, never as a user-level 1-tuple.
    let valueTupleEntities = Dictionary<int, EntityHandle>()

    let eValueTupleN (arity: int) : EntityHandle =
        if arity < 1 || arity > ClrTuples.MaxArity then
            failwithf
                "ClrProvider: ValueTuple arity %d is out of range, because only the generic family `ValueTuple`1..`8` exists (≥9 nests via `ValueTuple`8`'s `TRest`)."
                arity

        match valueTupleEntities.TryGetValue arity with
        | true, h -> h
        | _ ->
            let h =
                toEntity (ctx.TypeRef(coreRef.Value, ClrTuples.Namespace, ClrTuples.memberName arity))

            valueTupleEntities.[arity] <- h
            h

    /// A `TypeRef` row for a well-known nominal, spelled from its own `TypeKey`: the
    /// namespace and the arity-suffixed segment name are the key's, so the row the backend
    /// emits and the identity the front end matched cannot denote different types.
    let typeRefOfKey (scope: EntityHandle) (key: TypeKey) : EntityHandle =
        toEntity (ctx.TypeRef(scope, key.Namespace.Dotted, SymbolKeyOps.typeSegmentName key))

    // The function interfaces and the `%A` sinks live in `Vesper.Core`.
    let vesperCoreRef =
        lazy (toEntity (ctx.AssemblyRef(refRequired "Vesper.Core" "a function value needs Vesper.Fun")))

    // No fallback to `vesperCoreRef`: that would mint a wrong `Vesper.Core::List`1`.
    let vesperListRef =
        lazy (toEntity (ctx.AssemblyRef(refRequired "Vesper.List" "a list literal needs Vesper.Collections.List")))

    let eVesperList1 =
        lazy (typeRefOfKey vesperListRef.Value RuntimeNames.vesperListKey)

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

    let eIsByRefLikeAttrCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eIsByRefLikeAttr.Value, ".ctor", s)))

    // `System.Runtime.CompilerServices.IsReadOnlyAttribute` — a consumer reads it as C#'s
    // `readonly struct` and skips defensive copies.
    let eIsReadOnlyAttr =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System.Runtime.CompilerServices", "IsReadOnlyAttribute")))

    let eIsReadOnlyAttrCtor =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eIsReadOnlyAttr.Value, ".ctor", s)))

    // `System.AttributeUsageAttribute::.ctor(System.AttributeTargets)` — the CLR spelling of a
    // `[<AttributeUsage>]` row (`ClrAttributeNames` carries the key mapping).
    let eAttributeUsageAttrCtor =
        lazy
            (let attrRef =
                toEntity (
                    ctx.TypeRef(
                        coreRef.Value,
                        ClrAttributeNames.AttributeUsageNamespace,
                        ClrAttributeNames.AttributeUsageName
                    )
                )

             let targetsRef =
                 toEntity (
                     ctx.TypeRef(
                         coreRef.Value,
                         ClrAttributeNames.AttributeTargetsNamespace,
                         ClrAttributeNames.AttributeTargetsName
                     )
                 )

             let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Type(targetsRef, true))
                 )

             toEntity (ctx.MemberRef(attrRef, ".ctor", s)))

    // `System.ComponentModel.EditorBrowsableAttribute::.ctor(EditorBrowsableState)` —
    // stamped `Never` to withhold a member from IDE completion.
    let eEditorBrowsableAttrCtor =
        lazy
            (let attrRef =
                toEntity (ctx.TypeRef(coreRef.Value, "System.ComponentModel", "EditorBrowsableAttribute"))

             let stateRef =
                 toEntity (ctx.TypeRef(coreRef.Value, "System.ComponentModel", "EditorBrowsableState"))

             let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Type(stateRef, true))
                 )

             toEntity (ctx.MemberRef(attrRef, ".ctor", s)))

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

    // `System.Type`, the type of a reified `typeof<T>` / `typedefof<T>` value.
    let eSystemType = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "Type")))

    let eRuntimeTypeHandle =
        lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "RuntimeTypeHandle")))

    let eTypeGetTypeFromHandle =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = false)
                 .Parameters(
                     1,
                     (fun (ret: ReturnTypeEncoder) -> ret.Type().Type(eSystemType.Value, false)),
                     (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Type(eRuntimeTypeHandle.Value, true))
                 )

             toEntity (ctx.MemberRef(eSystemType.Value, "GetTypeFromHandle", s)))

    /// A parameterless instance member of `System.Type` returning `ret`.
    let typeInstanceGetter (name: string) (ret: ReturnTypeEncoder -> unit) =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = true)
                 .Parameters(0, (fun (r: ReturnTypeEncoder) -> ret r), (fun (_: ParametersEncoder) -> ()))

             toEntity (ctx.MemberRef(eSystemType.Value, name, s)))

    let eTypeIsGenericType =
        typeInstanceGetter "get_IsGenericType" (fun ret -> ret.Type().Boolean())

    let eTypeGetGenericTypeDefinition =
        typeInstanceGetter "GetGenericTypeDefinition" (fun ret -> ret.Type().Type(eSystemType.Value, false))

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

    let eString = lazy (toEntity (ctx.TypeRef(coreRef.Value, "System", "String")))

    // `static (string, string) -> T` on `System.String`.
    let stringBinaryStatic (name: string) (ret: ReturnTypeEncoder -> unit) =
        lazy
            (let s = BlobBuilder()

             BlobEncoder(s)
                 .MethodSignature(isInstanceMethod = false)
                 .Parameters(
                     2,
                     ret,
                     (fun (pars: ParametersEncoder) ->
                         pars.AddParameter().Type().String()
                         pars.AddParameter().Type().String()
                     )
                 )

             toEntity (ctx.MemberRef(eString.Value, name, s)))

    let eStringEquals = stringBinaryStatic "Equals" (fun ret -> ret.Type().Boolean())

    let eStringCompareOrdinal =
        stringBinaryStatic "CompareOrdinal" (fun ret -> ret.Type().Int32())

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

    /// Module-level VALUES homed in this compilation's own assembly, by `ValueKey` → the local
    /// static `FieldDef`. A cross-file read freezes to `External` exactly as a call does, and
    /// is emitted as a `ldsfld` rather than a method ref.
    let localModuleValues = Dictionary<SymbolKey, EntityHandle>()

    /// Project-local `[<Struct>]` value-type keys, so a user struct emits as
    /// `ELEMENT_TYPE_VALUETYPE` rather than `ELEMENT_TYPE_CLASS` in every signature.
    let userValueTypes = System.Collections.Generic.HashSet<TypeKey>()

    let genericUnions = Dictionary<TypeKey, GenericUnionShape>()

    let genericRecords = Dictionary<TypeKey, GenericRecordShape>()

    let genericClasses = Dictionary<TypeKey, GenericClassShape>()
    // Closures have no `SymbolKey` (synthetic names), so they stay string-keyed.
    let genericClosures = Dictionary<string, GenericClosureShape>()

    /// The `GenericParam` row count a `` Name`n `` metadata name carries; `0` without a suffix.
    let arityOfMetaName (name: string) : int<typeSlot> =
        match name.LastIndexOf '`' with
        | i when i >= 0 ->
            match System.Int32.TryParse(name.Substring(i + 1)) with
            | true, n -> TyparIndex.typeSlot n
            | _ -> 0<typeSlot>
        | _ -> 0<typeSlot>

    let externalAsmRef (origin: SymbolHome) : EntityHandle =
        // The CLR emits ONE PE per assembly, so a home refined to its declaring file
        // scopes to the same `AssemblyRef`, and the assembly is all this reads.
        match origin.AssemblyOption with
        | ValueNone ->
            failwith
                "ClrProvider: an external symbol carries no home assembly (project-local symbols are resolved before the provider)."
        | ValueSome home ->
            let simpleName = home.Name

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

    /// The compiled module class's metadata name at `origin`. Each declaration of a module
    /// path emits its own class, so `origin` picks among them.
    let externalModuleMetaName (origin: SymbolOrigin) (m: ModuleKey) : string =
        match
            symbols.DeclarationsOf m
            |> Block.tryFind (fun declaration -> declaration.Home = origin.Home)
        with
        | ValueSome declaration -> CompiledName.Emitted(declaration.Facts.CompiledName, m.Name)
        | ValueNone ->
            failwithf
                "ClrProvider: no referenced surface homed in %A declares module '%s', so the class it emits as is unknown."
                origin.Home
                m.DeclaredPath

    /// The metadata path of module class `m` declared at `origin`, with `nested` beneath it.
    let rec externalModulePath (origin: SymbolOrigin) (m: ModuleKey) (nested: string list) : ExternalTypePath =
        let metaName = externalModuleMetaName origin m

        match m.Container with
        | ModuleContainer.InModule parent -> externalModulePath origin parent (metaName :: nested)
        | ModuleContainer.InNamespace ns ->
            {
                Home = origin.Home
                Namespace = ns.Dotted
                Root = metaName
                Nested = nested
            }

    /// The metadata path of type `t` declared at `origin`, with `nested` beneath it. A
    /// module-held type is nested in the module's class.
    let rec externalTypePathAt (origin: SymbolOrigin) (t: TypeKey) (nested: string list) : ExternalTypePath =
        let segment = SymbolKeyOps.typeSegmentName t

        match t.Container with
        | TypeContainer.InType outer -> externalTypePathAt origin outer (segment :: nested)
        | TypeContainer.InModule m -> externalModulePath origin m (segment :: nested)
        | TypeContainer.InNamespace ns ->
            {
                Home = origin.Home
                Namespace = ns.Dotted
                Root = segment
                Nested = nested
            }

    /// The `TypeRef` rows of `path`: the root scoped by the `AssemblyRef` and carrying the
    /// namespace column, each nested segment scoped by the row before it. A flat
    /// `Outer+Inner` under the `AssemblyRef` scope throws `TypeLoadException`.
    let typeRefOfPath (path: ExternalTypePath) : EntityHandle =
        let root =
            toEntity (ctx.TypeRef(externalAsmRef path.Home, path.Namespace, path.Root))

        path.Nested
        |> List.fold (fun scope segment -> toEntity (ctx.TypeRef(scope, "", segment))) root

    /// The `TypeRef` for an external module's compiled module class (an F# module compiles
    /// to a static class). A key never says WHERE, hence `origin`.
    let externalModuleRef (origin: SymbolOrigin) (m: ModuleKey) : EntityHandle =
        typeRefOfPath (externalModulePath origin m [])

    /// The declaration site of a referenced nominal; `ValueNone` for a shape carrying no
    /// `SymbolOrigin` (an intrinsic, an abbreviation, a measure) and for an undeclared key.
    let externalOrigin (key: TypeKey) : SymbolOrigin voption =
        match symbols.TryLookupType key with
        | ValueSome(ExternalTypeShape.Class info) -> ValueSome info.Origin
        | ValueSome(ExternalTypeShape.Record r) -> ValueSome r.Origin
        | ValueSome(ExternalTypeShape.Union u) -> ValueSome u.Origin
        | ValueSome(ExternalTypeShape.Enum e) -> ValueSome e.Origin
        | ValueSome(ExternalTypeShape.IntrinsicInterface s) -> ValueSome s.Origin
        | _ -> ValueNone

    /// The metadata path of a referenced nominal. A canonically-authored capability
    /// interface (`interface disposable`) has no emitted type of its own and resolves
    /// through its platform interface (`System.IDisposable`).
    let rec externalTypePath (key: TypeKey) : ExternalTypePath voption =
        match symbols.TryLookupType key with
        | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
            externalTypePath (SymbolKeyOps.qualifiedTypeKeyOf platform.Value 0)
        | _ ->
            externalOrigin key
            |> ValueOption.map (fun origin -> externalTypePathAt origin key [])

    /// The `TypeRef` of a referenced class, through its platform interface for a capability
    /// interface. `ValueNone` for a record, union or enum key.
    let rec externalClassRef (key: TypeKey) : EntityHandle voption =
        match symbols.TryLookupType key with
        | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
            externalClassRef (SymbolKeyOps.qualifiedTypeKeyOf platform.Value 0)
        | ValueSome(ExternalTypeShape.Class info) -> ValueSome(typeRefOfPath (externalTypePathAt info.Origin key []))
        | _ -> ValueNone

    /// A self-host `Vesper.Collections.List` and a referenced one share a key, so `userTypes`
    /// membership is what separates `Local` from `Foreign`.
    let classOrigin (key: TypeKey) : ClassOrigin =
        match userTypes.TryGetValue key with
        | true, handle -> ClassOrigin.Local handle
        | _ ->
            match externalClassRef key with
            | ValueSome tref -> ClassOrigin.Foreign tref
            | ValueNone -> ClassOrigin.Unresolved

    /// Referenced-assembly record shape by key + arity.
    let externalRecordShape (key: TypeKey) (arity: int) : (Block<ExternalFieldShape> * SymbolOrigin) voption =
        match symbols.TryLookupType key with
        | ValueSome(ExternalTypeShape.Record r) when int r.TyparArity = arity && r.Origin.Home <> SymbolHome.Unstamped ->
            ValueSome(r.Fields, r.Origin)
        | _ -> ValueNone

    let externalRecordRef (key: TypeKey) (arity: int) : (EntityHandle * Block<ExternalFieldShape>) voption =
        match externalRecordShape key arity with
        | ValueSome(fields, origin) -> ValueSome(typeRefOfPath (externalTypePathAt origin key []), fields)
        | ValueNone -> ValueNone

    /// Referenced-assembly union shape by key + arity, for cross-package case
    /// construction (`Some` / `None`).
    let externalUnionShape (key: TypeKey) (arity: int) : ExternalUnionShape voption =
        match symbols.TryLookupType key with
        | ValueSome(ExternalTypeShape.Union u) when int u.TyparArity = arity && u.Origin.Home <> SymbolHome.Unstamped ->
            ValueSome u
        | _ -> ValueNone

    let externalUnionRef (key: TypeKey) (arity: int) : (EntityHandle * ExternalUnionShape) voption =
        match externalUnionShape key arity with
        | ValueSome u -> ValueSome(typeRefOfPath (externalTypePathAt u.Origin key []), u)
        | ValueNone -> ValueNone

    // The ambient slot resolution, `Declared` until a synthesised owner's emission sets it.
    let mutable typarSlots: TyparSlots = TyparSlots.Declared

    let rec uncurryTy (t: FrozenType) : FrozenType list * FrozenType =
        match t with
        | FTFun(a, b) ->
            let ps, r = uncurryTy b
            a :: ps, r
        | other -> [], other

    member _.Ctx = ctx
    member _.References = references
    member _.Symbols: ICodegenSymbols = symbols

    /// A Vesper primitive's canon `SymbolKey` → its platform type id, single-sourced
    /// from the `.fs` `(# … #)`: this file's OWN intrinsics first, then the dependency
    /// closure's. Keyed by the canon KEY, never by short name.
    member _.TryPrimitiveTypeId(key: TypeKey) : PlatformTypeId voption =
        match bindings.TryGetValue key with
        | true, typeId -> ValueSome typeId
        | _ -> symbols.TryPlatformTypeId key

    member _.CoreRef = coreRef
    member _.VesperRef = vesperRef
    member _.ConsoleRef = consoleRef
    member _.EValueTuple = eValueTuple
    member _.EValueTupleN arity = eValueTupleN arity
    member _.EFun2() = eFun2 ()
    member _.FlatFunEntity(genericArity: int) = flatFunEntity genericArity
    member _.VesperListRef = vesperListRef
    member _.EVesperList1 = eVesperList1
    member _.EObject = eObject
    member _.EValueType = eValueType
    member _.EEnum = eEnum
    member _.EIsByRefLikeAttrCtor = eIsByRefLikeAttrCtor
    member _.EIsReadOnlyAttrCtor = eIsReadOnlyAttrCtor
    member _.EAttributeUsageAttrCtor = eAttributeUsageAttrCtor
    member _.EEditorBrowsableAttrCtor = eEditorBrowsableAttrCtor
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
    member _.EStringEquals = eStringEquals
    member _.EStringCompareOrdinal = eStringCompareOrdinal
    member _.ENotSupportedExceptionCtor = eNotSupportedExceptionCtor
    member _.EDecimalCtor = eDecimalCtor
    member _.EHashCodeToHashCode = eHashCodeToHashCode

    /// The metadata path of a referenced nominal; `ValueNone` for a type this compilation
    /// emits and for an undeclared key.
    member _.TryExternalTypePath(key: TypeKey) : ExternalTypePath voption =
        if userTypes.ContainsKey key then
            ValueNone
        else
            externalTypePath key

    member _.ETypeGetTypeFromHandle = eTypeGetTypeFromHandle
    member _.ETypeIsGenericType = eTypeIsGenericType
    member _.ETypeGetGenericTypeDefinition = eTypeGetGenericTypeDefinition

    member _.UserTypes = userTypes
    member _.LocalModuleFns = localModuleFns
    member _.LocalModuleValues = localModuleValues
    member _.UserValueTypes = userValueTypes
    member _.GenericUnions = genericUnions
    member _.GenericRecords = genericRecords
    member _.GenericClasses = genericClasses
    member _.GenericClosures = genericClosures

    member _.TyparSlots = typarSlots

    /// Run `f` with the ambient slot resolution set to `slots`.
    member _.WithTyparSlots(slots: TyparSlots, f: unit -> 'T) : 'T =
        let saved = typarSlots
        typarSlots <- slots

        try
            f ()
        finally
            typarSlots <- saved

    member _.ArityOfMetaName name = arityOfMetaName name
    member _.UncurryTy t = uncurryTy t

    member _.ExternalAsmRef asm = externalAsmRef asm
    member _.ExternalModuleRef(origin: SymbolOrigin, m: ModuleKey) = externalModuleRef origin m
    member _.ExternalClassRef key = externalClassRef key
    member _.ClassOrigin key = classOrigin key
    member _.LookupTypeByKey key = symbols.TryLookupType key
    /// Drives the `VALUETYPE` vs `CLASS` element tag an encoded type spec carries.
    member _.ExternalIsValueType key = CodegenSymbols.isValueType symbols key

    member _.ExternalLayout key =
        CodegenSymbols.externalLayout symbols key

    member _.ExternalRecordShape(key, arity) = externalRecordShape key arity
    member _.ExternalRecordRef(key, arity) = externalRecordRef key arity
    member _.ExternalUnionShape(key, arity) = externalUnionShape key arity
    member _.ExternalUnionRef(key, arity) = externalUnionRef key arity
