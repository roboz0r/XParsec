namespace XParsec.FSharp.Codegen.Clr

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis

// Resolves BCL types to `ExternalTypeShape` and their members to `FrozenType`
// signature templates via `System.Reflection.MetadataLoadContext`.

/// `System.Type` → `FrozenType` template mapping, over the declaring type's generic
/// parameters (`FTTypar(Declaring,i)`; method-owned ones as `FTTypar(Method,j)`).
/// A shape that doesn't map yields `None`.
module private MetadataMapping =

    /// `intrinsics` reconciles a platform repr to its canon (`"System.Int32"` → `int`), so a
    /// BCL member's `System.Int32` parameter presents as `int` and is callable. A name it
    /// does not reconcile is a real class.
    let rec tryBuildType (intrinsics: IntrinsicTypeMap) (t: Type) : FrozenType option =
        let go = tryBuildType intrinsics

        if t.IsByRef then
            // `in`/`out`/`ref` all collapse to `T&`. A C# `in` param's
            // `modreq(InAttribute)` is dropped, so calling one fails CLR member-ref binding.
            match go (t.GetElementType()) with
            | Some elem -> Some(FTConst(RuntimeNames.byrefKey, EqArray.singleton elem))
            | None -> None
        elif t.IsPointer then
            None
        elif t.IsArray then
            match go (t.GetElementType()) with
            | Some elem -> Some(FTConst(RuntimeNames.arrayKey (t.GetArrayRank()), EqArray.singleton elem))
            | None -> None
        elif t.IsGenericParameter then
            let pos = t.GenericParameterPosition

            if isNull t.DeclaringMethod then
                Some(FTTypar(TyparAxis.Declaring, pos))
            else
                Some(FTTypar(TyparAxis.Method, pos))
        elif t.IsGenericType then
            // Open generic has null `FullName`; this branch must precede the `FullName` match.
            let name = t.GetGenericTypeDefinition().FullName
            let args = t.GetGenericArguments() |> Array.map go

            if Array.exists Option.isNone args then
                None
            else
                let frozen = args |> Array.map Option.get

                let key = SymbolKeyOps.qualifiedTypeKeyOf name frozen.Length

                Some(FTClass(key, EqArray.ofArray frozen))
        else
            match t.FullName with
            | null -> None // constructed/exotic type with no metadata full name
            | "System.Void" -> Some(FTConst(RuntimeNames.unitKey, EqArray.empty))
            // A BCL name with a canon surfaces AS the canon: scalars
            // (`System.Int32` → `int`) and subtype roots (`System.Object` → `obj`,
            // `System.Exception` → `exn`) alike. Anything else stays a nominal `FTClass`.
            | fullName ->
                match IntrinsicTypeMap.tryCanon fullName intrinsics with
                | ValueSome canon -> Some(FTConst(canon, EqArray.empty))
                | ValueNone -> Some(FTClass(SymbolKeyOps.qualifiedTypeKeyOf fullName 0, EqArray.empty))

    /// `(per-parameter templates, return)` for a method; `None` if any type doesn't map.
    /// UNCOLLAPSED: one entry per value parameter, so `.Length` is the value arity and the
    /// array serves as a member key's structural `ArgSig` directly.
    let tryMethodSignature (intrinsics: IntrinsicTypeMap) (m: MethodInfo) : (FrozenType[] * FrozenType) option =
        let paramTys =
            m.GetParameters()
            |> Array.map (fun p -> tryBuildType intrinsics p.ParameterType)

        let retTy = tryBuildType intrinsics m.ReturnType

        if retTy.IsNone || Array.exists Option.isNone paramTys then
            None
        else
            Some(paramTys |> Array.map Option.get, retTy.Value)

    /// Method-axis generic-parameter count; `0` for a non-generic method.
    let methodTyparArityOf (m: MethodInfo) : int =
        if m.IsGenericMethodDefinition then
            m.GetGenericArguments().Length
        else
            0

    /// The `IntKind` a BCL integral primitive's name denotes. `System.IntPtr` /
    /// `System.UIntPtr` are absent: neither a parameter default nor a metadata `Constant`
    /// row can carry one.
    let private intKindOfClrName (fullName: string) : IntKind voption =
        match fullName with
        | "System.SByte" -> ValueSome IntKind.SByte
        | "System.Byte" -> ValueSome IntKind.Byte
        | "System.Int16" -> ValueSome IntKind.Int16
        | "System.UInt16" -> ValueSome IntKind.UInt16
        | "System.Int32" -> ValueSome IntKind.Int32
        | "System.UInt32" -> ValueSome IntKind.UInt32
        | "System.Int64" -> ValueSome IntKind.Int64
        | "System.UInt64" -> ValueSome IntKind.UInt64
        | _ -> ValueNone

    /// `default(T)` as a `TConstValue` for primitive value types; `None` otherwise.
    let private zeroOfValueType (t: Type) : TConstValue option =
        if not t.IsValueType then
            None
        else
            match intKindOfClrName t.FullName with
            | ValueSome k -> Some(TConstValue.Integral(k, 0L))
            | ValueNone ->
                match t.FullName with
                | "System.Boolean" -> Some(TConstValue.Bool false)
                | "System.Char" -> Some(TConstValue.Char '\000')
                | "System.Single" -> Some(TConstValue.Float32 0.0f)
                | "System.Double" -> Some(TConstValue.Float 0.0)
                | _ -> None

    /// Boxed `RawDefaultValue` → `TConstValue`. The box's runtime type is the only kind
    /// witness metadata gives, so each arm spells its own `IntKind`; widening to `bits`
    /// sign-extends the signed cases, zero-extends the unsigned (`uint64` reinterprets).
    let private constOfBoxed (v: obj) : TConstValue option =
        let inline integral (k: IntKind) (bits: int64) = Some(TConstValue.Integral(k, bits))

        match v with
        | :? bool as b -> Some(TConstValue.Bool b)
        | :? char as c -> Some(TConstValue.Char c)
        | :? sbyte as n -> integral IntKind.SByte (int64 n)
        | :? byte as n -> integral IntKind.Byte (int64 n)
        | :? int16 as n -> integral IntKind.Int16 (int64 n)
        | :? uint16 as n -> integral IntKind.UInt16 (int64 n)
        | :? int as n -> integral IntKind.Int32 (int64 n)
        | :? uint32 as n -> integral IntKind.UInt32 (int64 n)
        | :? int64 as n -> integral IntKind.Int64 n
        | :? uint64 as n -> integral IntKind.UInt64 (int64 n)
        | :? single as f -> Some(TConstValue.Float32 f)
        | :? double as f -> Some(TConstValue.Float f)
        | :? string as s -> Some(TConstValue.String s)
        | _ -> None

    /// A `[<Literal>]` / `const` field's declared value. `None` where the value maps to no
    /// `TConstValue` (a `null` reference constant, an unmapped kind).
    let tryLiteralValue (f: FieldInfo) : TConstValue option =
        match
            (try
                Some(f.GetRawConstantValue())
             with _ ->
                 None)
        with
        | Some v when not (isNull v) -> constOfBoxed v
        | _ -> None

    /// Trailing omittable parameter defaults in declaration order. Walks from the end;
    /// stops at the first non-optional or non-representable-constant parameter.
    let optionalDefaults (ps: ParameterInfo[]) : OptionalDefault list =
        let tryConstOf (p: ParameterInfo) : TConstValue option =
            if not p.IsOptional then
                None
            elif p.HasDefaultValue then
                match
                    (try
                        Some p.RawDefaultValue
                     with _ ->
                         None)
                with
                | Some v when not (isNull v) -> constOfBoxed v
                | _ -> None // a `null` (reference-type) default isn't a `TConstValue`
            else
                zeroOfValueType p.ParameterType

        let mutable acc = []
        let mutable i = ps.Length - 1
        let mutable go = true

        while go && i >= 0 do
            match tryConstOf ps.[i] with
            | Some c ->
                acc <- OptionalDefault.Const c :: acc
                i <- i - 1
            | None -> go <- false

        acc

    /// Property signature: value type only (no `->`). `Storage = Property` on the member.
    let tryPropertySignature (intrinsics: IntrinsicTypeMap) (p: PropertyInfo) : FrozenType option =
        tryBuildType intrinsics p.PropertyType

    /// Constructor as `(params) → declType`. Zero-param ctor reads as `unit → declType`.
    /// `None` if any type doesn't map. Surfaced as member `".ctor"`.
    let tryCtorSignature (intrinsics: IntrinsicTypeMap) (c: ConstructorInfo) : (FrozenType[] * FrozenType) option =
        let paramTys =
            c.GetParameters()
            |> Array.map (fun p -> tryBuildType intrinsics p.ParameterType)

        let retTy = tryBuildType intrinsics c.DeclaringType

        if retTy.IsNone || Array.exists Option.isNone paramTys then
            None
        else
            Some(paramTys |> Array.map Option.get, retTy.Value)

    /// Property `ExternalSignature`: no argument group, value type in `Return`.
    let propertySignature (declaringTyparArity: int) (valueTy: FrozenType) : ExternalSignature =
        ExternalSignature.value (declaringTyparArity, 0, valueTy)

    /// Method/ctor `ExternalSignature` from its `(Parameters, Return)` templates.
    let methodSignature
        (declaringTyparArity: int)
        (methodTyparArity: int)
        (parameters: FrozenType, ret: FrozenType)
        : ExternalSignature =
        ExternalSignature.make (declaringTyparArity, methodTyparArity, parameters, ret)

    /// The declaring type's `TypeKey`, built by recursion through `Type.DeclaringType`,
    /// not by cutting `FullName` on `.` and `+`. `Type.Name` is the innermost METADATA
    /// segment (bare name plus its own `` `N ``), which `typeKeyOfSegment` parses.
    let rec declTypeKey (t: Type) : TypeKey =
        let t =
            if t.IsGenericType && not t.IsGenericTypeDefinition then
                t.GetGenericTypeDefinition()
            else
                t

        let container =
            if t.IsNested then
                TypeContainer.InType(declTypeKey t.DeclaringType)
            else
                let ns = if isNull t.Namespace then "" else t.Namespace
                TypeContainer.InNamespace(SymbolKeyOps.namespaceKey ns)

        SymbolKeyOps.typeKeyOfSegment container t.Name

/// `IExternalSymbolProvider` over reference-assembly paths, sharing one
/// `MetadataLoadContext`. `intrinsics` is empty when no Vesper.Core is in scope, so
/// BCL primitives stay nominal classes.
type MetadataSymbolProvider(intrinsics: IntrinsicTypeMap, assemblyPaths: string seq) =
    let paths = Seq.toArray assemblyPaths
    let mlc = new MetadataLoadContext(PathAssemblyResolver paths)

    // `MetadataLoadContext` is not thread-safe; metadata access serialises through
    // `gate`. Result caches are read lock-free; only a miss takes the gate.
    let gate = obj ()
    let resolveCache = Dictionary<string, Type option>(StringComparer.Ordinal)

    let typeCache =
        ConcurrentDictionary<string, ExternalTypeShape voption>(StringComparer.Ordinal)

    let membersCache =
        ConcurrentDictionary<ExternalMemberName, EqArray<ExternalMember>>()

    let declaredFlags =
        BindingFlags.Public
        ||| BindingFlags.Static
        ||| BindingFlags.Instance
        ||| BindingFlags.DeclaredOnly

    /// Resolve a type by full metadata name. Checks core assembly first; scans the
    /// reference set only for namespace-qualified names. Must hold `gate`.
    let resolveTypeLocked (name: string) : Type option =
        match resolveCache.TryGetValue name with
        | true, t -> t
        | _ ->
            // `Type.IsVisible` is true iff public top-level, or public-nested in a
            // visible chain. Surfacing an external assembly's *internal* type would let
            // it shadow a locally-declared one of the same name.
            let tryAsm (asm: Assembly) : Type option =
                try
                    match asm.GetType(name, false) |> Option.ofObj with
                    | Some t when t.IsVisible -> Some t
                    | _ -> None
                with _ ->
                    None

            let found =
                match
                    (try
                        tryAsm mlc.CoreAssembly
                     with _ ->
                         None)
                with
                | Some _ as r -> r
                | None when name.Contains '.' ->
                    paths
                    |> Array.tryPick (fun p ->
                        match
                            (try
                                Some(mlc.LoadFromAssemblyPath p)
                             with _ ->
                                 None)
                        with
                        | Some asm -> tryAsm asm
                        | None -> None
                    )
                | None -> None

            resolveCache.[name] <- found
            found

    /// The physical home of a reflected type: the assembly it was loaded from plus its
    /// namespace. The backend's `TypeRef` scope comes from here, never from the key.
    let originOf (t: Type) : SymbolOrigin =
        {
            // A nameless home (a dynamic assembly) is unimportable; don't fabricate one.
            Home =
                match t.Assembly.GetName().Name with
                | null -> failwithf "MetadataSymbols: reflected type '%s' has a null assembly simple name" t.FullName
                | name -> SymbolHome.InAssembly(AssemblyName name)
            Namespace =
                SymbolKeyOps.namespaceKey (
                    match t.Namespace with
                    | null -> ""
                    | ns -> ns
                )
        }

    /// A public FIELD (`String.Empty`, `Vector3.X`) as an `ExternalMember`, shaped and keyed
    /// as a property, read by `ldfld`/`ldsfld`. A `const` (`Int32.MaxValue`, `Math.PI`) also
    /// carries its `ConstValue`. `None` for the enum `value__` and for an unmappable `const`.
    let fieldMemberOf (declKey: TypeKey) (origin: SymbolOrigin) (arity: int) (f: FieldInfo) : ExternalMember option =
        let constValue () =
            if f.IsLiteral then
                MetadataMapping.tryLiteralValue f |> Option.map ValueSome
            else
                Some ValueNone

        if f.IsSpecialName then
            None
        else
            match constValue (), MetadataMapping.tryBuildType intrinsics f.FieldType with
            | Some constValue, Some valueTy ->
                Some
                    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey f.Name EqArray.empty 0 MemberKind.Property) with
                        IsStatic = f.IsStatic
                        Storage = MemberStorage.Field
                        ConstValue = constValue
                        Signature = MetadataMapping.propertySignature arity valueTy
                        Origin = origin
                    }
            | _ -> None

    /// A mapped method as an `ExternalMember`; `None` if its signature doesn't map.
    let methodMemberOf (declKey: TypeKey) (origin: SymbolOrigin) (arity: int) (m: MethodInfo) : ExternalMember option =
        MetadataMapping.tryMethodSignature intrinsics m
        |> Option.map (fun (ps, ret) ->
            let argSig = EqArray.ofArray ps
            let methodTyparArity = MetadataMapping.methodTyparArityOf m

            { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey m.Name argSig methodTyparArity MemberKind.Method) with
                IsStatic = m.IsStatic
                Signature =
                    MetadataMapping.methodSignature arity methodTyparArity (ExternalSignature.tupledParams argSig, ret)
                MethodTyparArity = methodTyparArity
                Origin = origin
                OptionalDefaults = MetadataMapping.optionalDefaults (m.GetParameters())
            }
        )

    /// A mapped property as an `ExternalMember`; `None` if its value type doesn't map.
    let propertyMemberOf
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (arity: int)
        (p: PropertyInfo)
        : ExternalMember option =
        MetadataMapping.tryPropertySignature intrinsics p
        |> Option.map (fun valueTy ->
            { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey p.Name EqArray.empty 0 MemberKind.Property) with
                IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                Storage = MemberStorage.Property
                Signature = MetadataMapping.propertySignature arity valueTy
                Origin = origin
            }
        )

    /// Public declared members of `t` whose signatures map. Accessors are modelled
    /// through `Storage = Property` and filtered from the method walk. Must hold `gate`.
    let enumerateClassMembers (t: Type) : EqArray<ExternalMember> =
        let origin = originOf t
        let declKey = MetadataMapping.declTypeKey t
        // The declaring type's typar count, the width of the signature
        // template's declaring axis (`FTTypar(Declaring,i)`, `i < arity`).
        let arity =
            if t.IsGenericType then
                t.GetGenericArguments().Length
            else
                0

        let properties =
            t.GetProperties declaredFlags
            |> Array.choose (propertyMemberOf declKey origin arity)

        let methods =
            t.GetMethods declaredFlags
            // `IsSpecialName` covers property getters/setters and event add/remove.
            |> Array.filter (fun m -> not m.IsSpecialName)
            |> Array.choose (methodMemberOf declKey origin arity)

        // Indexers surface as `get_Item` method members (the property shape can't
        // model index arguments). Ref-returning indexers (`Span<T>.Item : T&`) are
        // skipped by the property walk above, so this is their only surface.
        let indexers =
            t.GetProperties declaredFlags
            |> Array.filter (fun p -> p.GetIndexParameters().Length > 0 && not (isNull p.GetMethod))
            |> Array.choose (fun p ->
                let getter = p.GetMethod

                MetadataMapping.tryMethodSignature intrinsics getter
                |> Option.map (fun (ps, ret) ->
                    let argSig = EqArray.ofArray ps

                    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey "get_Item" argSig 0 MemberKind.Method) with
                        IsStatic = getter.IsStatic
                        Signature =
                            MetadataMapping.methodSignature arity 0 (ExternalSignature.tupledParams argSig, ret)
                        Origin = origin
                    }
                )
            )

        let fields =
            t.GetFields declaredFlags |> Array.choose (fieldMemberOf declKey origin arity)

        // Constructors surface as `".ctor"`. `GetMethods` excludes them, so a
        // separate `GetConstructors` pass is required.
        let ctors =
            t.GetConstructors declaredFlags
            |> Array.choose (fun c ->
                MetadataMapping.tryCtorSignature intrinsics c
                |> Option.map (fun (ps, ret) ->
                    let argSig = EqArray.ofArray ps

                    ExternalMember.ctor
                        declKey
                        (MetadataMapping.methodSignature arity 0 (ExternalSignature.tupledParams argSig, ret))
                        argSig
                        origin
                        (MetadataMapping.optionalDefaults (c.GetParameters()))
                )
            )

        EqArray.ofArray (Array.concat [| properties; methods; indexers; fields; ctors |])

    /// Interface set. An interface whose own type args do not map is skipped. Must hold `gate`.
    let buildClassInterfaces (t: Type) : EqArray<FrozenNominal> =
        t.GetInterfaces()
        |> Array.choose (fun i ->
            match MetadataMapping.tryBuildType intrinsics i with
            | Some frozen ->
                match FrozenNominal.TryOfFrozen frozen with
                | ValueSome iface -> Some iface
                | ValueNone -> None
            | None -> None
        )
        |> EqArray.ofArray

    /// Declared base type as a template over the declaring typars. `ValueNone` for interfaces,
    /// for `System.Object`, and for a base the intrinsic map sends to a non-nominal (a union
    /// repr), dropped as an unmappable interface is. Must hold `gate`.
    let buildClassBaseType (t: Type) : FrozenNominal voption =
        if t.IsInterface || isNull t.BaseType then
            ValueNone
        else
            match MetadataMapping.tryBuildType intrinsics t.BaseType with
            | Some frozen -> FrozenNominal.TryOfFrozen frozen
            | None -> ValueNone

    /// `[<AllowNullLiteral>]` is emitted into metadata and visible in reflection-only loads.
    let hasAllowNullLiteral (t: Type) : bool =
        t.CustomAttributes
        |> Seq.exists (fun a ->
            match a.AttributeType.FullName with
            | "Microsoft.FSharp.Core.AllowNullLiteralAttribute" -> true
            | _ -> false
        )

    let decodeClassFlags (t: Type) : ExternalClassFlags =
        { ExternalClassFlags.Default with
            Declared =
                {
                    IsSealed = t.IsSealed
                    IsAbstract = t.IsAbstract
                    AllowNullLiteral = hasAllowNullLiteral t
                }
            IsValueType = t.IsValueType
        }

    let computeType (name: string) : ExternalTypeShape voption =
        lock
            gate
            (fun () ->
                match resolveTypeLocked name with
                | Some t ->
                    let arity =
                        if t.IsGenericType then
                            t.GetGenericArguments().Length
                        else
                            0

                    let shape: ExternalClassShape =
                        {
                            TyparArity = arity
                            IsInterface = t.IsInterface
                            Members = enumerateClassMembers t
                            FrozenInterfaces = buildClassInterfaces t
                            FrozenBaseType = buildClassBaseType t
                            Flags = decodeClassFlags t
                            Origin = originOf t
                        }

                    ValueSome(ExternalTypeShape.Class shape)
                | None -> ValueNone
            )

    /// All overloads of `memberName` whose signatures map. Methods sorted most-params
    /// first; a property wins as a singleton over a like-named method.
    let computeMembers (typeName: string) (memberName: string) : ExternalMember[] =
        lock
            gate
            (fun () ->
                match resolveTypeLocked typeName with
                | None -> [||]
                | Some t ->
                    // The probes below read ONE type's own members (`DeclaredOnly`); each
                    // member's `declKey`/`origin` come from the type it is declared on.
                    let commonOf (st: Type) =
                        let origin = originOf st
                        let declKey = MetadataMapping.declTypeKey st

                        let arity =
                            if st.IsGenericType then
                                st.GetGenericArguments().Length
                            else
                                0

                        origin, declKey, arity

                    let ctorsOn (st: Type) : ExternalMember[] =
                        let origin, declKey, arity = commonOf st

                        st.GetConstructors declaredFlags
                        |> Array.sortByDescending (fun c -> c.GetParameters().Length)
                        |> Array.choose (fun c ->
                            MetadataMapping.tryCtorSignature intrinsics c
                            |> Option.map (fun (ps, ret) ->
                                let argSig = EqArray.ofArray ps

                                ExternalMember.ctor
                                    declKey
                                    (MetadataMapping.methodSignature
                                        arity
                                        0
                                        (ExternalSignature.tupledParams argSig, ret))
                                    argSig
                                    origin
                                    (MetadataMapping.optionalDefaults (c.GetParameters()))
                            )
                        )

                    let methodsOn (st: Type) : ExternalMember[] =
                        let origin, declKey, arity = commonOf st

                        st.GetMethods declaredFlags
                        |> Array.filter (fun m -> m.Name = memberName)
                        |> Array.sortByDescending (fun m -> m.GetParameters().Length)
                        |> Array.choose (methodMemberOf declKey origin arity)

                    let propertyOn (st: Type) : ExternalMember[] =
                        let origin, declKey, arity = commonOf st

                        match st.GetProperty(memberName, declaredFlags) with
                        | (null: PropertyInfo) -> [||]
                        | p ->
                            match propertyMemberOf declKey origin arity p with
                            | Some m -> [| m |]
                            | None -> [||]

                    // No property / method by this name, so a hit here is a genuine field
                    // (`String.Empty`, `ValueTuple.Item1`).
                    let fieldOn (st: Type) : ExternalMember[] =
                        let origin, declKey, arity = commonOf st

                        match st.GetField(memberName, declaredFlags) with
                        | (null: FieldInfo) -> [||]
                        | f ->
                            match fieldMemberOf declKey origin arity f with
                            | Some m -> [| m |]
                            | None -> [||]

                    // The object arg's type plus what it inherits members from, most-derived
                    // first: a class or struct walks its base chain to `System.Object`; an
                    // interface walks its base interfaces, then `Object`, appended by hand.
                    let candidates =
                        if t.IsInterface then
                            let objectTy =
                                match resolveTypeLocked "System.Object" with
                                | Some ot -> [| ot |]
                                | None -> [||]

                            Array.concat [ [| t |]; t.GetInterfaces(); objectTy ]
                        else
                            let rec baseChain (ty: Type) : Type list =
                                if isNull ty then [] else ty :: baseChain ty.BaseType

                            baseChain t |> List.toArray

                    // Fed a most-derived-first array, `HashSet.Add` keeps the first sighting
                    // of a signature, so an override drops its base twin and overloads split
                    // across levels survive. Re-sorted most-params-first, so `[0]` is widest.
                    let dedupMethods (methods: ExternalMember[]) : ExternalMember[] =
                        let seen = System.Collections.Generic.HashSet<_>(HashIdentity.Structural)

                        methods
                        |> Array.filter (fun m -> seen.Add((m.Key.ArgSig, m.Key.Kind, m.MethodTyparArity)))
                        |> Array.sortByDescending (fun m -> m.Key.ArgSig.Length)

                    // Constructors are NOT inherited, so a `.ctor` request stays on `t`.
                    if memberName = ".ctor" then
                        ctorsOn t
                    else
                        // CLR by-name hiding: a property or field owns `memberName` outright
                        // and hides every base member of it; methods collect down the chain
                        // until such a level. Within one level, property > method > field.
                        let rec resolve (methods: ExternalMember[]) (i: int) : ExternalMember[] =
                            if i >= candidates.Length then
                                dedupMethods methods
                            else
                                let st = candidates.[i]

                                match propertyOn st with
                                | [| _ |] as p -> if Array.isEmpty methods then p else dedupMethods methods
                                | _ ->
                                    match methodsOn st with
                                    | [||] ->
                                        match fieldOn st with
                                        | [||] -> resolve methods (i + 1)
                                        | f -> if Array.isEmpty methods then f else dedupMethods methods
                                    | ms -> resolve (Array.append methods ms) (i + 1)

                        resolve [||] 0
            )

    // The metadata provider is string-keyed internally (its caches address the BCL
    // compiled name); the store view projects the resolved key to that name.
    member private _.LookupTypeByName(name: string) =
        match typeCache.TryGetValue name with
        | true, v -> v
        | _ ->
            let v = computeType name
            typeCache.[name] <- v
            v

    member private _.LookupMembersByName(key: ExternalMemberName) =
        match membersCache.TryGetValue key with
        | true, v -> v
        | _ ->
            let v = EqArray.ofArray (computeMembers key.DeclaringType key.Name)
            membersCache.[key] <- v
            v

    interface IExternalSymbolProvider

    interface IExternalSymbolResolver with
        // IL metadata exposes no module structure: a namespace is a prefix of a type name.
        member _.Scope = ScopeContents.empty
        member _.TryLookup _ = ValueNone

        // Bare IL has no module chains, so a name IS the identity.
        member this.TryLookupType(name: string) =
            this.LookupTypeByName name
            |> ValueOption.map (ExternalSymbols.nameKeyedTypeHit name)

        member _.TryLookupUnionCases _ = EqArray.empty
        // The metadata layer scrapes IL, never F# record tycons, so it never contributes to
        // the reverse field index (F#'s `isILOrRequiredQualifiedAccess` excludes IL too).
        member _.TryRecordsWithField _ = EqArray.empty
        member _.AmbientOpenPrefixes = []

    interface IExternalSymbolStore with
        member this.TryLookupType(key: TypeKey) =
            this.LookupTypeByName(SymbolKeyOps.typeMetaName key)

        member this.TryLookupMembers(key, memberName) =
            this.LookupMembersByName(
                ExternalMemberName.ofKeyed
                    {
                        DeclaringType = key
                        Name = memberName
                    }
            )

        // The cache is keyed by (declaring name, member NAME), so a key is answered by
        // exact-identity selection out of that name's overload set; a best-by-arity
        // collapse would answer it with a SIBLING overload's entry.
        member this.TryLookupMemberByKey(key: MemberKey) =
            this.LookupMembersByName(
                ExternalMemberName.ofKeyed
                    {
                        DeclaringType = key.Decl
                        Name = key.Name
                    }
            )
            |> ExternalSymbols.memberByKey key

        // .NET metadata has no TS index-signature concept, so an indexer is a `get_Item`
        // member, served through `TryLookupMember`.
        member _.TryLookupIndexSignature _ = []
        // The metadata layer models no free-function symbols at all (`TryLookup` is a
        // constant miss), so its key-addressed twin is one too.
        member _.TryLookupByKey _ = ValueNone
        // The metadata layer CONSUMES the axis to canonicalize BCL names; it declares none.
        member _.IntrinsicTypeMap = IntrinsicTypeMap.empty

        // .NET metadata IS the platform, so this source is the one that answers.
        member this.Platform = ValueSome(this :> IPlatformFacts)

    interface IPlatformFacts with

        // `Vesper.int` is a value type here because the repr its `.clr.fs` binds,
        // `System.Int32`, is one. A canon declared UNSUPPORTED on this target has no repr to
        // reflect; any other key answers under its plain metadata name.
        member this.IsValueType(key: TypeKey) =
            let reflected (name: string) =
                match this.LookupTypeByName name with
                | ValueSome(ExternalTypeShape.Class shape) -> ValueSome shape.Flags.IsValueType
                | _ -> ValueNone

            match IntrinsicTypeMap.tryRepr key intrinsics with
            | ValueSome(IntrinsicPlatform.Repr repr) -> reflected repr
            | ValueSome(IntrinsicPlatform.Unsupported _) -> ValueNone
            | ValueNone -> reflected (SymbolKeyOps.typeMetaName key)

        member _.TupleType(arity: int) =
            if ClrTuples.isTupleArity arity then
                ValueSome(ClrTuples.typeKey arity)
            else
                ValueNone

module MetadataSymbols =

    /// Host runtime TPA, so a stamped `SymbolHome` resolves to `System.Private.CoreLib`
    /// (impl), not `System.Runtime` (ref).
    let runtimeAssemblyPaths () : string list =
        match AppContext.GetData "TRUSTED_PLATFORM_ASSEMBLIES" with
        | :? string as tpa when tpa.Length > 0 ->
            tpa.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        | _ ->
            Directory.GetFiles(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), "*.dll")
            |> Array.toList

    /// Provider over an explicit reference-assembly path set, canonicalizing BCL
    /// primitives through the extracted intrinsic axis.
    let createWith (intrinsics: IntrinsicTypeMap) (paths: string seq) : IExternalSymbolProvider =
        MetadataSymbolProvider(intrinsics, paths) :> IExternalSymbolProvider

    /// `createWith` with no axis, so BCL primitives stay nominal classes.
    let create (paths: string seq) : IExternalSymbolProvider = createWith IntrinsicTypeMap.empty paths

    /// Process-wide provider over the host runtime's assemblies, a test convenience;
    /// production composes a per-compilation reader seeded with the extracted intrinsic axis.
    let provider: IExternalSymbolProvider = create (runtimeAssemblyPaths ())
