namespace XParsec.FSharp.Codegen.Clr

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open XParsec.FSharp.SemanticAnalysis

// Resolves BCL types to `ExternalTypeShape` and their members to `FrozenType`
// signature templates via `System.Reflection.MetadataLoadContext`.

/// `System.Type` → `FrozenType` template mapping. Each template is over the
/// declaring type's generic parameters (`FTTypar(Declaring,i)`, method-owned as
/// `FTTypar(Method,j)`). Shapes that don't map yield `None` — skipped, never
/// faked. Templates are inert data; no live `Type` escapes.
module private MetadataMapping =

    /// Open-generic-definition name for a constructed generic, else `FullName`.
    let metadataName (t: Type) : string =
        if t.IsGenericType && not t.IsGenericTypeDefinition then
            t.GetGenericTypeDefinition().FullName
        else
            t.FullName

    /// `reverseCanon` is the dynamically-harvested `{ platform-repr → [canon] }` map
    /// (`System.Int32 → [int]`), folded from the layer-1 providers' `IntrinsicReverseCanon`
    /// — the reverse face of `type int = (# "System.Int32" #)`. It is what lets a BCL
    /// member's `System.Int32` parameter present as a Vesper `int` so semantic analysis
    /// can call it (`int` and `System.Int32` are otherwise distinct, never-unifying
    /// types). NOT a static table: a BCL type absent from the map is a real class. On CLR
    /// each platform name maps to exactly one canon, so the list is a singleton and the
    /// head is taken.
    let rec tryBuildType (reverseCanon: Map<string, SymbolKey list>) (t: Type) : FrozenType option =
        let go = tryBuildType reverseCanon

        if t.IsByRef then
            // `in`/`out`/`ref` all collapse to `T&` here — direction-agnostic.
            // A C# `in` param additionally carries `modreq(InAttribute)` which is
            // dropped; calling such a member would fail CLR member-ref binding until
            // the modifier is threaded through the encoder (paired TODO at
            // `mintMemberRef`, ClrExternalMembers.fs).
            match go (t.GetElementType()) with
            | Some elem -> Some(FTConst(BuiltinTypes.intrinsicKey RuntimeNames.byrefName, EqArray.singleton elem))
            | None -> None
        elif t.IsPointer then
            None
        elif t.IsArray then
            match go (t.GetElementType()) with
            | Some elem ->
                Some(
                    FTConst(
                        BuiltinTypes.intrinsicKey (RuntimeNames.arrayName (t.GetArrayRank())),
                        EqArray.singleton elem
                    )
                )
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

                let key =
                    SymbolKeyOps.qualifiedTypeKeyOf (Some(t.Assembly.GetName().Name)) name frozen.Length

                Some(FTClass(key, EqArray.ofArray frozen))
        else
            match t.FullName with
            | null -> None // constructed/exotic type with no metadata full name
            | "System.Void" -> Some(FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty))
            // Canonicalize a BCL primitive (`System.Int32 → int`) only when it is a
            // SEALED leaf type. The harvested reverse map also carries the unsealed
            // subtype ROOTS (`System.Object → obj`, `System.Exception → exn`) and
            // capability interfaces; those must keep their BCL nominal form here so
            // ctor / `new` / subtype resolution still keys on it — they reconcile to
            // their canon at the unification bridge (`Engine.canonName`), not eagerly.
            // Scalar primitives + `string` are sealed; `obj`/`exn`/interfaces are not,
            // so `IsSealed` partitions them exactly (and dynamically — no name list).
            | fullName when
                t.IsSealed
                && (reverseCanon |> Map.tryFind fullName |> Option.exists (List.isEmpty >> not))
                ->
                Some(FTConst(reverseCanon.[fullName] |> List.head, EqArray.empty))
            | fullName ->
                Some(
                    FTClass(SymbolKeyOps.qualifiedTypeKeyOf (Some(t.Assembly.GetName().Name)) fullName 0, EqArray.empty)
                )

    /// The tupled parameter template: 0 → `unit`, 1 → bare param, N≥2 → `FTTuple`
    /// (.NET calling convention — not curried).
    let frozenParams (ps: FrozenType[]) : FrozenType =
        match ps.Length with
        | 0 -> FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty)
        | 1 -> ps.[0]
        | _ -> FTTuple(EqArray.ofArray ps)

    /// `(Parameters, Return)` templates for a method. `None` if any type doesn't map.
    let tryMethodSignature
        (reverseCanon: Map<string, SymbolKey list>)
        (m: MethodInfo)
        : (FrozenType * FrozenType) option =
        let paramTys =
            m.GetParameters()
            |> Array.map (fun p -> tryBuildType reverseCanon p.ParameterType)

        let retTy = tryBuildType reverseCanon m.ReturnType

        if retTy.IsNone || Array.exists Option.isNone paramTys then
            None
        else
            Some(frozenParams (paramTys |> Array.map Option.get), retTy.Value)

    /// Method-axis generic-parameter count; `0` for a non-generic method.
    let methodArityOf (m: MethodInfo) : int =
        if m.IsGenericMethodDefinition then
            m.GetGenericArguments().Length
        else
            0

    /// `default(T)` as a `TConstValue` for primitive value types; `None` otherwise.
    let private zeroOfValueType (t: Type) : TConstValue option =
        if not t.IsValueType then
            None
        else
            match t.FullName with
            | "System.Boolean" -> Some(TConstValue.Bool false)
            | "System.Char" -> Some(TConstValue.Char '\000')
            | "System.SByte"
            | "System.Int16"
            | "System.UInt16"
            | "System.Int32"
            | "System.UInt32" -> Some(TConstValue.Int 0)
            | "System.Byte" -> Some(TConstValue.Byte 0uy)
            | "System.Int64"
            | "System.UInt64" -> Some(TConstValue.Int64 0L)
            | "System.Single" -> Some(TConstValue.Float32 0.0f)
            | "System.Double" -> Some(TConstValue.Float 0.0)
            | _ -> None

    /// Boxed `RawDefaultValue` → `TConstValue`. Unsigned forms fold onto their signed
    /// counterpart (bit-identical; only used to fill omitted arguments).
    let private constOfBoxed (v: obj) : TConstValue option =
        match v with
        | :? bool as b -> Some(TConstValue.Bool b)
        | :? char as c -> Some(TConstValue.Char c)
        | :? sbyte as n -> Some(TConstValue.Int(int n))
        | :? int16 as n -> Some(TConstValue.Int(int n))
        | :? uint16 as n -> Some(TConstValue.Int(int n))
        | :? int as n -> Some(TConstValue.Int n)
        | :? uint32 as n -> Some(TConstValue.Int(int n))
        | :? byte as n -> Some(TConstValue.Byte n)
        | :? int64 as n -> Some(TConstValue.Int64 n)
        | :? uint64 as n -> Some(TConstValue.Int64(int64 n))
        | :? single as f -> Some(TConstValue.Float32 f)
        | :? double as f -> Some(TConstValue.Float f)
        | :? string as s -> Some(TConstValue.String s)
        | _ -> None

    /// Trailing omittable parameter defaults in declaration order. Walks from the end;
    /// stops at the first non-optional or non-representable-constant parameter.
    let optionalDefaults (ps: ParameterInfo[]) : TConstValue list =
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
                acc <- c :: acc
                i <- i - 1
            | None -> go <- false

        acc

    /// Property signature: value type only (no arrow). `Storage = Property` on the member.
    let tryPropertySignature (reverseCanon: Map<string, SymbolKey list>) (p: PropertyInfo) : FrozenType option =
        tryBuildType reverseCanon p.PropertyType

    /// Constructor as `(params) → declType`. Zero-param ctor reads as `unit → declType`.
    /// `None` if any type doesn't map. Surfaced as member `".ctor"`.
    let tryCtorSignature
        (reverseCanon: Map<string, SymbolKey list>)
        (c: ConstructorInfo)
        : (FrozenType * FrozenType) option =
        let paramTys =
            c.GetParameters()
            |> Array.map (fun p -> tryBuildType reverseCanon p.ParameterType)

        let retTy = tryBuildType reverseCanon c.DeclaringType

        if retTy.IsNone || Array.exists Option.isNone paramTys then
            None
        else
            Some(frozenParams (paramTys |> Array.map Option.get), retTy.Value)

    /// Render a type with open typars for an `argSig` overload key: `!i` / `!!i` for
    /// declaring/method typars, `FullName` otherwise. Never re-parsed — only disambiguates.
    let rec openTyparSig (t: Type) : string =
        if t.IsGenericParameter then
            if isNull t.DeclaringMethod then
                "!" + string t.GenericParameterPosition
            else
                "!!" + string t.GenericParameterPosition
        elif t.IsGenericType && not t.IsGenericTypeDefinition then
            let def = t.GetGenericTypeDefinition().FullName
            let args = t.GetGenericArguments() |> Array.map openTyparSig |> String.concat ","
            def + "<" + args + ">"
        else
            match t.FullName with
            | null -> t.Name
            | fn -> fn

    /// Property `ExternalSignature`: `Parameters = unit`, value type in `Return`.
    let propertySignature (declaringArity: int) (valueTy: FrozenType) : ExternalSignature =
        ExternalSignature.make (declaringArity, 0, FTConst(BuiltinTypes.intrinsicKey "unit", EqArray.empty), valueTy)

    /// Method/ctor `ExternalSignature` from its `(Parameters, Return)` templates.
    let methodSignature
        (declaringArity: int)
        (methodArity: int)
        (parameters: FrozenType, ret: FrozenType)
        : ExternalSignature =
        ExternalSignature.make (declaringArity, methodArity, parameters, ret)

    /// `SymbolKey.TypeKey` for the declaring type.
    let declTypeKey (t: Type) : SymbolKey =
        let asm = t.Assembly.GetName().Name |> Option.ofObj
        let full = metadataName t
        let ns = if isNull t.Namespace then "" else t.Namespace
        let simple = SymbolOrigin.StripNamespace ns full
        SymbolKey.TypeKey(asm, ns, simple)

/// `IExternalSymbolProvider` over reference assembly paths via a shared `MetadataLoadContext`.
/// `reverseCanon` is the harvested `{ platform-repr → [canon] }` map (`System.Int32 → [int]`)
/// the leaf canonicalizes BCL primitive types through (see `MetadataMapping.tryBuildType`);
/// `Map.empty` for a leaf with no Vesper.Core in scope (BCL types then stay nominal classes).
type MetadataSymbolProvider(reverseCanon: Map<string, SymbolKey list>, assemblyPaths: string seq) =
    let paths = Seq.toArray assemblyPaths
    let mlc = new MetadataLoadContext(PathAssemblyResolver paths)

    // `MetadataLoadContext` is not thread-safe; metadata access serialises through
    // `gate`. Result caches are read lock-free; only a miss takes the gate.
    let gate = obj ()
    let resolveCache = Dictionary<string, Type option>(StringComparer.Ordinal)

    let typeCache =
        ConcurrentDictionary<string, ExternalTypeShape voption>(StringComparer.Ordinal)

    let memberCache =
        ConcurrentDictionary<struct (string * string), ExternalMember voption>()

    let membersCache =
        ConcurrentDictionary<struct (string * string), ExternalMember[]>()

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
            // Only surface types a consumer can actually reference; `Type.IsVisible`
            // is true iff public top-level or public-nested in a visible chain.
            // Resolving an external assembly's *internal* type is unsound — it lets it
            // shadow a locally-declared one of the same name.
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

    let originOf (t: Type) (declaring: string option) : SymbolOrigin =
        {
            Assembly = t.Assembly.GetName().Name |> Option.ofObj
            Namespace =
                (match t.Namespace with
                 | null -> ""
                 | ns -> ns)
            DeclaringType = declaring
        }

    /// A genuine public FIELD (`String.Empty`, `Vector3.X`, `ValueTuple.Item1`) as an
    /// `ExternalMember` — a value member the property/method walks never see. Reads via
    /// `ldfld`/`ldsfld` (not a `get_X` accessor), so `Storage = Field`; its shape is a
    /// value member (no params, value in `Return`), identical to a property, so it reuses
    /// `propertySignature` and keys under the `Property` identity (empty `argSig`).
    /// `IsLiteral` (a `const`, lowers to `ldc` not a field load) and `IsSpecialName` (the
    /// enum `value__`), plus unmappable field types, drop out as `None`. Shared by the
    /// eager `enumerateClassMembers` field walk and the lazy `TryLookupMember` fallback.
    let fieldMemberOf (declKey: SymbolKey) (origin: SymbolOrigin) (arity: int) (f: FieldInfo) : ExternalMember option =
        if f.IsLiteral || f.IsSpecialName then
            None
        else
            match MetadataMapping.tryBuildType reverseCanon f.FieldType with
            | Some valueTy ->
                Some
                    {
                        Name = f.Name
                        IsStatic = f.IsStatic
                        Storage = MemberStorage.Field
                        Signature = MetadataMapping.propertySignature arity valueTy
                        MethodArity = 0
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, f.Name, EqArray.empty, MemberKind.Property)
                        OptionalDefaults = []
                        IsOptional = false
                    }
            | None -> None

    /// A mapped method as an `ExternalMember`; `None` if its signature doesn't map.
    /// Shared by the eager `enumerateClassMembers` method walk and the lazy
    /// `TryLookupMember` per-type probe so the two paths can never drift on the shape
    /// they mint for the same `MethodInfo`.
    let methodMemberOf
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (arity: int)
        (m: MethodInfo)
        : ExternalMember option =
        MetadataMapping.tryMethodSignature reverseCanon m
        |> Option.map (fun (ps, ret) ->
            let argSig =
                m.GetParameters()
                |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                |> EqArray.ofArray

            let methodArity = MetadataMapping.methodArityOf m

            {
                Name = m.Name
                IsStatic = m.IsStatic
                Storage = MemberStorage.Method
                Signature = MetadataMapping.methodSignature arity methodArity (ps, ret)
                MethodArity = methodArity
                Origin = origin
                Key = SymbolKey.MemberKey(declKey, m.Name, argSig, MemberKind.Method)
                OptionalDefaults = MetadataMapping.optionalDefaults (m.GetParameters())
                IsOptional = false
            }
        )

    /// A mapped property as an `ExternalMember`; `None` if its value type doesn't map.
    /// Shared by the eager and lazy paths (see `methodMemberOf`).
    let propertyMemberOf
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (arity: int)
        (p: PropertyInfo)
        : ExternalMember option =
        MetadataMapping.tryPropertySignature reverseCanon p
        |> Option.map (fun valueTy ->
            {
                Name = p.Name
                IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                Storage = MemberStorage.Property
                Signature = MetadataMapping.propertySignature arity valueTy
                MethodArity = 0
                Origin = origin
                Key = SymbolKey.MemberKey(declKey, p.Name, EqArray.empty, MemberKind.Property)
                OptionalDefaults = []
                IsOptional = false
            }
        )

    /// Public declared members of `t` whose signatures map. Accessors are modelled
    /// through `Storage = Property` and filtered from the method walk. Must hold `gate`.
    let enumerateClassMembers (t: Type) : ExternalMember[] =
        let origin = originOf t (Some(MetadataMapping.metadataName t))
        let declKey = MetadataMapping.declTypeKey t
        // The declaring type's typar count — the width of the signature
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

                MetadataMapping.tryMethodSignature reverseCanon getter
                |> Option.map (fun (ps, ret) ->
                    let argSig =
                        getter.GetParameters()
                        |> Array.map (fun ip -> MetadataMapping.openTyparSig ip.ParameterType)
                        |> EqArray.ofArray

                    {
                        Name = "get_Item"
                        IsStatic = getter.IsStatic
                        Storage = MemberStorage.Method
                        Signature = MetadataMapping.methodSignature arity 0 (ps, ret)
                        MethodArity = 0
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, "get_Item", argSig, MemberKind.Method)
                        OptionalDefaults = []
                        IsOptional = false
                    }
                )
            )

        // Genuine public FIELDS (`String.Empty`, `Vector3.X`, `ValueTuple.Item1`) —
        // value members the property/method walks never see (see `fieldMemberOf`).
        let fields =
            t.GetFields declaredFlags |> Array.choose (fieldMemberOf declKey origin arity)

        // Constructors surface as `".ctor"`. `GetMethods` excludes them, so a
        // separate `GetConstructors` pass is required.
        let ctors =
            t.GetConstructors declaredFlags
            |> Array.choose (fun c ->
                MetadataMapping.tryCtorSignature reverseCanon c
                |> Option.map (fun (ps, ret) ->
                    let argSig =
                        c.GetParameters()
                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                        |> EqArray.ofArray

                    ExternalMember.ctor
                        declKey
                        (MetadataMapping.methodSignature arity 0 (ps, ret))
                        argSig
                        origin
                        (MetadataMapping.optionalDefaults (c.GetParameters()))
                )
            )

        Array.concat [| properties; methods; indexers; fields; ctors |]

    /// Interface set as `(compiled-name, type-args)` templates. Unmappable interfaces
    /// are skipped. Must hold `gate`.
    let buildClassInterfaces (t: Type) : (string * FrozenType[])[] =
        t.GetInterfaces()
        |> Array.choose (fun i ->
            let name = MetadataMapping.metadataName i

            let args =
                if i.IsGenericType then
                    i.GetGenericArguments() |> Array.map (MetadataMapping.tryBuildType reverseCanon)
                else
                    [||]

            if Array.exists Option.isNone args then
                None
            else
                Some(name, args |> Array.map Option.get)
        )

    /// Declared base type as a `FrozenType` template. `ValueNone` for interfaces and
    /// `System.Object`. Must hold `gate`.
    let buildClassBaseType (t: Type) : FrozenType voption =
        if t.IsInterface || isNull t.BaseType then
            ValueNone
        else
            match MetadataMapping.tryBuildType reverseCanon t.BaseType with
            | Some frozen -> ValueSome frozen
            | None -> ValueNone

    /// `[<AllowNullLiteral>]` is emitted into metadata and visible in reflection-only loads.
    let hasAllowNullLiteral (t: Type) : bool =
        t.CustomAttributes
        |> Seq.exists (fun a ->
            match a.AttributeType.FullName with
            | "Microsoft.FSharp.Core.AllowNullLiteralAttribute" -> true
            | _ -> false
        )

    // A real .NET type is never the synthetic grouping / native-attached JS shape, so
    // `MemberLowering` stays at the `ReceiverFirst` default; only the metadata-derived
    // fields are set here, so a future flag (e.g. R5's `Global`) is not restated.
    let decodeClassFlags (t: Type) : ExternalClassFlags =
        { ExternalClassFlags.Default with
            IsSealed = t.IsSealed
            IsAbstract = t.IsAbstract
            AllowNullLiteral = hasAllowNullLiteral t
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
                            Arity = arity
                            IsInterface = t.IsInterface
                            Members = enumerateClassMembers t
                            FrozenInterfaces = buildClassInterfaces t
                            FrozenBaseType = buildClassBaseType t
                            Flags = decodeClassFlags t
                            Origin = originOf t None
                            // The metadata layer never mints a dual-faced capability
                            // interface — a real BCL `System.IDisposable` arrives as
                            // itself; reconciliation to the canonical rides the
                            // contract layer's `(# … #)` face, not this shape.
                            CapabilityFace = ValueNone
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
                    // Per-type probes reading ONE type's own members (`DeclaredOnly`);
                    // the inheritance walk that feeds them the receiver's base types (so
                    // an *inherited* member resolves) is `candidates` below. Each member's
                    // `declKey`/`origin` come from the type it is declared on.
                    let commonOf (st: Type) =
                        let origin = originOf st (Some(MetadataMapping.metadataName st))
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
                            MetadataMapping.tryCtorSignature reverseCanon c
                            |> Option.map (fun (ps, ret) ->
                                let argSig =
                                    c.GetParameters()
                                    |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                                    |> EqArray.ofArray

                                ExternalMember.ctor
                                    declKey
                                    (MetadataMapping.methodSignature arity 0 (ps, ret))
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

                    // No property / method by this name — a genuine field (`String.Empty`,
                    // `ValueTuple.Item1`).
                    let fieldOn (st: Type) : ExternalMember[] =
                        let origin, declKey, arity = commonOf st

                        match st.GetField(memberName, declaredFlags) with
                        | (null: FieldInfo) -> [||]
                        | f ->
                            match fieldMemberOf declKey origin arity f with
                            | Some m -> [| m |]
                            | None -> [||]

                    // The receiver's type plus the types it inherits members from, in
                    // most-derived-first order.
                    //   * A class or struct walks its base chain, which terminates at
                    //     `System.Object` (whose `BaseType` is null): on the CLR every
                    //     value ultimately inherits `ToString` / `Equals` / `GetHashCode`
                    //     from `Object`, plus any un-overridden member of an intermediate
                    //     base (`SystemException.Message`).
                    //   * An interface walks its transitive base interfaces, then
                    //     `System.Object` — an interface reference inherits the `Object`
                    //     members too, but `GetInterfaces()` never yields `Object` (it is
                    //     not an interface) and an interface's `BaseType` is null, so it
                    //     is appended explicitly.
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

                    // Dedupe collected method overloads by signature `(argSig, kind,
                    // methodArity)`: fed a most-derived-first array, `HashSet.Add` keeps
                    // the first sighting, so a derived override drops its base twin while
                    // overloads split across levels all survive. Re-sorted most-params-first
                    // (stable) so `computeMember`'s `arr.[0]` is the widest overload.
                    let dedupMethods (methods: ExternalMember[]) : ExternalMember[] =
                        let seen = System.Collections.Generic.HashSet<_>(HashIdentity.Structural)

                        methods
                        |> Array.filter (fun m ->
                            match m.Key with
                            | SymbolKey.MemberKey(_, _, argSig, kind) -> seen.Add((argSig, kind, m.MethodArity))
                            | _ -> true
                        )
                        |> Array.sortByDescending (fun m ->
                            match m.Key with
                            | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
                            | _ -> 0
                        )

                    // Constructors are NOT inherited — a `.ctor` request stays on `t`.
                    if memberName = ".ctor" then
                        ctorsOn t
                    else
                        // Resolve `memberName` under CLR by-name hiding: the MOST-DERIVED
                        // declaration of the name wins, and lookup never falls through to a
                        // base member of a DIFFERENT KIND. Concretely:
                        //   * A property or field owns the name outright — it hides every
                        //     base member (any kind) of that name.
                        //   * Methods overload, so they are COLLECTED down the chain (an
                        //     override drops its base twin; overloads split across levels all
                        //     survive) — but a property/field on a lower level hides all
                        //     further base methods, so collection stops at that level.
                        // Walking most-derived first, the first level that declares the name
                        // therefore decides everything: a non-method with no method seen above
                        // it IS the answer; otherwise the methods gathered above it win and the
                        // non-method (and everything below) is hidden. Within a single level,
                        // precedence is property > method > field, mirroring the single-type
                        // `enumerateClassMembers` order (real types never collide across kinds
                        // at one level, so this only orders the theoretical IL case).
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

    /// Single best member (most-params wins). Call sites use `computeMembers` for overloads.
    let computeMember (typeName: string) (memberName: string) : ExternalMember voption =
        match computeMembers typeName memberName with
        | [||] -> ValueNone
        | arr -> ValueSome arr.[0]

    interface IExternalSymbolProvider with
        member _.TryLookup _ = ValueNone

        member _.TryLookupType name =
            match typeCache.TryGetValue name with
            | true, v -> v
            | _ ->
                let v = computeType name
                typeCache.[name] <- v
                v

        member _.TryLookupMember(typeName, memberName) =
            let key = struct (typeName, memberName)

            match memberCache.TryGetValue key with
            | true, v -> v
            | _ ->
                let v = computeMember typeName memberName
                memberCache.[key] <- v
                v

        member _.TryLookupMembers(typeName, memberName) =
            let key = struct (typeName, memberName)

            match membersCache.TryGetValue key with
            | true, v -> v
            | _ ->
                let v = computeMembers typeName memberName
                membersCache.[key] <- v
                v

        // .NET metadata has no TS index-signature concept — an indexer is a `get_Item`
        // member, served through `TryLookupMember`.
        member _.TryLookupIndexSignature _ = []
        member _.TryLookupUnionCase _ = ValueNone
        member _.AmbientOpenPrefixes = []
        member _.TryLookupInlineBody _ = ValueNone
        member _.TryLookupInlineBodyByName _ = ValueNone
        member _.IntrinsicReverseCanon = Map.empty
        member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr

module MetadataSymbols =

    /// Host runtime TPA. `Origin.Assembly` resolves to `System.Private.CoreLib`
    /// (impl), not `System.Runtime` (ref); a future driver should supply the target
    /// TFM's reference-pack paths instead.
    let runtimeAssemblyPaths () : string list =
        match AppContext.GetData "TRUSTED_PLATFORM_ASSEMBLIES" with
        | :? string as tpa when tpa.Length > 0 ->
            tpa.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        | _ ->
            Directory.GetFiles(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), "*.dll")
            |> Array.toList

    /// Provider over an explicit reference-assembly path set, canonicalizing BCL
    /// primitives through the harvested `{ platform-repr → canon }` map.
    let createWith (reverseCanon: Map<string, SymbolKey list>) (paths: string seq) : IExternalSymbolProvider =
        MetadataSymbolProvider(reverseCanon, paths) :> IExternalSymbolProvider

    /// `createWith` with no primitive reverse map — BCL primitive types stay nominal
    /// classes. For a leaf that resolves no Vesper primitive surface.
    let create (paths: string seq) : IExternalSymbolProvider = createWith Map.empty paths

    /// Process-wide provider over the host runtime's assemblies. A convenience for
    /// tests (`MetadataSymbolsTests`); production composes a per-compilation leaf
    /// seeded with the harvested reverse map via `SymbolProviders`.
    let provider: IExternalSymbolProvider = create (runtimeAssemblyPaths ())
