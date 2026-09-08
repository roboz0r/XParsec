namespace XParsec.FSharp.Codegen.Clr

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis

// Resolves BCL types to `ExternalTypeShape` and their members to `FrozenType`
// signature templates via `System.Reflection.MetadataLoadContext`.

/// `System.Type` → `FrozenType` template mapping, over the declaring type's generic
/// parameters (`FTTypar(Type _, i)`; method-owned ones as `FTTypar(Member _, j)`).
/// A shape that doesn't map yields `None`.
module private MetadataMapping =

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

    /// `intrinsics` reconciles a platform type id to its canon (`"System.Int32"` → `int`), so a
    /// BCL member's `System.Int32` parameter presents as `int` and is callable. A name it
    /// does not reconcile is a real class.
    let rec tryBuildType (intrinsics: IntrinsicTypeMap) (t: Type) : FrozenType option =
        let go = tryBuildType intrinsics

        if t.IsByRef then
            // `in`/`out`/`ref` all collapse to `T&`. A C# `in` param's
            // `modreq(InAttribute)` is dropped, so calling one fails CLR member-ref binding.
            match go (t.GetElementType()) with
            | Some elem -> Some(FTConst(RuntimeNames.byrefKey, Block.singleton elem))
            | None -> None
        elif t.IsPointer then
            None
        elif t.IsArray then
            match go (t.GetElementType()) with
            | Some elem -> Some(FTConst(RuntimeNames.arrayKey (t.GetArrayRank()), Block.singleton elem))
            | None -> None
        elif t.IsGenericParameter then
            let pos = TyparIndex.typeSlot t.GenericParameterPosition

            if isNull t.DeclaringMethod then
                Some(FTTypar(TyparScope.Type(declTypeKey t.DeclaringType), pos))
            else
                Some(FTTypar(TyparScope.Member(declTypeKey t.DeclaringMethod.DeclaringType), pos))
        elif t.IsGenericType then
            // Open generic has null `FullName`; this branch must precede the `FullName` match.
            let name = t.GetGenericTypeDefinition().FullName
            let args = t.GetGenericArguments() |> Array.map go

            if Array.exists Option.isNone args then
                None
            else
                let frozen = args |> Array.map Option.get

                let key = SymbolKeyOps.qualifiedTypeKeyOf name frozen.Length

                Some(FTClass(key, Block.ofArray frozen))
        else
            match t.FullName with
            | null -> None // constructed/exotic type with no metadata full name
            | "System.Void" -> Some(RuntimeNames.unitTy)
            // A BCL name with a canon surfaces AS the canon: scalars
            // (`System.Int32` → `int`) and subtype roots (`System.Object` → `obj`,
            // `System.Exception` → `exn`) alike. Anything else stays a nominal `FTClass`.
            | fullName ->
                match IntrinsicTypeMap.tryCanon (PlatformTypeId fullName) intrinsics with
                | ValueSome canon -> Some(FTConst(canon, Block.empty))
                | ValueNone -> Some(FTClass(SymbolKeyOps.qualifiedTypeKeyOf fullName 0, Block.empty))

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

    /// The method's own generic-parameter count; `0` for a non-generic method.
    let methodTyparArityOf (m: MethodInfo) : int<typeSlot> =
        if m.IsGenericMethodDefinition then
            TyparIndex.typeSlot (m.GetGenericArguments().Length)
        else
            0<typeSlot>

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
            | ValueSome k -> Some(TConstValue.Integral(IntValue.zero k))
            | ValueNone ->
                match t.FullName with
                | "System.Boolean" -> Some(TConstValue.Bool false)
                | "System.Char" -> Some(TConstValue.Char '\000')
                | "System.Single" -> Some(TConstValue.Float32 0.0f)
                | "System.Double" -> Some(TConstValue.Float 0.0)
                | _ -> None

    /// Boxed `RawDefaultValue` → `TConstValue`. The box's runtime type is the only kind
    /// witness metadata gives.
    let private constOfBoxed (v: obj) : TConstValue option =
        let inline integral (n: IntValue) = Some(TConstValue.Integral n)

        match v with
        | :? bool as b -> Some(TConstValue.Bool b)
        | :? char as c -> Some(TConstValue.Char c)
        | :? sbyte as n -> integral (IntValue.SByte n)
        | :? byte as n -> integral (IntValue.Byte n)
        | :? int16 as n -> integral (IntValue.Int16 n)
        | :? uint16 as n -> integral (IntValue.UInt16 n)
        | :? int as n -> integral (IntValue.Int32 n)
        | :? uint32 as n -> integral (IntValue.UInt32 n)
        | :? int64 as n -> integral (IntValue.Int64 n)
        | :? uint64 as n -> integral (IntValue.UInt64 n)
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
    let propertySignature (declaringTyparArity: int<typeSlot>) (valueTy: FrozenType) : ExternalSignature =
        ExternalSignature.value (declaringTyparArity, 0<_>, valueTy)

    /// Method/ctor `ExternalSignature` from its `(Parameters, Return)` templates.
    let methodSignature
        (declaringTyparArity: int<typeSlot>)
        (methodTyparArity: int<typeSlot>)
        (parameters: FrozenType, ret: FrozenType)
        : ExternalSignature =
        ExternalSignature.make (declaringTyparArity, methodTyparArity, parameters, ret)


/// What one inheritance level has for a member name. A property or field `Owns` the name and
/// hides every base member of it; methods are `Overloads` and combine with the levels below.
[<RequireQualifiedAccess>]
type private LevelHit =
    | Owns of ExternalMember
    | Overloads of ExternalMember[]
    | Absent

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

    // `ValueNone` when the core assembly fails to load. Forced only under `gate`.
    let coreAssembly: Lazy<Assembly voption> =
        lazy
            (try
                ValueSome mlc.CoreAssembly
             with _ ->
                 ValueNone)

    let pathAssemblies: Lazy<Assembly[]> =
        lazy
            [|
                for p in paths do
                    match
                        (try
                            Some(mlc.LoadFromAssemblyPath p)
                         with _ ->
                             None)
                    with
                    | Some asm -> asm
                    | None -> ()
            |]

    let typeCache =
        ConcurrentDictionary<string, ExternalTypeShape voption>(StringComparer.Ordinal)

    let membersCache = ConcurrentDictionary<ExternalMemberName, Block<ExternalMember>>()

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

            let inCore =
                match coreAssembly.Value with
                | ValueSome core -> tryAsm core
                | ValueNone -> None

            let found =
                match inCore with
                | Some _ as r -> r
                | None when name.Contains '.' -> pathAssemblies.Value |> Array.tryPick tryAsm
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
    let fieldMemberOf
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (arity: int<typeSlot>)
        (f: FieldInfo)
        : ExternalMember option =
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
                    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey f.Name Block.empty 0 MemberKind.Property) with
                        IsStatic = f.IsStatic
                        Storage = MemberStorage.Field
                        ConstValue = constValue
                        Signature = MetadataMapping.propertySignature arity valueTy
                        Origin = origin
                    }
            | _ -> None

    /// A mapped method as an `ExternalMember`; `None` if its signature doesn't map.
    let methodMemberOf
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (arity: int<typeSlot>)
        (m: MethodInfo)
        : ExternalMember option =
        MetadataMapping.tryMethodSignature intrinsics m
        |> Option.map (fun (ps, ret) ->
            let argSig = Block.ofArray ps
            let methodTyparArity = MetadataMapping.methodTyparArityOf m

            { ExternalMember.OfKey(
                  SymbolKeyOps.memberKeyOf declKey m.Name argSig (int methodTyparArity) MemberKind.Method
              ) with
                IsStatic = m.IsStatic
                Signature =
                    MetadataMapping.methodSignature arity methodTyparArity (ExternalSignature.tupledParams argSig, ret)
                Origin = origin
                OptionalDefaults = MetadataMapping.optionalDefaults (m.GetParameters())
            }
        )

    /// A mapped property as an `ExternalMember`; `None` if its value type doesn't map.
    let propertyMemberOf
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (arity: int<typeSlot>)
        (p: PropertyInfo)
        : ExternalMember option =
        MetadataMapping.tryPropertySignature intrinsics p
        |> Option.map (fun valueTy ->
            { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey p.Name Block.empty 0 MemberKind.Property) with
                IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                Storage = MemberStorage.Property
                Signature = MetadataMapping.propertySignature arity valueTy
                Origin = origin
            }
        )

    /// Public declared members of `t` whose signatures map. Accessors are modelled
    /// through `Storage = Property` and filtered from the method walk. Must hold `gate`.
    let enumerateClassMembers (t: Type) : Block<ExternalMember> =
        let origin = originOf t
        let declKey = MetadataMapping.declTypeKey t
        // The declaring type's typar count, the width of the signature
        // template's declaring slots (`FTTypar(Type _, i)`, `i < arity`).
        let arity =
            if t.IsGenericType then
                TyparIndex.typeSlot (t.GetGenericArguments().Length)
            else
                0<typeSlot>

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
                    let argSig = Block.ofArray ps

                    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey "get_Item" argSig 0 MemberKind.Method) with
                        IsStatic = getter.IsStatic
                        Signature =
                            MetadataMapping.methodSignature arity 0<_> (ExternalSignature.tupledParams argSig, ret)
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
                    let argSig = Block.ofArray ps

                    ExternalMember.ctor
                        declKey
                        (MetadataMapping.methodSignature arity 0<_> (ExternalSignature.tupledParams argSig, ret))
                        argSig
                        origin
                        (MetadataMapping.optionalDefaults (c.GetParameters()))
                )
            )

        Block.ofArray (Array.concat [| properties; methods; indexers; fields; ctors |])

    /// Interface set. An interface whose own type args do not map is skipped. Must hold `gate`.
    let buildClassInterfaces (t: Type) : Block<FrozenNominal> =
        t.GetInterfaces()
        |> Array.choose (fun i ->
            match MetadataMapping.tryBuildType intrinsics i with
            | Some frozen ->
                match FrozenNominal.tryOfFrozen frozen with
                | ValueSome iface -> Some iface
                | ValueNone -> None
            | None -> None
        )
        |> Block.ofArray

    /// Declared base type as a template over the declaring typars. `ValueNone` for interfaces,
    /// for `System.Object`, and for a base the intrinsic map sends to a non-nominal (a union
    /// binding), dropped as an unmappable interface is. Must hold `gate`.
    let buildClassBaseType (t: Type) : FrozenNominal voption =
        if t.IsInterface || isNull t.BaseType then
            ValueNone
        else
            match MetadataMapping.tryBuildType intrinsics t.BaseType with
            | Some frozen -> FrozenNominal.tryOfFrozen frozen
            | None -> ValueNone

    /// The Vesper spelling of `[<AllowNullLiteral>]`, as this backend's emitted
    /// `CustomAttribute` row carries it.
    let vesperAllowNullLiteralName =
        SymbolKeyOps.typeMetaName RuntimeNames.allowNullLiteralAttributeKey

    /// `[<AllowNullLiteral>]` read off the type's `CustomAttribute` rows: FSharp.Core's
    /// spelling for an fsc-compiled assembly, the Vesper spelling for a Vesper-emitted one.
    let hasAllowNullLiteral (t: Type) : bool =
        t.CustomAttributes
        |> Seq.exists (fun a ->
            let n = a.AttributeType.FullName

            n = "Microsoft.FSharp.Core.AllowNullLiteralAttribute"
            || n = vesperAllowNullLiteralName
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
                    let typars =
                        if t.IsGenericType then
                            TyparList.typeOnly (t.GetGenericArguments() |> Seq.map (fun p -> "'" + p.Name))
                        else
                            TyparList.empty

                    let shape: ExternalClassShape =
                        {
                            Typars = typars
                            Commitment = ClassCommitment.ofIsInterface t.IsInterface
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
                                TyparIndex.typeSlot (st.GetGenericArguments().Length)
                            else
                                0<typeSlot>

                        origin, declKey, arity

                    let ctorsOn (st: Type) : ExternalMember[] =
                        let origin, declKey, arity = commonOf st

                        st.GetConstructors declaredFlags
                        |> Array.sortByDescending (fun c -> c.GetParameters().Length)
                        |> Array.choose (fun c ->
                            MetadataMapping.tryCtorSignature intrinsics c
                            |> Option.map (fun (ps, ret) ->
                                let argSig = Block.ofArray ps

                                ExternalMember.ctor
                                    declKey
                                    (MetadataMapping.methodSignature
                                        arity
                                        0<_>
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

                    let propertyOn (st: Type) : ExternalMember option =
                        let origin, declKey, arity = commonOf st

                        match st.GetProperty(memberName, declaredFlags) with
                        | (null: PropertyInfo) -> None
                        | p -> propertyMemberOf declKey origin arity p

                    let fieldOn (st: Type) : ExternalMember option =
                        let origin, declKey, arity = commonOf st

                        match st.GetField(memberName, declaredFlags) with
                        | (null: FieldInfo) -> None
                        | f -> fieldMemberOf declKey origin arity f

                    let probeLevel (st: Type) : LevelHit =
                        match propertyOn st with
                        | Some p -> LevelHit.Owns p
                        | None ->
                            match methodsOn st with
                            | [||] ->
                                match fieldOn st with
                                | Some f -> LevelHit.Owns f
                                | None -> LevelHit.Absent
                            | ms -> LevelHit.Overloads ms

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
                        |> Array.filter (fun m -> seen.Add((m.Key.ArgSig, m.Key.Kind, m.Signature.MethodTyparArity)))
                        |> Array.sortByDescending (fun m -> m.Key.ArgSig.Length)

                    // Constructors are NOT inherited, so a `.ctor` request stays on `t`.
                    if memberName = ".ctor" then
                        ctorsOn t
                    else
                        let rec resolve (methods: ExternalMember[]) (i: int) : ExternalMember[] =
                            if i >= candidates.Length then
                                dedupMethods methods
                            else
                                match probeLevel candidates.[i] with
                                | LevelHit.Owns m ->
                                    match methods with
                                    | [||] -> [| m |]
                                    | ms -> dedupMethods ms
                                | LevelHit.Overloads ms -> resolve (Array.append methods ms) (i + 1)
                                | LevelHit.Absent -> resolve methods (i + 1)

                        resolve [||] 0
            )

    let lookupTypeByName (name: string) : ExternalTypeShape voption =
        match typeCache.TryGetValue name with
        | true, v -> v
        | _ ->
            let v = computeType name
            typeCache.[name] <- v
            v

    let rec heldByModule (c: TypeContainer) : bool =
        match c with
        | TypeContainer.InModule _ -> true
        | TypeContainer.InNamespace _ -> false
        | TypeContainer.InType outer -> heldByModule outer.Container

    /// The reflection lookup for `key`, declined for a type an F# module holds:
    /// `typeMetaName` spells the module under the name its source writes, and IL nests the
    /// type under the module's compiled class, so the rendering would address nothing.
    let lookupTypeByKey (key: TypeKey) : ExternalTypeShape voption =
        if heldByModule key.Container then
            ValueNone
        else
            lookupTypeByName (SymbolKeyOps.typeMetaName key)

    /// Every `(namespace, plain name, arity)` the reference set exports at top level,
    /// forwarded types included: `asm.GetType` follows a type forwarder, so the directory
    /// must too. Identity alone; a slot's shape resolves on demand.
    let directorySlots () : seq<struct (string * string * int)> =
        lock
            gate
            (fun () ->
                let slots = HashSet<struct (string * string * int)>()

                // Top level only: `ModuleContainer` spells `InNamespace` and `InModule`, so a
                // nested type stays out. `MetadataSymbolsTests` pins the gap that leaves.
                let addType (t: Type) =
                    if not t.IsNested && t.IsVisible then
                        let ns =
                            match t.Namespace with
                            | null -> ""
                            | ns -> ns

                        slots.Add(struct (ns, SymbolKeyOps.bareName t.Name, t.GetGenericArguments().Length))
                        |> ignore

                // A forward whose target assembly is missing loads partially; the resolved
                // types survive.
                let salvage (f: unit -> Type[]) : Type[] =
                    try
                        f ()
                    with
                    | :? ReflectionTypeLoadException as e -> e.Types |> Array.filter (isNull >> not)
                    | _ -> [||]

                let addAssembly (asm: Assembly) =
                    salvage asm.GetExportedTypes |> Array.iter addType
                    salvage asm.GetForwardedTypes |> Array.iter addType

                match coreAssembly.Value with
                | ValueSome core -> addAssembly core
                | ValueNone -> ()

                Array.iter addAssembly pathAssemblies.Value

                Seq.toArray slots :> seq<_>
            )

    let scope =
        ScopeContents.typeDirectory directorySlots (fun key -> lookupTypeByName (SymbolKeyOps.typeMetaName key))

    member private _.LookupMembersByName(key: ExternalMemberName) =
        match membersCache.TryGetValue key with
        | true, v -> v
        | _ ->
            let v = Block.ofArray (computeMembers key.DeclaringType key.Name)
            membersCache.[key] <- v
            v

    interface IExternalSymbolProvider

    interface IExternalSymbolResolver with
        member _.Scope = scope

        // The metadata layer scrapes IL, never F# record tycons, so it never contributes to
        // the reverse field index (F#'s `isILOrRequiredQualifiedAccess` excludes IL too).
        member _.TryRecordsWithField _ = Block.empty
        // GAP: a referenced assembly's `[<assembly: AutoOpen>]` rows are dropped, so an
        // fsc-built F# reference loses its prelude. Pinned by MetadataSymbolsTests, "GAP: a
        // reference assembly's [<assembly: AutoOpen>] rows reach ImplicitOpens".
        member _.ImplicitOpens = []

    interface IExternalSymbolStore with
        // The caches address a reflection name, so a key is rendered before the read.
        member _.TryLookupType(key: TypeKey) = lookupTypeByKey key

        // GAP: no `CustomAttribute` row is decoded into `TAttributes`, so each attribute
        // needed off a referenced declaration gets its own flag, the way `hasAllowNullLiteral`
        // does. Pinned by MetadataSymbolsTests, "GAP: a referenced type's CustomAttribute
        // rows reach TryLookupAttributes".
        member _.TryLookupAttributes _ = Block.empty

        member this.TryLookupMembers(key, memberName) =
            this.LookupMembersByName(
                ExternalMemberName.ofKeyed
                    {
                        DeclaringType = key
                        Name = memberName
                    }
            )

        // The cache is keyed by (declaring name, member NAME), so a key is resolved by
        // exact-identity selection out of the name's overload set; a best-by-arity
        // collapse would return a SIBLING overload's entry.
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
        // The metadata layer models types and their members alone; a free function is
        // outside what IL declares.
        member _.TryLookupByKey _ = ValueNone
        // The metadata layer CONSUMES the axis to canonicalize BCL names; it declares none.
        member _.IntrinsicTypeMap = IntrinsicTypeMap.empty

        // .NET metadata IS the platform, so this source supplies the platform facts.
        member this.Platform = ValueSome(this :> IPlatformFacts)

    interface IPlatformFacts with

        // `Vesper.int` is a value type here because the type its `.clr.fs` binds,
        // `System.Int32`, is one. A canon declared UNSUPPORTED on this target has no type to
        // reflect; any other key is reflected under its plain metadata name.
        member _.IsValueType(key: TypeKey) =
            let reflected (name: string) =
                match lookupTypeByName name with
                | ValueSome(ExternalTypeShape.Class shape) -> ValueSome shape.Flags.IsValueType
                | _ -> ValueNone

            match IntrinsicTypeMap.tryPlatform key intrinsics with
            | ValueSome(IntrinsicPlatform.Bound typeId) -> reflected typeId.Value
            | ValueSome(IntrinsicPlatform.Unsupported _) -> ValueNone
            | ValueNone ->
                match lookupTypeByKey key with
                | ValueSome(ExternalTypeShape.Class shape) -> ValueSome shape.Flags.IsValueType
                | _ -> ValueNone

        member _.TupleType(arity: int) =
            if ClrTuples.isTupleArity arity then
                ValueSome(ClrTuples.typeKey arity)
            else
                ValueNone

        member _.ConstEncoding(declared: FrozenType, value: TConstExpr) =
            match AttributeBlob.tryUnencodable declared value with
            | ValueNone -> ConstEncoding.Encodable
            | ValueSome ty -> ConstEncoding.Unencodable ty

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
