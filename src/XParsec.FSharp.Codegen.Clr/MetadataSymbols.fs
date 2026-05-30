namespace XParsec.FSharp.Codegen.Clr

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open XParsec.FSharp.SemanticAnalysis

// Layer 2 of the symbol-resolution stack (symbol-resolution-plan §6): the
// referenced *assemblies* (BCL + binary deps), read through a
// `System.Reflection.MetadataLoadContext` over a `PathAssemblyResolver`. This is
// the ".NET provider" of the layered stack ([[project_dotnet_provider_stack]]):
// reflection-only metadata, never FCS. It resolves a BCL type
// (`System.Collections.Generic.EqualityComparer`1`) to an `ExternalTypeShape.Class`
// and its members (`Default`, `GetHashCode`) to target-agnostic `SemType`
// signatures — the substrate the `hash` milestone (M) needs (P2 → P3 → P4 → M).

/// `System.Type` → `SemType` mapping (symbol-resolution-plan §6.1). Each builder is
/// written over the *declaring type's* generic parameters: callers pass a
/// `SemType[]` (one entry per declared typar) and the builder substitutes them
/// through, exactly like `ExternalFieldShape.BuildType`. Shapes the milestone
/// doesn't model (arrays, pointers, by-refs, method-owned generic params, a
/// generic argument that itself can't map) yield `None`: the symbol is skipped,
/// never faked into a wrong `TyConst` (§6.1). The closures capture only plain data
/// (typar positions, primitive names, type names) — no live `Type` escapes, so a
/// `BuildSignature` call is pure and safe off the `MetadataLoadContext` gate.
module private MetadataMapping =

    /// IL representation full name → Vesper primitive name — the *reverse* of
    /// `IntrinsicRepr.defaults` (`"System.Int32"` → `"int"`), so a metadata
    /// `System.Int32` resolves to the same `TyConst "int"` the front end uses.
    let reprToName: Map<string, string> =
        IntrinsicRepr.defaults
        |> Map.toSeq
        |> Seq.map (fun (name, repr) -> repr, name)
        |> Map.ofSeq

    /// The open-generic-definition name (`` EqualityComparer`1 ``) for a constructed
    /// generic, else the plain full name. Used for both the `TyClass` name and a
    /// member's `Origin.DeclaringType`.
    let metadataName (t: Type) : string =
        if t.IsGenericType && not t.IsGenericTypeDefinition then
            t.GetGenericTypeDefinition().FullName
        else
            t.FullName

    let rec tryBuildType (t: Type) : (SemType[] -> SemType) option =
        if t.IsByRef || t.IsPointer || t.IsArray then
            // No `SemType` array/pointer case yet — skip rather than fake (§6.1).
            None
        elif t.IsGenericParameter then
            // A declaring-type typar substitutes the instance's i-th argument; a
            // method-owned generic parameter (`DeclaringMethod` set) has no
            // enclosing-type slot, so it's unsupported in P2.
            if isNull t.DeclaringMethod then
                let pos = t.GenericParameterPosition
                Some(fun (args: SemType[]) -> args.[pos])
            else
                None
        elif t.IsGenericType then
            // A generic type still *containing* a type parameter (e.g. the open
            // `EqualityComparer<'T>` returned by the `Default` property) has a null
            // `FullName`, so this branch must precede the `FullName` match — the
            // open-generic-definition name is always present. Each argument is
            // mapped recursively (a `'T` argument → the i-th instance arg).
            let name = t.GetGenericTypeDefinition().FullName
            let argBuilders = t.GetGenericArguments() |> Array.map tryBuildType

            if Array.exists Option.isNone argBuilders then
                None
            else
                let builders = argBuilders |> Array.map Option.get
                Some(fun args -> TyClass(name, EqArray.ofSeq (seq { for b in builders -> b args })))
        else
            match t.FullName with
            | null -> None // constructed/exotic type with no metadata full name
            | "System.Void" -> Some(fun _ -> TyConst "unit")
            | fullName when reprToName.ContainsKey fullName ->
                let name = reprToName.[fullName]
                Some(fun _ -> TyConst name)
            | fullName -> Some(fun _ -> TyClass(fullName, EqArray.empty))

    /// **Tupled** member signature `(p1 * … * pN) → ret` over the declaring type's
    /// typars — the .NET calling convention (`m(a, b)` is one application to the
    /// tuple `(a, b)`), NOT a curried `p1 → … → pN → ret` (a concrete .NET method is
    /// a single N-ary method, not curried). A zero-parameter method reads as
    /// `unit → ret`; a one-parameter method as `p → ret` (curried and tupled
    /// coincide at arity ≤ 1). Modelling N ≥ 2 tupled makes the existing front-end
    /// `unify`/`recoverTypeArgs` TyTuple arms recover the declaring typar from the
    /// element, not the whole tuple (type-args-bug.md Layer 1). `None` if any
    /// parameter or the return type doesn't map, or the method has its own generic
    /// parameters (P2 resolves no method-owned typars).
    let tryMethodSignature (m: MethodInfo) : (SemType[] -> SemType) option =
        if m.IsGenericMethodDefinition then
            None
        else
            let paramBuilders =
                m.GetParameters() |> Array.map (fun p -> tryBuildType p.ParameterType)

            let retBuilder = tryBuildType m.ReturnType

            if retBuilder.IsNone || Array.exists Option.isNone paramBuilders then
                None
            else
                let pbs = paramBuilders |> Array.map Option.get
                let rb = retBuilder.Value

                Some(fun args ->
                    let ret = rb args

                    match pbs.Length with
                    | 0 -> TyFun(TyConst "unit", ret)
                    | 1 -> TyFun(pbs.[0] args, ret)
                    | _ -> TyFun(TyTuple(EqArray.ofSeq (seq { for pb in pbs -> pb args })), ret)
                )

    /// A property reads as a value of its type (no leading arrow) — `Default` is a
    /// `EqualityComparer<'T>`, not a function. `IsProperty` tells the consumer not
    /// to expect a `TyFun`.
    let tryPropertySignature (p: PropertyInfo) : (SemType[] -> SemType) option = tryBuildType p.PropertyType

    /// A constructor reads as `(p1 * … * pN) → declType` — the .NET calling
    /// convention, same tupling as `tryMethodSignature`. The return type is the
    /// declaring type instantiated to the caller's `args` (`tryBuildType`'s
    /// generic-parameter arm makes the typars resolve positionally). `None` if
    /// any parameter or the declaring type doesn't map. A zero-parameter ctor
    /// reads as `unit → declType`. Surfaced through `extractMembers` as a
    /// member named `".ctor"`, picked up by `inferNew` / `TryEmitCtor`'s
    /// overload resolution to lower `new ExternalType(args)`.
    let tryCtorSignature (c: ConstructorInfo) : (SemType[] -> SemType) option =
        let paramBuilders =
            c.GetParameters() |> Array.map (fun p -> tryBuildType p.ParameterType)

        let retBuilder = tryBuildType c.DeclaringType

        if retBuilder.IsNone || Array.exists Option.isNone paramBuilders then
            None
        else
            let pbs = paramBuilders |> Array.map Option.get
            let rb = retBuilder.Value

            Some(fun args ->
                let ret = rb args

                match pbs.Length with
                | 0 -> TyFun(TyConst "unit", ret)
                | 1 -> TyFun(pbs.[0] args, ret)
                | _ -> TyFun(TyTuple(EqArray.ofSeq (seq { for pb in pbs -> pb args })), ret)
            )

    /// A type rendered in OPEN typars for a `SymbolKey.MemberKey.argSig`
    /// (symbol-resolution-plan §7.3): the declaring type's i-th typar is `!i`, a
    /// method-owned typar `!!i`, a constructed generic recurses, everything else is
    /// its metadata full name. The argSig only *disambiguates overloads* and is
    /// never re-parsed, so an exotic shape rendering by `Name` is harmless.
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

    /// The declaring type's `SymbolKey.TypeKey` — `(assembly, namespace,
    /// name`arity)` with the namespace stripped off the metadata name so the key's
    /// `name` is the simple `` EqualityComparer`1 `` (symbol-resolution-plan §7.3).
    let declTypeKey (t: Type) : SymbolKey =
        let asm = t.Assembly.GetName().Name |> Option.ofObj
        let full = metadataName t
        let ns = if isNull t.Namespace then "" else t.Namespace
        let simple = SymbolOrigin.StripNamespace ns full
        SymbolKey.TypeKey(asm, ns, simple)

/// `IExternalSymbolProvider` over a set of reference assembly paths, read through a
/// single shared `MetadataLoadContext`. `assemblyPaths` is the resolver's search
/// set — supplied as a compiler input (like `fsc`'s `-r:`), see
/// `MetadataSymbols.runtimeAssemblyPaths` for the current host-runtime first cut.
type MetadataSymbolProvider(assemblyPaths: string seq) =
    let paths = Seq.toArray assemblyPaths
    let mlc = new MetadataLoadContext(PathAssemblyResolver paths)

    // `MetadataLoadContext` is NOT safe for concurrent loads (symbol-resolution-plan
    // §6); every metadata access serialises through `gate`. Results are immutable
    // `SemType`-only descriptors (no live `Type` is captured — §6.1/§7.1), so the
    // result caches are read lock-free and only a miss takes the gate.
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

    /// Resolve a type by full metadata name (`` Ns.Name`arity ``): the core assembly
    /// first (where the BCL milestone types live), then — only for a
    /// namespace-qualified name — the rest of the reference set. A bare short name
    /// the core assembly doesn't define isn't a referenced-assembly type, so it
    /// short-circuits without the load-every-path scan (keeps the front end's
    /// primitive / user-type probes O(1)). Must hold `gate`.
    let resolveTypeLocked (name: string) : Type option =
        match resolveCache.TryGetValue name with
        | true, t -> t
        | _ ->
            let tryAsm (asm: Assembly) : Type option =
                try
                    asm.GetType(name, false) |> Option.ofObj
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

    /// Enumerate the public declared methods + properties of `t` whose signatures
    /// the §6.1 mapping can represent. Property accessors (`get_X` / `set_X`) are
    /// modelled through the `IsProperty = true` member and filtered out of the
    /// method walk — without this, a property `Default` would surface twice (once
    /// as `Default` and once as `get_Default`). Members the mapping can't model
    /// (open generic-method definitions, by-ref parameters, …) are skipped, not
    /// faked. Must hold `gate`.
    let enumerateClassMembers (t: Type) : ExternalMember[] =
        let origin = originOf t (Some(MetadataMapping.metadataName t))
        let declKey = MetadataMapping.declTypeKey t

        let properties =
            t.GetProperties declaredFlags
            |> Array.choose (fun p ->
                match MetadataMapping.tryPropertySignature p with
                | Some build ->
                    Some
                        {
                            Name = p.Name
                            IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                            IsProperty = true
                            BuildSignature = build
                            Origin = origin
                            Key = SymbolKey.MemberKey(declKey, p.Name, EqArray.empty, MemberKind.Property)
                        }
                | None -> None
            )

        let methods =
            t.GetMethods declaredFlags
            // `IsSpecialName` covers property getters/setters and event add/remove —
            // their first-class form is the property itself, already in `properties`.
            |> Array.filter (fun m -> not m.IsSpecialName)
            |> Array.choose (fun m ->
                MetadataMapping.tryMethodSignature m
                |> Option.map (fun build ->
                    let argSig =
                        m.GetParameters()
                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                        |> EqArray.ofArray

                    {
                        Name = m.Name
                        IsStatic = m.IsStatic
                        IsProperty = false
                        BuildSignature = build
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, m.Name, argSig, MemberKind.Method)
                    }
                )
            )

        // Constructors surface under the canonical name `".ctor"` — the same
        // name CIL uses, and the lookup key `inferNew` / `TryEmitCtor` probe
        // when lowering `new ExternalType(args)`. `t.GetMethods` excludes them
        // (an instance ctor isn't a `MethodInfo`), so a separate `GetConstructors`
        // pass is required. `IsStatic = false` always — a static `.cctor`
        // never resolves through `new`.
        let ctors =
            t.GetConstructors declaredFlags
            |> Array.choose (fun c ->
                MetadataMapping.tryCtorSignature c
                |> Option.map (fun build ->
                    let argSig =
                        c.GetParameters()
                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                        |> EqArray.ofArray

                    {
                        Name = ".ctor"
                        IsStatic = false
                        IsProperty = false
                        BuildSignature = build
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, ".ctor", argSig, MemberKind.Method)
                    }
                )
            )

        Array.concat [| properties; methods; ctors |]

    /// Build the type's interface set as `(compiled-name, type-args)` pairs over
    /// the declaring type's typars. Each interface arg goes through
    /// `tryBuildType` (it may reference the enclosing typars by position), and an
    /// interface whose args don't all map is skipped — same posture as
    /// `tryMethodSignature`. Must hold `gate`.
    let buildClassInterfaces (t: Type) : SemType[] -> (string * SemType[])[] =
        let entries =
            t.GetInterfaces()
            |> Array.choose (fun i ->
                let name = MetadataMapping.metadataName i

                let argBuilders =
                    if i.IsGenericType then
                        i.GetGenericArguments() |> Array.map MetadataMapping.tryBuildType
                    else
                        [||]

                if Array.exists Option.isNone argBuilders then
                    None
                else
                    let bs = argBuilders |> Array.map Option.get
                    Some(name, bs)
            )

        fun typeArgs -> entries |> Array.map (fun (name, bs) -> name, [| for b in bs -> b typeArgs |])

    /// Decode the type's declared base type as a builder over the declaring
    /// type's typars. Interfaces and `System.Object` itself read as `ValueNone`
    /// (an interface has no real base; `Object`'s base is the implicit root).
    /// Must hold `gate`.
    let buildClassBaseType (t: Type) : (SemType[] -> SemType) voption =
        if t.IsInterface || isNull t.BaseType then
            ValueNone
        else
            match MetadataMapping.tryBuildType t.BaseType with
            | Some build -> ValueSome build
            | None -> ValueNone

    /// `[<AllowNullLiteral>]` is F# `Microsoft.FSharp.Core.AllowNullLiteralAttribute`
    /// (emitted into metadata so reflection-only code can see it without an FSharp.Core load).
    let hasAllowNullLiteral (t: Type) : bool =
        t.CustomAttributes
        |> Seq.exists (fun a ->
            match a.AttributeType.FullName with
            | "Microsoft.FSharp.Core.AllowNullLiteralAttribute" -> true
            | _ -> false
        )

    let decodeClassFlags (t: Type) : ExternalClassFlags =
        {
            IsSealed = t.IsSealed
            IsAbstract = t.IsAbstract
            AllowNullLiteral = hasAllowNullLiteral t
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
                            Interfaces = buildClassInterfaces t
                            BaseType = buildClassBaseType t
                            Flags = decodeClassFlags t
                            Origin = originOf t None
                        }

                    ValueSome(ExternalTypeShape.Class shape)
                | None -> ValueNone
            )

    /// All overloads of `memberName` whose signature maps — the candidate set for
    /// application-site overload resolution (type-args-bug.md Layer 2). A property
    /// wins as a singleton (a property and a like-named method don't coexist as a
    /// call group — `Default` is a property). Methods are sorted most-parameters
    /// first so the singular `computeMember` reading (`Array.head`) keeps its
    /// "most-params wins" tie-break.
    let computeMembers (typeName: string) (memberName: string) : ExternalMember[] =
        lock
            gate
            (fun () ->
                match resolveTypeLocked typeName with
                | None -> [||]
                | Some t ->
                    let origin = originOf t (Some(MetadataMapping.metadataName t))
                    let declKey = MetadataMapping.declTypeKey t

                    // A property wins over a like-named method (`Default` is a property).
                    // `.ctor` is asked of `GetConstructors`, not `GetMethods` — an
                    // instance ctor isn't a `MethodInfo`, so it never appears in the
                    // method walk. Most-params-wins ordering still applies.
                    match t.GetProperty(memberName, declaredFlags) with
                    | (null: PropertyInfo) when memberName = ".ctor" ->
                        t.GetConstructors declaredFlags
                        |> Array.sortByDescending (fun c -> c.GetParameters().Length)
                        |> Array.choose (fun c ->
                            MetadataMapping.tryCtorSignature c
                            |> Option.map (fun build ->
                                let argSig =
                                    c.GetParameters()
                                    |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                                    |> EqArray.ofArray

                                {
                                    Name = ".ctor"
                                    IsStatic = false
                                    IsProperty = false
                                    BuildSignature = build
                                    Origin = origin
                                    Key = SymbolKey.MemberKey(declKey, ".ctor", argSig, MemberKind.Method)
                                }
                            )
                        )
                    | (null: PropertyInfo) ->
                        t.GetMethods declaredFlags
                        |> Array.filter (fun m -> m.Name = memberName)
                        |> Array.sortByDescending (fun m -> m.GetParameters().Length)
                        |> Array.choose (fun m ->
                            MetadataMapping.tryMethodSignature m
                            |> Option.map (fun build ->
                                let argSig =
                                    m.GetParameters()
                                    |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                                    |> EqArray.ofArray

                                {
                                    Name = memberName
                                    IsStatic = m.IsStatic
                                    IsProperty = false
                                    BuildSignature = build
                                    Origin = origin
                                    Key = SymbolKey.MemberKey(declKey, memberName, argSig, MemberKind.Method)
                                }
                            )
                        )
                    | p ->
                        match MetadataMapping.tryPropertySignature p with
                        | Some build ->
                            [|
                                {
                                    Name = memberName
                                    IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                                    IsProperty = true
                                    BuildSignature = build
                                    Origin = origin
                                    // A property carries no parameters → empty argSig.
                                    Key = SymbolKey.MemberKey(declKey, memberName, EqArray.empty, MemberKind.Property)
                                }
                            |]
                        | None -> [||]
            )

    /// The single best member by the legacy name + arity heuristic (most-params
    /// wins). Kept for the bare member-as-value path and single-candidate access;
    /// the call site resolves overloads through `computeMembers` instead.
    let computeMember (typeName: string) (memberName: string) : ExternalMember voption =
        match computeMembers typeName memberName with
        | [||] -> ValueNone
        | arr -> ValueSome arr.[0]

    interface IExternalSymbolProvider with
        // The BCL exposes no F#-style module values; type + member access is the P2
        // surface. (Static fields / literals could resolve here in a later phase.)
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

        // The BCL metadata layer contributes no implicit prelude — the ambient
        // `[<AutoOpen>]` / namespace prefixes come from the contract layer
        // (`ReferencedProject`), so this returns `[]` (intrinsic-repr-handoff.md
        // — `IAmbientOpenScope` folded into `IExternalSymbolProvider`).
        member _.AmbientOpenPrefixes = []

module MetadataSymbols =

    /// The host runtime's trusted-platform assemblies — the BCL the codegen host
    /// was launched with. **First-cut host-runtime fallback**
    /// (symbol-resolution-plan §6/§9): correct `AssemblyRef` identity wants the
    /// *target* TFM's reference pack, not the host's implementation assemblies
    /// (so `Origin.Assembly` here reads `System.Private.CoreLib`, the impl, not
    /// `System.Runtime`, the ref). TODO: take the ref-pack / `ProjectInfo.References`
    /// paths as a compiler input (a driver produces them — §9) instead of the
    /// host's TPA.
    let runtimeAssemblyPaths () : string list =
        match AppContext.GetData "TRUSTED_PLATFORM_ASSEMBLIES" with
        | :? string as tpa when tpa.Length > 0 ->
            tpa.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        | _ ->
            Directory.GetFiles(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(), "*.dll")
            |> Array.toList

    /// A provider over an explicit reference-assembly path set.
    let create (paths: string seq) : IExternalSymbolProvider =
        MetadataSymbolProvider paths :> IExternalSymbolProvider

    /// The process-wide default: one `MetadataLoadContext` over the host runtime's
    /// assemblies, caching resolved descriptors (§6/§7.1). Shared so
    /// `SymbolProviders.build` doesn't stand up a fresh metadata context per compile.
    let provider: IExternalSymbolProvider = create (runtimeAssemblyPaths ())
