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
                Some(fun args -> TyClass(name, [ for b in builders -> b args ]))
        else
            match t.FullName with
            | null -> None // constructed/exotic type with no metadata full name
            | "System.Void" -> Some(fun _ -> TyConst "unit")
            | fullName when reprToName.ContainsKey fullName ->
                let name = reprToName.[fullName]
                Some(fun _ -> TyConst name)
            | fullName -> Some(fun _ -> TyClass(fullName, []))

    /// Curried `arg1 → … → argN → ret` over the declaring type's typars; a
    /// zero-parameter method reads as `unit → ret`. `None` if any parameter or the
    /// return type doesn't map, or the method has its own generic parameters (P2
    /// resolves no method-owned typars).
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

                    if pbs.Length = 0 then
                        TyFun(TyConst "unit", ret)
                    else
                        Array.foldBack (fun (pb: SemType[] -> SemType) acc -> TyFun(pb args, acc)) pbs ret
                )

    /// A property reads as a value of its type (no leading arrow) — `Default` is a
    /// `EqualityComparer<'T>`, not a function. `IsProperty` tells the consumer not
    /// to expect a `TyFun`.
    let tryPropertySignature (p: PropertyInfo) : (SemType[] -> SemType) option = tryBuildType p.PropertyType

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

                    ValueSome(ExternalTypeShape.Class(arity, t.IsInterface, originOf t None))
                | None -> ValueNone
            )

    let computeMember (typeName: string) (memberName: string) : ExternalMember voption =
        lock
            gate
            (fun () ->
                match resolveTypeLocked typeName with
                | None -> ValueNone
                | Some t ->
                    let origin = originOf t (Some(MetadataMapping.metadataName t))

                    // A property wins over a like-named method (`Default` is a property).
                    let asProperty =
                        match t.GetProperty(memberName, declaredFlags) with
                        | null -> None
                        | p ->
                            MetadataMapping.tryPropertySignature p
                            |> Option.map (fun build ->
                                {
                                    Name = memberName
                                    IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                                    IsProperty = true
                                    BuildSignature = build
                                    Origin = origin
                                }
                            )

                    let resolved =
                        match asProperty with
                        | Some _ -> asProperty
                        | None ->
                            // First overload whose whole signature maps; prefer the
                            // most-parameters one so `GetHashCode(T)` wins over an
                            // inherited-shape `GetHashCode()` were both ever declared.
                            t.GetMethods declaredFlags
                            |> Array.filter (fun m -> m.Name = memberName)
                            |> Array.sortByDescending (fun m -> m.GetParameters().Length)
                            |> Array.tryPick (fun m ->
                                MetadataMapping.tryMethodSignature m
                                |> Option.map (fun build ->
                                    {
                                        Name = memberName
                                        IsStatic = m.IsStatic
                                        IsProperty = false
                                        BuildSignature = build
                                        Origin = origin
                                    }
                                )
                            )

                    match resolved with
                    | Some m -> ValueSome m
                    | None -> ValueNone
            )

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
