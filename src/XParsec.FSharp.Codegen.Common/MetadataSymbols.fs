namespace XParsec.FSharp.Codegen.Common

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open System.Reflection
open XParsec.FSharp.SemanticAnalysis

// Layer 2 of the symbol-resolution stack: the
// referenced *assemblies* (BCL + binary deps), read through a
// `System.Reflection.MetadataLoadContext` over a `PathAssemblyResolver`. This is
// the ".NET provider" of the layered stack ([[project_dotnet_provider_stack]]):
// reflection-only metadata, never FCS. It resolves a BCL type
// (`System.Collections.Generic.EqualityComparer`1`) to an `ExternalTypeShape.Class`
// and its members (`Default`, `GetHashCode`) to target-agnostic `FrozenType`
// signature templates.

/// `System.Type` → `FrozenType` template mapping. Each template is written over
/// the *declaring type's* generic parameters, baked as `FTTypar(Declaring,i)`
/// (and method-owned typars as `FTTypar(Method,j)`): a consumer substitutes its
/// declaring args (one per declared typar) through the placeholders. Shapes
/// the milestone doesn't model (arrays, pointers, by-refs, a generic argument
/// that itself can't map) yield `None`: the symbol is skipped, never faked into a
/// wrong `TyConst`. The templates are inert data (typar positions, primitive
/// names, type keys) — no live `Type` escapes, so building them is pure and safe
/// off the `MetadataLoadContext` gate.
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

    let rec tryBuildType (t: Type) : FrozenType option =
        if t.IsByRef then
            // A managed by-ref (`T&`) maps onto the byref intrinsic
            // `FTConst(byrefName, [elem])`, mirroring the array intrinsic below — so
            // a byref-returning member (e.g. `Span<T>.get_Item : T&`) and a byref/
            // `out`/`ref` *parameter* (`Int32.TryParse(string, int&)`, PP5d) resolve
            // instead of being dropped. Legal only in param / return position; the
            // encoder emits `ELEMENT_TYPE_BYREF` at that seam, never inside the
            // recursive type encoder (PP2b).
            //
            // TODO(inref): direction-agnostic — `in`/`out`/`ref` all collapse to the
            // same `T&` here. A C# `in` parameter additionally carries a *required
            // custom modifier* `modreq(System.Runtime.InteropServices.InAttribute)`
            // (via `t`'s declaring `ParameterInfo.GetRequiredCustomModifiers()`),
            // which this drops. Since the CLR matches member-ref signatures
            // including modreqs, calling a BCL method with an `in` parameter would
            // fail to bind until that modifier is threaded through to the encoder
            // (`mintMemberRef`, ClrExternalMembers.fs — paired TODO there). No
            // current consumer (`Formatter`/printf) hits an `in` parameter, so it is
            // unbuilt + untested rather than wrong.
            match tryBuildType (t.GetElementType()) with
            | Some elem -> Some(FTConst(RuntimeNames.byrefName, EqArray.singleton elem))
            | None -> None
        elif t.IsPointer then
            // Unmanaged pointers stay unrepresentable — skip rather than fake (§6.1).
            None
        elif t.IsArray then
            // A reflection array maps onto Vesper's generic array intrinsic
            // `FTConst(arrayName rank, [elem])` (rank 1 → `"[]"`), the same repr the
            // front end uses for `'T[]`. This lets array-returning BCL members (e.g.
            // `List`1::ToArray() : T[]`) resolve instead of being dropped.
            match tryBuildType (t.GetElementType()) with
            | Some elem -> Some(FTConst(RuntimeNames.arrayName (t.GetArrayRank()), EqArray.singleton elem))
            | None -> None
        elif t.IsGenericParameter then
            // A declaring-type typar bakes as `FTTypar(Declaring, pos)` (the
            // consumer substitutes its pos-th declaring arg); a method-owned generic
            // parameter (`DeclaringMethod` set) lives on the *method* axis —
            // `FTTypar(Method, pos)`. The method axis is
            // intrinsic to the member, so the position is fixed and the open node
            // rides straight through. A consumer instantiates it to a fresh
            // inference var per call site; codegen encodes it as `!!pos`.
            let pos = t.GenericParameterPosition

            if isNull t.DeclaringMethod then
                Some(FTTypar(TyparAxis.Declaring, pos))
            else
                Some(FTTypar(TyparAxis.Method, pos))
        elif t.IsGenericType then
            // A generic type still *containing* a type parameter (e.g. the open
            // `EqualityComparer<'T>` returned by the `Default` property) has a null
            // `FullName`, so this branch must precede the `FullName` match — the
            // open-generic-definition name is always present. Each argument is
            // mapped recursively (a `'T` argument → `FTTypar(Declaring, i)`).
            let name = t.GetGenericTypeDefinition().FullName
            let args = t.GetGenericArguments() |> Array.map tryBuildType

            if Array.exists Option.isNone args then
                None
            else
                let frozen = args |> Array.map Option.get
                // Stamp the type's home assembly (its defining assembly's simple
                // name) so this key unifies with the same BCL type resolved via a
                // provider shape's origin.
                let key =
                    SymbolKeyOps.qualifiedTypeKeyOf (Some(t.Assembly.GetName().Name)) name frozen.Length

                Some(FTClass(key, EqArray.ofArray frozen))
        else
            match t.FullName with
            | null -> None // constructed/exotic type with no metadata full name
            | "System.Void" -> Some(FTConst("unit", EqArray.empty))
            | fullName when reprToName.ContainsKey fullName -> Some(FTConst(reprToName.[fullName], EqArray.empty))
            | fullName ->
                Some(
                    FTClass(SymbolKeyOps.qualifiedTypeKeyOf (Some(t.Assembly.GetName().Name)) fullName 0, EqArray.empty)
                )

    /// The `.NET`-tupled parameter template `(p1 * … * pN)` from the per-parameter
    /// templates: `N = 0` → `unit`, `N = 1` → the bare parameter, `N ≥ 2` → one
    /// `FTTuple` (the .NET calling convention — `m(a, b)` is one application to the
    /// tuple `(a, b)`, not a curried `p1 → … → pN`). Modelling N ≥ 2 tupled makes
    /// the front-end `unify` and the codegen `recoverOpenTypars` `Tuple` arms
    /// recover the declaring typar from the element, not the whole tuple. (Equal,
    /// by construction, to splitting a `TyFun(params, ret)` template's argument —
    /// the contract layer does the same split when freezing a member signature.)
    let frozenParams (ps: FrozenType[]) : FrozenType =
        match ps.Length with
        | 0 -> FTConst("unit", EqArray.empty)
        | 1 -> ps.[0]
        | _ -> FTTuple(EqArray.ofArray ps)

    /// The tupled member signature as `(Parameters, Return)` templates over the
    /// declaring type's typars. `None` if any parameter or the return type doesn't
    /// map. A generic method definition (`Take<TSource>`) is not skipped: its
    /// method-owned typars bake as `FTTypar(Method, j)` through `tryBuildType`.
    let tryMethodSignature (m: MethodInfo) : (FrozenType * FrozenType) option =
        let paramTys =
            m.GetParameters() |> Array.map (fun p -> tryBuildType p.ParameterType)

        let retTy = tryBuildType m.ReturnType

        if retTy.IsNone || Array.exists Option.isNone paramTys then
            None
        else
            Some(frozenParams (paramTys |> Array.map Option.get), retTy.Value)

    /// The method's own generic-parameter count — the method axis arity stamped
    /// onto `ExternalMember.MethodArity`. `0` for a non-generic method.
    let methodArityOf (m: MethodInfo) : int =
        if m.IsGenericMethodDefinition then
            m.GetGenericArguments().Length
        else
            0

    /// The zero value of a primitive value type as a `TConstValue` — the default an
    /// `[<Optional>]` parameter with no `[<DefaultParameterValue>]` receives
    /// (`default(T)`). `None` for a reference type (its default is `null`, which has
    /// no `TConstValue`) or a non-primitive struct (no representable constant), which
    /// ends an optional-parameter run rather than being faked.
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

    /// A boxed constant (a parameter's `RawDefaultValue`) as a `TConstValue`. The
    /// unsigned forms fold onto the matching signed `TConstValue` — the IL constant
    /// is bit-identical and only disambiguates an *omitted* argument, never re-typed.
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

    /// The compile-time-constant defaults of a member's *trailing* optional
    /// parameters, in declaration order — surfaced on `ExternalMember.OptionalDefaults`
    /// so a call may omit any suffix of them. Walks parameters from the end: an
    /// optional parameter contributes its explicit constant default
    /// (`[<DefaultParameterValue>]`) or, lacking one, the zero of a primitive value
    /// type (`[<Optional>]` alone ⇒ `default(T)`). The walk stops at the first
    /// parameter that is not optional or whose default isn't a representable constant
    /// (a `null` reference default, a non-primitive `default(struct)`), so the
    /// surfaced list is exactly the omittable suffix.
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

    /// A property reads as a value of its type (no leading arrow) — `Default` is a
    /// `EqualityComparer<'T>`, not a function. `IsProperty` tells the consumer not
    /// to expect a `TyFun`.
    let tryPropertySignature (p: PropertyInfo) : FrozenType option = tryBuildType p.PropertyType

    /// A constructor reads as `(p1 * … * pN) → declType` — the .NET calling
    /// convention, same tupling as `tryMethodSignature`. The return type is the
    /// declaring type's open template (`tryBuildType`'s generic-parameter arm bakes
    /// the typars positionally). `None` if any parameter or the declaring type
    /// doesn't map. A zero-parameter ctor reads as `unit → declType`. Surfaced
    /// through `extractMembers` as a member named `".ctor"`, picked up by
    /// `inferNew` / `TryEmitCtor`'s overload resolution to lower
    /// `new ExternalType(args)`.
    let tryCtorSignature (c: ConstructorInfo) : (FrozenType * FrozenType) option =
        let paramTys =
            c.GetParameters() |> Array.map (fun p -> tryBuildType p.ParameterType)

        let retTy = tryBuildType c.DeclaringType

        if retTy.IsNone || Array.exists Option.isNone paramTys then
            None
        else
            Some(frozenParams (paramTys |> Array.map Option.get), retTy.Value)

    /// A type rendered in OPEN typars for a `SymbolKey.MemberKey.argSig`
    /// the declaring type's i-th typar is `!i`, a
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

    /// Assemble a property's two-axis `ExternalSignature` template: no parameters
    /// (`Parameters = unit`), the value type in `Return`, no method axis.
    let propertySignature (declaringArity: int) (valueTy: FrozenType) : ExternalSignature =
        {
            DeclaringArity = declaringArity
            MethodArity = 0
            Parameters = FTConst("unit", EqArray.empty)
            Return = valueTy
        }

    /// Assemble a method / ctor's two-axis `ExternalSignature` template from its
    /// `(Parameters, Return)` templates.
    let methodSignature
        (declaringArity: int)
        (methodArity: int)
        (parameters: FrozenType, ret: FrozenType)
        : ExternalSignature =
        {
            DeclaringArity = declaringArity
            MethodArity = methodArity
            Parameters = parameters
            Return = ret
        }

    /// The declaring type's `SymbolKey.TypeKey` — `(assembly, namespace,
    /// name`arity)` with the namespace stripped off the metadata name so the key's
    /// `name` is the simple `` EqualityComparer`1 ``.
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

    // `MetadataLoadContext` is NOT safe for concurrent loads; every metadata access
    // serialises through `gate`. Results are immutable `FrozenType`-template
    // descriptors (no live `Type` is captured — §6.1/§7.1), so the result caches
    // are read lock-free and only a miss takes the gate.
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
            // Only surface types a consumer could actually reference: an external
            // assembly's *internal* (or private-nested) types are invisible across
            // the assembly boundary, so resolving them here is unsound — it lets an
            // unrelated assembly's internal type shadow a locally-declared one of the
            // same name (e.g. the C# `Vesper.Printf`'s `internal Vesper.Doc` family
            // shadowing the Vesper-compiled `structural-printer.fs` `Doc` DU). `Type.IsVisible`
            // is true iff the type is public top-level or public-nested in a visible
            // chain — exactly the externally-referenceable set.
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
        // The declaring type's typar count — the width of the signature
        // template's declaring axis (`FTTypar(Declaring,i)`, `i < arity`).
        let arity =
            if t.IsGenericType then
                t.GetGenericArguments().Length
            else
                0

        let properties =
            t.GetProperties declaredFlags
            |> Array.choose (fun p ->
                match MetadataMapping.tryPropertySignature p with
                | Some valueTy ->
                    Some
                        {
                            Name = p.Name
                            IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                            IsProperty = true
                            Signature = MetadataMapping.propertySignature arity valueTy
                            MethodArity = 0
                            Origin = origin
                            Key = SymbolKey.MemberKey(declKey, p.Name, EqArray.empty, MemberKind.Property)
                            OptionalDefaults = []
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
                |> Option.map (fun (ps, ret) ->
                    let argSig =
                        m.GetParameters()
                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                        |> EqArray.ofArray

                    let methodArity = MetadataMapping.methodArityOf m

                    {
                        Name = m.Name
                        IsStatic = m.IsStatic
                        IsProperty = false
                        Signature = MetadataMapping.methodSignature arity methodArity (ps, ret)
                        MethodArity = methodArity
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, m.Name, argSig, MemberKind.Method)
                        OptionalDefaults = MetadataMapping.optionalDefaults (m.GetParameters())
                    }
                )
            )

        // Indexers (`this[i]`) surface under their accessor's CIL name `get_Item`
        // as an ordinary *method* member carrying the index parameter(s) + the
        // (possibly by-ref) element return — NOT as a parameterless property, whose
        // `propertySignature` shape can't model the index argument. The `Item`
        // property itself is skipped by the property walk above whenever its getter
        // returns by-ref (`Span<T>.Item : T&`, unmappable as a value type), so this
        // is the only surface for a ref-returning indexer; for a by-value indexer it
        // is additive (the lookup names `get_Item` vs `Item` don't collide). The
        // front-end indexer dispatch (`inferIndexedLookup`) probes `get_Item`.
        let indexers =
            t.GetProperties declaredFlags
            |> Array.filter (fun p -> p.GetIndexParameters().Length > 0 && not (isNull p.GetMethod))
            |> Array.choose (fun p ->
                let getter = p.GetMethod

                MetadataMapping.tryMethodSignature getter
                |> Option.map (fun (ps, ret) ->
                    let argSig =
                        getter.GetParameters()
                        |> Array.map (fun ip -> MetadataMapping.openTyparSig ip.ParameterType)
                        |> EqArray.ofArray

                    {
                        Name = "get_Item"
                        IsStatic = getter.IsStatic
                        IsProperty = false
                        Signature = MetadataMapping.methodSignature arity 0 (ps, ret)
                        MethodArity = 0
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, "get_Item", argSig, MemberKind.Method)
                        OptionalDefaults = []
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
                |> Option.map (fun (ps, ret) ->
                    let argSig =
                        c.GetParameters()
                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                        |> EqArray.ofArray

                    {
                        Name = ".ctor"
                        IsStatic = false
                        IsProperty = false
                        Signature = MetadataMapping.methodSignature arity 0 (ps, ret)
                        MethodArity = 0
                        Origin = origin
                        Key = SymbolKey.MemberKey(declKey, ".ctor", argSig, MemberKind.Method)
                        OptionalDefaults = MetadataMapping.optionalDefaults (c.GetParameters())
                    }
                )
            )

        Array.concat [| properties; methods; indexers; ctors |]

    /// Build the type's interface set as `(compiled-name, type-args)` template
    /// pairs over the declaring type's typars. Each interface arg goes through
    /// `tryBuildType` (it may reference the enclosing typars by position, baked as
    /// `FTTypar(Declaring,i)`), and an interface whose args don't all map is
    /// skipped — same posture as `tryMethodSignature`. Must hold `gate`.
    let buildClassInterfaces (t: Type) : (string * FrozenType[])[] =
        t.GetInterfaces()
        |> Array.choose (fun i ->
            let name = MetadataMapping.metadataName i

            let args =
                if i.IsGenericType then
                    i.GetGenericArguments() |> Array.map MetadataMapping.tryBuildType
                else
                    [||]

            if Array.exists Option.isNone args then
                None
            else
                Some(name, args |> Array.map Option.get)
        )

    /// Decode the type's declared base type as a `FrozenType` template over the
    /// declaring type's typars. Interfaces and `System.Object` itself read as
    /// `ValueNone` (an interface has no real base; `Object`'s base is the implicit
    /// root). Must hold `gate`.
    let buildClassBaseType (t: Type) : FrozenType voption =
        if t.IsInterface || isNull t.BaseType then
            ValueNone
        else
            match MetadataMapping.tryBuildType t.BaseType with
            | Some frozen -> ValueSome frozen
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
                            // The interface / base-type templates carry the declaring
                            // typars as `FTTypar(Declaring,i)` directly.
                            FrozenInterfaces = buildClassInterfaces t
                            FrozenBaseType = buildClassBaseType t
                            Flags = decodeClassFlags t
                            Origin = originOf t None
                        }

                    ValueSome(ExternalTypeShape.Class shape)
                | None -> ValueNone
            )

    /// All overloads of `memberName` whose signature maps — the candidate set for
    /// application-site overload resolution. A property
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
                    // A property wins over a like-named method (`Default` is a property).
                    // `.ctor` is asked of `GetConstructors`, not `GetMethods` — an
                    // instance ctor isn't a `MethodInfo`, so it never appears in the
                    // method walk. Most-params-wins ordering still applies.
                    //
                    // `declaredFlags` carries `DeclaredOnly`, correct for classes (a
                    // class's `GetMethods` walks its inheritance chain, but the BCL
                    // milestone types keep their members where queried). An *interface*,
                    // though, does not inherit members through `DeclaredOnly`:
                    // `IEnumerator`1` declares `Current` but inherits `MoveNext`/`Reset`
                    // from the non-generic `IEnumerator` and `Dispose` from
                    // `IDisposable`. So for an interface, search `t` then its full
                    // transitive interface set, taking the first that has the member.
                    // Each matched member's `declKey`/`origin` come from *its* declaring
                    // interface (`IEnumerator` for `MoveNext`), so codegen mints the
                    // `callvirt` against the correct interface slot.
                    let lookupOn (st: Type) : ExternalMember[] =
                        let origin = originOf st (Some(MetadataMapping.metadataName st))
                        let declKey = MetadataMapping.declTypeKey st

                        let arity =
                            if st.IsGenericType then
                                st.GetGenericArguments().Length
                            else
                                0

                        match st.GetProperty(memberName, declaredFlags) with
                        | (null: PropertyInfo) when memberName = ".ctor" ->
                            st.GetConstructors declaredFlags
                            |> Array.sortByDescending (fun c -> c.GetParameters().Length)
                            |> Array.choose (fun c ->
                                MetadataMapping.tryCtorSignature c
                                |> Option.map (fun (ps, ret) ->
                                    let argSig =
                                        c.GetParameters()
                                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                                        |> EqArray.ofArray

                                    {
                                        Name = ".ctor"
                                        IsStatic = false
                                        IsProperty = false
                                        Signature = MetadataMapping.methodSignature arity 0 (ps, ret)
                                        MethodArity = 0
                                        Origin = origin
                                        Key = SymbolKey.MemberKey(declKey, ".ctor", argSig, MemberKind.Method)
                                        OptionalDefaults = MetadataMapping.optionalDefaults (c.GetParameters())
                                    }
                                )
                            )
                        | (null: PropertyInfo) ->
                            st.GetMethods declaredFlags
                            |> Array.filter (fun m -> m.Name = memberName)
                            |> Array.sortByDescending (fun m -> m.GetParameters().Length)
                            |> Array.choose (fun m ->
                                MetadataMapping.tryMethodSignature m
                                |> Option.map (fun (ps, ret) ->
                                    let argSig =
                                        m.GetParameters()
                                        |> Array.map (fun p -> MetadataMapping.openTyparSig p.ParameterType)
                                        |> EqArray.ofArray

                                    let methodArity = MetadataMapping.methodArityOf m

                                    {
                                        Name = memberName
                                        IsStatic = m.IsStatic
                                        IsProperty = false
                                        Signature = MetadataMapping.methodSignature arity methodArity (ps, ret)
                                        MethodArity = methodArity
                                        Origin = origin
                                        Key = SymbolKey.MemberKey(declKey, memberName, argSig, MemberKind.Method)
                                        OptionalDefaults = MetadataMapping.optionalDefaults (m.GetParameters())
                                    }
                                )
                            )
                        | p ->
                            match MetadataMapping.tryPropertySignature p with
                            | Some valueTy ->
                                [|
                                    {
                                        Name = memberName
                                        IsStatic = (not (isNull p.GetMethod) && p.GetMethod.IsStatic)
                                        IsProperty = true
                                        Signature = MetadataMapping.propertySignature arity valueTy
                                        MethodArity = 0
                                        Origin = origin
                                        // A property carries no parameters → empty argSig.
                                        Key =
                                            SymbolKey.MemberKey(
                                                declKey,
                                                memberName,
                                                EqArray.empty,
                                                MemberKind.Property
                                            )
                                        OptionalDefaults = []
                                    }
                                |]
                            | None -> [||]

                    // Classes: members live where queried (DeclaredOnly is correct).
                    // Interfaces: union `t` with its transitive base interfaces and take
                    // the first that resolves the member (`GetInterfaces` returns the
                    // full set — `IEnumerator`1` → `IEnumerator` + `IDisposable`).
                    if t.IsInterface then
                        Array.append [| t |] (t.GetInterfaces())
                        |> Array.tryPick (fun st ->
                            match lookupOn st with
                            | [||] -> None
                            | arr -> Some arr
                        )
                        |> Option.defaultValue [||]
                    else
                        lookupOn t
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

        // The BCL models no F#-style discriminated unions whose cases we
        // construct (FSharp.Core's `Option`/`Result`/`Choice` come from the
        // Vesper contract layer, not from metadata), so there is nothing to
        // index here — union construction is a contract-layer capability.
        member _.TryLookupUnionCase _ = ValueNone

        // The BCL metadata layer contributes no implicit prelude — the ambient
        // `[<AutoOpen>]` / namespace prefixes come from the contract layer
        // (`ReferencedProject`), so this returns `[]`.
        member _.AmbientOpenPrefixes = []

        // BCL metadata exposes compiled members, never spliceable F# `inline`
        // bodies — those ride the Vesper contract stack.
        member _.TryLookupInlineBody _ = ValueNone
        member _.TryLookupInlineBodyByName _ = ValueNone

module MetadataSymbols =

    /// The host runtime's trusted-platform assemblies — the BCL the codegen host
    /// was launched with. **First-cut host-runtime fallback**
    /// correct `AssemblyRef` identity wants the
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
