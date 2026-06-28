namespace XParsec.FSharp.Codegen.Js

open System.IO

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest

/// Layer-2 provider backed by a TS-derived JSON manifest (`Vesper.Ts.Manifest`),
/// the consumer end of the extractor→manifest→provider slice. It is the JS analog
/// of `MetadataSymbols` (which reads .NET assemblies via `MetadataLoadContext`):
/// here the "metadata oracle" is the serialised manifest the TS extractor emitted,
/// and this maps its type-description grammar into the seam's `ExternalTypeShape`
/// / `ExternalSymbol` / `FrozenType`. Mirrors `JsNativeSymbols` structurally.
///
/// MVP scope: non-generic `Interface` members (primitive-typed) + free
/// `Function`s. `Class` is handled too; the remaining grammar (unions, dynamic,
/// structural, generics, import shapes) is mapped conservatively or deferred —
/// see the inline TODOs.
module TsManifestProvider =

    // ─── TypeRef → FrozenType (member signature templates) ─────────────────

    let rec private toFrozen (t: Schema.TypeRef) : FrozenType =
        match t with
        | Schema.TypeRef.Named(name, []) -> FTConst(name, EqArray.empty)
        | Schema.TypeRef.Named(name, args) -> FTConst(name, EqArray.ofSeq (List.map toFrozen args))
        | Schema.TypeRef.Typar i -> FTTypar(TyparAxis.Declaring, i)
        | Schema.TypeRef.Fun(args, ret) -> List.foldBack (fun a acc -> FTFun(toFrozen a, acc)) args (toFrozen ret)
        | Schema.TypeRef.Tuple items -> FTTuple(EqArray.ofSeq (List.map toFrozen items))
        | Schema.TypeRef.Union members -> FTOr(EqArray.ofSeq (List.map toFrozen members))
        | Schema.TypeRef.Dynamic -> FTUnknown "any" // TODO: TyDynamic once it lands
        | Schema.TypeRef.Structural(hash, _) -> FTUnknown("structural:" + hash) // TODO: content-hash record

    // ─── TypeRef → SemType (free-function Instantiate result) ──────────────
    // TODO: `toSem` and `toFrozen` are deliberately parallel recursions over the
    // SAME `TypeRef` grammar, differing only in target constructor per arm
    // (FrozenType templates for members, SemType for free-fn symbols — see the
    // two external-symbol APIs). KEEP THEM IN SYNC: a new `TypeRef` case must be
    // handled in both. If a third arm ever needs to genuinely diverge, that is
    // the signal the FrozenType/SemType split is leaking and wants a shared map.

    let rec private toSem (t: Schema.TypeRef) : SemType =
        match t with
        | Schema.TypeRef.Named(name, []) -> TyConst(name, EqArray.empty)
        | Schema.TypeRef.Named(name, args) -> TyConst(name, EqArray.ofSeq (List.map toSem args))
        | Schema.TypeRef.Typar _ -> TyUnknown "?ts-typar" // MVP free fns are monomorphic
        | Schema.TypeRef.Fun(args, ret) -> List.foldBack (fun a acc -> TyFun(toSem a, acc)) args (toSem ret)
        | Schema.TypeRef.Tuple items -> TyTuple(EqArray.ofSeq (List.map toSem items))
        | Schema.TypeRef.Union members -> SemType.MkUnion(List.map toSem members)
        | Schema.TypeRef.Dynamic -> TyUnknown "any"
        | Schema.TypeRef.Structural(hash, _) -> TyUnknown("structural:" + hash)

    let private unitFrozen: FrozenType = FTConst("unit", EqArray.empty)

    /// .NET-tupled parameter encoding: 0 → unit, 1 → bare, N≥2 → tuple.
    let private paramsFrozen (ps: Schema.Param list) : FrozenType =
        match ps with
        | [] -> unitFrozen
        | [ p ] -> toFrozen p.Type
        | many -> FTTuple(EqArray.ofSeq (many |> List.map (fun p -> toFrozen p.Type)))

    let private singleSignature (name: string) (sigs: Schema.Signature list) : Schema.Signature =
        // Prototype: handle the one-signature case only. Throw (rather than
        // silently picking the first / synthesizing a default) so an overload set
        // surfaces a stack trace pointing at exactly what to implement — full
        // overload sets will ride TryLookupMembers once the extractor emits them.
        match sigs with
        | [ s ] -> s
        | [] -> failwithf "symbol '%s' has no call signature" name
        | _ -> failwithf "symbol '%s' has %d overloads; overload sets not yet supported" name (List.length sigs)

    /// The overload-identity string a single parameter contributes to a `MemberKey`'s
    /// `argSig` (distinct from the `Signature.Parameters` the runtime pick reads). The
    /// thin TS type vocabulary names primitives/nominals exactly; everything richer
    /// collapses to `obj` — the deliberate forcing function: two ctor/method overloads
    /// that collapse to the same argSig throw (see `expandCtor`), pointing at the
    /// fixture whose erased distinction wants a sharper extracted type.
    let rec private argSigOf (t: Schema.TypeRef) : string =
        match t with
        | Schema.TypeRef.Named(name, []) -> name
        | Schema.TypeRef.Named(name, args) -> name + "<" + System.String.Join(",", List.map argSigOf args) + ">"
        | Schema.TypeRef.Typar i -> "!" + string i
        | Schema.TypeRef.Fun(args, ret) -> "(" + System.String.Join(",", List.map argSigOf args) + ")->" + argSigOf ret
        | Schema.TypeRef.Tuple items -> "(" + System.String.Join("*", List.map argSigOf items) + ")"
        | Schema.TypeRef.Union _
        | Schema.TypeRef.Dynamic
        | Schema.TypeRef.Structural _ -> "obj"

    let private paramArgSig (ps: Schema.Param list) : string list =
        ps |> List.map (fun p -> argSigOf p.Type)

    let private signatureOf (declArity: int) (sg: Schema.Signature) : ExternalSignature =
        {
            DeclaringArity = declArity
            MethodArity = sg.TypeParams
            Parameters = paramsFrozen sg.Params
            Return = toFrozen sg.Returns
        }

    /// Intern each overload signature's parameter shape into its `argSig`, guarding
    /// the set for collisions: two overloads that collapse to the same argSig (same
    /// param count AND types) would mint the SAME `MemberKey`, so throw rather than let
    /// them silently coincide — the forcing function that fires exactly when the
    /// `obj`-collapse has erased a real distinction (it points at the fixture whose
    /// thin extracted type wants sharpening). `label` names the member in the error.
    let private overloadArgSigs (label: string) (mem: Schema.Member) : (string list * Schema.Signature) list =
        let built = mem.Signatures |> List.map (fun sg -> paramArgSig sg.Params, sg)

        built
        |> List.countBy (fun (a, _) -> System.String.Join(",", a))
        |> List.tryFind (fun (_, n) -> n > 1)
        |> Option.iter (fun (k, _) ->
            failwithf "%s has duplicate overload argSig (%s); sharpen the extracted parameter types" label k
        )

        built

    /// Expand a `.ctor` member's N overload signatures into N `ExternalMember.ctor`s —
    /// the canonical seam constructor (`Name = ".ctor"`, instance, non-property, keyed
    /// `MemberKey(declKey, ".ctor", argSig, Method)`), the exact shape
    /// `InferCtor.inferExternalCtorOn` → `TryLookupMembers(name, ".ctor")` →
    /// `pickBestOverload` expects. Each ctor's `argSig` interns its parameter shape.
    let private expandCtor
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (mem: Schema.Member)
        : ExternalMember list =
        overloadArgSigs (sprintf "type '%A' .ctor" declKey) mem
        |> List.map (fun (argSig, sg) ->
            ExternalMember.ctor declKey (signatureOf declArity sg) (EqArray.ofList argSig) origin []
        )

    /// Expand a named method's N overload signatures into N `ExternalMember`s — one per
    /// call signature, each keyed `MemberKey(declKey, name, argSig, kind)` so that
    /// `TryLookupMembers` returns the full candidate set and overload-keyed lookups see
    /// distinct members (a single-signature method expands to a list of one). Mirrors
    /// `expandCtor`, but builds the records directly (no `.ctor` name/kind to bake) and
    /// carries the InterfaceMethod-vs-Method `kind` chosen by the caller.
    let private expandMethod
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (kind: MemberKind)
        (mem: Schema.Member)
        : ExternalMember list =
        overloadArgSigs (sprintf "type '%A' method '%s'" declKey mem.Name) mem
        |> List.map (fun (argSig, sg) ->
            {
                Name = mem.Name
                IsStatic = mem.Static
                IsProperty = false
                Signature = signatureOf declArity sg
                MethodArity = sg.TypeParams
                Origin = origin
                Key = SymbolKey.MemberKey(declKey, mem.Name, EqArray.ofList argSig, kind)
                OptionalDefaults = []
            }
        )

    let private toExternalMembers
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (isInterface: bool)
        (mem: Schema.Member)
        : ExternalMember list =
        match mem.Kind with
        | Schema.MemberKind.Method when mem.Name = ".ctor" -> expandCtor declKey origin declArity mem
        | Schema.MemberKind.Property ->
            let ret =
                match mem.Type with
                | Some t -> toFrozen t
                | None -> unitFrozen

            [
                {
                    Name = mem.Name
                    IsStatic = mem.Static
                    IsProperty = true
                    Signature =
                        {
                            DeclaringArity = declArity
                            MethodArity = 0
                            Parameters = unitFrozen
                            Return = ret
                        }
                    MethodArity = 0
                    Origin = origin
                    Key = SymbolKey.MemberKey(declKey, mem.Name, EqArray.empty, MemberKind.Property)
                    OptionalDefaults = []
                }
            ]
        | Schema.MemberKind.Method ->
            let kind =
                if isInterface && not mem.Static then
                    MemberKind.InterfaceMethod declKey
                else
                    MemberKind.Method

            expandMethod declKey origin declArity kind mem

    /// Split a flat `heritage` list into implemented/extended INTERFACES (`FrozenInterfaces`)
    /// and the single base CLASS (`FrozenBaseType`). The schema's `heritage` is a FLAT
    /// `TypeRef list` that does NOT, by itself, record which entry is the base class vs an
    /// interface (no schema field — and adding one is a deliberate contract bump we avoid).
    /// So we DISAMBIGUATE by resolving each entry's name against the manifest's own type
    /// table (`kindOf`): a name registered as an interface → interface slot, as a class →
    /// base-type slot. An entry we cannot resolve locally (a cross-package base, or any
    /// non-`Named` ref) DEFAULTS to the interface slot — a cross-package base CLASS is far
    /// rarer than a cross-package interface, and mis-slotting only loses base-member lookup
    /// for that rare case while never corrupting interface resolution. TS guarantees at most
    /// one base class, so a single `FrozenBaseType` slot suffices (last class-resolved entry
    /// wins if a malformed manifest somehow lists two).
    let private classifyHeritage
        (kindOf: string -> bool option) // Some true = interface, Some false = class, None = unknown
        (heritage: Schema.TypeRef list)
        : (string * FrozenType[])[] * FrozenType voption =
        let interfaces = ResizeArray<string * FrozenType[]>()
        let mutable baseTy = ValueNone

        for h in heritage do
            // Heritage entries are always NOMINAL (a class/interface ref); a structural
            // or union base is not expressible in TS, so a non-`Named` entry is a genuine
            // anomaly — throw rather than silently drop a declared supertype.
            let name, args =
                match h with
                | Schema.TypeRef.Named(name, args) -> name, args
                | other -> failwithf "heritage entry is not a nominal type reference: %A" other

            match kindOf name with
            | Some false ->
                // Resolves to a CLASS in this package → the single base class slot (full
                // `FrozenType`). TS guarantees at most one base class; a second would
                // overwrite, which only a malformed manifest could produce.
                baseTy <- ValueSome(toFrozen h)
            | _ ->
                // An interface, or an unresolved (cross-package) name → the interface slot
                // as a `(compiled-name, type-args)` pair, matching the metadata layer's
                // `buildClassInterfaces` shape. Cross-package defaults here because a
                // cross-package base CLASS is far rarer than a cross-package interface, and
                // mis-slotting only loses base-member lookup for that rare case.
                interfaces.Add(name, args |> List.map toFrozen |> Array.ofList)

        interfaces.ToArray(), baseTy

    let private originFor (moduleSpec: string) : SymbolOrigin =
        // The home label is the symbol's MODULE SPECIFIER (the import path), not the
        // package name. For a flat single-file package the two coincide, but the
        // value is threaded explicitly so a later tier's namespace recursion can
        // supply a nested path here. `Namespace` stays empty for flat files.
        {
            Assembly = Some moduleSpec
            Namespace = ""
            DeclaringType = None
        }

    let private toTypeShape
        (kindOf: string -> bool option)
        (moduleSpec: string)
        (ex: Schema.Export)
        : (string * ExternalTypeShape) option =
        let build name tp members heritage isInterface =
            let origin = originFor moduleSpec
            let key = SymbolKey.TypeKey(Some moduleSpec, "", name)

            let mems =
                members
                |> List.collect (toExternalMembers key origin tp isInterface)
                |> List.toArray

            let frozenInterfaces, frozenBaseType = classifyHeritage kindOf heritage

            Some(
                name,
                ExternalTypeShape.Class
                    {
                        Arity = tp
                        IsInterface = isInterface
                        Members = mems
                        FrozenInterfaces = frozenInterfaces
                        FrozenBaseType = frozenBaseType
                        Flags = ExternalClassFlags.Default
                        Origin = origin
                    }
            )

        match ex with
        | Schema.Export.Interface(name, tp, members, heritage) -> build name tp members heritage true
        | Schema.Export.Class(name, tp, members, heritage, _import) -> build name tp members heritage false
        | Schema.Export.TypeAlias(name, _tp, target) ->
            // `type X = …` maps onto the seam's transparent abbreviation shape: a use
            // site of `name` expands to the target's `FrozenType` (via
            // `FrozenTypeBridge.instantiateDeclaring`), so alias-to-union / -primitive /
            // -structural all resolve through the same `toFrozen` the members use.
            // `arity = 0`: generics are Tier 3 (the producer emits `typeParams = 0`).
            Some(name, ExternalTypeShape.Abbrev(0, toFrozen target))
        | Schema.Export.Enum(name, _members) ->
            // The seam has no enum-member representation: `ExternalTypeShape`'s doc names
            // an enum as exactly the `Opaque` (body-less) residue, so register the NAME
            // (keeping `TryLookupType` total) but leave the members unmodelled. Mapping
            // the members onto the seam needs a settled front-end decision (constant
            // fields vs a union of literal types) that isn't made yet.
            // TODO: model enum members once the front end commits to a representation.
            Some(name, ExternalTypeShape.Opaque 0)
        | _ -> None

    let private toFunctionSymbol (ex: Schema.Export) : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Function(name, signatures, _import) ->
            let sg = singleSignature name signatures

            let paramTypes =
                match sg.Params with
                | [] -> [ TyConst("unit", EqArray.empty) ]
                | ps -> ps |> List.map (fun p -> toSem p.Type)

            let semTy = List.foldBack (fun a acc -> TyFun(a, acc)) paramTypes (toSem sg.Returns)
            Some(name, ExternalSymbols.mono name semTy)
        | _ -> None

    /// A `Variable` export → a singleton VALUE symbol, resolved by name via
    /// `TryLookup` exactly like a free function but carrying the variable's type
    /// directly (a VALUE, not an arrow). `isConst` carries no front-end distinction
    /// at this seam (JS lowering reads the imported binding by name regardless of
    /// mutability), so it is not consumed here.
    let private toValueSymbol (ex: Schema.Export) : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Variable(name, ty, _isConst, _import) -> Some(name, ExternalSymbols.mono name (toSem ty))
        | _ -> None

    /// Build a provider from an already-parsed manifest.
    let providerOfManifest (man: Schema.PackageManifest) : IExternalSymbolProvider =
        let pkg = man.Package
        // Flat single-file package: the module specifier IS the package name. A
        // later tier supplies nested namespace paths here instead of `pkg` directly.
        let moduleSpec = pkg
        // Classify each top-level type by KIND (interface vs class) so `classifyHeritage`
        // can name-resolve a heritage entry to a slot (interface vs base class) without a
        // schema field. `None` for a name not in this package = cross-package / unknown.
        let typeKinds =
            man.Exports
            |> List.choose (fun ex ->
                match ex with
                | Schema.Export.Interface(name, _, _, _) -> Some(name, true)
                | Schema.Export.Class(name, _, _, _, _) -> Some(name, false)
                | _ -> None
            )
            |> Map.ofList

        let kindOf (name: string) : bool option = Map.tryFind name typeKinds
        let types = man.Exports |> List.choose (toTypeShape kindOf moduleSpec) |> Map.ofList
        // Free functions and singleton VARIABLES both resolve by name via `TryLookup`,
        // so they share the one value map (a variable is a value, not an arrow).
        let funcs =
            (man.Exports |> List.choose toFunctionSymbol)
            @ (man.Exports |> List.choose toValueSymbol)
            |> Map.ofList

        let membersOf (typeName: string) (memberName: string) : ExternalMember[] =
            match Map.tryFind typeName types with
            | Some(ExternalTypeShape.Class shape) -> shape.Members |> Array.filter (fun m -> m.Name = memberName)
            | _ -> [||]

        { new IExternalSymbolProvider with
            member _.TryLookup name =
                match Map.tryFind name funcs with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupType name =
                match Map.tryFind name types with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupMember(typeName, memberName) =
                match membersOf typeName memberName with
                | [||] -> ValueNone
                | arr -> ValueSome arr.[0]

            member _.TryLookupMembers(typeName, memberName) = membersOf typeName memberName
            member _.TryLookupUnionCase _ = ValueNone
            member _.AmbientOpenPrefixes = []
            member _.TryLookupInlineBody _ = ValueNone
            member _.TryLookupInlineBodyByName _ = ValueNone
            member _.IntrinsicReverseCanon = Map.empty
        }

    /// Parse a manifest JSON file and build its provider.
    let tryLoadFile (path: string) : Result<IExternalSymbolProvider, string> =
        try
            File.ReadAllText path |> Codec.deserialize |> Result.map providerOfManifest
        with ex ->
            Error(sprintf "Failed to read TS manifest '%s': %s" path ex.Message)

    /// Compose TS-manifest providers as the JS layer-2 metadata tail (the slot
    /// `JsNativeSymbols.buildJsNativeContractFor` uses), behind referenced-package
    /// contracts. `manifestPaths` are the `.fsi` package manifests; `tsManifestPaths`
    /// are the extractor's JSON outputs.
    let buildContractFor
        (target: string option)
        (manifestPaths: string list)
        (tsManifestPaths: string list)
        : IExternalSymbolProvider =
        let tsProviders =
            tsManifestPaths
            |> List.map (fun p ->
                match tryLoadFile p with
                | Ok prov -> prov
                | Error msg -> failwith msg
            )

        SymbolProviders.buildContractWithMetadata "tsmanifest" tsProviders target manifestPaths
