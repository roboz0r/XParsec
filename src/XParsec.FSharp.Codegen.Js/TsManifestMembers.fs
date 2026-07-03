namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest
open XParsec.FSharp.Codegen.Js.TsManifestTranslate

/// Member and type-shape building for the TS-manifest provider: expand a manifest
/// type's members into seam `ExternalMember`s (with per-overload `MemberKey`s) and
/// each type-flavoured export into an `ExternalTypeShape`. Consumed by
/// `TsManifestProvider`.
module internal TsManifestMembers =

    /// Expand a `.ctor` member's N overload signatures into N `ExternalMember.ctor`s —
    /// the canonical seam constructor (`Name = ".ctor"`, instance, non-property, keyed
    /// `MemberKey(declKey, ".ctor", argSig, Method)`), the exact shape
    /// `InferCtor.inferExternalCtorOn` → `TryLookupMembers(name, ".ctor")` →
    /// `pickBestOverload` expects. Each ctor's `argSig` interns its parameter shape.
    let private expandCtor
        (ctx: TranslateCtx)
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (mem: Schema.Member)
        : ExternalMember list =
        overloadArgSigs ctx (sprintf "type '%A' .ctor" declKey) mem
        |> List.map (fun (argSig, sg) ->
            ExternalMember.ctor declKey (signatureOf ctx declArity sg) (EqArray.ofList argSig) origin []
        )

    /// Expand a named method's N overload signatures into N `ExternalMember`s — one per
    /// call signature, each keyed `MemberKey(declKey, name, argSig, kind)` so that
    /// `TryLookupMembers` returns the full candidate set and overload-keyed lookups see
    /// distinct members (a single-signature method expands to a list of one). Mirrors
    /// `expandCtor`, but builds the records directly (no `.ctor` name/kind to bake) and
    /// carries the InterfaceMethod-vs-Method `kind` chosen by the caller.
    let expandMethod
        (ctx: TranslateCtx)
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (kind: MemberKind)
        (mem: Schema.Member)
        : ExternalMember list =
        overloadArgSigs ctx (sprintf "type '%A' method '%s'" declKey mem.Name) mem
        |> List.map (fun (argSig, sg) ->
            {
                Name = mem.Name
                IsStatic = mem.Static
                Storage = MemberStorage.Method
                Signature = signatureOf ctx declArity sg
                MethodArity = sg.TypeParams
                Origin = origin
                Key = SymbolKey.MemberKey(declKey, mem.Name, EqArray.ofList argSig, kind)
                OptionalDefaults = []
            }
        )

    let private toExternalMembers
        (ctx: TranslateCtx)
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (isInterface: bool)
        (mem: Schema.Member)
        : ExternalMember list =
        match mem.Kind with
        | Schema.MemberKind.Method when mem.Name = ".ctor" -> expandCtor ctx declKey origin declArity mem
        | Schema.MemberKind.Property ->
            let ret =
                match mem.Type with
                | Some t -> toFrozen ctx t
                | None -> unitFrozen

            [
                {
                    Name = mem.Name
                    IsStatic = mem.Static
                    Storage = MemberStorage.Property
                    Signature = ExternalSignature.make (declArity, 0, unitFrozen, ret)
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

            expandMethod ctx declKey origin declArity kind mem

    /// Split a flat `heritage` list into implemented/extended INTERFACES (`FrozenInterfaces`)
    /// and the single base CLASS (`FrozenBaseType`). The schema's `heritage` is a FLAT
    /// `TypeRef list` that does NOT, by itself, record which entry is the base class vs an
    /// interface (no schema field — and adding one is a deliberate contract bump we avoid).
    /// So we DISAMBIGUATE by resolving each entry's name against the manifest's own type
    /// table (`ctx.TryFindType`): a name registered as an interface → interface slot, as a
    /// class → base-type slot. An entry we cannot resolve locally (a cross-package base, or
    /// any non-`Named` ref) DEFAULTS to the interface slot — a cross-package base CLASS is
    /// far rarer than a cross-package interface, and mis-slotting only loses base-member
    /// lookup for that rare case while never corrupting interface resolution. TS guarantees
    /// at most one base class, so a single `FrozenBaseType` slot suffices (last
    /// class-resolved entry wins if a malformed manifest somehow lists two).
    let private classifyHeritage
        (ctx: TranslateCtx)
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

            // Suffix the heritage entry's bare name by its applied arg count before the
            // table lookup (THE LAW, see `mint`): a GENERIC base/interface
            // (`extends Foo<T>`) must classify under `` Foo`1 `` — `arityName` is a
            // no-op at arity 0, so a non-generic base is byte-identical.
            match ctx.TryFindType(SymbolKeyOps.arityName name (List.length args)) with
            | Some id when not id.IsInterface ->
                // Resolves to a CLASS in this package → the single base class slot (full
                // `FrozenType`). TS guarantees at most one base class; a second would
                // overwrite, which only a malformed manifest could produce.
                baseTy <- ValueSome(toFrozen ctx h)
            | _ ->
                // An interface, or an unresolved (cross-package) name → the interface slot
                // as a `(compiled-name, type-args)` pair, matching the metadata layer's
                // `buildClassInterfaces` shape. Cross-package defaults here because a
                // cross-package base CLASS is far rarer than a cross-package interface, and
                // mis-slotting only loses base-member lookup for that rare case.
                interfaces.Add(name, args |> List.map (toFrozen ctx) |> Array.ofList)

        interfaces.ToArray(), baseTy

    let toTypeShape
        (ctx: TranslateCtx)
        (isGlobal: bool)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalTypeShape) option =
        let build name tp members heritage isInterface =
            let origin = originFor ctx nsPath
            // The identity comes FROM the ctx table (`declaredIdentity`), never
            // re-minted here, so the registered shape and `ctx.Resolve`'s answer agree
            // by construction — see `mint` for the map-key/`TypeKey` split.
            let qn, key = declaredIdentity ctx nsPath name tp

            let mems =
                members
                |> List.collect (toExternalMembers ctx key origin tp isInterface)
                |> List.toArray

            let frozenInterfaces, frozenBaseType = classifyHeritage ctx heritage

            Some(
                qn,
                ExternalTypeShape.Class
                    {
                        Arity = tp
                        IsInterface = isInterface
                        Members = mems
                        FrozenInterfaces = frozenInterfaces
                        FrozenBaseType = frozenBaseType
                        // A real manifest Interface/Class is a native object: its instance
                        // members live ON it as prototype/own methods, so JS emit must lower
                        // them as `receiver.member(args)`, not receiver-first free-fn imports
                        // (Vesper's own-runtime tree-shaking form). The synthetic erased
                        // grouping type (`providerOfManifest`) uses `ErasedBare` instead —
                        // its members go through the bare-export path anyway.
                        Flags =
                            { ExternalClassFlags.Default with
                                MemberLowering = MemberLowering.AttachedNative
                                // Global rides the HOME: a global pack's types are
                                // import-free (bare-name emit), a real package's are not.
                                Global = isGlobal
                            }
                        Origin = origin
                        // JS is single-faced — no BCL platform spelling to reconcile.
                        CapabilityFace = ValueNone
                    }
            )

        match ex with
        | Schema.Export.Interface(name, tp, members, heritage) -> build name tp members heritage true
        | Schema.Export.Class(name, tp, members, heritage, _import) -> build name tp members heritage false
        | Schema.Export.TypeAlias(name, tp, target) ->
            // `type X = …` maps onto the seam's transparent abbreviation shape: a use
            // site of `name` expands to the target's `FrozenType` (via
            // `FrozenTypeBridge.instantiateDeclaring`), so alias-to-union / -primitive /
            // -structural all resolve through the same `toFrozen` the members use. `tp`
            // is the alias's declaring-axis arity: a generic alias `Pair<A,B>` expands
            // `FTTypar(Declaring,0/1)` against the two use-site args.
            //
            // The map key shares `mint`'s qualified-name spelling (so a generic alias's
            // use site — `arityName "Pair" 2` at lookup — hits this key; a no-op at
            // arity 0), but an alias never enters the ctx table: the resolver must MISS
            // it so it stays `FTConst` and expands through this `Abbrev`.
            Some(fst (mint ctx.ModuleSpec nsPath name tp), ExternalTypeShape.Abbrev(tp, toFrozen ctx target))
        | Schema.Export.Enum(name, members) ->
            // A TS enum → `ExternalTypeShape.Enum`: the closed name→value case table
            // the front end resolves `(x: E)` / `E.Ci` against (the enum's nominal
            // identity) and JS imports the object map for. The wire `LiteralValue`
            // (numeric / string) carries straight onto `ExternalEnumCaseValue`; the
            // numeric / string / mixed variant falls out of the values, never baked.
            // A `None` (computed / non-constant) member is DROPPED — it has no value
            // to reference by, so it is unrepresentable as a case; dropping mirrors the
            // authored JS emission, which omits an unresolved case from the object map.
            let origin = originFor ctx nsPath

            let cases =
                members
                |> List.choose (fun (caseName, v) ->
                    match v with
                    | Some(Schema.LiteralValue.IntVal n) ->
                        Some
                            {
                                Name = caseName
                                Value = ExternalEnumCaseValue.IntVal n
                            }
                    | Some(Schema.LiteralValue.StringVal s) ->
                        Some
                            {
                                Name = caseName
                                Value = ExternalEnumCaseValue.StringVal s
                            }
                    | None -> None
                )
                |> List.toArray

            Some(qualify nsPath name, ExternalTypeShape.Enum(cases, origin))
        | _ -> None
