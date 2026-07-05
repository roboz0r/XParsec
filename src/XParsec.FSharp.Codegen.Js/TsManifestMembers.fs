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

    /// The count of TRAILING optional parameters (`readFile(path, cb, opts?)` ⇒ 1) a
    /// call may omit — carried onto the member's `OptionalDefaults` so the SHARED
    /// optional-fill seam (`InferExternalCall.tryFillOptionalCall`, which admits an
    /// under-applied arity, and `FreezeExpr.optionalDefaultNode`, which synthesises the
    /// omitted slots) permits `api.readFile(path, cb)`. Each omitted slot is
    /// `TConstValue.Unit`, whose JS value repr IS `undefined` — the correct absence
    /// value for an omitted TS optional. (The `undefined` TYPE is a DISTINCT identity
    /// from `unit`, see `prim-types-undefined.js.fs`; the fill exploits only the shared
    /// `unit`→`undefined` VALUE repr, never the type, and the fill node is never
    /// re-unified against the parameter type.)
    ///
    /// A REST parameter (`...args: T[]`) is NOT counted: an omitted rest means ZERO
    /// args, which a single-`undefined` fill would wrongly materialise as one supplied
    /// element — variadic rest lowering is deferred, so a trailing rest stays a required
    /// array param. TS forbids a required parameter after an optional one, so the
    /// optionals are always a trailing run; counting from the end is exact.
    let private trailingOptionalCount (ps: Schema.Param list) : int =
        ps
        |> List.rev
        |> List.takeWhile (fun p -> p.Optional && not p.Rest)
        |> List.length

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
        overloadArgSigs ctx mem
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
        overloadArgSigs ctx mem
        |> List.map (fun (argSig, sg) ->
            {
                Name = mem.Name
                IsStatic = mem.Static
                Storage = MemberStorage.Method
                Signature = signatureOf ctx declArity sg
                MethodArity = sg.TypeParams
                Origin = origin
                Key = SymbolKey.MemberKey(declKey, mem.Name, EqArray.ofList argSig, kind)
                OptionalDefaults = List.replicate (trailingOptionalCount sg.Params) TConstValue.Unit
                IsOptional = mem.Optional
            }
        )

    let toExternalMembers
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
                    IsOptional = mem.Optional
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
                // `buildClassInterfaces` shape (which keys by the arity-suffixed
                // `metadataName`). The stored name MUST be arity-suffixed — the subtype
                // walk / inherited-member walk compare it against a nominal target whose
                // name is `arityName`'d (THE LAW), so a generic `extends Foo<T>` stored bare
                // as `Foo` would never match `` Foo`1 ``. `arityName` is a no-op at arity 0.
                // Cross-package defaults here because a cross-package base CLASS is far rarer
                // than a cross-package interface, and mis-slotting only loses base-member
                // lookup for that rare case.
                interfaces.Add(
                    SymbolKeyOps.arityName name (List.length args),
                    args |> List.map (toFrozen ctx) |> Array.ofList
                )

        interfaces.ToArray(), baseTy

    /// TypeScript escapes a `[Symbol.iterator]()` method as `__@iterator@<symbolId>`; the
    /// trailing id varies by lib, so match by prefix. The extractor carries this member
    /// verbatim — the provider reads it here to home the type.
    [<Literal>]
    let private symbolIteratorPrefix = "__@iterator"

    /// A TS `[Symbol.iterator](): Iterator<T>` IS the `seq<'T>` capability on JS (both are
    /// the native iterator protocol). This backend judgment lives here, not in
    /// SemanticAnalysis: peel the element — the first type arg of the iterator the signature
    /// returns (`IterableIterator<T>` → `T`; `Map`'s `IterableIterator<[K,V]>` → the `[K,V]`
    /// tuple), over the declaring typars so `instantiateInterfaces` substitutes the
    /// receiver's args. `None` when the member is absent or its return is not an applied
    /// nominal. The element is later injected as the erased `IEnumerable\`1` interface so
    /// `for … in` recognition (`pickEnumerableElem`) admits the type.
    let private tryIteratorElement (ctx: TranslateCtx) (members: Schema.Member list) : FrozenType voption =
        members
        |> List.tryPick (fun m ->
            if m.Name.StartsWith symbolIteratorPrefix then
                match m.Signatures with
                | {
                      Returns = Schema.TypeRef.Named(_, elem :: _)
                  } :: _ -> Some(toFrozen ctx elem)
                | _ -> None
            else
                None
        )
        |> function
            | Some elem -> ValueSome elem
            | None -> ValueNone

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

            let heritageInterfaces, frozenBaseType = classifyHeritage ctx heritage

            // Home a `[Symbol.iterator]`-bearing type as `seq<'T>` by injecting the erased
            // `IEnumerable\`1` head with the peeled element: the existing `tryForInEnumerator`
            // arm then admits it and `for … in` lowers to `for..of`, no front-end or emit
            // change — the capability just has to appear in the interface set.
            let frozenInterfaces =
                match tryIteratorElement ctx members with
                | ValueSome elem ->
                    Array.append heritageInterfaces [| JsNativeSymbols.enumerableInterfaceName, [| elem |] |]
                | ValueNone -> heritageInterfaces

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

    /// Build the ERASING structural nominals for a manifest. Pre-scan EVERY TypeRef
    /// reachable from this manifest's exports (variable types, function/member
    /// signatures, member types, heritage) for anonymous OBJECT shapes, recursing into
    /// their fields so nested shapes register too, deduped by canonical shape-hash. Each
    /// unique shape becomes an ERASING nominal: an interface (data-only, no ctor) whose
    /// Property members lower to native `receiver.field` reads, homed under the reserved
    /// synthetic namespace so `.field` resolves and NOTHING is emitted for the type.
    /// Member REGISTRATION is per-manifest (each provider scans only its own exports);
    /// structural IDENTITY (`structuralKey`) is cross-manifest by construction — see
    /// `structuralKey`.
    let buildStructuralTypes
        (ctx: TranslateCtx)
        (flatExports: (string * Schema.Export) list)
        : (string * ExternalTypeShape) list =
        flatExports
        |> List.collect (fun (_, ex) -> exportTypeRefs ex)
        |> List.collect structuralShapesIn
        |> List.map (fun (printed, fields) -> structuralHash printed fields, fields)
        |> List.distinctBy fst
        |> List.map (fun (hash, fields) ->
            let qn, declKey = structuralKey hash

            let origin: SymbolOrigin =
                {
                    Assembly = Some structuralHome
                    Namespace = structuralHome
                    DeclaringType = None
                }

            // One Property member per field, through the SAME `toExternalMembers`
            // Property arm a real interface uses — the field's type freezes via `ctx`
            // (a nested structural field resolves to its OWN registered shape).
            let members =
                fields
                |> List.collect (fun (fname, fty) ->
                    let mem: Schema.Member =
                        {
                            Name = fname
                            Kind = Schema.MemberKind.Property
                            Type = Some fty
                            Signatures = []
                            Static = false
                            Optional = false
                        }

                    toExternalMembers ctx declKey origin 0 true mem
                )
                |> List.toArray

            qn,
            ExternalTypeShape.Class
                {
                    Arity = 0
                    IsInterface = true
                    Members = members
                    FrozenInterfaces = [||]
                    FrozenBaseType = ValueNone
                    Flags =
                        { ExternalClassFlags.Default with
                            // A structural field is an INSTANCE property read through a
                            // receiver, so it lowers `receiver.field` via the native
                            // attached path — NOT `ErasedBare`, which holds only static
                            // members and is an invariant break on an instance receiver.
                            MemberLowering = MemberLowering.AttachedNative
                            // Import-free: an anonymous erased shape has no home module
                            // to import — the type itself emits nothing.
                            Global = true
                        }
                    Origin = origin
                    // JS is single-faced — no BCL platform spelling to reconcile.
                    CapabilityFace = ValueNone
                }
        )

    /// The deterministic SIMPLE name of the synthetic grouping type that holds a
    /// module's overloaded free functions as static members. F# has no free-function
    /// overloading, so a TS `export function format(x:string);
    /// export function format(x:number);` cannot ride the name-keyed `funcs` map; the
    /// overloads are grouped as static members of ONE synthetic type and the call
    /// (`Util.format(x)`) erases at JS emit to the bare export (`format(x)`).
    ///
    /// Rule (stable for the golden): take the LAST '/'-segment of the module
    /// specifier — handling scoped/pathed specs like `@scope/util` → `util` — and
    /// upper-case its first character (`util` → `Util`). Derives from the MODULE
    /// specifier, never a user identifier, so the only way it can collide with a real
    /// exported type is a same-module type whose name equals the capitalised module
    /// segment; the caller guards that collision by throwing (it would otherwise
    /// silently shadow a real type) rather than mangling the name (which would diverge
    /// from the erase contract that keys off the bare member name, not the
    /// grouping-type name).
    let private syntheticTypeName (moduleSpec: string) : string =
        let lastSeg =
            match moduleSpec.Split('/') |> Array.filter (fun s -> s <> "") |> Array.tryLast with
            | Some s -> s
            | None -> moduleSpec

        if lastSeg = "" then
            lastSeg
        else
            string (System.Char.ToUpperInvariant lastSeg.[0]) + lastSeg.Substring 1

    /// An overloaded (N>1 call signature) free-function export, bound for the
    /// synthetic per-namespace grouping type — F# has no free-function overloading,
    /// so it cannot ride the name-keyed `funcs` map (the last overload would win).
    type private OverloadedFn =
        {
            NsPath: string
            Name: string
            Signatures: Schema.Signature list
            Import: Schema.ImportShape
        }

    /// Synthesize one erased grouping type per (nsPath) GROUP of overloaded free
    /// functions: its static members are the overloads, expanded with `expandMethod`
    /// (the member-overload expansion) so each carries its own argSig `MemberKey`, and
    /// the member name stays the REAL export name so the erase lowers `Util.format` to
    /// the bare `format`. Grouped by namespace path so namespaced overloads land in a
    /// sibling synthetic type under their qualified name.
    ///
    /// A single-signature function stays a BARE free function (the name-keyed `funcs`
    /// map / `TryLookup`); only N>1-signature exports partition in here.
    let buildOverloadGroupingTypes
        (ctx: TranslateCtx)
        (moduleSpec: string)
        (isGlobalPack: bool)
        (flatExports: (string * Schema.Export) list)
        : (string * ExternalTypeShape) list =
        // Partition free functions by call-signature count: an OVERLOADED one (N>1
        // signatures) is grouped into a synthetic per-module static-method type.
        let overloadedFns =
            flatExports
            |> List.choose (fun (nsPath, ex) ->
                match ex with
                | Schema.Export.Function(name, sigs, import) when List.length sigs > 1 ->
                    Some
                        {
                            NsPath = nsPath
                            Name = name
                            Signatures = sigs
                            Import = import
                        }
                | _ -> None
            )

        overloadedFns
        |> List.groupBy (fun fn -> fn.NsPath)
        |> List.map (fun (nsPath, fns) ->
            // The group's UNIFORM import form, stamped on the erased type's flags so
            // `erasedGroupingRef` lowers `Util.format(x)` to the right import shape.
            // The overloads of one grouping share a home module, so they share an
            // import form; a MIXED group is a manifest anomaly (a single module cannot
            // be both `export =` and named-export) — throw rather than silently pick.
            let groupImportForm =
                let forms = fns |> List.map (fun fn -> importFormOfShape fn.Import) |> List.distinct

                match forms with
                | [ single ] -> single
                | many ->
                    failwithf
                        "overloaded free functions at namespace '%s' mix import forms %A; a synthetic grouping type carries ONE import form"
                        nsPath
                        many

            let simpleName = syntheticTypeName moduleSpec
            // The synthetic type's identity goes through the same `mint` spelling
            // (arity 0 — the grouping type is never generic) even though it never
            // enters the ctx table: it resolves via `TryLookupType`/`TryLookupMembers`
            // by qualified name, never through `ctx.Resolve`.
            let qn, declKey = mint moduleSpec nsPath simpleName 0
            let origin = originFor ctx nsPath

            let members =
                fns
                |> List.collect (fun fn ->
                    // Reuse the member-overload expansion: wrap the free function's
                    // signatures as a synthetic STATIC method named after the real export.
                    let mem: Schema.Member =
                        {
                            Name = fn.Name
                            Kind = Schema.MemberKind.Method
                            Type = None
                            Signatures = fn.Signatures
                            Static = true
                            Optional = false
                        }

                    expandMethod ctx declKey origin 0 MemberKind.Method mem
                )
                |> List.toArray

            qn,
            ExternalTypeShape.Class
                {
                    Arity = 0
                    IsInterface = false
                    Members = members
                    FrozenInterfaces = [||]
                    FrozenBaseType = ValueNone
                    Flags =
                        { ExternalClassFlags.Default with
                            MemberLowering = MemberLowering.ErasedBare
                            // Global rides the HOME: a global pack's grouping type is
                            // import-free like its real types.
                            Global = isGlobalPack
                            // The group's import form, consumed by `erasedGroupingRef`
                            // to pick `import { format }` (Named) vs `import format`
                            // (Default/CommonJs) vs `import * as util; util.format`
                            // (Namespace).
                            ImportForm = groupImportForm
                        }
                    Origin = origin
                    // JS is single-faced — no BCL platform spelling to reconcile.
                    CapabilityFace = ValueNone
                }
        )
