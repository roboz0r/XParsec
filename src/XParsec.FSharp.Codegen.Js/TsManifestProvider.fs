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
        | Schema.TypeRef.MethodTypar i -> FTTypar(TyparAxis.Method, i)
        | Schema.TypeRef.Fun(args, ret) -> List.foldBack (fun a acc -> FTFun(toFrozen a, acc)) args (toFrozen ret)
        | Schema.TypeRef.Tuple items -> FTTuple(EqArray.ofSeq (List.map toFrozen items))
        | Schema.TypeRef.Union members -> FTOr(EqArray.ofSeq (List.map toFrozen members))
        | Schema.TypeRef.Dynamic -> FTUnknown "any" // TODO: TyDynamic once it lands
        | Schema.TypeRef.Structural(hash, _) -> FTUnknown("structural:" + hash) // TODO: content-hash record

    let private unitFrozen: FrozenType = FTConst("unit", EqArray.empty)

    /// .NET-tupled parameter encoding: 0 → unit, 1 → bare, N≥2 → tuple.
    let private paramsFrozen (ps: Schema.Param list) : FrozenType =
        match ps with
        | [] -> unitFrozen
        | [ p ] -> toFrozen p.Type
        | many -> FTTuple(EqArray.ofSeq (many |> List.map (fun p -> toFrozen p.Type)))

    let private singleSignature (name: string) (sigs: Schema.Signature list) : Schema.Signature =
        // The bare free-function path is single-signature ONLY: `providerOfManifest`
        // routes overloaded (N>1) free functions into a synthetic per-module grouping
        // type (Tier 2 item 9b) before this is reached, so the N>1 arm is now a
        // defensive guard (it should be unreachable from `funcs`). Throwing — rather
        // than silently picking the first — keeps that invariant load-bearing.
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
        | Schema.TypeRef.MethodTypar i -> "!!" + string i
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
                Storage = MemberStorage.Method
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
                    Storage = MemberStorage.Property
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

    let private originFor (moduleSpec: string) (nsPath: string) : SymbolOrigin =
        // The home label is the symbol's MODULE SPECIFIER (the import path), not the
        // package name. For a flat single-file package the module spec and package
        // coincide. `Namespace` carries the symbol's namespace PATH within the module
        // (item 17): "" for a top-level export, `NS`/`NS.Inner` for a member nested in
        // one (or more) `export namespace`s — the JS analog of a .NET `Type.Namespace`.
        {
            Assembly = Some moduleSpec
            Namespace = nsPath
            DeclaringType = None
        }

    /// The lookup key a symbol declared at namespace path `nsPath` is registered/found
    /// under: its DOTTED QUALIFIED name (`NS.Foo`, `NS.Inner.Baz`), matching exactly
    /// the name `Passes.NameResolution` forms from a `NS.Foo` use site and hands to
    /// `TryLookupType`/`TryLookup` — the same full-dotted-name convention `MetadataSymbols`
    /// keys a .NET namespaced type by. A top-level export (`nsPath = ""`) keeps its bare name.
    let private qualify (nsPath: string) (name: string) : string =
        if nsPath = "" then name else nsPath + "." + name

    /// The deterministic SIMPLE name of the synthetic grouping type that holds a
    /// module's overloaded free functions as static members (Tier 2 item 9b). F# has
    /// no free-function overloading, so a TS `export function format(x:string);
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
    /// from the Phase-2 erase contract that keys off the bare member name, not the
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

    let private toTypeShape
        (kindOf: string -> bool option)
        (moduleSpec: string)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalTypeShape) option =
        let build name tp members heritage isInterface =
            let origin = originFor moduleSpec nsPath
            // The `SymbolKey.TypeKey` carries the SIMPLE name + namespace path (the
            // codegen-minting split), while the MAP key (`qualify`) is the dotted
            // qualified name the front end looks up by — mirroring `MetadataSymbols`,
            // where the key's `ns`/simple-name decompose `Type.FullName` but the lookup
            // string is the full name.
            let key = SymbolKey.TypeKey(Some moduleSpec, nsPath, name)

            let mems =
                members
                |> List.collect (toExternalMembers key origin tp isInterface)
                |> List.toArray

            let frozenInterfaces, frozenBaseType = classifyHeritage kindOf heritage

            Some(
                qualify nsPath name,
                ExternalTypeShape.Class
                    {
                        Arity = tp
                        IsInterface = isInterface
                        Members = mems
                        FrozenInterfaces = frozenInterfaces
                        FrozenBaseType = frozenBaseType
                        Flags = ExternalClassFlags.Default
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
            // is the alias's declaring-axis arity (item 11): a generic alias `Pair<A,B>`
            // expands `FTTypar(Declaring,0/1)` against the two use-site args.
            Some(qualify nsPath name, ExternalTypeShape.Abbrev(tp, toFrozen target))
        | Schema.Export.Enum(name, members) ->
            // A TS enum → `ExternalTypeShape.Enum`: the closed name→value case table
            // the front end resolves `(x: E)` / `E.Ci` against (the enum's nominal
            // identity) and JS imports the object map for. The wire `EnumValue`
            // (numeric / string) carries straight onto `ExternalEnumCaseValue`; the
            // numeric / string / mixed variant falls out of the values, never baked.
            // A `None` (computed / non-constant) member is DROPPED — it has no value
            // to reference by, so it is unrepresentable as a case; dropping mirrors the
            // authored JS emission, which omits an unresolved case from the object map.
            let origin = originFor moduleSpec nsPath

            let cases =
                members
                |> List.choose (fun (caseName, v) ->
                    match v with
                    | Some(Schema.EnumValue.IntVal n) ->
                        Some
                            {
                                Name = caseName
                                Value = ExternalEnumCaseValue.IntVal n
                            }
                    | Some(Schema.EnumValue.StringVal s) ->
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

    let private toFunctionSymbol
        (moduleSpec: string)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Function(name, signatures, _import) ->
            let sg = singleSignature name signatures

            // A TRAILING optional parameter (`mitt(all?)`) is dropped from the curried
            // arrow: a zero-arg use site (`mitt()`) applies to `unit`, so a sole trailing
            // optional collapses the function to `unit -> ret`. (Only trailing optionals
            // drop — an optional followed by a required one keeps its slot; TS forbids
            // that ordering anyway.) The runtime default (`n = n || new Map`) supplies the
            // omitted argument, mirroring how `OptionalDefaults` elides member arguments.
            let requiredParams =
                sg.Params |> List.rev |> List.skipWhile (fun p -> p.Optional) |> List.rev

            let paramTypes =
                match requiredParams with
                | [] -> [ unitFrozen ]
                | ps -> ps |> List.map (fun p -> toFrozen p.Type)

            let frozenTy =
                List.foldBack (fun a acc -> FTFun(a, acc)) paramTypes (toFrozen sg.Returns)
            // Registered/keyed under the dotted qualified name (item 17); the symbol's
            // own `Name` carries it too so lowering emits the qualified binding. The
            // `Origin`/`Key` are stamped with the MODULE SPECIFIER (the analog of
            // `toTypeShape`'s `originFor`/`TypeKey`), so `JsImports.addRef` can resolve
            // the `import … from '<moduleSpec>'` statement — without it the symbol carries
            // `asm = None` and emit fails on a `ValueKey(None, …)`.
            //
            // A GENERIC free function (`identity<T>`) carries its own typars as
            // `FTTypar(Declaring,i)` (via `toFrozen`); `sg.TypeParams` is their count, so
            // `scheme` freshens them per use site — genuinely polymorphic, not the frozen
            // markers the former `mono` froze in place.
            let qn = qualify nsPath name

            let sym =
                { ExternalSymbols.scheme qn frozenTy sg.TypeParams [] with
                    Origin = originFor moduleSpec nsPath
                    Key = SymbolKey.ValueKey(Some moduleSpec, nsPath, name)
                }

            Some(qn, sym)
        | _ -> None

    /// A `Variable` export → a singleton VALUE symbol, resolved by name via
    /// `TryLookup` exactly like a free function but carrying the variable's type
    /// directly (a VALUE, not an arrow). `isConst` carries no front-end distinction
    /// at this seam (JS lowering reads the imported binding by name regardless of
    /// mutability), so it is not consumed here.
    let private toValueSymbol
        (moduleSpec: string)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Variable(name, ty, _isConst, _import) ->
            let qn = qualify nsPath name
            // Same `Origin`/`Key` module-spec stamp as `toFunctionSymbol`: a value symbol
            // resolves its `import … from '<moduleSpec>'` through `JsImports.addRef` and so
            // needs a `ValueKey(Some moduleSpec, …)`, not `monoFrozen`'s `None` origin.
            let sym =
                { ExternalSymbols.monoFrozen qn (toFrozen ty) with
                    Origin = originFor moduleSpec nsPath
                    Key = SymbolKey.ValueKey(Some moduleSpec, nsPath, name)
                }

            Some(qn, sym)
        | _ -> None

    /// Build a provider from an already-parsed manifest.
    let providerOfManifest (man: Schema.PackageManifest) : IExternalSymbolProvider =
        let pkg = man.Package
        // Flat single-file package: the module specifier IS the package name. A
        // later tier supplies nested namespace paths here instead of `pkg` directly.
        let moduleSpec = pkg

        // Flatten the export tree into (namespacePath, export) pairs (item 17): a
        // top-level export pairs with `""`; a member nested in one (or more)
        // `Export.Namespace`s pairs with its dotted path (`NS`, `NS.Inner`). The
        // `Namespace` container itself produces no symbol — only its members do, each
        // registered under its QUALIFIED name (`qualify`), so a nested symbol resolves
        // through the SAME flat `types`/`funcs` maps as a top-level one. Recursion folds
        // arbitrarily deep nesting.
        let rec flatten (nsPath: string) (exports: Schema.Export list) : (string * Schema.Export) list =
            exports
            |> List.collect (fun ex ->
                match ex with
                | Schema.Export.Namespace(nsName, nested) -> flatten (qualify nsPath nsName) nested
                | other -> [ nsPath, other ]
            )

        let flatExports = flatten "" man.Exports

        // Classify each type by KIND (interface vs class) so `classifyHeritage` can
        // name-resolve a heritage entry to a slot (interface vs base class) without a
        // schema field. Keyed by QUALIFIED name so a namespaced heritage reference
        // resolves. `None` for a name not in this package = cross-package / unknown.
        let typeKinds =
            flatExports
            |> List.choose (fun (nsPath, ex) ->
                match ex with
                | Schema.Export.Interface(name, _, _, _) -> Some(qualify nsPath name, true)
                | Schema.Export.Class(name, _, _, _, _) -> Some(qualify nsPath name, false)
                | _ -> None
            )
            |> Map.ofList

        let kindOf (name: string) : bool option = Map.tryFind name typeKinds

        // Partition free functions by call-signature count (Tier 2 item 9b). A
        // single-signature function stays a BARE free function (the name-keyed `funcs`
        // map / `TryLookup`). An OVERLOADED one (N>1 signatures) cannot ride the
        // name-keyed map — F# has no free-function overloading, so the last would win —
        // so it is grouped into a synthetic per-module static-method type instead.
        let overloadedFns =
            flatExports
            |> List.choose (fun (nsPath, ex) ->
                match ex with
                | Schema.Export.Function(name, sigs, import) when List.length sigs > 1 ->
                    Some(nsPath, name, sigs, import)
                | _ -> None
            )

        let regularTypes =
            flatExports
            |> List.choose (fun (nsPath, ex) -> toTypeShape kindOf moduleSpec nsPath ex)

        // Synthesize one erased grouping type per (nsPath) GROUP of overloaded free
        // functions: its static members are the overloads, expanded with `expandMethod`
        // (reusing 9a) so each carries its own argSig `MemberKey`, and the member name
        // stays the REAL export name so the Phase-2 erase lowers `Util.format` to the
        // bare `format`. Grouped by namespace path so namespaced overloads land in a
        // sibling synthetic type under their qualified name.
        let syntheticTypes =
            overloadedFns
            |> List.groupBy (fun (nsPath, _, _, _) -> nsPath)
            |> List.map (fun (nsPath, fns) ->
                // v1 gate (named-imports only): the erase path reuses the existing
                // named-import `addRef` lowering; Default/Namespace/CommonJS import forms
                // have no JS AST yet. Throw loudly on a non-Named overloaded free function
                // so the deferred import-form work is gated to exactly that fixture.
                for (_, name, _, import) in fns do
                    match import with
                    | Schema.ImportShape.Named -> ()
                    | other ->
                        failwithf
                            "overloaded free function '%s' uses import shape %A; only Named imports are supported for the synthetic free-function-overload grouping type (Tier 2 item 9b v1)"
                            name
                            other

                let simpleName = syntheticTypeName moduleSpec
                let qn = qualify nsPath simpleName
                let declKey = SymbolKey.TypeKey(Some moduleSpec, nsPath, simpleName)
                let origin = originFor moduleSpec nsPath

                let members =
                    fns
                    |> List.collect (fun (_, name, sigs, _) ->
                        // Reuse the member-overload expansion (9a): wrap the free function's
                        // signatures as a synthetic STATIC method named after the real export.
                        let mem: Schema.Member =
                            {
                                Name = name
                                Kind = Schema.MemberKind.Method
                                Type = None
                                Signatures = sigs
                                Static = true
                                Optional = false
                            }

                        expandMethod declKey origin 0 MemberKind.Method mem
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
                                Erased = true
                            }
                        Origin = origin
                        // JS is single-faced — no BCL platform spelling to reconcile.
                        CapabilityFace = ValueNone
                    }
            )

        // Guard the synthetic name against a real exported type of the same qualified
        // name (structurally possible only when a same-module type matches the
        // capitalised module segment): silently shadowing it would corrupt resolution.
        let regularTypeNames = regularTypes |> List.map fst |> Set.ofList

        for (qn, _) in syntheticTypes do
            if Set.contains qn regularTypeNames then
                failwithf
                    "synthetic free-function-overload grouping type '%s' collides with a real exported type of the same name; rename the module or the type"
                    qn

        let types = (regularTypes @ syntheticTypes) |> Map.ofList

        // Free functions and singleton VARIABLES both resolve by name via `TryLookup`,
        // so they share the one value map (a variable is a value, not an arrow).
        // OVERLOADED functions are excluded here — they resolve through their synthetic
        // type's static members (`TryLookupMembers`), not by bare name.
        let funcs =
            (flatExports
             |> List.choose (fun (nsPath, ex) ->
                 match ex with
                 | Schema.Export.Function(_, sigs, _) when List.length sigs > 1 -> None
                 | _ -> toFunctionSymbol moduleSpec nsPath ex
             ))
            @ (flatExports
               |> List.choose (fun (nsPath, ex) -> toValueSymbol moduleSpec nsPath ex))
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
            member _.IntrinsicForwardRepr = Map.empty
        }

    /// The `(asm, ns, name)` identity of every top-level/namespaced FUNCTION or VARIABLE
    /// export whose `ImportShape` is `Default` — the exact `ValueKey` decomposition
    /// `JsImports.addRef` sees on a use site, so a backend can seed `JsImports` with the
    /// set that must lower to `import <alias> from '<spec>'` (default) rather than the
    /// `import { name as … }` (named) form. Import shape cannot ride the `SymbolKey`
    /// itself (no field, and `SymbolKey` has no `comparison` for a `Set`) nor the node
    /// (which carries only the key), so this side set is the channel; it is derived from
    /// the SAME manifest the provider is built from, keyed identically to
    /// `toFunctionSymbol`/`toValueSymbol` (`ValueKey(Some moduleSpec, nsPath, name)`).
    let defaultValueKeys (man: Schema.PackageManifest) : Set<string * string * string> =
        let moduleSpec = man.Package

        let rec flatten (nsPath: string) (exports: Schema.Export list) : (string * Schema.Export) list =
            exports
            |> List.collect (fun ex ->
                match ex with
                | Schema.Export.Namespace(nsName, nested) -> flatten (qualify nsPath nsName) nested
                | other -> [ nsPath, other ]
            )

        flatten "" man.Exports
        |> List.choose (fun (nsPath, ex) ->
            match ex with
            | Schema.Export.Function(name, _, Schema.ImportShape.Default)
            | Schema.Export.Variable(name, _, _, Schema.ImportShape.Default) -> Some(moduleSpec, nsPath, name)
            | _ -> None
        )
        |> Set.ofList

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
