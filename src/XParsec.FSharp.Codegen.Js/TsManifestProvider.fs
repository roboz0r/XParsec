namespace XParsec.FSharp.Codegen.Js

open System.IO

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest
open XParsec.FSharp.Codegen.Js.TsManifestTranslate
open XParsec.FSharp.Codegen.Js.TsManifestMembers

/// Layer-2 provider backed by a TS-derived JSON manifest (`Vesper.Ts.Manifest`),
/// the consumer end of the extractor→manifest→provider slice. It is the JS analog
/// of `MetadataSymbols` (which reads .NET assemblies via `MetadataLoadContext`):
/// here the "metadata oracle" is the serialised manifest the TS extractor emitted,
/// and this maps its type-description grammar into the seam's `ExternalTypeShape`
/// / `ExternalSymbol` / `FrozenType`. Mirrors `JsNativeSymbols` structurally.
///
/// This module assembles and loads the provider (free-function/value symbols,
/// overload grouping, the `IExternalSymbolProvider` maps); type translation lives
/// in `TsManifestTranslate` (TsManifestTypes.fs) and member/type-shape building in
/// `TsManifestMembers` (TsManifestMembers.fs).
///
/// MVP scope: non-generic `Interface` members (primitive-typed) + free
/// `Function`s. `Class` is handled too; the remaining grammar (unions, dynamic,
/// structural, generics, import shapes) is mapped conservatively or deferred —
/// see the inline TODOs.
module TsManifestProvider =

    let private singleSignature (name: string) (sigs: Schema.Signature list) : Schema.Signature =
        // The bare free-function path is single-signature ONLY: `providerOfManifest`
        // routes overloaded (N>1) free functions into a synthetic per-module grouping
        // type before this is reached, so the N>1 arm is now a defensive guard (it
        // should be unreachable from `funcs`). Throwing — rather than silently picking
        // the first — keeps that invariant load-bearing.
        match sigs with
        | [ s ] -> s
        | [] -> failwithf "symbol '%s' has no call signature" name
        | _ -> failwithf "symbol '%s' has %d overloads; overload sets not yet supported" name (List.length sigs)

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

    /// The `Origin`/`Key`/`ImportForm` module-spec stamp shared by free-function and
    /// variable symbols: both resolve their `import … from '<moduleSpec>'` through
    /// `JsImports.addRef`, which needs a `ValueKey(Some moduleSpec, …)` — without it
    /// the symbol carries `asm = None` and emit fails on a `ValueKey(None, …)`.
    /// `import` is the manifest's per-export shape: a TS `export default` stamps
    /// `ImportForm.Default` so the JS backend lowers the use site to a DEFAULT import
    /// (`import x from '<spec>'` — a default export cannot be imported by name);
    /// everything else collapses to `Named` (Namespace/CommonJS forms have no
    /// fixture yet — see `ImportForm`).
    let private stampValueSymbol
        (ctx: TranslateCtx)
        (nsPath: string)
        (name: string)
        (import: Schema.ImportShape)
        (sym: ExternalSymbol)
        : ExternalSymbol =
        { sym with
            Origin = originFor ctx nsPath
            Key = SymbolKey.ValueKey(Some ctx.ModuleSpec, nsPath, name)
            ImportForm =
                match import with
                | Schema.ImportShape.Default -> ImportForm.Default
                | _ -> ImportForm.Named
        }

    let private toFunctionSymbol
        (ctx: TranslateCtx)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Function(name, signatures, import) ->
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
                | ps -> ps |> List.map (fun p -> toFrozen ctx p.Type)

            let frozenTy =
                List.foldBack (fun a acc -> FTFun(a, acc)) paramTypes (toFrozen ctx sg.Returns)
            // Registered/keyed under the dotted qualified name; the symbol's own `Name`
            // carries it too so lowering emits the qualified binding. The `Origin`/`Key`
            // module-spec stamp (`stampValueSymbol`) is the analog of `toTypeShape`'s
            // `originFor`/`TypeKey`.
            //
            // A GENERIC free function (`identity<T>`) carries its own typars as
            // `FTTypar(Declaring,i)` (via `toFrozen`); `sg.TypeParams` is their count, so
            // `scheme` freshens them per use site — genuinely polymorphic, not the frozen
            // markers the former `mono` froze in place.
            let qn = qualify nsPath name

            let sym =
                ExternalSymbols.scheme qn frozenTy sg.TypeParams []
                |> stampValueSymbol ctx nsPath name import

            Some(qn, sym)
        | _ -> None

    /// A `Variable` export → a singleton VALUE symbol, resolved by name via
    /// `TryLookup` exactly like a free function but carrying the variable's type
    /// directly (a VALUE, not an arrow). `isConst` carries no front-end distinction
    /// at this seam (JS lowering reads the imported binding by name regardless of
    /// mutability), so it is not consumed here.
    let private toValueSymbol
        (ctx: TranslateCtx)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Variable(name, ty, _isConst, import) ->
            let qn = qualify nsPath name
            // `monoFrozen` alone would leave the `None` origin `stampValueSymbol` fixes.
            let sym =
                ExternalSymbols.monoFrozen qn (toFrozen ctx ty)
                |> stampValueSymbol ctx nsPath name import

            Some(qn, sym)
        | _ -> None

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

    /// Build a provider from an already-parsed manifest.
    let providerOfManifest (man: Schema.PackageManifest) : IExternalSymbolProvider =
        let pkg = man.Package
        // Flat single-file package: the module specifier IS the package name. A
        // later tier supplies nested namespace paths here instead of `pkg` directly.
        let moduleSpec = pkg

        // A GLOBAL pack (its `Package`/home appears in `globalLibHomes`) mounts every
        // export under its Vesper-facing namespace: start the flatten at that prefix,
        // so `es2015`'s `Map` registers as `Js.Map` (nsPath `Js`) and every downstream
        // site — `mint`/`originFor`/`buildCtx`/`toTypeShape`/`funcs`/synthetic types —
        // picks up the prefix from the SAME `flatExports`. `mountPrefix = ""` for a
        // real package (flatten at root, as before). ONE source: `globalLibHomes`.
        let mountPrefix =
            TsGlobalHomes.globalLibHomes.TryFind man.Package |> Option.defaultValue ""

        // Global rides the HOME: a type this manifest builds is import-free iff its
        // home is a global pack (equivalently, `mountPrefix <> ""`). Stamped onto
        // every class/interface shape below.
        let isGlobalPack = mountPrefix <> ""

        let flatExports = flatten mountPrefix man.Exports

        // ONE pre-pass mints every declared `Interface`/`Class` identity (see
        // `TsManifestTranslate.mint`/`buildCtx`) over ALL flat exports before any
        // per-export walk, so a member signature that names a type declared LATER
        // (mitt's `mitt` referencing `Emitter`) still resolves.
        let ctx = buildCtx moduleSpec mountPrefix man.Refs flatExports

        // Partition free functions by call-signature count. A single-signature
        // function stays a BARE free function (the name-keyed `funcs` map /
        // `TryLookup`). An OVERLOADED one (N>1 signatures) is grouped into a
        // synthetic per-module static-method type instead.
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

        let regularTypes =
            flatExports
            |> List.choose (fun (nsPath, ex) -> toTypeShape ctx isGlobalPack nsPath ex)

        // Pre-scan EVERY TypeRef reachable from this manifest's exports (variable types,
        // function/member signatures, member types, heritage) for anonymous OBJECT shapes,
        // recursing into their fields so nested shapes register too, deduped by canonical
        // shape-hash. Each unique shape becomes an ERASING nominal in the `types` map: an
        // interface (data-only, no ctor) whose Property members lower to native
        // `receiver.field` reads, homed under the reserved synthetic namespace so `.field`
        // resolves and NOTHING is emitted for the type. Member REGISTRATION is per-manifest
        // (each provider scans only its own exports); structural IDENTITY (`structuralKey`)
        // is cross-manifest by construction — see `structuralKey`.
        let structuralTypes =
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

        // Synthesize one erased grouping type per (nsPath) GROUP of overloaded free
        // functions: its static members are the overloads, expanded with `expandMethod`
        // (the member-overload expansion) so each carries its own argSig `MemberKey`,
        // and the member name stays the REAL export name so the erase lowers
        // `Util.format` to the bare `format`. Grouped by namespace path so namespaced
        // overloads land in a sibling synthetic type under their qualified name.
        let syntheticTypes =
            overloadedFns
            |> List.groupBy (fun fn -> fn.NsPath)
            |> List.map (fun (nsPath, fns) ->
                // Named-imports-only gate: the erase path reuses the existing
                // named-import `addRef` lowering; Default/Namespace/CommonJS import forms
                // have no JS AST yet. Throw loudly on a non-Named overloaded free function
                // so the deferred import-form work is gated to exactly that fixture.
                for fn in fns do
                    match fn.Import with
                    | Schema.ImportShape.Named -> ()
                    | other ->
                        failwithf
                            "overloaded free function '%s' uses import shape %A; only Named imports are supported for the synthetic free-function-overload grouping type"
                            fn.Name
                            other

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

        // The structural erasing nominal is homed under the reserved `@struct` namespace,
        // which a real export's qualified name cannot spell — but keep the same collision
        // guard as the grouping type rather than trusting that reservation silently.
        for (qn, _) in structuralTypes do
            if Set.contains qn regularTypeNames then
                failwithf "synthetic structural type '%s' collides with a real exported type of the same name" qn

        let types = (regularTypes @ syntheticTypes @ structuralTypes) |> Map.ofList

        // Free functions and singleton VARIABLES both resolve by name via `TryLookup`,
        // so they share the one value map (a variable is a value, not an arrow).
        // OVERLOADED functions are excluded here — they resolve through their synthetic
        // type's static members (`TryLookupMembers`), not by bare name.
        let funcs =
            (flatExports
             |> List.choose (fun (nsPath, ex) ->
                 match ex with
                 | Schema.Export.Function(_, sigs, _) when List.length sigs > 1 -> None
                 | _ -> toFunctionSymbol ctx nsPath ex
             ))
            @ (flatExports |> List.choose (fun (nsPath, ex) -> toValueSymbol ctx nsPath ex))
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
