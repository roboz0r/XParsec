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
/// / `ExternalSymbol` / `FrozenType`. It mirrors `MetadataSymbolProvider`'s
/// concrete-type shape: a named provider type (`TsManifestSymbolProvider`) whose
/// ctor resolves the manifest into `let` fields, with `IExternalSymbolProvider`
/// implemented as its interface members.
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

    /// The `Origin`/`Key`/`ImportForm` module-spec stamp shared by free-function and
    /// variable symbols: both resolve their `import … from '<moduleSpec>'` through
    /// `JsImports.addRef`, which needs a `ValueKey(Some moduleSpec, …)` — without it
    /// the symbol carries `asm = None` and emit fails on a `ValueKey(None, …)`.
    /// `import` is the manifest's per-export shape, mapped faithfully through the ONE
    /// `importFormOfShape` (a TS `export default` → `Default` → DEFAULT import; an
    /// `export =` → `CommonJs`; a namespace module → `Namespace`; else `Named`).
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
            ImportForm = importFormOfShape import
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

    /// Concrete provider holding the manifest's resolved state — the JS analog of
    /// `MetadataSymbols`'s `MetadataSymbolProvider`: the ctor builds every map/guard
    /// into `let` fields and the `IExternalSymbolProvider` members serve them. It is a
    /// named type (not an object expression) precisely because it carries that state.
    type internal TsManifestSymbolProvider(man: Schema.PackageManifest) =
        let pkg = man.Package
        // Flat single-file package: the module specifier IS the package name. A
        // later tier supplies nested namespace paths here instead of `pkg` directly.
        let moduleSpec = pkg

        // A MOUNTED pack (`TsGlobalHomes.mountFor` non-empty) mounts every export under
        // its Vesper-facing namespace: start the flatten at that prefix, so `es2015`'s
        // `Map` registers as `Js.Map` (nsPath `Js`) and a `node/fs` export registers
        // under `Node.Fs`, and every downstream site — `mint`/`originFor`/`buildCtx`/
        // `toTypeShape`/`funcs`/synthetic types — picks up the prefix from the SAME
        // `flatExports`. `mountPrefix = ""` for a real flat package (flatten at root).
        let mountPrefix = TsGlobalHomes.mountFor man.Package

        // Global rides the HOME, and is a SEPARATE axis from the mount: a global pack's
        // types are import-free, but a node module MOUNTS (`Node.Fs`) while STILL
        // requiring a real import — so this is `isGlobalHome`, NOT `mountPrefix <> ""`.
        // Stamped onto every class/interface shape below.
        let isGlobalPack = TsGlobalHomes.isGlobalHome man.Package

        let flatExports = flatten mountPrefix man.Exports

        // ONE pre-pass mints every declared `Interface`/`Class` identity (see
        // `TsManifestTranslate.mint`/`buildCtx`) over ALL flat exports before any
        // per-export walk, so a member signature that names a type declared LATER
        // (mitt's `mitt` referencing `Emitter`) still resolves.
        let ctx = buildCtx moduleSpec mountPrefix man.Refs flatExports

        let regularTypes =
            flatExports
            |> List.choose (fun (nsPath, ex) -> toTypeShape ctx isGlobalPack nsPath ex)

        let structuralTypes = buildStructuralTypes ctx flatExports

        let syntheticTypes =
            buildOverloadGroupingTypes ctx moduleSpec isGlobalPack flatExports

        // Guard the synthetic name against a real exported type of the same qualified
        // name (structurally possible only when a same-module type matches the
        // capitalised module segment): silently shadowing it would corrupt resolution.
        let regularTypeNames = regularTypes |> List.map fst |> Set.ofList

        do
            for (qn, _) in syntheticTypes do
                if Set.contains qn regularTypeNames then
                    failwithf
                        "synthetic free-function-overload grouping type '%s' collides with a real exported type of the same name; rename the module or the type"
                        qn

        // The structural erasing nominal is homed under the reserved `@struct` namespace,
        // which a real export's qualified name cannot spell — but keep the same collision
        // guard as the grouping type rather than trusting that reservation silently.
        do
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

        // The TS index signatures `{ [k: K]: V }` a type carries, keyed by the SAME
        // qualified name its members register under (so `TryLookupIndexSignature` and
        // `TryLookupMember` share one key). A named `Interface`/`Class` keys by its
        // `declaredIdentity` qn; an anonymous `Structural` shape (field-bearing OR a
        // fieldless bare `{ [k: K]: V }`) keys by its `structuralKey` (the same identity
        // `toFrozen` freezes it to). Each `(key,
        // value)` `Schema.TypeRef` pair is `toFrozen`ed over the type's declaring typars —
        // a generic `Dict<T>`'s value `T` stays `FTTypar(Declaring,0)`, realised against
        // the receiver's args at the lookup site. First-declaration-wins on a duplicate qn.
        let indexSigs =
            let named =
                flatExports
                |> List.choose (fun (nsPath, ex) ->
                    match ex with
                    | Schema.Export.Interface(name, tp, _, _, ((_ :: _) as index)) ->
                        Some(fst (declaredIdentity ctx nsPath name tp), index)
                    | Schema.Export.Class(name, tp, _, _, _, ((_ :: _) as index)) ->
                        Some(fst (declaredIdentity ctx nsPath name tp), index)
                    | _ -> None
                )

            let structural =
                flatExports
                |> List.collect (fun (_, ex) -> exportTypeRefs ex)
                |> List.collect structuralIndexSigsIn
                |> List.map (fun (hash, index) -> fst (structuralKey hash), index)

            (named @ structural)
            |> List.distinctBy fst
            |> List.map (fun (qn, index) -> qn, index |> List.map (fun (k, v) -> toFrozen ctx k, toFrozen ctx v))
            |> Map.ofList

        interface IExternalSymbolProvider

        interface IExternalSymbolResolver with
            member _.TryLookup name =
                match Map.tryFind name funcs with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupType(name: string) =
                match Map.tryFind name types with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupUnionCase _ = ValueNone
            member _.AmbientOpenPrefixes = []

        interface IExternalSymbolStore with
            member _.TryLookupType(key: SymbolKey) =
                match Map.tryFind (SymbolKeyOps.qualifiedName key) types with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupMember(key, memberName) =
                match membersOf (SymbolKeyOps.qualifiedName key) memberName with
                | [||] -> ValueNone
                | arr -> ValueSome arr.[0]

            member _.TryLookupMembers(key, memberName) =
                membersOf (SymbolKeyOps.qualifiedName key) memberName

            member _.TryLookupIndexSignature key =
                match Map.tryFind (SymbolKeyOps.qualifiedName key) indexSigs with
                | Some pairs -> pairs
                | None -> []

            member _.TryLookupInlineBody _ = ValueNone
            member _.IntrinsicReverseCanon = Map.empty
            member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr

    /// Build a provider from an already-parsed manifest.
    let providerOfManifest (man: Schema.PackageManifest) : IExternalSymbolProvider =
        TsManifestSymbolProvider man :> IExternalSymbolProvider

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
        // Resolve the covariant `number → float` identity of the retained TS token at the
        // Codegen.Js seam (the front end stays number-agnostic). Wraps the COMPOSED
        // provider so the assertion reads the merged forward axis.
        |> NumberCovariance.wrap
        // One general per-lookup cache atop the whole stack (the `stack` fall-through and
        // the `number` rewrite otherwise re-run on every hit of a hot symbol).
        |> ExternalSymbols.memoize
