namespace XParsec.FSharp.Codegen.Js

open System.IO

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest
open XParsec.FSharp.Codegen.Js.TsManifestTranslate
open XParsec.FSharp.Codegen.Js.TsManifestMembers

/// A parsed TS manifest → an `IExternalSymbolProvider`: value symbols (free functions and
/// variables), the type/synthetic-type table, and the by-name lookup maps over both.
module TsManifestProvider =

    let private singleSignature (name: string) (sigs: Schema.Signature list) : Schema.Signature =
        match sigs with
        | [ s ] -> s
        | [] -> failwithf "symbol '%s' has no call signature" name
        | _ -> failwithf "symbol '%s' has %d overloads; overload sets not yet supported" name (List.length sigs)

    /// A TS `export namespace Foo` mints a namespace, so `Foo.bar`'s container is `InNamespace Foo`.
    let private declaringContainer (ctx: TranslateCtx) (nsPath: string) : SymbolOrigin * ModuleContainer =
        let origin = originFor ctx nsPath
        origin, ModuleContainer.InNamespace origin.Namespace

    /// The symbol constructors mint `Origin = SymbolOrigin.Empty`; the module spec stamped
    /// here becomes the `'<mod>'` of `import x from '<mod>'`, and a home-less origin throws.
    let private stampValueSymbol
        (origin: SymbolOrigin)
        (import: Schema.ImportShape)
        (sym: ExternalSymbol)
        : ExternalSymbol =
        { sym with
            Origin = origin
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

            // Trailing optionals drop out of the curried signature: `mitt(all?)` freezes as
            // `unit -> ret`, and the JS-side default (`n = n || new Map`) supplies the arg.
            let requiredParams =
                sg.Params |> List.rev |> List.skipWhile (fun p -> p.Optional) |> List.rev

            let paramTypes =
                match requiredParams with
                | [] -> [ unitFrozen ]
                | ps -> ps |> List.map (fun p -> toFrozen ctx p.Type)

            let frozenTy =
                List.foldBack (fun a acc -> FTFun(a, acc)) paramTypes (toFrozen ctx sg.Returns)

            let origin, decl = declaringContainer ctx nsPath

            let sym =
                ExternalSymbols.scheme decl name frozenTy sg.TypeParams []
                |> stampValueSymbol origin import

            Some(sym.Name, sym)
        | _ -> None

    /// `_isConst` is dropped: the imported binding is read by name either way.
    let private toValueSymbol
        (ctx: TranslateCtx)
        (nsPath: string)
        (ex: Schema.Export)
        : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Variable(name, ty, _isConst, import) ->
            let origin, decl = declaringContainer ctx nsPath

            let sym =
                ExternalSymbols.monoFrozen decl name (toFrozen ctx ty)
                |> stampValueSymbol origin import

            Some(sym.Name, sym)
        | _ -> None

    /// Resolves every map and guard in the manifest EAGERLY into a by-name leaf.
    let private manifestLeaf (man: Schema.PackageManifest) : ExternalSymbolProviders.NamedLeaf =
        let pkg = man.Package
        // Flat single-file package: the module specifier IS the package name.
        let moduleSpec = pkg

        let mountPrefix = TsGlobalHomes.mountFor man.Package

        let isGlobalPack = TsGlobalHomes.isGlobalHome man.Package

        // Flattening FROM the mount prefix is what registers `es2015`'s `Map` as `Js.Map`.
        let flatExports = flatten mountPrefix man.Exports

        let ctx = buildCtx moduleSpec mountPrefix man.Refs flatExports

        let regularTypes =
            flatExports
            |> List.choose (fun (nsPath, ex) -> toTypeShape ctx isGlobalPack nsPath ex)

        let structuralTypes = buildStructuralTypes ctx flatExports

        let syntheticTypes =
            buildOverloadGroupingTypes ctx moduleSpec isGlobalPack flatExports

        let regularTypeNames = regularTypes |> List.map fst |> Set.ofList

        do
            for (qn, _) in syntheticTypes do
                if Set.contains qn regularTypeNames then
                    failwithf
                        "synthetic free-function-overload grouping type '%s' collides with a real exported type of the same name; rename the module or the type"
                        qn

        do
            for (qn, _) in structuralTypes do
                if Set.contains qn regularTypeNames then
                    failwithf "synthetic structural type '%s' collides with a real exported type of the same name" qn

        let types = (regularTypes @ syntheticTypes @ structuralTypes) |> Map.ofList

        // Free functions and variables share this by-name map. An OVERLOADED function is
        // excluded: it resolves as a static of its synthetic grouping type, not by bare name.
        let funcs =
            (flatExports
             |> List.choose (fun (nsPath, ex) ->
                 match ex with
                 | Schema.Export.Function(_, sigs, _) when List.length sigs > 1 -> None
                 | _ -> toFunctionSymbol ctx nsPath ex
             ))
            @ (flatExports |> List.choose (fun (nsPath, ex) -> toValueSymbol ctx nsPath ex))
            |> Map.ofList

        let membersOf (key: ExternalMemberName) : EqArray<ExternalMember> =
            match Map.tryFind key.DeclaringType types with
            | Some(ExternalTypeShape.Class shape) -> shape.Members |> EqArray.filter (fun m -> m.Name = key.Name)
            | _ -> EqArray.empty

        // The TS `{ [k: K]: V }` signatures a type carries, under the SAME qualified name its
        // members register under, so one key answers a member and an index lookup alike.
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

        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookup =
                fun name ->
                    match Map.tryFind name funcs with
                    | Some s -> ValueSome s
                    | None -> ValueNone
            TryLookupType =
                fun name ->
                    match Map.tryFind name types with
                    | Some s -> ValueSome s
                    | None -> ValueNone
            TryLookupMembers = membersOf
            TryLookupIndexSignature =
                fun typeName ->
                    match Map.tryFind typeName indexSigs with
                    | Some pairs -> pairs
                    | None -> []
        }

    /// Build a provider from an already-parsed manifest.
    let providerOfManifest (man: Schema.PackageManifest) : IExternalSymbolProvider =
        ExternalSymbolProviders.ofNamedLeaf (manifestLeaf man)

    /// Parse a manifest JSON file and build its provider.
    let tryLoadFile (path: string) : Result<IExternalSymbolProvider, string> =
        try
            File.ReadAllText path |> Codec.deserialize |> Result.map providerOfManifest
        with ex ->
            Error(sprintf "Failed to read TS manifest '%s': %s" path ex.Message)

    /// The TS-manifest providers as the JS layer-2 metadata tail, behind referenced-package
    /// contracts. `packageDirs` are `.fsi` package directories, `tsManifestPaths` extractor JSON.
    let buildContract (packageDirs: string list) (tsManifestPaths: string list) : IExternalSymbolProvider =
        let tsProviders =
            tsManifestPaths
            |> List.map (fun p ->
                match tryLoadFile p with
                | Ok prov -> prov
                | Error msg -> failwith msg
            )

        SymbolProviders.buildContractWithMetadata "tsmanifest" tsProviders Target.Js packageDirs
        // Wraps the COMPOSED stack: its `float` must-repr-to-`number` check reads the merged axis.
        |> NumberCovariance.wrap
        |> ExternalSymbolProviders.memoize
