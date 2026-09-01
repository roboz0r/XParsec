namespace XParsec.FSharp.Codegen.Js

open System.Collections.Generic
open System.IO

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest
open XParsec.FSharp.Codegen.Js.TsManifestTranslate
open XParsec.FSharp.Codegen.Js.TsManifestMembers

/// A parsed TS manifest → an `IExternalSymbolProvider`, through the `PublishedSurface` a
/// referenced Vesper package publishes: values, types, their members, and the scope holding
/// both, so a written `Js.spin` and `Js.Widget` resolve segment by segment.
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

    let private toFunctionSymbol (ctx: TranslateCtx) (nsPath: string) (ex: Schema.Export) : ExternalSymbol option =
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

            Some sym
        | _ -> None

    /// `_isConst` is dropped: the imported binding is read by name either way.
    let private toValueSymbol (ctx: TranslateCtx) (nsPath: string) (ex: Schema.Export) : ExternalSymbol option =
        match ex with
        | Schema.Export.Variable(name, ty, _isConst, import) ->
            let origin, decl = declaringContainer ctx nsPath

            let sym =
                ExternalSymbols.monoFrozen decl name (toFrozen ctx ty)
                |> stampValueSymbol origin import

            Some sym
        | _ -> None

    /// The `{ [k: K]: V }` signatures a manifest type carries, keyed by the type's identity.
    /// The one manifest channel a `PublishedSurface` has no table for.
    type private IndexSignatures
        (inner: IExternalSymbolProvider, byType: Dictionary<TypeKey, (FrozenType * FrozenType) list>) =
        inherit ExternalSymbolProviders.ProviderDecorator(inner)

        override _.TryLookupIndexSignature key =
            match byType.TryGetValue key with
            | true, pairs -> pairs
            | _ -> []

    type private ManifestPublication =
        {
            Surface: PublishedSurface
            IndexSignatures: Dictionary<TypeKey, (FrozenType * FrozenType) list>
        }

    /// Resolves every map and guard in the manifest EAGERLY into what it publishes.
    let private publicationOf (intrinsics: IntrinsicTypeMap) (man: Schema.PackageManifest) : ManifestPublication =
        let pkg = man.Package
        // Flat single-file package: the module specifier IS the package name.
        let moduleSpec = pkg

        let mountPrefix = TsGlobalHomes.mountFor man.Package

        let isGlobalPack = TsGlobalHomes.isGlobalHome man.Package

        // Flattening FROM the mount prefix is what registers `es2015`'s `Map` as `Js.Map`.
        let flatExports = flatten mountPrefix man.Exports

        let ctx = buildCtx intrinsics moduleSpec mountPrefix man.Refs flatExports

        let regularTypes =
            flatExports
            |> List.choose (fun (nsPath, ex) -> toTypeShape ctx isGlobalPack nsPath ex)

        let structuralTypes = buildStructuralTypes ctx flatExports

        let syntheticTypes =
            buildOverloadGroupingTypes ctx moduleSpec isGlobalPack flatExports

        let declaredTypes = HashSet<TypeKey>(HashIdentity.Structural)

        for (declared, _) in regularTypes do
            declaredTypes.Add declared.Key |> ignore

        do
            for (declared, _) in syntheticTypes do
                if declaredTypes.Contains declared.Key then
                    failwithf
                        "synthetic free-function-overload grouping type '%s' collides with a real exported type of the same name; rename the module or the type"
                        declared.QualifiedName

        do
            for (declared, _) in structuralTypes do
                if declaredTypes.Contains declared.Key then
                    failwithf
                        "synthetic structural type '%s' collides with a real exported type of the same name"
                        declared.QualifiedName

        // Free functions and variables both publish as values. An OVERLOADED function is
        // excluded: it resolves as a static of its synthetic grouping type, not by bare name.
        let values =
            (flatExports
             |> List.choose (fun (nsPath, ex) ->
                 match ex with
                 | Schema.Export.Function(_, sigs, _) when List.length sigs > 1 -> None
                 | _ -> toFunctionSymbol ctx nsPath ex
             ))
            @ (flatExports |> List.choose (fun (nsPath, ex) -> toValueSymbol ctx nsPath ex))

        let surface =
            PublishedSurface.build (fun published ->
                for sym in values do
                    PublishedSurfaceBuilder.addValue published sym

                for (declared, shape) in regularTypes @ syntheticTypes @ structuralTypes do
                    let members =
                        match shape with
                        | ExternalTypeShape.Class c -> c.Members
                        | _ -> EqArray.empty

                    PublishedSurfaceBuilder.addTypeWith published declared.Key shape members
            )

        let indexSignatures =
            let named =
                flatExports
                |> List.choose (fun (nsPath, ex) ->
                    match ex with
                    | Schema.Export.Interface(name, tp, _, _, ((_ :: _) as index)) ->
                        Some((declaredIdentity ctx nsPath name tp).Key, index)
                    | Schema.Export.Class(name, tp, _, _, _, ((_ :: _) as index)) ->
                        Some((declaredIdentity ctx nsPath name tp).Key, index)
                    | _ -> None
                )

            let structural =
                flatExports
                |> List.collect (fun (_, ex) -> exportTypeRefs ex)
                |> List.collect structuralIndexSigsIn
                |> List.map (fun (hash, index) -> (structuralKey hash).Key, index)

            let byType =
                Dictionary<TypeKey, (FrozenType * FrozenType) list>(HashIdentity.Structural)

            for (key, index) in named @ structural do
                byType.TryAdd(key, index |> List.map (fun (k, v) -> toFrozen ctx k, toFrozen ctx v))
                |> ignore

            byType

        {
            Surface = surface
            IndexSignatures = indexSignatures
        }

    /// Build a provider from an already-parsed manifest, minting `Vesper` identities for the
    /// canon names `intrinsics` declares.
    let providerOfManifest (intrinsics: IntrinsicTypeMap) (man: Schema.PackageManifest) : IExternalSymbolProvider =
        let published = publicationOf intrinsics man
        IndexSignatures(PublishedSurface.toProvider published.Surface, published.IndexSignatures)

    /// Parse a manifest JSON file.
    let tryLoadFile (path: string) : Result<Schema.PackageManifest, string> =
        try
            File.ReadAllText path |> Codec.deserialize
        with ex ->
            Error(sprintf "Failed to read TS manifest '%s': %s" path ex.Message)

    /// The TS-manifest providers as the JS layer-2 platform metadata, behind referenced-package
    /// contracts. `packageDirs` are `.fsi` package directories, `tsManifestPaths` extractor JSON.
    let buildContract (packageDirs: string list) (tsManifestPaths: string list) : IExternalSymbolProvider =
        let manifests =
            tsManifestPaths
            |> List.map (fun p ->
                match tryLoadFile p with
                | Ok man -> man
                | Error msg -> failwith msg
            )

        let tsMetadata: SymbolProviders.PlatformMetadataFactory =
            fun intrinsics -> manifests |> List.map (providerOfManifest intrinsics)

        (SymbolProviders.buildContract tsMetadata Target.Js packageDirs).Provider
        // Wraps the COMPOSED stack: its `float` must-repr-to-`number` check reads the merged axis.
        |> NumberCovariance.wrap
        |> ExternalSymbolProviders.memoize
