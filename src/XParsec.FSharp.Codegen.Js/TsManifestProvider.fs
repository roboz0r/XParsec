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

    let private firstSignature (sigs: Schema.Signature list) : Schema.Signature =
        // MVP collapses overloads to the first; full overload sets ride
        // TryLookupMembers once the extractor emits them all.
        match sigs with
        | s :: _ -> s
        | [] ->
            {
                TypeParams = 0
                Params = []
                Returns = Schema.TypeRef.Named("unit", [])
            }

    let private toExternalMember
        (declKey: SymbolKey)
        (origin: SymbolOrigin)
        (declArity: int)
        (isInterface: bool)
        (mem: Schema.Member)
        : ExternalMember =
        match mem.Kind with
        | Schema.MemberKind.Property ->
            let ret =
                match mem.Type with
                | Some t -> toFrozen t
                | None -> unitFrozen

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
        | Schema.MemberKind.Method ->
            let sg = firstSignature mem.Signatures

            let kind =
                if isInterface && not mem.Static then
                    MemberKind.InterfaceMethod declKey
                else
                    MemberKind.Method

            {
                Name = mem.Name
                IsStatic = mem.Static
                IsProperty = false
                Signature =
                    {
                        DeclaringArity = declArity
                        MethodArity = sg.TypeParams
                        Parameters = paramsFrozen sg.Params
                        Return = toFrozen sg.Returns
                    }
                MethodArity = sg.TypeParams
                Origin = origin
                Key = SymbolKey.MemberKey(declKey, mem.Name, EqArray.empty, kind)
                OptionalDefaults = []
            }

    let private originFor (pkg: string) : SymbolOrigin =
        // TODO: re-role to the module specifier / namespace once ImportShape is
        // threaded through; today the package name stands in as the home label.
        {
            Assembly = Some pkg
            Namespace = ""
            DeclaringType = None
        }

    let private toTypeShape (pkg: string) (ex: Schema.Export) : (string * ExternalTypeShape) option =
        let build name tp members isInterface =
            let origin = originFor pkg
            let key = SymbolKey.TypeKey(Some pkg, "", name)

            let mems =
                members |> List.map (toExternalMember key origin tp isInterface) |> List.toArray

            Some(
                name,
                ExternalTypeShape.Class
                    {
                        Arity = tp
                        IsInterface = isInterface
                        Members = mems
                        FrozenInterfaces = [||] // TODO: map heritage
                        FrozenBaseType = ValueNone
                        Flags = ExternalClassFlags.Default
                        Origin = origin
                    }
            )

        match ex with
        | Schema.Export.Interface(name, tp, members, _heritage) -> build name tp members true
        | Schema.Export.Class(name, tp, members, _heritage, _import) -> build name tp members false
        | _ -> None

    let private toFunctionSymbol (ex: Schema.Export) : (string * ExternalSymbol) option =
        match ex with
        | Schema.Export.Function(name, signatures, _import) ->
            let sg = firstSignature signatures

            let paramTypes =
                match sg.Params with
                | [] -> [ TyConst("unit", EqArray.empty) ]
                | ps -> ps |> List.map (fun p -> toSem p.Type)

            let semTy = List.foldBack (fun a acc -> TyFun(a, acc)) paramTypes (toSem sg.Returns)
            Some(name, ExternalSymbols.mono name semTy)
        | _ -> None

    /// Build a provider from an already-parsed manifest.
    let providerOfManifest (man: Schema.PackageManifest) : IExternalSymbolProvider =
        let pkg = man.Package
        let types = man.Exports |> List.choose (toTypeShape pkg) |> Map.ofList
        let funcs = man.Exports |> List.choose toFunctionSymbol |> Map.ofList

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
