namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest
open XParsec.FSharp.Codegen.Js.TsManifestTranslate

/// A manifest export → the seam's `ExternalMember`s, one per overload signature, and its
/// `ExternalTypeShape`.
module internal TsManifestMembers =

    /// TRAILING optionals a call may omit (`readFile(path, cb, opts?)` ⇒ 1). TS forbids a
    /// required parameter after an optional, so counting from the end is exact. A trailing
    /// REST is NOT one: omitting `...args: T[]` means ZERO args, not one `undefined`.
    let private trailingOptionalCount (ps: Schema.Param list) : int =
        ps
        |> List.rev
        |> List.takeWhile (fun p -> p.Optional && not p.Rest)
        |> List.length

    /// One seam ctor per overload signature, each keyed by its own interned `argSig`;
    /// `ExternalMember.ctor` bakes the `.ctor` name and method kind a lookup asks for.
    let private expandCtor
        (ctx: TranslateCtx)
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (declTyparArity: int)
        (mem: Schema.Member)
        : ExternalMember list =
        overloadArgSigs ctx mem
        |> List.map (fun (argSig, sg) ->
            ExternalMember.ctor declKey (signatureOf ctx declTyparArity sg) (EqArray.ofList argSig) origin []
        )

    /// One `ExternalMember` per overload signature, each keyed by its own interned `argSig`
    /// so a by-name lookup sees them as distinct candidates rather than one member.
    let expandMethod
        (ctx: TranslateCtx)
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (declTyparArity: int)
        (kind: MemberKind)
        (mem: Schema.Member)
        : ExternalMember list =
        overloadArgSigs ctx mem
        |> List.map (fun (argSig, sg) ->
            { ExternalMember.OfKey(
                  SymbolKeyOps.memberKeyOf declKey mem.Name (EqArray.ofList argSig) sg.TypeParams kind
              ) with
                IsStatic = mem.Static
                Signature = signatureOf ctx declTyparArity sg
                Origin = origin
                OptionalDefaults = List.replicate (trailingOptionalCount sg.Params) OptionalDefault.Omitted
                IsOptional = mem.Optional
            }
        )

    let toExternalMembers
        (ctx: TranslateCtx)
        (declKey: TypeKey)
        (origin: SymbolOrigin)
        (declTyparArity: int)
        (isInterface: bool)
        (mem: Schema.Member)
        : ExternalMember list =
        match mem.Kind with
        | Schema.MemberKind.Method when mem.Name = ".ctor" -> expandCtor ctx declKey origin declTyparArity mem
        | Schema.MemberKind.Property ->
            let ret =
                match mem.Type with
                | Some t -> toFrozen ctx t
                | None -> unitFrozen

            [
                { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey mem.Name EqArray.empty 0 MemberKind.Property) with
                    IsStatic = mem.Static
                    Storage = MemberStorage.Property
                    Signature = ExternalSignature.value (declTyparArity, 0, ret)
                    Origin = origin
                    IsOptional = mem.Optional
                }
            ]
        | Schema.MemberKind.Method ->
            let kind =
                if isInterface && not mem.Static then
                    MemberKind.InterfaceMethod declKey
                else
                    MemberKind.Method

            expandMethod ctx declKey origin declTyparArity kind mem

    /// The schema's `heritage` is FLAT: nothing in it marks which entry is the base class,
    /// so each entry's name is resolved against the manifest's own type table. A class takes
    /// the single base slot, an interface or an unresolved cross-package name the list.
    let private classifyHeritage
        (ctx: TranslateCtx)
        (heritage: Schema.TypeRef list)
        : EqArray<FrozenNominal> * FrozenNominal voption =
        let interfaces = ResizeArray<FrozenNominal>()
        let mutable baseTy = ValueNone

        for h in heritage do
            // TS cannot express a structural or union supertype, so a non-nominal entry
            // is a corrupt manifest, not a shape to drop.
            let name, args =
                match h with
                | Schema.TypeRef.Named(name, args) -> name, args
                | other -> failwithf "heritage entry is not a nominal type reference: %A" other

            // The table keys by the arity-suffixed name: `extends Foo<T>` classifies under
            // `` Foo`1 ``. `arityName` is a no-op at arity 0.
            match ctx.TryFindType(SymbolKeyOps.arityName name (List.length args)) with
            | Some id when not id.IsInterface ->
                baseTy <- ValueSome(FrozenNominal.ofFrozen "a manifest heritage base" (toFrozen ctx h))
            | found ->
                let ifaceArgs = args |> List.map (toFrozen ctx) |> Array.ofList

                // A name this package declares takes its REGISTERED identity; a cross-package
                // or unknown one has only its arity-suffixed spelling to cut a key from.
                let key =
                    match found with
                    | Some id -> id.Minted.Key
                    | None ->
                        SymbolKeyOps.qualifiedTypeKeyOf (SymbolKeyOps.arityName name ifaceArgs.Length) ifaceArgs.Length

                interfaces.Add(NominalG.ofClass key (EqArray.ofArray ifaceArgs))

        EqArray.ofResizeArray interfaces, baseTy

    /// TypeScript escapes a `[Symbol.iterator]()` method as `__@iterator@<symbolId>`; the
    /// trailing id varies by lib (`@1`, `@112`), so match by prefix.
    [<Literal>]
    let private symbolIteratorPrefix = "__@iterator"

    /// A TS `[Symbol.iterator](): Iterator<T>` IS the `seq<'T>` capability on JS, because both
    /// are the native iterator protocol. The element is the FIRST type arg of the returned
    /// iterator: `IterableIterator<T>` → `T`, `Map`'s `IterableIterator<[K,V]>` → `[K,V]`.
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
        : (MintedType * ExternalTypeShape) option =
        let build name tp members heritage isInterface =
            let origin = originFor ctx nsPath
            let declared = declaredIdentity ctx nsPath name tp

            let mems =
                members
                |> List.collect (toExternalMembers ctx declared.Key origin tp isInterface)
                |> EqArray.ofList

            let heritageInterfaces, frozenBaseType = classifyHeritage ctx heritage

            // Adding the `seq` capability with the peeled element is what makes `for … in`
            // over this type lower to `for..of`: the recogniser scans the interface set for
            // the enumerable capability.
            let frozenInterfaces =
                match tryIteratorElement ctx members with
                | ValueSome elem ->
                    EqArray.ofSeq
                        [
                            yield! heritageInterfaces
                            NominalG.ofClass RuntimeNames.seqKey (EqArray.singleton elem)
                        ]
                | ValueNone -> heritageInterfaces

            Some(
                declared,
                ExternalTypeShape.Class
                    {
                        TyparArity = tp
                        IsInterface = isInterface
                        Members = mems
                        FrozenInterfaces = frozenInterfaces
                        FrozenBaseType = frozenBaseType
                        // A manifest type is a native object: its instance members live ON it
                        // as prototype/own methods, so a call emits `objArg.member(args)`,
                        // not a type-prefixed free function.
                        Flags =
                            { ExternalClassFlags.Default with
                                MemberLowering = MemberLowering.AttachedNative
                                // A global pack's types emit as bare names, with no `import`.
                                Global = isGlobal
                            }
                        Attributes = EqArray.empty
                        Origin = origin
                    }
            )

        match ex with
        | Schema.Export.Interface(name, tp, members, heritage, _index) -> build name tp members heritage true
        | Schema.Export.Class(name, tp, members, heritage, _import, _index) -> build name tp members heritage false
        | Schema.Export.TypeAlias(name, tp, target) ->
            // `type X = …` is a transparent abbreviation: a use of `name` expands to the
            // target's `FrozenType`. `mint`, not `declaredIdentity`, because an alias never
            // enters the ctx table, so it stays `FTConst` and expands through this `Abbrev`.
            Some(mint nsPath name tp, ExternalTypeShape.Abbrev(tp, toFrozen ctx target))
        | Schema.Export.Enum(name, members) ->
            // A computed (non-constant) member has no value to reference it by, so it
            // cannot be a case at all and is dropped.
            let origin = originFor ctx nsPath

            let cases =
                members
                |> List.choose (fun (caseName, v) ->
                    match v with
                    | Some(Schema.LiteralValue.IntVal n) ->
                        Some(
                            {
                                Name = caseName
                                Value = ExternalEnumCaseValue.IntVal n
                            }
                            : ExternalEnumCaseShape
                        )
                    | Some(Schema.LiteralValue.StringVal s) ->
                        Some(
                            {
                                Name = caseName
                                Value = ExternalEnumCaseValue.StringVal s
                            }
                            : ExternalEnumCaseShape
                        )
                    | None -> None
                )
                |> EqArray.ofList

            Some(mint nsPath name 0, ExternalTypeShape.Enum(cases, origin))
        | _ -> None

    /// One ERASING nominal per distinct anonymous object shape reachable from the exports,
    /// deduped by shape-hash: a ctor-less interface whose Property members lower to native
    /// `objArg.field` reads, and for which NOTHING is emitted.
    let buildStructuralTypes
        (ctx: TranslateCtx)
        (flatExports: (string * Schema.Export) list)
        : (MintedType * ExternalTypeShape) list =
        flatExports
        |> List.collect (fun (_, ex) -> exportTypeRefs ex)
        |> List.collect structuralShapesIn
        |> List.map (fun (printed, fields) -> structuralHash printed fields, fields)
        |> List.distinctBy fst
        |> List.map (fun (hash, fields) ->
            let declared = structuralKey hash

            let origin: SymbolOrigin =
                {
                    Home = SymbolHome.InAssembly(AssemblyName structuralHome)
                    Namespace = SymbolKeyOps.namespaceKey structuralHome
                }

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

                    toExternalMembers ctx declared.Key origin 0 true mem
                )
                |> EqArray.ofList

            declared,
            ExternalTypeShape.Class
                {
                    TyparArity = 0
                    IsInterface = true
                    Members = members
                    FrozenInterfaces = EqArray.empty
                    FrozenBaseType = ValueNone
                    Flags =
                        { ExternalClassFlags.Default with
                            MemberLowering = MemberLowering.AttachedNative
                            // No home module to import: the shape itself emits nothing.
                            Global = true
                        }
                    Attributes = EqArray.empty
                    Origin = origin
                }
        )

    /// The grouping type's simple name: the LAST '/'-segment of the module specifier
    /// (`@scope/util` → `util`), first character upper-cased (`util` → `Util`).
    let private syntheticTypeName (moduleSpec: string) : string =
        let lastSeg =
            match moduleSpec.Split('/') |> Array.filter (fun s -> s <> "") |> Array.tryLast with
            | Some s -> s
            | None -> moduleSpec

        if lastSeg = "" then
            lastSeg
        else
            string (System.Char.ToUpperInvariant lastSeg.[0]) + lastSeg.Substring 1

    /// A free-function export with N>1 call signatures, bound for its namespace's
    /// grouping type.
    type private OverloadedFn =
        {
            NsPath: string
            Name: string
            Signatures: Schema.Signature list
            Import: Schema.ImportShape
        }

    /// F# has no free-function overloading, so `export function format(x: string); export
    /// function format(x: number);` cannot share the name-keyed `funcs` map. Such exports
    /// become statics of one synthetic type per namespace; `Util.format(x)` erases to `format(x)`.
    let buildOverloadGroupingTypes
        (ctx: TranslateCtx)
        (moduleSpec: string)
        (isGlobalPack: bool)
        (flatExports: (string * Schema.Export) list)
        : (MintedType * ExternalTypeShape) list =
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
            // One home module ⇒ one import form: a module cannot be both `export =` and
            // named-export, so a mixed group is a corrupt manifest.
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
            // `mint`, not `declaredIdentity`: the grouping type never enters the ctx table,
            // but is found by qualified name through the seam's lookups.
            let declared = mint nsPath simpleName 0
            let origin = originFor ctx nsPath

            let members =
                fns
                |> List.collect (fun fn ->
                    // The member name stays the REAL export name: that is what the erase
                    // emits as the callee.
                    let mem: Schema.Member =
                        {
                            Name = fn.Name
                            Kind = Schema.MemberKind.Method
                            Type = None
                            Signatures = fn.Signatures
                            Static = true
                            Optional = false
                        }

                    expandMethod ctx declared.Key origin 0 MemberKind.Method mem
                )
                |> EqArray.ofList

            declared,
            ExternalTypeShape.Class
                {
                    TyparArity = 0
                    IsInterface = false
                    Members = members
                    FrozenInterfaces = EqArray.empty
                    FrozenBaseType = ValueNone
                    Flags =
                        { ExternalClassFlags.Default with
                            MemberLowering = MemberLowering.ErasedBare
                            Global = isGlobalPack
                            // Picks the emitted import: `import { format }` (Named),
                            // `import format` (Default/CommonJs), `import * as util` plus
                            // `util.format` (Namespace).
                            ImportForm = groupImportForm
                        }
                    Attributes = EqArray.empty
                    Origin = origin
                }
        )
