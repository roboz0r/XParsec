namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest

/// Type translation for the TS-manifest provider: the manifest's `TypeRef` grammar
/// → the seam's `FrozenType` / `ExternalSignature`, plus the per-manifest
/// `TranslateCtx` (the type-identity table every walk threads). Consumed by
/// `TsManifestMembers` and `TsManifestProvider`.
module internal TsManifestTranslate =

    /// The lookup key a symbol declared at namespace path `nsPath` is registered/found
    /// under: its DOTTED QUALIFIED name (`NS.Foo`, `NS.Inner.Baz`), matching exactly
    /// the name `Passes.NameResolution` forms from a `NS.Foo` use site and hands to
    /// `TryLookupType`/`TryLookup` — the same full-dotted-name convention `MetadataSymbols`
    /// keys a .NET namespaced type by. A top-level export (`nsPath = ""`) keeps its bare name.
    let qualify (nsPath: string) (name: string) : string =
        if nsPath = "" then name else nsPath + "." + name

    /// Flatten the export tree into (namespacePath, export) pairs: a top-level export
    /// pairs with `""`; a member nested in one (or more) `Export.Namespace`s pairs with
    /// its dotted path (`NS`, `NS.Inner`). The `Namespace` container itself produces no
    /// symbol — only its members do, each registered under its QUALIFIED name
    /// (`qualify`), so a nested symbol resolves through the SAME flat maps as a
    /// top-level one. Recursion folds arbitrarily deep nesting.
    let rec flatten (nsPath: string) (exports: Schema.Export list) : (string * Schema.Export) list =
        exports
        |> List.collect (fun ex ->
            match ex with
            | Schema.Export.Namespace(nsName, nested) -> flatten (qualify nsPath nsName) nested
            | other -> [ nsPath, other ]
        )

    /// The identity minted for one declared `Interface`/`Class` export. `IsInterface`
    /// disambiguates a heritage entry's slot (interface list vs base class) in
    /// `classifyHeritage`; only these two export kinds enter the table at all.
    type TypeIdentity = { Key: SymbolKey; IsInterface: bool }

    /// THE LAW (`SymbolKeyOps.arityName`) — the ONE spelling site of a declared type's
    /// identity. A generic nominal type's compiled name is arity-suffixed
    /// (`` Emitter`1 ``) and (name, arity) pairs are DISTINCT nominal types, so the
    /// SIMPLE name minted into the `TypeKey` carries the `` `n `` suffix by the declared
    /// arity — matching what `TypeTranslate`/`Freeze` form when resolving an annotation
    /// (`arityName`, suffixed-first; a no-op at arity 0). The returned pair is the two
    /// faces of that one identity: the MAP key (dotted qualified name — the exact string
    /// the front end hands to `TryLookupType`/`TryLookupMember`) and the
    /// `SymbolKey.TypeKey` (simple suffixed name + namespace-path split, the
    /// codegen-minting decomposition — mirroring `MetadataSymbols`, where the key
    /// decomposes `Type.FullName` but the lookup string is the full name).
    /// `SymbolKeyOps.qualifiedName` of the key equals the map key by construction.
    /// Reference sites (`toFrozen`, `classifyHeritage`) suffix the manifest's bare
    /// spelling by the APPLIED arg count before probing the table: a TS `Named`
    /// reference applies ALL its type args (TS has no partial application and no arity
    /// overloading), so the applied count IS the declared arity for an in-package
    /// resolution.
    let mint (moduleSpec: string) (nsPath: string) (name: string) (arity: int) : string * SymbolKey =
        let simple = SymbolKeyOps.arityName name arity
        qualify nsPath simple, SymbolKey.TypeKey(Some moduleSpec, nsPath, simple)

    /// The per-manifest translation context, threaded as ONE argument through every
    /// walk rather than positional parameters: a new per-manifest fact (a refs table
    /// for cross-package identity, an ambient-global flag) lands as one field here,
    /// not a re-thread of ten signatures.
    type TranslateCtx =
        {
            /// The manifest's name→identity table for names declared as an
            /// `Interface`/`Class`, keyed by the `mint`ed qualified name. Built by
            /// `buildCtx` over ALL flat exports before any per-export walk, so a member
            /// signature that names a type declared LATER (mitt's `mitt` referencing
            /// `Emitter`) still resolves. A primitive, a cross-package name, and a
            /// `TypeAlias` name all MISS — deliberately (see `Resolve`).
            Types: Map<string, TypeIdentity>
            /// The symbols' import path (for a flat single-file package, the package
            /// name); stamped into every minted key and `SymbolOrigin`.
            ModuleSpec: string
        }

        /// THE gate that turns a nominal `Named` into `FTClass` — and only for a
        /// class/interface: a primitive, an unresolved/cross-package name, and a
        /// `TypeAlias` name all miss the table and stay `FTConst` (aliases stay
        /// transparent through `ExternalTypeShape.Abbrev`). This is what makes a
        /// manifest interface resolve as `TyClass` at the front end so
        /// `resolveFieldStep`'s external-`TyClass` arm admits `.member` access via the
        /// provider. `SymbolKeyOps.qualifiedName` of the returned key equals the table
        /// key (`mint`) — the exact string the front end hands to `TryLookupMember`.
        member ctx.Resolve(name: string) : SymbolKey option =
            ctx.Types |> Map.tryFind name |> Option.map (fun id -> id.Key)

        /// The full declared identity (key + interface-vs-class kind); `None` for a
        /// name not declared in this package (cross-package / unknown).
        member ctx.TryFindType(name: string) : TypeIdentity option = Map.tryFind name ctx.Types

    /// Build the per-manifest context: ONE pre-pass over the flat exports, minting
    /// each declared `Interface`/`Class` identity through `mint` exactly once. Only
    /// those two export kinds register — the mirror image of what `Resolve` must miss.
    let buildCtx (moduleSpec: string) (flatExports: (string * Schema.Export) list) : TranslateCtx =
        let types =
            flatExports
            |> List.choose (fun (nsPath, ex) ->
                match ex with
                | Schema.Export.Interface(name, tp, _, _) ->
                    let qn, key = mint moduleSpec nsPath name tp
                    Some(qn, { Key = key; IsInterface = true })
                | Schema.Export.Class(name, tp, _, _, _) ->
                    let qn, key = mint moduleSpec nsPath name tp
                    Some(qn, { Key = key; IsInterface = false })
                | _ -> None
            )
            |> Map.ofList

        {
            Types = types
            ModuleSpec = moduleSpec
        }

    /// The identity `buildCtx` registered for a DECLARED `Interface`/`Class` export —
    /// looked up from the table, never re-minted, so the shape a type walk registers
    /// and the key `Resolve` hands out cannot diverge. Total for exports of the
    /// manifest the ctx was built from; a miss means the table builder and the export
    /// walker disagree on the export list (a bug, not a data condition).
    let declaredIdentity (ctx: TranslateCtx) (nsPath: string) (name: string) (arity: int) : string * SymbolKey =
        let qn = fst (mint ctx.ModuleSpec nsPath name arity)

        match Map.tryFind qn ctx.Types with
        | Some id -> qn, id.Key
        | None -> failwithf "declared type '%s' is missing from the identity table" qn

    let originFor (ctx: TranslateCtx) (nsPath: string) : SymbolOrigin =
        // The home label is the symbol's MODULE SPECIFIER (the import path), not the
        // package name. For a flat single-file package the module spec and package
        // coincide. `Namespace` carries the symbol's namespace PATH within the module:
        // "" for a top-level export, `NS`/`NS.Inner` for a member nested in one (or
        // more) `export namespace`s — the JS analog of a .NET `Type.Namespace`.
        {
            Assembly = Some ctx.ModuleSpec
            Namespace = nsPath
            DeclaringType = None
        }

    // ─── TypeRef → FrozenType (member signature templates) ─────────────────

    let rec toFrozen (ctx: TranslateCtx) (t: Schema.TypeRef) : FrozenType =
        let nominal name (args: FrozenType[]) =
            // Suffix the manifest's bare spelling by the applied arg count before
            // probing the table (THE LAW, see `mint`). The `FTConst` fallback keeps the
            // BARE name (a primitive/cross-package/alias name carries no arity suffix).
            match ctx.Resolve(SymbolKeyOps.arityName name args.Length) with
            | Some key -> FTClass(key, EqArray.ofSeq args)
            | None -> FTConst(name, EqArray.ofSeq args)

        match t with
        | Schema.TypeRef.Named(name, []) -> nominal name [||]
        | Schema.TypeRef.Named(name, args) -> nominal name (List.map (toFrozen ctx) args |> Array.ofList)
        | Schema.TypeRef.Typar i -> FTTypar(TyparAxis.Declaring, i)
        | Schema.TypeRef.MethodTypar i -> FTTypar(TyparAxis.Method, i)
        | Schema.TypeRef.Fun(args, ret) ->
            List.foldBack (fun a acc -> FTFun(toFrozen ctx a, acc)) args (toFrozen ctx ret)
        | Schema.TypeRef.Tuple items -> FTTuple(EqArray.ofSeq (List.map (toFrozen ctx) items))
        // Route through the smart constructor — flatten/dedupe/collapse per TS's
        // semantic union rules (a singleton `("a")` collapses to `FTLiteral "a"`).
        | Schema.TypeRef.Union members -> FrozenType.MkUnion(List.map (toFrozen ctx) members)
        // A TS literal TYPE → `FTLiteral` (structural, external-vocabulary only).
        | Schema.TypeRef.Literal(Schema.LiteralValue.StringVal s) -> FTLiteral(LiteralConst.String s)
        | Schema.TypeRef.Literal(Schema.LiteralValue.IntVal n) -> FTLiteral(LiteralConst.Int n)
        // keyof / indexed-access / conditional → CARRIER `FrozenType` nodes, INERT:
        // rehydrated with their children, threaded through every walk, but NOT
        // evaluated — the front end owns the ground fold.
        | Schema.TypeRef.KeyOf t -> FTKeyOf(toFrozen ctx t)
        | Schema.TypeRef.IndexedAccess(objTy, index) -> FTIndexedAccess(toFrozen ctx objTy, toFrozen ctx index)
        | Schema.TypeRef.Conditional(check, extends, whenTrue, whenFalse) ->
            FTConditional(toFrozen ctx check, toFrozen ctx extends, toFrozen ctx whenTrue, toFrozen ctx whenFalse)
        | Schema.TypeRef.Dynamic -> FTUnknown "any" // TODO: TyDynamic once it lands
        | Schema.TypeRef.Structural(hash, _) -> FTUnknown("structural:" + hash) // TODO: content-hash record

    let unitFrozen: FrozenType = FTConst("unit", EqArray.empty)

    /// .NET-tupled parameter encoding: 0 → unit, 1 → bare, N≥2 → tuple.
    let private paramsFrozen (ctx: TranslateCtx) (ps: Schema.Param list) : FrozenType =
        match ps with
        | [] -> unitFrozen
        | [ p ] -> toFrozen ctx p.Type
        | many -> FTTuple(EqArray.ofSeq (many |> List.map (fun p -> toFrozen ctx p.Type)))

    let signatureOf (ctx: TranslateCtx) (declArity: int) (sg: Schema.Signature) : ExternalSignature =
        // Per-method-typar bound (`<Key extends keyof Events>`), carried FAITHFULLY as a
        // `FrozenType` (`FTKeyOf(FTTypar(Declaring,0))`) so the front end can keyof-fold
        // it at the call site (the call-site literal-grounding rule). The schema OMITS
        // `TypeParamBounds` when every entry is `None`, so an unconstrained signature
        // yields the empty array (the churn-free default every non-TS producer already
        // uses) — never a `MethodArity`-long array of `ValueNone`, which would be
        // observationally identical but noisier.
        let bounds =
            if sg.TypeParamBounds |> List.exists Option.isSome then
                sg.TypeParamBounds
                |> List.map (
                    function
                    | Some b -> ValueSome(toFrozen ctx b)
                    | None -> ValueNone
                )
                |> Array.ofList
            else
                [||]

        {
            DeclaringArity = declArity
            MethodArity = sg.TypeParams
            Parameters = paramsFrozen ctx sg.Params
            Return = toFrozen ctx sg.Returns
            MethodTyparBounds = bounds
        }

    /// Intern each overload signature's parameter shape into its `argSig`, guarding
    /// the set for collisions: two overloads that collapse to the same argSig (same
    /// param count AND types) would mint the SAME `MemberKey`, so throw rather than let
    /// them silently coincide — the forcing function that fires exactly when a
    /// collapsed spelling has erased a real distinction (it points at the fixture whose
    /// thin extracted type wants sharpening). `label` names the member in the error.
    /// Each parameter renders through the SHARED `FrozenType` spelling grammar
    /// (`ExternalSymbols.argTypeName`, over the same `toFrozen` translation the
    /// `Signature.Parameters` template carries) — one renderer with the `.fsi`
    /// contract layer, so the two producers cannot drift on overload identity.
    let overloadArgSigs
        (ctx: TranslateCtx)
        (label: string)
        (mem: Schema.Member)
        : (string list * Schema.Signature) list =
        let built =
            mem.Signatures
            |> List.map (fun sg ->
                sg.Params
                |> List.map (fun p -> ExternalSymbols.argTypeName (toFrozen ctx p.Type)),
                sg
            )

        built
        |> List.countBy (fun (a, _) -> System.String.Join(",", a))
        |> List.tryFind (fun (_, n) -> n > 1)
        |> Option.iter (fun (k, _) ->
            failwithf "%s has duplicate overload argSig (%s); sharpen the extracted parameter types" label k
        )

        built
