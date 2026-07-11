namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest

/// THE single-source home→namespace mount + global-home classification for ref packs.
/// TWO axes, deliberately SPLIT (a node module needs one without the other):
///
///   • `mountFor home` — the VESPER-FACING namespace a home's exports mount under
///     (`""` = root, a real flat package). `es2015 → Js`; a node module
///     `node/<mod> → Node.<Mod>` (`node/fs → Node.Fs`). Ties three sites together:
///       1. the provider mounts a home's exports under it (`providerOfManifest`
///          flattens from the mount prefix);
///       2. the consumer ref-minting (`toFrozen`'s `nominal`) homes a `RefEntry`
///          under the same namespace, so a homed `Map` ref and the mounted es2015
///          `Map` share the qualified name `Js.Map`;
///       3. the resolution walk starts at the mount prefix.
///
///   • `isGlobalHome home` — whether the JS runtime provides the home's types
///     INTRINSICALLY, so they emit with NO `import` (`ExternalClassFlags.Global`,
///     bare-name `addRef`). ONLY the ambient ES-core lib (`es2015`). A node module
///     mounts under a namespace (axis 1) but is NOT global: `import fs from "fs"` is
///     REQUIRED, so it must still emit a real import.
///
/// The es2015 case sets BOTH (mount `Js` + global); node sets ONLY the mount. That
/// split is exactly why this is two functions, not one `Map` whose membership
/// conflated namespace-mount with import-suppression.
///
/// Design-table row: the ECMAScript CORE library flattens into a single `Js`
/// namespace — the `es2015`/`es2017`/… lib version suffix is TS's compile-TARGET
/// mechanism, not a semantic namespace, so every ES-core lib home maps to `Js`.
module TsGlobalHomes =

    /// The ECMAScript CORE library home — mounts under `Js` AND is import-free (a
    /// JS-runtime intrinsic). The one home for which `mountFor` and `isGlobalHome` both
    /// fire, so it reads from ONE token: an ES-core rename can't desync mount from
    /// is-global.
    [<Literal>]
    let private esCoreHome = "es2015"

    /// The `node/` home prefix — a `@types/node` per-module manifest is homed
    /// `<packageName>/<module>` (W1), e.g. `node/fs`; its exports mount under `Node.*`.
    [<Literal>]
    let private nodeHomePrefix = "node/"

    /// Upper-case the first character (`fs → Fs`, `child_process → Child_process`);
    /// the module segment is already a legal JS/namespace identifier.
    let private capitalize (s: string) : string =
        if s = "" then
            s
        else
            string (System.Char.ToUpperInvariant s.[0]) + s.Substring 1

    /// The Vesper-facing namespace `home`'s exports mount under (`""` = root).
    let mountFor (home: string) : string =
        if home = esCoreHome then
            "Js"
        elif home.StartsWith nodeHomePrefix then
            "Node." + capitalize (home.Substring nodeHomePrefix.Length)
        else
            ""

    /// Whether `home`'s types are JS-runtime intrinsics (bare name, NO import).
    let isGlobalHome (home: string) : bool = home = esCoreHome

/// Type translation for the TS-manifest provider: the manifest's `TypeRef` grammar
/// → the seam's `FrozenType` / `ExternalSignature`, plus the per-manifest
/// `TranslateCtx` (the type-identity table every walk threads). Consumed by
/// `TsManifestMembers` and `TsManifestProvider`.
module internal TsManifestTranslate =

    /// THE ONE mapping from the manifest's wire `Schema.ImportShape` to the seam's
    /// `ImportForm` (the JS backend's import-statement selector). Shared by the value
    /// stamp (`stampValueSymbol`) and the overloaded-free-function grouping type
    /// (`buildOverloadGroupingTypes`), so a free function and its overloaded sibling
    /// lower through the SAME classification. `CommonJsExport → CommonJs` and
    /// `Namespace → Namespace` are faithful (they were formerly collapsed to `Named`).
    let importFormOfShape (import: Schema.ImportShape) : ImportForm =
        match import with
        | Schema.ImportShape.Named -> ImportForm.Named
        | Schema.ImportShape.Default -> ImportForm.Default
        | Schema.ImportShape.CommonJsExport -> ImportForm.CommonJs
        | Schema.ImportShape.Namespace -> ImportForm.Namespace

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
    /// arity — matching what `TypeTranslate`/`Elaborate` form when resolving an annotation
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
            /// The manifest's foreign-reference table (`man.Refs`), keyed by the
            /// referenced type's BARE name. Consulted by `toFrozen`'s `nominal` helper
            /// AFTER the own-registry miss: a class/interface-kind entry mints a HOMED
            /// `FTClass` identity so member access resolves through the ordinary provider
            /// stack when the home manifest is stacked. IDENTITY ONLY — never the foreign
            /// shape (the ECMA-335 `TypeRef` analog).
            Refs: Map<string, Schema.RefEntry>
            /// The symbols' import path (for a flat single-file package, the package
            /// name); stamped into every minted key and `SymbolOrigin`.
            ModuleSpec: string
            /// The namespace a MOUNTED pack's exports are MOUNTED under (`Js` for an
            /// `es2015` home, `Node.Fs` for `node/fs`; `""` for a real flat package). A
            /// mounted pack registers its types under `<Mount>.<name>`, but its member
            /// signatures still spell an intra-pack sibling by its BARE name (`Map`'s
            /// ctor returns `Map`, not `Js.Map`), so `nominal`'s own-registry probe
            /// retries the BARE name prefixed by this mount (`Js.Map`). `""` makes the
            /// retry a no-op, so a real package is byte-identical. ONE source:
            /// `TsGlobalHomes.mountFor`.
            MountPrefix: string
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
    let buildCtx
        (moduleSpec: string)
        (mountPrefix: string)
        (refs: (string * Schema.RefEntry) list)
        (flatExports: (string * Schema.Export) list)
        : TranslateCtx =
        let types =
            flatExports
            |> List.choose (fun (nsPath, ex) ->
                match ex with
                | Schema.Export.Interface(name, tp, _, _, _) ->
                    let qn, key = mint moduleSpec nsPath name tp
                    Some(qn, { Key = key; IsInterface = true })
                | Schema.Export.Class(name, tp, _, _, _, _) ->
                    let qn, key = mint moduleSpec nsPath name tp
                    Some(qn, { Key = key; IsInterface = false })
                | _ -> None
            )
            |> Map.ofList

        {
            Types = types
            Refs = Map.ofList refs
            ModuleSpec = moduleSpec
            MountPrefix = mountPrefix
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

    // ─── Structural shape-hash ─────────────────────────────────────────────

    /// Canonical, ACYCLIC structural shape-hash — the field-ORDER-INVARIANT identity a
    /// `Structural` shape freezes to. The canonical STRING *is* the identity (no
    /// numeric/crypto digest — a deterministic string is easier to debug and equally
    /// discriminating): `FTUnknown` unifies by NAME equality (`SemanticInfo.fs`), so two
    /// field-order-permuted shapes render the SAME name and unify as opaque, without any
    /// member resolution yet.
    ///
    /// INVARIANTS that keep the hash acyclic and canonical:
    ///   • `Named` refs are LEAVES — hashed by name + hashed args, NEVER expanded. TS
    ///     recursion requires a name (`interface Node { children: Node[] }` is nominal),
    ///     so leaving named refs unexpanded bounds the recursion to the finite schema tree.
    ///   • `Structural` fields are SORTED by name before rendering — order-invariant.
    ///   • `Union` members are SORTED (an order-invariant multiset) — a TS union's source
    ///     order is diagnostic-only, immaterial to identity.
    /// Every case renders a per-case tag so no two shapes alias across constructors.
    let rec private shapeHash (t: Schema.TypeRef) : string =
        match t with
        | Schema.TypeRef.Named(name, []) -> "N:" + name
        | Schema.TypeRef.Named(name, args) -> "N:" + name + "<" + String.concat "," (List.map shapeHash args) + ">"
        | Schema.TypeRef.Typar i -> "T:" + string i
        | Schema.TypeRef.MethodTypar i -> "M:" + string i
        | Schema.TypeRef.Fun(args, ret) -> "Fn(" + String.concat "," (List.map shapeHash args) + ")->" + shapeHash ret
        | Schema.TypeRef.Tuple items -> "Tup(" + String.concat "," (List.map shapeHash items) + ")"
        | Schema.TypeRef.Union members -> "U(" + String.concat "|" (List.sort (List.map shapeHash members)) + ")"
        | Schema.TypeRef.Literal(Schema.LiteralValue.StringVal s) -> "Ls:" + s
        | Schema.TypeRef.Literal(Schema.LiteralValue.IntVal n) -> "Li:" + string n
        | Schema.TypeRef.KeyOf t -> "K(" + shapeHash t + ")"
        | Schema.TypeRef.IndexedAccess(objTy, index) -> "Ix(" + shapeHash objTy + "," + shapeHash index + ")"
        | Schema.TypeRef.Conditional(check, extends, whenTrue, whenFalse) ->
            "Cond("
            + shapeHash check
            + ","
            + shapeHash extends
            + ","
            + shapeHash whenTrue
            + ","
            + shapeHash whenFalse
            + ")"
        | Schema.TypeRef.Dynamic -> "Dyn"
        // The index-signature facet does not participate in the structural shape-hash
        // (identity is the field set); ignore it, leaving a facet-free shape hash-identical.
        | Schema.TypeRef.Structural(printed, fields, _) -> structuralHash printed fields

    /// The `Structural` arm of `shapeHash`, split out so `toFrozen` reaches it directly.
    /// FIELDLESS fallback: the extractor emits empty `fields` for a non-object structural
    /// form (function&, branded), which carries NO usable shape — so DON'T collapse all
    /// such forms to one `{}` identity; fall back to the tsc-`printed` string (preserving
    /// the fieldless behaviour). Only a genuine object shape gets the order-invariant
    /// `{name:hash;…}` canonicalisation.
    and structuralHash (printed: string) (fields: (string * Schema.TypeRef) list) : string =
        match fields with
        | [] -> "printed:" + printed
        | _ ->
            fields
            |> List.sortBy fst
            |> List.map (fun (name, ft) -> name + ":" + shapeHash ft)
            |> String.concat ";"
            |> fun body -> "{" + body + "}"

    /// The reserved synthetic HOME/namespace an anonymous object shape's erasing nominal
    /// is registered and homed under. A real TS namespace-path segment is a JS identifier
    /// and a real package/module specifier is an import path — neither can contain `@` or
    /// the `{ : ; }` a shape-hash carries, so a structural type's qualified name
    /// (`@struct.{x:number;y:number}`) cannot collide with any real export's qualified name
    /// by construction. The home is never imported: the shape has no ctor, and its Property
    /// fields lower to native `receiver.field` reads (`MemberLowering.AttachedNative`), so
    /// nothing is ever emitted for the type itself.
    let structuralHome = "@struct"

    /// Mint the erasing-nominal identity of an anonymous object shape from its CANONICAL
    /// shape-hash (arity 0 — a structural shape is never generic). Used by BOTH the provider
    /// registration and `toFrozen`'s `Structural` arm, so the frozen `FTClass` identity and
    /// the `types`-map key AGREE by construction (the same map-key/`TypeKey` alignment `mint`
    /// gives a declared type: `SymbolKeyOps.qualifiedName` of the returned key equals the
    /// returned qualified string). The `hash` passed in MUST be `structuralHash printed
    /// fields` — the interning string — so a field-order-permuted shape and its twin resolve
    /// to ONE type. IDENTITY is cross-manifest (any manifest freezes the same shape to the
    /// same key), but member REGISTRATION is per-manifest: each provider pre-scans only its
    /// OWN exports. A structural value flowing across manifests and accessed only where a
    /// DIFFERENT manifest registered the members is a known gap, not exercised by current
    /// fixtures — cross-manifest structural member resolution is deliberately not built here.
    let structuralKey (hash: string) : string * SymbolKey =
        mint structuralHome structuralHome hash 0

    /// Every anonymous OBJECT shape (`Structural` with fields) reachable from a `TypeRef`,
    /// as `(printed, fields)` pairs — RECURSING into each shape's field types so a nested
    /// `{pt:{x;y}}` yields BOTH levels. Case coverage mirrors `shapeHash`. A `Named` ref is
    /// a LEAF for hashing, but its type ARGS are still descended (a shape inside
    /// `Array<{x}>` is a real value whose members get accessed). A FIELDLESS structural
    /// carries no members, so it is skipped — it stays an opaque `FTUnknown`.
    let rec structuralShapesIn (t: Schema.TypeRef) : (string * (string * Schema.TypeRef) list) list =
        match t with
        | Schema.TypeRef.Named(_, args) -> args |> List.collect structuralShapesIn
        | Schema.TypeRef.Typar _
        | Schema.TypeRef.MethodTypar _ -> []
        | Schema.TypeRef.Fun(args, ret) -> (args |> List.collect structuralShapesIn) @ structuralShapesIn ret
        | Schema.TypeRef.Tuple items -> items |> List.collect structuralShapesIn
        | Schema.TypeRef.Union members -> members |> List.collect structuralShapesIn
        | Schema.TypeRef.Literal _ -> []
        | Schema.TypeRef.KeyOf t -> structuralShapesIn t
        | Schema.TypeRef.IndexedAccess(objTy, index) -> structuralShapesIn objTy @ structuralShapesIn index
        | Schema.TypeRef.Conditional(check, extends, whenTrue, whenFalse) ->
            [ check; extends; whenTrue; whenFalse ] |> List.collect structuralShapesIn
        | Schema.TypeRef.Dynamic -> []
        | Schema.TypeRef.Structural(_, [], _) -> []
        | Schema.TypeRef.Structural(printed, fields, _) ->
            (printed, fields)
            :: (fields |> List.collect (fun (_, ft) -> structuralShapesIn ft))

    /// Every anonymous OBJECT shape carrying a non-empty TS index signature, as
    /// `(shapeHash, index-pairs)` — the structural analogue of the named `Interface`/`Class`
    /// `index` facet. Keyed by the SAME `structuralHash printed fields` its frozen
    /// `FTClass(structuralKey …)` carries, so a use site's resolved nominal and this index
    /// entry AGREE. Covers BOTH a field-bearing shape AND a FIELDLESS one (a bare `{ [k: K]:
    /// V }` / `Record<K,V>`): the latter now freezes to a nominal too (its index IS its whole
    /// content), so its index must register for `x.[k]` to reach `TryLookupIndexSignature`.
    /// Only a TRULY EMPTY shape (no fields AND no index) contributes nothing — it stays an
    /// opaque `FTUnknown`. Recurses into field types (mirroring `structuralShapesIn`) so a
    /// nested shape's index registers too. Case coverage mirrors `shapeHash`.
    let rec structuralIndexSigsIn (t: Schema.TypeRef) : (string * (Schema.TypeRef * Schema.TypeRef) list) list =
        match t with
        | Schema.TypeRef.Named(_, args) -> args |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Typar _
        | Schema.TypeRef.MethodTypar _ -> []
        | Schema.TypeRef.Fun(args, ret) -> (args |> List.collect structuralIndexSigsIn) @ structuralIndexSigsIn ret
        | Schema.TypeRef.Tuple items -> items |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Union members -> members |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Literal _ -> []
        | Schema.TypeRef.KeyOf t -> structuralIndexSigsIn t
        | Schema.TypeRef.IndexedAccess(objTy, index) -> structuralIndexSigsIn objTy @ structuralIndexSigsIn index
        | Schema.TypeRef.Conditional(check, extends, whenTrue, whenFalse) ->
            [ check; extends; whenTrue; whenFalse ] |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Dynamic -> []
        // A TRULY EMPTY shape (no fields AND no index) contributes nothing. A FIELDLESS shape
        // with a NON-EMPTY index (a bare `{ [k: K]: V }`) DOES register its index — keyed by
        // `structuralHash printed []`, the SAME identity its frozen `FTClass(structuralKey …)`
        // carries — so `x.[k]` on it reaches `TryLookupIndexSignature`. Aligns with the
        // field-bearing arm below.
        | Schema.TypeRef.Structural(_, [], []) -> []
        | Schema.TypeRef.Structural(printed, fields, index) ->
            let here =
                match index with
                | [] -> []
                | _ -> [ structuralHash printed fields, index ]

            here @ (fields |> List.collect (fun (_, ft) -> structuralIndexSigsIn ft))

    /// Every `TypeRef` an export directly mentions (member/signature/heritage types), for
    /// the structural pre-scan. `Namespace` produces none — `flatten` unwraps it to leaf
    /// exports before this is reached. `Enum` carries only literal values, no `TypeRef`.
    let exportTypeRefs (ex: Schema.Export) : Schema.TypeRef list =
        let sigRefs (sg: Schema.Signature) : Schema.TypeRef list =
            [
                for p in sg.Params -> p.Type
                yield sg.Returns
                for b in sg.TypeParamBounds do
                    match b with
                    | Some t -> yield t
                    | None -> ()
            ]

        let memberRefs (mem: Schema.Member) : Schema.TypeRef list =
            [
                match mem.Type with
                | Some t -> yield t
                | None -> ()
                for sg in mem.Signatures do
                    yield! sigRefs sg
            ]

        match ex with
        | Schema.Export.Variable(_, ty, _, _) -> [ ty ]
        | Schema.Export.Function(_, sigs, _) -> sigs |> List.collect sigRefs
        | Schema.Export.Interface(_, _, members, heritage, _) -> heritage @ (members |> List.collect memberRefs)
        | Schema.Export.Class(_, _, members, heritage, _, _) -> heritage @ (members |> List.collect memberRefs)
        | Schema.Export.TypeAlias(_, _, target) -> [ target ]
        | Schema.Export.Enum _ -> []
        | Schema.Export.Namespace _ -> []

    // ─── TypeRef → FrozenType (member signature templates) ─────────────────

    let rec toFrozen (ctx: TranslateCtx) (t: Schema.TypeRef) : FrozenType =
        let nominal name (args: FrozenType[]) =
            // A bare name that misses BOTH the own registry and the foreign refs table is
            // either a Vesper intrinsic PRIMITIVE spelled by its canonical name — a TS
            // `.d.ts` via `TypeMap` (`boolean`→`bool`, `void`→`unit`, `undefined`), or a
            // Vesper-authored manifest by the numeric / reference-primitive names directly
            // (`float`, `string`) — or a genuinely-external / token type carried
            // origin-less. A primitive mints its canonical `Vesper` key so a manifest param
            // (a `.d.ts` `string`, a `float`) unifies with the same intrinsic the front end
            // mints for a literal arg. This is a syntactic primitive-name recogniser (the
            // shared `RuntimeNames.numericTypeNames` + `referencePrimitiveNames` cores plus
            // the two contract intrinsics a manifest may spell but that live in neither
            // core), NOT a provider/contract lookup — the manifest translator has no
            // provider in hand. `number` (the WIDENING token `NumberCovariance` resolves),
            // `null` (the literal union member), and every real external name stay
            // origin-less BY DESIGN.
            let intrinsicOrOpaque (name: string) : FrozenType =
                let isVesperPrimitive =
                    RuntimeNames.numericTypeNames.Contains name
                    || RuntimeNames.referencePrimitiveNames.Contains name
                    // Use-site extras beyond the shared cores: `undefined` (JS-only) and
                    // `bigint` — both are contract intrinsics spellable by a manifest
                    // param but absent from the numeric/reference cores. `null` is NOT
                    // here: it is a reserved keyword with no `Vesper` namespace, so a
                    // manifest `null` member mints the BARE `nullKey` (`opaqueKey "null"`)
                    // via the opaque branch below — the SAME identity the front end and
                    // extractor mint, so a `T | null` union unifies across all three.
                    || name = "undefined"
                    || name = "bigint"

                if isVesperPrimitive then
                    FTConst(RuntimeNames.primitiveKey name, EqArray.ofSeq args)
                else
                    FTConst(RuntimeNames.opaqueKey name, EqArray.ofSeq args)

            // Suffix the manifest's bare spelling by the applied arg count before
            // probing the table (THE LAW, see `mint`).
            let suffixed = SymbolKeyOps.arityName name args.Length

            // A GLOBAL pack registers its types under its mount namespace (`Js.Map`) but
            // spells an intra-pack sibling by its BARE name (`Map`) — retry the
            // mount-qualified name on the direct miss. `MountPrefix = ""` (a real
            // package) makes `qualify` a no-op, so this is the SAME lookup twice.
            let owned =
                match ctx.Resolve suffixed with
                | Some key -> Some key
                | None -> ctx.Resolve(qualify ctx.MountPrefix suffixed)

            match owned with
            | Some key -> FTClass(key, EqArray.ofSeq args)
            | None ->
                // Own-registry miss: consult the FOREIGN refs table (keyed by the BARE
                // name — a `RefEntry` carries its own declared `Arity`). A
                // class/interface-kind ref mints a HOMED `FTClass` IDENTITY — the
                // ECMA-335 `TypeRef` analog — whose `qualifiedName` equals what the home
                // manifest's provider registers its own type under (`mint` at nsPath "":
                // just the arity-suffixed simple name), so member access resolves through
                // the ordinary provider stack once the home is stacked (`resolveFieldStep`
                // → `TryLookupMember`). Alias/Enum-kind refs stay a carried `FTConst` for
                // v1: a homed alias must resolve through its home manifest's `Abbrev`
                // (deferred), and there is no home-independent identity to mint. A name
                // that misses BOTH the own registry and the refs table stays `FTConst` (a
                // true primitive / genuinely-unknown name).
                match Map.tryFind name ctx.Refs with
                | Some entry ->
                    match entry.Kind with
                    | Schema.RefKind.Class
                    | Schema.RefKind.Interface ->
                        // A MOUNTED home mints its ref under the Vesper-facing namespace
                        // (`es2015` → `Js`, `node/fs → Node.Fs`) so this homed identity's
                        // `qualifiedName` (`Js.Map\`2`) equals what the MOUNTED home
                        // provider registers its own type under (`providerOfManifest`
                        // starts its flatten at the same mount prefix). A real flat-package
                        // home stays namespace "" (normal in-package spelling). ONE source:
                        // `TsGlobalHomes.mountFor`.
                        let ns = TsGlobalHomes.mountFor entry.Home

                        let key =
                            SymbolKey.TypeKey(Some entry.Home, ns, SymbolKeyOps.arityName name entry.Arity)

                        FTClass(key, EqArray.ofSeq args)
                    | Schema.RefKind.Alias
                    | Schema.RefKind.Enum -> intrinsicOrOpaque name
                | None -> intrinsicOrOpaque name

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
            FTConditional
                {
                    Check = toFrozen ctx check
                    Extends = toFrozen ctx extends
                    WhenTrue = toFrozen ctx whenTrue
                    WhenFalse = toFrozen ctx whenFalse
                }
        // TS `any` → the opaque `dynamic` JS intrinsic (no special unifier behaviour;
        // its only capability is the `?` operator). It is `FTConst "dynamic"` everywhere.
        | Schema.TypeRef.Dynamic -> FTConst(RuntimeNames.dynamicKey, EqArray.empty)
        // An anonymous OBJECT shape (`fields` non-empty) freezes to a hash-keyed ERASING
        // nominal: an `FTClass` homed under the reserved synthetic namespace, whose members
        // the provider registers (one Property per field) so `.x` resolves and lowers to a
        // native `receiver.x` read — while NOTHING is emitted for the type (no decl, no
        // import, no ctor). Identity is the canonical, field-ORDER-INVARIANT shape-hash
        // (`{x;y}` ≡ `{y;x}`), so two permuted shapes intern to the SAME key and unify. The
        // FIELDLESS shape carrying a NON-EMPTY index signature (a bare `{ [k: K]: V }` /
        // `Record<K,V>`) is NOT opaque: its index IS its whole content, so it too freezes to
        // the nominal `FTClass(structuralKey)` — the SAME identity a field-bearing shape
        // gets — and its index is reached via `TryLookupIndexSignature` keyed by that qn.
        // Only a TRULY EMPTY shape (no fields AND no index) has nothing to resolve, so it
        // stays an OPAQUE `FTUnknown` keyed by the tsc-`printed` fallback (a
        // `function&`/branded form — see `structuralHash`).
        // The index-signature facet is threaded through but not consumed by the frozen
        // nominal: an index-sig receiver resolves its element type at the lookup site
        // (`inferIndexedLookup` → `GetIndex`/`SetIndex`), not through this shape's members.
        | Schema.TypeRef.Structural(printed, fields, index) ->
            match fields, index with
            | [], [] -> FTUnknown("structural:" + structuralHash printed fields)
            | _ -> FTClass(structuralKey (structuralHash printed fields) |> snd, EqArray.empty)

    let unitFrozen: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

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

    /// Intern each overload signature's parameter shape into its `argSig`, KEEPING THE
    /// FIRST of any that intern to the same `argSig` (same param count AND spelled
    /// types). Each parameter renders through the SHARED `FrozenType` spelling grammar
    /// (`ExternalSymbols.argTypeName`, over the same `toFrozen` translation the
    /// `Signature.Parameters` template carries) — one renderer with the `.fsi`
    /// contract layer, so the two producers cannot drift on overload identity.
    ///
    /// DEGRADE-AND-DEDUP, never abort: two overloads that erase to one `argSig` after
    /// numeric/structural degradation (node's `number`-family and config-object
    /// overload storms collapse pervasively — `read(x: number)` beside a `number`
    /// literal, two options-object overloads that widen to the same opaque
    /// `Structural`) key the SAME dispatch slot, so the first is authoritative and any
    /// later twin is UNREACHABLE. Dedup keeps the first; the rest are dropped rather
    /// than throwing (the former `ErasedDistinction` abort was untenable at node scale,
    /// where such collisions are the norm, not an extraction bug). Both the method and
    /// constructor sites want identical handling — a ctor selects on ARGUMENTS ALONE
    /// (never the return type), a method's dispatch keys on the argSig — so there is one
    /// behaviour, not a per-caller collision policy.
    let overloadArgSigs (ctx: TranslateCtx) (mem: Schema.Member) : (string list * Schema.Signature) list =
        mem.Signatures
        |> List.map (fun sg ->
            sg.Params
            |> List.map (fun p -> ExternalSymbols.argTypeName (toFrozen ctx p.Type)),
            sg
        )
        |> List.distinctBy fst
