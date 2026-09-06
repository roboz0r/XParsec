namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open Vesper.Ts.Manifest

/// Mount and global-home are TWO axes: a node module MOUNTS under a namespace (`node/fs →
/// Node.Fs`) yet still needs a real `import`. Only the ES-core lib is both.
module TsGlobalHomes =

    /// The extractor flattens the whole `lib.es*` closure into this ONE home, so the
    /// version suffix is TS's compile-TARGET mechanism, not a second home to match.
    [<Literal>]
    let private esCoreHome = "es2015"

    /// A `@types/node` per-module manifest is homed `node/<module>`; its exports mount
    /// under `Node.*`.
    [<Literal>]
    let private nodeHomePrefix = "node/"

    /// `fs → Fs`, `child_process → Child_process` — the first character only.
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

/// The manifest's `TypeRef` grammar → the seam's `FrozenType` / `ExternalSignature`, plus
/// the per-manifest type-identity table every walk threads.
module internal TsManifestTranslate =

    let importFormOfShape (import: Schema.ImportShape) : ImportForm =
        match import with
        | Schema.ImportShape.Named -> ImportForm.Named
        | Schema.ImportShape.Default -> ImportForm.Default
        | Schema.ImportShape.CommonJsExport -> ImportForm.CommonJs
        | Schema.ImportShape.Namespace -> ImportForm.Namespace

    /// A symbol is registered and found under the DOTTED QUALIFIED name (`NS.Inner.Baz`) that
    /// resolution forms from a use site. A top-level export keeps its bare name.
    let qualify (nsPath: string) (name: string) : string =
        if nsPath = "" then name else nsPath + "." + name

    /// A `Namespace` container produces NO symbol. Only its members do, each paired with
    /// the dotted path it nests under, so nested and top-level share the same flat maps.
    let rec flatten (nsPath: string) (exports: Schema.Export list) : (string * Schema.Export) list =
        exports
        |> List.collect (fun ex ->
            match ex with
            | Schema.Export.Namespace(nsName, nested) -> flatten (qualify nsPath nsName) nested
            | other -> [ nsPath, other ]
        )

    /// A nominal type's two spellings, minted together: `QualifiedName` is the arity-suffixed
    /// dotted name (`` NS.Emitter`1 ``) of `Key`, which carries the arity as an INT instead.
    type MintedType = { QualifiedName: string; Key: TypeKey }

    /// `(name, arity)` pairs are DISTINCT nominal types. The home module is NOT part of the
    /// identity, so a cross-package ref mints the same one.
    let mint (nsPath: string) (name: string) (arity: int) : MintedType =
        {
            QualifiedName = qualify nsPath (SymbolKeyOps.arityName name arity)
            Key = SymbolKeyOps.typeKeyOfArity nsPath name arity
        }

    /// `IsInterface` picks a heritage entry's slot: interface list vs single base class.
    type TypeIdentity =
        {
            Minted: MintedType
            IsInterface: bool
        }

    type TranslateCtx =
        {
            /// The contract-declared intrinsic axis. A manifest-spelled canon name (`string`,
            /// `float`, `undefined`) mints its `Vesper` identity only if declared here.
            Intrinsics: IntrinsicTypeMap
            /// Names declared as an `Interface`/`Class`, keyed by their minted qualified name.
            /// Built over ALL flat exports first, so a signature referencing a LATER type resolves.
            Types: Map<string, TypeIdentity>
            /// The manifest's foreign references, keyed by the referenced type's BARE name.
            /// IDENTITY ONLY, never the foreign shape (the ECMA-335 `TypeRef` analog).
            Refs: Map<string, Schema.RefEntry>
            /// The symbols' import path; stamped into every minted key and `SymbolOrigin`.
            ModuleSpec: string
            /// The namespace a MOUNTED pack's exports register under (`Js` for an `es2015`
            /// home, `Node.Fs` for `node/fs`); `""` for a real flat package.
            MountPrefix: string
            /// The scope a `TypeRef.Typar` resolves under: the declaration being translated.
            /// `ValueNone` outside a declaration, where a typar is a corrupt manifest.
            TyparScope: TyparScope voption
            /// The scope a `TypeRef.MethodTypar` resolves under: the member being translated.
            MethodTyparScope: TyparScope voption
        }

        /// The context for translating the structure of a declaration with typar scope
        /// `scope`: its members, heritage and body.
        member ctx.InScope(scope: TyparScope) : TranslateCtx =
            { ctx with
                TyparScope = ValueSome scope
                MethodTyparScope = ValueNone
            }

        /// The context for translating a member of `declKey`: a `TypeRef.MethodTypar`
        /// resolves under the member's scope.
        member ctx.InMember(declKey: TypeKey) : TranslateCtx =
            { ctx with
                MethodTyparScope = ValueSome(TyparScope.Member declKey)
            }

        /// The gate that turns a nominal `Named` into `FTClass`: only a declared class or
        /// interface hits. A primitive, a cross-package name and a `TypeAlias` stay `FTConst`.
        member ctx.Resolve(name: string) : TypeKey option =
            ctx.Types |> Map.tryFind name |> Option.map (fun id -> id.Minted.Key)

        /// `None` for a name not declared in this package (cross-package / unknown).
        member ctx.TryFindType(name: string) : TypeIdentity option = Map.tryFind name ctx.Types

    let buildCtx
        (intrinsics: IntrinsicTypeMap)
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
                    let minted = mint nsPath name tp
                    Some(minted.QualifiedName, { Minted = minted; IsInterface = true })
                | Schema.Export.Class(name, tp, _, _, _, _) ->
                    let minted = mint nsPath name tp
                    Some(minted.QualifiedName, { Minted = minted; IsInterface = false })
                | _ -> None
            )
            |> Map.ofList

        {
            Intrinsics = intrinsics
            Types = types
            Refs = Map.ofList refs
            ModuleSpec = moduleSpec
            MountPrefix = mountPrefix
            TyparScope = ValueNone
            MethodTyparScope = ValueNone
        }

    /// Total for the exports the ctx was built from; a miss is a bug, not a data condition.
    let declaredIdentity (ctx: TranslateCtx) (nsPath: string) (name: string) (arity: int) : MintedType =
        let qn = (mint nsPath name arity).QualifiedName

        match Map.tryFind qn ctx.Types with
        | Some id -> id.Minted
        | None -> failwithf "declared type '%s' is missing from the identity table" qn

    /// A key carries no home, so codegen reads the import path here. `Home` is the MODULE
    /// SPECIFIER, not the package name; `Namespace` is the path within the module.
    let originFor (ctx: TranslateCtx) (nsPath: string) : SymbolOrigin =
        {
            Home = SymbolHome.InAssembly(AssemblyName ctx.ModuleSpec)
            Namespace = SymbolKeyOps.namespaceKey nsPath
        }

    // ─── Structural shape-hash ─────────────────────────────────────────────

    /// The interning string an anonymous object shape's identity IS. Opaque, because only
    /// `structuralHash` sorts the fields that make a permuted twin hash the same.
    type StructuralHash = private | StructuralHash of string

    let private hashText (StructuralHash s) = s

    /// The canonical string IS the identity, and every case carries a tag so no two shapes
    /// alias. `Named` refs are LEAVES: never expanded, which bounds the recursion, since TS
    /// recursion requires a name. Fields and union disjuncts are SORTED: order-invariant.
    let rec private shapeHash (t: Schema.TypeRef) : string =
        match t with
        | Schema.TypeRef.Named(name, []) -> "N:" + name
        | Schema.TypeRef.Named(name, args) -> "N:" + name + "<" + String.concat "," (List.map shapeHash args) + ">"
        | Schema.TypeRef.Typar i -> "T:" + string i
        | Schema.TypeRef.MethodTypar i -> "M:" + string i
        | Schema.TypeRef.Fun(args, ret) -> "Fn(" + String.concat "," (List.map shapeHash args) + ")->" + shapeHash ret
        | Schema.TypeRef.Tuple items -> "Tup(" + String.concat "," (List.map shapeHash items) + ")"
        | Schema.TypeRef.Union disjuncts -> "U(" + String.concat "|" (List.sort (List.map shapeHash disjuncts)) + ")"
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
        // Identity is the field set: the index facet does not participate.
        | Schema.TypeRef.Structural(printed, fields, _) -> hashText (structuralHash printed fields)

    /// A FIELDLESS structural form (function&, branded) carries no usable shape, so it falls
    /// back to the tsc-`printed` string rather than collapsing them all to one `{}` identity.
    and structuralHash (printed: string) (fields: (string * Schema.TypeRef) list) : StructuralHash =
        match fields with
        | [] -> StructuralHash("printed:" + printed)
        | _ ->
            fields
            |> List.sortBy fst
            |> List.map (fun (name, ft) -> name + ":" + shapeHash ft)
            |> String.concat ";"
            |> fun body -> StructuralHash("{" + body + "}")

    /// A namespace segment is a JS identifier and a module specifier is an import path, so
    /// neither can spell the `@` or `{ : ; }` of `@struct.{x:number;y:number}`.
    let structuralHome = "@struct"

    /// The erasing-nominal identity of an anonymous object shape. Identity is
    /// cross-manifest; member REGISTRATION is per-manifest.
    let structuralKey (hash: StructuralHash) : MintedType = mint structuralHome (hashText hash) 0

    /// Every anonymous OBJECT shape reachable from a `TypeRef`, nested ones included. A `Named`
    /// ref hashes by name, but its ARGS are descended: a shape in `Array<{x}>` is a value.
    let rec structuralShapesIn (t: Schema.TypeRef) : (string * (string * Schema.TypeRef) list) list =
        match t with
        | Schema.TypeRef.Named(_, args) -> args |> List.collect structuralShapesIn
        | Schema.TypeRef.Typar _
        | Schema.TypeRef.MethodTypar _ -> []
        | Schema.TypeRef.Fun(args, ret) -> (args |> List.collect structuralShapesIn) @ structuralShapesIn ret
        | Schema.TypeRef.Tuple items -> items |> List.collect structuralShapesIn
        | Schema.TypeRef.Union disjuncts -> disjuncts |> List.collect structuralShapesIn
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

    /// Every anonymous shape carrying a non-empty TS index signature, keyed by the SAME hash its
    /// frozen nominal carries. A FIELDLESS one counts: a bare `{ [k: K]: V }` IS its index.
    let rec structuralIndexSigsIn (t: Schema.TypeRef) : (StructuralHash * (Schema.TypeRef * Schema.TypeRef) list) list =
        match t with
        | Schema.TypeRef.Named(_, args) -> args |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Typar _
        | Schema.TypeRef.MethodTypar _ -> []
        | Schema.TypeRef.Fun(args, ret) -> (args |> List.collect structuralIndexSigsIn) @ structuralIndexSigsIn ret
        | Schema.TypeRef.Tuple items -> items |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Union disjuncts -> disjuncts |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Literal _ -> []
        | Schema.TypeRef.KeyOf t -> structuralIndexSigsIn t
        | Schema.TypeRef.IndexedAccess(objTy, index) -> structuralIndexSigsIn objTy @ structuralIndexSigsIn index
        | Schema.TypeRef.Conditional(check, extends, whenTrue, whenFalse) ->
            [ check; extends; whenTrue; whenFalse ] |> List.collect structuralIndexSigsIn
        | Schema.TypeRef.Dynamic -> []
        | Schema.TypeRef.Structural(_, [], []) -> []
        | Schema.TypeRef.Structural(printed, fields, index) ->
            let here =
                match index with
                | [] -> []
                | _ -> [ structuralHash printed fields, index ]

            here @ (fields |> List.collect (fun (_, ft) -> structuralIndexSigsIn ft))

    /// Every `TypeRef` an export directly mentions, for the structural pre-scan. A namespace
    /// is flattened to its member exports before this is reached; an enum carries only literals.
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

    /// A typar reference outside any declaration that could bind it is a corrupt manifest.
    let private scopeOf (what: string) (scope: TyparScope voption) : TyparScope =
        match scope with
        | ValueSome s -> s
        | ValueNone -> failwithf "manifest %s type parameter referenced outside a declaration" what

    let rec toFrozen (ctx: TranslateCtx) (t: Schema.TypeRef) : FrozenType =
        let nominal name (args: FrozenType[]) =
            // An axis-declared canon, `Unsupported` included, mints its `Vesper` key, so a
            // manifest `string`/`float` unifies with the front end's intrinsic. The lookup is
            // by canon, not platform type id, so `number` keeps its own identity.
            let intrinsicOrOpaque (name: string) : FrozenType =
                let canon = RuntimeNames.primitiveKey name

                match IntrinsicTypeMap.tryPlatform canon ctx.Intrinsics with
                | ValueSome _ -> FTConst(canon, EqArray.ofSeq args)
                | ValueNone -> FTConst(RuntimeNames.opaqueKey name, EqArray.ofSeq args)

            // TS has no partial application and no arity overloading, so the APPLIED arg
            // count is the declared arity of an in-package reference.
            let suffixed = SymbolKeyOps.arityName name args.Length

            // A mounted pack registers under its mount (`Js.Map`) but spells an intra-pack
            // sibling BARE (`Map`), so the miss retries mount-qualified.
            let owned =
                match ctx.Resolve suffixed with
                | Some key -> Some key
                | None -> ctx.Resolve(qualify ctx.MountPrefix suffixed)

            match owned with
            | Some key -> FTClass(key, EqArray.ofSeq args)
            | None ->
                // Own-registry miss: the FOREIGN refs table, keyed by the BARE name, mints
                // IDENTITY ONLY, the key the home manifest registers, so member access
                // resolves once that home is stacked. Alias/Enum refs stay `FTConst`.
                match Map.tryFind name ctx.Refs with
                | Some entry ->
                    match entry.Kind with
                    | Schema.RefKind.Class
                    | Schema.RefKind.Interface ->
                        // A mounted home mints its ref under the Vesper-facing namespace, so
                        // `Js.Map` here is what that home's provider registers under.
                        let ns = TsGlobalHomes.mountFor entry.Home

                        let key = (mint ns name entry.TyparArity).Key

                        FTClass(key, EqArray.ofSeq args)
                    | Schema.RefKind.Alias
                    | Schema.RefKind.Enum -> intrinsicOrOpaque name
                | None -> intrinsicOrOpaque name

        match t with
        | Schema.TypeRef.Named(name, []) -> nominal name [||]
        | Schema.TypeRef.Named(name, args) -> nominal name (List.map (toFrozen ctx) args |> Array.ofList)
        | Schema.TypeRef.Typar i -> FTTypar(scopeOf "type" ctx.TyparScope, i)
        | Schema.TypeRef.MethodTypar i -> FTTypar(scopeOf "method" ctx.MethodTyparScope, i)
        | Schema.TypeRef.Fun(args, ret) ->
            List.foldBack (fun a acc -> FTFun(toFrozen ctx a, acc)) args (toFrozen ctx ret)
        | Schema.TypeRef.Tuple items -> FTTuple(EqArray.ofSeq (List.map (toFrozen ctx) items))
        // Flatten/dedupe/collapse per TS's union rules: a singleton `("a")` becomes a literal.
        | Schema.TypeRef.Union disjuncts -> FrozenType.MkUnion(List.map (toFrozen ctx) disjuncts)
        // A TS literal TYPE → `FTLiteral` (structural, external-vocabulary only).
        | Schema.TypeRef.Literal(Schema.LiteralValue.StringVal s) -> FTLiteral(LiteralConst.String s)
        | Schema.TypeRef.Literal(Schema.LiteralValue.IntVal n) -> FTLiteral(LiteralConst.Int n)
        // INERT carriers: rehydrated with their children and threaded through every walk,
        // but never evaluated, because the front end owns the ground fold.
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
        // TS `any` → the opaque `dynamic` intrinsic; its only capability is the `?` operator.
        | Schema.TypeRef.Dynamic -> FTConst(RuntimeNames.dynamicKey, EqArray.empty)
        // An anonymous shape freezes to a hash-keyed ERASING nominal: its members resolve and
        // lower to native `objArg.x` reads while NOTHING is emitted for the type. A bare
        // With no field and no index (`{ [k: K]: V }` counts, its index is its content) there
        // are no members to resolve, so the same identity stands alone as an opaque const.
        | Schema.TypeRef.Structural(printed, fields, index) ->
            let key = (structuralKey (structuralHash printed fields)).Key

            match fields, index with
            | [], [] -> FTConst(key, EqArray.empty)
            | _ -> FTClass(key, EqArray.empty)

    let unitFrozen: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

    /// .NET-tupled parameter encoding: 0 → unit, 1 → bare, N≥2 → tuple.
    let private paramsFrozen (ctx: TranslateCtx) (ps: Schema.Param list) : FrozenType =
        match ps with
        | [] -> unitFrozen
        | [ p ] -> toFrozen ctx p.Type
        | many -> FTTuple(EqArray.ofSeq (many |> List.map (fun p -> toFrozen ctx p.Type)))

    let signatureOf (ctx: TranslateCtx) (declTyparArity: int) (sg: Schema.Signature) : ExternalSignature =
        // A bound (`<Key extends keyof Events>`) is carried faithfully so the front end can
        // keyof-fold it at the call site.
        let methodTypars =
            sg.TypeParamBounds
            |> List.map (
                function
                | Some b -> ValueSome(toFrozen ctx b)
                | None -> ValueNone
            )
            |> EqArray.ofList

        // A TS signature has ONE parameter list, so one argument group.
        {
            DeclaringTyparArity = declTyparArity
            MethodTypars = methodTypars
            ArgGroups = EqArray.singleton (paramsFrozen ctx sg.Params)
            Return = toFrozen ctx sg.Returns
        }

    /// Each overload's parameter shapes as its `argSig`, KEEPING THE FIRST of any that intern
    /// equal. Numeric/structural degradation collapses node's overload storms onto one
    /// dispatch slot pervasively, so a later twin is dropped as unreachable, not an error.
    let overloadArgSigs (ctx: TranslateCtx) (mem: Schema.Member) : (FrozenType list * Schema.Signature) list =
        mem.Signatures
        |> List.map (fun sg -> sg.Params |> List.map (fun p -> toFrozen ctx p.Type), sg)
        |> List.distinctBy fst
