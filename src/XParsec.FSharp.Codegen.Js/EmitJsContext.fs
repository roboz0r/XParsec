namespace XParsec.FSharp.Codegen.Js

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers
open JsMapSources
open EmitJsTypes

/// The `buildExpr`-free foundation the JS emitters share: the ambient `WalkCtx`, the
/// name/type helpers, and `compileMatchPattern`. Nothing here enters that recursion group.
module EmitJsContext =

    /// What a node's anchor resolves through on the way to a source-map position. Both fields
    /// or neither: an anchor is an INDEX, meaningless without the `Lexed` that numbered it.
    /// The pair is the COMPILING file's, at source index 0; `Retained` is every declaring file.
    type Resolution =
        {
            Lexed: Lexed
            Lines: LineIndex
            Retained: LexedFiles
        }

    /// `ValueNone` disables maps: no source text was supplied for an anchor to resolve against.
    type Resolver = Resolution voption

    /// A nominal type's emitted JS shape, whether the file declares it or imports it.
    type LocalThenExternal<'Info> =
        {
            /// The types emitted in THIS file, collected before the walk starts.
            Local: Dictionary<TypeKey, 'Info>
            External: Dictionary<TypeKey, 'Info>
            Resolve: TypeKey -> 'Info voption
        }

    module LocalThenExternal =
        let create (local: Dictionary<TypeKey, 'Info>) (resolve: TypeKey -> 'Info voption) : LocalThenExternal<'Info> =
            {
                Local = local
                External = Dictionary()
                Resolve = resolve
            }

        /// Local first, then the provider, whose answer is cached.
        let tryFind (table: LocalThenExternal<'Info>) (key: TypeKey) : 'Info voption =
            match table.Local.TryGetValue key with
            | true, info -> ValueSome info
            | _ ->
                match table.External.TryGetValue key with
                | true, info -> ValueSome info
                | _ ->
                    match table.Resolve key with
                    | ValueSome info ->
                        table.External.[key] <- info
                        ValueSome info
                    | ValueNone -> ValueNone

        /// For a question only an emitted declaration answers, where an import must not.
        let tryLocal (table: LocalThenExternal<'Info>) (key: TypeKey) : 'Info voption =
            match table.Local.TryGetValue key with
            | true, info -> ValueSome info
            | _ -> ValueNone

    /// The name and import home an external nominal is reached through: the key's simple name,
    /// because a JS name carries no generic arity, and the module its origin stamps.
    let private importedAs (kind: string) (key: TypeKey) (origin: SymbolOrigin) : struct (string * JsHome) =
        let (DisplayName name) = SymbolKeyOps.typeSimpleName key
        struct (name, JsHome.ofOrigin (sprintf "external %s '%s' (key %A)" kind name key) origin.Home)

    /// An external record's class is NOT re-emitted locally, but imported from the record's
    /// home module at each construction site.
    let private externalRecord (provider: IExternalSymbolProvider) (key: TypeKey) : JsRecordInfo voption =
        match provider.TryLookupType key with
        | ValueSome(ExternalTypeShape.Record(_, fields, origin, _)) ->
            let struct (name, home) = importedAs "record" key origin

            ValueSome
                {
                    Name = name
                    Fields = [ for f in fields -> f.Name ]
                    Home = ValueSome home
                }
        | _ -> ValueNone

    /// An external union's case classes are NOT re-emitted locally, but imported from its home
    /// module at each `UnionCons` site.
    let private externalUnion (provider: IExternalSymbolProvider) (key: TypeKey) : JsUnionInfo voption =
        match provider.TryLookupType key with
        | ValueSome(ExternalTypeShape.Union(_, cases, _, origin)) ->
            let struct (baseName, home) = importedAs "union" key origin

            let info, _ =
                buildUnionInfo (ValueSome home) baseName [ for c in cases -> c.Name, EqArray.toList c.FieldNames ]

            ValueSome info
        | _ -> ValueNone

    type WalkCtx =
        {
            Resolver: Resolver
            /// A node the expansion left anchored in a file OTHER than the one being compiled
            /// → that file and its index into THAT file's tokens; a node absent from it is the
            /// compiling file's own. Read through `Derivation`: a re-authored node is no key.
            NodeOrigins: Dictionary<TastAccessor.ExprId, InlineExpand.NodeOrigin>
            /// Every node the expansion or this walk AUTHORED → the node it was authored from,
            /// which is what keeps `NodeOrigins` and every node-keyed table readable on it.
            Derivation: InlineExpand.Derivation
            /// The declaring files this emission published, read back by the driver building
            /// the map. Shared mutable state, like `Pool`.
            MapSources: MapSources
            /// The file's node pool with this emission's append-only overlay: nodes the walker
            /// DERIVES (an `InlinableLet` splice) are appended to it mid-walk.
            Pool: PoolBuilder
            Records: LocalThenExternal<JsRecordInfo>
            Unions: LocalThenExternal<JsUnionInfo>
            /// Locally-emitted classes → emitted JS class name. A `New` resolves its ctor name
            /// here; an external `exn` subtype resolves through its native repr instead.
            Classes: Dictionary<TypeKey, string>
            /// Locally-emitted enums → the emitted JS object-map name. A `StaticFieldGet`
            /// (`E.Ci`) and an `EnumCase` pattern (`scrut === E.Ci`) both resolve it here.
            Enums: Dictionary<TypeKey, string>
            /// NOT optional: a compilation that resolves no external symbols still HAS a
            /// provider, the null one, which answers `ValueNone` to every lookup.
            Provider: IExternalSymbolProvider
            Imports: JsImports
            /// `true` in library mode: top-level `let` emits `export const …`.
            ExportTopLevel: bool
            /// Top-level module functions with their flat compiled form. Drives the FLAT
            /// emission: a module function emits as one multi-arg arrow (tuple groups
            /// flattened, lone unit erased); anything unsaturated gets a curried adapter.
            CompiledFns: Dictionary<BoundVarId, CompiledFns.CompiledFn>
            /// Keys of the file's locally-declared interfaces. A member whose declaring type
            /// is in this set lowers to `objArg.<member>(args)`, not the free type-prefixed
            /// `<Type>__<member>`. `CallVia` cannot say: an interface-TYPED object argument is `Self`.
            LocalInterfaces: HashSet<TypeKey>
            Capabilities: RuntimeNames.CapabilityIds
        }

    /// What an emission is handed, before the file's own declarations are collected. The
    /// collection needs `Capabilities` and `ExportTopLevel`, which is why they live here and
    /// not only on the `WalkCtx` the collected tables then complete.
    type EmissionInputs =
        {
            Resolver: Resolver
            Pool: PoolBuilder
            Provider: IExternalSymbolProvider
            Imports: JsImports
            /// The declaring files this emission publishes. Driver-owned shared mutable state,
            /// like `Pool` and `Imports`: the driver reads it back to build the map.
            MapSources: MapSources
            ExportTopLevel: bool
            /// Depends only on the provider, not the file, so it is resolved once, here.
            Capabilities: RuntimeNames.CapabilityIds
        }

    module EmissionInputs =
        let create
            (resolver: Resolver)
            (pool: PoolBuilder)
            (provider: IExternalSymbolProvider)
            (imports: JsImports)
            (exportTopLevel: bool)
            : EmissionInputs =
            {
                Resolver = resolver
                Pool = pool
                Provider = provider
                Imports = imports
                MapSources = MapSources.create ()
                ExportTopLevel = exportTopLevel
                Capabilities = ExternalSymbols.resolveCapabilities provider
            }

    module WalkCtx =
        /// Every table arrives filled, so no field of a `WalkCtx` is ever a placeholder.
        /// `NodeOrigins` and `Derivation` are the walk's own state, which it keeps appending to.
        let create
            (inputs: EmissionInputs)
            (collected: EmitJsTypes.CollectedTypes)
            (compiledFns: Dictionary<BoundVarId, CompiledFns.CompiledFn>)
            (localInterfaces: HashSet<TypeKey>)
            : WalkCtx =
            {
                Resolver = inputs.Resolver
                NodeOrigins = Dictionary()
                Derivation = InlineExpand.Derivation.create ()
                MapSources = inputs.MapSources
                Pool = inputs.Pool
                Records = LocalThenExternal.create collected.Records (externalRecord inputs.Provider)
                Unions = LocalThenExternal.create collected.Unions (externalUnion inputs.Provider)
                Classes = collected.Classes
                Enums = collected.Enums
                Provider = inputs.Provider
                Imports = inputs.Imports
                ExportTopLevel = inputs.ExportTopLevel
                CompiledFns = compiledFns
                LocalInterfaces = localInterfaces
                Capabilities = inputs.Capabilities
            }

    /// Where a node lands in the source it was WRITTEN in, for the map. Takes the NODE, not its
    /// anchor: a node copied out of a specialization was MOVED onto the call site, so its own
    /// anchor points to the compiling file; its declaring position rides beside it in `NodeOrigins`.
    let locOf (ctx: WalkCtx) (e: TastAccessor.ExprId) : JsLoc voption =
        match ctx.Resolver with
        | ValueNone -> ValueNone
        | ValueSome r ->
            let compiling () =
                match (TastAccessor.exprTok e).Index with
                | ValueSome i -> ValueSome(LineIndex.resolve r.Lines 0 r.Lexed.Tokens.[i].StartIndex)
                | ValueNone -> ValueNone

            match InlineExpand.Derivation.tryFind ctx.Derivation ctx.NodeOrigins e with
            | ValueSome origin ->
                match MapSources.tryFind origin.Path ctx.MapSources with
                | ValueNone ->
                    failwithf
                        "EmitJs: the node's origin file %s (assembly %s) was reached but never published to the map, so its position is readable only against the compiling file, because the provider that served the body and the retained anchor domain are not the same contract"
                        origin.Path.Relative.Name
                        (AssemblyName.toStored origin.Path.Assembly)
                | ValueSome declaring ->
                    let tok = LexedFiles.tokenAt r.Retained origin.Path origin.At

                    match tok.Index with
                    | TokenIndex.Regular _ -> ValueSome(LineIndex.resolve declaring.Lines declaring.Slot tok.StartIndex)
                    // A node no source spells keeps no position rather than borrowing the call
                    // site's, which would put declaring-file code on a caller line.
                    | TokenIndex.Virtual -> ValueNone
            | ValueNone -> compiling ()

    /// The inlinable-`let` reduction against the walk's own derivation: the splice re-authors
    /// every ancestor of a substituted `Var`, and `locOf` resolves origins along that chain.
    let (|InlinableLet|_|) (ctx: WalkCtx) (e: TastAccessor.ExprId) : TastAccessor.ExprId option =
        JsEmitHelpers.reduceInlinableLet ctx.Derivation e

    // ---- Records -------------------------------------------------------------

    /// Resolve a `RecordCons` / `RecordClone` / `FieldGet` record type to its `JsRecordInfo`,
    /// local or external.
    let recordInfoOf (ctx: WalkCtx) (what: string) (key: TypeKey) : JsRecordInfo =
        match LocalThenExternal.tryFind ctx.Records key with
        | ValueSome info -> info
        | ValueNone -> failwithf "EmitJs: %s on record with no emitted type (key %A)" what key

    /// The identifier this module writes for `klass`, recording the import an `Imported` one
    /// costs. THE place a `JsClassRef` becomes text, so nothing else has to know the split.
    let classIdentifier (ctx: WalkCtx) (klass: JsClassRef) : string =
        match klass with
        | JsClassRef.Local name
        | JsClassRef.Global name -> name
        | JsClassRef.Imported(home, name) -> JsImports.addTypeRef ctx.Imports home name

    /// The class identifier a nominal's construction site uses.
    let nominalCtorRef (ctx: WalkCtx) (klass: JsClassRef) (loc: JsLoc voption) : JsExpr =
        match klass with
        | JsClassRef.Local _
        | JsClassRef.Global _ -> JsExpr.Identifier(classIdentifier ctx klass, ValueNone)
        | JsClassRef.Imported _ -> JsExpr.Identifier(classIdentifier ctx klass, loc)

    /// The classes THIS file emits, as `tryClassRef` asks for them.
    let localClassOf (ctx: WalkCtx) (key: TypeKey) : string voption =
        match ctx.Classes.TryGetValue key with
        | true, name -> ValueSome name
        | _ -> ValueNone

    /// WHICH class an `ExprShape.New` constructs, and so how its arguments are passed.
    [<RequireQualifiedAccess>]
    type NewTarget =
        /// A class with a constructor of its own: every argument passes.
        | Class of JsClassRef
        /// The `exn` ROOT, whose repr IS the runtime class: `Error` has no constructor slot
        /// past the message, so only the leading argument survives.
        | ExnRoot of JsClassRef

    let tryNewTarget (ctx: WalkCtx) (key: TypeKey) : NewTarget voption =
        JsExternalMembers.tryClassRef ctx.Provider (localClassOf ctx) key
        |> ValueOption.map (fun klass ->
            // The root is the INTRINSIC carrying the repr, as against a class declared over it.
            match klass, ctx.Provider.TryLookupType key with
            | JsClassRef.Global _, ValueSome(ExternalTypeShape.Intrinsic _) -> NewTarget.ExnRoot klass
            | _ -> NewTarget.Class klass
        )

    // ---- Unions --------------------------------------------------------------

    /// Resolve a `UnionCons` / union-pattern type to its `JsUnionInfo`, local or
    /// external.
    let unionInfoOf (ctx: WalkCtx) (what: string) (key: TypeKey) : JsUnionInfo =
        match LocalThenExternal.tryFind ctx.Unions key with
        | ValueSome info -> info
        | ValueNone -> failwithf "EmitJs: %s on union with no emitted type (key %A)" what key

    let unionCaseFromInfo (info: JsUnionInfo) (what: string) (caseName: string) : JsUnionCaseDecl =
        match info.Cases.TryGetValue caseName with
        | true, c -> c
        | _ -> failwithf "EmitJs: %s on union '%s' has no case '%s'" what info.Name caseName

    let unionCaseOf (ctx: WalkCtx) (what: string) (key: TypeKey) (caseName: string) : JsUnionCaseDecl =
        unionCaseFromInfo (unionInfoOf ctx what key) what caseName

    // ---- Members -------------------------------------------------------------

    /// Key→name resolution for LOCAL member calls.
    module Members =

        /// The emitted type name for mangling: local union/record `Name`, else the key's simple name.
        let typeName (ctx: WalkCtx) (key: TypeKey) : string =
            match LocalThenExternal.tryLocal ctx.Unions key with
            | ValueSome info -> info.Name
            | ValueNone ->
                match LocalThenExternal.tryLocal ctx.Records key with
                | ValueSome info -> info.Name
                | ValueNone ->
                    let (DisplayName name) = SymbolKeyOps.typeSimpleName key
                    name

        /// The callable identifier of a local member's emitted function.
        let localFn (ctx: WalkCtx) (key: SymbolKey) (isStatic: bool) (isProperty: bool) (loc: JsLoc voption) : JsExpr =
            let dk = JsExternalMembers.declKey key
            let (DisplayName memberName) = SymbolKeyOps.simpleName key

            JsExpr.Identifier(JsExternalMembers.mangledName (typeName ctx dk) isStatic isProperty memberName, loc)

    /// `throw new Error("…")` — the fallthrough for a non-exhaustive match.
    let matchFailure: JsStatement =
        JsStatement.Throw(
            JsExpr.New(
                JsExpr.Identifier("Error", ValueNone),
                [
                    JsExpr.Literal(JsLiteral.String "The match cases were incomplete", ValueNone)
                ],
                ValueNone
            )
        )

    /// Conjoin a list of optional pattern tests with `&&` (a `None` test is
    /// always-true and drops out). `None` ⇒ the pattern is irrefutable.
    let conjoin (tests: JsExpr option list) : JsExpr option =
        match List.choose id tests with
        | [] -> None
        | t :: rest -> Some(List.fold (fun acc x -> JsExpr.Logical("&&", acc, x, ValueNone)) t rest)

    /// Disjoin refutability tests for an or-pattern's alternatives (`t1 || … || tn`). An
    /// irrefutable (`None`) alternative matches everything, so any `None` yields `None`.
    let disjoin (tests: JsExpr option list) : JsExpr option =
        match tests with
        | [] -> None
        | _ when List.exists Option.isNone tests -> None
        | t :: rest ->
            List.fold (fun acc x -> Option.map2 (fun a b -> JsExpr.Logical("||", a, b, ValueNone)) acc x) t rest

    /// Emit a top-level binding. `reassignable` (the bound variable is mutated elsewhere in the
    /// module) selects `export let` / `let` over the default `export const` / `const`, so a
    /// module-scope `let mutable` write is not an assignment-to-const `TypeError`.
    let topLevelBinding (ctx: WalkCtx) (reassignable: bool) (name: string) (init: JsExpr) : JsStatement =
        if ctx.ExportTopLevel then
            JsStatement.Export(name, init, reassignable)
        elif reassignable then
            JsStatement.Let(name, init)
        else
            JsStatement.Const(name, init)

    /// Emit a nested `let`/`const` for bound variable `k`: a reassignable `let` when the body
    /// mutates the bound variable (`k <- …`), else a `const`.
    let localBinding (k: BoundVarId) (body: TastAccessor.ExprId) (name: string) (init: JsExpr) : JsStatement =
        if isAssignedIn k body then
            JsStatement.Let(name, init)
        else
            JsStatement.Const(name, init)

    /// `console.<method>` — the sink for `printf`/`eprintf`.
    let console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("console", ValueNone), JsExpr.Identifier(method, ValueNone), false, ValueNone)

    /// An enum-case reference `E.Ci` → a property read on the enum's frozen JS object map.
    let enumCaseAccess (ctx: WalkCtx) (enumKey: TypeKey) (caseName: string) (loc: JsLoc voption) : JsExpr =
        match ctx.Enums.TryGetValue enumKey with
        | true, name -> JsExpr.Member(JsExpr.Identifier(name, loc), JsExpr.Identifier(caseName, ValueNone), false, loc)
        | _ ->
            // An EXTERNAL (TS-manifest) enum: its object map lives in the home module the TS
            // extractor produced, so `import { E }` and read `E.Ci`, which is what `tsc` emits.
            let home =
                JsExternalMembers.homeOf ctx.Provider enumKey (sprintf "enum case '%s'" caseName)

            // The enum object is imported under the name `tsc` exports it as.
            let (DisplayName enumName) = SymbolKeyOps.typeSimpleName enumKey
            let local = JsImports.addTypeRef ctx.Imports home enumName
            JsExpr.Member(JsExpr.Identifier(local, loc), JsExpr.Identifier(caseName, ValueNone), false, loc)

    /// A class `static let` backing field → a property on the emitted class object
    /// (`ClassName.field`); the static preamble initialises it at module load.
    let staticFieldRef (ctx: WalkCtx) (declKey: TypeKey) (fieldName: string) (loc: JsLoc voption) : JsExpr =
        match ctx.Classes.TryGetValue declKey with
        | true, className ->
            JsExpr.Member(JsExpr.Identifier(className, loc), JsExpr.Identifier(fieldName, ValueNone), false, loc)
        | _ -> failwithf "EmitJs: static field '%s' does not resolve to an emitted class ('%A')" fieldName declKey

    /// The import reference for an external VALUE: home and import FORM both read off the one
    /// `ExternalSymbol` the provider resolved. `Default` means a TS `export default`, lowered
    /// to `import x from '<spec>'`. A miss leaves no home, and the import then fails loudly.
    let externalValueRef (provider: IExternalSymbolProvider) (key: SymbolKey voption) : JsValueRef =
        let resolved =
            match key with
            | ValueSome k -> provider.TryLookup(SymbolKeyOps.qualifiedName k)
            | ValueNone -> ValueNone

        match resolved with
        | ValueSome sym ->
            {
                Key = key
                Home = JsHome.tryOfOrigin sym.Origin.Home
                Form = sym.ImportForm
            }
        | ValueNone ->
            {
                Key = key
                Home = ValueNone
                Form = ImportForm.Named
            }

    /// A `Vesper.Printf.mjs` runtime entry the BACKEND synthesises: codegen hardcodes both the
    /// key and the module. `structural-printer.js.fs` (a `.fs` without a `.fsi`) now publishes
    /// `structuralFormat` / `float32ToString` across the assembly boundary, so an emit-time
    /// provider lookup deletes this hardcode; it stands until that lookup exists.
    let private printfRuntimeRef (name: string) : JsValueRef =
        {
            Key = ValueSome(SymbolKeyOps.moduleValueKey "Vesper" "StructuralPrinter" name)
            Home = ValueSome(JsHome.ofAssembly "Vesper.Printf")
            Form = ImportForm.Named
        }

    /// The runtime entry for a `%A` (`Structured`) hole: the shape-keyed structural formatter
    /// in `Vesper.Printf.mjs`, imported and `$`-aliased through the external-call path.
    let structuralFormatRef: JsValueRef = printfRuntimeRef "structuralFormat"

    /// The runtime entry a `%O` on a `float32` renders through.
    let float32ToStringRef: JsValueRef = printfRuntimeRef "float32ToString"

    /// The JS repr `int64` / `uint64` bind to.
    [<Literal>]
    let private BigIntRepr = "bigint"

    /// How a plain-value (`%O`) hole's operand must be stringified. JS renders a number by its
    /// RUNTIME type; F# renders it by the operand's STATIC WIDTH, and the two disagree wherever
    /// a width's JS repr is not the width itself. `%O` is the only hole whose letter fixes none.
    [<RequireQualifiedAccess>]
    type PlainRender =
        /// The JS repr IS the F# width, so JS's own coercion already renders it .NET's way.
        | Native
        /// A JS `bigint` (`int64` / `uint64`). `console.log` inspects a bigint WITH its literal
        /// suffix (`1000000000001n`); .NET prints the digits alone, as `String(v)` does.
        | BigInt
        /// A `float32` whose repr is a JS `number`, an IEEE-754 DOUBLE, so JS renders it at
        /// double precision (`0.1f + 0.2f` → `0.30000001192092896`). Needs the runtime search.
        | Single

    /// Classify a `%O` operand's static type. The `BigInt` arm reads the type's declared JS
    /// repr, so a width that later binds to `bigint` inherits the suffix-stripping untouched.
    /// `float32` cannot: its repr (`number`) is what LOSES the width, so it goes by identity.
    let plainRenderOf (ctx: WalkCtx) (ty: FrozenType) : PlainRender =
        match ty with
        | FTConst(key, _) when key = RuntimeNames.float32Key -> PlainRender.Single
        | FTConst(key, _) ->
            match IntrinsicTypeMap.tryPlatformRepr key ctx.Provider.IntrinsicTypeMap with
            | ValueSome BigIntRepr -> PlainRender.BigInt
            | _ -> PlainRender.Native
        | _ -> PlainRender.Native

    /// Compile a pattern against a pure scrutinee-access expression `access` into a
    /// refutability test (`None` ⇒ irrefutable) and the `const` bindings its named sub-patterns
    /// introduce. `access` must be pure, because it is duplicated across the test and bindings.
    let rec compileMatchPattern
        (ctx: WalkCtx)
        (access: JsExpr)
        (pat: TastAccessor.PatId)
        : JsExpr option * JsStatement list =
        let memberAccess (field: string) =
            JsExpr.Member(access, JsExpr.Identifier(field, ValueNone), false, ValueNone)

        match TastAccessor.patKind pat with
        | PatShape.Wildcard -> None, []
        | PatShape.NamedSimple ->
            let k = (TastAccessor.patBoundVar pat).Value
            None, [ JsStatement.Const(boundVarNameOf ctx.Pool k, access) ]
        | PatShape.Const ->
            let value = TastAccessor.patConstValue pat
            Some(JsExpr.Binary("===", access, constExpr value ValueNone, ValueNone)), []
        | PatShape.Union ->
            let caseName = TastAccessor.patUnionCaseName pat
            let subPats = TastAccessor.patChildren pat
            let c = unionCaseOf ctx "match pattern" (TastAccessor.patNominalTy pat).Key caseName

            let tagTest =
                JsExpr.Binary(
                    "===",
                    memberAccess "tag",
                    JsExpr.Literal(JsLiteral.Number(string c.Tag), ValueNone),
                    ValueNone
                )

            // `map2` asserts the front-end invariant of one sub-pattern per field.
            let childTests, childBinds =
                List.map2
                    (fun fld sub -> compileMatchPattern ctx (memberAccess fld) sub)
                    c.Fields
                    (List.ofArray subPats)
                |> List.unzip

            conjoin (Some tagTest :: childTests), List.concat childBinds
        | PatShape.Record ->
            // Validate each field name against the emitted record so a stale name
            // fails here rather than silently reading `undefined`.
            let fields = TastAccessor.patRecordFields pat
            let info = recordInfoOf ctx "record pattern" (TastAccessor.patNominalTy pat).Key
            let known = Set.ofList info.Fields

            let tests, binds =
                [
                    for (fieldName, sub) in fields do
                        if not (Set.contains fieldName known) then
                            failwithf "EmitJs: record pattern on '%s' names unknown field '%s'" info.Name fieldName

                        compileMatchPattern ctx (memberAccess fieldName) sub
                ]
                |> List.unzip

            conjoin tests, List.concat binds
        // Tuple pattern: each element matches its positional index `access[i]`.
        | PatShape.Tuple ->
            let items = TastAccessor.patChildren pat

            let indexAccess i =
                JsExpr.Member(access, JsExpr.Literal(JsLiteral.Number(string i), ValueNone), true, ValueNone)

            let tests, binds =
                List.ofArray items
                |> List.mapi (fun i sub -> compileMatchPattern ctx (indexAccess i) sub)
                |> List.unzip

            conjoin tests, List.concat binds
        | PatShape.TypeTestAs -> failwithf "EmitJs: type-test patterns are not supported"
        // An enum case lowers to equality against its object-map slot (`scrut === E.Ci`); JS
        // `===` is value equality for the numbers and strings a case can hold.
        | PatShape.EnumCase ->
            let ec = TastAccessor.patEnumCase pat

            Some(JsExpr.Binary("===", access, enumCaseAccess ctx ec.EnumKey ec.CaseName ValueNone, ValueNone)), []
        // `null` pattern: JS loose `== null` matches both `null` and `undefined`.
        | PatShape.Null -> Some(JsExpr.Binary("==", access, JsExpr.Identifier("null", ValueNone), ValueNone)), []
        // `p1 | … | pn`: the arm matches iff SOME alternative matches. An alternative that binds
        // names is rejected before lowering, so the alternatives' binding lists are discarded.
        | PatShape.Or ->
            let alts = TastAccessor.patChildren pat
            let tests = [ for alt in alts -> fst (compileMatchPattern ctx access alt) ]

            disjoin tests, []
