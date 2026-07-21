namespace XParsec.FSharp.Codegen.Js

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers
open EmitJsTypes

/// The `buildExpr`-free foundation the JS emitters share: the ambient `WalkCtx`, its
/// source-map line index, the name/type resolution helpers, and `compileMatchPattern`
/// (which never recurses into expression emission). `EmitJs`, `EmitJsFormat`, and
/// `EmitJsMembers` all open this module.
module EmitJsContext =

    /// Maps a source char offset to 0-based (line, column) — V3 source-map
    /// coordinates. `Starts.[n]` is the char offset at which line `n` begins.
    /// Columns count UTF-16 code units, as V3 maps require.
    type LineIndex = { Starts: int[]; Length: int }

    module LineIndex =
        let build (source: string) : LineIndex =
            let starts = ResizeArray<int>()
            starts.Add 0

            for i in 0 .. source.Length - 1 do
                if source.[i] = '\n' then
                    starts.Add(i + 1)

            {
                Starts = starts.ToArray()
                Length = source.Length
            }

        /// Resolve a char offset to a `JsLoc`. Clamps past-end offsets to the last line.
        let resolve (idx: LineIndex) (offset: int) : JsLoc =
            let offset = max 0 (min offset idx.Length)
            let starts = idx.Starts
            // Binary search for the greatest line start <= offset.
            let mutable lo = 0
            let mutable hi = starts.Length - 1

            while lo < hi do
                let mid = (lo + hi + 1) / 2

                if starts.[mid] <= offset then lo <- mid else hi <- mid - 1

            {
                Line = lo
                Column = offset - starts.[lo]
            }

    /// Resolve a node's `'tok` to a source `loc`. `ValueNone` disables maps
    /// (no source text supplied); `ValueSome` carries the line index.
    type Resolver = LineIndex voption

    /// The walker's ambient context.
    type WalkCtx =
        {
            Resolver: Resolver
            Source: string voption
            Records: Dictionary<SymbolKey, JsRecordInfo>
            Unions: Dictionary<SymbolKey, JsUnionInfo>
            /// Locally-emitted classes (`[<CustomEquality>]` & plain classes), keyed
            /// by type `SymbolKey` → emitted JS class name. A `New` of a local class
            /// resolves its constructor name here (external `exn` subtypes go through
            /// `exnReprOf` instead).
            Classes: Dictionary<SymbolKey, string>
            /// Locally-emitted enums, keyed by enum-type `SymbolKey` → the emitted JS
            /// object-map name (`collectTypes`). A `StaticFieldGet` (`E.Ci`) / an
            /// `EnumCase` pattern (`scrut === E.Ci`) resolves the object name here.
            Enums: Dictionary<SymbolKey, string>
            /// External union types (`Option`, `List`) not in the file's `tast.Decls`;
            /// their case shapes are read off the provider on first use and emitted as
            /// nominal JS classes (same base-class + subclass shape a local union gets).
            ///
            /// NOT optional. A compilation that resolves no external symbols still HAS a
            /// provider — `ExternalSymbolProviders.nullProvider`, which answers `ValueNone`
            /// to every lookup. Modelling absence a second time, as a `voption` around it,
            /// bought nothing and cost correctness: it forced every reader to write an arm
            /// for "no provider", and the only honest answer there is "I cannot know" —
            /// which `plainRenderOf` (`%O` width fidelity) had to fake as "renders
            /// natively", the wrong answer for exactly the widths it exists to catch.
            Provider: IExternalSymbolProvider
            /// External unions resolved on demand, keyed by `SymbolKey`. A miss in
            /// `Unions` falls back here. Their case classes are imported from the
            /// union's home module at each `UnionCons` site, not re-emitted.
            ExternalUnions: Dictionary<SymbolKey, JsUnionInfo>
            Imports: JsImports
            /// `true` in library mode: top-level `let` emits `export const …`.
            ExportTopLevel: bool
            /// Top-level module functions keyed by binding, with their flat compiled
            /// form (`CompiledFns.gather`). Drives the Fable-style FLAT emission: a
            /// module function emits as one multi-arg arrow (tuple groups flattened,
            /// lone unit erased) and a saturated call collapses its spine to a single
            /// flat call; a value-use / under-application gets an inline curried adapter.
            /// Empty until `buildProgram`
            /// populates it from the lowered decls.
            CompiledFns: Dictionary<NodeKey, CompiledFns.CompiledFn>
            /// Keys of the file's locally-declared interfaces (`TTypeKindG.Interface`).
            /// A `PropertyGet`/`MethodCall` whose member's declaring type is in this set
            /// dispatches through a LOCAL interface slot (`(r :> IRank).Rank`): the impl is
            /// an ATTACHED method on the receiver's class, so the access lowers to
            /// `receiver.<member>(args)` rather than the free receiver-first
            /// `<Type>__<member>` form. This is NOT derivable from the node's `CallVia`:
            /// the front end stamps `CallVia.Interface` only for the generic-typar rung-3
            /// case (`'T :> IFace`, `mkInterfaceMethodCall`); an interface-typed receiver
            /// (`(r :> IRank).M()`) is an ordinary `CallVia.Self` whose interface-ness
            /// lives only in the member's declaring key. CLR needs no such table (native
            /// interface dispatch handles both); JS, lacking it, recovers the fact here.
            /// Empty until `buildProgram` populates it from `tast.Decls`.
            LocalInterfaces: HashSet<TypeKey>
            /// The language-capability identities behind both halves of the JS capability
            /// protocol (`EmitJsCapabilities`). Unlike the tables above, this depends only on
            /// the provider, not on the file — so `WalkCtx.create` resolves it up front and no
            /// caller ever holds a `WalkCtx` whose capabilities are a placeholder.
            Capabilities: RuntimeNames.CapabilityIds
        }

    module WalkCtx =
        /// The ONE `WalkCtx` constructor — production (`Codegen.compileWith`) and the tests
        /// share it. The file-derived tables start empty for `EmitJs.buildProgram` to fill
        /// (they need the `TastFile`); `Capabilities` is resolved HERE, since it needs only the
        /// provider. A provider that names no capability resolves to the all-unnamed set, so
        /// there is no absent-provider case to carry.
        let create
            (resolver: Resolver)
            (source: string voption)
            (provider: IExternalSymbolProvider)
            (imports: JsImports)
            (exportTopLevel: bool)
            : WalkCtx =
            {
                Resolver = resolver
                Source = source
                Records = Dictionary()
                Unions = Dictionary()
                Classes = Dictionary()
                Enums = Dictionary()
                Provider = provider
                ExternalUnions = Dictionary()
                Imports = imports
                ExportTopLevel = exportTopLevel
                CompiledFns = Dictionary()
                LocalInterfaces = HashSet()
                Capabilities = ExternalSymbols.resolveCapabilities provider
            }

    let locOf (ctx: WalkCtx) (tok: SyntaxToken) : JsLoc voption =
        match ctx.Resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    // ---- Records -------------------------------------------------------------

    /// The nominal key of a record/union construct's receiver type, widened for the
    /// kind-blind emitted-type tables (`ctx.Records` / `ctx.Unions`).
    /// A non-nominal receiver is an invariant break.
    let nominalKey (what: string) (ty: FrozenType) : SymbolKey =
        match TastLower.receiverShape ty with
        | ValueSome(key, _) -> SymbolKey.Type key
        | ValueNone -> failwithf "EmitJs: %s on non-nominal type %A" what ty

    /// Resolve a `RecordCons` / `RecordClone` / `FieldGet` receiver to its `JsRecordInfo`.
    let recordInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsRecordInfo =
        let key = nominalKey what ty

        match ctx.Records.TryGetValue key with
        | true, info -> info
        | _ -> failwithf "EmitJs: %s on record with no emitted type (key %A)" what key

    // ---- Unions --------------------------------------------------------------

    /// Resolve an external union to a `JsUnionInfo` via the provider, caching in
    /// `ExternalUnions`. The case classes are NOT re-emitted locally — they are
    /// imported from the union's home module at each `UnionCons` site (`Home`
    /// carries the home assembly). `ValueNone` when the provider does not know the type,
    /// or knows it as something other than a union — caller fails loudly.
    let resolveExternalUnion (ctx: WalkCtx) (key: SymbolKey) : JsUnionInfo voption =
        match ctx.ExternalUnions.TryGetValue key with
        | true, info -> ValueSome info
        | _ ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Union(_, cases, _, origin)) ->
                // Backend name emission: the JS class/factory identifiers this union's
                // cases are imported under are mangled off its name (JS names carry no
                // generic arity).
                let (DisplayName baseName) = SymbolKeyOps.simpleName key

                // The shape the provider resolved is what knows WHERE the union lives —
                // the key names only WHAT it is.
                let home =
                    match origin.Home.AssemblyOption with
                    | ValueSome _ as h -> h
                    | ValueNone -> failwithf "EmitJs: external union '%s' has no home assembly (key %A)" baseName key

                let info, _ =
                    buildUnionInfo home baseName [ for c in cases -> c.Name, List.ofArray c.FieldNames ]

                ctx.ExternalUnions.[key] <- info
                ValueSome info
            | _ -> ValueNone

    /// Resolve a `UnionCons` / union-pattern receiver type + case name to the emitted
    /// `JsUnionCaseDecl`. Falls through to the external-union provider on a local miss.
    let unionInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsUnionInfo =
        let key = nominalKey what ty

        match ctx.Unions.TryGetValue key with
        | true, info -> info
        | _ ->
            match resolveExternalUnion ctx key with
            | ValueSome info -> info
            | ValueNone -> failwithf "EmitJs: %s on union with no emitted type (key %A)" what key

    let unionCaseFromInfo (info: JsUnionInfo) (what: string) (caseName: string) : JsUnionCaseDecl =
        match info.Cases.TryGetValue caseName with
        | true, c -> c
        | _ -> failwithf "EmitJs: %s on union '%s' has no case '%s'" what info.Name caseName

    let unionCaseOf (ctx: WalkCtx) (what: string) (ty: FrozenType) (caseName: string) : JsUnionCaseDecl =
        unionCaseFromInfo (unionInfoOf ctx what ty) what caseName

    // ---- Members -------------------------------------------------------------

    /// Key→name resolution for LOCAL member calls, shared between `emitMemberFn` and
    /// the call-site lowerings in the walker. (The mangling scheme itself and every
    /// external-world helper live in `JsExternalMembers`.)
    module Members =

        /// The emitted type name for mangling: local union/record `Name`, else the key's simple name.
        let typeName (ctx: WalkCtx) (key: TypeKey) : string =
            match ctx.Unions.TryGetValue(SymbolKey.Type key) with
            | true, info -> info.Name
            | _ ->
                match ctx.Records.TryGetValue(SymbolKey.Type key) with
                | true, info -> info.Name
                | _ ->
                    // Backend name emission: what the type is called in the emitted JS.
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

    /// Disjoin refutability tests for an or-pattern's alternatives (`t1 || … || tn`).
    /// A `None` alternative is irrefutable, and an irrefutable alternative makes the
    /// whole disjunction irrefutable (matches everything) — so any `None`, or an empty
    /// list, yields `None`; otherwise every alternative is a real test and they `||` up.
    let disjoin (tests: JsExpr option list) : JsExpr option =
        match tests with
        | [] -> None
        | _ when List.exists Option.isNone tests -> None
        | t :: rest ->
            List.fold (fun acc x -> Option.map2 (fun a b -> JsExpr.Logical("||", a, b, ValueNone)) acc x) t rest

    /// Emit a top-level binding. `reassignable` (the binder is mutated elsewhere in the
    /// module) selects the reassignable form — `export let` / `let` — over the default
    /// `export const` / `const`, so a module-scope `let mutable` write is not an
    /// assignment-to-const `TypeError`. Mirrors the nested `localBinding` selection.
    let topLevelBinding (ctx: WalkCtx) (reassignable: bool) (name: string) (init: JsExpr) : JsStatement =
        if ctx.ExportTopLevel then
            JsStatement.Export(name, init, reassignable)
        elif reassignable then
            JsStatement.Let(name, init)
        else
            JsStatement.Const(name, init)

    /// Emit a nested (non-top-level) `let`/`const` for binder `k`: a reassignable `let`
    /// when the body mutates the binder (`k <- …`), else a `const`. The top-level
    /// analogue is `topLevelBinding`.
    let localBinding (k: NodeKey) (body: Frozen.TExpr) (name: string) (init: JsExpr) : JsStatement =
        if isAssignedIn k body then
            JsStatement.Let(name, init)
        else
            JsStatement.Const(name, init)

    /// `console.<method>` — the sink for `printf`/`eprintf`.
    let console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("console", ValueNone), JsExpr.Identifier(method, ValueNone), false, ValueNone)

    /// An enum-case reference `E.Ci` → a property read on the frozen object map (the
    /// step-6 JS enum repr). Shared by the `StaticFieldGet` expression and the
    /// `EnumCase` pattern's `scrut === E.Ci` test, so both name the identical slot and
    /// the object map stays the single source of truth (no per-case constant inlined,
    /// no reverse map). The enum object name is looked up by its type `SymbolKey`.
    let enumCaseAccess (ctx: WalkCtx) (enumKey: SymbolKey) (caseName: string) (loc: JsLoc voption) : JsExpr =
        match ctx.Enums.TryGetValue enumKey with
        | true, name -> JsExpr.Member(JsExpr.Identifier(name, loc), JsExpr.Identifier(caseName, ValueNone), false, loc)
        | _ ->
            // An EXTERNAL (TS-manifest) enum: its object map is NOT emitted locally —
            // it lives in the home module the TS extractor produced. Import the enum
            // object (`import { E } from './<asm>.mjs'`) and read the case (`E.Ci`),
            // mirroring the external-union case-class import (`addTypeRef`). The
            // `import { E } + E.Ci` shape is exactly what `tsc` emits for the enum, so
            // no object map is re-emitted. The enum's resolved SHAPE names the module.
            let asm =
                JsExternalMembers.assemblyOf ctx.Provider enumKey (sprintf "enum case '%s'" caseName)

            // Backend name emission: the enum object is imported under the name `tsc` emits.
            let (DisplayName enumName) = SymbolKeyOps.simpleName enumKey
            let local = JsImports.addTypeRef ctx.Imports asm enumName
            JsExpr.Member(JsExpr.Identifier(local, loc), JsExpr.Identifier(caseName, ValueNone), false, loc)

    /// A class `static let` backing field → a property on the emitted class object
    /// (`ClassName.field`). Shared by the `StaticFieldGet` read and the `StaticFieldSet`
    /// write so both name the identical slot; the static preamble initialises it at module
    /// load. `declKey` is the declaring class's nominal `SymbolKey`, keying `ctx.Classes`.
    let staticFieldRef (ctx: WalkCtx) (declKey: SymbolKey) (fieldName: string) (loc: JsLoc voption) : JsExpr =
        match ctx.Classes.TryGetValue declKey with
        | true, className ->
            JsExpr.Member(JsExpr.Identifier(className, loc), JsExpr.Identifier(fieldName, ValueNone), false, loc)
        | _ -> failwithf "EmitJs: static field '%s' resolves to no emitted class ('%A')" fieldName declKey

    /// The import reference for an external VALUE: its home and the FORM its home module
    /// exports it under, BOTH read off the one `ExternalSymbol` the provider resolved for
    /// the key — the shape is the `key -> home` oracle (`Home`), and the same seam carries
    /// `ImportForm` (`Default` only for a TS `export default` — mitt's factory — which
    /// `JsImports.addRef` must lower to `import x from '<spec>'`). Key-based lookup exactly
    /// as `JsFlatFns.externalGroups`: codegen reads what the front end already resolved.
    ///
    /// A miss (no key / a symbol the provider doesn't model) leaves the home `Local` and
    /// the form `Named`: nothing names a module to import from, so `addRef` fails loudly
    /// rather than emitting a dangling import — and every Vesper-emitted runtime export is
    /// named anyway.
    let externalValueRef (provider: IExternalSymbolProvider) (key: SymbolKey voption) : JsValueRef =
        let resolved =
            match key with
            | ValueSome k -> provider.TryLookup(SymbolKeyOps.qualifiedName k)
            | ValueNone -> ValueNone

        match resolved with
        | ValueSome sym ->
            {
                Key = key
                Home = sym.Origin.Home
                Form = sym.ImportForm
            }
        | ValueNone ->
            {
                Key = key
                Home = Origin.Unstamped
                Form = ImportForm.Named
            }

    /// A `Vesper.Printf.mjs` runtime entry the BACKEND synthesises: no front-end symbol
    /// resolves to it (the specifiers it serves are front-end special-cased), so no
    /// provider shape carries its home either — codegen names both the key and the module,
    /// here, in one place.
    let private printfRuntimeRef (name: string) : JsValueRef =
        {
            Key = ValueSome(SymbolKeyOps.moduleValueKey "Vesper" "StructuralPrinter" name)
            Home = Origin.InAssembly(AssemblyName "Vesper.Printf")
            Form = ImportForm.Named
        }

    /// The runtime entry for a `%A` (`Structured`) hole: the shape-keyed structural
    /// formatter in `Vesper.Printf.mjs` (Printf owns `%A`; the JS analogue of the
    /// Vesper.Printf CLR DLL), imported + `$`-aliased through the ordinary external-call
    /// path (like `structuralEquals`) — the codegen-owned analogue of the CLR backend's
    /// `AppendStructured<T>` member ref.
    let structuralFormatRef: JsValueRef = printfRuntimeRef "structuralFormat"

    /// The runtime entry a `%O` on a `float32` renders through — `float32ToString` in the
    /// same `Vesper.Printf.mjs`.
    let float32ToStringRef: JsValueRef = printfRuntimeRef "float32ToString"

    /// The JS repr `int64` / `uint64` bind to (`prim-types-int.js.fs`).
    [<Literal>]
    let private BigIntRepr = "bigint"

    /// How a plain-value (`%O`) hole's operand must be stringified. JS renders a number by
    /// its RUNTIME type; F# renders it by the operand's STATIC WIDTH, and the two disagree
    /// wherever a width's JS repr is not the width itself. `%O` is the only specifier whose
    /// argument type the letter does not fix (`PrintfSpec.argType` types `%d` as `int` and
    /// `%f` as `float`), so it is the only hole that can carry such a width.
    [<RequireQualifiedAccess>]
    type PlainRender =
        /// The JS repr IS the F# width — JS's own coercion already renders it .NET's way.
        | Native
        /// A JS `bigint` (`int64` / `uint64`). `console.log` inspects a bigint WITH its
        /// literal suffix (`1000000000001n`); .NET's `ToString()` prints the digits alone,
        /// which is what an explicit `String(v)` yields.
        | BigInt
        /// A `float32`, whose repr is a JS `number` — an IEEE-754 DOUBLE — so JS renders
        /// the single at double precision (`0.1f + 0.2f` → `0.30000001192092896`, not
        /// `0.3`). Needs the runtime's shortest-round-trip search (`float32ToString`).
        | Single

    /// Classify a `%O` operand's static type. The `BigInt` arm reads the type's declared JS
    /// repr — the single source of truth (`prim-types-*.js.fs`), so a width that later binds
    /// to `bigint` inherits the suffix-stripping without touching this. `float32` cannot be
    /// recovered that way, and is matched against its canonical IDENTITY
    /// (`RuntimeNames.float32Key`) instead: its repr (`number`) is precisely what LOSES the
    /// width, so the type itself is the only carrier of the fact. The identity needs no
    /// alias canonicalisation — an intrinsic abbreviation (`single`) is expanded eagerly at
    /// name resolution, so only `float32` ever reaches here (see
    /// `Inline.staticOptTypesMatch`).
    let plainRenderOf (ctx: WalkCtx) (ty: FrozenType) : PlainRender =
        match ty with
        | FTConst(key, _) when key = RuntimeNames.float32Key -> PlainRender.Single
        | FTConst(key, _) ->
            match ctx.Provider.IntrinsicForwardRepr.TryGetValue key with
            | true, repr when repr = BigIntRepr -> PlainRender.BigInt
            | _ -> PlainRender.Native
        | _ -> PlainRender.Native

    /// Compile a pattern against a pure scrutinee-access expression `access` into a
    /// refutability test (`None` ⇒ irrefutable) and the `const` bindings its named
    /// sub-patterns introduce. `access` must be pure — it is duplicated across test
    /// and bindings; the short-circuit ensures a sub-field is read only after its tag matched.
    ///
    /// Free of expression emission (no `buildExpr`), so it lives here rather than in the
    /// main walker recursion — both `buildMatchArm` and the destructuring `for … in`
    /// binder reuse it.
    let rec compileMatchPattern (ctx: WalkCtx) (access: JsExpr) (pat: Frozen.TPat) : JsExpr option * JsStatement list =
        let memberAccess (field: string) =
            JsExpr.Member(access, JsExpr.Identifier(field, ValueNone), false, ValueNone)

        match TastAccessor.patKind pat with
        | PatShape.Wildcard -> None, []
        | PatShape.NamedSimple ->
            let k = (TastAccessor.patBinder pat).Value
            None, [ JsStatement.Const(binderName ctx.Source k, access) ]
        | PatShape.Const ->
            let value = TastAccessor.patConstValue pat
            Some(JsExpr.Binary("===", access, constExpr value ValueNone, ValueNone)), []
        | PatShape.Union ->
            let caseName = TastAccessor.patUnionCaseName pat
            let ty = TastAccessor.patTy pat
            let subPats = TastAccessor.patChildren pat
            let c = unionCaseOf ctx "match pattern" ty caseName

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
            let ty = TastAccessor.patTy pat
            let fields = TastAccessor.patRecordFields pat
            let info = recordInfoOf ctx "record pattern" ty
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
        // v1 lowers an enum-case pattern to equality against the case's frozen
        // object-map slot (`scrut === E.Ci`). JS `===` is value equality for numbers
        // and strings, so this is correct for all three variants (v1 = equality only),
        // and keeping the test against `E.Ci` keeps the object map the single source
        // of truth (no per-case literal duplicated into the pattern).
        | PatShape.EnumCase ->
            let ec = TastAccessor.patEnumCase pat
            Some(JsExpr.Binary("===", access, enumCaseAccess ctx ec.EnumKey ec.CaseName ValueNone, ValueNone)), []
        // `null` pattern: JS loose `== null` matches both `null` and `undefined`.
        | PatShape.Null -> Some(JsExpr.Binary("==", access, JsExpr.Identifier("null", ValueNone), ValueNone)), []
        // `p1 | … | pn`: the arm matches iff SOME alternative matches, so the test
        // is the disjunction of the alternatives' tests. An irrefutable alternative
        // (`test = None`) makes the whole or-pattern irrefutable — `disjoin` folds
        // it to `None`. Alternatives bind nothing (name resolution drops or-pattern
        // binders — `ElaboratePatterns` rejects a binding alternative), so the binding
        // lists are empty and discarded.
        | PatShape.Or ->
            let alts = TastAccessor.patChildren pat
            let tests = [ for alt in alts -> fst (compileMatchPattern ctx access alt) ]

            disjoin tests, []
