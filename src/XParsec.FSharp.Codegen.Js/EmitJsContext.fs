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
            Provider: IExternalSymbolProvider voption
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
            LocalInterfaces: HashSet<SymbolKey>
        }

    let locOf (ctx: WalkCtx) (tok: SyntaxToken) : JsLoc voption =
        match ctx.Resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    // ---- Records -------------------------------------------------------------

    /// The nominal `SymbolKey` of a record/union construct's receiver type.
    /// A non-nominal receiver is an invariant break.
    let nominalKey (what: string) (ty: FrozenType) : SymbolKey =
        match TastLower.receiverShape ty with
        | ValueSome(key, _) -> key
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
    /// carries the home assembly). `ValueNone` when no provider or the type is not a
    /// union — caller fails loudly.
    let resolveExternalUnion (ctx: WalkCtx) (key: SymbolKey) : JsUnionInfo voption =
        match ctx.ExternalUnions.TryGetValue key with
        | true, info -> ValueSome info
        | _ ->
            match ctx.Provider with
            | ValueNone -> ValueNone
            | ValueSome provider ->
                match ExternalSymbols.tryLookupType provider key with
                | ValueSome(ExternalTypeShape.Union(_, cases, _, _)) ->
                    let baseName = SymbolKeyOps.simpleName key

                    let home =
                        match key with
                        | SymbolKey.TypeKey(Some asm, _, _) -> ValueSome asm
                        | _ -> failwithf "EmitJs: external union '%s' has no home assembly (key %A)" baseName key

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
        let typeName (ctx: WalkCtx) (key: SymbolKey) : string =
            match ctx.Unions.TryGetValue key with
            | true, info -> info.Name
            | _ ->
                match ctx.Records.TryGetValue key with
                | true, info -> info.Name
                | _ -> SymbolKeyOps.simpleName key

        /// The callable identifier of a local member's emitted function.
        let localFn (ctx: WalkCtx) (key: SymbolKey) (isStatic: bool) (isProperty: bool) (loc: JsLoc voption) : JsExpr =
            let dk = JsExternalMembers.declKey key

            JsExpr.Identifier(
                JsExternalMembers.mangledName (typeName ctx dk) isStatic isProperty (SymbolKeyOps.simpleName key),
                loc
            )

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

    /// A NATIVE well-known symbol — a member access on the global `Symbol`
    /// (`Symbol.dispose`, `Symbol.iterator`). Distinct from a `Symbol.for("…")` registry
    /// call (`registrySymbol`).
    let nativeSymbol (name: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("Symbol", ValueNone), JsExpr.Identifier(name, ValueNone), false, ValueNone)

    /// A process-wide REGISTRY symbol — `Symbol.for("<key>")`, resolving to the same
    /// symbol in every module with no import wiring (the eq/comp/hash dispatch slots).
    let registrySymbol (key: string) : JsExpr =
        JsExpr.Call(nativeSymbol "for", [ JsExpr.Literal(JsLiteral.String key, ValueNone) ], ValueNone)

    /// `Symbol.dispose` — shared by the disposable-impl method KEY (`emitDisposeMethod`)
    /// and `use`'s disposal call site (`disposeStmts`) so both name the identical slot.
    let symbolDispose: JsExpr = nativeSymbol "dispose"

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
            // no object map is re-emitted. The key's home assembly selects the module.
            match enumKey with
            | SymbolKey.TypeKey(Some asm, _, _) ->
                let local = JsImports.addTypeRef ctx.Imports asm (SymbolKeyOps.simpleName enumKey)
                JsExpr.Member(JsExpr.Identifier(local, loc), JsExpr.Identifier(caseName, ValueNone), false, loc)
            | _ ->
                failwithf
                    "EmitJs: enum case '%s' on a type with no emitted enum object and no home assembly (key %A)"
                    caseName
                    enumKey

    /// The import FORM of an external value's home-module export, read off the
    /// resolved `ExternalSymbol.ImportForm` (the provider seam that also carries
    /// `AttachMembers`): `Default` only for a TS `export default` (mitt's factory),
    /// which `JsImports.addRef` must lower to `import x from '<spec>'`. Key-based
    /// lookup exactly as `JsFlatFns.externalGroups` — codegen reads what the front
    /// end already resolved. A miss (no key / a symbol the provider doesn't model)
    /// is `Named`: every Vesper-emitted runtime export is named.
    let importFormOf (provider: IExternalSymbolProvider voption) (key: SymbolKey voption) : ImportForm =
        match provider, key with
        | ValueSome provider, ValueSome key ->
            match provider.TryLookup(SymbolKeyOps.qualifiedName key) with
            | ValueSome sym -> sym.ImportForm
            | ValueNone -> ImportForm.Named
        | _ -> ImportForm.Named

    /// The runtime entry for a `%A` (`Structured`) hole: the shape-keyed structural
    /// formatter in `Vesper.Printf.mjs` (Printf owns `%A`; the JS analogue of the
    /// Vesper.Printf CLR DLL), imported + `$`-aliased through the ordinary external-call
    /// path (like `structuralEquals`). No front-end symbol resolves to it — `%A` is
    /// front-end special-cased — so the backend synthesises its key, the codegen-owned
    /// analogue of the CLR backend's `AppendStructured<T>` member ref.
    let structuralFormatKey: SymbolKey voption =
        ValueSome(SymbolKey.ValueKey(Some "Vesper.Printf", "Vesper.StructuralPrinter", "structuralFormat"))

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

        match pat with
        | TPatG.Wildcard _ -> None, []
        | TPatG.NamedSimple(k, _, _) -> None, [ JsStatement.Const(identName ctx.Source k, access) ]
        | TPatG.Const(value, _, _) -> Some(JsExpr.Binary("===", access, constExpr value ValueNone, ValueNone)), []
        | TPatG.Union(caseName, subPats, ty, _) ->
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
                    (EqArray.toList subPats)
                |> List.unzip

            conjoin (Some tagTest :: childTests), List.concat childBinds
        | TPatG.Record(fields, ty, _) ->
            // Validate each field name against the emitted record so a stale name
            // fails here rather than silently reading `undefined`.
            let info = recordInfoOf ctx "record pattern" ty
            let known = Set.ofList info.Fields

            let tests, binds =
                EqArray.toList fields
                |> List.map (fun (fieldName, sub) ->
                    if not (Set.contains fieldName known) then
                        failwithf "EmitJs: record pattern on '%s' names unknown field '%s'" info.Name fieldName

                    compileMatchPattern ctx (memberAccess fieldName) sub
                )
                |> List.unzip

            conjoin tests, List.concat binds
        // Tuple pattern: each element matches its positional index `access[i]`.
        | TPatG.Tuple(items, _, _) ->
            let indexAccess i =
                JsExpr.Member(access, JsExpr.Literal(JsLiteral.Number(string i), ValueNone), true, ValueNone)

            let tests, binds =
                EqArray.toList items
                |> List.mapi (fun i sub -> compileMatchPattern ctx (indexAccess i) sub)
                |> List.unzip

            conjoin tests, List.concat binds
        | TPatG.TypeTestAs _ -> failwithf "EmitJs: type-test patterns are not supported"
        // v1 lowers an enum-case pattern to equality against the case's frozen
        // object-map slot (`scrut === E.Ci`). JS `===` is value equality for numbers
        // and strings, so this is correct for all three variants (v1 = equality only),
        // and keeping the test against `E.Ci` keeps the object map the single source
        // of truth (no per-case literal duplicated into the pattern).
        | TPatG.EnumCase(enumKey, caseName, _, _) ->
            Some(JsExpr.Binary("===", access, enumCaseAccess ctx enumKey caseName ValueNone, ValueNone)), []
        // `null` pattern: JS loose `== null` matches both `null` and `undefined`.
        | TPatG.Null _ -> Some(JsExpr.Binary("==", access, JsExpr.Identifier("null", ValueNone), ValueNone)), []
        // `p1 | … | pn`: the arm matches iff SOME alternative matches, so the test
        // is the disjunction of the alternatives' tests. An irrefutable alternative
        // (`test = None`) makes the whole or-pattern irrefutable — `disjoin` folds
        // it to `None`. Alternatives bind nothing (name resolution drops or-pattern
        // binders — `FreezePatterns` rejects a binding alternative), so the binding
        // lists are empty and discarded.
        | TPatG.Or(alts, _, _) ->
            let tests =
                EqArray.toList alts
                |> List.map (fun alt -> fst (compileMatchPattern ctx access alt))

            disjoin tests, []
