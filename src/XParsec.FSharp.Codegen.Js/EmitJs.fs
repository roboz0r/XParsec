namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm
open JsEmitHelpers
open EmitJsTypes

/// The `TAST → JsAst` walker. Every un-handled node is an explicit `failwithf`,
/// so an unsupported arm fails loudly rather than dropping silently.
///
/// Durable conventions:
///   * Functions are **curried unary arrows** — `Lambda` → nested `(a) => (b) => …`,
///     `App` → unary calls (`f a b` → `f(a)(b)`). Tail self-recursion trampolines to
///     `while (true)` with param-shadow mutation for constant stack.
///   * Operator bodies arrive pre-spliced as `ILIntrinsic` `$N`-templates;
///     `finishOps` is identity (templates are already JS-emit-able). `TastLower.lower`
///     drops `type` decls — record/union/member shapes are read off the un-lowered
///     decls in `collectTypes`.
///   * Records/unions emit as data-only JS `class`es (positional ctor; union = base
///     `tag` + one `extends`-subclass per case). Members emit as free, curried,
///     *receiver-first* functions — never prototype methods (match + the structural
///     runtime read `.tag`/own-keys, never `instanceof`).
///   * `Match` lowers to an IIFE testing each arm in order, an unmatched value throwing.
module EmitJs =

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
            Records: System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>
            Unions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            /// Locally-emitted classes (`[<CustomEquality>]` & plain classes), keyed
            /// by type `SymbolKey` → emitted JS class name. A `New` of a local class
            /// resolves its constructor name here (external `exn` subtypes go through
            /// `exnReprOf` instead).
            Classes: System.Collections.Generic.Dictionary<SymbolKey, string>
            /// Locally-emitted enums, keyed by enum-type `SymbolKey` → the emitted JS
            /// object-map name (`collectTypes`). A `StaticFieldGet` (`E.Ci`) / an
            /// `EnumCase` pattern (`scrut === E.Ci`) resolves the object name here.
            Enums: System.Collections.Generic.Dictionary<SymbolKey, string>
            /// External union types (`Option`, `List`) not in the file's `tast.Decls`;
            /// their case shapes are read off the provider on first use and emitted as
            /// nominal JS classes (same base-class + subclass shape a local union gets).
            Provider: IExternalSymbolProvider voption
            /// External unions resolved on demand, keyed by `SymbolKey`. A miss in
            /// `Unions` falls back here. Their case classes are imported from the
            /// union's home module at each `UnionCons` site, not re-emitted.
            ExternalUnions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
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
            CompiledFns: System.Collections.Generic.Dictionary<NodeKey, CompiledFns.CompiledFn>
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
            LocalInterfaces: System.Collections.Generic.HashSet<SymbolKey>
        }

    let private locOf (ctx: WalkCtx) (tok: SyntaxToken) : JsLoc voption =
        match ctx.Resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    // ---- Records -------------------------------------------------------------

    /// The nominal `SymbolKey` of a record/union construct's receiver type.
    /// A non-nominal receiver is an invariant break.
    let private nominalKey (what: string) (ty: FrozenType) : SymbolKey =
        match TastLower.receiverShape ty with
        | ValueSome(key, _) -> key
        | ValueNone -> failwithf "EmitJs: %s on non-nominal type %A" what ty

    /// Resolve a `RecordCons` / `RecordClone` / `FieldGet` receiver to its `JsRecordInfo`.
    let private recordInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsRecordInfo =
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
    let private resolveExternalUnion (ctx: WalkCtx) (key: SymbolKey) : JsUnionInfo voption =
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
    let private unionInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsUnionInfo =
        let key = nominalKey what ty

        match ctx.Unions.TryGetValue key with
        | true, info -> info
        | _ ->
            match resolveExternalUnion ctx key with
            | ValueSome info -> info
            | ValueNone -> failwithf "EmitJs: %s on union with no emitted type (key %A)" what key

    let private unionCaseFromInfo (info: JsUnionInfo) (what: string) (caseName: string) : JsUnionCaseDecl =
        match info.Cases.TryGetValue caseName with
        | true, c -> c
        | _ -> failwithf "EmitJs: %s on union '%s' has no case '%s'" what info.Name caseName

    let private unionCaseOf (ctx: WalkCtx) (what: string) (ty: FrozenType) (caseName: string) : JsUnionCaseDecl =
        unionCaseFromInfo (unionInfoOf ctx what ty) what caseName

    // ---- Members -------------------------------------------------------------

    /// Key→name resolution for LOCAL member calls, shared between `emitMemberFn` and
    /// the call-site lowerings in the walker. (The mangling scheme itself and every
    /// external-world helper live in `JsExternalMembers`.)
    module private Members =

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
    let private matchFailure: JsStatement =
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
    let private conjoin (tests: JsExpr option list) : JsExpr option =
        match List.choose id tests with
        | [] -> None
        | t :: rest -> Some(List.fold (fun acc x -> JsExpr.Logical("&&", acc, x, ValueNone)) t rest)

    /// Emit a top-level binding as `export const` (library) or `const` (script).
    let private topLevelBinding (ctx: WalkCtx) (name: string) (init: JsExpr) : JsStatement =
        if ctx.ExportTopLevel then
            JsStatement.Export(name, init)
        else
            JsStatement.Const(name, init)

    // The Fable-style FLAT module-function helpers — flat-call collapse, the curried
    // adapter, external-`ValRepr` resolution — live in `JsFlatFns`, decoupled from this
    // walker via a `build` callback (mirroring the CLR `EmitCall.flattenGroupPushes`
    // `recur` parameter). The EXTERNAL-member lowerings — provider flags, the `exn`
    // repr climb, the attached / erased / mangled call shapes — live in
    // `JsExternalMembers` on the same seam. Only the trampoline-coupled
    // `emitFlatModuleFn` / `trampolineOrExpr` stay in this recursion group.

    // ---- The walker ----------------------------------------------------------

    /// A NATIVE well-known symbol — a member access on the global `Symbol`
    /// (`Symbol.dispose`, `Symbol.iterator`). Distinct from a `Symbol.for("…")` registry
    /// call (`registrySymbol`).
    let private nativeSymbol (name: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("Symbol", ValueNone), JsExpr.Identifier(name, ValueNone), false, ValueNone)

    /// A process-wide REGISTRY symbol — `Symbol.for("<key>")`, resolving to the same
    /// symbol in every module with no import wiring (the eq/comp/hash dispatch slots).
    let private registrySymbol (key: string) : JsExpr =
        JsExpr.Call(nativeSymbol "for", [ JsExpr.Literal(JsLiteral.String key, ValueNone) ], ValueNone)

    /// `Symbol.dispose` — shared by the disposable-impl method KEY (`emitDisposeMethod`)
    /// and `use`'s disposal call site (`disposeStmts`) so both name the identical slot.
    let private symbolDispose: JsExpr = nativeSymbol "dispose"

    /// An enum-case reference `E.Ci` → a property read on the frozen object map (the
    /// step-6 JS enum repr). Shared by the `StaticFieldGet` expression and the
    /// `EnumCase` pattern's `scrut === E.Ci` test, so both name the identical slot and
    /// the object map stays the single source of truth (no per-case constant inlined,
    /// no reverse map). The enum object name is looked up by its type `SymbolKey`.
    let private enumCaseAccess (ctx: WalkCtx) (enumKey: SymbolKey) (caseName: string) (loc: JsLoc voption) : JsExpr =
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
    let private importFormOf (provider: IExternalSymbolProvider voption) (key: SymbolKey voption) : ImportForm =
        match provider, key with
        | ValueSome provider, ValueSome key ->
            match provider.TryLookup(SymbolKeyOps.qualifiedName key) with
            | ValueSome sym -> sym.ImportForm
            | ValueNone -> ImportForm.Named
        | _ -> ImportForm.Named

    let rec buildExpr (ctx: WalkCtx) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok e)

        match e with
        | TExprG.Const(value, _, _) -> constExpr value loc

        // A bare reference to a local module FUNCTION is a value-use (an escape): it
        // wraps the flat function in an inline curried adapter so a higher-order
        // consumer (or a partial application) sees the SOURCE-shaped currying. A simple
        // single-arg / lone-unit function needs no adapter (flat == curried there).
        | TExprG.Var(k, _, _) ->
            let ident = JsExpr.Identifier(identName ctx.Source k, loc)

            match ctx.CompiledFns.TryGetValue k with
            | true, cf when JsFlatFns.needsAdapter cf.Groups -> JsFlatFns.curryAdapter ident cf.Groups k.Offset loc
            | _ -> ident

        // An external module function — imported from its package's JS runtime module.
        // A value-use of a multi-arg / tupled external function gets the same curried
        // adapter (its producer emits flat); a saturated call flattens at the `App` arm.
        | TExprG.External(compiledName, key, _, _) ->
            let alias =
                JsExpr.Identifier(JsImports.addRef ctx.Imports compiledName key (importFormOf ctx.Provider key), loc)

            match JsFlatFns.externalGroups ctx.Provider key with
            | ValueSome groups when JsFlatFns.needsAdapter groups ->
                JsFlatFns.curryAdapter alias groups (TastWalk.exprTok e).StartIndex loc
            | _ -> alias

        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            JsExpr.Conditional(buildExpr ctx cond, buildExpr ctx thenE, buildExpr ctx elseE, loc)

        // A `Sequential` in expression position is a comma expression (top-level it
        // expands to statements via `buildStatements`).
        | TExprG.Sequential(xs, _, _) -> JsExpr.Sequence([ for x in xs -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]`; a pattern reads elements by index.
        | TExprG.Tuple(items, _, _) -> JsExpr.Array([ for x in items -> buildExpr ctx x ], loc)

        // Pure `let` in expression position: substitute into uses (collapse operator
        // templates). A *mutable* binder (assigned in the body) is excluded — it must
        // stay a real binding so its writes land; it falls to the IIFE arm, where the
        // arrow parameter is the (reassignable) mutable cell.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value && not (isAssignedIn k body) ->
            buildExpr ctx (substVar k value body)

        // Non-pure (or mutable) `let` in expression position: JS has no let-expression,
        // so lowers to an IIFE `((x) => <body>)(<value>)` — the binder evaluated once,
        // and (for a mutable binder) reassignable as the arrow parameter.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let name = identName ctx.Source k

            JsExpr.Call(
                JsExpr.Arrow([ name ], JsFnBody.Expr(buildExpr ctx body), ValueNone),
                [ buildExpr ctx value ],
                loc
            )

        // `let _ = value in body` — a Wildcard binder discards the value, kept only for
        // its effects (`let _ = renderInto buf` over `|> ignore`, which leaves a bare
        // recipe value). A pure value contributes nothing, so drop it; otherwise a comma
        // sequence evaluates `value` then yields `body` (JS has no let-expression).
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) when isPureValue value -> buildExpr ctx body
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) ->
            JsExpr.Sequence([ buildExpr ctx value; buildExpr ctx body ], loc)

        // Anonymous lambda — no binder key, so no self-tail-call analysis applies.
        | TExprG.Lambda _ -> emitFunction ctx ValueNone e

        // Application. A SATURATED call to a module function (local or external)
        // collapses its whole spine into a single FLAT call (`f(a, b)`, tuple groups
        // flattened, lone unit dropped); any residual over-application folds on as unary
        // calls. Everything else — closures, members, under-applied module functions —
        // keeps the curried `f(a)(b)` shape (one unary call per `App`); an under-applied
        // module function reaches its head's curried adapter through this fallback.
        | TExprG.App(fn, arg, _, _) ->
            let head, spine = TastWalk.collectSpine [] e

            // Flat dispatch: a native attached-member call (`JsExternalMembers.
            // tryAttachedCall`) folds the whole spine into ONE `receiver.member(args)`;
            // else a saturated module-function call collapses to a flat call; anything
            // else keeps the curried unary fallback.
            match JsExternalMembers.tryAttachedCall ctx.Provider (buildExpr ctx) head spine loc with
            | ValueSome call -> call
            | ValueNone ->
                // Resolve a spine head that names a module function to its flat callee +
                // SOURCE groups — a local `CompiledFns` entry or an external `ValRepr`. The
                // groups are non-empty by construction (both `gather` and the external
                // `ValRepr` capture require ≥ 1 source group), so the saturation predicate
                // below is written ONCE for both kinds: a flat call exactly when the spine
                // is at least the group count. Anything else keeps the curried fallback.
                let flatHead: (JsExpr * Frozen.ArgGroup list) voption =
                    let identAt name =
                        JsExpr.Identifier(name, locOf ctx (TastWalk.exprTok head))

                    match head with
                    | TExprG.Var(k, _, _) ->
                        match ctx.CompiledFns.TryGetValue k with
                        | true, cf -> ValueSome(identAt (identName ctx.Source k), cf.Groups)
                        | _ -> ValueNone
                    | TExprG.External(compiledName, key, _, _) ->
                        JsFlatFns.externalGroups ctx.Provider key
                        |> ValueOption.map (fun groups ->
                            identAt (JsImports.addRef ctx.Imports compiledName key (importFormOf ctx.Provider key)),
                            groups
                        )
                    | _ -> ValueNone

                match flatHead with
                | ValueSome(callee, groups) when List.length spine >= List.length groups ->
                    JsFlatFns.emitFlatCall (buildExpr ctx) callee groups spine loc
                | _ -> JsExpr.Call(buildExpr ctx fn, [ buildExpr ctx arg ], loc)

        // A record literal `{ X = e1; Y = e2 }` → `new R(args…)`, the args
        // reordered from source order to the class's *declaration*-order
        // positional constructor.
        | TExprG.RecordCons(srcFields, ty, _) ->
            let info = recordInfoOf ctx "RecordCons" ty
            let srcMap = Map.ofSeq (EqArray.toList srcFields)

            let args =
                [
                    for f in info.Fields ->
                        match Map.tryFind f srcMap with
                        | Some e -> buildExpr ctx e
                        | None -> failwithf "EmitJs (Step 3): record literal for '%s' is missing field '%s'" info.Name f
                ]

            JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), args, loc)

        // `{ r with X = v; … }` → reconstruction `new R(…)`: each field takes its
        // override if listed, else reads `<src>.field`. `<src>` is read once per
        // copied field, so a bare `Var` is spliced inline; any other source is bound
        // once through an IIFE binder (avoids re-evaluating / duplicating it).
        | TExprG.RecordClone(source, overrides, ty, _) ->
            let info = recordInfoOf ctx "RecordClone" ty
            let overrideMap = Map.ofSeq (EqArray.toList overrides)

            let argsFrom (srcRef: JsExpr) =
                [
                    for f in info.Fields ->
                        match Map.tryFind f overrideMap with
                        | Some ov -> buildExpr ctx ov
                        | None -> JsExpr.Member(srcRef, JsExpr.Identifier(f, ValueNone), false, ValueNone)
                ]

            match source with
            | TExprG.Var _ -> JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), argsFrom (buildExpr ctx source), loc)
            | _ ->
                let sName = "_rc" + string (TastWalk.exprTok e).StartIndex

                let newExpr =
                    JsExpr.New(
                        JsExpr.Identifier(info.Name, ValueNone),
                        argsFrom (JsExpr.Identifier(sName, ValueNone)),
                        loc
                    )

                JsExpr.Call(JsExpr.Arrow([ sName ], JsFnBody.Expr newExpr, ValueNone), [ buildExpr ctx source ], loc)

        // `r.X` → `r.X` — a member access on the record's like-named property
        // (the emitted class stores each field under its source field name).
        | TExprG.FieldGet(receiver, fieldName, _, _) ->
            JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(fieldName, ValueNone), false, loc)

        // `r.X <- v` → `(r.X = v)` — a mutable (`val mutable`) instance-field write,
        // the field analogue of the mutable-local `Assignment` arm. Unit-typed in F#,
        // so the yielded value is unused; in statement position `buildStatements`
        // wraps it as an expression statement.
        | TExprG.FieldSet(receiver, fieldName, value, _, _) ->
            JsExpr.Assign(
                JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(fieldName, ValueNone), false, loc),
                buildExpr ctx value,
                loc
            )

        // A union constructor `Case e0 e1 …` → `new <Union>_<Case>(args…)`. The
        // args already arrive in declaration (field) order, so — unlike a record
        // literal — no reordering is needed; the subclass constructor stores them
        // positionally under the case's field names.
        | TExprG.UnionCons(caseName, args, ty, _) ->
            let info = unionInfoOf ctx "UnionCons" ty
            let c = unionCaseFromInfo info "UnionCons" caseName

            // A local union's class is in this file; an external union's case class is
            // imported from its home module (no local re-emit).
            let callee =
                match info.Home with
                | ValueSome asm -> JsExpr.Identifier(JsImports.addTypeRef ctx.Imports asm c.ClassName, loc)
                | ValueNone -> JsExpr.Identifier(c.ClassName, ValueNone)

            JsExpr.New(callee, [ for a in args -> buildExpr ctx a ], loc)

        // External exception construction → `new <exn repr>(msg)`. The repr is sourced
        // from the `inherit` chain via `exnReprOf`; only the leading message arg is kept
        // (`Error` has no slot for further args). Non-`exn`-subtype external `New` fails loudly.
        | TExprG.New(className, args, ty, _) ->
            // A locally-emitted class constructs by its emitted name with positional
            // args (the ctor stores each into the like-named field). Resolved before
            // the external `exn`-repr path.
            let localClassName =
                match TastLower.receiverShape ty with
                | ValueSome(key, _) ->
                    match ctx.Classes.TryGetValue key with
                    | true, name -> ValueSome name
                    | _ -> ValueNone
                | ValueNone -> ValueNone

            // A GLOBAL (ambient) external class — its home is a global pack (`Js.Map`,
            // `Js.Widget`) — constructs by its BARE export name with NO import: the JS
            // runtime provides it intrinsically. `Global` rides the HOME (the resolved
            // shape's `ExternalClassFlags.Global`, reached via the receiver-shape key),
            // so this fires for an es2015-home type but not a real package. The bare
            // name is the key's simple name (`Js.Widget` → `Widget`). Resolved after
            // the local-class path and before the external `exn`-repr fallback.
            let globalClassName =
                match TastLower.receiverShape ty with
                | ValueSome(key, _) ->
                    JsExternalMembers.classFlagsOf ctx.Provider key
                    |> ValueOption.filter (fun flags -> flags.Global)
                    |> ValueOption.map (fun _ -> SymbolKeyOps.simpleName key)
                | ValueNone -> ValueNone

            match localClassName, globalClassName with
            | ValueSome name, _
            | ValueNone, ValueSome name ->
                JsExpr.New(JsExpr.Identifier(name, ValueNone), [ for a in args -> buildExpr ctx a ], loc)
            | ValueNone, ValueNone ->
                match JsExternalMembers.exnReprOf ctx.Provider ty with
                | ValueSome repr ->
                    let errArgs =
                        match EqArray.toList args with
                        | [] -> []
                        | msg :: _ -> [ buildExpr ctx msg ]

                    JsExpr.New(JsExpr.Identifier(repr, ValueNone), errArgs, loc)
                | ValueNone ->
                    failwithf
                        "EmitJs (Step 8): construction of external type '%s' has no JS analogue (only `exn` subtypes lower to `new <exn repr>`)"
                        className

        // A member access through a LOCAL interface slot (`(r :> IRank).Rank`): the impl
        // is an ATTACHED method on the receiver's class (the plain-attached partition), so
        // dispatch as a flat member access `receiver.<member>(args)` — the free-function
        // `<Type>__<member>` form names no emitted function for an interface member. The
        // member's declaring type being a local interface is the signal (not the node's
        // `CallVia` — see `WalkCtx.LocalInterfaces`). An interface-impl PROPERTY emits as a
        // zero-arg attached method, so its read is the same member access called with no
        // args.
        | TExprG.PropertyGet(receiver, key, _, _, _) when ctx.LocalInterfaces.Contains(JsExternalMembers.declKey key) ->
            JsExpr.Call(attachedAccess ctx loc receiver key, [], loc)

        | TExprG.MethodCall(receiver, key, _, args, _, _) when
            ctx.LocalInterfaces.Contains(JsExternalMembers.declKey key)
            ->
            JsExpr.Call(attachedAccess ctx loc receiver key, [ for a in args -> buildExpr ctx a ], loc)

        // Member access on a local record/union: each member is a free receiver-first function.
        | TExprG.PropertyGet(receiver, key, _, _, _) ->
            JsExpr.Call(Members.localFn ctx key false true ValueNone, [ buildExpr ctx receiver ], loc)

        | TExprG.MethodCall(receiver, key, _, args, _, _) ->
            let withRecv =
                JsExpr.Call(Members.localFn ctx key false false ValueNone, [ buildExpr ctx receiver ], loc)

            applyArgs ctx withRecv args

        | TExprG.StaticPropertyGet(key, _, _) -> Members.localFn ctx key true true loc

        // An enum-case reference `E.Ci` → a property read on the frozen object map.
        // `StaticFieldGet` is the general static-field carrier (a class `static let`
        // backing-field read also lowers to it), so route to `enumCaseAccess` ONLY
        // when the node's type is the enum itself (`FTEnum`, stamped by Unification's
        // enum arm). A non-enum key (a future `static let`) falls through to a loud
        // failure rather than `enumCaseAccess` fabricating a bogus self-import.
        | TExprG.StaticFieldGet(enumKey, caseName, FTEnum _, _) -> enumCaseAccess ctx enumKey caseName loc
        | TExprG.StaticFieldGet(declKey, fieldName, _, _) ->
            failwithf
                "EmitJs: static-field read of '%s' on '%A' is not an enum case; class `static let` field reads are not yet supported on the JS target"
                fieldName
                declKey

        | TExprG.StaticMethodCall(key, args, _, _) -> applyArgs ctx (Members.localFn ctx key true false loc) args

        // A member on an external type. The declaring type's provider flags × the
        // receiver's presence pick the lowering — the whole dispatch in one table:
        //   * an ERASED grouping type erases to the bare module export
        //     (`erasedGroupingRef`); it holds only STATIC members, so a receiver is
        //     an invariant break;
        //   * an ATTACH-MEMBERS instance member reached WITHOUT an applying spine —
        //     the CALL form is folded in the `App` head-case — is a native value
        //     read: a data-property READ for a property, an eta-wrapped method
        //     value for a method. (R2 scope is INSTANCE members: a static member /
        //     ctor — `receiver = ValueNone` — falls through until its native
        //     lowering lands.)
        //   * everything else — including statics/ctors on an AttachMembers type —
        //     takes the mangled-import path, which is only satisfiable by a
        //     Vesper-provided runtime module (`JsImports.entryFor` fails loudly
        //     when the package has none — a real npm package cannot export a
        //     mangled name).
        | TExprG.ExternalMember(receiver, key, memberName, storage, _, _) ->
            let declKey = JsExternalMembers.declKey key
            // JS has no field/property distinction at access — both are a value member
            // (the `get_`-style mangled import); only a `Method` is an arrow. (A `Field`
            // here would gain only `readonly` fidelity, not yet modelled.)
            let isProperty = storage.IsValueMember

            match JsExternalMembers.classFlagsOf ctx.Provider declKey, receiver with
            | ValueSome {
                            MemberLowering = MemberLowering.ErasedBare
                        },
              ValueSome _ ->
                failwithf
                    "EmitJs (Step 9b): erased grouping type member '%s' has an instance receiver, but a synthetic free-function-overload type carries only static members"
                    memberName
            | ValueSome {
                            MemberLowering = MemberLowering.ErasedBare
                        },
              ValueNone -> JsExternalMembers.erasedGroupingRef ctx.Imports declKey memberName loc
            | ValueSome {
                            MemberLowering = MemberLowering.AttachedNative
                        },
              ValueSome r when isProperty ->
                // A manifest Property is a JS DATA property — native access is a plain
                // member READ `recv.prop`, NOT a zero-arg call. (Contrast the LOCAL
                // interface-impl property path, which emits `Call(attachedAccess, [])`
                // because Vesper compiles interface properties as zero-arg methods; a
                // TS property is genuinely a data slot, not a method.)
                JsExternalMembers.attachedMember (buildExpr ctx r) memberName loc
            | ValueSome {
                            MemberLowering = MemberLowering.AttachedNative
                        },
              ValueSome r ->
                JsExternalMembers.etaWrapAttachedMethod
                    (buildExpr ctx)
                    r
                    key
                    memberName
                    (TastWalk.exprTok e).StartIndex
                    loc
            | _ ->
                JsExternalMembers.mangledMemberAccess
                    ctx.Provider
                    ctx.Imports
                    (buildExpr ctx)
                    declKey
                    receiver
                    memberName
                    isProperty
                    loc

        // `match scrut with …` → an IIFE binding the scrutinee once, then testing each
        // arm in order and `return`ing the first whose pattern (+ guard) matches; an
        // unmatched value `throw`s. Sequential test (not `switch(tag)`) so it covers
        // guards, constants, nested patterns, and non-union scrutinees uniformly.
        | TExprG.Match(scrutinee, arms, _, _) ->
            let mv = "_m" + string (TastWalk.exprTok e).StartIndex
            let access = JsExpr.Identifier(mv, ValueNone)

            let body =
                [
                    for arm in EqArray.toList arms do
                        yield! buildMatchArm ctx access arm
                    yield matchFailure
                ]

            JsExpr.Call(JsExpr.Arrow([ mv ], JsFnBody.Block body, loc), [ buildExpr ctx scrutinee ], loc)

        // A mutable-local / array-element write `lhs <- rhs` → the JS assignment
        // expression `(lhs = rhs)`. Unit-typed in F#, so its yielded value is unused;
        // in statement position `buildStatements` wraps it as an expression statement.
        | TExprG.Assignment(lhs, rhs, _, _) -> JsExpr.Assign(buildExpr ctx lhs, buildExpr ctx rhs, loc)

        // `while cond do body` in expression position. JS `while` is a statement, so it
        // lowers to a zero-arg IIFE `(() => { while (<cond>) { <body> } })()` that yields
        // `undefined` (the F# `unit` result). Statement position keeps the bare loop —
        // see `buildStatements`.
        | TExprG.While(cond, body, _, _) ->
            let loop = JsStatement.While(buildExpr ctx cond, buildStatements ctx body)
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block [ loop ], loc), [], loc)

        // `for i = a to b do body` in expression position — same IIFE wrapper as `while`;
        // `buildStatements` produces the hoisted-limit `const` + the `for` statement.
        | TExprG.ForTo _ -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `for x in source do body` in expression position — same IIFE wrapper as `for…to`
        // (the loop yields `unit`); `buildStatements` produces the `for…of`.
        | TExprG.ForIn _ -> JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block(buildStatements ctx e), loc), [], loc)

        // `use x = value in body` in expression position. JS `try/finally` is a
        // statement, so it lowers to a zero-arg IIFE that parks the binder, `return`s
        // the body's value from the `try`, and disposes in the `finally`. The body is
        // a single expression (`Sequential` becomes a comma expression, a nested `let`
        // its own IIFE), so returning `buildExpr ctx body` preserves the result through
        // the disposal in the `finally`.
        | TExprG.Use(binding, value, body, dispose, _ty, _tok) ->
            let name = useBinderName ctx binding

            let tryFinally =
                JsStatement.TryFinally([ JsStatement.Return(buildExpr ctx body) ], disposeStmts ctx dispose name)

            let block = [ JsStatement.Const(name, buildExpr ctx value); tryFinally ]
            JsExpr.Call(JsExpr.Arrow([], JsFnBody.Block block, loc), [], loc)

        // `e :> obj` (value→`obj` box, synthesised at Freeze for an `obj` parameter/field).
        // JS is dynamically typed — every value is already a boxed `obj` — so the box is a
        // no-op; emit the source verbatim. The downcast `e :?> T` is likewise identity (no
        // runtime nominal type to check).
        | TExprG.Upcast(source, _, _)
        | TExprG.Downcast(source, _, _) -> buildExpr ctx source

        // The tokenful array intrinsics — `Array.zeroCreate` / `arr.[i]` / `arr.[i] <- v`
        // / `arr.Length`, desugared to `newarr`/`ldelem`/`stelem`/`ldlen` (the same
        // mnemonics the CLR backend reads; they are target-neutral, the element-type
        // operand is dropped on JS). They reach the backend because their inline bodies
        // live in `ops-platform.js.fs` (`array.fs`'s `zeroCreate` for `newarr`).
        | TExprG.ILIntrinsic("newarr", _, args, _, _) ->
            // `Array.zeroCreate count` → `Array(count).fill(null)`: a *dense* array (not
            // the sparse `new Array(count)`), so `Object.keys` / iteration observe every
            // slot. Unset slots read as `null`, not the element type's zero — the JS
            // zero-init erasure corner (callers fill before reading).
            match EqArray.toList args with
            | [ count ] ->
                let alloc =
                    JsExpr.Call(JsExpr.Identifier("Array", ValueNone), [ buildExpr ctx count ], ValueNone)

                let fill =
                    JsExpr.Member(alloc, JsExpr.Identifier("fill", ValueNone), false, ValueNone)

                JsExpr.Call(fill, [ JsExpr.Identifier("null", ValueNone) ], loc)
            | _ -> failwith "EmitJs: 'newarr' expects one operand (the element count)"

        // `arr.[i]` → `arr[i]` (a computed member read).
        | TExprG.ILIntrinsic("ldelem", _, args, _, _) ->
            match EqArray.toList args with
            | [ arr; idx ] -> JsExpr.Member(buildExpr ctx arr, buildExpr ctx idx, true, loc)
            | _ -> failwith "EmitJs: 'ldelem' expects two operands (array, index)"

        // `arr.[i] <- v` → `(arr[i] = v)` (a computed-member assignment expression).
        | TExprG.ILIntrinsic("stelem", _, args, _, _) ->
            match EqArray.toList args with
            | [ arr; idx; value ] ->
                let target = JsExpr.Member(buildExpr ctx arr, buildExpr ctx idx, true, ValueNone)
                JsExpr.Assign(target, buildExpr ctx value, loc)
            | _ -> failwith "EmitJs: 'stelem' expects three operands (array, index, value)"

        // `arr.Length` → `arr.length`.
        | TExprG.ILIntrinsic("ldlen", _, args, _, _) ->
            match EqArray.toList args with
            | [ arr ] -> JsExpr.Member(buildExpr ctx arr, JsExpr.Identifier("length", ValueNone), false, loc)
            | _ -> failwith "EmitJs: 'ldlen' expects one operand (the array)"

        // The empty-string identity intrinsic `(# "" x : 'U #)` — FSharp.Core's
        // erasing reinterpret (`retype`, the primitive `dynamic` enter/exit builds on).
        // It has NO runtime effect: emit the lone operand verbatim, re-typed (the CLR
        // emits nothing likewise). Handled before the generic `$N`-template expander,
        // which would (correctly) reject an operand-bearing template with no hole.
        | TExprG.ILIntrinsic("", _, args, _, _) when args.Length = 1 -> buildExpr ctx args.[0]

        | TExprG.ILIntrinsic(opCode, _, args, _, _) -> JsExpr.Raw(expandTemplate ctx opCode (EqArray.toList args), loc)

        | TExprG.Format(sink, segments, _, _) ->
            let arg = buildFormatArg ctx segments

            match sink with
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            | other -> failwithf "EmitJs: unsupported format sink %A" other

        | other -> failwithf "EmitJs: unsupported expression %A" other

    and private console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("console", ValueNone), JsExpr.Identifier(method, ValueNone), false, ValueNone)

    /// Expand a `$N` JS-expression template into `JsRawSeg`s: verbatim chunks
    /// interleaved with operand expressions. `$$` is a literal `$`. A bare CIL
    /// mnemonic with operands but no `$N` hole is a hard error.
    and private expandTemplate (ctx: WalkCtx) (template: string) (args: Frozen.TExpr list) : JsRawSeg list =
        let segs = ResizeArray<JsRawSeg>()
        let buf = System.Text.StringBuilder()
        let mutable sawHole = false

        let flush () =
            if buf.Length > 0 then
                segs.Add(JsRawSeg.Verbatim(buf.ToString()))
                buf.Clear() |> ignore

        let mutable i = 0

        while i < template.Length do
            let c = template.[i]

            if c = '$' && i + 1 < template.Length && template.[i + 1] = '$' then
                buf.Append '$' |> ignore
                i <- i + 2
            elif c = '$' && i + 1 < template.Length && System.Char.IsDigit template.[i + 1] then
                flush ()
                let mutable j = i + 1

                while j < template.Length && System.Char.IsDigit template.[j] do
                    j <- j + 1

                let idx =
                    System.Int32.Parse(template.Substring(i + 1, j - i - 1), CultureInfo.InvariantCulture)

                if idx < 0 || idx >= List.length args then
                    failwithf
                        "EmitJs: template '%s' references operand $%d but only %d supplied"
                        template
                        idx
                        (List.length args)

                segs.Add(JsRawSeg.Hole(buildExpr ctx (List.item idx args)))
                sawHole <- true
                i <- j
            else
                buf.Append c |> ignore
                i <- i + 1

        flush ()

        // Operands present but no hole → bare CIL mnemonic escaped the CLR-only finish pass.
        if not (List.isEmpty args) && not sawHole then
            failwithf "EmitJs: non-template ILIntrinsic opcode '%s' reached the JS backend" template

        List.ofSeq segs

    /// Build the single argument a `console.log`/`error` call prints from format segments.
    /// Mixed formats are concatenations seeded with `""` so every `+` is string-valued.
    and private buildFormatArg (ctx: WalkCtx) (segments: EqArray<Frozen.FormatSeg>) : JsExpr =
        match EqArray.toList segments with
        | [ FormatSegG.Lit s ] -> JsExpr.Literal(JsLiteral.String s, ValueNone)
        | [ FormatSegG.Hole(hole, operand) ] -> buildHole ctx hole operand
        | segs ->
            let pieces = ResizeArray<JsRawSeg>()
            // Seed with `""` so the first `+` already concatenates strings, even
            // when the format opens with two adjacent holes (`%d%d`).
            // Seed with `""` so the first `+` already concatenates strings even when
            // the format opens with two adjacent holes (`%d%d`).
            pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String "", ValueNone)))

            for seg in segs do
                pieces.Add(JsRawSeg.Verbatim " + ")

                match seg with
                | FormatSegG.Lit s -> pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String s, ValueNone)))
                | FormatSegG.Hole(hole, operand) -> pieces.Add(JsRawSeg.Hole(buildHole ctx hole operand))

            JsExpr.Raw(List.ofSeq pieces, ValueNone)

    /// The runtime entry for a `%A` (`Structured`) hole: the shape-keyed structural
    /// formatter in `Vesper.Printf.mjs` (Printf owns `%A`; the JS analogue of the
    /// Vesper.Printf CLR DLL), imported + `$`-aliased through the ordinary external-call
    /// path (like `structuralEquals`). No front-end symbol resolves to it — `%A` is
    /// front-end special-cased — so the backend synthesises its key, the codegen-owned
    /// analogue of the CLR backend's `AppendStructured<T>` member ref.
    and private structuralFormatKey: SymbolKey voption =
        ValueSome(SymbolKey.ValueKey(Some "Vesper.Printf", "Vesper.StructuralPrinter", "structuralFormat"))

    /// Build the JS expression a format hole's argument contributes.
    ///
    /// The JS backend reads the hole's classified semantic model (`HoleForm` /
    /// `FieldFormat`, on `hole.Source`)
    /// directly and builds real `JsExpr` nodes from the typed fields — no `.NET`
    /// format string is reconstructed or re-parsed (that dialect is CLR-only). A
    /// `RawFormat` interpolation clause (`{x:X}`) is such a CLR dialect string, so
    /// JS doesn't interpret it — the raw operand stands (the concat coerces it).
    ///
    /// `%A` (`PercentA`) renders the value as copy-pasteable source through the
    /// `structuralFormat` runtime, a flat call `(value, width, size)`; the width /
    /// node-size budgets resolve through `percentAWidth` / `percentASize` (the
    /// `80` / `10000` defaults, shared with the CLR `AppendStructured`).
    ///
    /// Every `Field` hole is the specifier's per-hole formatting reproduced as an
    /// inline JS expression (no runtime import), byte-matching the CLR `Formatter`
    /// members. The operand is evaluated exactly once: single-reference forms splice
    /// it directly; the one form that reads it repeatedly (`DecimalZeroPad`) binds it
    /// in an arrow IIFE, so `%05d (f ())` still calls `f` once.
    ///
    /// Covered: `%d`/`%i`/`%s`/`%O`/`%c`/`%M` (`Verbatim`), width + alignment
    /// (`padStart` / `padEnd`), `%x`/`%X`/`%B`/`%o` (`IntRadix`), `%u` (`Unsigned`
    /// reinterpret), `%b` (`Bool`), `%f` (`Fixed`), `%0wd` (`DecimalZeroPad`),
    /// `%0w.pf` (`FixedZeroPad`), and `%+`/`% ` (`ForcedSign`). Still cold (raw
    /// operand, deferred per the plan): `%e`/`%E`/`%g`/`%G` (`Exponential`/`Compact`)
    /// — the .NET exponent / compact field widths have no faithful JS analogue.
    and private buildHole (ctx: WalkCtx) (hole: Frozen.HoleSpec) (operand: Frozen.TExpr) : JsExpr =
        let num (n: int) =
            JsExpr.Literal(JsLiteral.Number(string n), ValueNone)

        let str (s: string) =
            JsExpr.Literal(JsLiteral.String s, ValueNone)

        let id (s: string) = JsExpr.Identifier(s, ValueNone)
        let call callee args = JsExpr.Call(callee, args, ValueNone)
        // A method call `recv.m(args…)`.
        let invoke (recv: JsExpr) (m: string) (args: JsExpr list) =
            JsExpr.Call(JsExpr.Member(recv, id m, false, ValueNone), args, ValueNone)
        // Parenthesise a bare numeric-literal `.method` receiver — `5.toFixed(0)` is a
        // JS syntax error and `-3.14.toFixed(2)` mis-binds as `-(3.14.toFixed(2))`.
        // Self-parenthesising receivers (`Binary` shifts, `Call`s) need no wrap.
        let receiver (e: JsExpr) : JsExpr =
            match e with
            | JsExpr.Literal _ -> JsExpr.Sequence([ e ], ValueNone)
            | _ -> e

        // The hole's field alignment, applied to an already-built string expression:
        // `Some w` ⇒ right-justify (`padStart w`), `Some -w` ⇒ left (`padEnd w`). The
        // zero-pad forms carry their width inside the `FieldFormat` and set alignment
        // `None`, so this is a no-op there.
        let withAlign (alignment: int option) (e: JsExpr) : JsExpr =
            match alignment with
            | Some a when a >= 0 -> invoke e "padStart" [ num a ]
            | Some a -> invoke e "padEnd" [ num (-a) ]
            | None -> e

        // Splice the operand once into `build value`. Use for forms that read the
        // value a single time (a duplicated side-effecting operand would re-run).
        let direct (build: JsExpr -> JsExpr) : JsExpr = build (buildExpr ctx operand)

        // Bind the operand to `v` in an arrow IIFE — for forms that read it more than
        // once, so it evaluates exactly once: `((v) => build(v))(operand)`.
        let iife (build: JsExpr -> JsExpr) : JsExpr =
            call (JsExpr.Arrow([ "v" ], JsFnBody.Expr(build (id "v")), ValueNone)) [ buildExpr ctx operand ]

        // `((s) => build(s))(inner)` — name an intermediate *string* result `s` so the
        // sign-aware float forms can inspect it (`s.startsWith("-")`) without rebuilding
        // it; `inner` already references the operand once.
        let strBind (inner: JsExpr) (build: JsExpr -> JsExpr) : JsExpr =
            call (JsExpr.Arrow([ "s" ], JsFnBody.Expr(build (id "s")), ValueNone)) [ inner ]

        let emitField (fmt: FieldFormat) (alignment: int option) : JsExpr =
            match fmt with
            // `%d`/`%s`/`%O`/`%c`/`%M`: plain stringification. Bare ⇒ the raw operand
            // (the surrounding concat coerces it, a lone `%d` stays `console.log(x)`);
            // with a width ⇒ `String(v)` then pad.
            | FieldFormat.Verbatim ->
                match alignment with
                | None -> buildExpr ctx operand
                | Some _ -> withAlign alignment (direct (fun v -> call (id "String") [ v ]))
            // `%0wd`: sign-aware zero-pad — zeros pad to `width` *after* the sign
            // (`(-42).ToString("D5") = "-00042"`), so the value is read three times.
            | FieldFormat.DecimalZeroPad width ->
                iife (fun v ->
                    let signStr =
                        JsExpr.Conditional(JsExpr.Binary("<", v, num 0, ValueNone), str "-", str "", ValueNone)

                    let digits =
                        invoke
                            (invoke (invoke (id "Math") "abs" [ v ]) "toString" [])
                            "padStart"
                            [ num width; str "0" ]

                    JsExpr.Binary("+", signStr, digits, ValueNone)
                )
            // `%x`/`%X`/`%B`/`%o`: `(v >>> 0).toString(base)` — `>>> 0` is JS's 32-bit
            // unsigned coercion (the CLI-stack reinterpret on CLR), then optional
            // upper-casing and zero-pad. `%o` never zero-pads (`zeroPad = None`).
            | FieldFormat.IntRadix(radix, zeroPad) ->
                let baseN, upper =
                    match radix with
                    | Radix.Hex u -> 16, u
                    | Radix.Binary -> 2, false
                    | Radix.Octal -> 8, false

                withAlign
                    alignment
                    (direct (fun v ->
                        let digits =
                            invoke (JsExpr.Binary(">>>", v, num 0, ValueNone)) "toString" [ num baseN ]

                        let cased = if upper then invoke digits "toUpperCase" [] else digits

                        match zeroPad with
                        | Some w -> invoke cased "padStart" [ num w; str "0" ]
                        | None -> cased
                    ))
            // `%u`: the source `int`'s bits reinterpreted unsigned (`>>> 0`).
            | FieldFormat.Unsigned ->
                withAlign alignment (direct (fun v -> invoke (JsExpr.Binary(">>>", v, num 0, ValueNone)) "toString" []))
            // `%b`: lowercase `true`/`false` (explicit ternary keeps the alignment path uniform).
            | FieldFormat.Bool ->
                withAlign alignment (direct (fun v -> JsExpr.Conditional(v, str "true", str "false", ValueNone)))
            // `%f` / `%.Nf`: fixed-point with `precision` fraction digits.
            | FieldFormat.Fixed precision ->
                withAlign alignment (direct (fun v -> invoke (receiver v) "toFixed" [ num precision ]))
            // `%0w.Nf`: fixed-point, then zeros after any sign to a total field of `width`.
            | FieldFormat.FixedZeroPad(precision, width) ->
                withAlign
                    alignment
                    (strBind
                        (direct (fun v -> invoke (receiver v) "toFixed" [ num precision ]))
                        (fun s ->
                            let padTail =
                                JsExpr.Binary(
                                    "+",
                                    str "-",
                                    invoke (invoke s "slice" [ num 1 ]) "padStart" [ num (width - 1); str "0" ],
                                    ValueNone
                                )

                            JsExpr.Conditional(
                                invoke s "startsWith" [ str "-" ],
                                padTail,
                                invoke s "padStart" [ num width; str "0" ],
                                ValueNone
                            )
                        ))
            // `%+d`/`% d`/`%+.Nf`/`% .Nf`: forced sign — a non-negative value takes the
            // sign char (`+` or a space), a negative keeps its `-`. `precision = 0` ⇒
            // integer (`toFixed(0)`).
            | FieldFormat.ForcedSign(space, precision) ->
                let sign = if space then " " else "+"

                withAlign
                    alignment
                    (strBind
                        (direct (fun v -> invoke (receiver v) "toFixed" [ num precision ]))
                        (fun s ->
                            JsExpr.Conditional(
                                invoke s "startsWith" [ str "-" ],
                                s,
                                JsExpr.Binary("+", str sign, s, ValueNone),
                                ValueNone
                            )
                        ))
            // `%e`/`%E`: scientific notation via `v.toExponential(precision)`. JS uses
            // a lowercase `e` and a minimal (1-2 digit) exponent, so this is NOT byte-
            // identical to F#/.NET (which zero-pads the exponent to 3 digits,
            // `1.234500e+004`); it's an accepted close approximation. `%E` upper-cases
            // the `e` (only letter in the string, so `toUpperCase` is safe).
            | FieldFormat.Exponential(precision, upper) ->
                withAlign
                    alignment
                    (direct (fun v ->
                        let e = invoke (receiver v) "toExponential" [ num precision ]
                        if upper then invoke e "toUpperCase" [] else e
                    ))
            // `%g`/`%G`: compact form via `v.toPrecision(significant)`. JS `toPrecision`
            // keeps trailing zeros and switches to exponential on different thresholds
            // than .NET `G`, so again an accepted approximation, not byte-exact.
            // `toPrecision` requires ≥ 1 significant digit, so clamp (a `%.0g` would
            // otherwise throw a RangeError at runtime).
            | FieldFormat.Compact(precision, upper) ->
                let sig' = max 1 precision

                withAlign
                    alignment
                    (direct (fun v ->
                        let g = invoke (receiver v) "toPrecision" [ num sig' ]
                        if upper then invoke g "toUpperCase" [] else g
                    ))

        match hole.Source with
        // A `{x:fmt}` interpolation custom-format clause: a CLR dialect string with no
        // printf placeholder, which JS does not interpret — the raw operand stands.
        | HoleSpecSource.RawFormat _ -> buildExpr ctx operand
        | HoleSpecSource.Classified(HoleForm.PercentA(width, size)) ->
            let fmtRef =
                // A Vesper-emitted runtime export — always a NAMED import.
                JsExpr.Identifier(
                    JsImports.addRef ctx.Imports "structuralFormat" structuralFormatKey ImportForm.Named,
                    ValueNone
                )

            let value = buildExpr ctx operand
            call fmtRef [ value; num (percentAWidth width); num (percentASize size) ]
        | HoleSpecSource.Classified(HoleForm.Field(fmt, alignment)) -> emitField fmt alignment

    /// Compile a pattern against a pure scrutinee-access expression `access` into a
    /// refutability test (`None` ⇒ irrefutable) and the `const` bindings its named
    /// sub-patterns introduce. `access` must be pure — it is duplicated across test
    /// and bindings; the short-circuit ensures a sub-field is read only after its tag matched.
    and private compileMatchPattern
        (ctx: WalkCtx)
        (access: JsExpr)
        (pat: Frozen.TPat)
        : JsExpr option * JsStatement list =
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

    /// Build one `match` arm's statements: when the pattern matches (and the guard,
    /// if any, passes) the arm `return`s its body. An always-matching arm
    /// (wildcard / bare variable, `test = None`) emits a bare `Block` so its
    /// bindings stay scoped (two arms may bind the same source name); a refutable
    /// arm guards that block with `if (test)`.
    and private buildMatchArm (ctx: WalkCtx) (access: JsExpr) (arm: Frozen.TMatchArm) : JsStatement list =
        let test, binds = compileMatchPattern ctx access arm.Pat

        let inner =
            match arm.Guard with
            | None -> binds @ [ JsStatement.Return(buildExpr ctx arm.Body) ]
            | Some g ->
                binds
                @ [
                    JsStatement.If(buildExpr ctx g, [ JsStatement.Return(buildExpr ctx arm.Body) ], [])
                ]

        match test with
        | None -> [ JsStatement.Block inner ]
        | Some t -> [ JsStatement.If(t, inner, []) ]

    /// The body of a function whose params are `names`: a `while (true)` trampoline
    /// (`buildTailBody`) when `selfKey` names the binding and its body makes a saturated
    /// tail self-call at `arity` (constant-stack recursion), else the plain expression.
    /// `arity` is the SOURCE-group count — for a flat module fn it differs from
    /// `names.Length` (tuple groups expand, lone unit erases), so the caller passes it.
    and private trampolineOrExpr
        (ctx: WalkCtx)
        (selfKey: NodeKey voption)
        (arity: int)
        (names: string list)
        (body: Frozen.TExpr)
        : JsFnBody =
        match selfKey with
        | ValueSome k when hasTailSelfCall k arity body ->
            JsFnBody.Block
                [
                    JsStatement.While(JsExpr.Literal(JsLiteral.Boolean true, ValueNone), buildTailBody ctx k names body)
                ]
        | _ -> JsFnBody.Expr(buildExpr ctx body)

    /// Emit a function value as nested *unary* arrows. When `selfKey` names the
    /// binding and its body makes a saturated tail self-call, the innermost arrow
    /// becomes a `while (true)` trampoline for constant-stack recursion; else the
    /// innermost body is the plain expression. The nested-unary shape keeps every
    /// arrow's param in scope at the innermost body, which is what lets the trampoline
    /// write them back and `continue`.
    and emitFunction (ctx: WalkCtx) (selfKey: NodeKey voption) (lam: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok lam)
        let names, body = peelArrow ctx.Source lam
        nestUnaryArrows loc names (trampolineOrExpr ctx selfKey (List.length names) names body)

    /// Build the statements of a self-tail-call trampoline's loop body, walking
    /// tail position. A saturated tail self-call writes its arguments back to the
    /// parameter variables — through per-argument temporaries first, so an
    /// argument that reads a parameter (`sum (n-1) (acc+n)`) sees the *old* value
    /// — then `continue`s. Tail `if`/`let`/`Sequential`-tail thread through;
    /// every other tail expression `return`s its value.
    and buildTailBody (ctx: WalkCtx) (selfKey: NodeKey) (paramNames: string list) (e: Frozen.TExpr) : JsStatement list =
        let arity = List.length paramNames
        let recur = buildTailBody ctx selfKey paramNames

        match e with
        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            [ JsStatement.If(buildExpr ctx cond, recur thenE, recur elseE) ]
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value && not (isAssignedIn k body) ->
            recur (substVar k value body)
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let name = identName ctx.Source k
            let init = buildExpr ctx value

            let binding =
                if isAssignedIn k body then
                    JsStatement.Let(name, init)
                else
                    JsStatement.Const(name, init)

            binding :: recur body
        // `let _ = value in body` — discard the value (effects only); body stays in tail
        // position. A pure value drops away (see `buildExpr`).
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) when isPureValue value -> recur body
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) -> buildStatements ctx value @ recur body
        | TExprG.Sequential(xs, _, _) when xs.Length > 0 ->
            let items = EqArray.toList xs
            let init = items.[.. items.Length - 2]
            let last = items.[items.Length - 1]
            (init |> List.collect (buildStatements ctx)) @ recur last
        | TailSelfCall selfKey arity args ->
            // `_tc<i>` temporaries: evaluate every new argument before any write-back,
            // so a self-call arg that mentions a parameter reads its pre-iteration
            // value. (Not collision-proof against a source param literally named
            // `_tc0` — synthetic names are keyed off strings, not `NodeKey`s.)
            let tmp i = "_tc" + string i

            [ for i, a in List.indexed args -> JsStatement.Const(tmp i, buildExpr ctx a) ]
            @ [
                for i, name in List.indexed paramNames -> JsStatement.Assign(name, JsExpr.Identifier(tmp i, ValueNone))
            ]
            @ [ JsStatement.Continue ]
        | _ -> [ JsStatement.Return(buildExpr ctx e) ]

    /// Curry `base` over `args` — one unary `Call` per argument, in source order
    /// (`base(a)(b)…`). Shared by the `MethodCall` / `StaticMethodCall` lowerings.
    and private applyArgs (ctx: WalkCtx) (baseExpr: JsExpr) (args: EqArray<Frozen.TExpr>) : JsExpr =
        EqArray.toList args
        |> List.fold (fun acc a -> JsExpr.Call(acc, [ buildExpr ctx a ], ValueNone)) baseExpr

    /// Emit a local module FUNCTION as one FLAT arrow over its compiled parameters
    /// (`let f x y` → `(x, y) => …`; tuple groups flattened, a lone unit erased to
    /// `() => …`). When every group is a plain binder and the body makes a saturated
    /// tail self-call, the body becomes a `while (true)` trampoline — the flat
    /// parameters are the mutated slots (only the all-`GSimple` shape maps a self-call's
    /// spine one-to-one onto them).
    and private emitFlatModuleFn
        (ctx: WalkCtx)
        (k: NodeKey)
        (cf: CompiledFns.CompiledFn)
        (loc: JsLoc voption)
        : JsExpr =
        let names = [ for p in cf.Params -> JsFlatFns.paramNameOf ctx.Source p ]

        // Only the all-`GSimple` shape maps a self-call's spine one-to-one onto the flat
        // params, so the trampoline is gated on it; otherwise no self-key is offered.
        let selfKey =
            if TastLower.allSimpleGroups cf.Groups then
                ValueSome k
            else
                ValueNone

        JsExpr.Arrow(names, trampolineOrExpr ctx selfKey (List.length cf.Groups) names cf.Body, loc)

    /// Emit a record/union member as a free, curried, receiver-first top-level function:
    /// `member this.Foo a b` → `<Type>__Foo = (this$) => (a) => (b) => <body>`.
    /// Static members drop the receiver; a static property emits as a plain value binding.
    and emitMemberFn (ctx: WalkCtx) (typeName: string) (m: Frozen.TTypeMember) : JsStatement =
        let isProperty = (m.Kind = TMemberKind.Property)
        let name = JsExternalMembers.mangledName typeName m.IsStatic isProperty m.Name

        let receiverNames =
            if m.IsStatic then
                []
            else
                match m.ThisKey with
                | ValueSome k -> [ identName ctx.Source k ]
                | ValueNone -> [ "this$" ]

        let paramNames = [ for (pk, _) in m.Params -> identName ctx.Source pk ]
        let allNames = receiverNames @ paramNames
        let body = buildExpr ctx m.Body

        let init =
            match allNames with
            | [] -> body
            | _ -> nestUnaryArrows ValueNone allNames (JsFnBody.Expr body)

        topLevelBinding ctx name init

    /// Re-bind a member's receiver source-name binder (`m.ThisKey`) to JS `this` via a
    /// leading `const`, leaving the body's `TExpr.Var(thisKey)` references intact. Empty
    /// when the binder already resolves to `this` (avoids a no-op `const this = this;`)
    /// or the member is static.
    and thisBinding (ctx: WalkCtx) (m: Frozen.TTypeMember) : JsStatement list =
        match m.ThisKey with
        | ValueSome k ->
            let recvName = identName ctx.Source k

            if recvName = "this" then
                []
            else
                [ JsStatement.Const(recvName, JsExpr.Identifier("this", ValueNone)) ]
        | ValueNone -> []

    /// Emit a plain (non-generator) ATTACHED instance method: receiver bound to JS
    /// `this` (not a curried param), the member's params curried-free, body returned.
    /// The runtimes dispatch by method presence — `Vesper.Core.eq` calls `a.Equals(b)`,
    /// `Vesper.Comparison.cmp` calls `a.CompareTo(b)`, `Vesper.Core.hashOf` calls
    /// `x.GetHashCode()`, `use` calls `obj[Symbol.dispose]()` — so a capability impl's
    /// slot IS its member body; only the `key` (a plain `Named`, a `Symbol.dispose`
    /// member-access, or a `Symbol.for("vesper.X")` registry call) tells them apart.
    and emitPlainMethod (ctx: WalkCtx) (key: JsMethodKey) (m: Frozen.TTypeMember) : JsClassMethod =
        {
            Key = key
            Params = [ for (pk, _) in m.Params -> identName ctx.Source pk ]
            Body = thisBinding ctx m @ [ JsStatement.Return(buildExpr ctx m.Body) ]
            Generator = false
        }

    /// An interface-impl / `Object`-override member (`Equals`/`CompareTo`/`GetHashCode`
    /// or a user interface method) as a name-keyed attached method.
    and emitAttachedMethod (ctx: WalkCtx) (m: Frozen.TTypeMember) : JsClassMethod =
        emitPlainMethod ctx (JsMethodKey.Named m.Name) m

    /// `receiver.<member>` for a call dispatched through a local interface slot — the
    /// member resolves to the attached method `partitionClassMembers` emitted on the
    /// receiver's class. Shared by the `PropertyGet`/`MethodCall` `CallVia.Interface` arms.
    and attachedAccess (ctx: WalkCtx) (loc: JsLoc voption) (receiver: Frozen.TExpr) (key: SymbolKey) : JsExpr =
        JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(SymbolKeyOps.simpleName key, ValueNone), false, loc)

    /// Emit an enumerable-capability `GetEnumerator` impl as a native
    /// `*[Symbol.iterator]()` GENERATOR — the JS realisation of "implement `seq<'T>`
    /// ⇒ emit the target iteration protocol". The generator binds the enumerator the
    /// impl returns (`const e = <GetEnumerator body>`, with `this` re-bound via
    /// `thisBinding`), then drives the F# enumerator protocol (`MoveNext(): bool` +
    /// `Current`) into JS's: `while (e.MoveNext()) yield e.Current()`. `yield` makes the
    /// protocol adaptation free — it auto-produces the `{ value, done }` iterator
    /// results, so no object literal is built. The enumerator is itself an
    /// `IEnumerator<'T>` implementer, so its `MoveNext`/`Current` are ATTACHED JS methods
    /// (`e.MoveNext()` / `e.Current()`), dispatched directly on the runtime object — not
    /// the free receiver-first form a regular member call lowers to.
    and emitIteratorMethod (ctx: WalkCtx) (m: Frozen.TTypeMember) : JsClassMethod =
        // A fresh enumerator binder, keyed on the body token so it can't shadow a
        // source binder the `GetEnumerator` body itself introduces.
        let eName = "_e" + string (TastWalk.exprTok m.Body).StartIndex
        let eIdent = JsExpr.Identifier(eName, ValueNone)

        // Direct attached calls on the enumerator object: `e.MoveNext()` / `e.Current()`
        // (its `IEnumerator<'T>` impl members are attached methods, the property `Current`
        // emitted as a zero-arg method).
        let attachedCall (name: string) =
            JsExpr.Call(JsExpr.Member(eIdent, JsExpr.Identifier(name, ValueNone), false, ValueNone), [], ValueNone)

        let body =
            thisBinding ctx m
            @ [
                JsStatement.Const(eName, buildExpr ctx m.Body)
                JsStatement.While(attachedCall "MoveNext", [ JsStatement.Yield(attachedCall "Current") ])
            ]

        {
            // `Symbol.iterator` — a native well-known symbol, distinct from a registry
            // `Symbol.for("…")` call (the eq/comp/hash sub-slice).
            Key = JsMethodKey.Computed(nativeSymbol "iterator")
            Params = []
            Body = body
            Generator = true
        }

    /// Emit a disposable-capability `Dispose` impl as a NATIVE well-known
    /// `[Symbol.dispose]()` method — the JS analogue of the CLR `IDisposable::Dispose`
    /// slot, driven by `use`'s `obj[Symbol.dispose]()` lowering. A plain
    /// (non-generator) method keyed by the `Symbol.dispose` member-access node, NOT a
    /// `Symbol.for("…")` registry call.
    and emitDisposeMethod (ctx: WalkCtx) (m: Frozen.TTypeMember) : JsClassMethod =
        emitPlainMethod ctx (JsMethodKey.Computed symbolDispose) m

    /// Emit an eq/comp/hash capability impl as a COMPUTED-KEY method
    /// `[Symbol.for("vesper.X")](params) { … }` — the registry-symbol dispatch slot the
    /// `Vesper.Core` / `Vesper.Comparison` runtimes look for (`a[Symbol.for("vesper.equality")](b)`,
    /// `a[Symbol.for("vesper.comparison")](b)`, `x[Symbol.for("vesper.hash")]()`). A registry
    /// symbol is present ONLY on a type that opted into the protocol, so it can't collide with a
    /// foreign object carrying an unrelated `.Equals`/`.CompareTo`/`.GetHashCode`. `registryName`
    /// is the registry key (`vesper.equality` etc.).
    and emitProtocolMethod (ctx: WalkCtx) (registryName: string) (m: Frozen.TTypeMember) : JsClassMethod =
        emitPlainMethod ctx (JsMethodKey.Computed(registrySymbol registryName)) m

    /// A value bound to a name (a module value, or a `let` binder). A `Lambda`
    /// value routes through `emitFunction` carrying its binder key, so a
    /// recursive binding (`let rec`) can recognise its own tail calls; any other
    /// value is a plain `buildExpr`.
    and emitBound (ctx: WalkCtx) (k: NodeKey) (value: Frozen.TExpr) : JsExpr =
        match value with
        | TExprG.Lambda _ -> emitFunction ctx (ValueSome k) value
        | _ -> buildExpr ctx value

    /// An expression in statement position. `Sequential` flattens; a `let` binder
    /// becomes a `const`; anything else is one `ExpressionStatement`.
    and buildStatements (ctx: WalkCtx) (e: Frozen.TExpr) : JsStatement list =
        match e with
        | TExprG.Sequential(xs, _, _) ->
            [
                for x in xs do
                    yield! buildStatements ctx x
            ]
        // Pure, immutable binder: substitute away so synthetic operand lets don't
        // surface as `const`s. A mutable binder is excluded (see `buildExpr`).
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value && not (isAssignedIn k body) ->
            buildStatements ctx (substVar k value body)
        // A mutable binder emits a reassignable `let`; an immutable one a `const`.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let name = identName ctx.Source k
            let init = emitBound ctx k value

            let binding =
                if isAssignedIn k body then
                    JsStatement.Let(name, init)
                else
                    JsStatement.Const(name, init)

            binding :: buildStatements ctx body
        // `let _ = value in body` — emit the discarded value as its own statement(s)
        // (effects only), then the body. A pure value drops away (see `buildExpr`).
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) when isPureValue value -> buildStatements ctx body
        | TExprG.Let(TPatG.Wildcard _, value, body, _, _) -> buildStatements ctx value @ buildStatements ctx body
        // `while cond do body` as a bare loop statement (no IIFE wrapper needed here).
        | TExprG.While(cond, body, _, _) -> [ JsStatement.While(buildExpr ctx cond, buildStatements ctx body) ]
        // `for i = a to b do body` — F# evaluates `b` once, so hoist the limit into a
        // `const` before the loop; the JS `for` then counts `i` from `a` up to that
        // limit inclusive. (JS numbers are doubles, so the CLR overflow-at-MaxValue
        // dance the IL backend needs is unnecessary — `i <= limit` is safe.)
        | TExprG.ForTo(var, startExpr, endExpr, body, _, _) ->
            let name = identName ctx.Source var
            let limit = "_lim" + string (TastWalk.exprTok e).StartIndex

            [
                JsStatement.Const(limit, buildExpr ctx endExpr)
                JsStatement.For(
                    name,
                    buildExpr ctx startExpr,
                    JsExpr.Identifier(limit, ValueNone),
                    buildStatements ctx body
                )
            ]
        // `for x in source do body` — lower to a JS `for…of`, which drives the source's
        // own `Symbol.iterator` at runtime. Only the `Interface` enumerator (a source
        // typed `IEnumerable<'T>`) reaches JS codegen, and it carries no member keys (the
        // CLR backend mints the `IEnumerator` interface slots itself; JS defers to the
        // iterator protocol), so there is nothing to resolve — `for…of` over the source is
        // the whole lowering. A duck-typed `Pattern` enumerator never type-checks against
        // the BCL-free JS provider, so it is unsupported here.
        | TExprG.ForIn(pat, source, body, enumerator, _ty, _tok) ->
            match enumerator with
            | ForInEnumeratorG.Interface ->
                let name = patBinderName ctx "_forin" pat
                [ JsStatement.ForOf(name, buildExpr ctx source, buildStatements ctx body) ]
            | ForInEnumeratorG.Pattern _ ->
                failwith
                    "EmitJs: duck-typed `for...in` (Pattern enumerator) is unsupported on JS; only IEnumerable<'T> sources lower to `for...of`"
        // `use x = value in body` — park the binder in a `const`, run the body inside a
        // `try`, and dispose the binder in the `finally` (the IL backend's exception
        // region, lowered to JS `try/finally`). The body keeps statement position.
        | TExprG.Use(binding, value, body, dispose, _ty, _tok) ->
            let name = useBinderName ctx binding

            [
                JsStatement.Const(name, buildExpr ctx value)
                JsStatement.TryFinally(buildStatements ctx body, disposeStmts ctx dispose name)
            ]
        | _ -> [ JsStatement.Expression(buildExpr ctx e) ]

    /// The JS binder name for a single-binder loop/scope pattern (`use x = …`,
    /// `for x in …`). A wildcard binder has no source name, so it gets a fresh
    /// `<prefix><tok>` slot — the value is still bound (parked/iterated) even though
    /// the body can't name it. Only simple/wildcard binders are supported; a
    /// destructuring binder (e.g. a tuple pattern) is rejected.
    and private patBinderName (ctx: WalkCtx) (prefix: string) (binding: Frozen.TPat) : string =
        match binding with
        | TPatG.NamedSimple(k, _, _) -> identName ctx.Source k
        | TPatG.Wildcard(_, tok) -> prefix + string tok.StartIndex
        | other -> failwithf "EmitJs: unsupported single binder pattern %A" other

    and private useBinderName (ctx: WalkCtx) (binding: Frozen.TPat) : string = patBinderName ctx "_use" binding

    /// The `finally` body that disposes a `use` binder: a null-guarded disposal call.
    /// F# `use` is null-safe — JS loose `!= null` catches both `null` and `undefined`
    /// (matching the `Null` pattern convention). Under the §3b disposal-model flip, a
    /// project-local `use` binder always implements `disposable` (the front end records
    /// `ValueNone`), so disposal is the native `binder[Symbol.dispose]()` — the same
    /// `Symbol.dispose` member-access node the disposable impl emits its method under
    /// (`emitDisposeMethod`). The `ValueSome key` path (the ref-struct carve-out / an
    /// external own-`Dispose`) keeps calling the keyed member's free receiver-first fn.
    and private disposeStmts (ctx: WalkCtx) (dispose: SymbolKey voption) (name: string) : JsStatement list =
        let binder = JsExpr.Identifier(name, ValueNone)

        let guard =
            JsExpr.Binary("!=", binder, JsExpr.Identifier("null", ValueNone), ValueNone)

        let disposeCall =
            match dispose with
            // Ref-struct carve-out / external own-`Dispose`: the front end resolved a
            // keyed `Dispose` member; call its free receiver-first function.
            | ValueSome key ->
                let disposeFn = Members.localFn ctx key false false ValueNone
                JsExpr.Call(disposeFn, [ binder ], ValueNone)
            // Implements `disposable`: lower to the native `binder[Symbol.dispose]()`
            // (a COMPUTED member access on the well-known symbol), with no args.
            | ValueNone -> JsExpr.Call(JsExpr.Member(binder, symbolDispose, true, ValueNone), [], ValueNone)

        [ JsStatement.If(guard, [ JsStatement.Expression disposeCall ], []) ]

    /// `finishOps` knob for JS: identity — operators are already `$N`-templates pre-freeze.
    let private jsFinishOps (e: Frozen.TExpr) : Frozen.TExpr = e

    /// Emit every class-method form of a partitioned member set, in the one place the
    /// partition→emitter mapping lives: attached dispatch slots, `[Symbol.iterator]`
    /// generators, `[Symbol.for("vesper.X")]` protocol methods, and the
    /// `[Symbol.dispose]()` method. (`Free` members are emitted elsewhere as free
    /// functions.) Shared by the pending-class and pending-union emission.
    let private emitCapabilityMethods (ctx: WalkCtx) (p: PartitionedMembers) : JsClassMethod list =
        [
            for m in p.Attached -> emitAttachedMethod ctx m
            for m in p.Iterators -> emitIteratorMethod ctx m
            for (sym, m) in p.Protocols -> emitProtocolMethod ctx sym m
            for m in p.Disposers -> emitDisposeMethod ctx m
        ]

    /// The whole frozen file → a `Program`. Type declarations become JS `class`es first
    /// (classes are not hoisted); remaining decls are lowered — `let inline` templates
    /// and `type` decls drop out, leaving module values and effectful expressions.
    let buildProgram (ctx0: WalkCtx) (tast: Frozen.TastFile) : JsProgram =
        // The language-capability identities, resolved through the provider — drives the
        // `seq<'T>`-impl → `[Symbol.iterator]` routing in `partitionClassMembers`. A
        // provider-less compile (`nullProvider`) names no capability.
        let caps =
            match ctx0.Provider with
            | ValueSome provider -> ExternalSymbols.resolveCapabilities provider
            | ValueNone -> RuntimeNames.CapabilityIds.none

        let collected = collectTypes caps ctx0.ExportTopLevel tast

        let lowered = TastLower.lower jsFinishOps tast.Decls

        // The top-level module functions and their flat compiled form — the same
        // `Codegen.Common.CompiledFns` analysis the CLR backend reads. Drives the FLAT
        // (Fable-style) emission of every module function and the spine-collapsing of
        // its saturated call sites.
        let compiledFns =
            System.Collections.Generic.Dictionary<NodeKey, CompiledFns.CompiledFn>()

        for f in CompiledFns.gather lowered do
            compiledFns.[f.Key] <- f

        // The file's locally-declared interface keys — drives the attached-method
        // dispatch of a `(r :> ILocal).M()` / `.Prop` access (see `WalkCtx.LocalInterfaces`).
        let localInterfaces = System.Collections.Generic.HashSet<SymbolKey>()

        for decl in tast.Decls do
            match decl with
            | TDeclG.Type({ Kind = TTypeKindG.Interface _ } as td) -> localInterfaces.Add td.Key |> ignore
            | _ -> ()

        let ctx =
            { ctx0 with
                Records = collected.Records
                Unions = collected.Unions
                Classes = collected.Classes
                Enums = collected.Enums
                CompiledFns = compiledFns
                LocalInterfaces = localInterfaces
            }

        // Class decls (with their attached instance methods) are built now — their
        // method bodies need the full ctx, unlike record/union decls which carry no
        // bodies. They join the record/union decls ahead of members and the body.
        let classDecls =
            [
                for pc in collected.PendingClasses ->
                    JsStatement.Class(pc.Name, pc.Fields, emitCapabilityMethods ctx pc.Members, ctx.ExportTopLevel)
            ]

        // Unions whose interface impls became base-class methods are built now too —
        // their `[Symbol.iterator]` / protocol bodies need the full ctx (the method
        // bodies may `new` a class), exactly like the pending classes. They render
        // base-first so the case subclasses inherit the protocol members.
        let pendingUnionDecls =
            [
                for pu in collected.PendingUnions ->
                    JsStatement.Union(
                        pu.Name,
                        pu.Brand,
                        pu.Cases,
                        emitCapabilityMethods ctx pu.Members,
                        ctx.ExportTopLevel
                    )
            ]

        // Member functions emitted after the class decls (they reference the classes
        // via `new`/match, and `const` arrows are not hoisted) and before the body.
        let memberDecls =
            [ for (typeName, m) in collected.Members -> emitMemberFn ctx typeName m ]

        let body =
            [
                for decl in lowered do
                    match decl with
                    | TDeclG.Expression(e, _) -> yield! buildStatements ctx e
                    | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                        // A module FUNCTION emits FLAT (Fable-style); a plain value
                        // routes through `emitBound` (closures stay curried).
                        let init =
                            match ctx.CompiledFns.TryGetValue k with
                            | true, cf -> emitFlatModuleFn ctx k cf (locOf ctx (TastWalk.exprTok value))
                            | _ -> emitBound ctx k value

                        topLevelBinding ctx (identName ctx.Source k) init
                    | other -> failwithf "EmitJs: unsupported declaration %A" other
            ]

        // Imports lead the program; local class decls follow — classes are not hoisted
        // and must precede every `new`/match site. External union case classes are not
        // emitted here: a `UnionCons` imports them from the union's home module.
        {
            Body =
                JsImports.importStatements ctx.Imports
                @ collected.Decls
                @ classDecls
                @ pendingUnionDecls
                @ memberDecls
                @ body
        }
