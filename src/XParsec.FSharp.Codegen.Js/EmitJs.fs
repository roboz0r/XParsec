namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open JsEmitHelpers

/// The `TAST → JsAst` walker — the JS analogue of `Emit*` in `Codegen.Clr`, grown
/// one step at a time (codegen-js-steps.md; `EmitExpr.buildExpr` is the CLR
/// checklist each step tracks). Every un-handled node is an explicit `failwithf`,
/// so an unsupported arm fails loudly rather than dropping silently.
///
/// Durable conventions the arms assume:
///   * Functions are **curried unary arrows** — `Lambda` → nested `(a) => (b) => …`,
///     `App` → unary calls (`f a b` → `f(a)(b)`); no call-site arity analysis. Tail
///     self-recursion is trampolined to `while (true)` with param-shadow mutation
///     (`emitFunction` / `buildTailBody`) for constant stack.
///   * Operator bodies arrive pre-spliced as `ILIntrinsic` `$N`-templates (from
///     `ops-platform.js.fs` via `Passes.InlineExpansion`); the JS `finishOps` is
///     identity (JS keeps operators as emit-able templates). `TastLower.lower` drops
///     `type` decls, so record/union/member shapes are read off the *un-lowered*
///     decls (`collectTypes`).
///   * Records/unions emit as data-only JS `class`es (positional ctor; union = base
///     `tag` + one `extends`-subclass per case). Members emit as free, curried,
///     *receiver-first* functions under a mangled name (`module Members`), never
///     prototype methods — so the structural-interop invariant holds (match + the
///     structural runtime read `.tag`/own-keys, never `instanceof`).
///   * `Match` lowers to an IIFE testing each arm in order (`compileMatchPattern` →
///     `&&`-conjoined test + path-projection `const`s), an unmatched value `throw`ing.
module EmitJs =

    /// Maps a source char offset (a `SyntaxToken.StartIndex`) to 0-based
    /// (line, column) — V3 source-map coordinates. Built once per compile from
    /// the raw source text: `Starts.[n]` is the char offset at which line `n`
    /// begins. A trailing `\r` rides its line; columns count UTF-16 code units,
    /// which V3 maps require.
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

        /// Resolve a char offset to a `JsLoc`. Offsets past end-of-source clamp
        /// to the last line (defensive — a virtual token can anchor at the spawn
        /// offset, which is in range, but synthetic ends-of-input may not be).
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

    /// A record type's JS shape (Step 3): the emitted class `Name` and its `Fields`
    /// in *declaration* order — the authority `RecordCons`/`RecordClone` order their
    /// `new Name(…)` args against (the literal's source order is reordered to match).
    type JsRecordInfo = { Name: string; Fields: string list }

    /// A union type's JS shape (Step 4): the emitted base-class `Name` and its cases
    /// keyed by F# case name. `UnionCons` looks a case up to pick its subclass + field
    /// order; a union *pattern* to compare the scrutinee's `tag` and read fields.
    type JsUnionInfo =
        {
            Name: string
            Cases: System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>
        }

    /// The walker's ambient context: the source-map resolver, the raw source text
    /// (for recovering a `let`-bound variable's *source* name from its binder token
    /// offset), and the record table (`SymbolKey` → `JsRecordInfo`, for ordering
    /// `RecordCons` / `RecordClone` arguments against the class's positional
    /// constructor). The resolver/source come from `JsProjectInfo.Source` (absent
    /// it, maps are off and variable names fall back to `_v<offset>`); the record
    /// table is built from the file's type declarations by `buildProgram`.
    type WalkCtx =
        {
            Resolver: Resolver
            Source: string voption
            Records: System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>
            Unions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            /// The external-symbol provider (`Some` once a program references a
            /// library union/record). External union types — `Option`, `List` —
            /// are not in the file's `tast.Decls`, so their case shapes (tag +
            /// field names) are read off the provider on first use and emitted as
            /// honest nominal JS classes (Step 5), exactly the base-class +
            /// per-case-subclass shape a local union gets.
            Provider: IExternalSymbolProvider voption
            /// External unions resolved on demand during the walk, keyed by the
            /// same `SymbolKey` the `Unions` table uses. Shared mutable state: a
            /// miss in `Unions` falls back here, resolving + caching the shape and
            /// recording its emission order in `ExternalUnionDecls` so `buildProgram`
            /// can prepend the classes (JS classes are not hoisted).
            ExternalUnions: System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>
            /// The `Union` statements for the external unions resolved during the
            /// walk, in discovery order — `buildProgram` prepends them to the body
            /// (deterministic given a deterministic walk; JS classes are not hoisted).
            ExternalUnionDecls: ResizeArray<JsStatement>
            /// Runtime-module imports discovered during the walk (`addRef` /
            /// `addMemberRef`). `buildProgram` reads `importStatements` (the leading
            /// `import …` block); `Codegen.compileWith` reads `modules` off this same
            /// accumulator to materialise the runtime files.
            Imports: JsImports
            /// `true` in *library* mode: a top-level `let` emits `export const …` (a
            /// compiled runtime module's public surface); `false` (script default)
            /// keeps it a plain `const`. The single decision lives in `topLevelBinding`.
            ExportTopLevel: bool
        }

    let private locOf (ctx: WalkCtx) (tok: SyntaxToken) : JsLoc voption =
        match ctx.Resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    // ---- Records -------------------------------------------------------------

    /// The nominal `SymbolKey` of a record/union construct's receiver type — the key
    /// its `type` decl filled the lookup table under. `what` names the construct for
    /// the diagnostic; a non-nominal receiver is an invariant break.
    let private nominalKey (what: string) (ty: FrozenType) : SymbolKey =
        match TastLower.receiverShape ty with
        | ValueSome(key, _) -> key
        | ValueNone -> failwithf "EmitJs: %s on non-nominal type %A" what ty

    /// Resolve a `RecordCons` / `RecordClone` / `FieldGet` receiver type to its
    /// emitted `JsRecordInfo`. An absent entry means the record's `type` decl never
    /// reached this file.
    let private recordInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsRecordInfo =
        let key = nominalKey what ty

        match ctx.Records.TryGetValue key with
        | true, info -> info
        | _ -> failwithf "EmitJs (Step 3): %s on record with no emitted type (key %A)" what key

    // ---- Unions --------------------------------------------------------------

    /// A union case's declaration-order field names, to F#'s compiled convention: a
    /// *named* field keeps its name; a *positional* field becomes `Item` (lone) or
    /// `Item1`/`Item2`/… (several), so `UnionCons` args and a pattern's sub-patterns
    /// address the same properties. Shared by the local and external paths.
    let private synthFieldNames (fieldNames: string voption list) : string list =
        match fieldNames with
        | [ ValueSome n ] -> [ n ]
        | [ ValueNone ] -> [ "Item" ]
        | many ->
            many
            |> List.mapi (fun i nm ->
                match nm with
                | ValueSome n -> n
                | ValueNone -> "Item" + string (i + 1)
            )

    /// Build a `JsUnionInfo` (+ case-name → `JsUnionCaseDecl` table) for `baseName`
    /// whose cases are `(caseName, fieldNames)` in declaration order. Shared by the
    /// local (`collectTypes`) and external (`resolveExternalUnion`) emission: tag =
    /// declaration index, subclass = `<baseName>_<case>`, fields via `synthFieldNames`.
    let private buildUnionInfo
        (baseName: string)
        (cases: (string * string voption list) list)
        : JsUnionInfo * JsUnionCaseDecl list =
        let caseDecls =
            cases
            |> List.mapi (fun tag (caseName, fieldNames) ->
                {
                    CaseName = caseName
                    ClassName = baseName + "_" + caseName
                    Tag = tag
                    Fields = synthFieldNames fieldNames
                }
            )

        let table = System.Collections.Generic.Dictionary<string, JsUnionCaseDecl>()

        for c in caseDecls do
            table.[c.CaseName] <- c

        { Name = baseName; Cases = table }, caseDecls

    /// Resolve an *external* union (`Option`, `List`) to a `JsUnionInfo`, reading its
    /// case shapes off the provider and emitting the same base+subclass shape a local
    /// union gets (queued in `ctx.ExternalUnionDecls`, cached in `ctx.ExternalUnions`
    /// so classes emit once). `ValueNone` with no provider or non-union type — the
    /// caller then fails loudly.
    let private resolveExternalUnion (ctx: WalkCtx) (key: SymbolKey) : JsUnionInfo voption =
        match ctx.ExternalUnions.TryGetValue key with
        | true, info -> ValueSome info
        | _ ->
            match ctx.Provider with
            | ValueNone -> ValueNone
            | ValueSome provider ->
                match ExternalSymbols.tryLookupType provider key with
                | ValueSome(ExternalTypeShape.Union(_, cases, _)) ->
                    let baseName = SymbolKeyOps.simpleName key

                    let info, caseDecls =
                        buildUnionInfo baseName [ for c in cases -> c.Name, List.ofArray c.FieldNames ]

                    ctx.ExternalUnions.[key] <- info
                    ctx.ExternalUnionDecls.Add(JsStatement.Union(baseName, caseDecls))
                    ValueSome info
                | _ -> ValueNone

    /// Resolve a `UnionCons` / union-pattern receiver type + case name to the
    /// emitted `JsUnionCaseDecl` (subclass name, tag, field names). A type not in
    /// the file's own `Unions` table is resolved as an *external* union (`Option`,
    /// `List`) through the provider; an absent case — or a type that resolves to
    /// neither — is an invariant break (the front end admitted a construct the
    /// declaration lacks).
    let private unionInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsUnionInfo =
        let key = nominalKey what ty

        match ctx.Unions.TryGetValue key with
        | true, info -> info
        | _ ->
            match resolveExternalUnion ctx key with
            | ValueSome info -> info
            | ValueNone -> failwithf "EmitJs (Step 4): %s on union with no emitted type (key %A)" what key

    let private unionCaseOf (ctx: WalkCtx) (what: string) (ty: FrozenType) (caseName: string) : JsUnionCaseDecl =
        let info = unionInfoOf ctx what ty

        match info.Cases.TryGetValue caseName with
        | true, c -> c
        | _ -> failwithf "EmitJs (Step 4): %s on union '%s' has no case '%s'" what info.Name caseName

    // ---- Members -------------------------------------------------------------

    /// The member subsystem (Step 7): the name-mangling convention and key→name
    /// resolution shared by member emission (`emitMemberFn`), the call-site lowerings,
    /// and the deferred `.d.ts` emitter. The `buildExpr`-independent pieces — the
    /// lowerings themselves stay in the walker nest (they recurse through `buildExpr`).
    module private Members =

        /// The shared name-mangling convention, mirroring Fable's: instance method →
        /// `<Type>__<member>`; instance property getter → `<Type>__get_<Prop>`; static
        /// member → `<Type>_<member>`. Static property and static method share the
        /// single-underscore form (the call site distinguishes read vs apply).
        let mangledName (typeName: string) (isStatic: bool) (isProperty: bool) (memberName: string) : string =
            if isStatic then typeName + "_" + memberName
            elif isProperty then typeName + "__get_" + memberName
            else typeName + "__" + memberName

        /// The declaring type's `SymbolKey` from a member-call node's `key`. An
        /// `ExternalMember` `key` may be a non-member key (a static access folded to
        /// the type), so the fallthrough returns it verbatim.
        let declKey (key: SymbolKey) : SymbolKey =
            match key with
            | SymbolKey.MemberKey(decl, _, _, _) -> decl
            | _ -> key

        /// The home assembly of an *external* type, for selecting its `runtime-js`
        /// module. A nominal `TypeKey` still carries `asm = None` (codegen rekey
        /// pending), so the assembly is recovered off the provider's type-shape
        /// `origin`, falling back to the key's own assembly when present.
        let assemblyOf (ctx: WalkCtx) (key: SymbolKey) (what: string) : string =
            match SymbolKeyOps.keyAsm key with
            | Some a -> a
            | None ->
                let origin =
                    match ctx.Provider with
                    | ValueSome provider ->
                        match ExternalSymbols.tryLookupType provider key with
                        | ValueSome(ExternalTypeShape.Union(_, _, o))
                        | ValueSome(ExternalTypeShape.Record(_, _, o)) -> o.Assembly
                        | ValueSome(ExternalTypeShape.Class shape) -> shape.Origin.Assembly
                        | _ -> None
                    | ValueNone -> None

                match origin with
                | Some a -> a
                | None -> failwithf "EmitJs (Step 7): %s has no resolvable home assembly (key %A)" what key

        /// The emitted type name a member's mangled name is built from: the local
        /// `JsUnionInfo`/`JsRecordInfo` `Name` when the declaring type is in this file,
        /// else the key's bare simple name (an external type — the consumer half).
        let typeName (ctx: WalkCtx) (key: SymbolKey) : string =
            match ctx.Unions.TryGetValue key with
            | true, info -> info.Name
            | _ ->
                match ctx.Records.TryGetValue key with
                | true, info -> info.Name
                | _ -> SymbolKeyOps.simpleName key

        /// The callable identifier of a *local* member's emitted function, resolved off
        /// the call node's `key`. The single place type-name lookup + mangling compose,
        /// shared by all four local member-call arms.
        let localFn (ctx: WalkCtx) (key: SymbolKey) (isStatic: bool) (isProperty: bool) (loc: JsLoc voption) : JsExpr =
            let dk = declKey key
            JsExpr.Identifier(mangledName (typeName ctx dk) isStatic isProperty (SymbolKeyOps.simpleName key), loc)

    /// The fallthrough a `match` reaches when no arm matched — `throw new
    /// Error("…")`. An exhaustive match never reaches it at runtime, but it gives
    /// a non-exhaustive one defined behaviour (mirrors the CLR backend's
    /// `buildMatchFailure`).
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

    /// Emit a top-level `name = init` binding, picking the statement form from the
    /// compile mode: `export const` in *library* mode (a runtime module's public
    /// surface, Step 5b Phase 3), a plain `const` in *script* mode. The single home
    /// of the `ExportTopLevel` decision — every top-level binder (`emitMemberFn`,
    /// `buildProgram`'s module values) routes through here rather than re-branching.
    let private topLevelBinding (ctx: WalkCtx) (name: string) (init: JsExpr) : JsStatement =
        if ctx.ExportTopLevel then
            JsStatement.Export(name, init)
        else
            JsStatement.Const(name, init)

    /// Walk an external type's contract `inherit` chain (`ExternalClassShape.FrozenBaseType`,
    /// extracted Step 8) up to the `exn` intrinsic root, then resolve `exn`'s
    /// `(# "Error" #)` repr to the **native runtime class** (`JsNativeSymbols.Error`)
    /// via the shared `ExternalSymbols.tryRuntimeType` query and return its compiled
    /// name — `Error` on JS. A constructed exception (`raise (InvalidOperationException …)`)
    /// lowers to `new <class>(message)` sourced from this chain, NOT a hardcoded `"Error"`
    /// / an `EndsWith "Exception"` name heuristic (codegen-js-steps.md Step 8). The
    /// runtime-type resolution (the former second hop) now lives in `tryRuntimeType`
    /// (intrinsic-runtime-type-plan.md), so this is just the genuine subtype climb plus
    /// one shared call: `ValueNone` (→ caller fails loudly) when the type is not an `exn`
    /// subtype, when the repr names no provider class, or with no provider.
    let private exnReprOf (ctx: WalkCtx) (ty: FrozenType) : string voption =
        match ctx.Provider with
        | ValueNone -> ValueNone
        | ValueSome provider ->
            // Resolve a base-chain `FrozenType` node to its shape: a nominal key looks
            // up directly; a bare `FTConst` name (how an intrinsic base such as `exn`
            // freezes — `mkNominal` returns the short name for an `Intrinsic`) routes
            // through the shared runtime-type query (bare name + ambient prefixes).
            let shapeOf (ft: FrozenType) : ExternalTypeShape voption =
                match ft with
                | FTClass(key, _)
                | FTUnion(key, _)
                | FTRecord(key, _) -> ExternalSymbols.tryLookupType provider key
                | FTConst(name, _) -> ExternalSymbols.tryRuntimeType provider name
                | _ -> ValueNone

            // Bounded climb: every hop is a strict ancestor, so the chain is finite;
            // the depth cap only backstops a malformed cyclic `inherit`.
            let rec climb (depth: int) (ft: FrozenType) : string voption =
                if depth > 16 then
                    ValueNone
                else
                    match shapeOf ft with
                    | ValueSome(ExternalTypeShape.Intrinsic(platform = Some platform)) ->
                        // `exn`'s `platform` repr (`"Error"` on JS) names a native
                        // runtime type; resolve it to that class through the shared
                        // query so we emit the `JsNativeSymbols.Error` *definition*'s
                        // name (the provider is authoritative for the runtime type),
                        // not a bare repr string. We read the PLATFORM face, never
                        // `canon` (`"System.Exception"`) — that is the unifier's
                        // identity key and has no JS analogue. No provider class for
                        // the platform repr (or no repr at all on this target) ⇒
                        // `ValueNone` → fail loudly.
                        match ExternalSymbols.tryRuntimeType provider platform with
                        | ValueSome(ExternalTypeShape.Class _) -> ValueSome platform
                        | _ -> ValueNone
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        match shape.FrozenBaseType with
                        | ValueSome b -> climb (depth + 1) b
                        | ValueNone -> ValueNone
                    | _ -> ValueNone

            climb 0 ty

    // ---- The walker ----------------------------------------------------------

    let rec buildExpr (ctx: WalkCtx) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok e)

        match e with
        | TExprG.Const(value, _, _) -> constExpr value loc

        | TExprG.Var(k, _, _) -> JsExpr.Identifier(identName ctx.Source k, loc)

        // An external module function (`List.length`) — imported from its package's
        // JS runtime module (Step 5b). The reference is the import alias; `App`
        // saturates it (`List.map f xs` ≡ `$…map(f)(xs)`).
        | TExprG.External(compiledName, key, _, _) ->
            JsExpr.Identifier(JsImports.addRef ctx.Imports compiledName key, loc)

        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            JsExpr.Conditional(buildExpr ctx cond, buildExpr ctx thenE, buildExpr ctx elseE, loc)

        // A `Sequential` in expression position is a comma expression (top-level it
        // expands to statements via `buildStatements`).
        | TExprG.Sequential(xs, _, _) -> JsExpr.Sequence([ for x in xs -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]` (Step 5); a pattern reads each
        // element back by positional index.
        | TExprG.Tuple(items, _, _) -> JsExpr.Array([ for x in items -> buildExpr ctx x ], loc)

        // A pure `let` in expression position (the operand lets `InlineExpansion`
        // splices around an operator body) substitutes into its uses, collapsing back
        // to the clean template.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            buildExpr ctx (substVar k value body)

        // A non-pure `let` in expression position (e.g. `n * fact (n - 1)`). JS has no
        // let-expression, so it lowers to an IIFE `((x) => <body>)(<value>)` — the
        // binder evaluated once, then the body.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            let name = identName ctx.Source k

            JsExpr.Call(
                JsExpr.Arrow([ name ], JsFnBody.Expr(buildExpr ctx body), ValueNone),
                [ buildExpr ctx value ],
                loc
            )

        // An anonymous function value. Named bindings route through `emitBound`
        // (which knows the binder key, so it can recognise — and trampoline —
        // self-recursion); an anonymous lambda has no name to call itself by, so
        // no self-tail-call analysis applies.
        | TExprG.Lambda _ -> emitFunction ctx ValueNone e

        // Curried application: `f a b` emits one unary call per `App` (`f(a)(b)`) —
        // correct for saturated calls, partial application, and higher-order values
        // alike, since functions emit as nested unary arrows. (The flat-call
        // optimisation needs sound boundary curry/uncurry adaptation; deferred.)
        | TExprG.App(fn, arg, _, _) -> JsExpr.Call(buildExpr ctx fn, [ buildExpr ctx arg ], loc)

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

        // A union constructor `Case e0 e1 …` → `new <Union>_<Case>(args…)`. The
        // args already arrive in declaration (field) order, so — unlike a record
        // literal — no reordering is needed; the subclass constructor stores them
        // positionally under the case's field names.
        | TExprG.UnionCons(caseName, args, ty, _) ->
            let c = unionCaseOf ctx "UnionCons" ty caseName

            JsExpr.New(JsExpr.Identifier(c.ClassName, ValueNone), [ for a in args -> buildExpr ctx a ], loc)

        // Construction of an *external* exception (`raise (InvalidOperationException
        // msg)`) → `new <exn repr>(msg)`. The repr is sourced from the type's contract
        // `inherit` chain (`exnReprOf` walks to the `exn` intrinsic — `Error` on JS),
        // NOT a hardcoded `"Error"` or an `EndsWith "Exception"` name heuristic
        // (Step 8). Every `exn` subtype erases to the one `exn` root, carrying the
        // leading message arg; further args (`paramName`, …) have no `Error` slot and
        // drop. A non-`exn`-subtype external construction has no JS analogue and fails
        // loudly (project-local classes never reach `New` — records/unions use
        // `RecordCons`/`UnionCons`).
        | TExprG.New(className, args, ty, _) ->
            match exnReprOf ctx ty with
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

        // Member calls on a *local* record/union (Step 7): each member is a free,
        // curried, receiver-first function (`emitMemberFn`), so a call is just a
        // `Call` of `Members.localFn` — no `.member` access path. `applyArgs` curries
        // the (receiver-first) base over the args, matching the unary-arrow emission.
        | TExprG.PropertyGet(receiver, key, _, _, _) ->
            JsExpr.Call(Members.localFn ctx key false true ValueNone, [ buildExpr ctx receiver ], loc)

        | TExprG.MethodCall(receiver, key, _, args, _, _) ->
            let withRecv =
                JsExpr.Call(Members.localFn ctx key false false ValueNone, [ buildExpr ctx receiver ], loc)

            applyArgs ctx withRecv args

        | TExprG.StaticPropertyGet(key, _, _) ->
            // A static property reads the module-level value binding directly (no call).
            Members.localFn ctx key true true loc

        | TExprG.StaticMethodCall(key, args, _, _) -> applyArgs ctx (Members.localFn ctx key true false loc) args

        // A member on an *external* type (the consumer half) — `o.IsSome` where
        // `Option` is imported. The mangled member is imported from the declaring
        // type's `runtime-js` module (`JsImports.addMemberRef`, the member analogue
        // of the `External`-value `addRef` path), then applied receiver-first; an
        // instance method's arguments arrive through the enclosing `App` chain.
        | TExprG.ExternalMember(receiver, key, memberName, isProperty, _, _) ->
            let declKey = Members.declKey key
            let isStatic = (receiver = ValueNone)

            let exportName =
                Members.mangledName (SymbolKeyOps.simpleName declKey) isStatic isProperty memberName

            let asm = Members.assemblyOf ctx declKey (sprintf "external member '%s'" memberName)
            let local = JsImports.addMemberRef ctx.Imports asm exportName

            match receiver with
            | ValueSome r -> JsExpr.Call(JsExpr.Identifier(local, ValueNone), [ buildExpr ctx r ], loc)
            | ValueNone -> JsExpr.Identifier(local, loc)

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

        | TExprG.ILIntrinsic(opCode, _, args, _, _) -> JsExpr.Raw(expandTemplate ctx opCode (EqArray.toList args), loc)

        | TExprG.Format(sink, segments, _, _) ->
            let arg = buildFormatArg ctx segments

            match sink with
            // `console.log` / `console.error` append the trailing newline
            // themselves, matching `printfn` / `eprintfn`. The no-newline
            // `printf` / `eprintf` sinks (a `process.stdout.write`) await a later
            // step, as do the `sprintf` (`ToString`) / `fprintf` (`ToWriter`) sinks.
            | FormatSinkG.ToStdOut true -> JsExpr.Call(console "log", [ arg ], loc)
            | FormatSinkG.ToStdErr true -> JsExpr.Call(console "error", [ arg ], loc)
            | other -> failwithf "EmitJs (Step 1): unsupported format sink %A" other

        | other -> failwithf "EmitJs (Step 1): unsupported expression %A" other

    and private console (method: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("console", ValueNone), JsExpr.Identifier(method, ValueNone), false, ValueNone)

    /// Expand a `$N` JS-expression template (the `ILIntrinsic` opCode) into
    /// `JsRawSeg`s: verbatim chunks interleaved with the operand expressions the
    /// `$N` holes index (zero-based, source order). `$$` is a literal `$`. A CLR
    /// CIL mnemonic that slipped through (no `$` hole though operands exist) is a
    /// hard error — only `$N` templates may reach the JS backend (F0).
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

        // A template carrying operands but no hole is a bare CIL mnemonic
        // (`ceq`, `add`) that escaped the CLR-only finish pass — it can't be
        // emitted as JS.
        if not (List.isEmpty args) && not sawHole then
            failwithf "EmitJs: non-template ILIntrinsic opcode '%s' reached the JS backend" template

        List.ofSeq segs

    /// Build the single argument a `console.log`/`error` call prints from a
    /// printf-family format's segments. An all-literal format is one string; a
    /// lone hole is its operand value (Node stringifies); a mixed format is a
    /// string concatenation seeded with `""` so every `+` is string-valued
    /// (printf width/precision fidelity — `%5.2f` &c. — is deferred).
    and private buildFormatArg (ctx: WalkCtx) (segments: EqArray<Frozen.FormatSeg>) : JsExpr =
        match EqArray.toList segments with
        | [ FormatSegG.Lit s ] -> JsExpr.Literal(JsLiteral.String s, ValueNone)
        | [ FormatSegG.Hole(_, operand) ] -> buildExpr ctx operand
        | segs ->
            let pieces = ResizeArray<JsRawSeg>()
            // Seed with `""` so the first `+` already concatenates strings, even
            // when the format opens with two adjacent holes (`%d%d`).
            pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String "", ValueNone)))

            for seg in segs do
                pieces.Add(JsRawSeg.Verbatim " + ")

                match seg with
                | FormatSegG.Lit s -> pieces.Add(JsRawSeg.Hole(JsExpr.Literal(JsLiteral.String s, ValueNone)))
                | FormatSegG.Hole(_, operand) -> pieces.Add(JsRawSeg.Hole(buildExpr ctx operand))

            JsExpr.Raw(List.ofSeq pieces, ValueNone)

    /// Compile a pattern against a (pure) scrutinee-access expression `access` into a
    /// refutability *test* (`None` ⇒ irrefutable, the `&&`-conjunction of every
    /// tag/constant comparison) and the `const` bindings its named sub-patterns
    /// introduce. JS analogue of `EmitPattern`'s `buildMatchTest`. Valid because
    /// `access` is a pure scrutinee projection — duplicable across test and bindings,
    /// and the test short-circuits so a sub-field is read only once its tag matched.
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

            // Each sub-pattern matches a field by name; tests conjoin under the
            // (short-circuiting) tag test. `map2` asserts the front-end invariant of
            // one sub-pattern per field (a mismatch fails here, attributably).
            let childTests, childBinds =
                List.map2
                    (fun fld sub -> compileMatchPattern ctx (memberAccess fld) sub)
                    c.Fields
                    (EqArray.toList subPats)
                |> List.unzip

            conjoin (Some tagTest :: childTests), List.concat childBinds
        | TPatG.Record(fields, ty, _) ->
            // A record pattern never fails on shape (no tag): only sub-patterns
            // refute, each matching `access.<fieldName>`. Validate each field against
            // the emitted record so a stale name fails here, not as silent `undefined`.
            let info = recordInfoOf ctx "record pattern" ty
            let known = Set.ofList info.Fields

            let tests, binds =
                EqArray.toList fields
                |> List.map (fun (fieldName, sub) ->
                    if not (Set.contains fieldName known) then
                        failwithf
                            "EmitJs (Step 4): record pattern on '%s' names unknown field '%s'"
                            info.Name
                            fieldName

                    compileMatchPattern ctx (memberAccess fieldName) sub
                )
                |> List.unzip

            conjoin tests, List.concat binds
        // A tuple pattern never fails on shape (fixed-arity array, no tag): each
        // element matches its positional index `access[i]` (a pure projection, like
        // the union/record field accesses).
        | TPatG.Tuple(items, _, _) ->
            let indexAccess i =
                JsExpr.Member(access, JsExpr.Literal(JsLiteral.Number(string i), ValueNone), true, ValueNone)

            let tests, binds =
                EqArray.toList items
                |> List.mapi (fun i sub -> compileMatchPattern ctx (indexAccess i) sub)
                |> List.unzip

            conjoin tests, List.concat binds
        | TPatG.TypeTestAs _ -> failwithf "EmitJs (Step 4): type-test patterns are out of MVP scope"
        // `null` pattern: refutable, binds nothing. JS loose `== null` matches both
        // `null` and `undefined` (the latter being how a unit/absent value emits),
        // mirroring the CLR `brtrue`-skips-non-null lowering.
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

    /// Emit a function value as nested *unary* arrows. When `selfKey` names the
    /// binding and its body makes a saturated tail self-call, the innermost arrow
    /// becomes a `while (true)` trampoline (`buildTailBody`) for constant-stack
    /// recursion; else the innermost body is the plain expression. The nested-unary
    /// shape keeps every arrow's param in scope at the innermost body, which is what
    /// lets the trampoline write them back and `continue`.
    and emitFunction (ctx: WalkCtx) (selfKey: NodeKey voption) (lam: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok lam)
        let names, body = peelArrow ctx.Source lam
        let arity = List.length names

        let innermost =
            match selfKey with
            | ValueSome k when hasTailSelfCall k arity body ->
                JsFnBody.Block
                    [
                        JsStatement.While(
                            JsExpr.Literal(JsLiteral.Boolean true, ValueNone),
                            buildTailBody ctx k names body
                        )
                    ]
            | _ -> JsFnBody.Expr(buildExpr ctx body)

        nestUnaryArrows loc names innermost

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
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            recur (substVar k value body)
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            JsStatement.Const(identName ctx.Source k, buildExpr ctx value) :: recur body
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

    /// Emit one record/union member as a free, curried, receiver-first top-level
    /// function (Step 7): `member this.Foo a b` → `<Type>__Foo = (this$) => (a) => (b)
    /// => <body>`; a static member drops the receiver; a static property (no params)
    /// emits as a plain value binding. The body is walked by `buildExpr` with the
    /// member's `ThisKey` + `Params` as parameters — `Var`/`identName` resolves them
    /// with no new machinery (the receiver shares its key, so body refs line up; a
    /// source `this` binder maps to `this$` via the reserved-word guard). `export`ed
    /// in library mode, else `const` — via `topLevelBinding`.
    and emitMemberFn (ctx: WalkCtx) (typeName: string) (m: Frozen.TTypeMember) : JsStatement =
        let isProperty = (m.Kind = TMemberKind.Property)
        let name = Members.mangledName typeName m.IsStatic isProperty m.Name

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

    /// A value bound to a name (a module value, or a `let` binder). A `Lambda`
    /// value routes through `emitFunction` carrying its binder key, so a
    /// recursive binding (`let rec`) can recognise its own tail calls; any other
    /// value is a plain `buildExpr`.
    and emitBound (ctx: WalkCtx) (k: NodeKey) (value: Frozen.TExpr) : JsExpr =
        match value with
        | TExprG.Lambda _ -> emitFunction ctx (ValueSome k) value
        | _ -> buildExpr ctx value

    /// An expression in *statement* position (a top-level `do`, or a `let … in …`
    /// body). `Sequential` flattens to one statement per element (the trailing
    /// value is discarded); a `let` binder becomes a `const` then the body
    /// continues. Anything else is one `ExpressionStatement` over `buildExpr`.
    and buildStatements (ctx: WalkCtx) (e: Frozen.TExpr) : JsStatement list =
        match e with
        | TExprG.Sequential(xs, _, _) ->
            [
                for x in xs do
                    yield! buildStatements ctx x
            ]
        // A pure binder substitutes away (mirrors `buildExpr`); the synthetic
        // operand lets `InlineExpansion` leaves never surface as named `const`s.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            buildStatements ctx (substVar k value body)
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) ->
            JsStatement.Const(identName ctx.Source k, emitBound ctx k value)
            :: buildStatements ctx body
        | _ -> [ JsStatement.Expression(buildExpr ctx e) ]

    /// JS `finishOps` (`TastLower.lower`'s knob): identity — JS has no stack-machine
    /// intrinsic to collapse saturated operators into (ground operators were already
    /// templated pre-freeze by `InlineExpansion`; un-ground residue fails loudly in
    /// `buildExpr`).
    let private jsFinishOps (e: Frozen.TExpr) : Frozen.TExpr = e

    /// Collect the file's nominal `type` decls (in source order) into the emission
    /// list (a `Class` per record, a `Union` per union), the two lookup tables the
    /// walker keys record/union nodes through, and the member list (paired with the
    /// declaring type's emitted name, emitted by `buildProgram` once the tables are
    /// built). Read off the *un-lowered* decls — `TastLower.lower` drops `type` decls.
    let private collectTypes (tast: Frozen.TastFile) =
        let ordered = ResizeArray<JsStatement>()
        let records = System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>()
        let unions = System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>()
        let members = ResizeArray<string * Frozen.TTypeMember>()

        let addMembers (typeName: string) (ms: EqArray<Frozen.TTypeMember>) =
            for m in ms do
                members.Add(typeName, m)

        for decl in tast.Decls do
            match decl with
            | TDeclG.Type td ->
                match td.Kind with
                | TTypeKindG.Record(fields, recMembers) ->
                    let info =
                        {
                            Name = td.Name
                            Fields = [ for f in fields -> f.Name ]
                        }

                    records.[td.Key] <- info
                    ordered.Add(JsStatement.Class(info.Name, info.Fields))
                    addMembers td.Name recMembers
                | TTypeKindG.Union(cases, unionMembers) ->
                    let info, caseDecls =
                        buildUnionInfo td.Name [ for case in cases -> case.Name, [ for (nm, _) in case.Fields -> nm ] ]

                    unions.[td.Key] <- info
                    ordered.Add(JsStatement.Union(td.Name, caseDecls))
                    addMembers td.Name unionMembers
                | _ -> ()
            | _ -> ()

        List.ofSeq ordered, records, unions, List.ofSeq members

    /// The whole frozen file → a `Program`. Record `type` declarations become JS
    /// `class`es first (classes are not hoisted, so they must precede their `new`
    /// sites); the remaining decls are lowered (shared `TastLower.lower` with the
    /// JS `finishOps`) — inline `let inline` templates and `type` decls drop out,
    /// leaving top-level `let` values and effectful expressions.
    ///
    /// A type with no representation on the JS target (`decimal`, `nativeint`) is NOT
    /// rejected here: that is a semantic verdict (the provider's
    /// `Intrinsic(_, platform = None)`) and is reported up front, like an unresolved
    /// generic, by `SemanticAnalysis.PlatformTypes` — so the frozen tree reaching the
    /// emitter is already known-representable (intrinsic-runtime-type-plan.md).
    let buildProgram (ctx0: WalkCtx) (tast: Frozen.TastFile) : JsProgram =
        let classDecls, recordTable, unionTable, memberDefs = collectTypes tast

        let ctx =
            { ctx0 with
                Records = recordTable
                Unions = unionTable
            }

        // Each record/union member is a free, curried, receiver-first function
        // (Step 7), emitted after the class decls (they reference the classes via
        // `new`/match, and `const` arrows are not hoisted) and before the main body.
        let memberDecls = [ for (typeName, m) in memberDefs -> emitMemberFn ctx typeName m ]

        let lowered = TastLower.lower jsFinishOps tast.Decls

        let body =
            [
                for decl in lowered do
                    match decl with
                    | TDeclG.Expression(e, _) -> yield! buildStatements ctx e
                    // A top-level module value. In *library* mode it is `export`ed
                    // (a compiled runtime module's public surface, Step 5b Phase 3);
                    // in *script* mode it stays a plain `const`.
                    | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                        topLevelBinding ctx (identName ctx.Source k) (emitBound ctx k value)
                    | other -> failwithf "EmitJs (Step 1): unsupported declaration %A" other
            ]

        // Runtime-module imports (`Vesper.List`) are also discovered during the
        // walk; they lead the program (an `import` must precede every reference).
        // External union classes (`Option`, …) and local classes follow — both must
        // precede every `new`/match site (JS classes are not hoisted).
        {
            Body =
                JsImports.importStatements ctx.Imports
                @ classDecls
                @ List.ofSeq ctx.ExternalUnionDecls
                @ memberDecls
                @ body
        }
