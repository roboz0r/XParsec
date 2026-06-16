namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The `TAST → JsAst` walker — the JS analogue of `Emit*` in `Codegen.Clr`. The
/// band of arms grows one step at a time (codegen-js-steps.md), and
/// `EmitExpr.buildExpr` in the CLR backend is the master checklist each step works
/// toward. Every un-handled node is an explicit `failwithf` so an unsupported arm
/// surfaces loudly rather than dropping silently.
///
/// Step 0a covered `printfn "hi"`; Step 0b added source-map `loc`s. **Step 1**
/// adds: scalar `Const`s (int32 / int64-`BigInt` / float / float32 / char / bool /
/// unit); `Var` / `Let` / `Sequential` / `IfThenElse`; and the `ILIntrinsic`
/// `$N`-template path (the **(a\*)** `JsRaw` MVP — universal parenthesization, no
/// JS-grammar knowledge). The decls are first run through the shared
/// `TastLower.lower` with a JS `finishOps` (identity — JS keeps operators as
/// emit-able templates, it has no stack-machine intrinsic to collapse to); the
/// templated operator bodies themselves were already spliced pre-freeze by
/// `Passes.InlineExpansion` from `ops-platform.js.fs` (Step F1), so a ground
/// `2 + 2` arrives as `ILIntrinsic("($0 + $1) | 0", …)`.
///
/// **Step 2** adds functions: `Lambda` → a chain of nested *unary* arrows,
/// `App` → a unary call (`f a b` → `f(a)(b)`). The curried representation is
/// correct for saturated calls, partial application, and higher-order values
/// with no call-site arity analysis (the flat-call optimisation is deferred —
/// it needs boundary curry/uncurry adaptation to stay sound). Self-recursion in
/// tail position is trampolined to a `while (true)` loop with param-shadow
/// mutation (`emitFunction` / `buildTailBody`) so it runs in constant stack.
///
/// **Step 3** adds records: a record `type` decl becomes a JS `class` with a
/// positional constructor (`collectRecords` reads the declaration-order field
/// names off the *un-lowered* decls, since `TastLower.lower` drops `type`
/// decls); `RecordCons` → `new R(…)` with the literal's source-order fields
/// reordered to declaration order; `RecordClone` (`{ r with … }`) →
/// reconstruction `new R(…)` copying `r.field` for unlisted fields (through an
/// IIFE binder when the source is effectful); `FieldGet` → a `Member` access.
///
/// **Step 4** adds DUs + match: a union `type` decl becomes a JS base class
/// (integer `tag` + `cases()`) plus one `extends`-subclass per case
/// (`collectTypes`, read off the *un-lowered* decls); `UnionCons` → `new
/// <Union>_<Case>(args…)` (args already in field order); `Match` lowers to an IIFE
/// that tests each arm (`compileMatchPattern` returns a `&&`-conjoined test plus
/// path-projection `const` bindings — the JS analogue of `EmitPattern`'s
/// branch-to-`nextLabel`) and `return`s the first match's body, an unmatched value
/// `throw`ing.
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

    /// A record type's JS shape (Step 3): the emitted class `Name` and its
    /// `Fields` in *declaration* order. `RecordCons` / `RecordClone` build with
    /// `new Name(…)` and must order their arguments to match the class's
    /// positional constructor, so the declaration order is the authority (the
    /// literal's source order is reordered against it).
    type JsRecordInfo = { Name: string; Fields: string list }

    /// A union type's JS shape (Step 4): the emitted base-class `Name` and its
    /// cases keyed by F# case name. `UnionCons` looks a case up by name to pick
    /// its subclass + field order; a union *pattern* looks it up to compare the
    /// scrutinee's `tag` and read each field by name.
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
            /// Runtime-module imports discovered during the walk (Step 5b): the
            /// `External`-value walker calls `JsImports.addRef` (which resolves +
            /// caches each home assembly's runtime module once). `buildProgram`
            /// reads `JsImports.importStatements` for the leading `import …` block;
            /// `Codegen.compileWith` reads `JsImports.modules` off this same shared
            /// accumulator to materialise the runtime files.
            Imports: JsImports
        }

    let private locOf (ctx: WalkCtx) (tok: SyntaxToken) : JsLoc voption =
        match ctx.Resolver with
        | ValueSome idx -> ValueSome(LineIndex.resolve idx tok.StartIndex)
        | ValueNone -> ValueNone

    // ---- Variable names ------------------------------------------------------

    let private isIdentStart (c: char) = System.Char.IsLetter c || c = '_'

    let private isIdentCont (c: char) =
        System.Char.IsLetterOrDigit c || c = '_' || c = '\''

    /// A `Var` / binder `NodeKey` → its JS identifier. The binder and every
    /// reference carry the *same* key (the CLR backend resolves them through one
    /// slot table the same way), so a key-derived name is consistent across the
    /// binding and its uses. When the source text is available the binder token's
    /// offset points at the source identifier, recovered verbatim (apostrophes,
    /// illegal in JS, become `_`); otherwise a synthetic `_v<offset>` keeps it
    /// stable and collision-free.
    let private identName (source: string voption) (k: NodeKey) : string =
        match source with
        | ValueSome s when k.Offset >= 0 && k.Offset < s.Length && isIdentStart s.[k.Offset] ->
            let mutable i = k.Offset

            while i < s.Length && isIdentCont s.[i] do
                i <- i + 1

            (s.Substring(k.Offset, i - k.Offset)).Replace('\'', '_')
        | _ -> "_v" + string k.Offset

    // ---- Scalar constants ----------------------------------------------------

    /// Format a `double` round-trippably for a JS `number` literal. `NaN` /
    /// `Infinity` / `-Infinity` map to the matching JS globals.
    let private formatDouble (d: double) : string =
        if System.Double.IsNaN d then "NaN"
        elif System.Double.IsPositiveInfinity d then "Infinity"
        elif System.Double.IsNegativeInfinity d then "-Infinity"
        else d.ToString("R", CultureInfo.InvariantCulture)

    /// A scalar `Const` value → its JS expression. Shared by the expression arm
    /// (`buildExpr`) and a `Const` *pattern* (whose equality test compares the
    /// scrutinee against this literal).
    let private constExpr (value: TConstValue) (loc: JsLoc voption) : JsExpr =
        match value with
        | TConstValue.Int n -> JsExpr.Literal(JsLiteral.Number(string n), loc)
        | TConstValue.Byte b -> JsExpr.Literal(JsLiteral.Number(string (int b)), loc)
        | TConstValue.Int64 n -> JsExpr.Literal(JsLiteral.BigInt(string n), loc)
        | TConstValue.Float d -> JsExpr.Literal(JsLiteral.Number(formatDouble d), loc)
        | TConstValue.Float32 f -> JsExpr.Literal(JsLiteral.Number(formatDouble (float f)), loc)
        | TConstValue.Bool b -> JsExpr.Literal(JsLiteral.Boolean b, loc)
        // A `char` is a length-1 JS string (no distinct char type).
        | TConstValue.Char c -> JsExpr.Literal(JsLiteral.String(string c), loc)
        | TConstValue.String s -> JsExpr.Literal(JsLiteral.String s, loc)
        // The unit value is `undefined` — JS has no unit, and `undefined` is the
        // harmless value a discarded effectful expression yields.
        | TConstValue.Unit -> JsExpr.Identifier("undefined", loc)
        | TConstValue.Decimal _ -> failwithf "EmitJs (Step 1): decimal literals are not yet supported"

    // ---- Pure-`let` substitution ---------------------------------------------

    /// A value safe to splice at its use site(s): no side effects and no
    /// evaluation-order dependence, so moving it (even duplicating it) preserves
    /// semantics. Covers the operands `Passes.InlineExpansion` `let`-binds when it
    /// splices an operator body (`2 + 2` → `let a = 2 in let b = 2 in (# … a b #)`):
    /// `Const`/`Var`, and a pure `ILIntrinsic` (the operator templates) over pure
    /// args.
    let rec private isPureValue (e: Frozen.TExpr) : bool =
        match e with
        | TExprG.Const _
        | TExprG.Var _ -> true
        | TExprG.ILIntrinsic(_, _, args, _, _) -> EqArray.toList args |> List.forall isPureValue
        // A pure `let` chain (the operand lets `InlineExpansion` nests around a
        // composite operator body — `a + b + c` binds the inner `a + b` to a let
        // whose value is *itself* a let chain) is pure when both its bound value and
        // body are: substituting it into the use site lets the recursive pure-`let`
        // collapse reduce it to a clean template, rather than emitting an IIFE whose
        // synthetic binder has no source name (it would mis-recover one from the
        // binder key's offset).
        | TExprG.Let(TPatG.NamedSimple _, value, body, _, _) -> isPureValue value && isPureValue body
        | _ -> false

    /// Replace every `Var k` in `e` with `value`. Used only for a pure `value`, so
    /// duplicating it across multiple uses is semantics-preserving.
    let rec private substVar (k: NodeKey) (value: Frozen.TExpr) (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.Var(vk, _, _) when vk.Raw = k.Raw -> value
        | _ -> TastLower.mapChildren (substVar k value) e

    // ---- Functions -----------------------------------------------------------

    /// A lambda parameter's JS binding form. A `NamedSimple` binder reuses the
    /// `Var`/binder naming (`identName`); a `unit` parameter (`fun () -> …`) is
    /// never referenced, so it gets a fresh unused name keyed off its source
    /// offset; a tuple parameter (`fun (a, b) -> …`, Step 5) becomes a JS
    /// array-destructuring pattern (`[a, b]`) — the leaf binders carry the same
    /// `NodeKey`s the body's `Var`s reference, so the names line up — recursing for
    /// nested tuples (`fun ((a, b), c) -> …` → `[[a, b], c]`). A `Wildcard` leaf
    /// gets a fresh unused name (JS array holes would shift later positions).
    /// Lambda parameters are irrefutable, so no refutable leaf (`Const` / `Union`)
    /// can appear here.
    //
    // TODO (boundary): a tuple leaf returns a JS *destructuring pattern* (`[a, b]`)
    // smuggled through this `string`, which `JsPrint` emits verbatim into `Arrow`'s
    // `string list` params. Consistent with the pre-existing opaque param strings
    // (`unit` / `_v<offset>`), but the seam wants a real `JsPattern` (identifier |
    // array-destructure | object-destructure) once nested/object-destructuring
    // params arrive — at which point `Arrow.parameters` should carry that, not text.
    let rec private lambdaParamName (source: string voption) (p: Frozen.TPat) : string =
        match p with
        | TPatG.NamedSimple(k, _, _) -> identName source k
        | TPatG.Wildcard(_, tok) -> "_w" + string tok.StartIndex
        | TPatG.Const(TConstValue.Unit, _, tok) -> "_u" + string tok.StartIndex
        | TPatG.Tuple(items, _, _) ->
            let parts = EqArray.toList items |> List.map (lambdaParamName source)
            "[" + System.String.Join(", ", parts) + "]"
        | other -> failwithf "EmitJs (Step 5): unsupported lambda parameter pattern %A" other

    /// Peel a curried `Lambda` chain into its parameter names and the innermost
    /// body. The inverse of the nested-arrow emission.
    let rec private peelArrow (source: string voption) (e: Frozen.TExpr) : string list * Frozen.TExpr =
        match e with
        | TExprG.Lambda(p, body, _, _) ->
            let names, inner = peelArrow source body
            lambdaParamName source p :: names, inner
        | _ -> [], e

    /// `e` is a fully-saturated self-call of the function bound to `selfKey` at
    /// `arity`; yields its argument expressions in source order. The single
    /// definition of "tail self-call" shared by the detector (`hasTailSelfCall`)
    /// and the rewriter (`buildTailBody`), so the two can't drift and the spine is
    /// walked once.
    let private (|TailSelfCall|_|) (selfKey: NodeKey) (arity: int) (e: Frozen.TExpr) : Frozen.TExpr list option =
        match e with
        | TExprG.App _ ->
            match TastWalk.collectSpine [] e with
            | TExprG.Var(k, _, _), spine when k.Raw = selfKey.Raw && List.length spine = arity ->
                Some [ for (a, _, _) in spine -> a ]
            | _ -> None
        | _ -> None

    /// Is `e`, in tail position, a fully-saturated self-call of the function
    /// bound to `selfKey` (arity `arity`)? Recurses through the constructs that
    /// preserve tail position (`if`/`let`/`Sequential`-tail); a saturated tail
    /// self-call is what the trampoline rewrites to param mutation + `continue`.
    let rec private hasTailSelfCall (selfKey: NodeKey) (arity: int) (e: Frozen.TExpr) : bool =
        match e with
        | TExprG.IfThenElse(_, thenE, elseE, _, _) ->
            hasTailSelfCall selfKey arity thenE || hasTailSelfCall selfKey arity elseE
        | TExprG.Let(_, _, body, _, _) -> hasTailSelfCall selfKey arity body
        | TExprG.Sequential(xs, _, _) when xs.Length > 0 -> hasTailSelfCall selfKey arity xs.[xs.Length - 1]
        | TailSelfCall selfKey arity _ -> true
        | _ -> false

    // ---- Records -------------------------------------------------------------

    /// The nominal `SymbolKey` of a `RecordCons` / `RecordClone` / `FieldGet` /
    /// `UnionCons` / pattern receiver type — the key the type declaration filled
    /// its lookup table under. `what` names the construct for the diagnostic; a
    /// non-nominal receiver is an invariant break (these constructs only ever
    /// target a declared record/union).
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

    /// A union case's declaration-order field names, synthesised to F#'s compiled
    /// convention from each field's optional source name: a *named* field
    /// (`| Case of x: int`, `Some of Value: 'T`) keeps its name; a *positional*
    /// field becomes `Item` (a lone field) or `Item1` / `Item2` / … (several), so
    /// `UnionCons`'s positional args and a union pattern's positional sub-patterns
    /// address the same property names. Shared by the local (`Frozen.TUnionCase`)
    /// and external (`ExternalCaseShape`) paths.
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

    /// Build a `JsUnionInfo` (+ the case-name → `JsUnionCaseDecl` table) for a union
    /// named `baseName` whose cases are `(caseName, fieldNames)` in declaration
    /// order. The shared core of the local (`collectTypes`) and external
    /// (`resolveExternalUnion`) emission: the tag is the declaration-order index,
    /// the subclass name is `<baseName>_<case>`, the fields are synthesised by
    /// `synthFieldNames`.
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

    /// Resolve an *external* union type (one not declared in this file — `Option`,
    /// `List`) to a `JsUnionInfo`, reading its case shapes off the symbol provider
    /// and emitting honest nominal JS classes for it (the same base-class +
    /// per-case-subclass shape a local union gets, queued in `ctx.ExternalUnionDecls`
    /// for `buildProgram` to prepend). Cached in `ctx.ExternalUnions` so the classes
    /// are emitted once. `ValueNone` when there is no provider or the type does not
    /// resolve to a union — the caller then fails loudly.
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

    // ---- The walker ----------------------------------------------------------

    let rec buildExpr (ctx: WalkCtx) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok e)

        match e with
        | TExprG.Const(value, _, _) -> constExpr value loc

        | TExprG.Var(k, _, _) -> JsExpr.Identifier(identName ctx.Source k, loc)

        // An external module function (`List.length`, `List.map`) — a value imported
        // from its package's JS runtime module (Step 5b). The reference is the
        // import alias; `App` saturates it (`List.map f xs` ≡ `$…map(f)(xs)`,
        // curried unary like every JS call).
        | TExprG.External(compiledName, key, _, _) ->
            JsExpr.Identifier(JsImports.addRef ctx.Imports compiledName key, loc)

        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            JsExpr.Conditional(buildExpr ctx cond, buildExpr ctx thenE, buildExpr ctx elseE, loc)

        // A `Sequential` in *expression* position is a comma expression: evaluate
        // each, yield the last. (At top level it is expanded to statements by
        // `buildStatements`.)
        | TExprG.Sequential(xs, _, _) -> JsExpr.Sequence([ for x in xs -> buildExpr ctx x ], loc)

        // A tuple `(a, b, …)` is a JS array `[a, b, …]` (Step 5); a tuple pattern
        // reads each element back by positional index.
        | TExprG.Tuple(items, _, _) -> JsExpr.Array([ for x in items -> buildExpr ctx x ], loc)

        // A `let` in expression position (the operand lets `InlineExpansion`
        // introduces around a spliced operator body). When the bound value is pure
        // it is substituted into its uses — collapsing `let a = 2 in let b = 2 in
        // (# … a b #)` back to the clean template.
        | TExprG.Let(TPatG.NamedSimple(k, _, _), value, body, _, _) when isPureValue value ->
            buildExpr ctx (substVar k value body)

        // A non-pure `let` in expression position (e.g. an operator operand that is
        // itself a function call: `n * fact (n - 1)` binds `fact (n - 1)` to a
        // let). JS has no let-expression, so it lowers to an IIFE
        // `((x) => <body>)(<value>)` — the binder evaluated once, then the body.
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

        // Curried application: `f a b` (`App(App(f, a), b)`) emits one unary call
        // per `App` (`f(a)(b)`). F# functions are emitted as nested unary arrows
        // (`emitFunction`), so a unary call chain is correct for saturated calls,
        // partial application, and higher-order values alike — without any
        // call-site arity analysis. (The flat-call optimisation the plan sketches
        // — `add(x, y)` with re-curry at partial sites — needs boundary
        // curry/uncurry adaptation to stay sound across generic higher-order
        // functions, so it is deferred to its own slice.)
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

        // `{ r with X = v; … }` → reconstruction `new R(…)`: each declaration-order
        // field takes its override expression if listed, else reads `<src>.field`.
        // The source is read once per copied field, so `<src>` must be cheap and
        // re-evaluable: a bare `Var` (`{ p with … }`) is spliced inline as the
        // identifier; *any* other source — effectful or merely a larger pure
        // expression that we'd otherwise duplicate across every field — is bound
        // once through an IIFE binder.
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

        // `match scrut with …` → an IIFE: bind the scrutinee once to a parameter,
        // then test each arm in order, `return`ing the first whose pattern (and
        // guard) matches; an unmatched value `throw`s. (The plan sketched a
        // `switch(tag)`; the ported `EmitPattern`/`EmitMatch` logic is the more
        // general sequential test — it subsumes the tag dispatch and also covers
        // guards, constants, nested patterns, and non-union scrutinees.)
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
        // The `ops-platform` equality / hash bodies call the structural core
        // (`equals($0, $1)`, `structuralHash($0)`) by bare name from their
        // templates — so emitting one of those templates is exactly when the
        // `Vesper.Core.mjs` import must be pulled in (under the unaliased export
        // name). `!equals(…)` (`<>`'s base) carries the same `equals(` substring.
        // (The `(` form avoids tripping on an unrelated user-FFI identifier.)
        if template.Contains "structuralHash(" then
            JsImports.ensureCoreImport ctx.Imports "structuralHash"

        if template.Contains "equals(" then
            JsImports.ensureCoreImport ctx.Imports "equals"

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

    /// Compile a pattern against a (pure) scrutinee-access expression `access`
    /// into a refutability *test* (`None` ⇒ irrefutable) and the `const` bindings
    /// its named sub-patterns introduce. The JS analogue of `EmitPattern`'s
    /// `buildMatchTest`: where the CLR backend branches to a `nextLabel` on a
    /// mismatch and aliases bound slots, this returns a boolean test (the
    /// `&&`-conjunction of every tag/constant comparison in the tree) plus
    /// path-projection `const`s — a bound name binds to its `access` sub-path. Both
    /// are valid because `access` is always a pure projection of the scrutinee
    /// variable, so it may be duplicated across the test and the bindings, and the
    /// test short-circuits so a sub-field is only read once its enclosing tag
    /// matched.
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

            // Each sub-pattern matches a declaration-order field, read by name off
            // the case's emitted property; the per-field tests conjoin under the tag
            // test (which short-circuits), and the bindings accumulate. `map2`
            // asserts the front-end invariant that a case pattern carries exactly one
            // sub-pattern per field (a mismatch fails here, attributably).
            let childTests, childBinds =
                List.map2
                    (fun fld sub -> compileMatchPattern ctx (memberAccess fld) sub)
                    c.Fields
                    (EqArray.toList subPats)
                |> List.unzip

            conjoin (Some tagTest :: childTests), List.concat childBinds
        | TPatG.Record(fields, ty, _) ->
            // A record pattern never fails on shape (no tag): only its sub-patterns
            // can refute. The emitted class stores each field under its source name,
            // so a field sub-pattern matches `access.<fieldName>`; validate each
            // pattern field against the emitted record so a stale name fails here
            // rather than emitting an `access.<bogus>` that is silently `undefined`.
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
        // A tuple pattern never fails on shape (a tuple is a fixed-arity array, no
        // tag): only its element sub-patterns can refute. Each element matches its
        // positional index `access[i]` — a *computed* member, a pure projection of
        // the scrutinee, so it may be duplicated across the test and the bindings
        // exactly as the union/record field accesses are.
        | TPatG.Tuple(items, _, _) ->
            let indexAccess i =
                JsExpr.Member(access, JsExpr.Literal(JsLiteral.Number(string i), ValueNone), true, ValueNone)

            let tests, binds =
                EqArray.toList items
                |> List.mapi (fun i sub -> compileMatchPattern ctx (indexAccess i) sub)
                |> List.unzip

            conjoin tests, List.concat binds
        | TPatG.TypeTestAs _ -> failwithf "EmitJs (Step 4): type-test patterns are out of MVP scope"

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

    /// Emit a function value as a chain of nested *unary* arrows. When `selfKey`
    /// names the binding the function is bound to and its body makes a saturated
    /// self-call in tail position, the innermost arrow becomes a `while (true)`
    /// trampoline (`buildTailBody`) so self-recursion runs in constant stack;
    /// otherwise the innermost body is the concise expression. The nested-unary
    /// shape keeps the parameters of every arrow in lexical scope at the innermost
    /// body, which is what lets the trampoline write them back and `continue`.
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

        let rec nest names =
            match names with
            | [ last ] -> JsExpr.Arrow([ last ], innermost, loc)
            | n :: rest -> JsExpr.Arrow([ n ], JsFnBody.Expr(nest rest), loc)
            | [] -> failwith "EmitJs (Step 2): a lambda peeled to zero parameters"

        nest names

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
            // `_tc<i>` temporaries only need to avoid the param names in scope;
            // they are not collision-proof against a source parameter literally
            // named `_tc0` (the JS backend keys synthetic names off strings, not
            // `NodeKey`s as the CLR backend does — see `_u`/`_v` in `identName`).
            let tmp i = "_tc" + string i
            // Evaluate every new argument into a temporary before any write-back,
            // so a self-call argument that mentions a parameter reads its current
            // (pre-iteration) value.
            [ for i, a in List.indexed args -> JsStatement.Const(tmp i, buildExpr ctx a) ]
            @ [
                for i, name in List.indexed paramNames -> JsStatement.Assign(name, JsExpr.Identifier(tmp i, ValueNone))
            ]
            @ [ JsStatement.Continue ]
        | _ -> [ JsStatement.Return(buildExpr ctx e) ]

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

    /// JS `finishOps` (`TastLower.lower`'s knob): identity. JS keeps operators as
    /// emit-able templates / `BinaryExpression`s — it has no stack-machine
    /// intrinsic to collapse a saturated operator into, so nothing is rewritten.
    /// (Ground operators were already templated pre-freeze by `InlineExpansion`;
    /// any un-ground residue stays an `External` head and fails loudly in
    /// `buildExpr` until a later step routes it.)
    let private jsFinishOps (e: Frozen.TExpr) : Frozen.TExpr = e

    /// Collect the file's nominal `type` declarations, in source order, into the
    /// emission list (one `Class` statement per record, one `Union` per union) and
    /// the two lookup tables the walker keys `RecordCons`/`FieldGet` and
    /// `UnionCons`/union-patterns through. `TastLower.lower` drops every `type`
    /// decl (they are metadata, not in the expression stream), so the shapes are
    /// read from the *un-lowered* decls here. Classes / interfaces are later steps.
    let private collectTypes (tast: Frozen.TastFile) =
        let ordered = ResizeArray<JsStatement>()
        let records = System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>()
        let unions = System.Collections.Generic.Dictionary<SymbolKey, JsUnionInfo>()

        for decl in tast.Decls do
            match decl with
            | TDeclG.Type td ->
                match td.Kind with
                | TTypeKindG.Record(fields, _) ->
                    let info =
                        {
                            Name = td.Name
                            Fields = [ for f in fields -> f.Name ]
                        }

                    records.[td.Key] <- info
                    ordered.Add(JsStatement.Class(info.Name, info.Fields))
                | TTypeKindG.Union(cases, _) ->
                    let info, caseDecls =
                        buildUnionInfo td.Name [ for case in cases -> case.Name, [ for (nm, _) in case.Fields -> nm ] ]

                    unions.[td.Key] <- info
                    ordered.Add(JsStatement.Union(td.Name, caseDecls))
                | _ -> ()
            | _ -> ()

        List.ofSeq ordered, records, unions

    /// The whole frozen file → a `Program`. Record `type` declarations become JS
    /// `class`es first (classes are not hoisted, so they must precede their `new`
    /// sites); the remaining decls are lowered (shared `TastLower.lower` with the
    /// JS `finishOps`) — inline `let inline` templates and `type` decls drop out,
    /// leaving top-level `let` values and effectful expressions.
    let buildProgram (ctx0: WalkCtx) (tast: Frozen.TastFile) : JsProgram =
        let classDecls, recordTable, unionTable = collectTypes tast

        let ctx =
            { ctx0 with
                Records = recordTable
                Unions = unionTable
            }

        let lowered = TastLower.lower jsFinishOps tast.Decls

        let body =
            [
                for decl in lowered do
                    match decl with
                    | TDeclG.Expression(e, _) -> yield! buildStatements ctx e
                    | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                        JsStatement.Const(identName ctx.Source k, emitBound ctx k value)
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
                @ body
        }
