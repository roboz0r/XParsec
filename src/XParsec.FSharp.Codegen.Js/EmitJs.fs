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
        | _ -> false

    /// Replace every `Var k` in `e` with `value`. Used only for a pure `value`, so
    /// duplicating it across multiple uses is semantics-preserving.
    let rec private substVar (k: NodeKey) (value: Frozen.TExpr) (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.Var(vk, _, _) when vk.Raw = k.Raw -> value
        | _ -> TastLower.mapChildren (substVar k value) e

    // ---- Functions -----------------------------------------------------------

    /// A lambda parameter's JS identifier. A `NamedSimple` binder reuses the
    /// `Var`/binder naming (`identName`); a `unit` parameter (`fun () -> …`) is
    /// never referenced, so it gets a fresh unused name keyed off its source
    /// offset. Tuple-destructuring parameters (`fun (a, b) -> …`) are Step 5 and
    /// fail loudly here.
    let private lambdaParamName (source: string voption) (p: Frozen.TPat) : string =
        match p with
        | TPatG.NamedSimple(k, _, _) -> identName source k
        | TPatG.Const(TConstValue.Unit, _, tok) -> "_u" + string tok.StartIndex
        | other -> failwithf "EmitJs (Step 2): unsupported lambda parameter pattern %A" other

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

    /// Resolve a `RecordCons` / `RecordClone` / `FieldGet` receiver type to its
    /// emitted `JsRecordInfo`. `what` names the construct for the diagnostic. The
    /// nominal `SymbolKey` keys the same table the type declaration filled, so an
    /// absent entry means the record's `type` decl never reached this file.
    let private recordInfoOf (ctx: WalkCtx) (what: string) (ty: FrozenType) : JsRecordInfo =
        match TastLower.receiverShape ty with
        | ValueSome(key, _) ->
            match ctx.Records.TryGetValue key with
            | true, info -> info
            | _ -> failwithf "EmitJs (Step 3): %s on record with no emitted type (key %A)" what key
        | ValueNone -> failwithf "EmitJs (Step 3): %s on non-nominal type %A" what ty

    // ---- The walker ----------------------------------------------------------

    let rec buildExpr (ctx: WalkCtx) (e: Frozen.TExpr) : JsExpr =
        let loc = locOf ctx (TastWalk.exprTok e)

        match e with
        | TExprG.Const(value, _, _) ->
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
            // The unit value is `undefined` — JS has no unit, and `undefined` is
            // the harmless value a discarded effectful expression yields.
            | TConstValue.Unit -> JsExpr.Identifier("undefined", loc)
            | TConstValue.Decimal _ -> failwithf "EmitJs (Step 1): decimal literals are not yet supported"

        | TExprG.Var(k, _, _) -> JsExpr.Identifier(identName ctx.Source k, loc)

        | TExprG.IfThenElse(cond, thenE, elseE, _, _) ->
            JsExpr.Conditional(buildExpr ctx cond, buildExpr ctx thenE, buildExpr ctx elseE, loc)

        // A `Sequential` in *expression* position is a comma expression: evaluate
        // each, yield the last. (At top level it is expanded to statements by
        // `buildStatements`.)
        | TExprG.Sequential(xs, _, _) -> JsExpr.Sequence([ for x in xs -> buildExpr ctx x ], loc)

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
        // field takes its override expression if listed, else reads `r.field`. A
        // pure source (`{ p with … }`, a `Var`) is read field-wise inline; an
        // effectful source is evaluated once through an IIFE binder so it is not
        // re-run per copied field.
        | TExprG.RecordClone(source, overrides, ty, _) ->
            let info = recordInfoOf ctx "RecordClone" ty
            let overrideMap = Map.ofSeq (EqArray.toList overrides)

            let argsFrom (srcRef: unit -> JsExpr) =
                [
                    for f in info.Fields ->
                        match Map.tryFind f overrideMap with
                        | Some ov -> buildExpr ctx ov
                        | None -> JsExpr.Member(srcRef (), JsExpr.Identifier(f, ValueNone), false, ValueNone)
                ]

            if isPureValue source then
                JsExpr.New(JsExpr.Identifier(info.Name, ValueNone), argsFrom (fun () -> buildExpr ctx source), loc)
            else
                let sName = "_rc" + string (TastWalk.exprTok e).StartIndex

                let newExpr =
                    JsExpr.New(
                        JsExpr.Identifier(info.Name, ValueNone),
                        argsFrom (fun () -> JsExpr.Identifier(sName, ValueNone)),
                        loc
                    )

                JsExpr.Call(
                    JsExpr.Arrow([ sName ], JsFnBody.Expr newExpr, ValueNone),
                    [ buildExpr ctx source ],
                    loc
                )

        // `r.X` → `r.X` — a member access on the record's like-named property
        // (the emitted class stores each field under its source field name).
        | TExprG.FieldGet(receiver, fieldName, _, _) ->
            JsExpr.Member(buildExpr ctx receiver, JsExpr.Identifier(fieldName, ValueNone), false, loc)

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

    /// Collect the file's record type declarations, in source order, into the
    /// emission list (one `class` statement each) and the lookup table the walker
    /// keys `RecordCons` / `RecordClone` / `FieldGet` through. `TastLower.lower`
    /// drops every `type` decl (they are metadata, not in the expression stream),
    /// so the record shape is read from the *un-lowered* decls here. Only `Record`
    /// kinds are collected; unions / classes / interfaces are later steps.
    let private collectRecords
        (tast: Frozen.TastFile)
        : (SymbolKey * JsRecordInfo) list * System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo> =
        let ordered = ResizeArray<SymbolKey * JsRecordInfo>()
        let table = System.Collections.Generic.Dictionary<SymbolKey, JsRecordInfo>()

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

                    ordered.Add(td.Key, info)
                    table.[td.Key] <- info
                | _ -> ()
            | _ -> ()

        List.ofSeq ordered, table

    /// The whole frozen file → a `Program`. Record `type` declarations become JS
    /// `class`es first (classes are not hoisted, so they must precede their `new`
    /// sites); the remaining decls are lowered (shared `TastLower.lower` with the
    /// JS `finishOps`) — inline `let inline` templates and `type` decls drop out,
    /// leaving top-level `let` values and effectful expressions.
    let buildProgram (ctx0: WalkCtx) (tast: Frozen.TastFile) : JsProgram =
        let recordList, recordTable = collectRecords tast
        let ctx = { ctx0 with Records = recordTable }
        let lowered = TastLower.lower jsFinishOps tast.Decls

        let classDecls = [ for (_, info) in recordList -> JsStatement.Class(info.Name, info.Fields) ]

        let body =
            [
                for decl in lowered do
                    match decl with
                    | TDeclG.Expression(e, _) -> yield! buildStatements ctx e
                    | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                        JsStatement.Const(identName ctx.Source k, emitBound ctx k value)
                    | other -> failwithf "EmitJs (Step 1): unsupported declaration %A" other
            ]

        { Body = classDecls @ body }
