namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The walker's recur-free leaf helpers — the JS analogue of the CLR backend's
/// `EmitPattern` / `EmitResolve` / `EmitLower` modules, which are factored out of
/// the `buildExpr` clique and compiled *before* it precisely because they never
/// call back into expression emission. None of these touch `WalkCtx` either, so
/// they depend only on `JsAst` + the frozen TAST: identifier naming, scalar-literal
/// formatting, the pure-`let` substitution, and the curried-arrow / tail-self-call
/// shaping. `EmitJs` `open`s this module, so its arms reference these unqualified.
module JsEmitHelpers =

    // ---- Variable names ------------------------------------------------------

    let isIdentStart (c: char) = System.Char.IsLetter c || c = '_'

    let isIdentCont (c: char) =
        System.Char.IsLetterOrDigit c || c = '_' || c = '\''

    /// JS reserved words that are legal F# identifiers and could be recovered as a
    /// binder name — most notably `this` (the `member this.X` receiver). A collision
    /// is suffixed with `$` (illegal in F#, so collision-free); binder and uses share
    /// the key and both go through `identName`, so the rewrite stays consistent.
    let jsReserved =
        Set.ofList
            [
                "this"
                "new"
                "in"
                "do"
                "if"
                "else"
                "for"
                "while"
                "return"
                "var"
                "let"
                "const"
                "function"
                "class"
                "delete"
                "typeof"
                "void"
                "instanceof"
                "default"
                "switch"
                "case"
                "break"
                "continue"
                "throw"
                "try"
                "catch"
                "finally"
                "yield"
                "await"
                "super"
                "extends"
                "import"
                "export"
                "null"
                "true"
                "false"
                "with"
                "enum"
            ]

    let jsSafe (name: string) =
        if Set.contains name jsReserved then name + "$" else name

    /// A `Var` / binder `NodeKey` → its JS identifier. Binder and uses share the key,
    /// so a key-derived name lines them up. A *real* binder's `Offset` indexes the
    /// source identifier, recovered verbatim (apostrophes → `_`); otherwise `_v<off>`.
    ///
    /// A *synthetic* key (`IsSynthetic` — e.g. `InlineExpansion`'s operand `let`s)
    /// carries a per-build counter in `Offset`, NOT a source position, so it must NOT
    /// index the source (that once recovered `"amespace"` from the `namespace` header).
    /// It gets `_s<off>` — disjoint from real binders' `_v<off>`, so the counter can't
    /// collide with a real offset.
    let identName (source: string voption) (k: NodeKey) : string =
        match source with
        | ValueSome s when
            not k.IsSynthetic
            && k.Offset >= 0
            && k.Offset < s.Length
            && isIdentStart s.[k.Offset]
            ->
            let mutable i = k.Offset

            while i < s.Length && isIdentCont s.[i] do
                i <- i + 1

            jsSafe ((s.Substring(k.Offset, i - k.Offset)).Replace('\'', '_'))
        | _ -> (if k.IsSynthetic then "_s" else "_v") + string k.Offset

    // ---- Scalar constants ----------------------------------------------------

    /// Format a `double` round-trippably for a JS `number` literal. `NaN` /
    /// `Infinity` / `-Infinity` map to the matching JS globals.
    let formatDouble (d: double) : string =
        if System.Double.IsNaN d then "NaN"
        elif System.Double.IsPositiveInfinity d then "Infinity"
        elif System.Double.IsNegativeInfinity d then "-Infinity"
        else d.ToString("R", CultureInfo.InvariantCulture)

    /// A scalar `Const` value → its JS expression. Shared by the expression arm
    /// (`buildExpr`) and a `Const` *pattern* (whose equality test compares the
    /// scrutinee against this literal).
    let constExpr (value: TConstValue) (loc: JsLoc voption) : JsExpr =
        match value with
        | TConstValue.Int n -> JsExpr.Literal(JsLiteral.Number(string n), loc)
        // `uint32` is a plain JS `number` — its range (≤ 2³²-1) fits a double
        // exactly, so the unsigned value emits as a decimal literal verbatim.
        | TConstValue.UInt n -> JsExpr.Literal(JsLiteral.Number(string n), loc)
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
    let rec isPureValue (e: Frozen.TExpr) : bool =
        match e with
        | TExprG.Const _
        | TExprG.Var _ -> true
        | TExprG.ILIntrinsic(_, _, args, _, _) -> EqArray.toList args |> List.forall isPureValue
        // A pure `let` chain (the nested operand lets of a composite operator body,
        // `a + b + c`) is pure when both value and body are — so the recursive collapse
        // reduces it to a clean template instead of an IIFE over a nameless synthetic.
        | TExprG.Let(TPatG.NamedSimple _, value, body, _, _) -> isPureValue value && isPureValue body
        | _ -> false

    /// Replace every `Var k` in `e` with `value`. Used only for a pure `value`, so
    /// duplicating it across multiple uses is semantics-preserving.
    let rec substVar (k: NodeKey) (value: Frozen.TExpr) (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.Var(vk, _, _) when vk.Raw = k.Raw -> value
        | _ -> TastLower.mapChildren (substVar k value) e

    // ---- Functions -----------------------------------------------------------

    /// A lambda parameter's JS binding form. `NamedSimple` reuses `identName`; `unit`
    /// and `Wildcard` get fresh unused names (the latter because JS array holes shift
    /// later positions); a tuple becomes an array-destructuring pattern (`[a, b]`),
    /// recursing for nesting — its leaf binders carry the same `NodeKey`s the body's
    /// `Var`s use, so they line up. Lambda params are irrefutable, so no `Const` /
    /// `Union` leaf appears here.
    //
    // TODO (boundary): a tuple leaf smuggles a destructuring pattern through this
    // `string` (emitted verbatim into `Arrow`'s `string list` params). The seam wants
    // a real `JsPattern` on `Arrow.parameters` once object-destructuring params arrive.
    let rec lambdaParamName (source: string voption) (p: Frozen.TPat) : string =
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
    let rec peelArrow (source: string voption) (e: Frozen.TExpr) : string list * Frozen.TExpr =
        match e with
        | TExprG.Lambda(p, body, _, _) ->
            let names, inner = peelArrow source body
            lambdaParamName source p :: names, inner
        | _ -> [], e

    /// Nest a non-empty parameter-name list into a chain of *unary* arrows around
    /// `innermost` (`["a"; "b"]` → `(a) => (b) => <innermost>`). The shared shape of
    /// `emitFunction`'s lambda emission and a member function's receiver-then-params
    /// chain.
    let rec nestUnaryArrows (loc: JsLoc voption) (names: string list) (innermost: JsFnBody) : JsExpr =
        match names with
        | [ last ] -> JsExpr.Arrow([ last ], innermost, loc)
        | n :: rest -> JsExpr.Arrow([ n ], JsFnBody.Expr(nestUnaryArrows loc rest innermost), loc)
        | [] -> failwith "EmitJs: nestUnaryArrows on an empty parameter list"

    /// `e` is a fully-saturated self-call of the function bound to `selfKey` at
    /// `arity`; yields its argument expressions in source order. The single
    /// definition of "tail self-call" shared by the detector (`hasTailSelfCall`)
    /// and the rewriter (`buildTailBody`), so the two can't drift and the spine is
    /// walked once.
    let (|TailSelfCall|_|) (selfKey: NodeKey) (arity: int) (e: Frozen.TExpr) : Frozen.TExpr list option =
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
    let rec hasTailSelfCall (selfKey: NodeKey) (arity: int) (e: Frozen.TExpr) : bool =
        match e with
        | TExprG.IfThenElse(_, thenE, elseE, _, _) ->
            hasTailSelfCall selfKey arity thenE || hasTailSelfCall selfKey arity elseE
        | TExprG.Let(_, _, body, _, _) -> hasTailSelfCall selfKey arity body
        | TExprG.Sequential(xs, _, _) when xs.Length > 0 -> hasTailSelfCall selfKey arity xs.[xs.Length - 1]
        | TailSelfCall selfKey arity _ -> true
        | _ -> false
