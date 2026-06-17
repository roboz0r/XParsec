namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Leaf helpers for the walker — no `WalkCtx`, no back-calls into expression
/// emission. Covers identifier naming, literal formatting, pure-`let` substitution,
/// and curried-arrow / tail-self-call shaping. `EmitJs` opens this module.
module JsEmitHelpers =

    // ---- Variable names ------------------------------------------------------

    let isIdentStart (c: char) = System.Char.IsLetter c || c = '_'

    let isIdentCont (c: char) =
        System.Char.IsLetterOrDigit c || c = '_' || c = '\''

    /// JS reserved words that are legal F# identifiers. A collision is suffixed with `$`
    /// (illegal in F#, so collision-free); binder and uses both go through `identName`.
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

    /// `NodeKey` → JS identifier. Real binders recover the source name from `Offset`
    /// (apostrophes → `_`). Synthetic keys (`IsSynthetic`) carry a per-build counter,
    /// NOT a source position — they get `_s<off>` to stay disjoint from `_v<off>`.
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

    /// A scalar `Const` value → its JS expression. Shared by `buildExpr` and `Const` patterns.
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
        | TConstValue.Decimal _ -> failwithf "EmitJs: decimal literals are not supported"

    // ---- Pure-`let` substitution ---------------------------------------------

    /// A value safe to duplicate at use sites: no side effects, no evaluation-order
    /// dependence. Covers `Const`/`Var` and `ILIntrinsic` templates over pure args.
    let rec isPureValue (e: Frozen.TExpr) : bool =
        match e with
        | TExprG.Const _
        | TExprG.Var _ -> true
        | TExprG.ILIntrinsic(_, _, args, _, _) -> EqArray.toList args |> List.forall isPureValue
        // A pure `let` chain is pure when both value and body are — the recursive
        // collapse reduces it to a clean template rather than an IIFE.
        | TExprG.Let(TPatG.NamedSimple _, value, body, _, _) -> isPureValue value && isPureValue body
        | _ -> false

    /// Replace every `Var k` in `e` with `value`. Used only for a pure `value`, so
    /// duplicating it across multiple uses is semantics-preserving.
    let rec substVar (k: NodeKey) (value: Frozen.TExpr) (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.Var(vk, _, _) when vk.Raw = k.Raw -> value
        | _ -> TastLower.mapChildren (substVar k value) e

    // ---- Functions -----------------------------------------------------------

    /// A lambda parameter → its JS binding form. Wildcards get fresh unused names (JS
    /// array holes shift later positions); a tuple becomes `[a, b]` destructuring.
    // TODO: tuple leaves smuggle a destructuring pattern through a `string` (emitted
    // verbatim). `Arrow.parameters` wants a real `JsPattern` for object-destructuring.
    let rec lambdaParamName (source: string voption) (p: Frozen.TPat) : string =
        match p with
        | TPatG.NamedSimple(k, _, _) -> identName source k
        | TPatG.Wildcard(_, tok) -> "_w" + string tok.StartIndex
        | TPatG.Const(TConstValue.Unit, _, tok) -> "_u" + string tok.StartIndex
        | TPatG.Tuple(items, _, _) ->
            let parts = EqArray.toList items |> List.map (lambdaParamName source)
            "[" + System.String.Join(", ", parts) + "]"
        | other -> failwithf "EmitJs: unsupported lambda parameter pattern %A" other

    /// Peel a curried `Lambda` chain into its parameter names and the innermost
    /// body. The inverse of the nested-arrow emission.
    let rec peelArrow (source: string voption) (e: Frozen.TExpr) : string list * Frozen.TExpr =
        match e with
        | TExprG.Lambda(p, body, _, _) ->
            let names, inner = peelArrow source body
            lambdaParamName source p :: names, inner
        | _ -> [], e

    /// `["a"; "b"]` → `(a) => (b) => <innermost>`. Shared by lambda and member emission.
    let rec nestUnaryArrows (loc: JsLoc voption) (names: string list) (innermost: JsFnBody) : JsExpr =
        match names with
        | [ last ] -> JsExpr.Arrow([ last ], innermost, loc)
        | n :: rest -> JsExpr.Arrow([ n ], JsFnBody.Expr(nestUnaryArrows loc rest innermost), loc)
        | [] -> failwith "EmitJs: nestUnaryArrows on an empty parameter list"

    /// Active pattern for a fully-saturated tail self-call — shared by the detector
    /// (`hasTailSelfCall`) and rewriter (`buildTailBody`) so they can't drift.
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
