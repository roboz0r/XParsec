namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Lexer
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
    /// (illegal in F#, so collision-free); binder and uses both go through `binderName`.
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

    /// A binder's NAMING PROJECTIONS → its emitted JS name. Real binders recover the
    /// source name from `Offset` (apostrophes → `_`). A synthetic binder has no source
    /// name, so it is NAMED after `NameIndex` — a spawning offset or a mint counter,
    /// whichever the binder carries — as `_s<n>`, disjoint from `_v<n>`. `NameIndex`, not
    /// `Offset`: a counter-minted binder's offset is negative, which is not a legal
    /// identifier tail.
    ///
    /// Takes a `BinderNaming`, not a `NodeKey`: after freeze a binder's identity is its
    /// slot, and these three projections are the naming DATA that slot carries, so a
    /// caller holding only an id (`exprVarNaming`) can name it. A caller holding a key
    /// projects one with `BinderNaming.ofKey` (`binderNameOf`) — the same three bits, by
    /// construction, that being the naming column's sole constructor.
    let binderName (source: string voption) (n: BinderNaming) : string =
        match source with
        | ValueSome s when
            not n.IsSynthetic
            && n.Offset >= 0
            && n.Offset < s.Length
            && isIdentStart s.[n.Offset]
            ->
            let mutable i = n.Offset

            while i < s.Length && isIdentCont s.[i] do
                i <- i + 1

            jsSafe ((s.Substring(n.Offset, i - n.Offset)).Replace('\'', '_'))
        | _ -> (if n.IsSynthetic then "_s" else "_v") + string n.NameIndex

    /// The emitted JS name of a binder still referenced by `NodeKey` — the side tables,
    /// a `ForTo` loop variable, a flattened parameter's slot. `BinderNaming.ofKey` is
    /// the sole constructor of the pool's naming column, so projecting the key here
    /// gives the same three bits that column holds, without a pool to resolve against.
    let binderNameOf (source: string voption) (k: NodeKey) : string =
        binderName source (BinderNaming.ofKey k)

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
        // An integral literal's TEXT is `IntWidth.render` — its decimal at the width's own
        // signedness — and its JS FORM follows from the width alone:
        //
        //   * `nativeint` / `unativeint` ship no JS repr at all. A program mentioning either
        //     is rejected by `SemanticAnalysis.PlatformTypes` long before emission.
        //   * the wide widths ARE JS BigInts (`prim-types-int.js.fs`: `type int64 =
        //     (# "bigint" #)`), so they emit a BigInt literal (`10n`). A plain number would
        //     silently lose the magnitudes past 2^53 that the width exists to carry — and
        //     would make `10UL / 3UL` true division.
        //   * everything narrower is a plain JS `number`: its range (≤ 2³²-1) fits a double
        //     exactly, so the value emits verbatim (signedness is not a property of the JS
        //     number, only of the width mask its operators carry).
        | TConstValue.Integral(w, _) when IntWidth.isNative w ->
            failwith "EmitJs: nativeint literals have no representation on the target platform"
        | TConstValue.Integral(w, bits) when IntWidth.isWide w ->
            JsExpr.Literal(JsLiteral.BigInt(IntWidth.render w bits), loc)
        | TConstValue.Integral(w, bits) -> JsExpr.Literal(JsLiteral.Number(IntWidth.render w bits), loc)
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

    /// A resolved enum-case literal → its JS object-map value literal (the
    /// frozen object map `{ C1: v1, … }`). A string case is the string verbatim; an
    /// integral case reuses the canonical `constExpr` int formatting (number / bigint
    /// for `int64`) — the single source of truth — so the enum map can't drift from
    /// scalar-`Const` emission. The integral arm always yields a `Literal`
    /// (`constExpr` maps every integral `TConstValue` to one), so a non-`Literal`
    /// here is a producer bug.
    let enumLiteral (lit: TEnumLiteral) : JsLiteral =
        match lit with
        | TEnumLiteral.String s -> JsLiteral.String s
        | TEnumLiteral.Int v ->
            match constExpr v ValueNone with
            | JsExpr.Literal(l, _) -> l
            | other -> failwithf "EmitJs: enum integral literal did not format as a JS literal: %A" other

    // ---- Pure-`let` substitution ---------------------------------------------

    /// A value safe to duplicate at use sites: no side effects, no evaluation-order
    /// dependence. Covers `Const`/`Var` and `ILIntrinsic` templates over pure args.
    let rec isPureValue (e: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind e with
        | ExprShape.Const
        | ExprShape.Var -> true
        // The array intrinsics touch allocated / mutable state, so duplicating one at
        // a use site (what substitution does) is unsound — `newarr` would re-allocate
        // a fresh array each time, and `ldelem`/`ldlen` would re-read after an
        // intervening `stelem`. The scalar `$N` templates remain pure.
        | ExprShape.ILIntrinsic ->
            match TastAccessor.exprILIntrinsicOpCode e with
            | "newarr"
            | "ldelem"
            | "stelem"
            | "ldlen"
            | "ldobj"
            | "ldloca" -> false
            | _ -> TastAccessor.exprChildren e |> Array.forall isPureValue
        // A pure `let` chain is pure when both value and body are — the recursive
        // collapse reduces it to a clean template rather than an IIFE.
        | ExprShape.Let ->
            let l = TastAccessor.exprLet e

            match TastAccessor.patKind l.Binding with
            | PatShape.NamedSimple -> isPureValue l.Value && isPureValue l.Body
            | _ -> false
        | _ -> false

    /// Replace every `Var k` in `e` with `value`. Used only for a pure `value`, so
    /// duplicating it across multiple uses is semantics-preserving.
    let rec substVar (k: NodeKey) (value: TastAccessor.ExprId) (e: TastAccessor.ExprId) : TastAccessor.ExprId =
        match TastAccessor.exprKind e with
        | ExprShape.Var when (TastAccessor.exprVarBinding e).Raw = k.Raw -> value
        | _ -> TastAccessor.mapChildren (substVar k value) e

    /// Is the binder `k` ever assigned (`k <- …`) within `e`? A `let mutable` whose
    /// cell stays a stack local surfaces as a `Let` binder plus `Assignment(Var k, …)`
    /// writes (a closure-captured one is promoted to a ref cell by `RefCellPromotion`
    /// and never reaches here). A mutable binder must NOT be pure-substituted away —
    /// the substitution would replace its reads with the initial value and corrupt the
    /// assignment lhs — and emits as a reassignable `let`, not a `const`.
    let rec isAssignedIn (k: NodeKey) (e: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind e with
        | ExprShape.Assignment ->
            let a = TastAccessor.exprAssignment e

            if
                TastAccessor.exprKind a.Lhs = ExprShape.Var
                && (TastAccessor.exprVarBinding a.Lhs).Raw = k.Raw
            then
                true
            else
                TastAccessor.existsChild (isAssignedIn k) e
        | _ -> TastAccessor.existsChild (isAssignedIn k) e

    /// Does `value` read a variable that `body` later reassigns? F# `let x = value`
    /// takes a *snapshot* of `value` at the bind point; substituting `value` into `x`'s
    /// uses re-reads it at each use, so if `value` reads a var that `body` mutates
    /// (`let x = m … m <- e … x`), the uses would observe the post-mutation value
    /// instead of the snapshot. `isPureValue` alone is not enough — a `Var` read is
    /// pure/effect-free but not *stable* across an intervening assignment. Only the
    /// duplicating `NamedSimple` substitution needs this; the `Wildcard` case drops the
    /// value unread, so a non-stable-but-pure value is safe to discard there.
    let rec valueReadsAssignedIn (body: TastAccessor.ExprId) (value: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind value with
        | ExprShape.Var -> isAssignedIn (TastAccessor.exprVarBinding value) body
        | _ -> TastAccessor.existsChild (valueReadsAssignedIn body) value

    /// A `NamedSimple` `let` whose value is safe to inline into its uses, reduced to
    /// its substituted body. The value must be duplicable (`isPureValue`), the binder
    /// never reassigned in the body (else the substitution would corrupt the assignment
    /// lhs and the binder must stay a real reassignable `let`), and the value must not
    /// read a var the body later mutates (F#'s bind-point snapshot — see
    /// `valueReadsAssignedIn`). Every `buildExpr`/`buildStatements`/`buildTailBody` site
    /// that collapses a pure `let` matches through here so the guard lives in one place.
    let (|InlinableLet|_|) (e: TastAccessor.ExprId) : TastAccessor.ExprId option =
        match TastAccessor.exprKind e with
        | ExprShape.Let ->
            let l = TastAccessor.exprLet e

            match TastAccessor.patKind l.Binding with
            | PatShape.NamedSimple ->
                let k = (TastAccessor.patBinder l.Binding).Value

                if
                    isPureValue l.Value
                    && not (isAssignedIn k l.Body)
                    && not (valueReadsAssignedIn l.Body l.Value)
                then
                    Some(substVar k l.Value l.Body)
                else
                    None
            | _ -> None
        | _ -> None

    // ---- Functions -----------------------------------------------------------

    /// A lambda parameter → its JS binding form. Wildcards get fresh unused names (JS
    /// array holes shift later positions); a tuple becomes `[a, b]` destructuring.
    // TODO: tuple leaves smuggle a destructuring pattern through a `string` (emitted
    // verbatim). `Arrow.parameters` wants a real `JsPattern` for object-destructuring.
    let rec lambdaParamName (source: string voption) (p: TastAccessor.PatId) : string =
        match TastAccessor.patKind p with
        | PatShape.NamedSimple -> binderName source (TastAccessor.patBinderNaming p).Value
        | PatShape.Wildcard -> "_w" + string (TastAccessor.patTok p).StartIndex
        | PatShape.Const when TastAccessor.patConstValue p = TConstValue.Unit ->
            "_u" + string (TastAccessor.patTok p).StartIndex
        | PatShape.Tuple ->
            let parts = TastAccessor.patChildren p |> Array.map (lambdaParamName source)
            "[" + System.String.Join(", ", parts) + "]"
        | _ -> failwithf "EmitJs: unsupported lambda parameter pattern %A" p

    /// Peel a curried `Lambda` chain into its parameter names and the innermost
    /// body. The inverse of the nested-arrow emission.
    let rec peelArrow (source: string voption) (e: TastAccessor.ExprId) : string list * TastAccessor.ExprId =
        match TastAccessor.exprKind e with
        | ExprShape.Lambda ->
            let l = TastAccessor.exprLambda e
            let names, inner = peelArrow source l.Body
            lambdaParamName source l.Param :: names, inner
        | _ -> [], e

    /// `["a"; "b"]` → `(a) => (b) => <innermost>`. Shared by lambda and member emission.
    let rec nestUnaryArrows (loc: JsLoc voption) (names: string list) (innermost: JsFnBody) : JsExpr =
        match names with
        | [ last ] -> JsExpr.Arrow([ last ], innermost, loc)
        | n :: rest -> JsExpr.Arrow([ n ], JsFnBody.Expr(nestUnaryArrows loc rest innermost), loc)
        | [] -> failwith "EmitJs: nestUnaryArrows on an empty parameter list"

    /// Active pattern for a fully-saturated tail self-call — shared by the detector
    /// (`hasTailSelfCall`) and rewriter (`buildTailBody`) so they can't drift.
    let (|TailSelfCall|_|) (selfKey: NodeKey) (arity: int) (e: TastAccessor.ExprId) : TastAccessor.ExprId list option =
        match TastAccessor.exprKind e with
        | ExprShape.App ->
            match TastAccessor.collectSpine [] e with
            | head, spine when
                TastAccessor.exprKind head = ExprShape.Var
                && (TastAccessor.exprVarBinding head).Raw = selfKey.Raw
                && List.length spine = arity
                ->
                Some [ for (a, _, _) in spine -> a ]
            | _ -> None
        | _ -> None

    /// Is `e`, in tail position, a fully-saturated self-call of the function
    /// bound to `selfKey` (arity `arity`)? Recurses through the constructs that
    /// preserve tail position (`if`/`let`/`Sequential`-tail); a saturated tail
    /// self-call is what the trampoline rewrites to param mutation + `continue`.
    let rec hasTailSelfCall (selfKey: NodeKey) (arity: int) (e: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind e with
        | ExprShape.IfThenElse ->
            let i = TastAccessor.exprIfThenElse e

            hasTailSelfCall selfKey arity i.ThenExpr
            || hasTailSelfCall selfKey arity i.ElseExpr
        | ExprShape.Let -> hasTailSelfCall selfKey arity (TastAccessor.exprLet e).Body
        | ExprShape.Sequential ->
            let xs = TastAccessor.exprChildren e
            xs.Length > 0 && hasTailSelfCall selfKey arity xs.[xs.Length - 1]
        | ExprShape.App ->
            match e with
            | TailSelfCall selfKey arity _ -> true
            | _ -> false
        | _ -> false
