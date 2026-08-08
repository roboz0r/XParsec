namespace XParsec.FSharp.Codegen.Js

open System.Globalization
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Leaf helpers for the walker: no `WalkCtx` parameter, no back-calls into expression emission.
module JsEmitHelpers =

    // ---- Variable names ------------------------------------------------------

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

    /// `x'` → `x_`, and so for every other character JS rejects; a leading digit (legal only in
    /// a quoted F# identifier) takes a `_` prefix. NOT injective: `x'` and `x_` both give `x_`.
    let jsIdent (name: string) : string =
        let legal (c: char) =
            System.Char.IsLetterOrDigit c || c = '_' || c = '$'

        let mangled = String.map (fun c -> if legal c then c else '_') name

        if mangled.Length > 0 && System.Char.IsDigit mangled.[0] then
            "_" + mangled
        else
            mangled

    let boundVarName (n: BoundVarNaming) : string =
        match n with
        | BoundVarNaming.Source name -> jsSafe (jsIdent name)
        | BoundVarNaming.Minted(BoundVarId slot) -> "_s" + string slot

    let boundVarNameOf (pool: PoolBuilder) (b: BoundVarId) : string =
        boundVarName (TastPoolBuilder.boundVarNaming pool b)

    let freshTemp (pool: PoolBuilder) (prefix: string) : string =
        let (BoundVarId slot) = TastPoolBuilder.mintBoundVar pool
        prefix + string slot

    // ---- Scalar constants ----------------------------------------------------

    /// A `double` as a JS `number` literal; `"R"` so the text reads back as the same value.
    let formatDouble (d: double) : string =
        if System.Double.IsNaN d then "NaN"
        elif System.Double.IsPositiveInfinity d then "Infinity"
        elif System.Double.IsNegativeInfinity d then "-Infinity"
        else d.ToString("R", CultureInfo.InvariantCulture)

    /// A scalar `Const` value → its JS expression.
    let constExpr (value: TConstValue) (loc: JsLoc voption) : JsExpr =
        match value with
        | TConstValue.Integral(w, _) when IntWidth.isNative w ->
            failwith "EmitJs: nativeint literals have no representation on the target platform"
        // `10L` → `10n`: a plain number would lose the magnitudes past 2^53 the width carries.
        | TConstValue.Integral(w, bits) when IntWidth.isWide w ->
            JsExpr.Literal(JsLiteral.BigInt(IntWidth.render w bits), loc)
        | TConstValue.Integral(w, bits) -> JsExpr.Literal(JsLiteral.Number(IntWidth.render w bits), loc)
        | TConstValue.Float d -> JsExpr.Literal(JsLiteral.Number(formatDouble d), loc)
        | TConstValue.Float32 f -> JsExpr.Literal(JsLiteral.Number(formatDouble (float f)), loc)
        | TConstValue.Bool b -> JsExpr.Literal(JsLiteral.Boolean b, loc)
        // JS has no char type; a `char` is a length-1 string.
        | TConstValue.Char c -> JsExpr.Literal(JsLiteral.String(string c), loc)
        | TConstValue.String s -> JsExpr.Literal(JsLiteral.String s, loc)
        // JS has no unit value; `()` is `undefined`.
        | TConstValue.Unit -> JsExpr.Identifier("undefined", loc)
        | TConstValue.Decimal _ -> failwithf "EmitJs: decimal literals are not supported"

    /// A resolved enum case → its value in the emitted frozen map `{ C1: v1, … }`.
    let enumLiteral (lit: TEnumLiteral) : JsLiteral =
        match lit with
        | TEnumLiteral.String s -> JsLiteral.String s
        | TEnumLiteral.Int v ->
            match constExpr v ValueNone with
            | JsExpr.Literal(l, _) -> l
            | other -> failwithf "EmitJs: enum integral literal did not format as a JS literal: %A" other

    // ---- Pure-`let` substitution ---------------------------------------------

    /// Safe to DUPLICATE at a use site: no side effects, no evaluation-order dependence.
    let rec isPureValue (e: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind e with
        | ExprShape.Const
        | ExprShape.Var -> true
        // Duplicating one of these would re-allocate (`newarr`) or re-read after an
        // intervening `stelem`.
        | ExprShape.ILIntrinsic ->
            match TastAccessor.exprILIntrinsicOpCode e with
            | "newarr"
            | "ldelem"
            | "stelem"
            | "ldlen"
            | "ldobj"
            | "ldloca" -> false
            | _ -> TastAccessor.exprChildren e |> Array.forall isPureValue
        | ExprShape.Let ->
            let l = TastAccessor.exprLet e

            match TastAccessor.patKind l.Pattern with
            | PatShape.NamedSimple -> isPureValue l.Value && isPureValue l.Body
            | _ -> false
        | _ -> false

    /// Replaces every `Var k` in `e` with `value`; sound only where `value` is duplicable.
    let rec substVar
        (derivation: InlineExpand.Derivation)
        (k: BoundVarId)
        (value: TastAccessor.ExprId)
        (e: TastAccessor.ExprId)
        : TastAccessor.ExprId =
        match TastAccessor.exprKind e with
        | ExprShape.Var when TastAccessor.exprVarBoundVar e = k -> value
        | _ ->
            let result = TastAccessor.mapChildren (substVar derivation k value) e
            InlineExpand.Derivation.authored derivation e result
            result

    let rec isAssignedIn (k: BoundVarId) (e: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind e with
        | ExprShape.Assignment ->
            let a = TastAccessor.exprAssignment e

            if
                TastAccessor.exprKind a.Lhs = ExprShape.Var
                && TastAccessor.exprVarBoundVar a.Lhs = k
            then
                true
            else
                TastAccessor.existsChild (isAssignedIn k) e
        | _ -> TastAccessor.existsChild (isAssignedIn k) e

    /// `let x = value` SNAPSHOTS at the bind point, but substituting re-reads at every use: in
    /// `let x = m in (m <- e; x)` the uses would see the post-assignment `m`. Purity is not enough.
    let rec valueReadsAssignedIn (body: TastAccessor.ExprId) (value: TastAccessor.ExprId) : bool =
        match TastAccessor.exprKind value with
        | ExprShape.Var -> isAssignedIn (TastAccessor.exprVarBoundVar value) body
        | _ -> TastAccessor.existsChild (valueReadsAssignedIn body) value

    /// A `let x = v` whose `v` is safe to inline, reduced to its substituted body.
    let reduceInlinableLet (derivation: InlineExpand.Derivation) (e: TastAccessor.ExprId) : TastAccessor.ExprId option =
        match TastAccessor.exprKind e with
        | ExprShape.Let ->
            let l = TastAccessor.exprLet e

            match l.Pattern with
            | TastAccessor.PNamed k ->
                if
                    isPureValue l.Value
                    && not (isAssignedIn k l.Body)
                    && not (valueReadsAssignedIn l.Body l.Value)
                then
                    Some(substVar derivation k l.Value l.Body)
                else
                    None
            | _ -> None
        | _ -> None

    // ---- Functions -----------------------------------------------------------

    /// A lambda parameter → its JS binding text: a name, or `[a, b]` for a tuple.
    // TODO: the tuple case smuggles destructuring syntax through a `string`, emitted verbatim;
    // `JsExpr.Arrow`'s parameter list wants a real pattern type for object-destructuring.
    let rec lambdaParamName (pool: PoolBuilder) (p: TastAccessor.PatId) : string =
        match p with
        | TastAccessor.PNamedNaming naming -> boundVarName naming
        | _ ->
            match TastAccessor.patKind p with
            | PatShape.Wildcard -> freshTemp pool "_w"
            | PatShape.Const when TastAccessor.patConstValue p = TConstValue.Unit -> freshTemp pool "_u"
            | PatShape.Tuple ->
                let parts = TastAccessor.patChildren p |> Array.map (lambdaParamName pool)
                "[" + System.String.Join(", ", parts) + "]"
            | _ -> failwithf "EmitJs: unsupported lambda parameter pattern %A" p

    let rec peelLambdas (pool: PoolBuilder) (e: TastAccessor.ExprId) : string list * TastAccessor.ExprId =
        match TastAccessor.exprKind e with
        | ExprShape.Lambda ->
            let l = TastAccessor.exprLambda e
            // Minted left to right, so temporaries ascend across the emitted arrows.
            let name = lambdaParamName pool l.Param
            let names, inner = peelLambdas pool l.Body
            name :: names, inner
        | _ -> [], e

    /// `["a"; "b"]` → `(a) => (b) => <innermost>`.
    let rec nestUnaryArrows (loc: JsLoc voption) (names: string list) (innermost: JsFnBody) : JsExpr =
        match names with
        | [ last ] -> JsExpr.Arrow([ last ], innermost, loc)
        | n :: rest -> JsExpr.Arrow([ n ], JsFnBody.Expr(nestUnaryArrows loc rest innermost), loc)
        | [] -> failwith "EmitJs: nestUnaryArrows on an empty parameter list"

    let (|TailSelfCall|_|)
        (selfKey: BoundVarId)
        (arity: int)
        (e: TastAccessor.ExprId)
        : TastAccessor.ExprId list option =
        match TastAccessor.exprKind e with
        | ExprShape.App ->
            match TastAccessor.collectAppChain [] e with
            | fn, appArgs when
                TastAccessor.exprKind fn = ExprShape.Var
                && TastAccessor.exprVarBoundVar fn = selfKey
                && List.length appArgs = arity
                ->
                Some [ for (a, _, _) in appArgs -> a ]
            | _ -> None
        | _ -> None

    let rec hasTailSelfCall (selfKey: BoundVarId) (arity: int) (e: TastAccessor.ExprId) : bool =
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
