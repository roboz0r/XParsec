module XParsec.FSharp.SemanticAnalysis.Tests.TastShape

open System.Collections.Generic
open System.Text
open XParsec.FSharp.SemanticAnalysis

// Test DSL for asserting TAST shape without manually nesting pattern matches
// on every TExpr.App / TExpr.Lambda / TExpr.External tuple.
//
// `prettyDecl tast.Decls.[0]` renders as F#-like text:
//   "let v0 = fun v1 -> ((-v1) + 0)"
//
// `v0`, `v1`, … are short fresh names assigned to NodeKeys in first-encounter
// order, so test strings are stable under source-offset changes that don't
// reorder bindings. Types and offsets are dropped — assert those separately
// with `declType` or `Expect.equal someTy …`.
//
// Known operators (op_Addition, op_UnaryNegation, …) render as their source
// form (+, -, …) so the output reads like the F# the user actually wrote.

/// Compiled-name → source form. Used by the prefix/infix detector so binary
/// ops render as `(L + R)` and unary minus as `(-X)`. Anything not in this
/// map renders as `(External arg arg)` — fine for diagnostic purposes, plain
/// ugly for high-frequency operators.
let private opSym =
    [
        "op_Addition", "+"
        "op_Subtraction", "-"
        "op_Multiply", "*"
        "op_Division", "/"
        "op_Modulus", "%"
        "op_LessThan", "<"
        "op_GreaterThan", ">"
        "op_LessThanOrEqual", "<="
        "op_GreaterThanOrEqual", ">="
        "op_Equality", "="
        "op_Inequality", "<>"
        "op_BooleanAnd", "&&"
        "op_BooleanOr", "||"
    ]
    |> Map.ofList

let private prefixSym = Map.ofList [ "op_UnaryNegation", "-" ]

let private (|InfixOp|_|) (e: TExpr) =
    match e with
    | TExpr.App(TExpr.App(TExpr.External(name, _), left, _), right, _) ->
        match Map.tryFind name opSym with
        | Some sym -> Some(sym, left, right)
        | None -> None
    | _ -> None

let private (|PrefixOp|_|) (e: TExpr) =
    match e with
    | TExpr.App(TExpr.External(name, _), operand, _) ->
        match Map.tryFind name prefixSym with
        | Some sym -> Some(sym, operand)
        | None -> None
    | _ -> None

type private Renderer() =
    let map = Dictionary<NodeKey, string>()
    let mutable counter = 0
    let sb = StringBuilder()

    let nameOf (k: NodeKey) =
        match map.TryGetValue k with
        | true, n -> n
        | false, _ ->
            let n = sprintf "v%d" counter
            counter <- counter + 1
            map.[k] <- n
            n

    let push (s: string) = sb.Append(s) |> ignore

    member _.Result = sb.ToString()

    member this.Expr(e: TExpr) : unit =
        match e with
        | TExpr.Const(TConstValue.Int n, _) -> push (string n)
        | TExpr.Const(TConstValue.Bool true, _) -> push "true"
        | TExpr.Const(TConstValue.Bool false, _) -> push "false"
        | TExpr.Const(TConstValue.Unit, _) -> push "()"
        | TExpr.Var(k, _) -> push (nameOf k)
        | TExpr.External(name, _) -> push name

        | InfixOp(sym, l, r) ->
            push "("
            this.Expr l
            push " "
            push sym
            push " "
            this.Expr r
            push ")"

        | PrefixOp(sym, x) ->
            push "("
            push sym
            this.Expr x
            push ")"

        | TExpr.App(fn, arg, _) ->
            push "("
            this.Expr fn
            push " "
            this.Expr arg
            push ")"

        | TExpr.Lambda(p, body, _) ->
            push "fun "
            push (nameOf p)
            push " -> "
            this.Expr body

        | TExpr.Let(k, v, b, _) ->
            push "let "
            push (nameOf k)
            push " = "
            this.Expr v
            push " in "
            this.Expr b

        | TExpr.IfThenElse(c, t, e, _) ->
            push "if "
            this.Expr c
            push " then "
            this.Expr t
            push " else "
            this.Expr e

        | TExpr.Tuple(items, _) ->
            push "("

            items
            |> List.iteri (fun i x ->
                if i > 0 then
                    push ", "

                this.Expr x
            )

            push ")"

        | TExpr.Sequential(items, _) ->
            push "("

            items
            |> List.iteri (fun i x ->
                if i > 0 then
                    push "; "

                this.Expr x
            )

            push ")"

    member this.Decl(d: TDecl) : unit =
        match d with
        | TDecl.Let(k, v, _) ->
            push "let "
            push (nameOf k)
            push " = "
            this.Expr v
        | TDecl.Expression(e, _) ->
            push "do "
            this.Expr e

let prettyExpr (e: TExpr) : string =
    let r = Renderer()
    r.Expr e
    r.Result

let prettyDecl (d: TDecl) : string =
    let r = Renderer()
    r.Decl d
    r.Result
