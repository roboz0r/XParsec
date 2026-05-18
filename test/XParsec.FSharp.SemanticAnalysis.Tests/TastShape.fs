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
        "op_ComposeRight", ">>"
        "op_ComposeLeft", "<<"
        "op_PipeRight", "|>"
        "op_PipeLeft", "<|"
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
        | TExpr.Const(TConstValue.Int64 n, _) ->
            push (string n)
            push "L"
        | TExpr.Const(TConstValue.Byte n, _) ->
            push (string n)
            push "uy"
        | TExpr.Const(TConstValue.Float n, _) -> push (n.ToString(System.Globalization.CultureInfo.InvariantCulture))
        | TExpr.Const(TConstValue.Bool true, _) -> push "true"
        | TExpr.Const(TConstValue.Bool false, _) -> push "false"
        | TExpr.Const(TConstValue.Unit, _) -> push "()"
        | TExpr.Const(TConstValue.String s, _) ->
            push "\""
            push s
            push "\""
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
            this.Pat p
            push " -> "
            this.Expr body

        | TExpr.Let(p, v, b, _) ->
            push "let "
            this.Pat p
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

        | TExpr.While(c, b, _) ->
            push "while "
            this.Expr c
            push " do "
            this.Expr b

        | TExpr.ForTo(v, s, e2, b, _) ->
            push "for "
            push (nameOf v)
            push " = "
            this.Expr s
            push " to "
            this.Expr e2
            push " do "
            this.Expr b

        | TExpr.ForIn(p, src, b, _) ->
            push "for "
            this.Pat p
            push " in "
            this.Expr src
            push " do "
            this.Expr b

        | TExpr.Match(scrutinee, arms, _) ->
            push "match "
            this.Expr scrutinee
            push " with"

            for arm in arms do
                push " | "
                this.Pat arm.Pat

                match arm.Guard with
                | Some g ->
                    push " when "
                    this.Expr g
                | None -> ()

                push " -> "
                this.Expr arm.Body

        | TExpr.TryWith(body, arms, _) ->
            push "try "
            this.Expr body
            push " with"

            for arm in arms do
                push " | "
                this.Pat arm.Pat

                match arm.Guard with
                | Some g ->
                    push " when "
                    this.Expr g
                | None -> ()

                push " -> "
                this.Expr arm.Body

        | TExpr.TryFinally(body, cleanup, _) ->
            push "try "
            this.Expr body
            push " finally "
            this.Expr cleanup

        | TExpr.Assignment(lhs, rhs, _) ->
            this.Expr lhs
            push " <- "
            this.Expr rhs

        | TExpr.Null _ -> push "null"

        | TExpr.Range(s, stepOpt, e2, _) ->
            push "("
            this.Expr s
            push ".."

            match stepOpt with
            | Some step ->
                this.Expr step
                push ".."
            | None -> ()

            this.Expr e2
            push ")"

        | TExpr.RecordCons(fields, _) ->
            push "{ "

            fields
            |> List.iteri (fun i (n, v) ->
                if i > 0 then
                    push "; "

                push n
                push " = "
                this.Expr v
            )

            push " }"

        | TExpr.RecordClone(src, overrides, _) ->
            push "{ "
            this.Expr src
            push " with "

            overrides
            |> List.iteri (fun i (n, v) ->
                if i > 0 then
                    push "; "

                push n
                push " = "
                this.Expr v
            )

            push " }"

        | TExpr.FieldGet(receiver, name, _) ->
            this.Expr receiver
            push "."
            push name

        | TExpr.FieldSet(receiver, name, value, _) ->
            this.Expr receiver
            push "."
            push name
            push " <- "
            this.Expr value

    member this.Pat(p: TPat) : unit =
        match p with
        | TPat.NamedSimple(k, _) -> push (nameOf k)
        | TPat.Wildcard _ -> push "_"
        | TPat.Const(TConstValue.Int n, _) -> push (string n)
        | TPat.Const(TConstValue.Int64 n, _) ->
            push (string n)
            push "L"
        | TPat.Const(TConstValue.Byte n, _) ->
            push (string n)
            push "uy"
        | TPat.Const(TConstValue.Float n, _) -> push (n.ToString(System.Globalization.CultureInfo.InvariantCulture))
        | TPat.Const(TConstValue.Bool true, _) -> push "true"
        | TPat.Const(TConstValue.Bool false, _) -> push "false"
        | TPat.Const(TConstValue.Unit, _) -> push "()"
        | TPat.Const(TConstValue.String s, _) ->
            push "\""
            push s
            push "\""
        | TPat.Tuple(items, _) ->
            push "("

            items
            |> List.iteri (fun i x ->
                if i > 0 then
                    push ", "

                this.Pat x
            )

            push ")"

        | TPat.Record(fields, _) ->
            push "{ "

            fields
            |> List.iteri (fun i (n, p) ->
                if i > 0 then
                    push "; "

                push n
                push " = "
                this.Pat p
            )

            push " }"

    member this.Decl(d: TDecl) : unit =
        match d with
        | TDecl.Let(p, v, _) ->
            push "let "
            this.Pat p
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
