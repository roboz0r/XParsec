namespace XParsec.FSharp.Codegen.Js

open System.Text

/// `JsProgram → source text`. Step 0a is text only; the V3 source-map writer
/// (generated line/column tracking + VLQ) lands in Step 0b against this same
/// printer. One statement per line, each `;`-terminated.
module JsPrint =

    /// A `"…"`-quoted JS string literal with the metacharacters escaped.
    let private quoteString (s: string) : string =
        let sb = StringBuilder(s.Length + 2)
        sb.Append('"') |> ignore

        for c in s do
            match c with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | '\n' -> sb.Append "\\n" |> ignore
            | '\r' -> sb.Append "\\r" |> ignore
            | '\t' -> sb.Append "\\t" |> ignore
            | c -> sb.Append c |> ignore

        sb.Append('"') |> ignore
        sb.ToString()

    let private literal (l: JsLiteral) : string =
        match l with
        | JsLiteral.String s -> quoteString s

    let rec private expr (e: JsExpr) : string =
        match e with
        | JsExpr.Identifier name -> name
        | JsExpr.Literal l -> literal l
        | JsExpr.Member(object, property, computed) ->
            if computed then
                sprintf "%s[%s]" (expr object) (expr property)
            else
                sprintf "%s.%s" (expr object) (expr property)
        | JsExpr.Call(callee, args) ->
            let argText = args |> List.map expr |> String.concat ", "
            sprintf "%s(%s)" (expr callee) argText

    let private statement (s: JsStatement) : string =
        match s with
        | JsStatement.Expression e -> expr e + ";"
        | JsStatement.Import(specifiers, source) ->
            sprintf "import { %s } from %s;" (String.concat ", " specifiers) (quoteString source)

    /// The emitted ESM source, trailing-newline terminated.
    let print (program: JsProgram) : string =
        (program.Body |> List.map statement |> String.concat "\n") + "\n"
