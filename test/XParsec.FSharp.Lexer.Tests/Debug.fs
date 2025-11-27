module rec XParsec.FSharp.Parser.Debug

open System
open System.CodeDom.Compiler

open XParsec.FSharp.Lexer

let private getOperatorText (input: string) (lexed: Lexed) (token: SyntaxToken) =
    match token.Index with
    | TokenIndex.Regular iT ->
        let i = int token.StartIndex

        let i1 =
            if iT + 1<token> < lexed.Tokens.LengthM then
                int lexed.Tokens.[iT + 1<token>].StartIndex
            else
                input.Length

        let len = i1 - i
        input.Substring(i, len)
    | TokenIndex.Virtual -> sprintf "<%O>" token.Token // For virtual tokens, just print the token type

let printTokenMin (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    match token.Index with
    | TokenIndex.Regular iT -> tw.Write($"{getOperatorText input lexed token}({iT})")
    | TokenIndex.Virtual -> tw.Write($"{getOperatorText input lexed token}")

let printTokenFull (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    match token.Index with
    | TokenIndex.Regular iT ->
        let i = int token.StartIndex

        let i1 =
            if iT + 1<token> < lexed.Tokens.LengthM then
                int lexed.Tokens.[iT + 1<token>].StartIndex
            else
                input.Length

        let len = i1 - i
        let tokenStr = input.Substring(i, len)
        tw.Write($"{tokenStr}({iT}) {token.Token} (0x%04x{uint16 token.Token.WithoutCommentFlags})")
    | TokenIndex.Virtual -> tw.Write($"{getOperatorText input lexed token}")

let printConstant
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (x: XParsec.FSharp.Parser.Constant<SyntaxToken>)
    =
    match x with
    | Constant.Literal value -> printTokenFull tw input lexed value
    | Constant.MeasuredLiteral(value, lAngle, measure, rAngle) -> failwith "Not implemented"

let printPat (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (pat: XParsec.FSharp.Parser.Pat<SyntaxToken>) =
    match pat with
    | Pat.Const value ->
        tw.Write("Pat.Const: ")
        printTokenFull tw input lexed value
        tw.WriteLine()
    | Pat.NamedSimple ident ->
        tw.Write("Pat.NamedSimple: ")
        printTokenFull tw input lexed ident
        tw.WriteLine()
    | _ -> failwith "Not implemented"

let printValueDefn
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (valueDefn: XParsec.FSharp.Parser.ValueDefn<SyntaxToken>)
    =
    match valueDefn with
    | ValueDefn(mutableToken, access, pat, typarDefns, returnType, equals, expr) ->
        match mutableToken with
        | ValueSome t ->
            printTokenMin tw input lexed t
            tw.Write(" ")
        | ValueNone -> ()

        match access with
        | ValueSome a ->
            printTokenMin tw input lexed a
            tw.Write(" ")
        | ValueNone -> ()

        printPat tw input lexed pat

        match typarDefns with
        | ValueSome typars -> failwith "Not implemented"
        | ValueNone -> ()

        match returnType with
        | ValueSome returnType -> failwith "Not implemented"
        | ValueNone -> ()

        printTokenMin tw input lexed equals
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed expr
        tw.Indent <- tw.Indent - 1

let printExpr (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (expr: XParsec.FSharp.Parser.Expr<SyntaxToken>) =
    match expr with
    | Expr.Const value ->
        tw.Write("Const: ")
        printConstant tw input lexed value
        tw.WriteLine()
    | Expr.Ident ident ->
        tw.Write("Ident: ")
        printTokenFull tw input lexed ident
        tw.WriteLine()
    | Expr.LetValue(letToken, valueDefn, inToken, body) ->
        tw.Write("LetValue: ")
        printTokenMin tw input lexed letToken
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printValueDefn tw input lexed valueDefn
        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed inToken
        tw.WriteLine()
        tw.WriteLine("Body:")
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed body
        tw.Indent <- tw.Indent - 1
    | Expr.InfixApp(left, op, right) ->
        tw.Write("InfixApp: ")
        printTokenMin tw input lexed op
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed left
        printExpr tw input lexed right
        tw.Indent <- tw.Indent - 1
    | Expr.Tuple elements ->
        tw.WriteLine("Tuple:")
        tw.Indent <- tw.Indent + 1

        for elem in elements do
            printExpr tw input lexed elem

        tw.Indent <- tw.Indent - 1
    | _ -> failwith "Not implemented"
