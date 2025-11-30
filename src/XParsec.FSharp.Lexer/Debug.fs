module rec XParsec.FSharp.Debug

open System
open System.CodeDom.Compiler

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let printTokenMin (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    match token.Index with
    | TokenIndex.Regular iT -> tw.Write($"{token.PositionedToken}({iT}<token>)")
    | TokenIndex.Virtual -> tw.Write($"{token.PositionedToken}(<virt>)")

let printTokenFull (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (synTok: SyntaxToken) =
    match synTok.Index with
    | TokenIndex.Regular iT ->
        let t1 = lexed.Tokens.[iT + 1<_>]
        let i = synTok.StartIndex
        let i1 = t1.StartIndex
        let iEnd = i1 - 1L
        let len = iEnd - i

        let tokenStr =
            if len > 10L then
                input.[int i .. int (i - 10L)] + "..."
            else
                input.[int i .. int (i1 - 1L)]

        let tokenStr =
            tokenStr.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

        tw.Write($"{synTok.PositionedToken}({iT}<token>) '{tokenStr}'")
    | TokenIndex.Virtual -> tw.Write($"{synTok.PositionedToken}(<virt>)")

let printConstant (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (x: Constant<SyntaxToken>) =
    match x with
    | Constant.Literal value -> printTokenFull tw input lexed value
    | Constant.MeasuredLiteral(value, lAngle, measure, rAngle) -> failwith "Not implemented"

let printPat (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (pat: Pat<SyntaxToken>) =
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

let printValueDefn (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (valueDefn: ValueDefn<SyntaxToken>) =
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

let printExpr (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (expr: Expr<SyntaxToken>) =
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
    | Expr.StructTuple(kw, lParen, elements, rParen) ->
        tw.WriteLine("StructTuple:")
        tw.Indent <- tw.Indent + 1

        for elem in elements do
            printExpr tw input lexed elem

        tw.Indent <- tw.Indent - 1
    | Expr.List(lBracket, elements, rBracket) ->
        tw.Write("List: ")
        printTokenMin tw input lexed lBracket
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1

        for elem in elements do
            printExpr tw input lexed elem

        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed rBracket
        tw.WriteLine()
    | Expr.Array(lBracket, elements, rBracket) ->
        tw.Write("Array: ")
        printTokenMin tw input lexed lBracket
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1

        for elem in elements do
            printExpr tw input lexed elem

        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed rBracket
        tw.WriteLine()
    | _ -> failwith "Not implemented"
