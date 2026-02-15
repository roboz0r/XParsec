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
        let iEnd = i1 - 1
        let len = iEnd - i

        let tokenStr =
            if len > 10 then
                input.[int i .. int (i - 10)] + "..."
            else
                input.[int i .. int (i1 - 1)]

        let tokenStr =
            tokenStr.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

        tw.Write($"{synTok.PositionedToken}({iT}<token>) '{tokenStr}'")
    | TokenIndex.Virtual -> tw.Write($"{synTok.PositionedToken}(<virt>)")

let printConstant (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (x: Constant<SyntaxToken>) =
    match x with
    | Constant.Literal value -> printTokenFull tw input lexed value
    | Constant.MeasuredLiteral(value, lAngle, measure, rAngle) -> failwith "Not implemented"

let rec printIdentOrOp (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (identOrOp: IdentOrOp<SyntaxToken>) =
    match identOrOp with
    | IdentOrOp.Ident ident ->
        tw.Write("Ident: ")
        printTokenFull tw input lexed ident
        tw.WriteLine()
    | IdentOrOp.ParenOp(lParen, opName, rParen) ->
        tw.Write("ParenOp: ")
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printTokenMin tw input lexed lParen
        tw.WriteLine()
        printOpName tw input lexed opName
        printTokenMin tw input lexed rParen
        tw.Indent <- tw.Indent - 1
        tw.WriteLine()

    | IdentOrOp.StarOp(lParen, star, rParen) -> tw.Write("StarOp: ")

and printOpName (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (opName: OpName<SyntaxToken>) =
    match opName with
    | OpName.SymbolicOp op ->
        tw.Write("SymbolicOp: ")
        printTokenFull tw input lexed op
        tw.WriteLine()
    | OpName.RangeOp rangeOp ->
        tw.Write("RangeOp: ")
        printRangeOpName tw input lexed rangeOp
    | OpName.ActivePatternOp activePatternOp ->
        tw.Write("ActivePatternOp: ")
        printActivePatternOpName tw input lexed activePatternOp

and printRangeOpName (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (rangeOpName: RangeOpName<SyntaxToken>) =
    match rangeOpName with
    | RangeOpName.DotDot dotDot ->
        tw.Write("DotDot: ")
        printTokenFull tw input lexed dotDot
        tw.WriteLine()
    | RangeOpName.DotDotDotDot dotDotDotDot ->
        tw.Write("DotDotDotDot: ")
        printTokenFull tw input lexed dotDotDotDot
        tw.WriteLine()

and printActivePatternOpName
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (activePatternOpName: ActivePatternOpName<SyntaxToken>)
    =
    match activePatternOpName with
    | ActivePatternOpName.ActivePatternOp(lBar, idents, finalUnderscore, rBar) ->
        tw.Write("ActivePatternOp: ")
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printTokenMin tw input lexed lBar
        tw.WriteLine()

        for ident in idents do
            printTokenFull tw input lexed ident
            tw.WriteLine()

        match finalUnderscore with
        | ValueSome u ->
            printTokenFull tw input lexed u
            tw.WriteLine()
        | ValueNone -> ()

        printTokenMin tw input lexed rBar
        tw.Indent <- tw.Indent - 1
        tw.WriteLine()

let printLongIdentOrOp
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (longIdentOrOp: LongIdentOrOp<SyntaxToken>)
    =
    match longIdentOrOp with
    | LongIdentOrOp.LongIdent idents ->
        tw.Write("LongIdent: ")
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1

        for ident in idents do
            printTokenFull tw input lexed ident
            tw.WriteLine()

        tw.Indent <- tw.Indent - 1
    | LongIdentOrOp.Op identOrOp -> tw.Write("Op: ")
    | LongIdentOrOp.QualifiedOp(longIdent, dot, op) -> tw.Write("QualifiedOp: ")


let printPat (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (pat: Pat<SyntaxToken>) =
    match pat with
    | Pat.Const value ->
        tw.Write("Pat.Const: ")
        printConstant tw input lexed value
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

let printTypar (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (typar: Typar<SyntaxToken>) =
    match typar with
    | Typar.Anon underscore ->
        tw.Write("Typar.Anon: ")
        printTokenFull tw input lexed underscore
        tw.WriteLine()
    | Typar.Named(quote, ident) ->
        tw.Write("Typar.Named: ")
        printTokenFull tw input lexed quote
        tw.Write(" ")
        printTokenFull tw input lexed ident
        tw.WriteLine()
    | Typar.Static(caret, ident) ->
        tw.Write("Typar.Static: ")
        printTokenFull tw input lexed caret
        tw.Write(" ")
        printTokenFull tw input lexed ident
        tw.WriteLine()


let printTypeArg (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (typeArg: TypeArg<SyntaxToken>) =
    match typeArg with
    | TypeArg.Type ty ->
        tw.Write("TypeArg.Type: ")
        printType tw input lexed ty
    | TypeArg.Measure measure ->
        tw.Write("TypeArg.Measure: ")
        printTokenFull tw input lexed measure
        tw.WriteLine()
    | TypeArg.StaticParameter staticParam ->
        tw.Write("TypeArg.StaticParameter: ")
        printTokenFull tw input lexed staticParam
        tw.WriteLine()


let printType (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (ty: Type<SyntaxToken>) =
    match ty with
    | Type.ParenType(lParen, typ, rParen) ->
        tw.Write("ParenType: ")
        printTokenMin tw input lexed lParen
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printType tw input lexed typ
        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed rParen
        tw.WriteLine()
    | Type.VarType typar ->
        tw.Write("VarType: ")
        printTypar tw input lexed typar
    | Type.NamedType longIdent ->
        tw.Write("NamedType: ")
        printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent longIdent)
    | Type.GenericType(longIdent, lAngle, typeArgs, rAngle) ->
        tw.Write("GenericType: ")
        printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent longIdent)
        printTokenMin tw input lexed lAngle
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1

        for typeArg in typeArgs do
            printTypeArg tw input lexed typeArg

        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed rAngle
        tw.WriteLine()
    | _ -> failwith "Not implemented"

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
        printTokenMin tw input lexed inToken
        tw.WriteLine()
        tw.WriteLine("Body:")
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed body
        tw.Indent <- tw.Indent - 1
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
    | Expr.ParenBlock(l, expr, r) ->
        tw.Write("ParenBlock: ")
        printTokenMin tw input lexed l
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed expr
        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed r
        tw.WriteLine()
    | Expr.BeginEndBlock(l, expr, r) ->
        tw.Write("BeginEndBlock: ")
        printTokenMin tw input lexed l
        tw.WriteLine()
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed expr
        tw.Indent <- tw.Indent - 1
        printTokenMin tw input lexed r
        tw.WriteLine()
    | Expr.LongIdentOrOp longIdentOrOp ->
        tw.Write("LongIdentOrOp: ")
        printLongIdentOrOp tw input lexed longIdentOrOp
    | Expr.TypeApp(expr, lAngle, types, rAngle) ->
        tw.WriteLine("TypeApp:")
        tw.Indent <- tw.Indent + 1
        tw.WriteLine("Expr:")
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed expr
        tw.Indent <- tw.Indent - 1
        tw.WriteLine("Types:")
        tw.Indent <- tw.Indent + 1

        for ty in types do
            printType tw input lexed ty

        tw.Indent <- tw.Indent - 1
        tw.Indent <- tw.Indent - 1
    | Expr.DotLookup(expr, dot, longIdentOrOp) ->
        tw.WriteLine("DotLookup:")
        tw.Indent <- tw.Indent + 1
        tw.WriteLine("Expr:")
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed expr
        tw.Indent <- tw.Indent - 1
        tw.Write("Dot: ")
        printTokenMin tw input lexed dot
        tw.WriteLine()
        tw.WriteLine("LongIdentOrOp:")
        tw.Indent <- tw.Indent + 1
        printLongIdentOrOp tw input lexed longIdentOrOp
        tw.Indent <- tw.Indent - 1
        tw.Indent <- tw.Indent - 1
    | Expr.IfThenElse(ifToken, condition, thenToken, thenExpr, elifBranches, elseBranch) ->
        tw.WriteLine("IfThenElse:")
        tw.Indent <- tw.Indent + 1
        tw.Write("IfToken: ")
        printTokenMin tw input lexed ifToken
        tw.WriteLine()
        tw.WriteLine("Condition:")
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed condition
        tw.Indent <- tw.Indent - 1
        tw.Write("ThenToken: ")
        printTokenMin tw input lexed thenToken
        tw.WriteLine()
        tw.WriteLine("ThenExpr:")
        tw.Indent <- tw.Indent + 1
        printExpr tw input lexed thenExpr
        tw.Indent <- tw.Indent - 1

        for elif' in elifBranches do
            tw.WriteLine("ElifBranch:")
            tw.Indent <- tw.Indent + 1
            let (ElifBranch.ElifBranch(elifToken, elifCondition, thenToken, elifExpr)) = elif'
            tw.Write("ElifToken: ")
            printTokenMin tw input lexed elifToken
            tw.WriteLine()
            tw.WriteLine("ElifCondition:")
            tw.Indent <- tw.Indent + 1
            printExpr tw input lexed elifCondition
            tw.Indent <- tw.Indent - 1
            tw.Write("ThenToken: ")
            printTokenMin tw input lexed thenToken
            tw.WriteLine()
            tw.WriteLine("ElifExpr:")
            tw.Indent <- tw.Indent + 1
            printExpr tw input lexed elifExpr
            tw.Indent <- tw.Indent - 1
            tw.Indent <- tw.Indent - 1

        match elseBranch with
        | ValueSome(ElseBranch.ElseBranch(elseToken, elseExpr)) ->
            tw.Write("ElseToken: ")
            printTokenMin tw input lexed elseToken
            tw.WriteLine()
            tw.WriteLine("ElseExpr:")
            tw.Indent <- tw.Indent + 1
            printExpr tw input lexed elseExpr
            tw.Indent <- tw.Indent - 1
        | ValueNone -> ()

        tw.Indent <- tw.Indent - 1
    | _ -> failwithf "Not implemented %A" expr
