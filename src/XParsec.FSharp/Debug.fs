module rec XParsec.FSharp.Debug

open System
open System.CodeDom.Compiler

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Temporarily increases the indentation level, runs `f`, then restores it.
let inline indent (tw: IndentedTextWriter) (f: unit -> unit) =
    tw.Indent <- tw.Indent + 1
    f ()
    tw.Indent <- tw.Indent - 1

/// Writes "label: <token>" on a single line using the minimal token format.
let printLabelledToken (label: string) (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    tw.Write($"{label}: ")
    printTokenMin tw input lexed token
    tw.WriteLine()

/// Writes "header:" then runs `f` indented by one level.
let printSection (tw: IndentedTextWriter) (header: string) (f: unit -> unit) =
    tw.WriteLine($"{header}:")
    indent tw f

/// Prints the list of match/try-with rules (shared logic).
let printRules (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (rules: Rules<SyntaxToken>) =
    let (Rules(firstBar, ruleList, bars)) = rules

    for i in 0 .. ruleList.Length - 1 do
        let bar = if i = 0 then firstBar else ValueSome bars[i - 1]

        printSection
            tw
            "Rule"
            (fun () ->
                match bar with
                | ValueSome b -> printLabelledToken "Bar" tw input lexed b
                | ValueNone -> ()

                let (Rule(pat, guard, arrow, ruleExpr)) = ruleList[i]
                tw.Write("Pat: ")
                printPat tw input lexed pat
                printLabelledToken "Arrow" tw input lexed arrow
                printSection tw "Expr" (fun () -> printExpr tw input lexed ruleExpr)
            )

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
                input.[int i .. int (i + 9)] + "..."
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
        printSection
            tw
            "ParenOp"
            (fun () ->
                printTokenMin tw input lexed lParen
                tw.WriteLine()
                printOpName tw input lexed opName
                printTokenMin tw input lexed rParen
                tw.WriteLine()
            )

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
        printSection
            tw
            "ActivePatternOp"
            (fun () ->
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
                tw.WriteLine()
            )

let printLongIdentOrOp
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (longIdentOrOp: LongIdentOrOp<SyntaxToken>)
    =
    match longIdentOrOp with
    | LongIdentOrOp.LongIdent idents ->
        printSection
            tw
            "LongIdent"
            (fun () ->
                for ident in idents do
                    printTokenFull tw input lexed ident
                    tw.WriteLine()
            )
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
    | Pat.Wildcard underscore ->
        tw.Write("Pat.Wildcard: ")
        printTokenMin tw input lexed underscore
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

        printLabelledToken "=" tw input lexed equals
        indent tw (fun () -> printExpr tw input lexed expr)

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
        printLabelledToken "ParenType" tw input lexed lParen
        indent tw (fun () -> printType tw input lexed typ)
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

        indent
            tw
            (fun () ->
                for typeArg in typeArgs do
                    printTypeArg tw input lexed typeArg
            )

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
        printLabelledToken "LetValue" tw input lexed letToken

        indent
            tw
            (fun () ->
                printValueDefn tw input lexed valueDefn
                printLabelledToken "in" tw input lexed inToken
                printSection tw "Body" (fun () -> printExpr tw input lexed body)
            )
    | Expr.InfixApp(left, op, right) ->
        printLabelledToken "InfixApp" tw input lexed op

        indent
            tw
            (fun () ->
                printExpr tw input lexed left
                printExpr tw input lexed right
            )
    | Expr.Sequential(left, sep, right) ->
        printLabelledToken "Sequential" tw input lexed sep

        indent
            tw
            (fun () ->
                printExpr tw input lexed left
                printExpr tw input lexed right
            )
    | Expr.Tuple elements ->
        printSection
            tw
            "Tuple"
            (fun () ->
                for elem in elements do
                    printExpr tw input lexed elem
            )
    | Expr.StructTuple(kw, lParen, elements, rParen) ->
        printSection
            tw
            "StructTuple"
            (fun () ->
                for elem in elements do
                    printExpr tw input lexed elem
            )
    | Expr.List(lBracket, elements, rBracket) ->
        printLabelledToken "List" tw input lexed lBracket

        indent
            tw
            (fun () ->
                for elem in elements do
                    printExpr tw input lexed elem
            )

        printTokenMin tw input lexed rBracket
        tw.WriteLine()
    | Expr.Array(lBracket, elements, rBracket) ->
        printLabelledToken "Array" tw input lexed lBracket

        indent
            tw
            (fun () ->
                for elem in elements do
                    printExpr tw input lexed elem
            )

        printTokenMin tw input lexed rBracket
        tw.WriteLine()
    | Expr.ParenBlock(l, expr, r) ->
        printLabelledToken "ParenBlock" tw input lexed l
        indent tw (fun () -> printExpr tw input lexed expr)
        printTokenMin tw input lexed r
        tw.WriteLine()
    | Expr.BeginEndBlock(l, expr, r) ->
        printLabelledToken "BeginEndBlock" tw input lexed l
        indent tw (fun () -> printExpr tw input lexed expr)
        printTokenMin tw input lexed r
        tw.WriteLine()
    | Expr.LongIdentOrOp longIdentOrOp ->
        tw.Write("LongIdentOrOp: ")
        printLongIdentOrOp tw input lexed longIdentOrOp
    | Expr.TypeApp(expr, lAngle, types, rAngle) ->
        printSection
            tw
            "TypeApp"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed expr)

                printSection
                    tw
                    "Types"
                    (fun () ->
                        for ty in types do
                            printType tw input lexed ty
                    )
            )
    | Expr.DotLookup(expr, dot, longIdentOrOp) ->
        printSection
            tw
            "DotLookup"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed expr)
                printLabelledToken "Dot" tw input lexed dot
                printSection tw "LongIdentOrOp" (fun () -> printLongIdentOrOp tw input lexed longIdentOrOp)
            )
    | Expr.IfThenElse(ifToken, condition, thenToken, thenExpr, elifBranches, elseBranch) ->
        printSection
            tw
            "IfThenElse"
            (fun () ->
                printLabelledToken "IfToken" tw input lexed ifToken
                printSection tw "Condition" (fun () -> printExpr tw input lexed condition)
                printLabelledToken "ThenToken" tw input lexed thenToken
                printSection tw "ThenExpr" (fun () -> printExpr tw input lexed thenExpr)

                for elif' in elifBranches do
                    printSection
                        tw
                        "ElifBranch"
                        (fun () ->
                            let (ElifBranch.ElifBranch(elifToken, elifCondition, thenToken, elifExpr)) = elif'
                            printLabelledToken "ElifToken" tw input lexed elifToken
                            printSection tw "ElifCondition" (fun () -> printExpr tw input lexed elifCondition)
                            printLabelledToken "ThenToken" tw input lexed thenToken
                            printSection tw "ElifExpr" (fun () -> printExpr tw input lexed elifExpr)
                        )

                match elseBranch with
                | ValueSome(ElseBranch.ElseBranch(elseToken, elseExpr)) ->
                    printLabelledToken "ElseToken" tw input lexed elseToken
                    printSection tw "ElseExpr" (fun () -> printExpr tw input lexed elseExpr)
                | ValueNone -> ()
            )
    | Expr.Match(matchToken, matchExpr, withToken, rules) ->
        printSection
            tw
            "Match"
            (fun () ->
                printLabelledToken "MatchToken" tw input lexed matchToken
                printSection tw "MatchExpr" (fun () -> printExpr tw input lexed matchExpr)
                printLabelledToken "WithToken" tw input lexed withToken
                printRules tw input lexed rules
            )
    | Expr.Fun(funToken, pats, arrow, expr) ->
        printLabelledToken "Fun" tw input lexed funToken

        printSection
            tw
            "Pats"
            (fun () ->
                for pat in pats do
                    printPat tw input lexed pat
            )

        printLabelledToken "Arrow" tw input lexed arrow
        printSection tw "Body" (fun () -> printExpr tw input lexed expr)
    | Expr.TryWith(tryToken, tryExpr, withToken, rules) ->
        printLabelledToken "TryWith" tw input lexed tryToken
        printSection tw "TryExpr" (fun () -> printExpr tw input lexed tryExpr)
        printLabelledToken "WithToken" tw input lexed withToken
        printRules tw input lexed rules
    | Expr.TryFinally(tryToken, tryExpr, finallyToken, finallyExpr) ->
        printLabelledToken "TryFinally" tw input lexed tryToken
        printSection tw "TryExpr" (fun () -> printExpr tw input lexed tryExpr)
        printLabelledToken "FinallyToken" tw input lexed finallyToken
        printSection tw "FinallyExpr" (fun () -> printExpr tw input lexed finallyExpr)
    | Expr.While(whileToken, cond, doToken, body, doneToken) ->
        printLabelledToken "While" tw input lexed whileToken
        printSection tw "Cond" (fun () -> printExpr tw input lexed cond)
        printLabelledToken "DoToken" tw input lexed doToken
        printSection tw "Body" (fun () -> printExpr tw input lexed body)
        printLabelledToken "DoneToken" tw input lexed doneToken
    | Expr.ForTo(forToken, ident, equals, startExpr, toToken, endExpr, doToken, body, doneToken) ->
        printLabelledToken "ForTo" tw input lexed forToken
        tw.Write("Ident: ")
        printTokenFull tw input lexed ident
        tw.WriteLine()
        printLabelledToken "Equals" tw input lexed equals
        printSection tw "Start" (fun () -> printExpr tw input lexed startExpr)
        printLabelledToken "ToToken" tw input lexed toToken
        printSection tw "End" (fun () -> printExpr tw input lexed endExpr)
        printLabelledToken "DoToken" tw input lexed doToken
        printSection tw "Body" (fun () -> printExpr tw input lexed body)
        printLabelledToken "DoneToken" tw input lexed doneToken
    | Expr.ForIn(forToken, pat, inToken, range, doToken, body, doneToken) ->
        printLabelledToken "ForIn" tw input lexed forToken
        tw.Write("Pat: ")
        printPat tw input lexed pat
        printLabelledToken "InToken" tw input lexed inToken

        printSection
            tw
            "Range"
            (fun () ->
                match range with
                | ExprOrRange.Expr e -> printExpr tw input lexed e
                | ExprOrRange.Range _ -> tw.WriteLine("(range)")
            )

        printLabelledToken "DoToken" tw input lexed doToken
        printSection tw "Body" (fun () -> printExpr tw input lexed body)
        printLabelledToken "DoneToken" tw input lexed doneToken
    | Expr.Use(useToken, ident, equals, expr, inToken, body) ->
        printLabelledToken "Use" tw input lexed useToken
        tw.Write("Ident: ")
        printTokenFull tw input lexed ident
        tw.WriteLine()
        printLabelledToken "Equals" tw input lexed equals
        printSection tw "Expr" (fun () -> printExpr tw input lexed expr)
        printLabelledToken "InToken" tw input lexed inToken
        printSection tw "Body" (fun () -> printExpr tw input lexed body)
    | _ -> failwithf "Not implemented %A" expr
