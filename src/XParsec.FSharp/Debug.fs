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

                match guard with
                | ValueSome(PatternGuard(whenToken, guardExpr)) ->
                    printLabelledToken "When" tw input lexed whenToken
                    printSection tw "Guard" (fun () -> printExpr tw input lexed guardExpr)
                | ValueNone -> ()

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
    | Constant.MeasuredLiteral(value, lAngle, measure, rAngle) ->
        printTokenFull tw input lexed value
        tw.Write(" <")
        printMeasure tw input lexed measure
        tw.Write(">")

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


let printPatParam (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (param: PatParam<SyntaxToken>) =
    match param with
    | PatParam.Const value ->
        tw.Write("PatParam.Const: ")
        printTokenFull tw input lexed value
        tw.WriteLine()
    | PatParam.LongIdent ident ->
        tw.Write("PatParam.LongIdent: ")
        printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent ident)
    | PatParam.App(ident, innerParam) ->
        printSection
            tw
            "PatParam.App"
            (fun () ->
                printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent ident)
                printPatParam tw input lexed innerParam
            )
    | PatParam.List(lBracket, parameters, rBracket) ->
        printLabelledToken "PatParam.List" tw input lexed lBracket

        indent
            tw
            (fun () ->
                for p in parameters do
                    printPatParam tw input lexed p
            )

        printTokenMin tw input lexed rBracket
        tw.WriteLine()
    | PatParam.Tuple(lParen, parameters, rParen) ->
        printLabelledToken "PatParam.Tuple" tw input lexed lParen

        indent
            tw
            (fun () ->
                for p in parameters do
                    printPatParam tw input lexed p
            )

        printTokenMin tw input lexed rParen
        tw.WriteLine()
    | PatParam.Typed(innerParam, colon, typ) ->
        printSection
            tw
            "PatParam.Typed"
            (fun () ->
                printPatParam tw input lexed innerParam
                printLabelledToken "Colon" tw input lexed colon
                printType tw input lexed typ
            )
    | PatParam.Null nullToken ->
        tw.Write("PatParam.Null: ")
        printTokenMin tw input lexed nullToken
        tw.WriteLine()
    | PatParam.Quoted _ -> failwith "printPatParam: Quoted not implemented"
    | PatParam.DoubleQuoted _ -> failwith "printPatParam: DoubleQuoted not implemented"

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
    | Pat.Named(longIdent, param, innerPat) ->
        printSection
            tw
            "Pat.Named"
            (fun () ->
                for ident in longIdent do
                    tw.Write("Ident: ")
                    printTokenFull tw input lexed ident
                    tw.WriteLine()

                match param with
                | ValueSome p ->
                    tw.Write("Param: ")
                    printPatParam tw input lexed p
                | ValueNone -> ()

                match innerPat with
                | ValueSome p ->
                    tw.Write("Pat: ")
                    printPat tw input lexed p
                | ValueNone -> ()
            )
    | Pat.As(innerPat, asToken, ident) ->
        printSection
            tw
            "Pat.As"
            (fun () ->
                printPat tw input lexed innerPat
                printLabelledToken "As" tw input lexed asToken
                tw.Write("Ident: ")
                printTokenFull tw input lexed ident
                tw.WriteLine()
            )
    | Pat.Or(left, bar, right) ->
        printLabelledToken "Pat.Or" tw input lexed bar

        indent
            tw
            (fun () ->
                printPat tw input lexed left
                printPat tw input lexed right
            )
    | Pat.And(left, ampersand, right) ->
        printLabelledToken "Pat.And" tw input lexed ampersand

        indent
            tw
            (fun () ->
                printPat tw input lexed left
                printPat tw input lexed right
            )
    | Pat.Cons(head, consToken, tail) ->
        printLabelledToken "Pat.Cons" tw input lexed consToken

        indent
            tw
            (fun () ->
                printPat tw input lexed head
                printPat tw input lexed tail
            )
    | Pat.Typed(innerPat, colon, typ) ->
        printSection
            tw
            "Pat.Typed"
            (fun () ->
                printPat tw input lexed innerPat
                printLabelledToken "Colon" tw input lexed colon
                printType tw input lexed typ
            )
    | Pat.Tuple(patterns, _commas) ->
        printSection
            tw
            "Pat.Tuple"
            (fun () ->
                for p in patterns do
                    printPat tw input lexed p
            )
    | Pat.StructTuple(structToken, _lParen, patterns, _commas, _rParen) ->
        printSection
            tw
            "Pat.StructTuple"
            (fun () ->
                printTokenMin tw input lexed structToken
                tw.WriteLine()

                for p in patterns do
                    printPat tw input lexed p
            )
    | Pat.Paren(lParen, innerPat, rParen) ->
        printLabelledToken "Pat.Paren" tw input lexed lParen
        indent tw (fun () -> printPat tw input lexed innerPat)
        printTokenMin tw input lexed rParen
        tw.WriteLine()
    | Pat.List(ListPat(lBracket, patterns, rBracket)) ->
        printLabelledToken "Pat.List" tw input lexed lBracket

        indent
            tw
            (fun () ->
                for p in patterns do
                    printPat tw input lexed p
            )

        printTokenMin tw input lexed rBracket
        tw.WriteLine()
    | Pat.Array(ArrayPat(lBarBracket, patterns, rBarBracket)) ->
        printLabelledToken "Pat.Array" tw input lexed lBarBracket

        indent
            tw
            (fun () ->
                for p in patterns do
                    printPat tw input lexed p
            )

        printTokenMin tw input lexed rBarBracket
        tw.WriteLine()
    | Pat.Record(RecordPat(lBrace, fieldPats, rBrace)) ->
        printLabelledToken "Pat.Record" tw input lexed lBrace

        indent
            tw
            (fun () ->
                for FieldPat(longIdent, equals, innerPat) in fieldPats do
                    printSection
                        tw
                        "FieldPat"
                        (fun () ->
                            for ident in longIdent do
                                tw.Write("Ident: ")
                                printTokenFull tw input lexed ident
                                tw.WriteLine()

                            printLabelledToken "=" tw input lexed equals
                            tw.Write("Pat: ")
                            printPat tw input lexed innerPat
                        )
            )

        printTokenMin tw input lexed rBrace
        tw.WriteLine()
    | Pat.TypeTest(colonQuestion, typ) ->
        printSection
            tw
            "Pat.TypeTest"
            (fun () ->
                printLabelledToken "ColonQuestion" tw input lexed colonQuestion
                printType tw input lexed typ
            )
    | Pat.TypeTestAs(colonQuestion, typ, asToken, ident) ->
        printSection
            tw
            "Pat.TypeTestAs"
            (fun () ->
                printLabelledToken "ColonQuestion" tw input lexed colonQuestion
                printType tw input lexed typ
                printLabelledToken "As" tw input lexed asToken
                tw.Write("Ident: ")
                printTokenFull tw input lexed ident
                tw.WriteLine()
            )
    | Pat.Null nullToken ->
        tw.Write("Pat.Null: ")
        printTokenMin tw input lexed nullToken
        tw.WriteLine()
    | Pat.Attributed(attributes, innerPat) ->
        printSection
            tw
            "Pat.Attributed"
            (fun () ->
                tw.WriteLine($"(Attributes: {attributes.Length} set(s))")
                printPat tw input lexed innerPat
            )
    | Pat.Struct(structToken, innerPat) ->
        printSection
            tw
            "Pat.Struct"
            (fun () ->
                printTokenMin tw input lexed structToken
                tw.WriteLine()
                printPat tw input lexed innerPat
            )

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

let printConstraint (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (c: Constraint<SyntaxToken>) =
    match c with
    | Constraint.Coercion(typar, colonGT, typ) ->
        printSection
            tw
            "Constraint.Coercion"
            (fun () ->
                printTypar tw input lexed typar
                printLabelledToken ":>" tw input lexed colonGT
                printType tw input lexed typ
            )
    | Constraint.Nullness(typar, _colon, _nullToken) ->
        printSection tw "Constraint.Nullness" (fun () -> printTypar tw input lexed typar)
    | Constraint.MemberTrait(_staticTypars, _colon, _lParen, _membersign, _rParen) ->
        tw.WriteLine("Constraint.MemberTrait: (not fully printed)")
    | Constraint.DefaultConstructor(typar, _colon, _lParen, _newToken, _colonUnit, _arrow, _quoteT, _rParen) ->
        printSection tw "Constraint.DefaultConstructor" (fun () -> printTypar tw input lexed typar)
    | Constraint.Struct(typar, _colon, _structToken) ->
        printSection tw "Constraint.Struct" (fun () -> printTypar tw input lexed typar)
    | Constraint.ReferenceType(typar, _colon, _notToken, _structToken) ->
        printSection tw "Constraint.ReferenceType" (fun () -> printTypar tw input lexed typar)
    | Constraint.Enum(typar, _colon, _enumToken, _lAngle, typ, _rAngle) ->
        printSection
            tw
            "Constraint.Enum"
            (fun () ->
                printTypar tw input lexed typar
                printType tw input lexed typ
            )
    | Constraint.Unmanaged(typar, _colon, _unmanagedToken) ->
        printSection tw "Constraint.Unmanaged" (fun () -> printTypar tw input lexed typar)
    | Constraint.Delegate(typar, _colon, _delegateToken, _lAngle, type1, _comma, type2, _rAngle) ->
        printSection
            tw
            "Constraint.Delegate"
            (fun () ->
                printTypar tw input lexed typar
                printType tw input lexed type1
                printType tw input lexed type2
            )
    | Constraint.Equality(typar, _colon, _equalityToken) ->
        printSection tw "Constraint.Equality" (fun () -> printTypar tw input lexed typar)
    | Constraint.Comparison(typar, _colon, _comparisonToken) ->
        printSection tw "Constraint.Comparison" (fun () -> printTypar tw input lexed typar)

let printTyparDefns (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (typars: TyparDefns<SyntaxToken>) =
    let (TyparDefns(lAngle, defns, constraints, rAngle)) = typars

    printSection
        tw
        "TyparDefns"
        (fun () ->
            printTokenMin tw input lexed lAngle
            tw.WriteLine()

            for (TyparDefn(_attrs, typar)) in defns do
                printTypar tw input lexed typar

            match constraints with
            | ValueSome(TyparConstraints(whenToken, constraintList)) ->
                printLabelledToken "when" tw input lexed whenToken

                for c in constraintList do
                    printConstraint tw input lexed c
            | ValueNone -> ()

            printTokenMin tw input lexed rAngle
            tw.WriteLine()
        )

let printTypeArg (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (typeArg: TypeArg<SyntaxToken>) =
    match typeArg with
    | TypeArg.Type ty ->
        tw.Write("TypeArg.Type: ")
        printType tw input lexed ty
    | TypeArg.Measure measure ->
        tw.Write("TypeArg.Measure: ")
        printMeasure tw input lexed measure
        tw.WriteLine()
    | TypeArg.StaticParameter staticParam ->
        tw.Write("TypeArg.StaticParameter: ")
        printTokenFull tw input lexed staticParam
        tw.WriteLine()

let rec printMeasure (tw: IndentedTextWriter) (input: string) (lexed: Lexed) (measure: Measure<SyntaxToken>) =
    match measure with
    | Measure.Named longIdent ->
        tw.Write("Measure.Named: ")
        printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent longIdent)
        tw.WriteLine()
    | Measure.One oneToken ->
        tw.Write("Measure.One: ")
        printTokenFull tw input lexed oneToken
        tw.WriteLine()
    | Measure.Anonymous underscore ->
        tw.Write("Measure.Anonymous: ")
        printTokenFull tw input lexed underscore
        tw.WriteLine()
    | Measure.Typar typar ->
        tw.Write("Measure.Typar: ")
        printTypar tw input lexed typar
        tw.WriteLine()
    | Measure.Juxtaposition(measures, ops) ->
        printSection
            tw
            "Measure.Juxtaposition"
            (fun () ->
                for m in measures do
                    printMeasure tw input lexed m
            )
    | Measure.Power(baseMeasure, powerToken, exponent) ->
        printSection
            tw
            "Measure.Power"
            (fun () ->
                printMeasure tw input lexed baseMeasure
                printTokenMin tw input lexed powerToken
                printTokenMin tw input lexed exponent
            )
    | Measure.Product(left, mulToken, right) ->
        printSection
            tw
            "Measure.Product"
            (fun () ->
                printMeasure tw input lexed left
                printTokenMin tw input lexed mulToken
                printMeasure tw input lexed right
            )
    | Measure.Quotient(numerator, divToken, denominator) ->
        printSection
            tw
            "Measure.Quotient"
            (fun () ->
                printMeasure tw input lexed numerator
                printTokenMin tw input lexed divToken
                printMeasure tw input lexed denominator
            )
    | Measure.Reciprocal(divToken, measure) ->
        printSection
            tw
            "Measure.Reciprocal"
            (fun () ->
                printTokenMin tw input lexed divToken
                printMeasure tw input lexed measure
            )
    | Measure.Paren(lParen, measure, rParen) ->
        printSection
            tw
            "Measure.Paren"
            (fun () ->
                printTokenMin tw input lexed lParen
                printMeasure tw input lexed measure
                printTokenMin tw input lexed rParen
            )

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
    | Type.FunctionType(fromType, arrow, toType) ->
        printSection
            tw
            "FunctionType"
            (fun () ->
                printType tw input lexed fromType
                printLabelledToken "->" tw input lexed arrow
                printType tw input lexed toType
            )
    | Type.TupleType(types) ->
        printSection
            tw
            "TupleType"
            (fun () ->
                for t in types do
                    printType tw input lexed t
            )
    | Type.StructTupleType(structToken, _lParen, types, _rParen) ->
        printSection
            tw
            "StructTupleType"
            (fun () ->
                printTokenMin tw input lexed structToken
                tw.WriteLine()

                for t in types do
                    printType tw input lexed t
            )
    | Type.IncompleteGenericType(longIdent, lAngle, rAngle) ->
        tw.Write("IncompleteGenericType: ")
        printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent longIdent)
        printTokenMin tw input lexed lAngle
        tw.Write(" ")
        printTokenMin tw input lexed rAngle
        tw.WriteLine()
    | Type.SuffixedType(baseType, longIdent) ->
        printSection
            tw
            "SuffixedType"
            (fun () ->
                printType tw input lexed baseType
                printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent longIdent)
            )
    | Type.ArrayType(baseType, lBracket, commas, rBracket) ->
        printSection
            tw
            "ArrayType"
            (fun () ->
                printType tw input lexed baseType
                printTokenMin tw input lexed lBracket
                tw.WriteLine()

                for comma in commas do
                    printTokenMin tw input lexed comma
                    tw.WriteLine()

                printTokenMin tw input lexed rBracket
                tw.WriteLine()
            )
    | Type.ConstrainedType(typ, constraints) ->
        printSection
            tw
            "ConstrainedType"
            (fun () ->
                printType tw input lexed typ
                printTyparDefns tw input lexed constraints
            )
    | Type.SubtypeConstraint(typar, colonGreaterThan, typ) ->
        printSection
            tw
            "SubtypeConstraint"
            (fun () ->
                printTypar tw input lexed typar
                printLabelledToken ":>" tw input lexed colonGreaterThan
                printType tw input lexed typ
            )
    | Type.AnonymousSubtype(hash, typ) ->
        printSection
            tw
            "AnonymousSubtype"
            (fun () ->
                printTokenMin tw input lexed hash
                tw.WriteLine()
                printType tw input lexed typ
            )

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
        | ValueSome typars -> printTyparDefns tw input lexed typars
        | ValueNone -> ()

        match returnType with
        | ValueSome(ReturnType(colon, typ)) ->
            printLabelledToken "ReturnColon" tw input lexed colon
            printType tw input lexed typ
        | ValueNone -> ()

        printLabelledToken "=" tw input lexed equals
        indent tw (fun () -> printExpr tw input lexed expr)

let printFunctionDefn
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (functionDefn: FunctionDefn<SyntaxToken>)
    =
    let (FunctionDefn(inlineToken, access, identOrOp, typarDefns, argumentPats, returnType, equals, expr)) =
        functionDefn

    match inlineToken with
    | ValueSome t -> printLabelledToken "inline" tw input lexed t
    | ValueNone -> ()

    match access with
    | ValueSome a -> printLabelledToken "access" tw input lexed a
    | ValueNone -> ()

    tw.Write("IdentOrOp: ")
    printIdentOrOp tw input lexed identOrOp

    match typarDefns with
    | ValueSome typars -> printTyparDefns tw input lexed typars
    | ValueNone -> ()

    printSection
        tw
        "Pats"
        (fun () ->
            for pat in argumentPats do
                printPat tw input lexed pat
        )

    match returnType with
    | ValueSome(ReturnType(colon, typ)) ->
        printLabelledToken "ReturnColon" tw input lexed colon
        printType tw input lexed typ
    | ValueNone -> ()

    printLabelledToken "=" tw input lexed equals
    indent tw (fun () -> printExpr tw input lexed expr)

let printFunctionOrValueDefn
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (defn: FunctionOrValueDefn<SyntaxToken>)
    =
    match defn with
    | FunctionOrValueDefn.Function functionDefn ->
        printSection tw "Function" (fun () -> printFunctionDefn tw input lexed functionDefn)
    | FunctionOrValueDefn.Value valueDefn -> printSection tw "Value" (fun () -> printValueDefn tw input lexed valueDefn)

let printFieldInitializer
    (tw: IndentedTextWriter)
    (input: string)
    (lexed: Lexed)
    (fieldInit: FieldInitializer<SyntaxToken>)
    =
    let (FieldInitializer(longIdent, equals, fieldExpr)) = fieldInit

    printSection
        tw
        "Field"
        (fun () ->
            printLongIdentOrOp tw input lexed (LongIdentOrOp.LongIdent longIdent)
            printLabelledToken "=" tw input lexed equals
            printSection tw "Expr" (fun () -> printExpr tw input lexed fieldExpr)
        )

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
    | Expr.PrefixApp(op, expr) ->
        printLabelledToken "PrefixApp" tw input lexed op
        indent tw (fun () -> printExpr tw input lexed expr)
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
    | Expr.App(funcExpr, argExpr) ->
        printSection
            tw
            "App"
            (fun () ->
                printSection tw "Func" (fun () -> printExpr tw input lexed funcExpr)
                printSection tw "Arg" (fun () -> printExpr tw input lexed argExpr)
            )
    | Expr.HighPrecedenceApp(funcExpr, lParen, argExpr, rParen) ->
        printSection
            tw
            "HighPrecedenceApp"
            (fun () ->
                printSection tw "Func" (fun () -> printExpr tw input lexed funcExpr)
                printLabelledToken "(" tw input lexed lParen
                printSection tw "Arg" (fun () -> printExpr tw input lexed argExpr)
                printTokenMin tw input lexed rParen
                tw.WriteLine()
            )
    | Expr.TypeAnnotation(innerExpr, colon, typ) ->
        printSection
            tw
            "TypeAnnotation"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed innerExpr)
                printLabelledToken "Colon" tw input lexed colon
                printType tw input lexed typ
            )
    | Expr.Lazy(lazyToken, innerExpr) ->
        printLabelledToken "Lazy" tw input lexed lazyToken
        indent tw (fun () -> printExpr tw input lexed innerExpr)
    | Expr.Assert(assertToken, innerExpr) ->
        printLabelledToken "Assert" tw input lexed assertToken
        indent tw (fun () -> printExpr tw input lexed innerExpr)
    | Expr.Null nullToken ->
        tw.Write("Null: ")
        printTokenMin tw input lexed nullToken
        tw.WriteLine()
    | Expr.Function(functionToken, rules) ->
        printLabelledToken "Function" tw input lexed functionToken
        indent tw (fun () -> printRules tw input lexed rules)
    | Expr.LetFunction(letToken, functionDefn, inToken, body) ->
        printLabelledToken "LetFunction" tw input lexed letToken

        indent
            tw
            (fun () ->
                printFunctionDefn tw input lexed functionDefn
                printLabelledToken "in" tw input lexed inToken
                printSection tw "Body" (fun () -> printExpr tw input lexed body)
            )
    | Expr.LetRec(letToken, recToken, defns, inToken, body) ->
        printLabelledToken "LetRec" tw input lexed letToken

        indent
            tw
            (fun () ->
                printLabelledToken "rec" tw input lexed recToken

                for defn in defns do
                    printFunctionOrValueDefn tw input lexed defn

                printLabelledToken "in" tw input lexed inToken
                printSection tw "Body" (fun () -> printExpr tw input lexed body)
            )
    | Expr.New(newToken, typ, newExpr) ->
        printLabelledToken "New" tw input lexed newToken

        indent
            tw
            (fun () ->
                printType tw input lexed typ
                printSection tw "Arg" (fun () -> printExpr tw input lexed newExpr)
            )
    | Expr.Assignment(leftExpr, arrow, rightExpr) ->
        printLabelledToken "Assignment" tw input lexed arrow

        indent
            tw
            (fun () ->
                printExpr tw input lexed leftExpr
                printExpr tw input lexed rightExpr
            )
    | Expr.Record(lBrace, fieldInitializers, rBrace) ->
        printLabelledToken "Record" tw input lexed lBrace

        indent
            tw
            (fun () ->
                for fi in fieldInitializers do
                    printFieldInitializer tw input lexed fi
            )

        printTokenMin tw input lexed rBrace
        tw.WriteLine()
    | Expr.RecordClone(lBrace, baseExpr, withToken, fieldInitializers, rBrace) ->
        printLabelledToken "RecordClone" tw input lexed lBrace

        indent
            tw
            (fun () ->
                printSection tw "Base" (fun () -> printExpr tw input lexed baseExpr)
                printLabelledToken "with" tw input lexed withToken

                for fi in fieldInitializers do
                    printFieldInitializer tw input lexed fi
            )

        printTokenMin tw input lexed rBrace
        tw.WriteLine()
    | Expr.IndexedLookup(indexedExpr, dot, lBracket, indexArgExpr, rBracket) ->
        printSection
            tw
            "IndexedLookup"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed indexedExpr)
                printLabelledToken "." tw input lexed dot
                printLabelledToken "[" tw input lexed lBracket
                printSection tw "Index" (fun () -> printExpr tw input lexed indexArgExpr)
                printTokenMin tw input lexed rBracket
                tw.WriteLine()
            )
    | Expr.StaticUpcast(castExpr, colonGT, typ) ->
        printSection
            tw
            "StaticUpcast"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed castExpr)
                printLabelledToken ":>" tw input lexed colonGT
                printType tw input lexed typ
            )
    | Expr.DynamicTypeTest(testExpr, colonQ, typ) ->
        printSection
            tw
            "DynamicTypeTest"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed testExpr)
                printLabelledToken ":?" tw input lexed colonQ
                printType tw input lexed typ
            )
    | Expr.DynamicDowncast(castExpr, colonQGT, typ) ->
        printSection
            tw
            "DynamicDowncast"
            (fun () ->
                printSection tw "Expr" (fun () -> printExpr tw input lexed castExpr)
                printLabelledToken ":?>" tw input lexed colonQGT
                printType tw input lexed typ
            )
    | Expr.Upcast(upcastToken, castExpr) ->
        printLabelledToken "Upcast" tw input lexed upcastToken
        indent tw (fun () -> printExpr tw input lexed castExpr)
    | Expr.Downcast(downcastToken, castExpr) ->
        printLabelledToken "Downcast" tw input lexed downcastToken
        indent tw (fun () -> printExpr tw input lexed castExpr)
    | Expr.UseFixed(useToken, ident, equals, fixedToken, fixedExpr) ->
        printLabelledToken "UseFixed" tw input lexed useToken

        indent
            tw
            (fun () ->
                tw.Write("Ident: ")
                printTokenFull tw input lexed ident
                tw.WriteLine()
                printLabelledToken "fixed" tw input lexed fixedToken
                printSection tw "Expr" (fun () -> printExpr tw input lexed fixedExpr)
            )
    | Expr.Missing -> tw.WriteLine("Missing")
    | Expr.SkipsTokens(skippedTokens, innerExpr) ->
        printSection
            tw
            "SkipsTokens"
            (fun () ->
                for t in skippedTokens do
                    printTokenMin tw input lexed t
                    tw.WriteLine()

                printSection tw "Expr" (fun () -> printExpr tw input lexed innerExpr)
            )
    | Expr.Pat innerPat ->
        tw.Write("Pat: ")
        printPat tw input lexed innerPat
    | _ -> failwithf "Not implemented %A" expr
