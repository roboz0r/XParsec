module rec XParsec.FSharp.Debug

open System
open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// ---- Buffered output types ----

[<Struct>]
type private TokenRowData =
    {
        IndentLevel: int
        Label: string
        QuotedLit: string
        Kind: string
        TokIdx: string
        CharPos: string
    }

[<RequireQualifiedAccess>]
type private OutputItem =
    | Line of indentLevel: int * text: string
    | TokenRow of TokenRowData

/// Collects print output into a buffer, then emits it with the label column
/// dynamically sized to fit the deepest indentation level, so that the
/// literal/kind/index columns align perfectly across the whole output.
type PrintContext(indentSize: int) =
    let items = ResizeArray<OutputItem>()
    let mutable currentIndent = 0

    member _.IndentSize = indentSize

    member _.Indent
        with get () = currentIndent
        and set v = currentIndent <- v

    member ctx.WriteLine(text: string) =
        items.Add(OutputItem.Line(ctx.Indent, text))

    member ctx.AddTokenRow(label: string, quotedLit: string, kind: string, tokIdx: string, charPos: string) =
        items.Add(
            OutputItem.TokenRow
                {
                    IndentLevel = ctx.Indent
                    Label = label
                    QuotedLit = quotedLit
                    Kind = kind
                    TokIdx = tokIdx
                    CharPos = charPos
                }
        )

    member _.Flush(writer: TextWriter) =
        // Dynamic label column width: widest (indent + label) across all token rows.
        let maxLabelColWidth =
            let mutable m = 0

            for item in items do
                match item with
                | OutputItem.TokenRow row ->
                    let w = row.IndentLevel * indentSize + row.Label.Length

                    if w > m then
                        m <- w
                | _ -> ()

            m

        for item in items do
            match item with
            | OutputItem.Line(ind, text) -> writer.WriteLine(String.replicate (ind * indentSize) " " + text)
            | OutputItem.TokenRow row ->
                let indentStr = String.replicate (row.IndentLevel * indentSize) " "
                let labelPadded = (indentStr + row.Label).PadRight(maxLabelColWidth)

                writer.WriteLine(
                    $"{labelPadded}  {row.QuotedLit, -16}  {row.Kind, -18}  {row.TokIdx, -8}  {row.CharPos}"
                )

    member ctx.FlushToString() =
        use sw = new StringWriter()
        ctx.Flush(sw)
        sw.ToString()

// ---- Helpers ----

/// Temporarily increases the indentation level, runs `f`, then restores it.
let inline indent (ctx: PrintContext) (f: unit -> unit) =
    ctx.Indent <- ctx.Indent + 1
    f ()
    ctx.Indent <- ctx.Indent - 1

/// Writes "header:" then runs `f` indented by one level.
let printSection (ctx: PrintContext) (header: string) (f: unit -> unit) =
    ctx.WriteLine($"{header}:")
    indent ctx f

/// Adds a single token row to the print buffer.
let printTokenRow (label: string) (ctx: PrintContext) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    match token.Index with
    | TokenIndex.Virtual ->
        let kind = $"{TokenInfo.withoutFlags token.PositionedToken.Token}"
        let charPos = token.PositionedToken.StartIndex
        ctx.AddTokenRow(label, "<virt>", kind, "tok=virt", $"pos=%4i{charPos}")
    | TokenIndex.Regular iT ->
        let t1 = lexed.Tokens.[iT + 1<_>]
        let charPos = token.PositionedToken.StartIndex
        let charEnd = t1.StartIndex - 1
        let len = charEnd - charPos + 1

        let rawLit =
            if len > 10 then
                input.[charPos .. charPos + 9] + "..."
            else
                input.[charPos..charEnd]

        let lit = rawLit.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")
        let kind = $"{TokenInfo.withoutFlags token.PositionedToken.Token}"
        ctx.AddTokenRow(label, $"'{lit}'", kind, $"tok=%4i{int iT}", $"pos=%4i{charPos}")

/// Alias for `printTokenRow` — kept so all 96 existing call sites compile unchanged.
let printLabelledToken (label: string) (ctx: PrintContext) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    printTokenRow label ctx input lexed token

// ---- Print functions ----

let printRules (ctx: PrintContext) (input: string) (lexed: Lexed) (rules: Rules<SyntaxToken>) =
    let (Rules(firstBar, ruleList, bars)) = rules

    for i in 0 .. ruleList.Length - 1 do
        let bar = if i = 0 then firstBar else ValueSome bars[i - 1]

        printSection
            ctx
            "Rule"
            (fun () ->
                match bar with
                | ValueSome b -> printLabelledToken "Bar" ctx input lexed b
                | ValueNone -> ()

                match ruleList[i] with
                | Rule.Rule(pat, guard, arrow, ruleExpr) ->
                    printSection ctx "Pat" (fun () -> printPat ctx input lexed pat)

                    match guard with
                    | ValueSome(PatternGuard(whenToken, guardExpr)) ->
                        printLabelledToken "When" ctx input lexed whenToken
                        printSection ctx "Guard" (fun () -> printExpr ctx input lexed guardExpr)
                    | ValueNone -> ()

                    printLabelledToken "Arrow" ctx input lexed arrow
                    printSection ctx "Expr" (fun () -> printExpr ctx input lexed ruleExpr)
                | Rule.Missing -> ctx.WriteLine("Missing")
                | Rule.SkipsTokens(skippedTokens, innerRule) ->
                    printSection
                        ctx
                        "SkipsTokens"
                        (fun () ->
                            for t in skippedTokens do
                                printTokenRow "(skipped)" ctx input lexed t

                            printSection
                                ctx
                                "Rule"
                                (fun () ->
                                    match innerRule with
                                    | Rule.Rule(pat, guard, arrow, ruleExpr) ->
                                        printSection ctx "Pat" (fun () -> printPat ctx input lexed pat)

                                        match guard with
                                        | ValueSome(PatternGuard(whenToken, guardExpr)) ->
                                            printLabelledToken "When" ctx input lexed whenToken
                                            printSection ctx "Guard" (fun () -> printExpr ctx input lexed guardExpr)
                                        | ValueNone -> ()

                                        printLabelledToken "Arrow" ctx input lexed arrow
                                        printSection ctx "Expr" (fun () -> printExpr ctx input lexed ruleExpr)
                                    | Rule.Missing -> ctx.WriteLine("Missing")
                                    | Rule.SkipsTokens _ -> ctx.WriteLine("SkipsTokens (nested)")
                                )
                        )
            )

let printConstant (label: string) (ctx: PrintContext) (input: string) (lexed: Lexed) (x: Constant<SyntaxToken>) =
    match x with
    | Constant.Literal value -> printTokenRow label ctx input lexed value
    | Constant.MeasuredLiteral(value, lAngle, measure, rAngle) ->
        printSection
            ctx
            label
            (fun () ->
                printTokenRow "Literal" ctx input lexed value
                printTokenRow "<" ctx input lexed lAngle
                printSection ctx "Measure" (fun () -> printMeasure ctx input lexed measure)
                printTokenRow ">" ctx input lexed rAngle
            )

let rec printIdentOrOp (ctx: PrintContext) (input: string) (lexed: Lexed) (identOrOp: IdentOrOp<SyntaxToken>) =
    match identOrOp with
    | IdentOrOp.Ident ident -> printTokenRow "Ident" ctx input lexed ident
    | IdentOrOp.ParenOp(lParen, opName, rParen) ->
        printSection
            ctx
            "ParenOp"
            (fun () ->
                printTokenRow "" ctx input lexed lParen
                printOpName ctx input lexed opName
                printTokenRow "" ctx input lexed rParen
            )
    | IdentOrOp.StarOp(lParen, star, rParen) -> ctx.WriteLine("StarOp: ")

and printOpName (ctx: PrintContext) (input: string) (lexed: Lexed) (opName: OpName<SyntaxToken>) =
    match opName with
    | OpName.SymbolicOp op -> printTokenRow "SymbolicOp" ctx input lexed op
    | OpName.RangeOp rangeOp -> printSection ctx "RangeOp" (fun () -> printRangeOpName ctx input lexed rangeOp)
    | OpName.ActivePatternOp activePatternOp ->
        printSection ctx "ActivePatternOp" (fun () -> printActivePatternOpName ctx input lexed activePatternOp)

and printRangeOpName (ctx: PrintContext) (input: string) (lexed: Lexed) (rangeOpName: RangeOpName<SyntaxToken>) =
    match rangeOpName with
    | RangeOpName.DotDot dotDot -> printTokenRow "DotDot" ctx input lexed dotDot
    | RangeOpName.DotDotDotDot dotDotDotDot -> printTokenRow "DotDotDotDot" ctx input lexed dotDotDotDot

and printActivePatternOpName
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (activePatternOpName: ActivePatternOpName<SyntaxToken>)
    =
    match activePatternOpName with
    | ActivePatternOpName.ActivePatternOp(lBar, idents, finalUnderscore, rBar) ->
        printSection
            ctx
            "ActivePatternOp"
            (fun () ->
                printTokenRow "" ctx input lexed lBar

                for ident in idents do
                    printTokenRow "" ctx input lexed ident

                match finalUnderscore with
                | ValueSome u -> printTokenRow "" ctx input lexed u
                | ValueNone -> ()

                printTokenRow "" ctx input lexed rBar
            )

let printLongIdentOrOp (ctx: PrintContext) (input: string) (lexed: Lexed) (longIdentOrOp: LongIdentOrOp<SyntaxToken>) =
    match longIdentOrOp with
    | LongIdentOrOp.LongIdent idents ->
        printSection
            ctx
            "LongIdent"
            (fun () ->
                for ident in idents do
                    printTokenRow "" ctx input lexed ident
            )
    | LongIdentOrOp.Op identOrOp -> ctx.WriteLine("Op: ")
    | LongIdentOrOp.QualifiedOp(longIdent, dot, op) -> ctx.WriteLine("QualifiedOp: ")


let printPatParam (ctx: PrintContext) (input: string) (lexed: Lexed) (param: PatParam<SyntaxToken>) =
    match param with
    | PatParam.Const value -> printTokenRow "PatParam.Const" ctx input lexed value
    | PatParam.LongIdent ident ->
        printSection
            ctx
            "PatParam.LongIdent"
            (fun () -> printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent ident))
    | PatParam.App(ident, innerParam) ->
        printSection
            ctx
            "PatParam.App"
            (fun () ->
                printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent ident)
                printPatParam ctx input lexed innerParam
            )
    | PatParam.List(lBracket, parameters, rBracket) ->
        printLabelledToken "PatParam.List" ctx input lexed lBracket

        indent
            ctx
            (fun () ->
                for p in parameters do
                    printPatParam ctx input lexed p
            )

        printTokenRow "" ctx input lexed rBracket
    | PatParam.Tuple(lParen, parameters, rParen) ->
        printLabelledToken "PatParam.Tuple" ctx input lexed lParen

        indent
            ctx
            (fun () ->
                for p in parameters do
                    printPatParam ctx input lexed p
            )

        printTokenRow "" ctx input lexed rParen
    | PatParam.Typed(innerParam, colon, typ) ->
        printSection
            ctx
            "PatParam.Typed"
            (fun () ->
                printPatParam ctx input lexed innerParam
                printLabelledToken "Colon" ctx input lexed colon
                printType ctx input lexed typ
            )
    | PatParam.Null nullToken -> printTokenRow "PatParam.Null" ctx input lexed nullToken
    | PatParam.Quoted _ -> failwith "printPatParam: Quoted not implemented"
    | PatParam.DoubleQuoted _ -> failwith "printPatParam: DoubleQuoted not implemented"

let printPat (ctx: PrintContext) (input: string) (lexed: Lexed) (pat: Pat<SyntaxToken>) =
    match pat with
    | Pat.Const value -> printConstant "Pat.Const" ctx input lexed value
    | Pat.NamedSimple ident -> printTokenRow "Pat.NamedSimple" ctx input lexed ident
    | Pat.Wildcard underscore -> printTokenRow "Pat.Wildcard" ctx input lexed underscore
    | Pat.Named(longIdent, param, innerPat) ->
        printSection
            ctx
            "Pat.Named"
            (fun () ->
                for ident in longIdent do
                    printTokenRow "Ident" ctx input lexed ident

                match param with
                | ValueSome p -> printSection ctx "Param" (fun () -> printPatParam ctx input lexed p)
                | ValueNone -> ()

                match innerPat with
                | ValueSome p -> printSection ctx "Pat" (fun () -> printPat ctx input lexed p)
                | ValueNone -> ()
            )
    | Pat.As(innerPat, asToken, ident) ->
        printSection
            ctx
            "Pat.As"
            (fun () ->
                printPat ctx input lexed innerPat
                printLabelledToken "As" ctx input lexed asToken
                printTokenRow "Ident" ctx input lexed ident
            )
    | Pat.Or(left, bar, right) ->
        printLabelledToken "Pat.Or" ctx input lexed bar

        indent
            ctx
            (fun () ->
                printPat ctx input lexed left
                printPat ctx input lexed right
            )
    | Pat.And(left, ampersand, right) ->
        printLabelledToken "Pat.And" ctx input lexed ampersand

        indent
            ctx
            (fun () ->
                printPat ctx input lexed left
                printPat ctx input lexed right
            )
    | Pat.Cons(head, consToken, tail) ->
        printLabelledToken "Pat.Cons" ctx input lexed consToken

        indent
            ctx
            (fun () ->
                printPat ctx input lexed head
                printPat ctx input lexed tail
            )
    | Pat.Typed(innerPat, colon, typ) ->
        printSection
            ctx
            "Pat.Typed"
            (fun () ->
                printPat ctx input lexed innerPat
                printLabelledToken "Colon" ctx input lexed colon
                printType ctx input lexed typ
            )
    | Pat.Tuple(patterns, _commas) ->
        printSection
            ctx
            "Pat.Tuple"
            (fun () ->
                for p in patterns do
                    printPat ctx input lexed p
            )
    | Pat.StructTuple(structToken, _lParen, patterns, _commas, _rParen) ->
        printSection
            ctx
            "Pat.StructTuple"
            (fun () ->
                printTokenRow "" ctx input lexed structToken

                for p in patterns do
                    printPat ctx input lexed p
            )
    | Pat.Paren(lParen, innerPat, rParen) ->
        printLabelledToken "Pat.Paren" ctx input lexed lParen
        indent ctx (fun () -> printPat ctx input lexed innerPat)
        printTokenRow "" ctx input lexed rParen
    | Pat.List(ListPat(lBracket, patterns, rBracket)) ->
        printLabelledToken "Pat.List" ctx input lexed lBracket

        indent
            ctx
            (fun () ->
                for p in patterns do
                    printPat ctx input lexed p
            )

        printTokenRow "" ctx input lexed rBracket
    | Pat.Array(ArrayPat(lBarBracket, patterns, rBarBracket)) ->
        printLabelledToken "Pat.Array" ctx input lexed lBarBracket

        indent
            ctx
            (fun () ->
                for p in patterns do
                    printPat ctx input lexed p
            )

        printTokenRow "" ctx input lexed rBarBracket
    | Pat.Record(RecordPat(lBrace, fieldPats, rBrace)) ->
        printLabelledToken "Pat.Record" ctx input lexed lBrace

        indent
            ctx
            (fun () ->
                for FieldPat(longIdent, equals, innerPat) in fieldPats do
                    printSection
                        ctx
                        "FieldPat"
                        (fun () ->
                            for ident in longIdent do
                                printTokenRow "Ident" ctx input lexed ident

                            printLabelledToken "=" ctx input lexed equals
                            printSection ctx "Pat" (fun () -> printPat ctx input lexed innerPat)
                        )
            )

        printTokenRow "" ctx input lexed rBrace
    | Pat.TypeTest(colonQuestion, typ) ->
        printSection
            ctx
            "Pat.TypeTest"
            (fun () ->
                printLabelledToken "ColonQuestion" ctx input lexed colonQuestion
                printType ctx input lexed typ
            )
    | Pat.TypeTestAs(colonQuestion, typ, asToken, ident) ->
        printSection
            ctx
            "Pat.TypeTestAs"
            (fun () ->
                printLabelledToken "ColonQuestion" ctx input lexed colonQuestion
                printType ctx input lexed typ
                printLabelledToken "As" ctx input lexed asToken
                printTokenRow "Ident" ctx input lexed ident
            )
    | Pat.Null nullToken -> printTokenRow "Pat.Null" ctx input lexed nullToken
    | Pat.Attributed(attributes, innerPat) ->
        printSection
            ctx
            "Pat.Attributed"
            (fun () ->
                ctx.WriteLine($"(Attributes: {attributes.Length} set(s))")
                printPat ctx input lexed innerPat
            )
    | Pat.Struct(structToken, innerPat) ->
        printSection
            ctx
            "Pat.Struct"
            (fun () ->
                printTokenRow "" ctx input lexed structToken
                printPat ctx input lexed innerPat
            )
    | Pat.Missing -> ctx.WriteLine("Missing")
    | Pat.SkipsTokens(skippedTokens, innerPat) ->
        printSection
            ctx
            "SkipsTokens"
            (fun () ->
                for t in skippedTokens do
                    printTokenRow "(skipped)" ctx input lexed t

                printPat ctx input lexed innerPat
            )

let printTypar (ctx: PrintContext) (input: string) (lexed: Lexed) (typar: Typar<SyntaxToken>) =
    match typar with
    | Typar.Anon underscore -> printTokenRow "Typar.Anon" ctx input lexed underscore
    | Typar.Named(quote, ident) ->
        printSection
            ctx
            "Typar.Named"
            (fun () ->
                printTokenRow "" ctx input lexed quote
                printTokenRow "" ctx input lexed ident
            )
    | Typar.Static(caret, ident) ->
        printSection
            ctx
            "Typar.Static"
            (fun () ->
                printTokenRow "" ctx input lexed caret
                printTokenRow "" ctx input lexed ident
            )

let printConstraint (ctx: PrintContext) (input: string) (lexed: Lexed) (c: Constraint<SyntaxToken>) =
    match c with
    | Constraint.Coercion(typar, colonGT, typ) ->
        printSection
            ctx
            "Constraint.Coercion"
            (fun () ->
                printTypar ctx input lexed typar
                printLabelledToken ":>" ctx input lexed colonGT
                printType ctx input lexed typ
            )
    | Constraint.Nullness(typar, _colon, _nullToken) ->
        printSection ctx "Constraint.Nullness" (fun () -> printTypar ctx input lexed typar)
    | Constraint.MemberTrait(_staticTypars, _colon, _lParen, _membersign, _rParen) ->
        ctx.WriteLine("Constraint.MemberTrait: (not fully printed)")
    | Constraint.DefaultConstructor(typar, _colon, _lParen, _newToken, _colonUnit, _arrow, _quoteT, _rParen) ->
        printSection ctx "Constraint.DefaultConstructor" (fun () -> printTypar ctx input lexed typar)
    | Constraint.Struct(typar, _colon, _structToken) ->
        printSection ctx "Constraint.Struct" (fun () -> printTypar ctx input lexed typar)
    | Constraint.ReferenceType(typar, _colon, _notToken, _structToken) ->
        printSection ctx "Constraint.ReferenceType" (fun () -> printTypar ctx input lexed typar)
    | Constraint.Enum(typar, _colon, _enumToken, _lAngle, typ, _rAngle) ->
        printSection
            ctx
            "Constraint.Enum"
            (fun () ->
                printTypar ctx input lexed typar
                printType ctx input lexed typ
            )
    | Constraint.Unmanaged(typar, _colon, _unmanagedToken) ->
        printSection ctx "Constraint.Unmanaged" (fun () -> printTypar ctx input lexed typar)
    | Constraint.Delegate(typar, _colon, _delegateToken, _lAngle, type1, _comma, type2, _rAngle) ->
        printSection
            ctx
            "Constraint.Delegate"
            (fun () ->
                printTypar ctx input lexed typar
                printType ctx input lexed type1
                printType ctx input lexed type2
            )
    | Constraint.Equality(typar, _colon, _equalityToken) ->
        printSection ctx "Constraint.Equality" (fun () -> printTypar ctx input lexed typar)
    | Constraint.Comparison(typar, _colon, _comparisonToken) ->
        printSection ctx "Constraint.Comparison" (fun () -> printTypar ctx input lexed typar)

let printTyparDefns (ctx: PrintContext) (input: string) (lexed: Lexed) (typars: TyparDefns<SyntaxToken>) =
    let (TyparDefns(lAngle, defns, constraints, rAngle)) = typars

    printSection
        ctx
        "TyparDefns"
        (fun () ->
            printTokenRow "" ctx input lexed lAngle

            for (TyparDefn(_attrs, typar)) in defns do
                printTypar ctx input lexed typar

            match constraints with
            | ValueSome(TyparConstraints(whenToken, constraintList)) ->
                printLabelledToken "when" ctx input lexed whenToken

                for c in constraintList do
                    printConstraint ctx input lexed c
            | ValueNone -> ()

            printTokenRow "" ctx input lexed rAngle
        )

let printTypeArg (ctx: PrintContext) (input: string) (lexed: Lexed) (typeArg: TypeArg<SyntaxToken>) =
    match typeArg with
    | TypeArg.Type ty -> printSection ctx "TypeArg.Type" (fun () -> printType ctx input lexed ty)
    | TypeArg.Measure measure -> printSection ctx "TypeArg.Measure" (fun () -> printMeasure ctx input lexed measure)
    | TypeArg.StaticParameter staticParam -> printTokenRow "TypeArg.StaticParameter" ctx input lexed staticParam

let rec printMeasure (ctx: PrintContext) (input: string) (lexed: Lexed) (measure: Measure<SyntaxToken>) =
    match measure with
    | Measure.Named longIdent ->
        printSection
            ctx
            "Measure.Named"
            (fun () -> printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent longIdent))
    | Measure.One oneToken -> printTokenRow "Measure.One" ctx input lexed oneToken
    | Measure.Anonymous underscore -> printTokenRow "Measure.Anonymous" ctx input lexed underscore
    | Measure.Typar typar -> printSection ctx "Measure.Typar" (fun () -> printTypar ctx input lexed typar)
    | Measure.Juxtaposition(measures, ops) ->
        printSection
            ctx
            "Measure.Juxtaposition"
            (fun () ->
                for m in measures do
                    printMeasure ctx input lexed m
            )
    | Measure.Power(baseMeasure, powerToken, exponent) ->
        printSection
            ctx
            "Measure.Power"
            (fun () ->
                printMeasure ctx input lexed baseMeasure
                printTokenRow "" ctx input lexed powerToken
                printTokenRow "" ctx input lexed exponent
            )
    | Measure.Product(left, mulToken, right) ->
        printSection
            ctx
            "Measure.Product"
            (fun () ->
                printMeasure ctx input lexed left
                printTokenRow "" ctx input lexed mulToken
                printMeasure ctx input lexed right
            )
    | Measure.Quotient(numerator, divToken, denominator) ->
        printSection
            ctx
            "Measure.Quotient"
            (fun () ->
                printMeasure ctx input lexed numerator
                printTokenRow "" ctx input lexed divToken
                printMeasure ctx input lexed denominator
            )
    | Measure.Reciprocal(divToken, measure) ->
        printSection
            ctx
            "Measure.Reciprocal"
            (fun () ->
                printTokenRow "" ctx input lexed divToken
                printMeasure ctx input lexed measure
            )
    | Measure.Paren(lParen, measure, rParen) ->
        printSection
            ctx
            "Measure.Paren"
            (fun () ->
                printTokenRow "" ctx input lexed lParen
                printMeasure ctx input lexed measure
                printTokenRow "" ctx input lexed rParen
            )

let printType (ctx: PrintContext) (input: string) (lexed: Lexed) (ty: Type<SyntaxToken>) =
    match ty with
    | Type.ParenType(lParen, typ, rParen) ->
        printLabelledToken "ParenType" ctx input lexed lParen
        indent ctx (fun () -> printType ctx input lexed typ)
        printTokenRow "" ctx input lexed rParen
    | Type.VarType typar -> printSection ctx "VarType" (fun () -> printTypar ctx input lexed typar)
    | Type.NamedType longIdent ->
        printSection ctx "NamedType" (fun () -> printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent longIdent))
    | Type.GenericType(longIdent, lAngle, typeArgs, rAngle) ->
        printSection
            ctx
            "GenericType"
            (fun () ->
                printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent longIdent)
                printTokenRow "<" ctx input lexed lAngle

                for typeArg in typeArgs do
                    printTypeArg ctx input lexed typeArg

                printTokenRow ">" ctx input lexed rAngle
            )
    | Type.FunctionType(fromType, arrow, toType) ->
        printSection
            ctx
            "FunctionType"
            (fun () ->
                printType ctx input lexed fromType
                printLabelledToken "->" ctx input lexed arrow
                printType ctx input lexed toType
            )
    | Type.TupleType(types) ->
        printSection
            ctx
            "TupleType"
            (fun () ->
                for t in types do
                    printType ctx input lexed t
            )
    | Type.StructTupleType(structToken, _lParen, types, _rParen) ->
        printSection
            ctx
            "StructTupleType"
            (fun () ->
                printTokenRow "" ctx input lexed structToken

                for t in types do
                    printType ctx input lexed t
            )
    | Type.IncompleteGenericType(longIdent, lAngle, rAngle) ->
        printSection
            ctx
            "IncompleteGenericType"
            (fun () ->
                printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent longIdent)
                printTokenRow "<" ctx input lexed lAngle
                printTokenRow ">" ctx input lexed rAngle
            )
    | Type.SuffixedType(baseType, longIdent) ->
        printSection
            ctx
            "SuffixedType"
            (fun () ->
                printType ctx input lexed baseType
                printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent longIdent)
            )
    | Type.ArrayType(baseType, lBracket, commas, rBracket) ->
        printSection
            ctx
            "ArrayType"
            (fun () ->
                printType ctx input lexed baseType
                printTokenRow "" ctx input lexed lBracket

                for comma in commas do
                    printTokenRow "" ctx input lexed comma

                printTokenRow "" ctx input lexed rBracket
            )
    | Type.ConstrainedType(typ, constraints) ->
        printSection
            ctx
            "ConstrainedType"
            (fun () ->
                printType ctx input lexed typ
                printTyparDefns ctx input lexed constraints
            )
    | Type.SubtypeConstraint(typar, colonGreaterThan, typ) ->
        printSection
            ctx
            "SubtypeConstraint"
            (fun () ->
                printTypar ctx input lexed typar
                printLabelledToken ":>" ctx input lexed colonGreaterThan
                printType ctx input lexed typ
            )
    | Type.AnonymousSubtype(hash, typ) ->
        printSection
            ctx
            "AnonymousSubtype"
            (fun () ->
                printTokenRow "" ctx input lexed hash
                printType ctx input lexed typ
            )
    | Type.Missing -> ctx.WriteLine("Missing")
    | Type.SkipsTokens(skippedTokens, innerType) ->
        printSection
            ctx
            "SkipsTokens"
            (fun () ->
                for t in skippedTokens do
                    printTokenRow "(skipped)" ctx input lexed t

                printType ctx input lexed innerType
            )

let printValueDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (valueDefn: ValueDefn<SyntaxToken>) =
    match valueDefn with
    | ValueDefn(mutableToken, access, pat, typarDefns, returnType, equals, expr) ->
        match mutableToken with
        | ValueSome t -> printTokenRow "mutable" ctx input lexed t
        | ValueNone -> ()

        match access with
        | ValueSome a -> printTokenRow "access" ctx input lexed a
        | ValueNone -> ()

        printPat ctx input lexed pat

        match typarDefns with
        | ValueSome typars -> printTyparDefns ctx input lexed typars
        | ValueNone -> ()

        match returnType with
        | ValueSome(ReturnType(colon, typ)) ->
            printLabelledToken "ReturnColon" ctx input lexed colon
            printType ctx input lexed typ
        | ValueNone -> ()

        printLabelledToken "=" ctx input lexed equals
        indent ctx (fun () -> printExpr ctx input lexed expr)

let printFunctionDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (functionDefn: FunctionDefn<SyntaxToken>) =
    let (FunctionDefn(inlineToken, access, identOrOp, typarDefns, argumentPats, returnType, equals, expr)) =
        functionDefn

    match inlineToken with
    | ValueSome t -> printLabelledToken "inline" ctx input lexed t
    | ValueNone -> ()

    match access with
    | ValueSome a -> printLabelledToken "access" ctx input lexed a
    | ValueNone -> ()

    printSection ctx "IdentOrOp" (fun () -> printIdentOrOp ctx input lexed identOrOp)

    match typarDefns with
    | ValueSome typars -> printTyparDefns ctx input lexed typars
    | ValueNone -> ()

    printSection
        ctx
        "Pats"
        (fun () ->
            for pat in argumentPats do
                printPat ctx input lexed pat
        )

    match returnType with
    | ValueSome(ReturnType(colon, typ)) ->
        printLabelledToken "ReturnColon" ctx input lexed colon
        printType ctx input lexed typ
    | ValueNone -> ()

    printLabelledToken "=" ctx input lexed equals
    indent ctx (fun () -> printExpr ctx input lexed expr)

let printFunctionOrValueDefn
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (defn: FunctionOrValueDefn<SyntaxToken>)
    =
    match defn with
    | FunctionOrValueDefn.Function functionDefn ->
        printSection ctx "Function" (fun () -> printFunctionDefn ctx input lexed functionDefn)
    | FunctionOrValueDefn.Value valueDefn ->
        printSection ctx "Value" (fun () -> printValueDefn ctx input lexed valueDefn)

let printFieldInitializer
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (fieldInit: FieldInitializer<SyntaxToken>)
    =
    let (FieldInitializer(longIdent, equals, fieldExpr)) = fieldInit

    printSection
        ctx
        "Field"
        (fun () ->
            printLongIdentOrOp ctx input lexed (LongIdentOrOp.LongIdent longIdent)
            printLabelledToken "=" ctx input lexed equals
            printSection ctx "Expr" (fun () -> printExpr ctx input lexed fieldExpr)
        )

let printExpr (ctx: PrintContext) (input: string) (lexed: Lexed) (expr: Expr<SyntaxToken>) =
    match expr with
    | Expr.Const value -> printConstant "Const" ctx input lexed value
    | Expr.Ident ident -> printTokenRow "Ident" ctx input lexed ident
    | Expr.LetValue(letToken, valueDefn, inToken, body) ->
        printLabelledToken "LetValue" ctx input lexed letToken

        indent
            ctx
            (fun () ->
                printValueDefn ctx input lexed valueDefn
                printLabelledToken "in" ctx input lexed inToken
                printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
            )
    | Expr.InfixApp(left, op, right) ->
        printLabelledToken "InfixApp" ctx input lexed op

        indent
            ctx
            (fun () ->
                printExpr ctx input lexed left
                printExpr ctx input lexed right
            )
    | Expr.PrefixApp(op, expr) ->
        printLabelledToken "PrefixApp" ctx input lexed op
        indent ctx (fun () -> printExpr ctx input lexed expr)
    | Expr.Sequential(left, sep, right) ->
        printLabelledToken "Sequential" ctx input lexed sep

        indent
            ctx
            (fun () ->
                printExpr ctx input lexed left
                printExpr ctx input lexed right
            )
    | Expr.Tuple elements ->
        printSection
            ctx
            "Tuple"
            (fun () ->
                for elem in elements do
                    printExpr ctx input lexed elem
            )
    | Expr.StructTuple(kw, lParen, elements, rParen) ->
        printSection
            ctx
            "StructTuple"
            (fun () ->
                for elem in elements do
                    printExpr ctx input lexed elem
            )
    | Expr.List(lBracket, elements, rBracket) ->
        printLabelledToken "List" ctx input lexed lBracket

        indent
            ctx
            (fun () ->
                for elem in elements do
                    printExpr ctx input lexed elem
            )

        printTokenRow "" ctx input lexed rBracket
    | Expr.Array(lBracket, elements, rBracket) ->
        printLabelledToken "Array" ctx input lexed lBracket

        indent
            ctx
            (fun () ->
                for elem in elements do
                    printExpr ctx input lexed elem
            )

        printTokenRow "" ctx input lexed rBracket
    | Expr.ParenBlock(l, expr, r) ->
        printLabelledToken "ParenBlock" ctx input lexed l
        indent ctx (fun () -> printExpr ctx input lexed expr)
        printTokenRow "" ctx input lexed r
    | Expr.BeginEndBlock(l, expr, r) ->
        printLabelledToken "BeginEndBlock" ctx input lexed l
        indent ctx (fun () -> printExpr ctx input lexed expr)
        printTokenRow "" ctx input lexed r
    | Expr.LongIdentOrOp longIdentOrOp ->
        printSection ctx "LongIdentOrOp" (fun () -> printLongIdentOrOp ctx input lexed longIdentOrOp)
    | Expr.TypeApp(expr, lAngle, types, rAngle) ->
        printSection
            ctx
            "TypeApp"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed expr)

                printSection
                    ctx
                    "Types"
                    (fun () ->
                        for ty in types do
                            printType ctx input lexed ty
                    )
            )
    | Expr.DotLookup(expr, dot, longIdentOrOp) ->
        printSection
            ctx
            "DotLookup"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed expr)
                printLabelledToken "Dot" ctx input lexed dot
                printSection ctx "LongIdentOrOp" (fun () -> printLongIdentOrOp ctx input lexed longIdentOrOp)
            )
    | Expr.IfThenElse(ifToken, condition, thenToken, thenExpr, elifBranches, elseBranch) ->
        printSection
            ctx
            "IfThenElse"
            (fun () ->
                printLabelledToken "IfToken" ctx input lexed ifToken
                printSection ctx "Condition" (fun () -> printExpr ctx input lexed condition)
                printLabelledToken "ThenToken" ctx input lexed thenToken
                printSection ctx "ThenExpr" (fun () -> printExpr ctx input lexed thenExpr)

                for elif' in elifBranches do
                    printSection
                        ctx
                        "ElifBranch"
                        (fun () ->
                            let (ElifBranch.ElifBranch(elifToken, elifCondition, thenToken, elifExpr)) = elif'
                            printLabelledToken "ElifToken" ctx input lexed elifToken
                            printSection ctx "ElifCondition" (fun () -> printExpr ctx input lexed elifCondition)
                            printLabelledToken "ThenToken" ctx input lexed thenToken
                            printSection ctx "ElifExpr" (fun () -> printExpr ctx input lexed elifExpr)
                        )

                match elseBranch with
                | ValueSome(ElseBranch.ElseBranch(elseToken, elseExpr)) ->
                    printLabelledToken "ElseToken" ctx input lexed elseToken
                    printSection ctx "ElseExpr" (fun () -> printExpr ctx input lexed elseExpr)
                | ValueNone -> ()
            )
    | Expr.Match(matchToken, matchExpr, withToken, rules) ->
        printSection
            ctx
            "Match"
            (fun () ->
                printLabelledToken "MatchToken" ctx input lexed matchToken
                printSection ctx "MatchExpr" (fun () -> printExpr ctx input lexed matchExpr)
                printLabelledToken "WithToken" ctx input lexed withToken
                printRules ctx input lexed rules
            )
    | Expr.Fun(funToken, pats, arrow, expr) ->
        printLabelledToken "Fun" ctx input lexed funToken

        printSection
            ctx
            "Pats"
            (fun () ->
                for pat in pats do
                    printPat ctx input lexed pat
            )

        printLabelledToken "Arrow" ctx input lexed arrow
        printSection ctx "Body" (fun () -> printExpr ctx input lexed expr)
    | Expr.TryWith(tryToken, tryExpr, withToken, rules) ->
        printLabelledToken "TryWith" ctx input lexed tryToken
        printSection ctx "TryExpr" (fun () -> printExpr ctx input lexed tryExpr)
        printLabelledToken "WithToken" ctx input lexed withToken
        printRules ctx input lexed rules
    | Expr.TryFinally(tryToken, tryExpr, finallyToken, finallyExpr) ->
        printLabelledToken "TryFinally" ctx input lexed tryToken
        printSection ctx "TryExpr" (fun () -> printExpr ctx input lexed tryExpr)
        printLabelledToken "FinallyToken" ctx input lexed finallyToken
        printSection ctx "FinallyExpr" (fun () -> printExpr ctx input lexed finallyExpr)
    | Expr.While(whileToken, cond, doToken, body, doneToken) ->
        printLabelledToken "While" ctx input lexed whileToken
        printSection ctx "Cond" (fun () -> printExpr ctx input lexed cond)
        printLabelledToken "DoToken" ctx input lexed doToken
        printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
        printLabelledToken "DoneToken" ctx input lexed doneToken
    | Expr.ForTo(forToken, ident, equals, startExpr, toToken, endExpr, doToken, body, doneToken) ->
        printLabelledToken "ForTo" ctx input lexed forToken
        printTokenRow "Ident" ctx input lexed ident
        printLabelledToken "Equals" ctx input lexed equals
        printSection ctx "Start" (fun () -> printExpr ctx input lexed startExpr)
        printLabelledToken "ToToken" ctx input lexed toToken
        printSection ctx "End" (fun () -> printExpr ctx input lexed endExpr)
        printLabelledToken "DoToken" ctx input lexed doToken
        printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
        printLabelledToken "DoneToken" ctx input lexed doneToken
    | Expr.ForIn(forToken, pat, inToken, range, doToken, body, doneToken) ->
        printLabelledToken "ForIn" ctx input lexed forToken
        printSection ctx "Pat" (fun () -> printPat ctx input lexed pat)
        printLabelledToken "InToken" ctx input lexed inToken

        printSection
            ctx
            "Range"
            (fun () ->
                match range with
                | ExprOrRange.Expr e -> printExpr ctx input lexed e
                | ExprOrRange.Range _ -> ctx.WriteLine("(range)")
            )

        printLabelledToken "DoToken" ctx input lexed doToken
        printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
        printLabelledToken "DoneToken" ctx input lexed doneToken
    | Expr.Use(useToken, ident, equals, expr, inToken, body) ->
        printLabelledToken "Use" ctx input lexed useToken
        printTokenRow "Ident" ctx input lexed ident
        printLabelledToken "Equals" ctx input lexed equals
        printSection ctx "Expr" (fun () -> printExpr ctx input lexed expr)
        printLabelledToken "InToken" ctx input lexed inToken
        printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
    | Expr.App(funcExpr, argExpr) ->
        printSection
            ctx
            "App"
            (fun () ->
                printSection ctx "Func" (fun () -> printExpr ctx input lexed funcExpr)
                printSection ctx "Arg" (fun () -> printExpr ctx input lexed argExpr)
            )
    | Expr.HighPrecedenceApp(funcExpr, lParen, argExpr, rParen) ->
        printSection
            ctx
            "HighPrecedenceApp"
            (fun () ->
                printSection ctx "Func" (fun () -> printExpr ctx input lexed funcExpr)
                printLabelledToken "(" ctx input lexed lParen
                printSection ctx "Arg" (fun () -> printExpr ctx input lexed argExpr)
                printTokenRow "" ctx input lexed rParen
            )
    | Expr.TypeAnnotation(innerExpr, colon, typ) ->
        printSection
            ctx
            "TypeAnnotation"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed innerExpr)
                printLabelledToken "Colon" ctx input lexed colon
                printType ctx input lexed typ
            )
    | Expr.Lazy(lazyToken, innerExpr) ->
        printLabelledToken "Lazy" ctx input lexed lazyToken
        indent ctx (fun () -> printExpr ctx input lexed innerExpr)
    | Expr.Assert(assertToken, innerExpr) ->
        printLabelledToken "Assert" ctx input lexed assertToken
        indent ctx (fun () -> printExpr ctx input lexed innerExpr)
    | Expr.Null nullToken -> printTokenRow "Null" ctx input lexed nullToken
    | Expr.Function(functionToken, rules) ->
        printLabelledToken "Function" ctx input lexed functionToken
        indent ctx (fun () -> printRules ctx input lexed rules)
    | Expr.LetFunction(letToken, functionDefn, inToken, body) ->
        printLabelledToken "LetFunction" ctx input lexed letToken

        indent
            ctx
            (fun () ->
                printFunctionDefn ctx input lexed functionDefn
                printLabelledToken "in" ctx input lexed inToken
                printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
            )
    | Expr.LetRec(letToken, recToken, defns, inToken, body) ->
        printLabelledToken "LetRec" ctx input lexed letToken

        indent
            ctx
            (fun () ->
                printLabelledToken "rec" ctx input lexed recToken

                for defn in defns do
                    printFunctionOrValueDefn ctx input lexed defn

                printLabelledToken "in" ctx input lexed inToken
                printSection ctx "Body" (fun () -> printExpr ctx input lexed body)
            )
    | Expr.New(newToken, typ, newExpr) ->
        printLabelledToken "New" ctx input lexed newToken

        indent
            ctx
            (fun () ->
                printType ctx input lexed typ
                printSection ctx "Arg" (fun () -> printExpr ctx input lexed newExpr)
            )
    | Expr.Assignment(leftExpr, arrow, rightExpr) ->
        printLabelledToken "Assignment" ctx input lexed arrow

        indent
            ctx
            (fun () ->
                printExpr ctx input lexed leftExpr
                printExpr ctx input lexed rightExpr
            )
    | Expr.Record(lBrace, fieldInitializers, rBrace) ->
        printLabelledToken "Record" ctx input lexed lBrace

        indent
            ctx
            (fun () ->
                for fi in fieldInitializers do
                    printFieldInitializer ctx input lexed fi
            )

        printTokenRow "" ctx input lexed rBrace
    | Expr.RecordClone(lBrace, baseExpr, withToken, fieldInitializers, rBrace) ->
        printLabelledToken "RecordClone" ctx input lexed lBrace

        indent
            ctx
            (fun () ->
                printSection ctx "Base" (fun () -> printExpr ctx input lexed baseExpr)
                printLabelledToken "with" ctx input lexed withToken

                for fi in fieldInitializers do
                    printFieldInitializer ctx input lexed fi
            )

        printTokenRow "" ctx input lexed rBrace
    | Expr.IndexedLookup(indexedExpr, dot, lBracket, indexArgExpr, rBracket) ->
        printSection
            ctx
            "IndexedLookup"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed indexedExpr)
                printLabelledToken "." ctx input lexed dot
                printLabelledToken "[" ctx input lexed lBracket
                printSection ctx "Index" (fun () -> printExpr ctx input lexed indexArgExpr)
                printTokenRow "" ctx input lexed rBracket
            )
    | Expr.StaticUpcast(castExpr, colonGT, typ) ->
        printSection
            ctx
            "StaticUpcast"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed castExpr)
                printLabelledToken ":>" ctx input lexed colonGT
                printType ctx input lexed typ
            )
    | Expr.DynamicTypeTest(testExpr, colonQ, typ) ->
        printSection
            ctx
            "DynamicTypeTest"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed testExpr)
                printLabelledToken ":?" ctx input lexed colonQ
                printType ctx input lexed typ
            )
    | Expr.DynamicDowncast(castExpr, colonQGT, typ) ->
        printSection
            ctx
            "DynamicDowncast"
            (fun () ->
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed castExpr)
                printLabelledToken ":?>" ctx input lexed colonQGT
                printType ctx input lexed typ
            )
    | Expr.Upcast(upcastToken, castExpr) ->
        printLabelledToken "Upcast" ctx input lexed upcastToken
        indent ctx (fun () -> printExpr ctx input lexed castExpr)
    | Expr.Downcast(downcastToken, castExpr) ->
        printLabelledToken "Downcast" ctx input lexed downcastToken
        indent ctx (fun () -> printExpr ctx input lexed castExpr)
    | Expr.UseFixed(useToken, ident, equals, fixedToken, fixedExpr) ->
        printLabelledToken "UseFixed" ctx input lexed useToken

        indent
            ctx
            (fun () ->
                printTokenRow "Ident" ctx input lexed ident
                printLabelledToken "fixed" ctx input lexed fixedToken
                printSection ctx "Expr" (fun () -> printExpr ctx input lexed fixedExpr)
            )
    | Expr.Missing -> ctx.WriteLine("Missing")
    | Expr.SkipsTokens(skippedTokens, innerExpr) ->
        printSection
            ctx
            "SkipsTokens"
            (fun () ->
                for t in skippedTokens do
                    printTokenRow "(skipped)" ctx input lexed t

                printSection ctx "Expr" (fun () -> printExpr ctx input lexed innerExpr)
            )
    | Expr.Pat innerPat -> printSection ctx "Pat" (fun () -> printPat ctx input lexed innerPat)
    | _ -> failwithf "Not implemented %A" expr

let printModuleFunctionOrValueDefn
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (defn: ModuleFunctionOrValueDefn<SyntaxToken>)
    =
    match defn with
    | ModuleFunctionOrValueDefn.LetFunction(attrs, letToken, functionDefn) ->
        printLabelledToken "let" ctx input lexed letToken
        indent ctx (fun () -> printFunctionDefn ctx input lexed functionDefn)
    | ModuleFunctionOrValueDefn.LetValue(attrs, letToken, valueDefn) ->
        printLabelledToken "let" ctx input lexed letToken
        indent ctx (fun () -> printValueDefn ctx input lexed valueDefn)
    | ModuleFunctionOrValueDefn.LetRec(attrs, letToken, recToken, defns) ->
        printLabelledToken "let" ctx input lexed letToken

        match recToken with
        | ValueSome t -> printLabelledToken "rec" ctx input lexed t
        | ValueNone -> ()

        indent
            ctx
            (fun () ->
                for defn in defns do
                    printFunctionOrValueDefn ctx input lexed defn
            )
    | ModuleFunctionOrValueDefn.Do(attrs, doToken, expr) ->
        printLabelledToken "do" ctx input lexed doToken
        indent ctx (fun () -> printExpr ctx input lexed expr)

let printImportDecl (ctx: PrintContext) (input: string) (lexed: Lexed) (decl: ImportDecl<SyntaxToken>) =
    let (ImportDecl.ImportDecl(openToken, longIdent)) = decl
    printLabelledToken "open" ctx input lexed openToken

    for ident in longIdent do
        printTokenRow "" ctx input lexed ident

let printModuleAbbrev (ctx: PrintContext) (input: string) (lexed: Lexed) (abbrev: ModuleAbbrev<SyntaxToken>) =
    let (ModuleAbbrev.ModuleAbbrev(moduleToken, ident, equals, longIdent)) = abbrev
    printLabelledToken "module" ctx input lexed moduleToken
    printTokenRow "ident" ctx input lexed ident
    printLabelledToken "=" ctx input lexed equals

    for id in longIdent do
        printTokenRow "" ctx input lexed id

let printCompilerDirective
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (decl: CompilerDirectiveDecl<SyntaxToken>)
    =
    let (CompilerDirectiveDecl.CompilerDirectiveDecl(hash, ident, strings)) = decl
    printLabelledToken "#" ctx input lexed hash
    printTokenRow "directive" ctx input lexed ident

    for s in strings do
        printTokenRow "" ctx input lexed s

let rec printModuleElem (ctx: PrintContext) (input: string) (lexed: Lexed) (elem: ModuleElem<SyntaxToken>) =
    match elem with
    | ModuleElem.FunctionOrValue defn ->
        printSection ctx "FunctionOrValue" (fun () -> printModuleFunctionOrValueDefn ctx input lexed defn)
    | ModuleElem.Type typeDefn -> ctx.WriteLine("Type: <not yet implemented>")
    | ModuleElem.Exception exnDefn -> ctx.WriteLine("Exception: <not yet implemented>")
    | ModuleElem.Module moduleDefn -> printSection ctx "Module" (fun () -> printModuleDefn ctx input lexed moduleDefn)
    | ModuleElem.ModuleAbbrev abbrev ->
        printSection ctx "ModuleAbbrev" (fun () -> printModuleAbbrev ctx input lexed abbrev)
    | ModuleElem.Import importDecl -> printSection ctx "Import" (fun () -> printImportDecl ctx input lexed importDecl)
    | ModuleElem.CompilerDirective directive ->
        printSection ctx "CompilerDirective" (fun () -> printCompilerDirective ctx input lexed directive)
    | ModuleElem.Expression expr -> printExpr ctx input lexed expr

and printModuleDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (defn: ModuleDefn<SyntaxToken>) =
    let (ModuleDefn.ModuleDefn(attrs, moduleToken, access, ident, equals, body)) = defn
    printLabelledToken "module" ctx input lexed moduleToken

    match access with
    | ValueSome a -> ctx.WriteLine("access: <not yet implemented>")
    | ValueNone -> ()

    printTokenRow "ident" ctx input lexed ident
    printLabelledToken "=" ctx input lexed equals
    let (ModuleDefnBody(beginToken, elems, endToken)) = body
    printLabelledToken "begin" ctx input lexed beginToken

    match elems with
    | ValueSome elems -> indent ctx (fun () -> printModuleElems ctx input lexed elems)
    | ValueNone -> ()

    printLabelledToken "end" ctx input lexed endToken

and printModuleElems (ctx: PrintContext) (input: string) (lexed: Lexed) (elems: ModuleElems<SyntaxToken>) =
    for elem in elems do
        printModuleElem ctx input lexed elem

let printNamespaceDeclGroup
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (group: NamespaceDeclGroup<SyntaxToken>)
    =
    match group with
    | NamespaceDeclGroup.Named(nsTok, longIdent, elems) ->
        printLabelledToken "namespace" ctx input lexed nsTok

        for id in longIdent do
            printTokenRow "" ctx input lexed id

        indent ctx (fun () -> printModuleElems ctx input lexed elems)
    | NamespaceDeclGroup.Global(nsTok, globalTok, elems) ->
        printLabelledToken "namespace" ctx input lexed nsTok
        printLabelledToken "global" ctx input lexed globalTok
        indent ctx (fun () -> printModuleElems ctx input lexed elems)

let printImplementationFile (ctx: PrintContext) (input: string) (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) =
    match file with
    | ImplementationFile.AnonymousModule elems ->
        ctx.WriteLine("AnonymousModule:")
        printModuleElems ctx input lexed elems
    | ImplementationFile.NamedModule namedModule ->
        let (NamedModule.NamedModule(modTok, longIdent, elems)) = namedModule
        printLabelledToken "module" ctx input lexed modTok

        for id in longIdent do
            printTokenRow "" ctx input lexed id

        printModuleElems ctx input lexed elems
    | ImplementationFile.Namespaces groups ->
        for group in groups do
            printNamespaceDeclGroup ctx input lexed group

let printFSharpAst (ctx: PrintContext) (input: string) (lexed: Lexed) (ast: FSharpAst<SyntaxToken>) =
    match ast with
    | FSharpAst.ImplementationFile file -> printImplementationFile ctx input lexed file
    | FSharpAst.SignatureFile _ -> ctx.WriteLine("SignatureFile: <not yet implemented>")
    | FSharpAst.ScriptFile _ -> ctx.WriteLine("ScriptFile: <not yet implemented>")
    | FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems) ->
        ctx.WriteLine("ScriptFragment:")
        printModuleElems ctx input lexed elems

/// Formats a DiagnosticCode as a short, stable string suitable for golden-file output.
let sprintDiagnosticCode (code: DiagnosticCode) : string =
    match code with
    | DiagnosticCode.MissingExpression _ -> "MissingExpression"
    | DiagnosticCode.MissingPattern _ -> "MissingPattern"
    | DiagnosticCode.MissingType _ -> "MissingType"
    | DiagnosticCode.MissingRule _ -> "MissingRule"
    | DiagnosticCode.MissingTypeDefn _ -> "MissingTypeDefn"
    | DiagnosticCode.Other msg -> $"Other({msg})"
    | DiagnosticCode.TyparInConstant _ -> "TyparInConstant"
    | DiagnosticCode.UnexpectedTokenSkipped tok ->
        let base' = TokenInfo.withoutFlags tok.Token
        $"UnexpectedTokenSkipped({base'})"
    | DiagnosticCode.UnclosedDelimiter(opened, expected) ->
        let openedBase = TokenInfo.withoutFlags opened.Token
        let expectedBase = TokenInfo.withoutFlags expected
        $"UnclosedDelimiter({openedBase}, {expectedBase})"

/// Appends a "---\nDiagnostics:" section to the buffer when there are diagnostics.
/// Diagnostics are emitted in source order (reversed from the accumulation order).
/// Has no effect when `diagnostics` is empty, so well-formed golden files are not touched.
let printDiagnostics (ctx: PrintContext) (diagnostics: Diagnostic list) =
    if not diagnostics.IsEmpty then
        ctx.WriteLine("---")
        ctx.WriteLine("Diagnostics:")

        indent
            ctx
            (fun () ->
                for diag in List.rev diagnostics do
                    let severity =
                        match diag.Severity with
                        | DiagnosticSeverity.Error -> "error"
                        | DiagnosticSeverity.Warning -> "warning"
                        | DiagnosticSeverity.Info -> "info"
                        | DiagnosticSeverity.Hint -> "hint"

                    ctx.WriteLine($"[{severity}] {sprintDiagnosticCode diag.Code} at {diag.Token.StartIndex}")
            )
