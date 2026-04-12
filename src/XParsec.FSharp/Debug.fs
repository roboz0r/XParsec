module XParsec.FSharp.Debug

open System
open System.IO

open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.AstTraversal

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

let tokenKindString (input: string) (lexed: Lexed) (token: PositionedToken) (i: int<token>) =
    let rawToken = lexed.Tokens.[i]

    // If this token has been rewritten by a split (kind or start index differs from the
    // raw lexer token at this index, e.g. `>]` split into `>` and `]`, or `^-` split into
    // `^` and `-`), use the rewritten kind name directly instead of deriving an operator
    // name from the original raw literal.
    if rawToken.Token <> token.Token || rawToken.StartIndex <> token.StartIndex then
        $"{TokenInfo.withoutFlags token.Token}"
    else
        let s = lexed.GetTokenString(i, input)

        match OperatorInfo.TryCreate token with
        | ValueSome opInfo -> opInfo.GetName(s)
        | ValueNone -> $"{TokenInfo.withoutFlags rawToken.Token}"

/// Adds a single token row to the print buffer.
let printTokenRow (label: string) (ctx: PrintContext) (input: string) (lexed: Lexed) (token: SyntaxToken) =
    let pt = token.PositionedToken

    match token.Index with
    | TokenIndex.Virtual ->
        let kind = $"{TokenInfo.withoutFlags pt.Token}"
        let charPos = pt.StartIndex
        ctx.AddTokenRow(label, "<virt>", kind, "tok=virt", $"pos=%4i{charPos}")
    | TokenIndex.Regular iT ->
        let t1 = lexed.Tokens.[iT + 1<_>]
        let charPos = pt.StartIndex
        let charEnd = t1.StartIndex - 1
        let len = charEnd - charPos + 1

        let lit =
            let rawLit =
                if len > 10 then
                    input.[charPos .. charPos + 9] + "..."
                else
                    input.[charPos..charEnd]

            rawLit.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

        let kind = tokenKindString input lexed pt iT
        ctx.AddTokenRow(label, $"'{lit}'", kind, $"tok=%4i{int iT}", $"pos=%4i{charPos}")

// ---- Visitor factory ----

/// Builds an AstVisitor<SyntaxToken> that writes debug output into ctx.
/// EnterSection with a non-empty name prints "name:" and increases indentation.
/// EnterSection with an empty name just increases indentation (like `indent`).
let makeDebugVisitor (ctx: PrintContext) (input: string) (lexed: Lexed) : AstVisitor<SyntaxToken> =
    {
        VisitToken = fun label tok -> printTokenRow label ctx input lexed tok
        EnterSection =
            fun name ->
                if name <> "" then
                    ctx.WriteLine($"{name}:")

                ctx.Indent <- ctx.Indent + 1
        ExitSection = fun _ -> ctx.Indent <- ctx.Indent - 1
        WriteLine = fun text -> ctx.WriteLine text
        EqualTokens = fun a b -> a.Index = b.Index
    }

// ---- Print functions (thin wrappers over AstTraversal walk functions) ----

let printAccess (ctx: PrintContext) (input: string) (lexed: Lexed) (access: Access<SyntaxToken>) =
    walkAccess (makeDebugVisitor ctx input lexed) access

let printConstant (label: string) (ctx: PrintContext) (input: string) (lexed: Lexed) (x: Constant<SyntaxToken>) =
    walkConstant (makeDebugVisitor ctx input lexed) label x

let printIdentOrOp (ctx: PrintContext) (input: string) (lexed: Lexed) (identOrOp: IdentOrOp<SyntaxToken>) =
    walkIdentOrOp (makeDebugVisitor ctx input lexed) identOrOp

let printOpName (ctx: PrintContext) (input: string) (lexed: Lexed) (opName: OpName<SyntaxToken>) =
    walkOpName (makeDebugVisitor ctx input lexed) opName

let printRangeOpName (ctx: PrintContext) (input: string) (lexed: Lexed) (rangeOpName: RangeOpName<SyntaxToken>) =
    walkRangeOpName (makeDebugVisitor ctx input lexed) rangeOpName

let printActivePatternOpName
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (activePatternOpName: ActivePatternOpName<SyntaxToken>)
    =
    walkActivePatternOpName (makeDebugVisitor ctx input lexed) activePatternOpName

let printLongIdentOrOp (ctx: PrintContext) (input: string) (lexed: Lexed) (longIdentOrOp: LongIdentOrOp<SyntaxToken>) =
    walkLongIdentOrOp (makeDebugVisitor ctx input lexed) longIdentOrOp

let printPat (ctx: PrintContext) (input: string) (lexed: Lexed) (pat: Pat<SyntaxToken>) =
    walkPat (makeDebugVisitor ctx input lexed) pat

let printTypar (ctx: PrintContext) (input: string) (lexed: Lexed) (typar: Typar<SyntaxToken>) =
    walkTypar (makeDebugVisitor ctx input lexed) typar

let printConstraint (ctx: PrintContext) (input: string) (lexed: Lexed) (c: Constraint<SyntaxToken>) =
    walkConstraint (makeDebugVisitor ctx input lexed) c

let printTyparDefns (ctx: PrintContext) (input: string) (lexed: Lexed) (typars: TyparDefns<SyntaxToken>) =
    walkTyparDefns (makeDebugVisitor ctx input lexed) typars

let printTypeArg (ctx: PrintContext) (input: string) (lexed: Lexed) (typeArg: TypeArg<SyntaxToken>) =
    walkTypeArg (makeDebugVisitor ctx input lexed) typeArg

let printMeasure (ctx: PrintContext) (input: string) (lexed: Lexed) (measure: Measure<SyntaxToken>) =
    walkMeasure (makeDebugVisitor ctx input lexed) measure

let printType (ctx: PrintContext) (input: string) (lexed: Lexed) (ty: Type<SyntaxToken>) =
    walkType (makeDebugVisitor ctx input lexed) ty

let printBinding (ctx: PrintContext) (input: string) (lexed: Lexed) (binding: Binding<SyntaxToken>) =
    walkBinding (makeDebugVisitor ctx input lexed) binding

let printFieldInitializer
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (fieldInit: FieldInitializer<SyntaxToken>)
    =
    walkFieldInitializer (makeDebugVisitor ctx input lexed) fieldInit

let printRules (ctx: PrintContext) (input: string) (lexed: Lexed) (rules: Rules<SyntaxToken>) =
    walkRules (makeDebugVisitor ctx input lexed) rules

let printExpr (ctx: PrintContext) (input: string) (lexed: Lexed) (expr: Expr<SyntaxToken>) =
    walkExpr (makeDebugVisitor ctx input lexed) expr

let printArgSpec (ctx: PrintContext) (input: string) (lexed: Lexed) (argSpec: ArgSpec<SyntaxToken>) =
    walkArgSpec (makeDebugVisitor ctx input lexed) argSpec

let printUncurriedSig (ctx: PrintContext) (input: string) (lexed: Lexed) (sign: UncurriedSig<SyntaxToken>) =
    walkUncurriedSig (makeDebugVisitor ctx input lexed) sign

let printCurriedSig (ctx: PrintContext) (input: string) (lexed: Lexed) (sign: CurriedSig<SyntaxToken>) =
    walkCurriedSig (makeDebugVisitor ctx input lexed) sign

let printMemberSig (ctx: PrintContext) (input: string) (lexed: Lexed) (sign: MemberSig<SyntaxToken>) =
    walkMemberSig (makeDebugVisitor ctx input lexed) sign

let printMethodOrPropDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (defn: MethodOrPropDefn<SyntaxToken>) =
    walkMethodOrPropDefn (makeDebugVisitor ctx input lexed) defn

let printMemberDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (memberDefn: MemberDefn<SyntaxToken>) =
    walkMemberDefn (makeDebugVisitor ctx input lexed) memberDefn

let printTypeDefnElement (ctx: PrintContext) (input: string) (lexed: Lexed) (elem: TypeDefnElement<SyntaxToken>) =
    walkTypeDefnElement (makeDebugVisitor ctx input lexed) elem

let printTypeName (ctx: PrintContext) (input: string) (lexed: Lexed) (typeName: TypeName<SyntaxToken>) =
    walkTypeName (makeDebugVisitor ctx input lexed) typeName

let printPrimaryConstrArgs (ctx: PrintContext) (input: string) (lexed: Lexed) (args: PrimaryConstrArgs<SyntaxToken>) =
    walkPrimaryConstrArgs (makeDebugVisitor ctx input lexed) args

let printUnionCaseData (ctx: PrintContext) (input: string) (lexed: Lexed) (data: UnionTypeCaseData<SyntaxToken>) =
    walkUnionCaseData (makeDebugVisitor ctx input lexed) data

let printExceptionDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (exnDefn: ExceptionDefn<SyntaxToken>) =
    walkExceptionDefn (makeDebugVisitor ctx input lexed) exnDefn

let printTypeDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (typeDefn: TypeDefn<SyntaxToken>) =
    walkTypeDefn (makeDebugVisitor ctx input lexed) typeDefn

let printModuleFunctionOrValueDefn
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (defn: ModuleFunctionOrValueDefn<SyntaxToken>)
    =
    walkModuleFunctionOrValueDefn (makeDebugVisitor ctx input lexed) defn

let printImportDecl (ctx: PrintContext) (input: string) (lexed: Lexed) (decl: ImportDecl<SyntaxToken>) =
    walkImportDecl (makeDebugVisitor ctx input lexed) decl

let printModuleAbbrev (ctx: PrintContext) (input: string) (lexed: Lexed) (abbrev: ModuleAbbrev<SyntaxToken>) =
    walkModuleAbbrev (makeDebugVisitor ctx input lexed) abbrev

let printCompilerDirective
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (decl: CompilerDirectiveDecl<SyntaxToken>)
    =
    walkCompilerDirective (makeDebugVisitor ctx input lexed) decl

let printModuleElem (ctx: PrintContext) (input: string) (lexed: Lexed) (elem: ModuleElem<SyntaxToken>) =
    walkModuleElem (makeDebugVisitor ctx input lexed) elem

let printModuleDefn (ctx: PrintContext) (input: string) (lexed: Lexed) (defn: ModuleDefn<SyntaxToken>) =
    walkModuleDefn (makeDebugVisitor ctx input lexed) defn

let printModuleElems (ctx: PrintContext) (input: string) (lexed: Lexed) (elems: ModuleElems<SyntaxToken>) =
    walkModuleElems (makeDebugVisitor ctx input lexed) elems

let printNamespaceDeclGroup
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (group: NamespaceDeclGroup<SyntaxToken>)
    =
    walkNamespaceDeclGroup (makeDebugVisitor ctx input lexed) group

let printImplementationFile (ctx: PrintContext) (input: string) (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) =
    walkImplementationFile (makeDebugVisitor ctx input lexed) file

let printFSharpAst (ctx: PrintContext) (input: string) (lexed: Lexed) (ast: FSharpAst<SyntaxToken>) =
    walkFSharpAst (makeDebugVisitor ctx input lexed) ast

/// Formats a DiagnosticCode as a short, stable string suitable for golden-file output.
let sprintDiagnosticCode (code: DiagnosticCode) : string =
    match code with
    | DiagnosticCode.MissingExpression -> "MissingExpression"
    | DiagnosticCode.MissingPattern -> "MissingPattern"
    | DiagnosticCode.MissingType -> "MissingType"
    | DiagnosticCode.MissingRule -> "MissingRule"
    | DiagnosticCode.MissingTypeDefn -> "MissingTypeDefn"
    | DiagnosticCode.MissingModuleElem -> "MissingModuleElem"
    | DiagnosticCode.UnexpectedTopLevel -> "UnexpectedTopLevel"
    | DiagnosticCode.Other msg -> $"Other({msg})"
    | DiagnosticCode.TyparInConstant _ -> "TyparInConstant"
    | DiagnosticCode.ExpectedEnd -> "ExpectedEnd"
    | DiagnosticCode.UnclosedDelimiter(opened, expected) ->
        let openedBase = TokenInfo.withoutFlags opened.Token
        let expectedBase = TokenInfo.withoutFlags expected
        $"UnclosedDelimiter({openedBase}, {expectedBase})"
    | DiagnosticCode.ExpectedRParen -> "ExpectedRParen"
    | DiagnosticCode.ExpectedRBracket -> "ExpectedRBracket"
    | DiagnosticCode.ExpectedRArrayBracket -> "ExpectedRArrayBracket"
    | DiagnosticCode.ExpectedRBraceBar -> "ExpectedRBraceBar"
    | DiagnosticCode.ExpectedQuotationTypedRight -> "ExpectedQuotationTypedRight"
    | DiagnosticCode.ExpectedQuotationUntypedRight -> "ExpectedQuotationUntypedRight"

/// Appends a "---\nDiagnostics:" section to the buffer when there are diagnostics.
/// Diagnostics are emitted in source order (reversed from the accumulation order).
/// Has no effect when `diagnostics` is empty, so well-formed golden files are not touched.
let printDiagnostics (ctx: PrintContext) (input: string) (diagnostics: Diagnostic list) =
    if not diagnostics.IsEmpty then
        let lineIndex = LineIndex.OfString input

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

                    let pos = diag.Token.StartIndex
                    let struct (ln, col) = lineIndex.GetLineCol(min pos (input.Length))
                    ctx.WriteLine($"[{severity}] {sprintDiagnosticCode diag.Code} at {pos} (Ln {ln}, Col {col})")

                    match diag.Error with
                    | Some err ->
                        indent
                            ctx
                            (fun () ->
                                let formatted = ErrorFormatting.splitAndFormatTokenErrors err

                                for line in formatted.Split('\n') do
                                    ctx.WriteLine(line.TrimEnd())
                            )
                    | None -> ()
            )

let printWarnDirectives (ctx: PrintContext) (warnDirectives: WarnDirective list) =
    if not warnDirectives.IsEmpty then
        ctx.WriteLine("---")
        ctx.WriteLine("WarnDirectives:")

        indent
            ctx
            (fun () ->
                // Print in source order (reverse of accumulation order)
                for d in List.rev warnDirectives do
                    let kind = if d.Suppress then "#nowarn" else "#warnon"
                    ctx.WriteLine($"{kind} {d.WarningNumber} (Ln {d.Line})")
            )
