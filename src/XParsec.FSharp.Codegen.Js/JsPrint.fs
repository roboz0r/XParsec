namespace XParsec.FSharp.Codegen.Js

open System.Text

module internal JsEscape =

    let quoted (s: string) : string =
        let sb = StringBuilder(s.Length + 2)
        sb.Append('"') |> ignore

        for c in s do
            match c with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | '\n' -> sb.Append "\\n" |> ignore
            | '\r' -> sb.Append "\\r" |> ignore
            | '\t' -> sb.Append "\\t" |> ignore
            | c when c < ' ' -> sb.AppendFormat("\\u{0:x4}", int c) |> ignore
            | c -> sb.Append c |> ignore

        sb.Append('"') |> ignore
        sb.ToString()

/// `JsProgram` → source text + V3 source map. The AST becomes a layout `Doc`, rendered in
/// one pass that tracks the generated line/column and emits a mapping at every `Mark`.
/// Layout is fixed: a statement block always breaks, an argument list never does.
module JsPrint =

    /// One generated→source correspondence. `SrcIndex` indexes the map's `sources[]`.
    type Mapping =
        {
            GenLine: int
            GenCol: int
            SrcIndex: int
            SrcLine: int
            SrcCol: int
        }

    /// The emitted ESM text plus the mappings collected while rendering it.
    type PrintResult =
        {
            Source: string
            Mappings: Mapping list
        }

    let private literal (l: JsLiteral) : string =
        match l with
        | JsLiteral.String s -> JsEscape.quoted s
        | JsLiteral.Number raw -> raw
        | JsLiteral.BigInt digits -> digits + "n"
        | JsLiteral.Boolean b -> if b then "true" else "false"

    // ---- The layout document -------------------------------------------------

    /// A break-only layout document: `Line` is an unconditional break (newline + current
    /// indent), so no layout decision depends on line width.
    type private Doc =
        | Nil
        | Text of string
        | Line
        | Nest of int * Doc
        | Cat of Doc list
        | Mark of JsLoc * Doc

    let private (++) (a: Doc) (b: Doc) = Cat [ a; b ]
    let private cat (docs: Doc list) = Cat docs
    let private text (s: string) = Text s

    let private indent (d: Doc) = Nest(2, d)

    let private marked (loc: JsLoc voption) (d: Doc) =
        match loc with
        | ValueSome l -> Mark(l, d)
        | ValueNone -> d

    /// `d0 sep d1 sep …` — interpose `sep` between docs (no trailing separator).
    let private punctuate (sep: Doc) (docs: Doc list) : Doc =
        match docs with
        | [] -> Nil
        | first :: rest -> cat (first :: [ for d in rest -> sep ++ d ])

    let private commaList (docs: Doc list) = punctuate (text ", ") docs

    // ---- AST → Doc -----------------------------------------------------------

    let rec private expr (e: JsExpr) : Doc =
        match e with
        | JsExpr.Identifier(name, loc) -> marked loc (text name)
        | JsExpr.Literal(l, loc) -> marked loc (text (literal l))
        | JsExpr.Member(object, property, computed, loc) ->
            marked
                loc
                (expr object
                 ++ (if computed then
                         text "[" ++ expr property ++ text "]"
                     else
                         text "." ++ expr property))
        | JsExpr.Call(callee, args, loc) ->
            // `((x) => …)(v)`, not `(x) => …(v)`: an unparenthesised arrow callee would
            // swallow the argument list into its body.
            let calleeDoc =
                match callee with
                | JsExpr.Identifier _
                | JsExpr.Member _
                | JsExpr.Call _ -> expr callee
                | _ -> text "(" ++ expr callee ++ text ")"

            marked loc (calleeDoc ++ text "(" ++ commaList (List.map expr args) ++ text ")")
        | JsExpr.New(callee, args, loc) ->
            marked
                loc
                (text "new "
                 ++ expr callee
                 ++ text "("
                 ++ commaList (List.map expr args)
                 ++ text ")")
        | JsExpr.Conditional(test, consequent, alternate, loc) ->
            marked
                loc
                (text "("
                 ++ expr test
                 ++ text " ? "
                 ++ expr consequent
                 ++ text " : "
                 ++ expr alternate
                 ++ text ")")
        | JsExpr.Sequence(exprs, loc) -> marked loc (text "(" ++ commaList (List.map expr exprs) ++ text ")")
        | JsExpr.Array(elements, loc) -> marked loc (text "[" ++ commaList (List.map expr elements) ++ text "]")
        | JsExpr.Raw(segments, loc) ->
            // Both the template and every hole are wrapped: `$0 + $1` prints `((a) + (b))`.
            let seg s =
                match s with
                | JsRawSeg.Verbatim v -> text v
                | JsRawSeg.Hole e -> text "(" ++ expr e ++ text ")"

            marked loc (text "(" ++ cat (List.map seg segments) ++ text ")")
        | JsExpr.Arrow(parameters, body, loc) ->
            let bodyDoc =
                match body with
                | JsFnBody.Expr e -> expr e
                | JsFnBody.Block stmts -> block stmts

            marked loc (text "(" ++ commaList (List.map text parameters) ++ text ") => " ++ bodyDoc)
        // `(target = value)` — parenthesised, so it is safe as a comma-sequence operand.
        | JsExpr.Assign(target, value, loc) ->
            marked loc (text "(" ++ expr target ++ text " = " ++ expr value ++ text ")")
        | JsExpr.Binary(op, left, right, loc)
        | JsExpr.Logical(op, left, right, loc) ->
            marked
                loc
                (text "("
                 ++ expr left
                 ++ text " "
                 ++ text op
                 ++ text " "
                 ++ expr right
                 ++ text ")")

    and private block (stmts: JsStatement list) : Doc =
        text "{"
        ++ indent (cat [ for s in stmts -> Line ++ statement s ])
        ++ Line
        ++ text "}"

    /// A class member `<header> { <body statements> }`.
    and private memberDecl (header: Doc) (body: Doc list) : Doc =
        header
        ++ text " {"
        ++ indent (cat [ for s in body -> Line ++ s ])
        ++ Line
        ++ text "}"

    /// `constructor(<params>) { … }`; `prologue` prints first, namely a union subclass's `super(3);`.
    and private ctorDecl (ctor: JsCtor) (prologue: Doc list) : Doc =
        memberDecl
            (text "constructor(" ++ commaList (List.map text ctor.Params) ++ text ")")
            (prologue @ [ for s in ctor.Body -> statement s ])

    /// `[export ]class Name [extends Base] { member… }`.
    and private classDecl (export: bool) (name: string) (extends: string option) (members: Doc list) : Doc =
        let ext =
            match extends with
            | Some b -> text " extends " ++ text b
            | None -> Nil

        (if export then text "export class " else text "class ")
        ++ text name
        ++ ext
        ++ text " {"
        ++ indent (cat [ for m in members -> Line ++ m ])
        ++ Line
        ++ text "}"

    /// `*[Symbol.iterator](p0) { … }` — `Generator` prints the `*`, a `Computed` key the `[…]`.
    and private methodDecl (m: JsClassMethod) : Doc =
        let star = if m.Generator then text "*" else Nil

        let key =
            match m.Key with
            | JsMethodKey.Computed keyExpr -> text "[" ++ expr keyExpr ++ text "]"
            | JsMethodKey.Named n -> text n

        memberDecl
            (star ++ key ++ text "(" ++ commaList (List.map text m.Params) ++ text ")")
            [ for s in m.Body -> statement s ]

    and private statement (s: JsStatement) : Doc =
        match s with
        | JsStatement.Expression e -> expr e ++ text ";"
        | JsStatement.Const(name, init) -> text "const " ++ text name ++ text " = " ++ expr init ++ text ";"
        | JsStatement.Let(name, init) -> text "let " ++ text name ++ text " = " ++ expr init ++ text ";"
        | JsStatement.Export(name, init, reassignable) ->
            text (if reassignable then "export let " else "export const ")
            ++ text name
            ++ text " = "
            ++ expr init
            ++ text ";"
        | JsStatement.Import(defaultBinding, named, source) ->
            // `import D from "m";` / `import { a as $a } from "m";` / both, comma-separated.
            // Every named binding carries a `$`-prefixed alias, so `as` renders unconditionally.
            let namedClause =
                if List.isEmpty named then
                    None
                else
                    Some(
                        sprintf
                            "{ %s }"
                            (named
                             |> List.map (fun (name, alias) -> name + " as " + alias)
                             |> String.concat ", ")
                    )

            let clause = [ defaultBinding; namedClause ] |> List.choose id |> String.concat ", "
            text (sprintf "import %s from %s;" clause (JsEscape.quoted source))
        | JsStatement.ImportNamespace(binding, source) ->
            text (sprintf "import * as %s from %s;" binding (JsEscape.quoted source))
        | JsStatement.If(test, consequent, alternate) ->
            let elseDoc =
                if List.isEmpty alternate then
                    Nil
                else
                    text " else " ++ block alternate

            text "if (" ++ expr test ++ text ") " ++ block consequent ++ elseDoc
        | JsStatement.While(test, body) -> text "while (" ++ expr test ++ text ") " ++ block body
        | JsStatement.For(var, init, limit, body) ->
            text "for (let "
            ++ text var
            ++ text " = "
            ++ expr init
            ++ text "; "
            ++ text var
            ++ text " <= "
            ++ expr limit
            ++ text "; "
            ++ text var
            ++ text "++) "
            ++ block body
        | JsStatement.ForOf(boundVar, source, body) ->
            text "for (const "
            ++ text boundVar
            ++ text " of "
            ++ expr source
            ++ text ") "
            ++ block body
        | JsStatement.Return e -> text "return " ++ expr e ++ text ";"
        | JsStatement.Continue -> text "continue;"
        | JsStatement.Assign(target, value) -> text target ++ text " = " ++ expr value ++ text ";"
        | JsStatement.FieldStore(field, value) -> text "this." ++ text field ++ text " = " ++ expr value ++ text ";"
        | JsStatement.Block body -> block body
        | JsStatement.Throw e -> text "throw " ++ expr e ++ text ";"
        | JsStatement.Yield e -> text "yield " ++ expr e ++ text ";"
        | JsStatement.TryFinally(tryBody, finallyBody) ->
            text "try " ++ block tryBody ++ text " finally " ++ block finallyBody
        | JsStatement.Class(name, ctor, methods, export) ->
            classDecl export name None (ctorDecl ctor [] :: [ for m in methods -> methodDecl m ])
        | JsStatement.Union(baseName, brand, cases, baseMethods, export) ->
            let baseClass =
                classDecl
                    export
                    baseName
                    None
                    [
                        yield ctorDecl (JsCtor.positional [ "tag" ] []) []
                        // `get $type() { return "Mod.U"; }` — a prototype getter, so absent from
                        // own-keys; the structural runtime's equality and comparison read it.
                        yield memberDecl (text "get $type()") [ text (sprintf "return %s;" (JsEscape.quoted brand)) ]
                        yield
                            memberDecl
                                (text "cases()")
                                [
                                    text "return ["
                                    ++ commaList [ for c in cases -> text (JsEscape.quoted c.CaseName) ]
                                    ++ text "];"
                                ]
                        // `[Symbol.iterator]`, equality and hash attach to the BASE class, so
                        // every case subclass inherits them.
                        for m in baseMethods -> methodDecl m
                    ]

            let subclass (c: JsUnionCaseDecl) =
                classDecl
                    export
                    c.ClassName
                    (Some baseName)
                    [
                        ctorDecl (JsCtor.positional c.Fields []) [ text (sprintf "super(%d);" c.Tag) ]
                    ]

            cat (baseClass :: [ for c in cases -> Line ++ subclass c ])
        | JsStatement.Enum(name, cases, export) ->
            // Case names print verbatim as object keys: `Object.freeze({ Red: 0, … })`.
            let entries =
                [
                    for (caseName, value) in cases -> text caseName ++ text ": " ++ text (literal value)
                ]

            (if export then text "export const " else text "const ")
            ++ text name
            ++ text " = Object.freeze({ "
            ++ commaList entries
            ++ text " });"

    // ---- Rendering -----------------------------------------------------------

    let private render (doc: Doc) : PrintResult =
        let sb = StringBuilder()
        let maps = ResizeArray<Mapping>()
        let mutable line = 0
        let mutable col = 0

        let rec go (ind: int) (d: Doc) =
            match d with
            | Nil -> ()
            | Text s ->
                sb.Append s |> ignore
                col <- col + s.Length
            | Line ->
                sb.Append '\n' |> ignore
                line <- line + 1
                sb.Append(' ', ind) |> ignore
                col <- ind
            | Nest(n, inner) -> go (ind + n) inner
            | Cat docs ->
                for d in docs do
                    go ind d
            | Mark(loc, inner) ->
                maps.Add
                    {
                        GenLine = line
                        GenCol = col
                        SrcIndex = loc.Source
                        SrcLine = loc.Line
                        SrcCol = loc.Column
                    }

                go ind inner

        go 0 doc

        {
            Source = sb.ToString()
            Mappings = List.ofSeq maps
        }

    /// The emitted ESM source (trailing-newline terminated) plus its mappings.
    let print (program: JsProgram) : PrintResult =
        render (cat [ for s in program.Body -> statement s ++ Line ])


/// V3 source-map JSON: base64-VLQ `mappings` + embedded `sourcesContent`. A segment is
/// `[genColΔ, srcIndexΔ, srcLineΔ, srcColΔ]`; the optional 5th name index is never emitted.
module JsSourceMap =

    [<Literal>]
    let private b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    /// Base64 VLQ: the value's sign rides the least-significant bit, then 5-bit
    /// groups little-endian with bit 6 (0x20) marking continuation.
    let private encodeVlq (value: int) : string =
        let sb = StringBuilder()
        let mutable vlq = if value < 0 then ((-value) <<< 1) ||| 1 else value <<< 1
        let mutable more = true

        while more do
            let mutable digit = vlq &&& 0x1F
            vlq <- vlq >>> 5

            if vlq > 0 then
                digit <- digit ||| 0x20

            sb.Append b64.[digit] |> ignore
            more <- vlq > 0

        sb.ToString()

    /// The `mappings` field: `;` ends a generated line, `,` separates segments within one.
    /// `maps` must already be in ascending generated order.
    let private encodeMappings (maps: JsPrint.Mapping list) : string =
        let sb = StringBuilder()
        // Generated column resets each line; source index, line and column are cumulative
        // deltas that run across line boundaries.
        let mutable curLine = 0
        let mutable prevGenCol = 0
        let mutable prevSrcIndex = 0
        let mutable prevSrcLine = 0
        let mutable prevSrcCol = 0
        let mutable firstOnLine = true

        for m in maps do
            while curLine < m.GenLine do
                sb.Append ';' |> ignore
                curLine <- curLine + 1
                prevGenCol <- 0
                firstOnLine <- true

            if not firstOnLine then
                sb.Append ',' |> ignore

            sb.Append(encodeVlq (m.GenCol - prevGenCol)) |> ignore
            sb.Append(encodeVlq (m.SrcIndex - prevSrcIndex)) |> ignore
            sb.Append(encodeVlq (m.SrcLine - prevSrcLine)) |> ignore
            sb.Append(encodeVlq (m.SrcCol - prevSrcCol)) |> ignore

            prevGenCol <- m.GenCol
            prevSrcIndex <- m.SrcIndex
            prevSrcLine <- m.SrcLine
            prevSrcCol <- m.SrcCol
            firstOnLine <- false

        sb.ToString()

    /// The V3 JSON mapping generated `file` back to `sources`, whose ORDER is the index space
    /// every mapping's `SrcIndex` names.
    let build (file: string) (sources: JsMapSource list) (maps: JsPrint.Mapping list) : string =
        let jsonArray (quoted: JsMapSource -> string) =
            sources |> List.map quoted |> String.concat ","

        let sb = StringBuilder()
        sb.Append "{\"version\":3" |> ignore
        sb.AppendFormat(",\"file\":{0}", JsEscape.quoted file) |> ignore
        sb.Append ",\"sourceRoot\":\"\"" |> ignore

        sb.AppendFormat(",\"sources\":[{0}]", jsonArray (fun s -> JsEscape.quoted s.Path))
        |> ignore

        sb.AppendFormat(",\"sourcesContent\":[{0}]", jsonArray (fun s -> JsEscape.quoted s.Content))
        |> ignore

        sb.Append ",\"names\":[]" |> ignore

        sb.AppendFormat(",\"mappings\":{0}", JsEscape.quoted (encodeMappings maps))
        |> ignore

        sb.Append "}" |> ignore
        sb.ToString()
