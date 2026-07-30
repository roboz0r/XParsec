namespace XParsec.FSharp.Codegen.Js

open System.Text

/// Double-quoted string-literal escaping shared by the JS printer (string
/// literals in emitted code) and the source-map JSON writer. The escaping is a
/// strict superset safe for both contexts: the five named escapes plus a
/// `\uXXXX` fallback for every other control char (< 0x20), which neither a JS
/// string literal nor JSON may carry raw.
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

/// `JsProgram → source text + V3 source map`. The printer builds an intermediate
/// layout `Doc` from the AST and renders it in one pass: rendering tracks the
/// generated line/column and emits a mapping for every `Mark` node.
///
/// `Doc` is the structural subset of a Wadler/Leijen pretty-printer —
/// `Text`/`Cat`/`Line`/`Nest` + a source-position `Mark`, with no width-driven
/// `group`/best-fit (blocks always break, argument lists always inline). Indentation
/// is carried by `Nest`; source mappings fall out of `Mark` during the render pass.
module JsPrint =

    /// One generated→source correspondence. `Src*` are 0-based source coordinates, read
    /// against the map's `sources[SrcIndex]` — never against "the" source, since an inlined
    /// body's line belongs to the file that body was written in.
    type Mapping =
        {
            GenLine: int
            GenCol: int
            SrcIndex: int
            SrcLine: int
            SrcCol: int
        }

    /// The printer's result: the emitted ESM text plus the mappings collected
    /// while rendering it (in generated-order: ascending line then column).
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

    /// A break-only layout document. `Line` is a hard break (newline + current
    /// indentation); `Nest` widens indentation; `Mark` records a source mapping.
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

    /// One indentation level = two spaces.
    let private indent (d: Doc) = Nest(2, d)

    /// Wrap a doc in a source `Mark` when the node carries a `loc`.
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
            // An arrow callee must be parenthesised so `(args)` doesn't extend the
            // arrow body — `((x) => …)(v)`, not `(x) => …(v)`.
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
            // Universal parenthesization: wrap the whole template and each operand hole
            // in `(…)` — precedence-correct by construction, zero JS-grammar knowledge.
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
        // `(target = value)` — whole node parenthesised, like `Binary`, so it is
        // safe in a comma sequence / expression-statement position.
        | JsExpr.Assign(target, value, loc) ->
            marked loc (text "(" ++ expr target ++ text " = " ++ expr value ++ text ")")
        // Both `Binary` and `Logical` print `(left <op> right)` — whole node parenthesised.
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

    /// A brace-delimited statement block: `{` then one indented statement per
    /// line, then the closing `}` at the enclosing indentation.
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

    /// The one constructor shape shared by records, union bases, union subclasses, and
    /// classes: `constructor(params) { <prologue> this.f = f; … <epilogue> }` — store each
    /// declaration-order field into the like-named property. Records and union bases pass
    /// no prologue; a union subclass passes `super(tag);`. `epilogue` is the class
    /// instance preamble, which runs AFTER the param stores because its initialisers read
    /// the params through `this`.
    and private ctorDecl
        (paramNames: string list)
        (prologue: Doc list)
        (assigns: string list)
        (epilogue: Doc list)
        : Doc =
        let assignStmts =
            [
                for f in assigns -> text "this." ++ text f ++ text " = " ++ text f ++ text ";"
            ]

        memberDecl
            (text "constructor(" ++ commaList (List.map text paramNames) ++ text ")")
            (prologue @ assignStmts @ epilogue)

    /// `[export ]class Name [extends Base] { member… }`. `export` is set in library
    /// mode so a consumer can import the type rather than re-emit it.
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

    /// One instance method of an emitted class or union base class. `Generator`
    /// prefixes `*` (so the body may `yield`); `Computed` renders a `[expr]` key
    /// (`[Symbol.iterator]`, `[Symbol.for("vesper.X")]`) instead of the string name.
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
            // `import D from`, `import { a, b } from`, or `import D, { a, b } from` — a TS
            // default export binds positionally (no braces), named bindings ride the braces.
            // Every named binding is aliased (`$`-prefixed aliases are collision-free with
            // export names), so the `name as alias` spelling renders unconditionally.
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
        | JsStatement.ForOf(binder, source, body) ->
            text "for (const "
            ++ text binder
            ++ text " of "
            ++ expr source
            ++ text ") "
            ++ block body
        | JsStatement.Return e -> text "return " ++ expr e ++ text ";"
        | JsStatement.Continue -> text "continue;"
        | JsStatement.Assign(target, value) -> text target ++ text " = " ++ expr value ++ text ";"
        | JsStatement.Block body -> block body
        | JsStatement.Throw e -> text "throw " ++ expr e ++ text ";"
        | JsStatement.Yield e -> text "yield " ++ expr e ++ text ";"
        | JsStatement.TryFinally(tryBody, finallyBody) ->
            text "try " ++ block tryBody ++ text " finally " ++ block finallyBody
        | JsStatement.Class(name, fields, ctorBody, methods, export) ->
            classDecl
                export
                name
                None
                (ctorDecl fields [] fields [ for s in ctorBody -> statement s ]
                 :: [ for m in methods -> methodDecl m ])
        | JsStatement.Union(baseName, brand, cases, baseMethods, export) ->
            let baseClass =
                classDecl
                    export
                    baseName
                    None
                    [
                        yield ctorDecl [ "tag" ] [] [ "tag" ] []
                        // Non-enumerable type brand (prototype getter, so absent from
                        // own-keys): the structural runtime distinguishes types by it.
                        yield memberDecl (text "get $type()") [ text (sprintf "return %s;" (JsEscape.quoted brand)) ]
                        yield
                            memberDecl
                                (text "cases()")
                                [
                                    text "return ["
                                    ++ commaList [ for c in cases -> text (JsEscape.quoted c.CaseName) ]
                                    ++ text "];"
                                ]
                        // Capability protocol members (`[Symbol.iterator]`, eq/comp/hash):
                        // attached to the BASE class so every case subclass inherits them.
                        for m in baseMethods -> methodDecl m
                    ]

            let subclass (c: JsUnionCaseDecl) =
                classDecl
                    export
                    c.ClassName
                    (Some baseName)
                    [ ctorDecl c.Fields [ text (sprintf "super(%d);" c.Tag) ] c.Fields [] ]

            cat (baseClass :: [ for c in cases -> Line ++ subclass c ])
        | JsStatement.Enum(name, cases, export) ->
            // The frozen object map `const Name = Object.freeze({ C1: v1, … });`.
            // Case names print verbatim as object keys (F# identifiers, the same
            // verbatim treatment record/union field names get); literal values reuse
            // the shared `literal` formatter.
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

    /// Render a `Doc` to text + V3 mappings in one pass. `Nest` carries indentation
    /// to each `Line`, so there is no mutable indent to balance.
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


/// V3 source-map document: base64-VLQ `mappings` + embedded `sourcesContent`.
/// Hand-rolled JSON. Each segment is `[genColΔ, srcIndexΔ, srcLineΔ, srcColΔ]`;
/// the optional name index is never emitted.
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

    /// Encode the collected mappings into the V3 `mappings` field.
    /// `maps` is already in ascending generated order.
    let private encodeMappings (maps: JsPrint.Mapping list) : string =
        let sb = StringBuilder()
        // Generated column resets each line; the source index, line and column are cumulative
        // deltas that run across line boundaries. A map with one source therefore encodes the
        // same bytes it did before there could be more than one: every source-index delta is 0.
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

    /// Build the V3 JSON document mapping generated `file` back to `sources`, whose ORDER is
    /// the index space every mapping's `SrcIndex` names — so the caller that assigned those
    /// indices is the caller that must pass the list in that order.
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
