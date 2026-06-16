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
/// layout document (`Doc`) from the AST, then renders it once: rendering tracks
/// the generated line/column and emits a mapping for every `Mark` node (the
/// in-data successor to Step 0b's imperative cursor pokes). `JsSourceMap.build`
/// then turns the collected mappings into a V3 JSON document (VLQ-encoded
/// `mappings`, embedded `sourcesContent`). One statement per line, each
/// `;`-terminated.
///
/// `Doc` is the structural subset of a Wadler/Leijen pretty-printer —
/// `Text`/`Cat`/`Line`/`Nest` with a source-position `Mark`, but *no* width-driven
/// `group`/best-fit: generated code lays out deterministically (blocks always
/// break, argument lists always inline), so the only layout decisions are
/// structural. The win over an imperative cursor is that indentation is carried by
/// `Nest` (no hand-balanced `Indent +/- 1` bookkeeping) and source mappings fall
/// out of the `Mark` nodes during a single render pass.
module JsPrint =

    /// One generated→source correspondence. `Src*` are 0-based source coordinates;
    /// the source index is implicitly 0 (the single input file).
    type Mapping =
        {
            GenLine: int
            GenCol: int
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

    /// A break-only layout document. `Text` carries a run with no embedded newline
    /// (string literals are escaped, so none arises); `Line` is a hard break
    /// (newline + the current indentation); `Nest` widens the indentation of the
    /// `Line`s inside it; `Cat` is sequencing; `Mark` records a source mapping at
    /// the generated position the document reaches during rendering.
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

    /// Wrap a doc in a source `Mark` when its node carries a `loc` (the mapping is
    /// recorded at the position the doc starts, matching the prior printer's
    /// "mark immediately before the node's first character").
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
            // A callee that is not a plain reference / call needs parenthesising
            // so the `(args)` binds to it and not to a sub-expression — notably an
            // arrow (`((x) => …)(v)`, the IIFE), whose body would otherwise extend
            // rightward and swallow the argument list.
            let calleeDoc =
                match callee with
                | JsExpr.Identifier _
                | JsExpr.Member _
                | JsExpr.Call _ -> expr callee
                | _ -> text "(" ++ expr callee ++ text ")"

            marked loc (calleeDoc ++ text "(" ++ commaList (List.map expr args) ++ text ")")
        | JsExpr.New(callee, args, loc) ->
            // The callee is a class-name `Identifier` (record construction), so it
            // self-delimits — no parenthesising needed as `Call` requires.
            marked
                loc
                (text "new "
                 ++ expr callee
                 ++ text "("
                 ++ commaList (List.map expr args)
                 ++ text ")")
        | JsExpr.Conditional(test, consequent, alternate, loc) ->
            // Parenthesised whole so the ternary composes safely wherever it lands
            // (it is the lowest-precedence JS operator).
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
            // (a*) universal parenthesization: wrap the whole template, and each
            // substituted operand, in `(…)` — precedence-correct by construction
            // with zero JS-grammar knowledge (codegen-js-steps §"Template →
            // ESTree").
            let seg s =
                match s with
                | JsRawSeg.Verbatim v -> text v
                | JsRawSeg.Hole e -> text "(" ++ expr e ++ text ")"

            marked loc (text "(" ++ cat (List.map seg segments) ++ text ")")
        | JsExpr.Arrow(parameters, body, loc) ->
            // A concise body composes safely as-is (arrow / call / `new` / ternary
            // / `Raw` all self-delimit). A bare object-*literal* body (`() => ({…})`)
            // would need wrapping, but records construct via `new R(…)`, which
            // self-delimits, so none is needed.
            let bodyDoc =
                match body with
                | JsFnBody.Expr e -> expr e
                | JsFnBody.Block stmts -> block stmts

            marked loc (text "(" ++ commaList (List.map text parameters) ++ text ") => " ++ bodyDoc)
        // `Binary` / `Logical` both print `(left <op> right)` — the whole node
        // parenthesised (the (a*) universal-parenthesization discipline), so no
        // precedence table is needed and they compose safely wherever they land.
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

    /// The one constructor shape shared by records, union bases, and union
    /// subclasses: `constructor(params) { <prologue> this.f = f; … }` — store each
    /// declaration-order field into the like-named property. Records and union
    /// bases pass no prologue; a union subclass passes `super(tag);`.
    and private ctorDecl (paramNames: string list) (prologue: Doc list) (assigns: string list) : Doc =
        let assignStmts =
            [
                for f in assigns -> text "this." ++ text f ++ text " = " ++ text f ++ text ";"
            ]

        memberDecl (text "constructor(" ++ commaList (List.map text paramNames) ++ text ")") (prologue @ assignStmts)

    /// `class Name [extends Base] { member… }`.
    and private classDecl (name: string) (extends: string option) (members: Doc list) : Doc =
        let ext =
            match extends with
            | Some b -> text " extends " ++ text b
            | None -> Nil

        text "class "
        ++ text name
        ++ ext
        ++ text " {"
        ++ indent (cat [ for m in members -> Line ++ m ])
        ++ Line
        ++ text "}"

    and private statement (s: JsStatement) : Doc =
        match s with
        | JsStatement.Expression e -> expr e ++ text ";"
        | JsStatement.Const(name, init) -> text "const " ++ text name ++ text " = " ++ expr init ++ text ";"
        | JsStatement.Export(name, init) -> text "export const " ++ text name ++ text " = " ++ expr init ++ text ";"
        | JsStatement.Import(specifiers, source) ->
            text (sprintf "import { %s } from %s;" (String.concat ", " specifiers) (JsEscape.quoted source))
        | JsStatement.If(test, consequent, alternate) ->
            let elseDoc =
                if List.isEmpty alternate then
                    Nil
                else
                    text " else " ++ block alternate

            text "if (" ++ expr test ++ text ") " ++ block consequent ++ elseDoc
        | JsStatement.While(test, body) -> text "while (" ++ expr test ++ text ") " ++ block body
        | JsStatement.Return e -> text "return " ++ expr e ++ text ";"
        | JsStatement.Continue -> text "continue;"
        | JsStatement.Assign(target, value) -> text target ++ text " = " ++ expr value ++ text ";"
        | JsStatement.Block body -> block body
        | JsStatement.Throw e -> text "throw " ++ expr e ++ text ";"
        | JsStatement.Class(name, fields) ->
            // The canonical record class: one positional constructor storing each
            // declaration-order field into the like-named property, so `new R(a, b)`
            // and `r.X` line up. No structural methods yet (Step 6).
            classDecl name None [ ctorDecl fields [] fields ]
        | JsStatement.Union(baseName, cases) ->
            // The base class — a `tag`-storing constructor plus `cases()` returning
            // the declaration-order case names (the hand-stub Step 6 grows the
            // structural triple onto) — then one `extends`-subclass per case storing
            // its named fields after `super(tag)`.
            let baseClass =
                classDecl
                    baseName
                    None
                    [
                        ctorDecl [ "tag" ] [] [ "tag" ]
                        memberDecl
                            (text "cases()")
                            [
                                text "return ["
                                ++ commaList [ for c in cases -> text (JsEscape.quoted c.CaseName) ]
                                ++ text "];"
                            ]
                    ]

            let subclass (c: JsUnionCaseDecl) =
                classDecl
                    c.ClassName
                    (Some baseName)
                    [ ctorDecl c.Fields [ text (sprintf "super(%d);" c.Tag) ] c.Fields ]

            cat (baseClass :: [ for c in cases -> Line ++ subclass c ])

    // ---- Rendering -----------------------------------------------------------

    /// Render a `Doc` to text + V3 mappings in one pass, tracking the generated
    /// cursor so each `Mark` records the (line, col) it lands at. `Nest` carries
    /// the indentation a `Line` re-emits, so there is no mutable indent to balance.
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


/// The V3 source-map document: base64-VLQ `mappings` + embedded
/// `sourcesContent`. Hand-rolled JSON (no serializer dependency); the mappings
/// grammar is `;`-per-generated-line, `,`-per-segment, each segment a VLQ tuple
/// `[genColΔ, srcIndexΔ, srcLineΔ, srcColΔ]` (the optional name index is never
/// emitted — Step 0b carries no `names`).
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

    /// Encode the collected mappings into the V3 `mappings` field. `maps` arrives
    /// in generated order (`JsPrint.print` records them as it advances the cursor,
    /// so they are already ascending by line then column).
    let private encodeMappings (maps: JsPrint.Mapping list) : string =
        let sb = StringBuilder()
        // Generated column resets each line; the source line/column deltas are
        // cumulative across the whole file. There is a single source, so the
        // source-index field is always 0 (its delta never changes).
        let mutable curLine = 0
        let mutable prevGenCol = 0
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
            sb.Append(encodeVlq 0) |> ignore
            sb.Append(encodeVlq (m.SrcLine - prevSrcLine)) |> ignore
            sb.Append(encodeVlq (m.SrcCol - prevSrcCol)) |> ignore

            prevGenCol <- m.GenCol
            prevSrcLine <- m.SrcLine
            prevSrcCol <- m.SrcCol
            firstOnLine <- false

        sb.ToString()

    /// Build the V3 JSON document mapping generated `file` back to a single
    /// source (`sourcePath`, content `sourceContent`).
    let build (file: string) (sourcePath: string) (sourceContent: string) (maps: JsPrint.Mapping list) : string =
        let sb = StringBuilder()
        sb.Append "{\"version\":3" |> ignore
        sb.AppendFormat(",\"file\":{0}", JsEscape.quoted file) |> ignore
        sb.Append ",\"sourceRoot\":\"\"" |> ignore
        sb.AppendFormat(",\"sources\":[{0}]", JsEscape.quoted sourcePath) |> ignore

        sb.AppendFormat(",\"sourcesContent\":[{0}]", JsEscape.quoted sourceContent)
        |> ignore

        sb.Append ",\"names\":[]" |> ignore

        sb.AppendFormat(",\"mappings\":{0}", JsEscape.quoted (encodeMappings maps))
        |> ignore

        sb.Append "}" |> ignore
        sb.ToString()
