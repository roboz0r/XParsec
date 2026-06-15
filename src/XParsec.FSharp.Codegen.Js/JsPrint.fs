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

/// `JsProgram → source text + V3 source map`. The printer tracks the generated
/// line/column as it writes, and records a mapping for every `JsExpr` that
/// carries a `loc` (Step 0b). `JsSourceMap.build` then turns the collected
/// mappings into a V3 JSON document (VLQ-encoded `mappings`, embedded
/// `sourcesContent`). One statement per line, each `;`-terminated.
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
    /// while emitting it (in generated-order: ascending line then column).
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

    /// Mutable emit cursor: the output buffer plus the running generated
    /// position and the accumulating (reverse-order) mappings.
    type private Printer =
        {
            Sb: StringBuilder
            mutable Line: int
            mutable Col: int
            mutable Indent: int
            mutable Maps: Mapping list
        }

    /// Append text with no embedded newline, advancing the column.
    let private write (p: Printer) (s: string) =
        p.Sb.Append s |> ignore
        p.Col <- p.Col + s.Length

    let private newline (p: Printer) =
        p.Sb.Append '\n' |> ignore
        p.Line <- p.Line + 1
        p.Col <- 0

    /// Newline then the current indentation (two spaces per level) — used inside
    /// brace-delimited blocks so generated coordinates (and therefore source-map
    /// columns) account for the leading whitespace.
    let private newlineIndent (p: Printer) =
        p.Sb.Append '\n' |> ignore
        p.Line <- p.Line + 1
        let pad = p.Indent * 2
        p.Sb.Append(' ', pad) |> ignore
        p.Col <- pad

    /// Record a mapping from the current generated position to `loc`'s source
    /// position, when the node carries one. Call immediately before writing the
    /// node's first character.
    let private mark (p: Printer) (loc: JsLoc voption) =
        match loc with
        | ValueSome l ->
            p.Maps <-
                {
                    GenLine = p.Line
                    GenCol = p.Col
                    SrcLine = l.Line
                    SrcCol = l.Column
                }
                :: p.Maps
        | ValueNone -> ()

    let rec private expr (p: Printer) (e: JsExpr) =
        match e with
        | JsExpr.Identifier(name, loc) ->
            mark p loc
            write p name
        | JsExpr.Literal(l, loc) ->
            mark p loc
            write p (literal l)
        | JsExpr.Member(object, property, computed, loc) ->
            mark p loc
            expr p object

            if computed then
                write p "["
                expr p property
                write p "]"
            else
                write p "."
                expr p property
        | JsExpr.Call(callee, args, loc) ->
            mark p loc
            // A callee that is not a plain reference / call needs parenthesising
            // so the `(args)` binds to it and not to a sub-expression — notably an
            // arrow (`((x) => …)(v)`, the IIFE), whose body would otherwise extend
            // rightward and swallow the argument list.
            match callee with
            | JsExpr.Identifier _
            | JsExpr.Member _
            | JsExpr.Call _ -> expr p callee
            | _ ->
                write p "("
                expr p callee
                write p ")"

            write p "("

            args
            |> List.iteri (fun i a ->
                if i > 0 then
                    write p ", "

                expr p a
            )

            write p ")"
        | JsExpr.Conditional(test, consequent, alternate, loc) ->
            // Parenthesised whole so the ternary composes safely wherever it
            // lands (it is the lowest-precedence JS operator).
            mark p loc
            write p "("
            expr p test
            write p " ? "
            expr p consequent
            write p " : "
            expr p alternate
            write p ")"
        | JsExpr.Sequence(exprs, loc) ->
            mark p loc
            write p "("

            exprs
            |> List.iteri (fun i e ->
                if i > 0 then
                    write p ", "

                expr p e
            )

            write p ")"
        | JsExpr.Raw(segments, loc) ->
            // (a*) universal parenthesization: wrap the whole template, and each
            // substituted operand, in `(…)` — precedence-correct by construction
            // with zero JS-grammar knowledge (codegen-js-steps §"Template →
            // ESTree").
            mark p loc
            write p "("

            for seg in segments do
                match seg with
                | JsRawSeg.Verbatim s -> write p s
                | JsRawSeg.Hole e ->
                    write p "("
                    expr p e
                    write p ")"

            write p ")"
        | JsExpr.Arrow(parameters, body, loc) ->
            mark p loc
            write p "("

            parameters
            |> List.iteri (fun i n ->
                if i > 0 then
                    write p ", "

                write p n
            )

            write p ") => "

            match body with
            // A concise body composes safely as-is in Step 2 (arrow / call /
            // ternary / `Raw` all self-delimit). An object-literal body (records,
            // Step 3) will need its own parenthesisation when it lands.
            | JsFnBody.Expr e -> expr p e
            | JsFnBody.Block stmts -> block p stmts

    /// A brace-delimited statement block: `{` then one indented statement per
    /// line, then the closing `}` at the enclosing indentation.
    and private block (p: Printer) (stmts: JsStatement list) =
        write p "{"
        p.Indent <- p.Indent + 1

        for s in stmts do
            newlineIndent p
            statement p s

        p.Indent <- p.Indent - 1
        newlineIndent p
        write p "}"

    and private statement (p: Printer) (s: JsStatement) =
        match s with
        | JsStatement.Expression e ->
            expr p e
            write p ";"
        | JsStatement.Const(name, init) ->
            write p "const "
            write p name
            write p " = "
            expr p init
            write p ";"
        | JsStatement.Import(specifiers, source) ->
            write p (sprintf "import { %s } from %s;" (String.concat ", " specifiers) (JsEscape.quoted source))
        | JsStatement.If(test, consequent, alternate) ->
            write p "if ("
            expr p test
            write p ") "
            block p consequent

            if not (List.isEmpty alternate) then
                write p " else "
                block p alternate
        | JsStatement.While(test, body) ->
            write p "while ("
            expr p test
            write p ") "
            block p body
        | JsStatement.Return e ->
            write p "return "
            expr p e
            write p ";"
        | JsStatement.Continue -> write p "continue;"
        | JsStatement.Assign(target, value) ->
            write p target
            write p " = "
            expr p value
            write p ";"

    /// The emitted ESM source (trailing-newline terminated) plus its mappings.
    let print (program: JsProgram) : PrintResult =
        let p =
            {
                Sb = StringBuilder()
                Line = 0
                Col = 0
                Indent = 0
                Maps = []
            }

        program.Body
        |> List.iter (fun s ->
            statement p s
            newline p
        )

        {
            Source = p.Sb.ToString()
            Mappings = List.rev p.Maps
        }


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
