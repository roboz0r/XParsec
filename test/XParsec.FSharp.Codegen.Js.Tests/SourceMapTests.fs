module XParsec.FSharp.Codegen.Js.Tests.SourceMapTests

open System
open System.Text.Json
open Expecto
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// V3 source maps: the emitted `console.log` resolves back to the source position of the
// `printfn` call it came from.

/// Compile `input` with its own text as the source, so source-map emission is on.
let private compileWithMap (input: string) (outputPath: string option) : JsArtifact =
    let project =
        { JsProjectInfo.defaults "Hi" with
            OutputPath = outputPath
            Source = Some(jsSource "hi.fsx" input)
        }

    Codegen.compile project (frozenOf input)

// ─── Multi-source maps ───────────────────────────────────────────────────
// A body served by another package splices onto the call site at emit, but its nodes keep
// their index into the PRODUCER's tokens, so the map publishes that file beside this one.

/// Compile through the real JS contract stack, so the manifest set's producer files are
/// retained and a served body's positions are readable.
let private compileMapped (name: string) (input: string) : JsArtifact =
    let project =
        { JsProjectInfo.defaults name with
            Source = Some(jsSource (name + ".fsx") input)
        }

    Codegen.compileWith jsContract.Value project (frozenOfJs input)

/// One decoded `mappings` segment. Decoded and not merely counted: a map that published the
/// right `sources` while encoding every segment against index 0 would look correct outside.
type private Segment =
    {
        GenLine: int
        GenCol: int
        SrcIndex: int
        SrcLine: int
        SrcCol: int
    }

[<Literal>]
let private b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/// Decode a V3 `mappings` field. An independent reader of the wire format: it shares no code
/// with the encoder, so an encoder consistent about a wrong convention still fails here.
let private decodeMappings (mappings: string) : Segment list =
    let digit (c: char) =
        match b64.IndexOf c with
        | -1 -> failtestf "`%c` is not a base64 VLQ digit" c
        | i -> i

    // The VLQ numbers of one segment: 5-bit groups little-endian, bit 6 continues, and the
    // sign rides the least-significant bit of the assembled value.
    let numbers (seg: string) : int list =
        let out = ResizeArray<int>()
        let mutable acc = 0
        let mutable shift = 0

        for c in seg do
            let d = digit c
            acc <- acc ||| ((d &&& 0x1F) <<< shift)

            if d &&& 0x20 <> 0 then
                shift <- shift + 5
            else
                let magnitude = acc >>> 1
                out.Add(if acc &&& 1 = 1 then -magnitude else magnitude)
                acc <- 0
                shift <- 0

        List.ofSeq out

    let out = ResizeArray<Segment>()
    // Generated column resets each line; the other three run across line boundaries.
    let mutable srcIndex = 0
    let mutable srcLine = 0
    let mutable srcCol = 0

    mappings.Split ';'
    |> Array.iteri (fun genLine group ->
        let mutable genCol = 0

        for seg in group.Split ',' do
            match seg with
            | "" -> ()
            | seg ->
                match numbers seg with
                | [ dGenCol; dSrcIndex; dSrcLine; dSrcCol ] ->
                    genCol <- genCol + dGenCol
                    srcIndex <- srcIndex + dSrcIndex
                    srcLine <- srcLine + dSrcLine
                    srcCol <- srcCol + dSrcCol

                    out.Add
                        {
                            GenLine = genLine
                            GenCol = genCol
                            SrcIndex = srcIndex
                            SrcLine = srcLine
                            SrcCol = srcCol
                        }
                | other -> failtestf "a segment carries %d fields, not the four this map emits" (List.length other)
    )

    List.ofSeq out

/// The map's `sources` / `sourcesContent` and its decoded segments. Read together, because a
/// source index means nothing apart from the array it indexes.
type private DecodedMap =
    {
        Sources: string[]
        Contents: string[]
        Segments: Segment list
    }

let private decodeMap (artifact: JsArtifact) : DecodedMap =
    match Codegen.toSourceMap artifact with
    | None -> failtest "expected a source map"
    | Some json ->
        use doc = JsonDocument.Parse json
        let root = doc.RootElement

        let strings (name: string) =
            [| for e in root.GetProperty(name).EnumerateArray() -> e.GetString() |]

        {
            Sources = strings "sources"
            Contents = strings "sourcesContent"
            Segments = decodeMappings (root.GetProperty("mappings").GetString())
        }

/// The 0-based `line` of `text`, as a V3 source line names it.
let private lineOf (text: string) (line: int) : string =
    let lines = text.Replace("\r\n", "\n").Split '\n'

    if line < lines.Length then
        lines.[line]
    else
        failtestf "line %d is past the end of a %d-line source" line lines.Length

/// Every source index a mapping actually resolves against, paired with the file it names.
let private attributions (m: DecodedMap) : (int * string) list =
    m.Segments
    |> List.map (fun s -> s.SrcIndex, m.Sources.[s.SrcIndex])
    |> List.distinct

[<Tests>]
let tests =
    testList
        "Codegen.Js source maps"
        [
            test "supplying source emits a `sourceMappingURL` comment" {
                let artifact = compileWithMap "printfn \"hi\"" None

                Expect.equal
                    (Codegen.toSource artifact)
                    "console.log(\"hi\");\n//# sourceMappingURL=Hi.mjs.map\n"
                    "the ESM source plus the sourceMappingURL comment"
            }

            test "no source text → no map, and the emitted JS is unchanged" {
                let artifact =
                    Codegen.compile (JsProjectInfo.defaults "Hi") (frozenOf "printfn \"hi\"")

                Expect.equal (Codegen.toSource artifact) "console.log(\"hi\");\n" "0a output unchanged"
                Expect.isNone (Codegen.toSourceMap artifact) "no map without source"
            }

            test "the map is well-formed V3 and resolves `console.log` to source origin" {
                let artifact = compileWithMap "printfn \"hi\"" None

                match Codegen.toSourceMap artifact with
                | None -> failtest "expected a source map"
                | Some mapJson ->
                    use doc = JsonDocument.Parse mapJson
                    let root = doc.RootElement
                    Expect.equal (root.GetProperty("version").GetInt32()) 3 "version 3"

                    Expect.equal (root.GetProperty("sources").[0].GetString()) "hi.fsx" "the source file name"

                    Expect.equal
                        (root.GetProperty("sourcesContent").[0].GetString())
                        "printfn \"hi\""
                        "the embedded source content"

                    // `console.log` is generated at (line 0, col 0); `printfn` is the first
                    // source token, also at (0, 0), so the single segment is four zero deltas.
                    Expect.equal (root.GetProperty("mappings").GetString()) "AAAA" "the VLQ mappings"
            }

            test "the `.js` and `.js.map` are written and Node still runs the `.js`" {
                let outDir = tmpDir "codegen-js-sourcemap"
                let jsPath = IO.Path.Combine(outDir, "hi.mjs")
                let mapPath = jsPath + ".map"

                let artifact = compileWithMap "printfn \"hi\"" (Some jsPath)
                Codegen.materialise artifact

                Expect.isTrue (IO.File.Exists jsPath) "the .js was written"
                Expect.isTrue (IO.File.Exists mapPath) "the .js.map was written"

                Expect.stringContains
                    (IO.File.ReadAllText jsPath)
                    "//# sourceMappingURL=hi.mjs.map"
                    "the sourceMappingURL references the emitted map basename"

                match runNode jsPath with
                | None -> skiptest "node not found on PATH"
                | Some(exitCode, output) ->
                    Expect.equal exitCode 0 (sprintf "node exits 0 (output: %s)" output)
                    Expect.equal (output.Replace("\r", "").Trim()) "hi" "Node prints hi"
            }

            test "a build that reaches no producer emits the single-source document verbatim" {
                // Pinned as the whole document rather than as a property of it, because the
                // only way to say the multi-source axis costs a one-file build nothing is to
                // fix the bytes.
                Expect.equal
                    (Codegen.toSourceMap (compileWithMap "printfn \"hi\"" None))
                    (Some(
                        "{\"version\":3,\"file\":\"Hi.mjs\",\"sourceRoot\":\"\","
                        + "\"sources\":[\"hi.fsx\"],\"sourcesContent\":[\"printfn \\\"hi\\\"\"],"
                        + "\"names\":[],\"mappings\":\"AAAA\"}"
                    ))
                    "one source, one content, and a source-index delta of 0 in the one segment"
            }

            test "an inlined body maps to the PRODUCER's own file and line" {
                // `1 + 2` reaches Vesper.Core twice: the operator's own `let inline` in
                // `ops-platform.js.fs`, whose trait call dispatches to `int`'s `(+)` in
                // `prim-types-min.js.fs`, where the template text is. Both splice onto here.
                let input = "let a = 1 + 2\n"
                let m = decodeMap (compileMapped "Add" input)

                Expect.equal
                    m.Sources.[0]
                    "Add.fsx"
                    "the consuming file keeps index 0; producers are published after it"

                let sourceIndex (file: string) =
                    match m.Sources |> Array.tryFindIndex ((=) file) with
                    | Some i -> i
                    | None -> failtestf "%s is not among the map's sources: %A" file m.Sources

                sourceIndex "Vesper.Core/ops-platform.js.fs" |> ignore
                let templateSrc = sourceIndex "Vesper.Core/prim-types-min.js.fs"

                match m.Segments |> List.filter (fun s -> s.SrcIndex = templateSrc) with
                | [ seg ] ->
                    let line = lineOf m.Contents.[seg.SrcIndex] seg.SrcLine

                    // Asserted against the CONTENT the map embeds, not against a line number,
                    // because editing the producer may legitimately move which line it is.
                    Expect.stringContains line "($0 + $1) | 0" "the producer line is `int`'s own `(+)` body"

                    Expect.stringStarts
                        (line.Substring seg.SrcCol)
                        "(#"
                        "…and the column is the intrinsic's own token, not the head of the line"
                | other -> failtestf "exactly one emitted node is the producer's `(# … #)`; got %d" (List.length other)

                // The operands were written HERE and stay here, because a map that simply
                // relabelled every segment onto the producer would pass the assertion above.
                Expect.equal
                    (m.Segments
                     |> List.filter (fun s -> s.SrcIndex = 0)
                     |> List.map (fun s -> s.SrcLine, s.SrcCol))
                    [ 0, input.IndexOf "1"; 0, input.IndexOf "2" ]
                    "the two literals map back to where the caller wrote them"
            }

            test "an inlined MEMBER body maps to the producer too, not to the indexing site" {
                // The member half of the same claim. `a.[i]` is `'T[]`'s `get_Item`, lifted off
                // `array-index.js.fs`, and its nodes map to the file the member was WRITTEN in,
                // not to the line that merely INDEXES the array.
                let input = "let read (a: int[]) (i: int) : int = a.[i]\n"
                let m = decodeMap (compileMapped "Idx" input)

                Expect.contains
                    m.Sources
                    "Vesper.Core/array-index.js.fs"
                    "the file the member was written in is published beside the caller's"

                let fromMember =
                    m.Segments
                    |> List.filter (fun s -> m.Sources.[s.SrcIndex] = "Vesper.Core/array-index.js.fs")

                Expect.isNonEmpty fromMember "the emitted index expression comes from the member body"

                for seg in fromMember do
                    Expect.stringContains
                        (lineOf m.Contents.[seg.SrcIndex] seg.SrcLine)
                        "ldelem.any"
                        "…on the member's own intrinsic line"

                // The object argument and the index were written HERE and stay here, because a
                // map relabelling the whole expansion onto the producer would pass the check.
                Expect.contains
                    (attributions m |> List.map snd)
                    "Idx.fsx"
                    "the array and the index still resolve against the file that names them"
            }

            test "a fused argument POPS back to the consuming file" {
                // `a && b` outlines as `if a then ⟨b⟩ else false`: the conditional is
                // `ops-std.fs`'s and `b` is the caller's, so without the pop the caller's own
                // `false` would be attributed to a Vesper source line.
                let input = "let a = true && false\n"
                let m = decodeMap (compileMapped "And" input)

                Expect.equal
                    m.Sources
                    [| "And.fsx"; "Vesper.Core/ops-std.fs" |]
                    "the entry's file is published beside the caller's"

                for seg in m.Segments |> List.filter (fun s -> s.SrcIndex = 1) do
                    Expect.stringContains
                        (lineOf m.Contents.[seg.SrcIndex] seg.SrcLine)
                        "if e1 then e2 else false"
                        "every producer-attributed node sits on the line the entry was written on"

                Expect.contains
                    (m.Segments
                     |> List.filter (fun s -> s.SrcIndex = 0)
                     |> List.map (fun s -> s.SrcLine, s.SrcCol))
                    (0, input.IndexOf "false")
                    "the `[<CallAtMostOnce>]` operand maps to the caller's `false`, in the caller's file"
            }

            test "a NESTED frame is attributed to its own file, not to the entry that reached it" {
                // `(|>)`'s body (in `ops-std.fs`) carries the edge naming `not`'s entry (in
                // `ops-platform.js.fs`), and the emitted `!` comes from `not`, so an
                // implementation attributing the expansion to its OUTERMOST entry would fail.
                let input = "let b = true\nlet a = b |> not\n"
                let m = decodeMap (compileMapped "Pipe" input)

                Expect.equal
                    m.Sources
                    [| "Pipe.fsx"; "Vesper.Core/ops-platform.js.fs"; "Vesper.Core/ops-std.fs" |]
                    "both entries' files are published, in the retention's path order"

                match m.Segments |> List.filter (fun s -> s.SrcIndex > 0) with
                | [ seg ] ->
                    Expect.equal
                        m.Sources.[seg.SrcIndex]
                        "Vesper.Core/ops-platform.js.fs"
                        "the inner frame names the file `not` was written in"

                    Expect.stringContains
                        (lineOf m.Contents.[seg.SrcIndex] seg.SrcLine)
                        "let inline not"
                        "…on `not`'s own definition line"
                | other -> failtestf "one emitted node comes from a producer; got %d" (List.length other)

                Expect.equal
                    (attributions m |> List.map snd |> List.sort)
                    [ "Pipe.fsx"; "Vesper.Core/ops-platform.js.fs" ]
                    "and the operand `b` still resolves against the file that wrote it"
            }
        ]
