module XParsec.FSharp.Codegen.Clr.Tests.PrintfHappyPathTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Vesper.Printf happy path: fully-applied literal printf lowered to the
// `Vesper.Formatter` write-through handler. The lowering is additive — any
// specifier the happy path doesn't handle keeps the existing FSharp.Core cold
// path. See docs/vesper-printf-plan.md / docs/printf-handoff.md.

let private soleDecl (src: string) : TDecl =
    let tast = analyse src
    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics for: %s" src)

    match tast.Decls with
    | EqList [ d ] -> d
    | _ -> failtestf "expected one decl for %s, got: %A" src tast.Decls

let private runPrints (name: string) (src: string) (expected: string) =
    let _, artifact = compileSource name src
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    Expect.equal exitCode 0 (sprintf "Main returns 0 for: %s" src)
    Expect.equal (output.Trim()) expected (sprintf "%s prints %s" src expected)

/// Compile + run `src`, asserting output matches `expected`. Trims only the
/// trailing newline (so leading/embedded alignment spaces survive) and pairs
/// with an `expected` from the test process's own `sprintf` for byte-for-byte
/// parity with real F#.
let private runParity (name: string) (src: string) (expected: string) =
    let _, artifact = compileSource name src
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    Expect.equal exitCode 0 (sprintf "Main returns 0 for: %s" src)
    Expect.equal (output.TrimEnd('\r', '\n')) expected (sprintf "%s == F# parity" src)

[<Tests>]
let tests =
    testList
        "PrintfHappyPath"
        [
            // A literal-only `printfn` (no holes) lowers to a Format node carrying a
            // single `Lit` segment — the simplest happy-path shape (former Slice1).
            test "`printfn \"hi\"` lowers to a single-literal Format node and prints \"hi\"" {
                match soleDecl "printfn \"hi\"" with
                | TDecl.Expression(TExpr.Format(sink, segs, _), _) ->
                    Expect.equal sink (FormatSink.ToStdOut true) "printfn → stdout with newline"

                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "hi" ] -> ()
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other

                runPrints "PHpHi" "printfn \"hi\"" "hi"
            }

            test "`printfn \"%s\"` lowers to a single string hole" {
                match soleDecl "printfn \"%s\" \"world\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.String "world", _)) ] ->
                        Expect.equal hole.Ty (TyConst("string", EqArray.empty)) "the %s hole types as string"
                        Expect.equal hole.Format None "no .NET format string for %s"
                        Expect.equal hole.Alignment None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`sprintf` lowers to a string-result sink" {
                match soleDecl "sprintf \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, _, ty), _) ->
                    Expect.equal ty (TyConst("string", EqArray.empty)) "sprintf yields a string"
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "interleaved literals and holes keep source order" {
                match soleDecl "printfn \"a=%d b=%s!\" 7 \"x\"" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "a="
                        FormatSeg.Hole(_, TExpr.Const(TConstValue.Int 7, _))
                        FormatSeg.Lit " b="
                        FormatSeg.Hole(_, TExpr.Const(TConstValue.String "x", _))
                        FormatSeg.Lit "!" ] -> ()
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%%` lowers, collapsing to a single literal percent (P2)" {
                match soleDecl "printfn \"100%%\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "100%" ] -> ()
                    | other -> failtestf "expected one Lit \"100%%\" → \"100%%\" collapse, got: %A" other
                | other -> failtestf "expected a Format node for %%%%, got: %A" other
            }

            test "`%5d` carries an alignment, no flags" {
                match soleDecl "printfn \"%5d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Alignment (Some 5) "width 5 → alignment 5"
                        Expect.equal hole.Format None "no format string for %d"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2f` carries a fixed-point format string" {
                match soleDecl "printfn \"%.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some "F2") "precision 2 → \"F2\""
                        Expect.equal hole.Ty (TyConst("float", EqArray.empty)) "the %f hole types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%x` lowers to a Formatted hole with the `\"x\"` .NET format" {
                match soleDecl "printfn \"%x\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%x is a Formatted hole"
                        Expect.equal hole.Format (Some "x") "%x → lowercase \"x\""
                        Expect.equal hole.Ty (TyConst("int", EqArray.empty)) "%x types its argument as int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%X` keeps the upper-case base (`\"X\"`)" {
                match soleDecl "printfn \"%X\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal hole.Format (Some "X") "%X → upper-case \"X\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%B` (binary) lowers via the .NET 8 `\"B\"` format" {
                match soleDecl "printfn \"%B\" 5" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%B is Formatted"
                        Expect.equal hole.Format (Some "B") "%B → \"B\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%e` carries an exponential format string at default precision 6" {
                match soleDecl "printfn \"%e\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some "e6") "%e → \"e6\""
                        Expect.equal hole.Ty (TyConst("float", EqArray.empty)) "%e types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%O` lowers as a Formatted hole typed at its argument" {
                match soleDecl "printfn \"%O\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%O is Formatted"
                        Expect.equal hole.Format None "no .NET format for %O"
                        Expect.equal hole.Ty (TyConst("int", EqArray.empty)) "%O's hole types as the argument (int)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%b` lowers to a BoolText hole (no format string)" {
                match soleDecl "printfn \"%b\" true" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.BoolText "%b is BoolText"
                        Expect.equal hole.Format None "no .NET format for %b"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%o` lowers to an Octal hole" {
                match soleDecl "printfn \"%o\" 8" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal hole.Kind PrintfSpec.HoleKind.Octal "%o is Octal"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%u` lowers to an Unsigned hole" {
                match soleDecl "printfn \"%u\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Unsigned "%u is Unsigned"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-5d` (left-align) carries a negative alignment, no format" {
                match soleDecl "printfn \"%-5d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Alignment (Some -5) "`-` flag → negative alignment"
                        Expect.equal hole.Format None "no format string for %d"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%05d` (zero-pad) carries a width-bearing format, no alignment" {
                match soleDecl "printfn \"%05d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some "D5") "`0` flag → \"D5\""
                        Expect.equal hole.Alignment None "zero-pad uses a format string, not alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08x` (zero-pad hex) carries a width-bearing hex format" {
                match soleDecl "printfn \"%08x\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some "x8") "`0` + width 8 → \"x8\""
                        Expect.equal hole.Alignment None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%g` (compact float) is NOT lowered (exponent case)" {
                match soleDecl "printfn \"%g\" 1.5" with
                | TDecl.Expression(TExpr.Format _, _) -> failtest "%g must stay on the cold path"
                | TDecl.Expression(TExpr.App _, _) -> ()
                | other -> failtestf "unexpected TAST for %%g: %A" other
            }

            test "`%+05d` (forced sign + zero-pad) is NOT lowered" {
                match soleDecl "printfn \"%+05d\" 42" with
                | TDecl.Expression(TExpr.Format _, _) -> failtest "%+05d must stay on the cold path"
                | TDecl.Expression(TExpr.App _, _) -> ()
                | other -> failtestf "unexpected TAST for %%+05d: %A" other
            }

            test "`%+e` (forced sign on exponential) is NOT lowered" {
                match soleDecl "printfn \"%+e\" 1234.5" with
                | TDecl.Expression(TExpr.Format _, _) -> failtest "%+e must stay on the cold path"
                | TDecl.Expression(TExpr.App _, _) -> ()
                | other -> failtestf "unexpected TAST for %%+e: %A" other
            }

            test "`%08e` (zero-pad on exponential) is NOT lowered" {
                // B2 lowers `0`-on-`%f` only; `%e` exponent zero-pad parity is
                // subtle, so it stays on the cold path (additive).
                match soleDecl "printfn \"%08e\" 1234.5" with
                | TDecl.Expression(TExpr.Format _, _) -> failtest "%08e must stay on the cold path"
                | TDecl.Expression(TExpr.App _, _) -> ()
                | other -> failtestf "unexpected TAST for %%08e: %A" other
            }

            // ---- `%A` structural format (P3 step 2) ----
            // The engine handles primitives / string / char / bool / tuple / list /
            // array faithfully today; those lower to a `Structured` hole. Records /
            // DUs hit the runtime `ToString` fallback (wrong by our spec) until
            // step-3 synthesis, so they keep the FSharp.Core cold path (the type
            // gate). The print-width budget rides in the `Alignment` slot.

            test "`%A` of an int lowers to a Structured hole (default width budget)" {
                match soleDecl "printfn \"%A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _)) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Structured "%A is a Structured hole"
                        Expect.equal hole.Format None "no .NET format string for %A"
                        Expect.equal hole.Alignment None "plain %A → no budget (emit defaults to 80)"
                        Expect.equal hole.Ty (TyConst("int", EqArray.empty)) "the %A hole types as its argument"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%0A` carries a zero width budget (the flat mode)" {
                match soleDecl "printfn \"%0A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Structured "%0A is Structured"
                        Expect.equal hole.Alignment (Some 0) "`0` flag → width budget 0 (never break)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%20A` carries the width as the print budget" {
                match soleDecl "printfn \"%20A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Structured "%20A is Structured"
                        Expect.equal hole.Alignment (Some 20) "width 20 → print budget 20"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%A` of a list lowers to a Structured hole (engine-faithful)" {
                match soleDecl "printfn \"%A\" [ 1; 2; 3 ]" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Structured "%A of a list is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2A` (precision) stays on the cold path" {
                match soleDecl "printfn \"%.2A\" 42" with
                | TDecl.Expression(TExpr.Format _, _) -> failtest "%.2A must stay on the cold path"
                | TDecl.Expression(TExpr.App _, _) -> ()
                | other -> failtestf "unexpected TAST for %%.2A: %A" other
            }

            // NOTE on the record/DU type-gate: a project-local record / DU is
            // engine-faithful only once step-3 synthesis attaches its `Format` — and
            // "project-local" is keyed on the compilation's target assembly. The bare
            // `analyse` harness compiles with no assembly name (home = `None`), so a
            // record there is still treated external (cold); the *real* codegen path
            // (`compileSource`, a named assembly) lowers it on the engine. The
            // runtime tests below prove that end-to-end. `%.2A` (precision) stays cold
            // regardless — see the cold-path test above.

            test "`printfn \"%s\"` prints the string" { runPrints "PHpString" "printfn \"%s\" \"world\"" "world" }

            test "multi-hole `printfn \"%d and %s\"` prints both in order" {
                runPrints "PHpMulti" "printfn \"%d and %s\" 7 \"x\"" "7 and x"
            }

            test "`printfn \"%.2f\"` prints invariant fixed-point" {
                runPrints "PHpFloat" "printfn \"%.2f\" 3.14159" "3.14"
            }

            test "`printfn \"%5d\"` right-justifies in a width-5 field" {
                let _, artifact = compileSource "PHpAlign" "printfn \"%5d\" 42"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.TrimEnd()) "   42" "right-justified in a width-5 field (3 leading spaces)"
            }

            test "`printfn \"100%%\"` prints a literal percent (via cold path)" {
                runPrints "PHpPercent" "printfn \"100%%\"" "100%"
            }

            test "`sprintf` result feeds another printf" {
                runPrints "PHpSprintf" "printfn \"%s\" (sprintf \"%d!\" 42)" "42!"
            }

            // `%A` runtime — oracle is the structural spec (copy-pasteable source),
            // not `sprintf "%A"`; small values coincide with F# (re-greening slice 4).
            test "`%A` of a list prints the copy-pasteable literal (slice 4)" {
                runPrints "PHpStructList" "printfn \"%A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            test "`%A` of an int prints the bare value" { runPrints "PHpStructInt" "printfn \"%A\" 42" "42" }

            test "`%A` of a string prints the quoted, escapable literal" {
                runPrints "PHpStructStr" "printfn \"%A\" \"hi\"" "\"hi\""
            }

            test "`%0A` of a list prints flat (fits the budget either way)" {
                runPrints "PHpStructFlat" "printfn \"%0A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            // ---- `%A` of a record / DU: step-3 synthesised `Format` (the engine) ----
            // The backend now synthesises `IStructuralFormattable.Format` on every
            // record / DU, so a project-local one renders on the engine (no cold
            // path). Oracle is the copy-pasteable spec, not `sprintf "%A"`.

            test "`%A` of a record prints the copy-pasteable record literal" {
                runPrints
                    "PHpStructRec"
                    "type R = { X: int; Y: string }\nprintfn \"%A\" { X = 1; Y = \"a\" }"
                    "{ X = 1; Y = \"a\" }"
            }

            test "`%A` of a record breaks under a tiny width budget (dedented closer)" {
                runPrints
                    "PHpStructRecBreak"
                    "type R = { X: int; Y: string }\nprintfn \"%5A\" { X = 1; Y = \"a\" }"
                    "{ X = 1;\n  Y = \"a\" }"
            }

            // NOTE: the DU value is bound to a lowercase local first. Writing the
            // constructor *directly* as the printf argument (`printfn "%A" (S 3)`)
            // trips a pre-existing front-end parse quirk — a constructor /
            // parenthesised application adjacent to the format string is
            // mis-associated as `printfn ("%A" (S 3))` — that is orthogonal to the
            // `%A` synthesis under test. Binding to a local sidesteps it; the
            // list-of-DUs test below also exercises the synthesised union `Format`.
            test "`%A` of a nullary DU case prints the bare identifier" {
                runPrints "PHpStructDuNullary" "type Opt = | N | S of int\nlet v = N\nprintfn \"%A\" v" "N"
            }

            test "`%A` of a payload DU case prints `Case payload` (no parens)" {
                runPrints "PHpStructDuPayload" "type Opt = | N | S of int\nlet v = S 3\nprintfn \"%A\" v" "S 3"
            }

            test "`%A` of a nested DU application parenthesises the argument" {
                runPrints "PHpStructDuNest" "type Opt = | N | S of Opt\nlet v = S (S N)\nprintfn \"%A\" v" "S (S N)"
            }

            test "`%A` of a record nested in a list renders both structurally" {
                runPrints
                    "PHpStructRecInList"
                    "type R = { X: int }\nprintfn \"%A\" [ { X = 1 }; { X = 2 } ]"
                    "[{ X = 1 }; { X = 2 }]"
            }

            test "`%x` prints lowercase hex" { runParity "PHpHex" "printfn \"%x\" 255" (sprintf "%x" 255) }

            test "`%X` prints upper-case hex" { runParity "PHpHexU" "printfn \"%X\" 255" (sprintf "%X" 255) }

            test "`%B` prints binary" { runParity "PHpBin" "printfn \"%B\" 5" (sprintf "%B" 5) }

            test "`%b` prints lowercase true/false" {
                runParity "PHpBoolT" "printfn \"%b\" true" (sprintf "%b" true)
                runParity "PHpBoolF" "printfn \"%b\" false" (sprintf "%b" false)
            }

            test "`%o` prints octal" { runParity "PHpOct" "printfn \"%o\" 8" (sprintf "%o" 8) }

            test "`%o` prints negative octal as 32-bit two's-complement" {
                runParity "PHpOctNeg" "printfn \"%o\" (0 - 1)" (sprintf "%o" (0 - 1))
            }

            test "`%u` prints unsigned" { runParity "PHpUns" "printfn \"%u\" 42" (sprintf "%u" 42) }

            test "`%u` reinterprets a negative int as unsigned" {
                runParity "PHpUnsNeg" "printfn \"%u\" (0 - 1)" (sprintf "%u" (0 - 1))
            }

            test "`%O` prints via ToString" { runParity "PHpObj" "printfn \"%O\" 42" (sprintf "%O" 42) }

            test "`%e` prints exponential at default precision" {
                runParity "PHpExp" "printfn \"%e\" 1234.5" (sprintf "%e" 1234.5)
            }

            test "`%-5d` left-justifies in a width-5 field" {
                runParity "PHpLeft" "printfn \"%-5d\" 42" (sprintf "%-5d" 42)
            }

            test "`%05d` zero-pads to width 5" { runParity "PHpZero" "printfn \"%05d\" 42" (sprintf "%05d" 42) }

            test "`%08x` zero-pads hex to width 8" {
                runParity "PHpZeroHex" "printfn \"%08x\" 255" (sprintf "%08x" 255)
            }

            test "`%c` lowers to a Formatted char hole (no format string)" {
                match soleDecl "printfn \"%c\" 'a'" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Char 'a', _)) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%c is a Formatted hole"
                        Expect.equal hole.Format None "no .NET format string for %c"
                        Expect.equal hole.Ty (TyConst("char", EqArray.empty)) "the %c hole types as char"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%M` lowers to a Formatted decimal hole (no format string)" {
                match soleDecl "printfn \"%M\" 3.14M" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Decimal d, _)) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%M is a Formatted hole"
                        Expect.equal hole.Format None "no .NET format string for %M"
                        Expect.equal hole.Ty (TyConst("decimal", EqArray.empty)) "the %M hole types as decimal"
                        Expect.equal d 3.14M "the decimal literal round-trips its value"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2M` (precision) stays on the cold path" {
                // F# %M precision semantics are unusual — the const subset unblocks
                // bare %M, but `%.2M` keeps the FSharp.Core path (additive).
                match soleDecl "printfn \"%.2M\" 3.14159M" with
                | TDecl.Expression(TExpr.Format _, _) -> failtest "%.2M must stay on the cold path"
                | TDecl.Expression(TExpr.App _, _) -> ()
                | other -> failtestf "unexpected TAST for %%.2M: %A" other
            }

            test "`%c` prints the char" { runParity "PHpChar" "printfn \"%c\" 'a'" (sprintf "%c" 'a') }

            test "`%c` prints a punctuation char" { runParity "PHpCharP" "printfn \"%c\" '*'" (sprintf "%c" '*') }

            test "`%c` prints an escaped char (tab)" {
                runParity "PHpCharEsc" "printfn \"%c\" '\\t'" (sprintf "%c" '\t')
            }

            test "`%5c` right-justifies the char in a width-5 field" {
                runParity "PHpCharAlign" "printfn \"%5c\" 'x'" (sprintf "%5c" 'x')
            }

            test "`%M` prints an invariant decimal" { runParity "PHpDec" "printfn \"%M\" 3.14M" (sprintf "%M" 3.14M) }

            test "`%M` prints a whole decimal" { runParity "PHpDecW" "printfn \"%M\" 42M" (sprintf "%M" 42M) }

            test "`%M` preserves trailing-zero scale" {
                runParity "PHpDecS" "printfn \"%M\" 1.50M" (sprintf "%M" 1.50M)
            }

            test "a char literal binds to a local and reloads" {
                runParity "PHpCharLet" "let c = 'Q'\nprintfn \"%c\" c" (sprintf "%c" 'Q')
            }

            test "a decimal literal binds to a local and reloads" {
                runParity "PHpDecLet" "let d = 1.5M\nprintfn \"%M\" d" (sprintf "%M" 1.5M)
            }

            test "`%+d` (forced sign) lowers to a Formatted hole with a section format" {
                match soleDecl "printfn \"%+d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%+d is a Formatted hole"
                        Expect.equal hole.Format (Some "+0;-0") "%+d → section format \"+0;-0\""
                        Expect.equal hole.Alignment None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`% d` (space sign) lowers with a leading-space section format" {
                match soleDecl "printfn \"% d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some " 0;-0") "% d → section format \" 0;-0\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+.2f` (forced sign float) builds the section format from the precision" {
                match soleDecl "printfn \"%+.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some "+0.00;-0.00") "%+.2f → \"+0.00;-0.00\""
                        Expect.equal hole.Ty (TyConst("float", EqArray.empty)) "%f types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+8.2f` rides a width as the handler alignment" {
                match soleDecl "printfn \"%+8.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Format (Some "+0.00;-0.00") "section format from precision"
                        Expect.equal hole.Alignment (Some 8) "width 8 → alignment 8"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+d` prints a forced + on a positive int" {
                runParity "PHpPlusD" "printfn \"%+d\" 42" (sprintf "%+d" 42)
            }

            test "`%+d` keeps - for a negative int" {
                runParity "PHpPlusDNeg" "printfn \"%+d\" (0 - 42)" (sprintf "%+d" (0 - 42))
            }

            test "`%+d` forces + on zero" { runParity "PHpPlusDZero" "printfn \"%+d\" 0" (sprintf "%+d" 0) }

            test "`% d` space-signs a positive int" { runParity "PHpSpaceD" "printfn \"% d\" 42" (sprintf "% d" 42) }

            test "`% d` keeps - for a negative int" {
                runParity "PHpSpaceDNeg" "printfn \"% d\" (0 - 42)" (sprintf "% d" (0 - 42))
            }

            test "`% d` space-signs zero" { runParity "PHpSpaceDZero" "printfn \"% d\" 0" (sprintf "% d" 0) }

            test "`%+.2f` forces + on a positive float" {
                runParity "PHpPlusF" "printfn \"%+.2f\" 3.14159" (sprintf "%+.2f" 3.14159)
            }

            // A negative *float* can't be produced in the codegen subset (no float
            // arithmetic / unary negation), so the float `-` section is covered only
            // by the shape test's `"+0.00;-0.00"` assertion, not a run.

            test "`%+8.2f` composes the forced sign with width-as-alignment" {
                runParity "PHpPlusFAlign" "printfn \"%+8.2f\" 3.14159" (sprintf "%+8.2f" 3.14159)
            }

            test "`%08.2f` lowers to a ZeroPaddedFloat hole (format body + width)" {
                match soleDecl "printfn \"%08.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.ZeroPaddedFloat "%08.2f is a ZeroPaddedFloat hole"
                        Expect.equal hole.Format (Some "F2") "the \"F<prec>\" body rides in Format"
                        Expect.equal hole.Alignment (Some 8) "the field width rides in Alignment"
                        Expect.equal hole.Ty (TyConst("float", EqArray.empty)) "the %f hole types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08f` defaults the precision to 6 in the format body" {
                match soleDecl "printfn \"%08f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.ZeroPaddedFloat "%08f is ZeroPaddedFloat"
                        Expect.equal hole.Format (Some "F6") "no precision → default 6 → \"F6\""
                        Expect.equal hole.Alignment (Some 8) "width 8"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // A negative *float* can't be produced in the codegen subset, so the
            // sign-then-zeros placement (F# `%08.2f` of `-3.14159` is `"-0003.14"`)
            // is exercised only by the C# handler; the runs below cover positive /
            // zero / wider-than-field cases.

            test "`%08.2f` zero-pads a positive float to width 8" {
                runParity "PHpZFloat" "printfn \"%08.2f\" 3.14159" (sprintf "%08.2f" 3.14159)
            }

            test "`%010.3f` zero-pads to width 10" {
                runParity "PHpZFloatW" "printfn \"%010.3f\" 3.14159" (sprintf "%010.3f" 3.14159)
            }

            test "`%08.0f` zero-pads a zero-precision float" {
                runParity "PHpZFloat0" "printfn \"%08.0f\" 3.0" (sprintf "%08.0f" 3.0)
            }

            test "`%08f` zero-pads at default precision 6 (no padding when already wider)" {
                runParity "PHpZFloatD" "printfn \"%08f\" 3.14159" (sprintf "%08f" 3.14159)
            }

            test "`%02.2f` leaves a body wider than the field untouched" {
                runParity "PHpZFloatN" "printfn \"%02.2f\" 3.14159" (sprintf "%02.2f" 3.14159)
            }

            test "`$\"x={1}\"` lowers to a ToString Format node" {
                match soleDecl "$\"x={1}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, ty), _) ->
                    Expect.equal ty (TyConst("string", EqArray.empty)) "interpolation yields a string"

                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "x="; FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 1, _)) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "a plain hole is Formatted"
                        Expect.equal hole.Format None "no format clause"
                        Expect.equal hole.Ty (TyConst("int", EqArray.empty)) "the hole types as int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "`$\"{255:X}\"` carries the :format clause as the .NET format" {
                match soleDecl "$\"{255:X}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal hole.Format (Some "X") "clause :X → \"X\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`$\"%d{x}\"` typed interpolation lowers via the printf spec" {
                match soleDecl "$\"%d{7}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal hole.Kind PrintfSpec.HoleKind.Formatted "%d hole is Formatted"
                        Expect.equal hole.Ty (TyConst("int", EqArray.empty)) "%d constrains the hole to int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "a plain (non-interpolated) string is still a Const, not a Format" {
                match soleDecl "\"hello\"" with
                | TDecl.Expression(TExpr.Const(TConstValue.String "hello", _), _) -> ()
                | other -> failtestf "a plain string must not become a Format node: %A" other
            }

            test "`$\"n={42}\"` prints the interpolated int" {
                runParity "InterpInt" "printfn \"%s\" $\"n={42}\"" (sprintf "%s" $"n={42}")
            }

            test "interpolation with a bound string hole" {
                let name = "world"
                runParity "InterpStr" "let name = \"world\"\nprintfn \"%s\" $\"hi {name}\"" (sprintf "%s" $"hi {name}")
            }

            test "multi-hole interpolation keeps source order" {
                runParity "InterpMulti" "printfn \"%s\" $\"a={1} b={2}\"" (sprintf "%s" $"a={1} b={2}")
            }

            test "`$\"{255:X}\"` formats hex via the clause" {
                runParity "InterpHex" "printfn \"%s\" $\"{255:X}\"" (sprintf "%s" $"{255:X}")
            }

            test "`$\"{3.14159:F2}\"` formats fixed-point via the clause" {
                runParity "InterpF2" "printfn \"%s\" $\"{3.14159:F2}\"" (sprintf "%s" $"{3.14159:F2}")
            }

            test "`$\"%d{7}\"` typed interpolation prints the int" {
                runParity "InterpTyped" "printfn \"%s\" $\"%d{7}\"" (sprintf "%s" $"%d{7}")
            }

            test "interpolation is a string value usable as a sprintf result" {
                runParity
                    "InterpSprintf"
                    "printfn \"%s\" (sprintf \"%s\" $\"v={9}\")"
                    (sprintf "%s" (sprintf "%s" $"v={9}"))
            }
        ]
