module XParsec.FSharp.Codegen.Clr.Tests.PrintfHappyPathTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `HoleSpec.{Kind,Format,Alignment}` projection members were retired; reconstruct the
// legacy CLR `(HoleKind, .NET-format, alignment)`
// triple here from the hole's classified `HoleForm` (`hole.Source`) via the CLR-only
// `ClrHoleFormat.toDotNetFormat`, so these per-specifier projection assertions keep
// pinning the CLR emission shape. `%A` reproduces the slot punning the old triple
// carried (width in the alignment slot, size as a decimal string in the format slot).
// The alignment slot is an `Alignment` now; project it back to the legacy signed
// `int option` these assertions were written against (`Star` — a runtime `%*d`
// width — has no static value, so `None`).
let private alignToOpt (a: PrintfHoleForm.Alignment) : int option =
    match a with
    | PrintfHoleForm.Alignment.None -> None
    | PrintfHoleForm.Alignment.Const n -> Some n
    | PrintfHoleForm.Alignment.Star _ -> None

let private triple (hole: HoleSpecG<'ty, 'tok>) : PrintfSpec.HoleKind * string option * int option =
    match hole.Source with
    | HoleSpecSource.RawFormat fmt -> PrintfSpec.HoleKind.Formatted, fmt, None
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(fmt, align)) ->
        let k, f, a = ClrHoleFormat.toDotNetFormat fmt align
        k, f, alignToOpt a
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(width, size)) ->
        let widthSlot =
            match width with
            | PrintfHoleForm.PrintWidth.Default -> None
            | PrintfHoleForm.PrintWidth.Never -> Some 0
            | PrintfHoleForm.PrintWidth.Cols n -> Some n
            | PrintfHoleForm.PrintWidth.Star -> None

        let sizeSlot =
            match size with
            | PrintfHoleForm.PrintSize.Default -> None
            | PrintfHoleForm.PrintSize.Cols n -> Some(string n)
            | PrintfHoleForm.PrintSize.Star -> None

        PrintfSpec.HoleKind.Structured, sizeSlot, widthSlot
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Callback _) ->
        // `%a`/`%t` callback holes carry no CLR `(HoleKind, format, alignment)` triple —
        // they ride a `FormatSeg.CallbackHole`, not a `Hole`; these projection assertions
        // never inspect one.
        failwith "triple: callback hole has no field projection"

let private kindOf hole =
    let k, _, _ = triple hole
    k

let private formatOf hole =
    let _, f, _ = triple hole
    f

let private alignmentOf hole =
    let _, _, a = triple hole
    a

let private widthBudgetOf hole = alignmentOf hole

let private sizeBudgetOf hole =
    match formatOf hole with
    | Some s -> Some(int s)
    | None -> None

// Vesper.Printf happy path: fully-applied literal printf lowered to the
// `Vesper.Formatter` write-through handler. Every lowerable specifier form now
// lowers natively — there is no FSharp.Core cold path left to fall back to (the
// only un-lowered forms are diagnosed `Severity.Error`s at the gate).

let private soleDecl (src: string) : TDecl =
    let tast = analyse src
    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics for: %s" src)

    match tast.Decls with
    | EqList [ d ] -> d
    | _ -> failtestf "expected one decl for %s, got: %A" src tast.Decls

let private runPrints (name: string) (src: string) (expected: string) =
    let exitCode, output = withPrintfAlc (fun alc -> runDriverInAlc alc src)
    Expect.equal exitCode 0 (sprintf "Main returns 0 for: %s" src)
    Expect.equal (output.Trim()) expected (sprintf "%s prints %s" src expected)

/// Compile + run `src` against the Vesper handler, asserting output matches
/// `expected`. Trims only the trailing newline (so leading/embedded alignment
/// spaces survive) and pairs with an `expected` from the test process's own
/// `sprintf` for byte-for-byte parity with real F#.
let private runParity (name: string) (src: string) (expected: string) =
    let exitCode, output = withPrintfAlc (fun alc -> runDriverInAlc alc src)
    Expect.equal exitCode 0 (sprintf "Main returns 0 for: %s" src)
    Expect.equal (output.TrimEnd('\r', '\n')) expected (sprintf "%s == F# parity" src)

/// The deepest exception the compiled driver's entry point throws, as its type's
/// full name paired with its `ParamName` (for an `ArgumentException`), or `None`
/// when it returns normally. The asserting run helpers turn a throw into a test
/// failure, so a run that must OBSERVE F#'s throw (a negative star width, which
/// `PadLeft` rejects with `ArgumentOutOfRangeException("totalWidth")`) invokes
/// directly. The `ParamName` is the native path's parity bar with F#.
let private entryPointThrew (src: string) : (string * string option) option =
    withPrintfAlc (fun alc ->
        let _, artifact = compileSource "PHpStarThrow" src
        use ms = new System.IO.MemoryStream(Codegen.toBytes artifact)
        let asm = alc.LoadFromStream ms
        let entry = asm.EntryPoint

        try
            entry.Invoke(null, [| box (Array.empty<string>) |]) |> ignore
            None
        with :? System.Reflection.TargetInvocationException as e when not (isNull e.InnerException) ->
            let rec deepest (ex: exn) =
                if isNull ex.InnerException then
                    ex
                else
                    deepest ex.InnerException

            let ex = deepest e.InnerException

            let paramName =
                match ex with
                | :? System.ArgumentException as ae -> Option.ofObj ae.ParamName
                | _ -> None

            Some(ex.GetType().FullName, paramName)
    )

[<Tests>]
let tests =
    testList
        "PrintfHappyPath"
        [
            // A literal-only `printfn` (no holes) lowers to a Format node carrying a
            // single `Lit` segment — the simplest happy-path shape (former Slice1).
            test "`printfn \"hi\"` lowers to a single-literal Format node and prints \"hi\"" {
                match soleDecl "printfn \"hi\"" with
                | TDecl.Expression(TExpr.Format(sink, segs, _, _), _) ->
                    Expect.equal sink (FormatSink.ToStdOut true) "printfn → stdout with newline"

                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "hi" ] -> ()
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other

                runPrints "PHpHi" "printfn \"hi\"" "hi"
            }

            // ---- E1: a format literal bound to a name / ascribed (const-prop) ----
            // A `PrintfFormat`-typed `let` (or `(… : Fmt)` ascription) is not a
            // syntactic literal AT the call site, but the bound literal is recovered
            // (`PrintfFormatLiterals`) and lowered natively — the ONLY runnable path: a
            // format *value* applied to the inline-only printf intrinsics has no cold
            // runtime in the self-host contract (it `TypeLoad`-fails on `Vesper.Printf`).
            // So each run-parity pass here is also proof the call lowered native, not
            // cold. Real F#'s `sprintf`/`printf` are the oracle.

            test "E1: `sprintf` on a let-bound annotated format (F# parity)" {
                runParity
                    "E1Sprintf"
                    "let fmt : Vesper.Format<int -> string, unit, string, string> = \"%d!\"\nprintfn \"%s\" (sprintf fmt 42)"
                    (sprintf "%s" (sprintf "%d!" 42))
            }

            test "E1: `printf` on a let-bound TextWriter-format (F# parity)" {
                runParity
                    "E1Printf"
                    "let fmt : Vesper.Format<int -> unit, unit, string, unit> = \"n=%d\"\nprintf fmt 7"
                    (sprintf "n=%d" 7)
            }

            test "E1: multi-hole `printfn` on a let-bound format (F# parity)" {
                runParity
                    "E1Multi"
                    "open Vesper\nlet fmt : Format<int -> string -> unit, unit, string, unit> = \"%d and %s\"\nprintfn fmt 7 \"x\""
                    (sprintf "%d and %s" 7 "x")
            }

            test "E1: a `StringFormat<_>` wildcard printer infers from the specifiers (F# parity)" {
                runParity
                    "E1Wildcard"
                    "open Vesper\nlet fmt : PrintfFormat<_, unit, string, string> = \"%.2f\"\nprintfn \"%s\" (sprintf fmt 3.14159)"
                    (sprintf "%s" (sprintf "%.2f" 3.14159))
            }

            test "E1: an ascribed format literal (`(… : Fmt)`) lowers native (F# parity)" {
                runParity
                    "E1Ascription"
                    "open Vesper\nlet fmt = (\"%d\" : PrintfFormat<int -> string, unit, string, string>)\nprintfn \"%s\" (sprintf fmt 99)"
                    (sprintf "%s" (sprintf "%d" 99))
            }

            test "`printfn \"%s\"` lowers to a single string hole" {
                match soleDecl "printfn \"%s\" \"world\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.String "world", _, _)) ] ->
                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "string", EqArray.empty))
                            "the %s hole types as string"

                        Expect.equal (formatOf hole) None "no .NET format string for %s"
                        Expect.equal (alignmentOf hole) None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`sprintf` lowers to a string-result sink" {
                match soleDecl "sprintf \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, _, ty, _), _) ->
                    Expect.equal
                        ty
                        (TyConst(BuiltinTypes.intrinsicKey "string", EqArray.empty))
                        "sprintf yields a string"
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "interleaved literals and holes keep source order" {
                match soleDecl "printfn \"a=%d b=%s!\" 7 \"x\"" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "a="
                        FormatSeg.Hole(_, TExpr.Const(TConstValue.Int 7, _, _))
                        FormatSeg.Lit " b="
                        FormatSeg.Hole(_, TExpr.Const(TConstValue.String "x", _, _))
                        FormatSeg.Lit "!" ] -> ()
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%%` lowers, collapsing to a single literal percent (P2)" {
                match soleDecl "printfn \"100%%\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "100%" ] -> ()
                    | other -> failtestf "expected one Lit \"100%%\" → \"100%%\" collapse, got: %A" other
                | other -> failtestf "expected a Format node for %%%%, got: %A" other
            }

            test "`%5d` carries an alignment, no flags" {
                match soleDecl "printfn \"%5d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (alignmentOf hole) (Some 5) "width 5 → alignment 5"
                        Expect.equal (formatOf hole) None "no format string for %d"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2f` carries a fixed-point format string" {
                match soleDecl "printfn \"%.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some "F2") "precision 2 → \"F2\""

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "float", EqArray.empty))
                            "the %f hole types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%x` lowers to a Formatted hole with the `\"x\"` .NET format" {
                match soleDecl "printfn \"%x\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%x is a Formatted hole"
                        Expect.equal (formatOf hole) (Some "x") "%x → lowercase \"x\""

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "int", EqArray.empty))
                            "%x types its argument as int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%X` keeps the upper-case base (`\"X\"`)" {
                match soleDecl "printfn \"%X\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal (formatOf hole) (Some "X") "%X → upper-case \"X\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%B` (binary) lowers via the .NET 8 `\"B\"` format" {
                match soleDecl "printfn \"%B\" 5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%B is Formatted"
                        Expect.equal (formatOf hole) (Some "B") "%B → \"B\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%e` carries an exponential format string at default precision 6" {
                match soleDecl "printfn \"%e\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some "e6") "%e → \"e6\""

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "float", EqArray.empty))
                            "%e types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%O` lowers as a Formatted hole typed at its argument" {
                match soleDecl "printfn \"%O\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%O is Formatted"
                        Expect.equal (formatOf hole) None "no .NET format for %O"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "int", EqArray.empty))
                            "%O's hole types as the argument (int)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%b` lowers to a BoolText hole (no format string)" {
                match soleDecl "printfn \"%b\" true" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.BoolText "%b is BoolText"
                        Expect.equal (formatOf hole) None "no .NET format for %b"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%o` lowers to an Octal hole" {
                match soleDecl "printfn \"%o\" 8" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal (kindOf hole) PrintfSpec.HoleKind.Octal "%o is Octal"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%u` lowers to an Unsigned hole" {
                match soleDecl "printfn \"%u\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Unsigned "%u is Unsigned"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-5d` (left-align) carries a negative alignment, no format" {
                match soleDecl "printfn \"%-5d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (alignmentOf hole) (Some -5) "`-` flag → negative alignment"
                        Expect.equal (formatOf hole) None "no format string for %d"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%05d` (zero-pad) carries a width-bearing format, no alignment" {
                match soleDecl "printfn \"%05d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some "D5") "`0` flag → \"D5\""
                        Expect.equal (alignmentOf hole) None "zero-pad uses a format string, not alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08x` (zero-pad hex) carries a width-bearing hex format" {
                match soleDecl "printfn \"%08x\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some "x8") "`0` + width 8 → \"x8\""
                        Expect.equal (alignmentOf hole) None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08o` (zero-pad octal) lowers to an OctalZeroPad hole carrying its width" {
                match soleDecl "printfn \"%08o\" 8" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.OctalZeroPad "%08o is OctalZeroPad"
                        Expect.equal (formatOf hole) None "octal has no .NET format string"
                        Expect.equal (alignmentOf hole) (Some 8) "width rides in the alignment slot"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%05u` (zero-pad unsigned) lowers to an UnsignedZeroPad hole carrying its width" {
                match soleDecl "printfn \"%05u\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.UnsignedZeroPad "%05u is UnsignedZeroPad"
                        Expect.equal (formatOf hole) None "unsigned has no .NET format string"
                        Expect.equal (alignmentOf hole) (Some 5) "width rides in the alignment slot"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%g` carries a compact format string at default precision 6" {
                match soleDecl "printfn \"%g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%g is Formatted"
                        Expect.equal (formatOf hole) (Some "g6") "%g → \"g6\""

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "float", EqArray.empty))
                            "%g types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%G` carries an upper-case compact format string (exponent case on the type char)" {
                match soleDecl "printfn \"%G\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal (formatOf hole) (Some "G6") "%G → \"G6\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.3g` (precision) carries a 3-significant-digit compact format" {
                match soleDecl "printfn \"%.3g\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal (formatOf hole) (Some "g3") "%.3g → \"g3\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%10g` (width, no flag) rides width as a positive alignment" {
                match soleDecl "printfn \"%10g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some "g6") "%10g → \"g6\""
                        Expect.equal (alignmentOf hole) (Some 10) "width → positive alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-10g` (left-align) rides width as a negative alignment" {
                match soleDecl "printfn \"%-10g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some "g6") "%-10g → \"g6\""
                        Expect.equal (alignmentOf hole) (Some -10) "`-` flag → negative alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%010g` (zero-pad compact) lowers to a ZeroPaddedFloat hole over the \"g6\" body" {
                match soleDecl "printfn \"%010g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.ZeroPaddedFloat "%010g is a ZeroPaddedFloat hole"
                        Expect.equal (formatOf hole) (Some "g6") "the \"g6\" body rides in Format"
                        Expect.equal (alignmentOf hole) (Some 10) "the field width rides in Alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // `%+g` / `% g` route through the signed dynamic handler (scientific /
            // compact notation can't ride a .NET section format), so they lower to a
            // Field hole carrying a `ForcedSign` form with the `g` letter — asserted on
            // the classified source, not the section-format `triple` projection.
            test "`%+g` (forced sign on compact) lowers to a ForcedSign field" {
                match soleDecl "printfn \"%+g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        match hole.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(false,
                                                                                                                        PrintfHoleForm.Prec.Const 6,
                                                                                                                        'g',
                                                                                                                        None),
                                                                                  _)) -> ()
                        | other -> failtestf "expected a ForcedSign 'g' field, got: %A" other
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`% g` (space-sign on compact) lowers to a space ForcedSign field" {
                match soleDecl "printfn \"% g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        match hole.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(true,
                                                                                                                        PrintfHoleForm.Prec.Const 6,
                                                                                                                        'g',
                                                                                                                        None),
                                                                                  _)) -> ()
                        | other -> failtestf "expected a space ForcedSign 'g' field, got: %A" other
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+05d` (forced sign + zero-pad) lowers to a zero-padded section format" {
                match soleDecl "printfn \"%+05d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%+05d is a Formatted (section) hole"
                        Expect.equal (formatOf hole) (Some "+0000;-0000") "zero-pad through the sign → digit count w-1"
                        Expect.equal (alignmentOf hole) None "the width rides inside the section format"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+e` (forced sign on exponential) lowers to a ForcedSign field" {
                match soleDecl "printfn \"%+e\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        match hole.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(false,
                                                                                                                        PrintfHoleForm.Prec.Const 6,
                                                                                                                        'e',
                                                                                                                        None),
                                                                                  _)) -> ()
                        | other -> failtestf "expected a ForcedSign 'e' field, got: %A" other
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08e` (zero-pad on exponential) lowers to a ZeroPaddedFloat hole over the \"e6\" body" {
                match soleDecl "printfn \"%08e\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.ZeroPaddedFloat "%08e is a ZeroPaddedFloat hole"
                        Expect.equal (formatOf hole) (Some "e6") "the \"e6\" body rides in Format"
                        Expect.equal (alignmentOf hole) (Some 8) "the field width rides in Alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // ---- `%A` structural format ----
            // The print-width budget rides in the `Alignment` slot.

            test "`%A` of an int lowers to a Structured hole (default width budget)" {
                match soleDecl "printfn \"%A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _, _)) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%A is a Structured hole"
                        Expect.equal (formatOf hole) None "no .NET format string for %A"
                        Expect.equal (alignmentOf hole) None "plain %A → no budget (emit defaults to 80)"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "int", EqArray.empty))
                            "the %A hole types as its argument"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%0A` carries a zero width budget (the flat mode)" {
                match soleDecl "printfn \"%0A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%0A is Structured"
                        Expect.equal (alignmentOf hole) (Some 0) "`0` flag → width budget 0 (never break)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%20A` carries the width as the print budget" {
                match soleDecl "printfn \"%20A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%20A is Structured"
                        Expect.equal (alignmentOf hole) (Some 20) "width 20 → print budget 20"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%A` of a list lowers to a Structured hole (engine-faithful)" {
                match soleDecl "printfn \"%A\" [ 1; 2; 3 ]" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%A of a list is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // A `%A` of an `FSharpOption` no longer forces the whole format cold: the
            // structural engine renders it via the runtime dispatcher's `ToString` tail
            // (its bytes may diverge from F#'s reflective `%A` — accepted), so the hole
            // lowers to a `Structured` node like any other nominal.
            test "`%A` of an FSharpOption lowers to a Structured hole (ToString-degrade)" {
                let tast = analyse "printfn \"%A\" (Some 1)"

                match tast.Decls with
                | EqList [ TDecl.Expression(TExpr.Format(_, segs, _, _), _) ] ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%A of an option is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a single Format-node decl, got: %A" other
            }

            // A `%A` of an arbitrary BCL type (a `System.Guid`) likewise lowers on the
            // engine — the dispatcher's `IFormattable` / `ToString` arm renders it.
            test "`%A` of a BCL type lowers to a Structured hole (ToString-degrade)" {
                match soleDecl "printfn \"%A\" System.Guid.Empty" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%A of a Guid is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2A` (precision) lowers to a Structured hole carrying a size budget" {
                match soleDecl "printfn \"%.2A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%.2A is Structured"
                        Expect.equal (formatOf hole) (Some "2") "precision 2 → size budget '2' in the Format slot"
                        Expect.equal (sizeBudgetOf hole) (Some 2) "and reads back as a size budget"
                        Expect.equal (alignmentOf hole) None "no width budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+A` (non-public) lowers as plain `%A` (no-op in the reflection-free engine)" {
                match soleDecl "printfn \"%+A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%+A is Structured"
                        Expect.equal (alignmentOf hole) None "the `+` flag is ignored (no budget)"
                        Expect.equal (formatOf hole) None "no size budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-A` (left-justify) lowers as plain `%A` (no-op, matching F#)" {
                match soleDecl "printfn \"%-A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%-A is Structured"
                        Expect.equal (alignmentOf hole) None "the `-` flag is ignored (no budget)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%10.2A` carries both a width and a size budget" {
                match soleDecl "printfn \"%10.2A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "%10.2A is Structured"
                        Expect.equal (widthBudgetOf hole) (Some 10) "width 10 → print-width budget"
                        Expect.equal (sizeBudgetOf hole) (Some 2) "precision 2 → print-size budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`% A` (space flag) lowers as plain `%A` (no-op, matching F#)" {
                match soleDecl "printfn \"% A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Structured "% A is Structured"
                        Expect.equal (alignmentOf hole) None "the space flag is ignored (no budget)"
                        Expect.equal (formatOf hole) None "no size budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // The bare `analyse` harness has no assembly name (home = `None`), so a
            // record there is treated external (cold path); `compileSource` (named
            // assembly) lowers it on the engine. The runtime tests below prove that
            // end-to-end.

            test "`printfn \"%s\"` prints the string" { runPrints "PHpString" "printfn \"%s\" \"world\"" "world" }

            test "multi-hole `printfn \"%d and %s\"` prints both in order" {
                runPrints "PHpMulti" "printfn \"%d and %s\" 7 \"x\"" "7 and x"
            }

            test "`printfn \"%.2f\"` prints invariant fixed-point" {
                runPrints "PHpFloat" "printfn \"%.2f\" 3.14159" "3.14"
            }

            test "`printfn \"%5d\"` right-justifies in a width-5 field" {
                let exitCode, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "printfn \"%5d\" 42")

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.TrimEnd()) "   42" "right-justified in a width-5 field (3 leading spaces)"
            }

            test "`printfn \"100%%\"` prints a literal percent" { runPrints "PHpPercent" "printfn \"100%%\"" "100%" }

            test "`sprintf` result feeds another printf" {
                runPrints "PHpSprintf" "printfn \"%s\" (sprintf \"%d!\" 42)" "42!"
            }

            // `fprintf`/`fprintfn` to a real `TextWriter` (`System.Console.Out`,
            // redirected to the capture writer by the harness). The writer is arg 0,
            // the format arg 1 — a `ToWriter` sink, not the FSharp.Core cold path.
            test "fully-applied `fprintf` lowers to a writer-sink Format" {
                match soleDecl "fprintf System.Console.Out \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToWriter(_, false), segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%d → Formatted"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a ToWriter Format node, got: %A" other
            }

            test "`fprintf System.Console.Out \"%d\" 42` writes 42 (no newline)" {
                let exitCode, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "fprintf System.Console.Out \"%d\" 42")

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal output "42" "fprintf writes the value with no trailing newline"
            }

            test "`fprintfn System.Console.Out \"%d\" 42` writes 42 and a trailing newline" {
                let exitCode, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "fprintfn System.Console.Out \"%d\" 42")

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal output "42\n" "fprintfn appends a trailing newline"
            }

            test "multi-hole `fprintf` writes both holes in order" {
                runPrints "PHpFWriterMulti" "fprintf System.Console.Out \"%d and %s\" 7 \"x\"" "7 and x"
            }

            // `bprintf` to a `StringBuilder` — the builder is arg 0, the format arg 1
            // — a `ToBuilder` sink, not the FSharp.Core cold path. No `bprintfn`, so
            // never a trailing newline.
            test "fully-applied `bprintf` lowers to a builder-sink Format" {
                match soleDecl "bprintf (System.Text.StringBuilder()) \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToBuilder _, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%d → Formatted"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a ToBuilder Format node, got: %A" other
            }

            test "`bprintf` appends to the builder (read back via ToString)" {
                let exitCode, output =
                    withPrintfAlc (fun alc ->
                        runDriverInAlc
                            alc
                            "let sb = System.Text.StringBuilder()\nbprintf sb \"%d\" 42\nprintf \"%s\" (sb.ToString())"
                    )

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal output "42" "bprintf appends the value to the builder (no newline)"
            }

            test "multi-hole `bprintf` appends both holes in order" {
                runPrints
                    "PHpBBuilderMulti"
                    "let sb = System.Text.StringBuilder()\nbprintf sb \"%d and %s\" 7 \"x\"\nprintf \"%s\" (sb.ToString())"
                    "7 and x"
            }

            // Track D — `%a` / `%t` callback holes. Freeze lowers each callback hole to
            // an ordinary residue-*string* expr (the callback is a `Vesper.Fun`, applied
            // through the native path — no FSharp.Core): `sprintf` splices the callback's
            // returned string (`cb unit [value]`); the writer/builder families splice a
            // `{ let s = new Scratch() in cb s [value]; s.ToString() }` block. Both
            // backends emit the residue exactly like a `%s` hole. Parity is against the
            // test process's own F# `%a`/`%t` with the identical callback.

            test "sprintf `%a` lowers to a ToString Format; residue is the applied callback (value-carrying)" {
                // `%a` residue = `cb unit value` — a double application (the value is the
                // outer arg), so the residue's head is `App(App(_, _), _)`.
                match soleDecl "sprintf \"%a\" (fun (s: unit) (x: int) -> sprintf \"%d\" x) 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.CallbackHole(_, TExpr.App(TExpr.App _, _, _, _)) ] -> ()
                    | other ->
                        failtestf "expected a CallbackHole whose residue applies the callback to a value, got: %A" other
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "sprintf `%t` lowers to a Format; residue is the value-less callback application" {
                // `%t` residue = `cb unit` — a single application (no value arg).
                match soleDecl "sprintf \"%t\" (fun (s: unit) -> \"hi\")" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.CallbackHole(_, TExpr.App(fn, _, _, _)) ] ->
                        match fn with
                        | TExpr.App _ ->
                            failtestf "expected a single (value-less) application for %%t, got a nested one"
                        | _ -> ()
                    | other ->
                        failtestf
                            "expected a CallbackHole whose residue is a single callback application, got: %A"
                            other
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "`printf \"%a\"` (writer family) lowers to a ToStdOut Format; residue is the scratch block" {
                // A writer-family `%a` residue is the capture-first block Freeze synthesises:
                // `{ let s = new StringWriter() in …; s.ToString() }` — a `Let`.
                match soleDecl "printf \"%a\" (fun (w: System.IO.TextWriter) (x: int) -> fprintf w \"%d\" x) 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut false, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.CallbackHole(_, TExpr.Let _) ] -> ()
                    | other ->
                        failtestf "expected a CallbackHole whose residue is the scratch `let` block, got: %A" other
                | other -> failtestf "expected a ToStdOut Format node, got: %A" other
            }

            test "sprintf `%a` invokes the callback and splices its residue (F# parity)" {
                let expected = sprintf "%a" (fun (s: unit) (x: int) -> sprintf "%d" x) 42

                runParity
                    "PHpSprintfA"
                    "printf \"%s\" (sprintf \"%a\" (fun (s: unit) (x: int) -> sprintf \"%d\" x) 42)"
                    expected
            }

            test "sprintf `%t` invokes the no-value callback (F# parity)" {
                let expected = sprintf "%t" (fun (s: unit) -> "hi")
                runParity "PHpSprintfT" "printf \"%s\" (sprintf \"%t\" (fun (s: unit) -> \"hi\"))" expected
            }

            test "`%a` callback closing over a local captures it (closure-walk, F# parity)" {
                // The callback is a closure over `y`; if the CallbackHole sub-exprs were
                // not walked by free-variable/escape analysis, `y` would go unregistered
                // and this would fault at runtime. Parity value proves the capture works.
                let y = 42
                let expected = sprintf "%a" (fun (s: unit) (x: int) -> sprintf "%d" (x + y)) 5

                runParity
                    "PHpSprintfAClosure"
                    "let y = 42\nprintf \"%s\" (sprintf \"%a\" (fun (s: unit) (x: int) -> sprintf \"%d\" (x + y)) 5)"
                    expected
            }

            test "`printf \"%a\"` writes the callback residue to stdout" {
                runPrints
                    "PHpPrintfA"
                    "printf \"%a\" (fun (w: System.IO.TextWriter) (x: int) -> fprintf w \"%d\" x) 42"
                    "42"
            }

            test "`fprintf` `%a` invokes the callback against the writer sink" {
                runPrints
                    "PHpFprintfA"
                    "fprintf System.Console.Out \"%a\" (fun (w: System.IO.TextWriter) (x: int) -> fprintf w \"%d\" x) 42"
                    "42"
            }

            test "`fprintf` `%t` invokes the no-value callback against the writer sink" {
                runPrints
                    "PHpFprintfT"
                    "fprintf System.Console.Out \"%t\" (fun (w: System.IO.TextWriter) -> fprintf w \"%s\" \"hi\")"
                    "hi"
            }

            test "`bprintf` `%a` invokes the callback against the builder sink (F# parity)" {
                let sb = System.Text.StringBuilder()
                Printf.bprintf sb "%a" (fun (b: System.Text.StringBuilder) (x: int) -> Printf.bprintf b "%d" x) 42
                let expected = sb.ToString()

                runParity
                    "PHpBprintfA"
                    "let sb = System.Text.StringBuilder()\nbprintf sb \"%a\" (fun (b: System.Text.StringBuilder) (x: int) -> bprintf b \"%d\" x) 42\nprintf \"%s\" (sb.ToString())"
                    expected
            }

            // `%A` runtime oracle is the structural spec (copy-pasteable source), not
            // `sprintf "%A"`; small values coincide with F#.
            test "`%A` of a list prints the copy-pasteable literal (slice 4)" {
                runPrints "PHpStructList" "printfn \"%A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            test "`%A` of an int prints the bare value" { runPrints "PHpStructInt" "printfn \"%A\" 42" "42" }

            test "`%A` of a string prints the quoted, escapable literal" {
                runPrints "PHpStructStr" "printfn \"%A\" \"hi\"" "\"hi\""
            }

            // `% A` (space flag) is a pure no-op for `%A`: `GenericToString`
            // (`printf.fs:1085`) never consults the space flag, so output is
            // byte-identical to `%A`. (Note real F#'s *compiler* rejects `% A` at
            // parse time — FS0741 — so `sprintf "% A"` can't be the oracle; XParsec
            // admits it and lowers it as `%A`, whose output the value fixes here.)
            test "`% A` of an int prints the bare value (like `%A`)" {
                runParity "PHpStructSpaceInt" "printfn \"% A\" 42" (sprintf "%A" 42)
            }

            test "`% A` of a list prints the copy-pasteable literal (like `%A`)" {
                runParity "PHpStructSpaceList" "printfn \"% A\" [ 1; 2; 3 ]" (sprintf "%A" [ 1; 2; 3 ])
            }

            test "`%0A` of a list prints flat (fits the budget either way)" {
                runPrints "PHpStructFlat" "printfn \"%0A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            // ---- `%A` flag forms: `%.NA` (PrintSize), `%+A`, `%-A` ----
            // `%.NA` is a global node budget: after N leaves the engine truncates with
            // `...`. `%+A` (non-public fields) and `%-A` (left-justify) are no-ops,
            // identical to plain `%A`.

            test "`%.2A` truncates a list after 2 nodes (PrintSize)" {
                runPrints "PHpStructSize2" "printfn \"%.2A\" [ 1; 2; 3; 4; 5 ]" "[1; 2; ...]"
            }

            test "`%.0A` truncates immediately (zero node budget)" {
                runPrints "PHpStructSize0" "printfn \"%.0A\" [ 1; 2; 3 ]" "..."
            }

            test "`%.3A` truncates a nested list per the shared node budget" {
                runPrints
                    "PHpStructSizeNest"
                    "printfn \"%.3A\" [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]"
                    "[[1; 2]; [3; ...]; ...]"
            }

            test "`%+A` of a record prints the same as plain `%A`" {
                runPrints
                    "PHpStructPlus"
                    "type R = { X: int; Y: string }\nprintfn \"%+A\" { X = 1; Y = \"a\" }"
                    "{ X = 1; Y = \"a\" }"
            }

            test "`%-A` of a list prints the same as plain `%A`" {
                runPrints "PHpStructMinus" "printfn \"%-A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            // ---- `%A` of a record / DU ----
            // The backend synthesises `IStructuralFormattable.Format` on every
            // record / DU. Oracle is the copy-pasteable spec, not `sprintf "%A"`.

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

            test "`%A` of a nullary DU case prints the bare identifier" {
                runPrints "PHpStructDuNullary" "type Opt = | N | S of int\nlet v = N\nprintfn \"%A\" v" "N"
            }

            test "`%A` of a payload DU case prints `Case payload` (no parens)" {
                runPrints "PHpStructDuPayload" "type Opt = | N | S of int\nlet v = S 3\nprintfn \"%A\" v" "S 3"
            }

            // A constructor / parenthesised application written *directly* as the
            // printf argument used to mis-parse: the last union-case field type
            // (`S of int`) swallowed the next line's `printfn` as a postfix type
            // application (`int printfn`), stranding `"%A" (S 3)` as a string applied
            // to the value. Fixed by an offside guard on postfix type suffixes
            // (`TypeParsing.pPostfixType` + the union body's `SeqBlock` context).
            test "`%A` of a DU constructor written directly as the printf arg" {
                runPrints "PHpStructDuDirect" "type Opt = | N | S of int\nprintfn \"%A\" (S 3)" "S 3"
            }

            test "`%A` of a nullary DU case written directly as the printf arg" {
                runPrints "PHpStructDuNullaryDirect" "type Opt = | N | S of int\nprintfn \"%A\" N" "N"
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

            // ---- `%A` of an external Vesper-package union ----
            // A referenced Vesper package's record / DU carries the same synthesised
            // `IStructuralFormattable.Format`, so `%A` of one lowers on the engine via
            // `.Union` / `.Record` resolved shape (not the FSharp.Core cold path).
            test "`%A` of an external Vesper union (Result) renders `Ok 5` on the engine" {
                runsResult "Ok 5" "open Vesper\nlet r : Result<int, string> = Ok 5\nprintfn \"%A\" r"
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

            // `%g`/`%G` parity — the oracle IS F#'s `sprintf "%g"` (byte-for-byte,
            // unlike `%A`): integer-valued, exponent boundaries, trailing-zero
            // stripping, negatives, -0, non-finite, precision/width forms, float32.
            test "`%g` prints an integer-valued float without a point" {
                runParity "PHpG1" "printfn \"%g\" 1.0" (sprintf "%g" 1.0)
            }

            test "`%g` strips trailing zeros" { runParity "PHpGStrip" "printfn \"%g\" 0.5" (sprintf "%g" 0.5) }

            test "`%g` switches to exponent form for large magnitudes" {
                runParity "PHpGBig" "printfn \"%g\" 123456789.0" (sprintf "%g" 123456789.0)
            }

            test "`%g` switches to exponent form for small magnitudes" {
                runParity "PHpGSmall" "printfn \"%g\" 0.00001234" (sprintf "%g" 0.00001234)
            }

            test "`%g` at the 6-sig-digit fixed↔scientific boundary (100000)" {
                runParity "PHpGBound1" "printfn \"%g\" 100000.0" (sprintf "%g" 100000.0)
            }

            test "`%g` at the 6-sig-digit fixed↔scientific boundary (1e+06)" {
                runParity "PHpGBound2" "printfn \"%g\" 1000000.0" (sprintf "%g" 1000000.0)
            }

            test "`%g` prints a negative" {
                runParity "PHpGNeg" "printfn \"%g\" (0.0 - 3.14159)" (sprintf "%g" -3.14159)
            }

            test "`%g` prints negative zero" {
                // IEEE `0.0 - 0.0` is `+0.0`; multiplying `+0.0` by a negative
                // yields a genuine `-0.0` (whose sign `%g` must preserve).
                runParity "PHpGNegZero" "printfn \"%g\" (0.0 * (0.0 - 1.0))" (sprintf "%g" -0.0)
            }

            test "`%g` prints NaN" { runParity "PHpGNaN" "printfn \"%g\" (0.0 / 0.0)" (sprintf "%g" (0.0 / 0.0)) }

            test "`%g` prints Infinity" { runParity "PHpGInf" "printfn \"%g\" (1.0 / 0.0)" (sprintf "%g" (1.0 / 0.0)) }

            test "`%g` prints -Infinity" {
                runParity "PHpGNegInf" "printfn \"%g\" (-1.0 / 0.0)" (sprintf "%g" (-1.0 / 0.0))
            }

            test "`%G` prints an upper-case exponent" {
                runParity "PHpGUpper" "printfn \"%G\" 123456789.0" (sprintf "%G" 123456789.0)
            }

            test "`%.3g` honours the precision" {
                runParity "PHpGPrec" "printfn \"%.3g\" 1234.5" (sprintf "%.3g" 1234.5)
            }

            test "`%10g` right-justifies in a width-10 field" {
                runParity "PHpGWidth" "printfn \"%10g\" 1.5" (sprintf "%10g" 1.5)
            }

            test "`%-10g` left-justifies in a width-10 field" {
                runParity "PHpGLeft" "printfn \"%-10g\" 1.5" (sprintf "%-10g" 1.5)
            }

            test "`%12.3g` combines width and precision" {
                runParity "PHpGWP" "printfn \"%12.3g\" 1234.5" (sprintf "%12.3g" 1234.5)
            }

            test "`%g` on a float32 calls ToString(\"g6\")" {
                runParity "PHpGF32" "printfn \"%g\" 1.5f" (sprintf "%g" 1.5f)
            }

            test "`%-5d` left-justifies in a width-5 field" {
                runParity "PHpLeft" "printfn \"%-5d\" 42" (sprintf "%-5d" 42)
            }

            test "`%05d` zero-pads to width 5" { runParity "PHpZero" "printfn \"%05d\" 42" (sprintf "%05d" 42) }

            test "`%08x` zero-pads hex to width 8" {
                runParity "PHpZeroHex" "printfn \"%08x\" 255" (sprintf "%08x" 255)
            }

            test "`%05u` zero-pads unsigned to width 5" {
                runParity "PHpZeroUns" "printfn \"%05u\" 42" (sprintf "%05u" 42)
            }

            // Overflow: the reinterpreted `uint` is 10 digits, wider than the
            // width — F# neither pads nor truncates.
            test "`%05u` of -1 overflows the width without padding or truncation" {
                runParity "PHpZeroUnsOvf" "printfn \"%05u\" (0 - 1)" (sprintf "%05u" -1)
            }

            test "`%08o` zero-pads octal to width 8" { runParity "PHpZeroOct" "printfn \"%08o\" 8" (sprintf "%08o" 8) }

            // Overflow: the two's-complement octal is 11 digits — no pad, no truncation.
            test "`%08o` of -1 overflows the width without padding or truncation" {
                runParity "PHpZeroOctOvf" "printfn \"%08o\" (0 - 1)" (sprintf "%08o" -1)
            }

            // ---- inert width-less `-`/`0` flags: ignored, plain form (A1a) ----
            test "`%-d` (left, no width) ignores the flag" {
                runParity "PHpLeftNoW" "printfn \"%-d\" 42" (sprintf "%-d" 42)
            }

            test "`%0d` (zero-pad, no width) ignores the flag" {
                runParity "PHpZeroNoW" "printfn \"%0d\" 42" (sprintf "%0d" 42)
            }

            test "`%-x` (left, no width) ignores the flag" {
                runParity "PHpLeftHexNoW" "printfn \"%-x\" 255" (sprintf "%-x" 255)
            }

            test "`%-u` (left, no width) ignores the flag" {
                runParity "PHpLeftUnsNoW" "printfn \"%-u\" 42" (sprintf "%-u" 42)
            }

            test "`%-o` (left, no width) ignores the flag" {
                runParity "PHpLeftOctNoW" "printfn \"%-o\" 8" (sprintf "%-o" 8)
            }

            test "`%-s` (left, no width) ignores the flag" {
                runParity "PHpLeftStrNoW" "printfn \"%-s\" \"hi\"" (sprintf "%-s" "hi")
            }

            test "`%-.2f` (left, no width) ignores the flag" {
                runParity "PHpLeftFixNoW" "printfn \"%-.2f\" 3.14159" (sprintf "%-.2f" 3.14159)
            }

            // ---- left-align wins over zero-pad, non-float (A1a) ----
            test "`%-05d` left-align beats zero-pad" {
                runParity "PHpLeftZeroD" "printfn \"%-05d\" 42" (sprintf "%-05d" 42)
            }

            test "`%-05d` left-align beats zero-pad (negative)" {
                runParity "PHpLeftZeroDNeg" "printfn \"%-05d\" (0 - 7)" (sprintf "%-05d" -7)
            }

            test "`%-05x` left-align beats zero-pad (hex)" {
                runParity "PHpLeftZeroHex" "printfn \"%-05x\" 255" (sprintf "%-05x" 255)
            }

            test "`%-08o` left-align beats zero-pad (octal)" {
                runParity "PHpLeftZeroOct" "printfn \"%-08o\" 8" (sprintf "%-08o" 8)
            }

            test "`%-05u` left-align beats zero-pad (unsigned)" {
                runParity "PHpLeftZeroUns" "printfn \"%-05u\" 42" (sprintf "%-05u" 42)
            }

            // Floats zero-pad on the RIGHT under left-align (`%-05.2f` 3.14159 ⇒
            // `"3.140"`) — the one A2 form needing a dedicated handler.
            test "`%-05.2f` (left + zero-pad float) lowers to a RightZeroPaddedFloat hole" {
                match soleDecl "printfn \"%-05.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal
                            (kindOf hole)
                            PrintfSpec.HoleKind.RightZeroPaddedFloat
                            "%-05.2f is a RightZeroPaddedFloat hole"

                        Expect.equal (formatOf hole) (Some "F2") "the \"F<prec>\" body rides in Format"
                        Expect.equal (alignmentOf hole) (Some 5) "the field width rides in Alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%c` lowers to a Formatted char hole (no format string)" {
                match soleDecl "printfn \"%c\" 'a'" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Char 'a', _, _)) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%c is a Formatted hole"
                        Expect.equal (formatOf hole) None "no .NET format string for %c"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "char", EqArray.empty))
                            "the %c hole types as char"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%M` lowers to a Formatted decimal hole (no format string)" {
                match soleDecl "printfn \"%M\" 3.14M" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Decimal d, _, _)) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%M is a Formatted hole"
                        Expect.equal (formatOf hole) None "no .NET format string for %M"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "decimal", EqArray.empty))
                            "the %M hole types as decimal"

                        Expect.equal d 3.14M "the decimal literal round-trips its value"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2M` (precision) lowers as plain `%M` (F# ignores the precision)" {
                // F# silently ignores a `%M` precision (`%.2M` 3.14159m ⇒ `"3.14159"`),
                // so a literal precision is inert — the plain Verbatim decimal hole.
                match soleDecl "printfn \"%.2M\" 3.14159M" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%.2M is a Formatted hole"
                        Expect.equal (formatOf hole) None "no .NET format string (the precision is ignored)"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "decimal", EqArray.empty))
                            "the %M hole types as decimal"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
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
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%+d is a Formatted hole"
                        Expect.equal (formatOf hole) (Some "+0;-0") "%+d → section format \"+0;-0\""
                        Expect.equal (alignmentOf hole) None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`% d` (space sign) lowers with a leading-space section format" {
                match soleDecl "printfn \"% d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (formatOf hole) (Some " 0;-0") "% d → section format \" 0;-0\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // `%+.Nf` / `% .Nf` no longer project to a .NET *section* format (which rounds
            // half-away): the CLR emit routes the fixed forced-sign float through the signed
            // dynamic handler, which formats a half-to-even `"F<prec>"` body then composes
            // the sign. So the classified form is asserted here; the half-to-even behaviour
            // is proven by the run-parity tests below.
            test "`%+.2f` (forced sign float) classifies as a ForcedSign fixed-float, no zero-pad" {
                match soleDecl "printfn \"%+.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        match hole.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(false,
                                                                                                                        PrintfHoleForm.Prec.Const 2,
                                                                                                                        'f',
                                                                                                                        Option.None),
                                                                                  PrintfHoleForm.Alignment.None)) -> ()
                        | other -> failtestf "expected ForcedSign(+, .2, 'f', no zero-pad), got: %A" other

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "float", EqArray.empty))
                            "%f types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+8.2f` carries its width as a Const alignment on the ForcedSign field" {
                match soleDecl "printfn \"%+8.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        match hole.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(false,
                                                                                                                        PrintfHoleForm.Prec.Const 2,
                                                                                                                        'f',
                                                                                                                        Option.None),
                                                                                  PrintfHoleForm.Alignment.Const 8)) ->
                            ()
                        | other -> failtestf "expected a ForcedSign field with Const 8 alignment, got: %A" other
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

            // Rounds half-to-even, NOT half-away: `0.125` (an exact float) → `+0.12`. The
            // old .NET *section*-format lowering (`"+0.00;-0.00"`) rounded this to `+0.13`;
            // the signed dynamic handler's `"F2"` body matches F#'s half-to-even.
            test "`%+.2f` rounds a float midpoint half-to-even (0.125 → +0.12)" {
                runParity "PHpPlusFEven" "printfn \"%+.2f\" 0.125" (sprintf "%+.2f" 0.125)
            }

            // A negative *float* can't be produced in the codegen subset (no float
            // arithmetic / unary negation), so the float `-` path is covered by the
            // formatter's shared sign-detection, not a run here.

            test "`%+8.2f` composes the forced sign with width-as-alignment" {
                runParity "PHpPlusFAlign" "printfn \"%+8.2f\" 3.14159" (sprintf "%+8.2f" 3.14159)
            }

            // `%+08.2f` / `% 08.2f` — forced sign, then zero-pad AFTER the sign to a total
            // field of 8. Formats a half-to-even `"F2"` body, so the midpoint case proves
            // the rounding as `%+.2f` does. Negatives are out of the codegen subset (as for
            // `%08.2f` / `%+8.2f`).
            test "`%+08.2f` zero-pads a positive float through the forced sign" {
                runParity "PHpPlusZeroF" "printfn \"%+08.2f\" 3.14159" (sprintf "%+08.2f" 3.14159)
            }

            test "`%+08.2f` of a wider value overflows the field without truncation" {
                runParity "PHpPlusZeroFOvf" "printfn \"%+08.2f\" 12345.5" (sprintf "%+08.2f" 12345.5)
            }

            test "`%+08.2f` rounds a float midpoint half-to-even (0.125 → +0000.12)" {
                runParity "PHpPlusZeroFEven" "printfn \"%+08.2f\" 0.125" (sprintf "%+08.2f" 0.125)
            }

            test "`% 08.2f` space-signs and zero-pads a positive float" {
                runParity "PHpSpaceZeroF" "printfn \"% 08.2f\" 3.14159" (sprintf "% 08.2f" 3.14159)
            }

            test "`%08.2f` lowers to a ZeroPaddedFloat hole (format body + width)" {
                match soleDecl "printfn \"%08.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal
                            (kindOf hole)
                            PrintfSpec.HoleKind.ZeroPaddedFloat
                            "%08.2f is a ZeroPaddedFloat hole"

                        Expect.equal (formatOf hole) (Some "F2") "the \"F<prec>\" body rides in Format"
                        Expect.equal (alignmentOf hole) (Some 8) "the field width rides in Alignment"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "float", EqArray.empty))
                            "the %f hole types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08f` defaults the precision to 6 in the format body" {
                match soleDecl "printfn \"%08f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.ZeroPaddedFloat "%08f is ZeroPaddedFloat"
                        Expect.equal (formatOf hole) (Some "F6") "no precision → default 6 → \"F6\""
                        Expect.equal (alignmentOf hole) (Some 8) "width 8"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // A negative *float* can't be produced in the codegen subset, so the
            // sign-then-zeros placement (F# `%08.2f` of `-3.14159` is `"-0003.14"`)
            // is not exercised here; the runs below cover positive / zero /
            // wider-than-field cases.

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

            // ---- byte-exact sign / zero-pad float forms ----

            // `%.NM` — F# ignores the precision on a decimal; the lowered plain `%M`
            // must print the same bytes as `sprintf "%.NM"`.
            test "`%.2M` ignores the precision (prints the full decimal)" {
                runParity "PHpDecPrec" "printfn \"%.2M\" 3.14159M" (sprintf "%.2M" 3.14159M)
            }

            test "`%.0M` ignores a zero precision too" {
                runParity "PHpDecPrec0" "printfn \"%.0M\" 3.14159M" (sprintf "%.0M" 3.14159M)
            }

            // `%+05d` / `% 05d` — the .NET section format zero-pads through the sign.
            test "`%+05d` zero-pads a positive int through the sign" {
                runParity "PHpPlusZeroD" "printfn \"%+05d\" 42" (sprintf "%+05d" 42)
            }

            test "`%+05d` zero-pads a negative int through the sign" {
                runParity "PHpPlusZeroDNeg" "printfn \"%+05d\" (0 - 42)" (sprintf "%+05d" -42)
            }

            test "`% 05d` space-signs and zero-pads" {
                runParity "PHpSpaceZeroD" "printfn \"% 05d\" 42" (sprintf "% 05d" 42)
            }

            // Overflow: the digit count exceeds the field — the section format's min
            // width neither pads nor truncates.
            test "`%+05d` of a wider value overflows the field without truncation" {
                runParity "PHpPlusZeroDOvf" "printfn \"%+05d\" 123456" (sprintf "%+05d" 123456)
            }

            // `%+e` / `%+g` (and uppercase) route through the signed dynamic handler.
            test "`%+e` forces + on a positive exponential" {
                runParity "PHpPlusE" "printfn \"%+e\" 1234.5" (sprintf "%+e" 1234.5)
            }

            test "`%+e` keeps - on a negative exponential" {
                runParity "PHpPlusENeg" "printfn \"%+e\" (0.0 - 1234.5)" (sprintf "%+e" -1234.5)
            }

            test "`% e` space-signs a positive exponential" {
                runParity "PHpSpaceE" "printfn \"% e\" 1234.5" (sprintf "% e" 1234.5)
            }

            test "`%+E` forces + and keeps the upper-case exponent" {
                runParity "PHpPlusEU" "printfn \"%+E\" 1234.5" (sprintf "%+E" 1234.5)
            }

            test "`%+g` forces + on a positive compact" {
                runParity "PHpPlusG" "printfn \"%+g\" 1234.5" (sprintf "%+g" 1234.5)
            }

            test "`% g` space-signs a positive compact" {
                runParity "PHpSpaceG" "printfn \"% g\" 1234.5" (sprintf "% g" 1234.5)
            }

            test "`%+G` forces + on an upper-case compact" {
                runParity "PHpPlusGU" "printfn \"%+G\" 1234.5" (sprintf "%+G" 1234.5)
            }

            test "`%+.2e` honours a literal precision on the signed handler" {
                runParity "PHpPlusEPrec" "printfn \"%+.2e\" 1234.5" (sprintf "%+.2e" 1234.5)
            }

            // `%08e` / `%014e` / `%010g` — zero-pad after any sign over the "e6"/"g6" body.
            test "`%08e` leaves an exponential wider than the field untouched" {
                runParity "PHpZeroE" "printfn \"%08e\" 1234.5" (sprintf "%08e" 1234.5)
            }

            test "`%014e` zero-pads an exponential to width 14" {
                runParity "PHpZeroEW" "printfn \"%014e\" 1234.5" (sprintf "%014e" 1234.5)
            }

            test "`%014E` zero-pads an upper-case exponential" {
                runParity "PHpZeroEU" "printfn \"%014E\" 1234.5" (sprintf "%014E" 1234.5)
            }

            test "`%010g` zero-pads a compact to width 10" {
                runParity "PHpZeroG" "printfn \"%010g\" 1234.5" (sprintf "%010g" 1234.5)
            }

            test "`%010g` zero-pads a negative compact after the sign" {
                runParity "PHpZeroGNeg" "printfn \"%010g\" (0.0 - 1234.5)" (sprintf "%010g" -1234.5)
            }

            test "`%010G` zero-pads an upper-case compact" {
                runParity "PHpZeroGU" "printfn \"%010G\" 1234.5" (sprintf "%010G" 1234.5)
            }

            // `%-05.2f` — left-align + zero-pad fills the RIGHT with zeros.
            test "`%-05.2f` right-zero-pads a positive float" {
                runParity "PHpRZeroF" "printfn \"%-05.2f\" 3.14159" (sprintf "%-05.2f" 3.14159)
            }

            test "`%-05.2f` right-zero-pads a negative float (no pad, already wide)" {
                runParity "PHpRZeroFNeg" "printfn \"%-05.2f\" (0.0 - 3.14159)" (sprintf "%-05.2f" -3.14159)
            }

            test "`%-08.2f` right-zero-pads to width 8" {
                runParity "PHpRZeroFW" "printfn \"%-08.2f\" 3.14159" (sprintf "%-08.2f" 3.14159)
            }

            // Overflow: the body already exceeds the field — no right padding.
            test "`%-05.2f` of a wider value overflows without padding" {
                runParity "PHpRZeroFOvf" "printfn \"%-05.2f\" 12345.6" (sprintf "%-05.2f" 12345.6)
            }

            test "`$\"x={1}\"` lowers to a ToString Format node" {
                match soleDecl "$\"x={1}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, ty, _), _) ->
                    Expect.equal
                        ty
                        (TyConst(BuiltinTypes.intrinsicKey "string", EqArray.empty))
                        "interpolation yields a string"

                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "x="; FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 1, _, _)) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "a plain hole is Formatted"
                        Expect.equal (formatOf hole) None "no format clause"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "int", EqArray.empty))
                            "the hole types as int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "`$\"{255:X}\"` carries the :format clause as the .NET format" {
                match soleDecl "$\"{255:X}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] -> Expect.equal (formatOf hole) (Some "X") "clause :X → \"X\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`$\"%d{x}\"` typed interpolation lowers via the printf spec" {
                match soleDecl "$\"%d{7}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, _) ] ->
                        Expect.equal (kindOf hole) PrintfSpec.HoleKind.Formatted "%d hole is Formatted"

                        Expect.equal
                            hole.Ty
                            (TyConst(BuiltinTypes.intrinsicKey "int", EqArray.empty))
                            "%d constrains the hole to int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "a plain (non-interpolated) string is still a Const, not a Format" {
                match soleDecl "\"hello\"" with
                | TDecl.Expression(TExpr.Const(TConstValue.String "hello", _, _), _) -> ()
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

            // ---- Star width (`%*d`, `%-*d`, `%*A`): native lowering ----
            // A star *width* consumes a leading runtime `int` (evaluated before the
            // value); the padding forms feed it — guarded, then negated for `-` — to
            // the signed-alignment handler members, and `%*A` feeds it (clamped) as the
            // structural print-width budget. Parity oracle IS the test process's own
            // `sprintf`. `%-*A` / `%+*A` also lower here (the `-`/`+` flags are no-ops on
            // `%A`). Star *precision* and the runtime-width zero-pad forms (`%0*d`, `%0*A`)
            // are diagnosed residuals — see the front-end PrintfTests.

            test "`%*d` freezes to a Format node with a DynHole (width only) segment" {
                match soleDecl "printfn \"%*d\" 5 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Width, d.Precision, d.Value with
                        | ValueSome(TExpr.Const(TConstValue.Int 5, _, _)),
                          ValueNone,
                          TExpr.Const(TConstValue.Int 42, _, _) ->
                            match d.Spec.Source with
                            | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.Verbatim,
                                                                                      PrintfHoleForm.Alignment.Star false)) ->
                                ()
                            | other -> failtestf "expected a Verbatim / Star(false) field, got: %A" other
                        | other -> failtestf "expected width 5, no precision, value 42, got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-*d` carries the `-` flag as Star(leftJustify = true)" {
                match soleDecl "printfn \"%-*d\" 5 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Spec.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(_, PrintfHoleForm.Alignment.Star true)) ->
                            ()
                        | other -> failtestf "expected Star(true), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%*A` freezes to a DynHole over a PercentA(Star) spec" {
                match soleDecl "printfn \"%*A\" 1 [1; 2; 3]" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueSome _,
                          ValueNone,
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(PrintfHoleForm.PrintWidth.Star, _)) ->
                            ()
                        | other -> failtestf "expected width-only DynHole over PercentA(Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // `%-*A` / `%+*A` — the `-`/`+` flags are pure no-ops on `%A`, so a flagged
            // star-`%A` lowers to the SAME `PercentA(Star)` hole as a bare `%*A` and
            // renders byte-identically. The oracle is the process's own `sprintf`.
            test "`%-*A` (no-op flag) freezes to the same PercentA(Star) hole as `%*A`" {
                match soleDecl "printfn \"%-*A\" 1 [1; 2; 3]" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Spec.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(PrintfHoleForm.PrintWidth.Star, _)) ->
                            ()
                        | other -> failtestf "expected PercentA(Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-*A` renders identically to `%*A` (F# parity)" {
                runParity "PHpLeftStarA" "printfn \"%-*A\" 20 [1; 2; 3]" (sprintf "%-*A" 20 [ 1; 2; 3 ])
            }

            // ---- Star precision (`%.*f`, `%*.*f`, `%.*e`, `%.*g`, `%+.*f`, `%.*A`) ----
            // A star *precision* consumes a leading runtime `int` (after any star width,
            // before the value). The float forms build the .NET format string in-handler
            // from the source type char + the runtime precision, reproducing FSharp.Core's
            // custom-format fallback for garbage precisions; the two-star path clamps to
            // 0..99 (the prec-only path uses the raw precision — a load-bearing asymmetry).
            // `%.*A` feeds the runtime size (`PrintSize`) budget. Oracle IS the process's
            // own `sprintf`. The runtime-width zero-pad `%0*.Nf` and the `0`-flag `%0*A`
            // are diagnosed residuals (see the front-end PrintfTests).

            test "`%.*f` freezes to a DynHole (precision only) over Fixed(Star)" {
                match soleDecl "printfn \"%.*f\" 3 3.14" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueNone,
                          ValueSome(TExpr.Const(TConstValue.Int 3, _, _)),
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.Fixed PrintfHoleForm.Prec.Star,
                                                                                  PrintfHoleForm.Alignment.None)) -> ()
                        | other -> failtestf "expected precision-only DynHole over Fixed(Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%*.*f` freezes to a DynHole with both width and precision" {
                match soleDecl "printfn \"%*.*f\" 8 3 3.14" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueSome(TExpr.Const(TConstValue.Int 8, _, _)),
                          ValueSome(TExpr.Const(TConstValue.Int 3, _, _)),
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.Fixed PrintfHoleForm.Prec.Star,
                                                                                  PrintfHoleForm.Alignment.Star false)) ->
                            ()
                        | other -> failtestf "expected both-dim DynHole over Fixed(Star)/Star, got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.*A` freezes to a DynHole (precision only) over PercentA(_, Star)" {
                match soleDecl "printfn \"%.*A\" 2 [1; 2; 3]" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.DynHole d ] ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueNone,
                          ValueSome _,
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(_, PrintfHoleForm.PrintSize.Star)) ->
                            ()
                        | other -> failtestf "expected precision-only DynHole over PercentA(_, Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // Padding forms — every specifier a star width lands on. The oracle pins
            // byte-for-byte parity with F#, native.
            test "`%*d` right-justifies to a runtime width" {
                runParity "PHpStarD" "printfn \"%*d\" 5 42" (sprintf "%*d" 5 42)
            }

            test "`%-*d` left-justifies to a runtime width" {
                runParity "PHpStarLeft" "printfn \"%-*d\" 5 42" (sprintf "%-*d" 5 42)
            }

            test "`%+*d` forces a sign inside a runtime width" {
                runParity "PHpStarPlus" "printfn \"%+*d\" 5 42" (sprintf "%+*d" 5 42)
            }

            test "`% *d` forces a leading space inside a runtime width" {
                runParity "PHpStarSpace" "printfn \"% *d\" 5 42" (sprintf "% *d" 5 42)
            }

            test "`%*s` right-justifies a string to a runtime width" {
                runParity "PHpStarS" "printfn \"%*s\" 6 \"hi\"" (sprintf "%*s" 6 "hi")
            }

            test "`%*c` right-justifies a char to a runtime width" {
                runParity "PHpStarC" "printfn \"%*c\" 4 'a'" (sprintf "%*c" 4 'a')
            }

            test "`%*u` right-justifies unsigned to a runtime width" {
                runParity "PHpStarU" "printfn \"%*u\" 6 42" (sprintf "%*u" 6 42)
            }

            test "`%*x` right-justifies hex to a runtime width" {
                runParity "PHpStarX" "printfn \"%*x\" 6 255" (sprintf "%*x" 6 255)
            }

            test "`%*o` right-justifies octal to a runtime width" {
                runParity "PHpStarO" "printfn \"%*o\" 6 8" (sprintf "%*o" 6 8)
            }

            test "`%*B` right-justifies binary to a runtime width" {
                runParity "PHpStarB" "printfn \"%*B\" 8 5" (sprintf "%*B" 8 5)
            }

            test "`%*b` right-justifies a bool to a runtime width" {
                runParity "PHpStarBool" "printfn \"%*b\" 8 true" (sprintf "%*b" 8 true)
            }

            test "`%*f` right-justifies a float to a runtime width" {
                runParity "PHpStarF" "printfn \"%*f\" 12 3.14159" (sprintf "%*f" 12 3.14159)
            }

            test "`%*.2f` combines a star width with a literal precision" {
                runParity "PHpStarFP" "printfn \"%*.2f\" 10 3.14159" (sprintf "%*.2f" 10 3.14159)
            }

            // Width edge cases: wider-than-value (pad), narrower-than-value (no pad),
            // and width 0 (no pad).
            test "`%*d` with a width smaller than the value does not truncate" {
                runParity "PHpStarNarrow" "printfn \"%*d\" 2 12345" (sprintf "%*d" 2 12345)
            }

            test "`%*d` with width 0 pads nothing" {
                runParity "PHpStarZeroW" "printfn \"%*d\" 0 42" (sprintf "%*d" 0 42)
            }

            test "`%-*s` with a width smaller than the value does not truncate" {
                runParity "PHpStarLeftNarrow" "printfn \"%-*s\" 2 \"hello\"" (sprintf "%-*s" 2 "hello")
            }

            // `%*A`: the star feeds the structural print-width budget. A tight budget
            // forces per-element breaks; a wide one keeps it on one line; a negative
            // budget renders flat (F# clamps rather than throwing).
            test "`%*A` with a tight width forces the list to break across lines" {
                // The structural engine's multi-line break regime diverges from F#'s by
                // design (only flat/small values are byte-identical), so the star-budget
                // effect is asserted behaviourally: a width-1 budget on a 3-element list
                // must span lines, whereas the wide-width run (below) stays flat.
                let _, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "printfn \"%*A\" 1 [1; 2; 3]")

                let body = output.TrimEnd('\r', '\n')
                Expect.isTrue (body.Contains '\n') (sprintf "width 1 breaks the list across lines (got: %A)" body)
            }

            test "`%*A` with a wide width stays flat and matches F#" {
                runParity "PHpStarAWide" "printfn \"%*A\" 80 [1; 2; 3]" (sprintf "%*A" 80 [ 1; 2; 3 ])
            }

            test "`%*A` with a negative width renders flat without throwing" {
                runParity "PHpStarANeg" "printfn \"%*A\" (0 - 1) [1; 2; 3]" (sprintf "%*A" -1 [ 1; 2; 3 ])
            }

            // Star precision, native. Oracle IS the process's own `sprintf`.
            test "`%.*f` renders a runtime precision" {
                runParity "PHpPrecF" "printfn \"%.*f\" 2 3.14159" (sprintf "%.*f" 2 3.14159)
            }

            test "`%*.*f` renders a runtime width and precision" {
                runParity "PHpPrecWF" "printfn \"%*.*f\" 12 1 123.456" (sprintf "%*.*f" 12 1 123.456)
            }

            test "`%.*e` renders a runtime precision (scientific)" {
                runParity "PHpPrecE" "printfn \"%.*e\" 3 31415.9" (sprintf "%.*e" 3 31415.9)
            }

            test "`%.*E` renders a runtime precision (upper scientific)" {
                runParity "PHpPrecEU" "printfn \"%.*E\" 3 31415.9" (sprintf "%.*E" 3 31415.9)
            }

            test "`%.*g` renders a runtime precision (compact)" {
                runParity "PHpPrecG" "printfn \"%.*g\" 4 31415.9" (sprintf "%.*g" 4 31415.9)
            }

            test "`%+.*f` forces a sign around a runtime precision" {
                runParity "PHpPrecPlus" "printfn \"%+.*f\" 3 3.14159" (sprintf "%+.*f" 3 3.14159)
            }

            test "`%+.*f` keeps a negative value's own sign" {
                runParity "PHpPrecPlusNeg" "printfn \"%+.*f\" 3 (0.0 - 3.14159)" (sprintf "%+.*f" 3 -3.14159)
            }

            test "`%+*.*f` forces a sign inside a runtime width and precision" {
                runParity "PHpPrecWPlus" "printfn \"%+*.*f\" 12 2 3.14159" (sprintf "%+*.*f" 12 2 3.14159)
            }

            test "`%.*A` feeds the runtime structural size budget" {
                // A tight size budget elides nodes as `...`; the value has more nodes than
                // the budget, so the output must contain the ellipsis (behavioural, like
                // the `%*A` width tests — the multi-line regime diverges by design).
                let _, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "printfn \"%.*A\" 1 [1; 2; 3; 4; 5]")

                Expect.isTrue
                    (output.Contains "...")
                    (sprintf "a size-1 budget elides later nodes as ... (got: %A)" output)
            }

            test "`%.*A` with a generous size budget matches F#" {
                runParity "PHpPrecAWide" "printfn \"%.*A\" 100 [1; 2; 3]" (sprintf "%.*A" 100 [ 1; 2; 3 ])
            }

            // The load-bearing clamp asymmetry (`printf.fs:632` vs `:649-657`), verified in
            // fsi: the two-star path clamps the precision to 0..99, the prec-only path uses
            // it raw (falling back to .NET custom-format interpretation for garbage).
            test "QUIRK: `%.*f` with precision -1 echoes the custom-format literal \"f-1\"" {
                runParity "PHpQuirkNeg" "printfn \"%.*f\" (0 - 1) 3.14" (sprintf "%.*f" -1 3.14)
            }

            test "QUIRK: `%*.*f` with precision -1 clamps to 0 (width 1 ⇒ \"3\")" {
                runParity "PHpQuirkWNeg" "printfn \"%*.*f\" 1 (0 - 1) 3.14" (sprintf "%*.*f" 1 -1 3.14)
            }

            test "QUIRK: `%.*f` precision 105 renders 105 fraction digits (raw)" {
                runParity "PHpQuirk105" "printfn \"%.*f\" 105 3.14" (sprintf "%.*f" 105 3.14)
            }

            test "QUIRK: `%*.*f` precision 105 clamps to 99 digits" {
                runParity "PHpQuirkW105" "printfn \"%*.*f\" 1 105 3.14" (sprintf "%*.*f" 1 105 3.14)
            }

            // Evaluation order for the dynamic dims: F# evaluates width, then precision,
            // then the value (curried application order). Each thunk prints a marker, so
            // the emitted spill order must produce `WPV` before the formatted result.
            test "`%*.*f` evaluates width, then precision, then value" {
                let src =
                    "let w () =\n    printf \"W\"\n    8\n"
                    + "let p () =\n    printf \"P\"\n    2\n"
                    + "let v () =\n    printf \"V\"\n    3.14159\n"
                    + "printfn \"%*.*f\" (w ()) (p ()) (v ())"

                runPrints "PHpPrecOrder" src ("WPV" + sprintf "%*.*f" 8 2 3.14159)
            }

            // Cold residual: the runtime-width zero-pad `%0*d` has no native handler and
            // no FSharp.Core fallback once the family lowers natively, so the gate
            // diagnoses it (naming the specifier) rather than routing it cold.
            test "`%0*d` (runtime-width zero-pad) is diagnosed, not lowered" {
                failsWith "%0*d" "printfn \"%0*d\" 5 42"
            }

            // The regression test for the width-before-value spill: both the width and
            // the value expression print a marker before returning. F# evaluates the
            // width argument first (curried application order), so `W` must precede `V`;
            // pushing the width inline (after the value) would flip them.
            test "`%*d` evaluates the width argument before the value" {
                let src =
                    "let w () =\n    printf \"W\"\n    5\n"
                    + "let v () =\n    printf \"V\"\n    42\n"
                    + "printfn \"%*d\" (w ()) (v ())"

                runPrints "PHpStarOrder" src ("WV" + sprintf "%*d" 5 42)
            }

            test "a negative star width throws ArgumentOutOfRangeException(\"totalWidth\"), matching F#" {
                Expect.equal
                    (entryPointThrew "printfn \"%*d\" (0 - 5) 42")
                    (Some("System.ArgumentOutOfRangeException", Some "totalWidth"))
                    "native negative-width throw matches F#'s exception type + ParamName"
            }
        ]
