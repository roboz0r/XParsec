module XParsec.FSharp.Codegen.Clr.Tests.PrintfHappyPathTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

/// A `Star` width (a runtime `%*d`) has no static value, so it projects to `None`.
let private alignToOpt (a: PrintfHoleForm.Alignment) : int option =
    match a with
    | PrintfHoleForm.Alignment.None -> None
    | PrintfHoleForm.Alignment.Const n -> Some n
    | PrintfHoleForm.Alignment.Star _ -> None

/// The `Vesper.Formatter` call a non-`%A` hole lowers to, rebuilt from its classified
/// `HoleForm`. The per-specifier assertions are written against this projection.
let private callOf (hole: HoleSpecG<'ty, 'tok>) : ClrHoleFormat.HoleCall =
    match hole.Source with
    | HoleSpecSource.RawFormat fmt -> ClrHoleFormat.HoleCall.Formatted(fmt, PrintfHoleForm.Alignment.None)
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(fmt, align)) -> ClrHoleFormat.holeCall fmt align
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA _) ->
        failwith "callOf: %A lowers to AppendStructured; read its budgets instead"
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Callback _) ->
        failwith "callOf: a callback hole has no field projection"

/// The .NET format string an `AppendFormatted` hole carries. A member that composes its own
/// text holds its body in the `HoleCall` payload, which `callOf` reads.
let private formatOf (hole: HoleSpecG<'ty, 'tok>) : string option =
    match callOf hole with
    | ClrHoleFormat.HoleCall.Formatted(fmt, _) -> fmt
    | other -> failtestf "formatOf: %A composes its own text" other

/// The field alignment an `AppendFormatted` hole carries. A zero-pad member takes a total
/// WIDTH instead, which its `HoleCall` payload holds.
let private alignmentOf (hole: HoleSpecG<'ty, 'tok>) : int option =
    match callOf hole with
    | ClrHoleFormat.HoleCall.Formatted(_, align) -> alignToOpt align
    | other -> failtestf "alignmentOf: %A carries a width, not an alignment" other

/// The `%A` budgets a `PercentA` hole carries. `Star` (`%*A` / `%.*A`) resolves at run
/// time, so it reads as `None`.
let private percentABudgets (hole: HoleSpecG<'ty, 'tok>) : int option * int option =
    match hole.Source with
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(width, size)) ->
        let w =
            match width with
            | PrintfHoleForm.PrintWidth.Default -> None
            | PrintfHoleForm.PrintWidth.Never -> Some 0
            | PrintfHoleForm.PrintWidth.Cols n -> Some n
            | PrintfHoleForm.PrintWidth.Star -> None

        let s =
            match size with
            | PrintfHoleForm.PrintSize.Default -> None
            | PrintfHoleForm.PrintSize.Cols n -> Some n
            | PrintfHoleForm.PrintSize.Star -> None

        w, s
    | other -> failtestf "percentABudgets: %A is not a %%A hole" other

/// True when the hole classified as `%A`, which lowers to `AppendStructured` rather than
/// through the field projection.
let private isStructured (hole: HoleSpecG<'ty, 'tok>) : bool =
    match hole.Source with
    | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA _) -> true
    | _ -> false

let private widthBudgetOf hole = fst (percentABudgets hole)

let private sizeBudgetOf hole = snd (percentABudgets hole)

// Fully-applied literal printf lowers to the `Vesper.Formatter` write-through handler.
// There is no FSharp.Core cold path: an un-lowerable form is a `Severity.Error` at the
// gate.

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

/// Trims only the trailing newline, so leading / embedded alignment spaces survive.
/// `expected` comes from the test process's own `sprintf`, so parity is byte-for-byte.
let private runParity (name: string) (src: string) (expected: string) =
    let exitCode, output = withPrintfAlc (fun alc -> runDriverInAlc alc src)
    Expect.equal exitCode 0 (sprintf "Main returns 0 for: %s" src)
    Expect.equal (output.TrimEnd('\r', '\n')) expected (sprintf "%s == F# parity" src)

/// The deepest exception the driver's entry point throws, with its `ParamName`. The
/// asserting run helpers turn a throw into a test failure, so a test that must OBSERVE
/// F#'s throw (`PadLeft` rejects a negative `%*d` width as its `totalWidth`) invokes directly.
let private entryPointThrew (src: string) : (string * string option) option =
    withPrintfAlc (fun alc ->
        let artifact = compileSource "PHpStarThrow" src
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
            test "`printfn \"hi\"` lowers to a single-literal Format node and prints \"hi\"" {
                match soleDecl "printfn \"hi\"" with
                | TDecl.Expression(TExpr.Format(sink, segs, _, _), _) ->
                    Expect.equal sink (FormatSink.ToStdOut true) "printfn → stdout with newline"

                    match segs with
                    | EqOne(FormatSeg.Lit "hi") -> ()
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other

                runPrints "PHpHi" "printfn \"hi\"" "hi"
            }

            // ---- a format literal bound to a name / ascribed (const-prop) ----
            // Not a syntactic literal AT the call site, but the bound literal is
            // recovered and lowered natively, so a format *value* has no cold runtime.

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

            // ---- a format in a real signature position ----
            // A let-bound format literal is const-propagated and its binding folded out, so
            // these three are the only places `Vesper.PrintfFormat`4` survives into metadata,
            // where it resolves as an ordinary external type out of `Vesper.Printf`.
            //
            // PENDING: the TYPE resolves, but the CALL cannot be emitted. `sprintf` is a
            // front-end intrinsic with no runtime behind it, so a format that arrives as a
            // VALUE rather than a literal has nothing to apply and codegen throws
            // "no call recipe for external 'sprintf'". A printf runtime is what closes these.

            ptest "a format-typed PARAMETER emits and runs (F# parity)" {
                runParity
                    "FmtParam"
                    "open Vesper\nlet f (fmt: PrintfFormat<int -> string, unit, string, string>) = sprintf fmt 1\nprintfn \"%s\" (f \"%d\")"
                    (sprintf "%d" 1)
            }

            ptest "a format-typed RETURN emits and runs (F# parity)" {
                runParity
                    "FmtReturn"
                    "open Vesper\nlet mk () : PrintfFormat<int -> string, unit, string, string> = \"%d\"\nprintfn \"%s\" (sprintf (mk ()) 5)"
                    (sprintf "%d" 5)
            }

            ptest "a format-typed record FIELD emits and runs (F# parity)" {
                runParity
                    "FmtField"
                    "open Vesper\ntype R = { F: PrintfFormat<int -> string, unit, string, string> }\nlet r = { F = \"%d\" }\nprintfn \"%s\" (sprintf r.F 5)"
                    (sprintf "%d" 5)
            }

            test "`printfn \"%s\"` lowers to a single string hole" {
                match soleDecl "printfn \"%s\" \"world\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, TExpr.Const(TConstValue.String "world", _, _))) ->
                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.stringKey, EqArray.empty))
                            "the %s hole types as string"

                        Expect.equal (formatOf hole) None "no .NET format string for %s"
                        Expect.equal (alignmentOf hole) None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`sprintf` lowers to a string-result sink" {
                match soleDecl "sprintf \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, _, ty, _), _) ->
                    Expect.equal ty (TyConst(RuntimeNames.stringKey, EqArray.empty)) "sprintf yields a string"
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "interleaved literals and holes keep source order" {
                match soleDecl "printfn \"a=%d b=%s!\" 7 \"x\"" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqList [ FormatSeg.Lit "a="
                               FormatSeg.Hole(_, TExpr.Const(TConstValue.Integral(IntKind.Int32, 7L), _, _))
                               FormatSeg.Lit " b="
                               FormatSeg.Hole(_, TExpr.Const(TConstValue.String "x", _, _))
                               FormatSeg.Lit "!" ] -> ()
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%%` lowers, collapsing to a single literal percent" {
                match soleDecl "printfn \"100%%\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Lit "100%") -> ()
                    | other -> failtestf "expected one Lit \"100%%\" → \"100%%\" collapse, got: %A" other
                | other -> failtestf "expected a Format node for %%%%, got: %A" other
            }

            test "`%5d` carries an alignment, no flags" {
                match soleDecl "printfn \"%5d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (alignmentOf hole) (Some 5) "width 5 → alignment 5"
                        Expect.equal (formatOf hole) None "no format string for %d"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2f` carries a fixed-point format string" {
                match soleDecl "printfn \"%.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "F2") "precision 2 → \"F2\""

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.floatKey, EqArray.empty))
                            "the %f hole types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%x` lowers to a Formatted hole with the `\"x\"` .NET format" {
                match soleDecl "printfn \"%x\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "x") "%x → AppendFormatted at lowercase \"x\""

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.intKey, EqArray.empty))
                            "%x types its argument as int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%X` keeps the upper-case base (`\"X\"`)" {
                match soleDecl "printfn \"%X\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) -> Expect.equal (formatOf hole) (Some "X") "%X → upper-case \"X\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%B` (binary) lowers via the .NET 8 `\"B\"` format" {
                match soleDecl "printfn \"%B\" 5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "B") "%B → AppendFormatted at \"B\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%e` carries an exponential format string at default precision 6" {
                match soleDecl "printfn \"%e\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "e6") "%e → \"e6\""

                        Expect.equal hole.Ty (TyConst(RuntimeNames.floatKey, EqArray.empty)) "%e types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%O` lowers as a Formatted hole typed at its argument" {
                match soleDecl "printfn \"%O\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) None "%O → AppendFormatted with no .NET format"

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.intKey, EqArray.empty))
                            "%O's hole types as the argument (int)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%b` lowers to a BoolText hole (no format string)" {
                match soleDecl "printfn \"%b\" true" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.BoolText PrintfHoleForm.Alignment.None)
                            "%b → AppendBool, unaligned"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%o` lowers to an Octal hole" {
                match soleDecl "printfn \"%o\" 8" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.Octal PrintfHoleForm.Alignment.None)
                            "%o → AppendOctal, unaligned"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%u` lowers to an Unsigned hole" {
                match soleDecl "printfn \"%u\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.Unsigned PrintfHoleForm.Alignment.None)
                            "%u → AppendUnsigned, unaligned"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-5d` (left-align) carries a negative alignment, no format" {
                match soleDecl "printfn \"%-5d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (alignmentOf hole) (Some -5) "`-` flag → negative alignment"
                        Expect.equal (formatOf hole) None "no format string for %d"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%05d` (zero-pad) carries a width-bearing format, no alignment" {
                match soleDecl "printfn \"%05d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "D5") "`0` flag → \"D5\""
                        Expect.equal (alignmentOf hole) None "zero-pad uses a format string, not alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08x` (zero-pad hex) carries a width-bearing hex format" {
                match soleDecl "printfn \"%08x\" 255" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "x8") "`0` + width 8 → \"x8\""
                        Expect.equal (alignmentOf hole) None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08o` (zero-pad octal) lowers to an OctalZeroPad hole carrying its width" {
                match soleDecl "printfn \"%08o\" 8" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.OctalZeroPad 8)
                            "%08o → AppendZeroPaddedOctal at a total width of 8"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%05u` (zero-pad unsigned) lowers to an UnsignedZeroPad hole carrying its width" {
                match soleDecl "printfn \"%05u\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.UnsignedZeroPad 5)
                            "%05u → AppendZeroPaddedUnsigned at a total width of 5"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%g` carries a compact format string at default precision 6" {
                match soleDecl "printfn \"%g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "g6") "%g → AppendFormatted at \"g6\""

                        Expect.equal hole.Ty (TyConst(RuntimeNames.floatKey, EqArray.empty)) "%g types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%G` carries an upper-case compact format string (exponent case on the type char)" {
                match soleDecl "printfn \"%G\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) -> Expect.equal (formatOf hole) (Some "G6") "%G → \"G6\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.3g` (precision) carries a 3-significant-digit compact format" {
                match soleDecl "printfn \"%.3g\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) -> Expect.equal (formatOf hole) (Some "g3") "%.3g → \"g3\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%10g` (width, no flag) rides width as a positive alignment" {
                match soleDecl "printfn \"%10g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "g6") "%10g → \"g6\""
                        Expect.equal (alignmentOf hole) (Some 10) "width → positive alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-10g` (left-align) rides width as a negative alignment" {
                match soleDecl "printfn \"%-10g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "g6") "%-10g → \"g6\""
                        Expect.equal (alignmentOf hole) (Some -10) "`-` flag → negative alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%010g` (zero-pad compact) lowers to a ZeroPaddedFloat hole over the \"g6\" body" {
                match soleDecl "printfn \"%010g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.ZeroPaddedFloat("g6", 10))
                            "%010g → AppendZeroPaddedFloat over a \"g6\" body, total width 10"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // Scientific / compact notation can't ride a .NET section format, so `%+g` /
            // `% g` lower to a Field hole carrying a `ForcedSign` form with the `g` letter.
            // The assertion reads the classified source, not the `triple` projection.
            test "`%+g` (forced sign on compact) lowers to a ForcedSign field" {
                match soleDecl "printfn \"%+g\" 1.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
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
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
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
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (formatOf hole)
                            (Some "+0000;-0000")
                            "%+05d → AppendFormatted; zero-pad through the sign → digit count w-1"

                        Expect.equal (alignmentOf hole) None "the width rides inside the section format"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+e` (forced sign on exponential) lowers to a ForcedSign field" {
                match soleDecl "printfn \"%+e\" 1234.5" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
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
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.ZeroPaddedFloat("e6", 8))
                            "%08e → AppendZeroPaddedFloat over an \"e6\" body, total width 8"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // ---- `%A` structural format ----
            // The print-width budget rides in the `Alignment` slot.

            test "`%A` of an int lowers to a Structured hole (default width budget)" {
                match soleDecl "printfn \"%A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, TExpr.Const(TConstValue.Integral(IntKind.Int32, 42L), _, _))) ->
                        Expect.isTrue (isStructured hole) "%A is a Structured hole"
                        Expect.equal (widthBudgetOf hole) None "plain %A → no width budget (emit defaults to 80)"
                        Expect.equal (sizeBudgetOf hole) None "and no size budget"

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.intKey, EqArray.empty))
                            "the %A hole types as its argument"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%0A` carries a zero width budget (the flat mode)" {
                match soleDecl "printfn \"%0A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%0A is Structured"
                        Expect.equal (widthBudgetOf hole) (Some 0) "`0` flag → width budget 0 (never break)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%20A` carries the width as the print budget" {
                match soleDecl "printfn \"%20A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%20A is Structured"
                        Expect.equal (widthBudgetOf hole) (Some 20) "width 20 → print budget 20"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%A` of a list lowers to a Structured hole (engine-faithful)" {
                match soleDecl "printfn \"%A\" [ 1; 2; 3 ]" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) -> Expect.isTrue (isStructured hole) "%A of a list is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // An `FSharpOption` renders via the runtime dispatcher's `ToString` fallback, so
            // the hole is a `Structured` node like any other nominal (bytes may diverge
            // from F#'s reflective `%A`, which is accepted).
            test "`%A` of an FSharpOption lowers to a Structured hole (ToString-degrade)" {
                let tast = analyse "printfn \"%A\" (Some 1)"

                match tast.Decls with
                | EqList [ TDecl.Expression(TExpr.Format(_, segs, _, _), _) ] ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%A of an option is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a single Format-node decl, got: %A" other
            }

            // An arbitrary BCL type (`System.Guid`) renders through the dispatcher's
            // `IFormattable` / `ToString` arm.
            test "`%A` of a BCL type lowers to a Structured hole (ToString-degrade)" {
                match soleDecl "printfn \"%A\" System.Guid.Empty" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) -> Expect.isTrue (isStructured hole) "%A of a Guid is Structured"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2A` (precision) lowers to a Structured hole carrying a size budget" {
                match soleDecl "printfn \"%.2A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%.2A is Structured"
                        Expect.equal (sizeBudgetOf hole) (Some 2) "precision 2 → print-size budget 2"
                        Expect.equal (widthBudgetOf hole) None "no width budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+A` (non-public) lowers as plain `%A` (no-op in the reflection-free engine)" {
                match soleDecl "printfn \"%+A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%+A is Structured"
                        Expect.equal (widthBudgetOf hole) None "the `+` flag is ignored (no budget)"
                        Expect.equal (sizeBudgetOf hole) None "no size budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-A` (left-justify) lowers as plain `%A` (no-op, matching F#)" {
                match soleDecl "printfn \"%-A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%-A is Structured"
                        Expect.equal (widthBudgetOf hole) None "the `-` flag is ignored (no budget)"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%10.2A` carries both a width and a size budget" {
                match soleDecl "printfn \"%10.2A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "%10.2A is Structured"
                        Expect.equal (widthBudgetOf hole) (Some 10) "width 10 → print-width budget"
                        Expect.equal (sizeBudgetOf hole) (Some 2) "precision 2 → print-size budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`% A` (space flag) lowers as plain `%A` (no-op, matching F#)" {
                match soleDecl "printfn \"% A\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.isTrue (isStructured hole) "% A is Structured"
                        Expect.equal (widthBudgetOf hole) None "the space flag is ignored (no budget)"
                        Expect.equal (sizeBudgetOf hole) None "no size budget"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // `analyse` has no assembly name (home = `None`), so a record there reads as
            // external and goes cold; `compileSource` names the assembly and lowers it
            // on the engine.

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

            // `fprintf`/`fprintfn` to a real `TextWriter`: the writer is arg 0, the
            // format arg 1, giving a `ToWriter` sink. The harness redirects
            // `System.Console.Out` to the capture writer.
            test "fully-applied `fprintf` lowers to a writer-sink Format" {
                match soleDecl "fprintf System.Console.Out \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToWriter(_, false), segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.Formatted(None, PrintfHoleForm.Alignment.None))
                            "%d → a bare AppendFormatted"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a ToWriter Format node, got: %A" other
            }

            // The writer arrives as an ANNOTATED PARAMETER, not a `System.Console.Out`
            // expression, so the written `System.IO.TextWriter` must unify with the class
            // the sink carries. Pins the hole's TYPE and the `unit` result of the write.
            test "fully-applied `fprintf` on an annotated TextWriter param lowers to a writer-sink Format" {
                match soleDecl "let f (w: System.IO.TextWriter) = fprintf w \"%d\" 42" with
                | TDecl.Let(_, TExpr.Lambda(_, body, _, _), _, _) ->
                    match body with
                    | TExpr.Format(FormatSink.ToWriter(_, false), segs, ty, _) ->
                        Expect.equal ty BuiltinTypes.tyUnit "fprintf result is unit"

                        match segs with
                        | EqOne(FormatSeg.Hole(hole, TExpr.Const(TConstValue.Integral(IntKind.Int32, 42L), _, _))) ->
                            Expect.equal hole.Ty BuiltinTypes.tyInt "the %d hole types as int"
                        | other -> failtestf "unexpected Format segments: %A" other
                    | other -> failtestf "expected a ToWriter Format body, got: %A" other
                | other -> failtestf "expected a lambda binding, got: %A" other
            }

            test "fully-applied `fprintfn` on an annotated TextWriter param lowers to a newline writer-sink Format" {
                match soleDecl "let f (w: System.IO.TextWriter) = fprintfn w \"%d\" 42" with
                | TDecl.Let(_, TExpr.Lambda(_, TExpr.Format(FormatSink.ToWriter(_, true), _, _, _), _, _), _, _) -> ()
                | other -> failtestf "expected a newline ToWriter Format body, got: %A" other
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

            // `bprintf` to a `StringBuilder`: the builder is arg 0, the format arg 1,
            // giving a `ToBuilder` sink. There is no `bprintfn`, so never a newline.
            test "fully-applied `bprintf` lowers to a builder-sink Format" {
                match soleDecl "bprintf (System.Text.StringBuilder()) \"%d\" 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToBuilder _, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.Formatted(None, PrintfHoleForm.Alignment.None))
                            "%d → a bare AppendFormatted"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a ToBuilder Format node, got: %A" other
            }

            // The builder twin of the annotated-writer case above: the written
            // `System.Text.StringBuilder` must resolve to the class the builder sink carries.
            test "fully-applied `bprintf` on an annotated StringBuilder param lowers to a builder-sink Format" {
                match soleDecl "let f (sb: System.Text.StringBuilder) = bprintf sb \"%d\" 42" with
                | TDecl.Let(_, TExpr.Lambda(_, body, _, _), _, _) ->
                    match body with
                    | TExpr.Format(FormatSink.ToBuilder _, segs, ty, _) ->
                        Expect.equal ty BuiltinTypes.tyUnit "bprintf result is unit"

                        match segs with
                        | EqOne(FormatSeg.Hole(hole, TExpr.Const(TConstValue.Integral(IntKind.Int32, 42L), _, _))) ->
                            Expect.equal hole.Ty BuiltinTypes.tyInt "the %d hole types as int"
                        | other -> failtestf "unexpected Format segments: %A" other
                    | other -> failtestf "expected a ToBuilder Format body, got: %A" other
                | other -> failtestf "expected a lambda binding, got: %A" other
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

            // A `%a` / `%t` hole lowers to an ordinary residue *string* expr, emitted like
            // a `%s`: `sprintf` splices the callback's returned string (`cb unit [value]`),
            // the writer/builder families splice `{ let s = new Scratch() in …; s.ToString() }`.

            test "sprintf `%a` lowers to a ToString Format; residue is the applied callback (value-carrying)" {
                // `%a` residue = `cb unit value` — a double application (the value is the
                // outer arg), so the residue's applied function is `App(App(_, _), _)`.
                match soleDecl "sprintf \"%a\" (fun (s: unit) (x: int) -> sprintf \"%d\" x) 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.CallbackHole(_, TExpr.App(TExpr.App _, _, _, _))) -> ()
                    | other ->
                        failtestf "expected a CallbackHole whose residue applies the callback to a value, got: %A" other
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "sprintf `%t` lowers to a Format; residue is the value-less callback application" {
                // `%t` residue = `cb unit` — a single application (no value arg).
                match soleDecl "sprintf \"%t\" (fun (s: unit) -> \"hi\")" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.CallbackHole(_, TExpr.App(fn, _, _, _))) ->
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
                // A writer-family `%a` residue is the capture-first block Elaborate synthesises:
                // `{ let s = new StringWriter() in …; s.ToString() }` — a `Let`.
                match soleDecl "printf \"%a\" (fun (w: System.IO.TextWriter) (x: int) -> fprintf w \"%d\" x) 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut false, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.CallbackHole(_, TExpr.Let _)) -> ()
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
                // The callback closes over `y`: if free-variable/escape analysis did not
                // walk the CallbackHole sub-exprs, `y` would go unregistered and this
                // would fault at runtime.
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
            test "`%A` of an array prints the copy-pasteable literal (slice 4)" {
                runPrints "PHpStructArray" "printfn \"%A\" [| 1; 2; 3 |]" "[1; 2; 3]"
            }

            // `Vesper.List` declares its own `IStructuralFormattable`, so a cons-list renders
            // as a sequence rather than as the synthesised `Cons (1, …)` spine.
            test "`%A` of a cons-list renders as a sequence" {
                runPrints "PHpStructList" "printfn \"%A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            // The sink stops pulling at the budget, so the walk stops there too rather than
            // being filtered afterwards.
            test "`%.2A` truncates a cons-list after 2 nodes" {
                runPrints "PHpStructListSize2" "printfn \"%.2A\" [ 1; 2; 3; 4; 5 ]" "[1; 2; ...]"
            }

            test "`%A` of an int prints the bare value" { runPrints "PHpStructInt" "printfn \"%A\" 42" "42" }

            test "`%A` of a string prints the quoted, escapable literal" {
                runPrints "PHpStructStr" "printfn \"%A\" \"hi\"" "\"hi\""
            }

            // `% A` is a no-op: the space flag leaves output byte-identical to `%A`. F#'s
            // compiler rejects `% A` at parse time (FS0741), so `sprintf "% A"` cannot be
            // the oracle; XParsec admits it and `sprintf "%A"` supplies the bytes.
            test "`% A` of an int prints the bare value (like `%A`)" {
                runParity "PHpStructSpaceInt" "printfn \"% A\" 42" (sprintf "%A" 42)
            }

            test "`% A` of a tuple prints the copy-pasteable literal (like `%A`)" {
                runParity "PHpStructSpaceTuple" "printfn \"% A\" (1, 2, 3)" (sprintf "%A" (1, 2, 3))
            }

            test "`%0A` of an array prints flat (fits the budget either way)" {
                runPrints "PHpStructFlat" "printfn \"%0A\" [| 1; 2; 3 |]" "[1; 2; 3]"
            }

            // ---- `%A` flag forms: `%.NA` (PrintSize), `%+A`, `%-A` ----
            // `%.NA` is a global node budget: after N leaves the engine truncates with `...`.
            // `%+A` (non-public fields) and `%-A` (left-justify) are no-ops on `%A`.

            test "`%.2A` truncates an array after 2 nodes (PrintSize)" {
                runPrints "PHpStructSize2" "printfn \"%.2A\" [| 1; 2; 3; 4; 5 |]" "[1; 2; ...]"
            }

            test "`%.0A` truncates immediately (zero node budget)" {
                runPrints "PHpStructSize0" "printfn \"%.0A\" [ 1; 2; 3 ]" "..."
            }

            test "`%.3A` truncates a nested array per the shared node budget" {
                runPrints
                    "PHpStructSizeNest"
                    "printfn \"%.3A\" [| [| 1; 2 |]; [| 3; 4 |]; [| 5; 6 |] |]"
                    "[[1; 2]; [3; ...]; ...]"
            }

            // The same budget through the DECLARED `Format` body rather than the `IEnumerable`
            // arm: a cons-list is the one type in the tree that reaches the renderer that way,
            // so the flag forms are pinned on it too, not only on arrays.
            test "`%.3A` truncates a nested cons-list per the shared node budget" {
                runPrints
                    "PHpStructSizeNestList"
                    "printfn \"%.3A\" [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]"
                    "[[1; 2]; [3; ...]; ...]"
            }

            test "`% A` of a cons-list prints the copy-pasteable literal (like `%A`)" {
                runPrints "PHpStructSpaceList" "printfn \"% A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            test "`%0A` of a cons-list prints flat (fits the budget either way)" {
                runPrints "PHpStructFlatList" "printfn \"%0A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            test "`%-A` of a cons-list prints the same as plain `%A`" {
                runPrints "PHpStructMinusList" "printfn \"%-A\" [ 1; 2; 3 ]" "[1; 2; 3]"
            }

            test "`%+A` of a record prints the same as plain `%A`" {
                runPrints
                    "PHpStructPlus"
                    "type R = { X: int; Y: string }\nprintfn \"%+A\" { X = 1; Y = \"a\" }"
                    "{ X = 1; Y = \"a\" }"
            }

            test "`%-A` of an array prints the same as plain `%A`" {
                runPrints "PHpStructMinus" "printfn \"%-A\" [| 1; 2; 3 |]" "[1; 2; 3]"
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

            // A regression: the last union-case field type (`S of int`) swallowing the
            // next line's `printfn` as a postfix type application (`int printfn`), which
            // stranded `"%A" (S 3)` as a string applied to a value. An offside guard stops it.
            test "`%A` of a DU constructor written directly as the printf arg" {
                runPrints "PHpStructDuDirect" "type Opt = | N | S of int\nprintfn \"%A\" (S 3)" "S 3"
            }

            test "`%A` of a nullary DU case written directly as the printf arg" {
                runPrints "PHpStructDuNullaryDirect" "type Opt = | N | S of int\nprintfn \"%A\" N" "N"
            }

            test "`%A` of a nested DU application parenthesises the argument" {
                runPrints "PHpStructDuNest" "type Opt = | N | S of Opt\nlet v = S (S N)\nprintfn \"%A\" v" "S (S N)"
            }

            test "`%A` of a record nested in an array renders both structurally" {
                runPrints
                    "PHpStructRecInArray"
                    "type R = { X: int }\nprintfn \"%A\" [| { X = 1 }; { X = 2 } |]"
                    "[{ X = 1 }; { X = 2 }]"
            }

            // ---- `%A` of an external Vesper-package union ----
            // A referenced package's record / DU carries the same synthesised
            // `IStructuralFormattable.Format`, so `%A` of one lowers on the engine.
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

            // `%u` and `%o` reinterpret at the argument's OWN width, so `-1y` is 255 and
            // `-1L` is 18446744073709551615. A negative narrow literal has no
            // `TConstValue`, hence the `0y - 1y` spelling.
            test "`%u` reinterprets at the argument's own width" {
                runParity "PHpUnsI8" "printfn \"%u\" (0y - 1y)" (sprintf "%u" (0y - 1y))
                runParity "PHpUnsI16" "printfn \"%u\" (0s - 1s)" (sprintf "%u" (0s - 1s))
                runParity "PHpUnsU8" "printfn \"%u\" 200uy" (sprintf "%u" 200uy)
                runParity "PHpUnsI64" "printfn \"%u\" (0L - 1L)" (sprintf "%u" (0L - 1L))
                runParity "PHpUnsU64" "printfn \"%u\" 18446744073709551615UL" (sprintf "%u" 18446744073709551615UL)
            }

            test "`%o` reinterprets at the argument's own width" {
                runParity "PHpOctI8" "printfn \"%o\" (0y - 1y)" (sprintf "%o" (0y - 1y))
                runParity "PHpOctI16" "printfn \"%o\" (0s - 1s)" (sprintf "%o" (0s - 1s))
                runParity "PHpOctI64" "printfn \"%o\" (0L - 1L)" (sprintf "%o" (0L - 1L))
            }

            test "the zero-pad integer forms take any width of the family" {
                runParity "PHpZUnsU8" "printfn \"%05u\" 200uy" (sprintf "%05u" 200uy)
                runParity "PHpZUnsI64" "printfn \"%05u\" (0L - 1L)" (sprintf "%05u" (0L - 1L))
                runParity "PHpZOctI8" "printfn \"%08o\" (0y - 1y)" (sprintf "%08o" (0y - 1y))
                runParity "PHpZOctI64" "printfn \"%08o\" (0L - 1L)" (sprintf "%08o" (0L - 1L))
            }

            // An 8- or 16-bit value occupies a full `int32` stack slot, and `~~~` / `<<<`
            // leave the bits above the width set. `%u` and `%o` truncate to the width
            // before widening, so they read `55`, not `4294967095`.
            test "`%u` and `%o` truncate an out-of-range narrow argument" {
                runParity "PHpUnsNotU8" "printfn \"%u\" (~~~200uy)" (sprintf "%u" (~~~200uy))
                runParity "PHpUnsShlU8" "printfn \"%u\" (200uy <<< 4)" (sprintf "%u" (200uy <<< 4))
                runParity "PHpUnsNotU16" "printfn \"%u\" (~~~200us)" (sprintf "%u" (~~~200us))
                runParity "PHpOctNotU8" "printfn \"%o\" (~~~200uy)" (sprintf "%o" (~~~200uy))
                runParity "PHpZUnsNotU8" "printfn \"%08u\" (~~~200uy)" (sprintf "%08u" (~~~200uy))
                runParity "PHpZOctNotU16" "printfn \"%08o\" (~~~200us)" (sprintf "%08o" (~~~200us))
            }

            test "`%O` prints via ToString" { runParity "PHpObj" "printfn \"%O\" 42" (sprintf "%O" 42) }

            test "`%e` prints exponential at default precision" {
                runParity "PHpExp" "printfn \"%e\" 1234.5" (sprintf "%e" 1234.5)
            }

            // For `%g`/`%G` the oracle IS F#'s `sprintf "%g"` (byte-for-byte,
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

            // `%g` types its argument over `float`/`float32`/`decimal`, so a float32 needs
            // no widening at the call.
            test "`%g` takes every float width" {
                runParity "PHpGF32" "printfn \"%g\" 1.5f" (sprintf "%g" 1.5f)
                runParity "PHpGDec" "printfn \"%g\" 1.5M" (sprintf "%g" 1.5M)
                runParity "PHpFF32" "printfn \"%.2f\" 1.5f" (sprintf "%.2f" 1.5f)
                runParity "PHpEDec" "printfn \"%e\" 2.5M" (sprintf "%e" 2.5M)
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
            // width, so F# neither pads nor truncates.
            test "`%05u` of -1 overflows the width without padding or truncation" {
                runParity "PHpZeroUnsOvf" "printfn \"%05u\" (0 - 1)" (sprintf "%05u" -1)
            }

            test "`%08o` zero-pads octal to width 8" { runParity "PHpZeroOct" "printfn \"%08o\" 8" (sprintf "%08o" 8) }

            // Overflow: the two's-complement octal is 11 digits, so no pad, no truncation.
            test "`%08o` of -1 overflows the width without padding or truncation" {
                runParity "PHpZeroOctOvf" "printfn \"%08o\" (0 - 1)" (sprintf "%08o" -1)
            }

            // ---- inert width-less `-`/`0` flags: ignored, plain form ----
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

            // ---- left-align wins over zero-pad, non-float ----
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

            // Floats zero-pad on the RIGHT under left-align: `%-05.2f` of 3.14159 ⇒ `"3.140"`.
            test "`%-05.2f` (left + zero-pad float) lowers to a RightZeroPaddedFloat hole" {
                match soleDecl "printfn \"%-05.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.RightZeroPaddedFloat("F2", 5))
                            "%-05.2f → AppendRightZeroPaddedFloat over an \"F2\" body, total width 5"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%c` lowers to a Formatted char hole (no format string)" {
                match soleDecl "printfn \"%c\" 'a'" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, TExpr.Const(TConstValue.Char 'a', _, _))) ->
                        Expect.equal (formatOf hole) None "%c → AppendFormatted with no .NET format string"

                        Expect.equal hole.Ty (TyConst(RuntimeNames.charKey, EqArray.empty)) "the %c hole types as char"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%M` lowers to a Formatted decimal hole (no format string)" {
                match soleDecl "printfn \"%M\" 3.14M" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, TExpr.Const(TConstValue.Decimal d, _, _))) ->
                        Expect.equal (formatOf hole) None "%M → AppendFormatted with no .NET format string"

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.decimalKey, EqArray.empty))
                            "the %M hole types as decimal"

                        Expect.equal d 3.14M "the decimal literal round-trips its value"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%.2M` (precision) lowers as plain `%M` (F# ignores the precision)" {
                // F# silently ignores a `%M` precision (`%.2M` 3.14159m ⇒ `"3.14159"`),
                // so a literal precision is inert and the hole stays `Formatted` with no
                // .NET format string.
                match soleDecl "printfn \"%.2M\" 3.14159M" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (formatOf hole)
                            None
                            "%.2M → AppendFormatted with no .NET format string (the precision is ignored)"

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.decimalKey, EqArray.empty))
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
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some "+0;-0") "%+d → AppendFormatted at section format \"+0;-0\""
                        Expect.equal (alignmentOf hole) None "no alignment"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`% d` (space sign) lowers with a leading-space section format" {
                match soleDecl "printfn \"% d\" 42" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal (formatOf hole) (Some " 0;-0") "% d → section format \" 0;-0\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // `%+.Nf` / `% .Nf` route through the signed dynamic handler: a half-to-even
            // `"F<prec>"` body with the sign composed on, not a .NET *section* format
            // (which would round half-away). So the classified form is what is asserted.
            test "`%+.2f` (forced sign float) classifies as a ForcedSign fixed-float, no zero-pad" {
                match soleDecl "printfn \"%+.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        match hole.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(false,
                                                                                                                        PrintfHoleForm.Prec.Const 2,
                                                                                                                        'f',
                                                                                                                        Option.None),
                                                                                  PrintfHoleForm.Alignment.None)) -> ()
                        | other -> failtestf "expected ForcedSign(+, .2, 'f', no zero-pad), got: %A" other

                        Expect.equal hole.Ty (TyConst(RuntimeNames.floatKey, EqArray.empty)) "%f types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%+8.2f` carries its width as a Const alignment on the ForcedSign field" {
                match soleDecl "printfn \"%+8.2f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
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

            // Half-to-even, NOT half-away: `0.125` (an exact float) → `+0.12`, not `+0.13`.
            test "`%+.2f` rounds a float midpoint half-to-even (0.125 → +0.12)" {
                runParity "PHpPlusFEven" "printfn \"%+.2f\" 0.125" (sprintf "%+.2f" 0.125)
            }

            // The codegen subset has no float arithmetic or unary negation, so a negative
            // float cannot be produced and the `-` path is not run here.

            test "`%+8.2f` composes the forced sign with width-as-alignment" {
                runParity "PHpPlusFAlign" "printfn \"%+8.2f\" 3.14159" (sprintf "%+8.2f" 3.14159)
            }

            // `%+08.2f` / `% 08.2f` — forced sign, then zero-pad AFTER the sign to a field
            // of 8, over a half-to-even `"F2"` body. Negatives are out of the subset.
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
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.ZeroPaddedFloat("F2", 8))
                            "%08.2f → AppendZeroPaddedFloat over an \"F2\" body, total width 8"

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.floatKey, EqArray.empty))
                            "the %f hole types as float"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%08f` defaults the precision to 6 in the format body" {
                match soleDecl "printfn \"%08f\" 3.14159" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.ZeroPaddedFloat("F6", 8))
                            "%08f → no precision → the default-6 \"F6\" body, total width 8"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // Not exercised: sign-then-zeros placement (F# `%08.2f` of `-3.14159` is
            // `"-0003.14"`) needs a negative float, which the codegen subset cannot build.

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

            // Overflow: the digit count exceeds the field, so the section format's min
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

            // Overflow: the body already exceeds the field, so no right padding.
            test "`%-05.2f` of a wider value overflows without padding" {
                runParity "PHpRZeroFOvf" "printfn \"%-05.2f\" 12345.6" (sprintf "%-05.2f" 12345.6)
            }

            // The float handlers are generic in the value, so `float32` and `decimal` format
            // at their own type. Widening `decimal` to `float` would lose digits, and
            // widening `float32` would change `%A`-style output.
            test "the composing float forms take any width of the family" {
                runParity "PHpZF32" "printfn \"%08.2f\" 1.5f" (sprintf "%08.2f" 1.5f)
                runParity "PHpZFDec" "printfn \"%08.2f\" 1.5M" (sprintf "%08.2f" 1.5M)
                runParity "PHpRZF32" "printfn \"%-08.2f\" 1.5f" (sprintf "%-08.2f" 1.5f)
                runParity "PHpSZFDec" "printfn \"%+08.2f\" 1.5M" (sprintf "%+08.2f" 1.5M)
                runParity "PHpDynF32" "printfn \"%.*f\" 3 1.5f" (sprintf "%.*f" 3 1.5f)
                runParity "PHpDynSDec" "printfn \"%+.*f\" 3 1.5M" (sprintf "%+.*f" 3 1.5M)

                runParity
                    "PHpZFDecWide"
                    "printfn \"%.2f\" 123456789012345678901234.5M"
                    (sprintf "%.2f" 123456789012345678901234.5M)
            }

            // `MemberRef` and `MethodSpec` rows are appended rather than deduplicated, so the
            // handler ABI and each open generic member are minted once per assembly, however
            // many holes call them. The byte-identity goldens cannot report a duplicate mint.
            test "the handler member refs are minted once per assembly, not per hole" {
                let src =
                    "printfn \"%08.2f\" 1.5\n\
                     printfn \"%08.2f\" 2.5\n\
                     printfn \"%08.2f\" 3.5\n\
                     printfn \"%d and %d\" 1 2\n\
                     printfn \"%A\" 7"

                let bytes = Codegen.toBytes (compileSource "PHpHandleRows" src)
                let rows = MetadataStructure.memberRefRowCount bytes

                Expect.equal (rows "AppendZeroPaddedFloat") 1 "three `%08.2f` holes share one open member ref"
                Expect.equal (rows "AppendLiteral") 1 "five format nodes share one `AppendLiteral`"
                Expect.equal (rows "Flush") 1 "five format nodes share one `Flush`"

                // `%d` at `int` and `%A` at `int` differ only in the member, and the two
                // `%d` holes of one node share a spec.
                Expect.equal (rows "AppendFormatted") 1 "both `%d` holes share one `AppendFormatted`"
                Expect.equal (rows "AppendStructured") 1 "one `%A` hole, one `AppendStructured`"
            }

            // `NaN` and `±Infinity` are not numbers, so F# space-pads the whole text (the
            // sign included) where a number takes the zero-pad, and does not force a `+`
            // onto them. Spelled `0.0 / z`, since Vesper binds no `nan`.
            test "the zero-pad float forms space-pad NaN and Infinity" {
                let z = 0.0

                let src spec =
                    sprintf "let z = 0.0\nprintfn \"%s\" (%s / z)" spec

                runParity "PHpZFNaN" (src "%012.2f" "0.0") (sprintf "%012.2f" (0.0 / z))
                runParity "PHpZFPosInf" (src "%012.2f" "1.0") (sprintf "%012.2f" (1.0 / z))
                runParity "PHpZFNegInf" (src "%012.2f" "(0.0 - 1.0)") (sprintf "%012.2f" ((0.0 - 1.0) / z))
                runParity "PHpRZFNaN" (src "%-012.2f" "0.0") (sprintf "%-012.2f" (0.0 / z))
                runParity "PHpRZFNegInf" (src "%-012.2f" "(0.0 - 1.0)") (sprintf "%-012.2f" ((0.0 - 1.0) / z))
                runParity "PHpSZFNaN" (src "%+012.2f" "0.0") (sprintf "%+012.2f" (0.0 / z))
                runParity "PHpSZFPosInf" (src "%+012.2f" "1.0") (sprintf "%+012.2f" (1.0 / z))
                runParity "PHpZENaN" (src "%014e" "0.0") (sprintf "%014e" (0.0 / z))
            }

            test "`$\"x={1}\"` lowers to a ToString Format node" {
                match soleDecl "$\"x={1}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, ty, _), _) ->
                    Expect.equal ty (TyConst(RuntimeNames.stringKey, EqArray.empty)) "interpolation yields a string"

                    match segs with
                    | EqTwo(FormatSeg.Lit "x=",
                            FormatSeg.Hole(hole, TExpr.Const(TConstValue.Integral(IntKind.Int32, 1L), _, _))) ->
                        Expect.equal (formatOf hole) None "a plain hole is AppendFormatted with no format clause"

                        Expect.equal hole.Ty (TyConst(RuntimeNames.intKey, EqArray.empty)) "the hole types as int"
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a ToString Format node, got: %A" other
            }

            test "`$\"{255:X}\"` carries the :format clause as the .NET format" {
                match soleDecl "$\"{255:X}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) -> Expect.equal (formatOf hole) (Some "X") "clause :X → \"X\""
                    | other -> failtestf "unexpected segments: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`$\"%d{x}\"` typed interpolation lowers via the printf spec" {
                match soleDecl "$\"%d{7}\"" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToString, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.Hole(hole, _)) ->
                        Expect.equal
                            (callOf hole)
                            (ClrHoleFormat.HoleCall.Formatted(None, PrintfHoleForm.Alignment.None))
                            "%d → a bare AppendFormatted"

                        Expect.equal
                            hole.Ty
                            (TyConst(RuntimeNames.intKey, EqArray.empty))
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

            // ---- Star width (`%*d`, `%-*d`, `%*A`) ----
            // A star width consumes a leading runtime `int`, evaluated before the value:
            // the padding forms negate it for `-`, `%*A` clamps it as the print width.

            test "`%*d` freezes to a Format node with a DynHole (width only) segment" {
                match soleDecl "printfn \"%*d\" 5 42" with
                | TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
                        match d.Width, d.Precision, d.Value with
                        | ValueSome(TExpr.Const(TConstValue.Integral(IntKind.Int32, 5L), _, _)),
                          ValueNone,
                          TExpr.Const(TConstValue.Integral(IntKind.Int32, 42L), _, _) ->
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
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
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
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueSome _,
                          ValueNone,
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(PrintfHoleForm.PrintWidth.Star, _)) ->
                            ()
                        | other -> failtestf "expected width-only DynHole over PercentA(Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // `-`/`+` are no-ops on `%A`, so `%-*A` / `%+*A` lower to the SAME
            // `PercentA(Star)` hole as a bare `%*A` and render byte-identically.
            test "`%-*A` (no-op flag) freezes to the same PercentA(Star) hole as `%*A`" {
                match soleDecl "printfn \"%-*A\" 1 [1; 2; 3]" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
                        match d.Spec.Source with
                        | HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(PrintfHoleForm.PrintWidth.Star, _)) ->
                            ()
                        | other -> failtestf "expected PercentA(Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%-*A` renders identically to `%*A` (F# parity)" {
                runParity "PHpLeftStarA" "printfn \"%-*A\" 20 (1, 2, 3)" (sprintf "%-*A" 20 (1, 2, 3))
            }

            test "`%-*A` of a cons-list renders identically to `%*A`" {
                runPrints "PHpLeftStarAList" "printfn \"%-*A\" 20 [1; 2; 3]" "[1; 2; 3]"
            }

            // ---- Star precision (`%.*f`, `%*.*f`, `%.*e`, `%.*g`, `%+.*f`, `%.*A`) ----
            // A star precision consumes a leading runtime `int`, after any star width and
            // before the value. `%.*A` feeds the size (`PrintSize`) budget instead.

            test "`%.*f` freezes to a DynHole (precision only) over Fixed(Star)" {
                match soleDecl "printfn \"%.*f\" 3 3.14" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueNone,
                          ValueSome(TExpr.Const(TConstValue.Integral(IntKind.Int32, 3L), _, _)),
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.Fixed PrintfHoleForm.Prec.Star,
                                                                                  PrintfHoleForm.Alignment.None)) -> ()
                        | other -> failtestf "expected precision-only DynHole over Fixed(Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            test "`%*.*f` freezes to a DynHole with both width and precision" {
                match soleDecl "printfn \"%*.*f\" 8 3 3.14" with
                | TDecl.Expression(TExpr.Format(_, segs, _, _), _) ->
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueSome(TExpr.Const(TConstValue.Integral(IntKind.Int32, 8L), _, _)),
                          ValueSome(TExpr.Const(TConstValue.Integral(IntKind.Int32, 3L), _, _)),
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
                    match segs with
                    | EqOne(FormatSeg.DynHole d) ->
                        match d.Width, d.Precision, d.Spec.Source with
                        | ValueNone,
                          ValueSome _,
                          HoleSpecSource.Classified(PrintfHoleForm.HoleForm.PercentA(_, PrintfHoleForm.PrintSize.Star)) ->
                            ()
                        | other -> failtestf "expected precision-only DynHole over PercentA(_, Star), got: %A" other
                    | other -> failtestf "expected one DynHole, got: %A" other
                | other -> failtestf "expected a Format node, got: %A" other
            }

            // Padding forms: every specifier a star width lands on, byte-for-byte vs F#.
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
                // The engine's multi-line break regime diverges from F#'s by design, so
                // this is behavioural: a width-1 budget on a 3-element list must break.
                let _, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "printfn \"%*A\" 1 [1; 2; 3]")

                let body = output.TrimEnd('\r', '\n')
                Expect.isTrue (body.Contains '\n') (sprintf "width 1 breaks the list across lines (got: %A)" body)
            }

            test "`%*A` with a wide width stays flat and matches F#" {
                runParity "PHpStarAWide" "printfn \"%*A\" 80 (1, 2, 3)" (sprintf "%*A" 80 (1, 2, 3))
            }

            test "`%*A` with a negative width renders flat without throwing" {
                runParity "PHpStarANeg" "printfn \"%*A\" (0 - 1) (1, 2, 3)" (sprintf "%*A" -1 (1, 2, 3))
            }

            test "`%*A` of a cons-list with a wide width stays flat" {
                runPrints "PHpStarAWideList" "printfn \"%*A\" 80 [1; 2; 3]" "[1; 2; 3]"
            }

            test "`%*A` of a cons-list with a negative width renders flat without throwing" {
                runPrints "PHpStarANegList" "printfn \"%*A\" (0 - 1) [1; 2; 3]" "[1; 2; 3]"
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
                // More nodes than budget, so the output must contain `...`. Behavioural,
                // for the same break-regime divergence as the `%*A` width tests.
                let _, output =
                    withPrintfAlc (fun alc -> runDriverInAlc alc "printfn \"%.*A\" 1 [1; 2; 3; 4; 5]")

                Expect.isTrue
                    (output.Contains "...")
                    (sprintf "a size-1 budget elides later nodes as ... (got: %A)" output)
            }

            test "`%.*A` with a generous size budget matches F#" {
                runParity "PHpPrecAWide" "printfn \"%.*A\" 100 (1, 2, 3)" (sprintf "%.*A" 100 (1, 2, 3))
            }

            test "`%.*A` of a cons-list applies the runtime size budget" {
                runPrints "PHpPrecAList" "printfn \"%.*A\" 2 [1; 2; 3; 4; 5]" "[1; 2; ...]"
            }

            // The clamp asymmetry: the two-star path clamps precision to 0..99, the
            // prec-only path uses it raw (falling back to .NET custom-format for garbage).
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

            // `%0*d` has no handler and no fallback, so the gate diagnoses it by name
            // rather than routing it cold.
            test "`%0*d` (runtime-width zero-pad) is diagnosed, not lowered" {
                failsWith "%0*d" "printfn \"%0*d\" 5 42"
            }

            // Both the width and the value expression print a marker. F# evaluates the
            // width argument first (curried application order), so `W` must precede `V`;
            // pushing the width inline, after the value, would flip them.
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
