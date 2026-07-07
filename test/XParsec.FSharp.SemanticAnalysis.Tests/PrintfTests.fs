module XParsec.FSharp.SemanticAnalysis.Tests.PrintfTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

/// Type of the last `let` declaration's binding.
let private lastDeclType (tast: TastFile) : SemType =
    if tast.Decls.IsEmpty then
        failwith "expected a trailing TDecl.Let, got no decls"
    else
        match tast.Decls.[tast.Decls.Length - 1] with
        | TDecl.Let(_, _, _, ty) -> ty
        | other -> failwithf "expected a trailing TDecl.Let, got %A" other

let private lastDeclValue (tast: TastFile) : TExpr =
    if tast.Decls.IsEmpty then
        failwith "expected a trailing TDecl.Let, got no decls"
    else
        match tast.Decls.[tast.Decls.Length - 1] with
        | TDecl.Let(_, v, _, _) -> v
        | other -> failwithf "expected a trailing TDecl.Let, got %A" other

/// FormatType of a single specifier, via the canonical lexer parser.
let private specType (s: string) : FormatType =
    match Lexing.parseFormatSpecifier s with
    | ValueSome ph -> ph.Type
    | ValueNone -> failwithf "not a well-formed placeholder: %s" s

let private tyUnit = BuiltinTypes.tyUnit
let private tyInt = BuiltinTypes.tyInt
let private tyString = BuiltinTypes.tyString

/// A dimensionless placeholder over a type letter — the shape `argTypes`
/// reduces to a single value argument for.
let private ph (t: FormatType) : FormatPlaceholder =
    {
        Flags = ""
        Width = FormatDim.Absent
        Precision = FormatDim.Absent
        Type = t
        TypeChar = ' '
    }

/// Assert `src` analyses to an error diagnostic naming `fragment` — a cold
/// residual the printf gate re-errors rather than lowering (there is no
/// FSharp.Core fallback once the family lowers natively).
let private rejectsResidual (fragment: string) (src: string) =
    let tast = analyse src

    Expect.isTrue
        (tast.Diagnostics
         |> List.exists (fun d -> d.Severity = Severity.Error && d.Message.Contains fragment))
        (sprintf
            "expected a residual diagnostic naming %s, got: %A"
            fragment
            (tast.Diagnostics |> List.map (fun d -> d.Message)))

[<Tests>]
let tests =
    testList
        "Printf"
        [
            test "specifier parser: %d -> DecimalInt" { Expect.equal (specType "%d") FormatType.DecimalInt "%d" }

            test "specifier parser: integer bases stay distinct" {
                Expect.equal (specType "%x") FormatType.UnsignedHex "%x -> hex"
                Expect.equal (specType "%o") FormatType.UnsignedOctal "%o -> octal"
                Expect.equal (specType "%B") FormatType.UnsignedBinary "%B -> binary"
            }

            test "specifier parser: %A and %O are distinct" {
                Expect.equal (specType "%A") FormatType.Structured "%A -> Structured"
                Expect.equal (specType "%O") FormatType.Object "%O -> Object"
            }

            test "specifier parser: width / precision captured" {
                match Lexing.parseFormatSpecifier "%5.2f" with
                | ValueSome ph ->
                    Expect.equal ph.Type FormatType.FloatDecimal "type f"
                    Expect.equal ph.Width (FormatDim.Literal(bigint 5)) "width 5"
                    Expect.equal ph.Precision (FormatDim.Literal(bigint 2)) "precision 2"
                | ValueNone -> failtest "expected a placeholder"
            }

            test "specifier parser: star width / precision captured" {
                let dims (s: string) =
                    match Lexing.parseFormatSpecifier s with
                    | ValueSome ph -> struct (ph.Width, ph.Precision, ph.Type)
                    | ValueNone -> failtestf "expected a placeholder: %s" s

                Expect.equal (dims "%*d") (FormatDim.Star, FormatDim.Absent, FormatType.DecimalInt) "%*d"
                Expect.equal (dims "%.*f") (FormatDim.Absent, FormatDim.Star, FormatType.FloatDecimal) "%.*f"
                Expect.equal (dims "%*.*f") (FormatDim.Star, FormatDim.Star, FormatType.FloatDecimal) "%*.*f"
                Expect.equal (dims "%-*d") (FormatDim.Star, FormatDim.Absent, FormatType.DecimalInt) "%-*d"
                Expect.equal (dims "%0*d") (FormatDim.Star, FormatDim.Absent, FormatType.DecimalInt) "%0*d"
                Expect.equal (dims "%+*d") (FormatDim.Star, FormatDim.Absent, FormatType.DecimalInt) "%+*d"
                Expect.equal (dims "%*A") (FormatDim.Star, FormatDim.Absent, FormatType.Structured) "%*A"
            }

            test "specifier parser: literal / absent dims unchanged" {
                let dims (s: string) =
                    match Lexing.parseFormatSpecifier s with
                    | ValueSome ph -> struct (ph.Width, ph.Precision)
                    | ValueNone -> failtestf "expected a placeholder: %s" s

                Expect.equal (dims "%d") (FormatDim.Absent, FormatDim.Absent) "%d"
                Expect.equal (dims "%5d") (FormatDim.Literal(bigint 5), FormatDim.Absent) "%5d"
                Expect.equal (dims "%5.2f") (FormatDim.Literal(bigint 5), FormatDim.Literal(bigint 2)) "%5.2f"
            }

            test "specifier parser: bare %* (star, no type letter) is invalid" {
                Expect.isTrue (Lexing.parseFormatSpecifier "%*").IsNone "%* has no type letter"
            }

            test "argTypes: every integer base types as one int" {
                let fresh () =
                    TyConst(BuiltinTypes.intrinsicKey "FRESH", EqArray.empty)

                for t in
                    [
                        FormatType.DecimalInt
                        FormatType.UnsignedDecimalInt
                        FormatType.UnsignedHex
                        FormatType.UnsignedOctal
                        FormatType.UnsignedBinary
                    ] do
                    Expect.equal
                        (PrintfSpec.argTypes fresh tyUnit tyUnit (ph t))
                        (ValueSome [ tyInt ])
                        (sprintf "%A : int" t)
            }

            test "argTypes: %A and %O both consume one fresh (polymorphic) arg" {
                let fresh () =
                    TyConst(BuiltinTypes.intrinsicKey "FRESH", EqArray.empty)

                Expect.equal
                    (PrintfSpec.argTypes fresh tyUnit tyUnit (ph FormatType.Structured))
                    (ValueSome [ TyConst(BuiltinTypes.intrinsicKey "FRESH", EqArray.empty) ])
                    "%A poly"

                Expect.equal
                    (PrintfSpec.argTypes fresh tyUnit tyUnit (ph FormatType.Object))
                    (ValueSome [ TyConst(BuiltinTypes.intrinsicKey "FRESH", EqArray.empty) ])
                    "%O poly"
            }

            test "argTypes: %a consumes printer + value sharing one fresh typar; %t consumes just the printer" {
                let mutable n = 0

                let fresh () =
                    n <- n + 1
                    TyConst(BuiltinTypes.intrinsicKey ("FRESH" + string n), EqArray.empty)

                let state = tyString
                let residue = tyInt

                // %a : the printer `state -> tv -> residue` and the value `tv` — the
                // SAME typar node in both slots, so exactly one fresh is minted.
                match PrintfSpec.argTypes fresh state residue (ph FormatType.FormatFunction) with
                | ValueSome [ TyFun(s, TyFun(tv1, r)); tv2 ] ->
                    Expect.equal s state "printer's state arg"
                    Expect.equal r residue "printer's residue result"
                    Expect.equal tv1 tv2 "value arg is the same typar as the printer's inner arg"
                    Expect.equal n 1 "%a mints exactly one fresh typar"
                | other -> failtestf "unexpected %%a shape: %A" other

                // %t : just the printer `state -> residue`, no value, no fresh.
                n <- 0

                Expect.equal
                    (PrintfSpec.argTypes fresh state residue (ph FormatType.Text))
                    (ValueSome [ TyFun(state, residue) ])
                    "%t printer"

                Expect.equal n 0 "%t mints no fresh typar"
            }

            test "argTypes: star dims prepend an int per star, width before precision" {
                let fresh () =
                    TyConst(BuiltinTypes.intrinsicKey "FRESH", EqArray.empty)

                let tyFloat = BuiltinTypes.tyFloat

                let star (w: FormatDim) (p: FormatDim) (t: FormatType) = { ph t with Width = w; Precision = p }

                // %*d : width int, then value int.
                Expect.equal
                    (PrintfSpec.argTypes
                        fresh
                        tyUnit
                        tyUnit
                        (star FormatDim.Star FormatDim.Absent FormatType.DecimalInt))
                    (ValueSome [ tyInt; tyInt ])
                    "%*d"

                // %.*f : precision int, then value float.
                Expect.equal
                    (PrintfSpec.argTypes
                        fresh
                        tyUnit
                        tyUnit
                        (star FormatDim.Absent FormatDim.Star FormatType.FloatDecimal))
                    (ValueSome [ tyInt; tyFloat ])
                    "%.*f"

                // %*.*f : width int, precision int, value float.
                Expect.equal
                    (PrintfSpec.argTypes
                        fresh
                        tyUnit
                        tyUnit
                        (star FormatDim.Star FormatDim.Star FormatType.FloatDecimal))
                    (ValueSome [ tyInt; tyInt; tyFloat ])
                    "%*.*f"

                // A literal width consumes no extra argument.
                Expect.equal
                    (PrintfSpec.argTypes
                        fresh
                        tyUnit
                        tyUnit
                        (star (FormatDim.Literal(bigint 5)) FormatDim.Absent FormatType.DecimalInt))
                    (ValueSome [ tyInt ])
                    "%5d"
            }

            test "isUnaryConcreteHole: concrete single-arg holes only" {
                let star (w: FormatDim) (p: FormatDim) (t: FormatType) = { ph t with Width = w; Precision = p }

                Expect.isTrue (PrintfSpec.isUnaryConcreteHole (ph FormatType.DecimalInt)) "%d unary concrete"

                Expect.isTrue
                    (PrintfSpec.isUnaryConcreteHole (
                        star (FormatDim.Literal(bigint 5)) FormatDim.Absent FormatType.DecimalInt
                    ))
                    "%5d unary concrete"

                Expect.isFalse
                    (PrintfSpec.isUnaryConcreteHole (star FormatDim.Star FormatDim.Absent FormatType.DecimalInt))
                    "%*d multi-arg → excluded"

                Expect.isFalse (PrintfSpec.isUnaryConcreteHole (ph FormatType.Structured)) "%A polymorphic → excluded"
            }

            test "star-width printer types: %*d printer is int -> int -> unit" {
                let tast = analyse "let g () = printf \"%*d\""

                Expect.equal
                    (lastDeclType tast)
                    (TyFun(tyUnit, TyFun(tyInt, TyFun(tyInt, tyUnit))))
                    "unit -> (int -> int -> unit)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "star-width printer types: %*.*f printer is int -> int -> float -> unit" {
                let tast = analyse "let g () = printf \"%*.*f\""
                let tyFloat = BuiltinTypes.tyFloat

                Expect.equal
                    (lastDeclType tast)
                    (TyFun(tyUnit, TyFun(tyInt, TyFun(tyInt, TyFun(tyFloat, tyUnit)))))
                    "unit -> (int -> int -> float -> unit)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "star-width mid-hole partial: printfn \"%*d\" 5 : int -> unit" {
                let tast = analyse "let f = printfn \"%*d\" 5"
                Expect.equal (lastDeclType tast) (TyFun(tyInt, tyUnit)) "int -> unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "star width fully applied types as unit and stays off the happy path" {
                // %*d types fine (a silent cold-path degrade, no diagnostic) —
                // classify still defers star, so no Format marker is stamped.
                let tast = analyse "let r = printfn \"%*d\" 5 42"
                Expect.equal (lastDeclType tast) tyUnit "result unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics for star width"
            }

            test "interpolated star width is an accurate error" {
                let tast = analyse "let x = 42\nlet r = $\"%*d{x}\""

                let hasStarDiag =
                    tast.Diagnostics
                    |> Seq.exists (fun d -> d.Message.Contains "star width/precision")

                Expect.isTrue hasStarDiag "interpolated star emits the accurate diagnostic"
            }

            test "printfn \"%d\" 42 : unit" {
                let tast = analyse "let r = printfn \"%d\" 42"
                Expect.equal (lastDeclType tast) tyUnit "result unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "sprintf \"%d\" 42 : string" {
                let tast = analyse "let r = sprintf \"%d\" 42"
                Expect.equal (lastDeclType tast) tyString "result string"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "printfn \"%s %d\" \"n\" 42 : unit (curries through both args)" {
                let tast = analyse "let r = printfn \"%s %d\" \"n\" 42"
                Expect.equal (lastDeclType tast) tyUnit "result unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "partial application printfn \"%d\" : int -> unit" {
                let tast = analyse "let f = printfn \"%d\""
                Expect.equal (lastDeclType tast) (TyFun(tyInt, tyUnit)) "int -> unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "integer-base specifier %x types its argument as int" {
                let tast = analyse "let r = sprintf \"%x\" 255"
                Expect.equal (lastDeclType tast) tyString "result string"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "%x with a non-int argument mismatches" {
                let tast = analyse "let r = printfn \"%x\" true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "type-mismatch diagnostic emitted"
            }

            test "%A and %O arguments unify with the supplied value" {
                let tastA = analyse "let r = sprintf \"%A\" 42"
                Expect.equal (lastDeclType tastA) tyString "%A : string result"
                Expect.isEmpty tastA.Diagnostics "no diagnostics for %A"

                let tastO = analyse "let r = sprintf \"%O\" 42"
                Expect.equal (lastDeclType tastO) tyString "%O : string result"
                Expect.isEmpty tastO.Diagnostics "no diagnostics for %O"
            }

            test "format / argument mismatch emits a diagnostic" {
                // %d wants int, but `true` is bool.
                let tast = analyse "let r = printfn \"%d\" true"

                let hasMismatch =
                    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "type-mismatch diagnostic emitted"
            }

            test "fprintf takes the writer first, then the format" {
                let tast = analyse "let f w = fprintf w \"%d\" 42"

                Expect.equal
                    (lastDeclType tast)
                    (TyFun(TyConst(BuiltinTypes.intrinsicKey "System.IO.TextWriter", EqArray.empty), tyUnit))
                    "TextWriter -> unit"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "fully-applied fprintf lowers to a writer-sink Format (native, not cold)" {
                // The format is arg 1 (arg 0 is the writer); a fully-applied
                // lowerable `fprintf` now mints a `TExpr.Format` with a
                // `ToWriter` sink rather than falling to the FSharp.Core path.
                let tast = analyse "let f (w: System.IO.TextWriter) = fprintf w \"%d\" 42"

                match lastDeclValue tast with
                | TExpr.Lambda(_, body, _, _) ->
                    match body with
                    | TExpr.Format(FormatSink.ToWriter(_, false), segs, ty, _) ->
                        Expect.equal ty tyUnit "fprintf result is unit"

                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _, _)) ] ->
                            Expect.equal hole.Ty tyInt "the %d hole types as int"
                        | other -> failtestf "unexpected Format segments: %A" other
                    | other -> failtestf "expected a ToWriter Format body, got: %A" other
                | other -> failtestf "expected a lambda, got: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "fully-applied fprintfn lowers to a newline writer-sink Format" {
                let tast = analyse "let f (w: System.IO.TextWriter) = fprintfn w \"%d\" 42"

                match lastDeclValue tast with
                | TExpr.Lambda(_, TExpr.Format(FormatSink.ToWriter(_, true), _, _, _), _, _) -> ()
                | other -> failtestf "expected a newline ToWriter Format body, got: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "fully-applied bprintf lowers to a builder-sink Format (native, not cold)" {
                // The format is arg 1 (arg 0 is the StringBuilder); a fully-applied
                // lowerable `bprintf` mints a `TExpr.Format` with a `ToBuilder` sink
                // rather than falling to the FSharp.Core path.
                let tast = analyse "let f (sb: System.Text.StringBuilder) = bprintf sb \"%d\" 42"

                match lastDeclValue tast with
                | TExpr.Lambda(_, body, _, _) ->
                    match body with
                    | TExpr.Format(FormatSink.ToBuilder _, segs, ty, _) ->
                        Expect.equal ty tyUnit "bprintf result is unit"

                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _, _)) ] ->
                            Expect.equal hole.Ty tyInt "the %d hole types as int"
                        | other -> failtestf "unexpected Format segments: %A" other
                    | other -> failtestf "expected a ToBuilder Format body, got: %A" other
                | other -> failtestf "expected a lambda, got: %A" other

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "%a demands a callback printer — a bare value is a type error" {
                // `%a` types as a printer `'State -> 'T -> 'Residue`, so the first
                // trailing arg must be a callback; here `42` (an `int`) unifies against
                // that arrow and is rejected.
                let tast = analyse "let r = printfn \"%a\" 42"
                Expect.isNonEmpty tast.Diagnostics "a non-callback %a argument is flagged, not silently mistyped"
            }

            test "a local binding shadowing `printfn` is an ordinary function" {
                // The local `printfn` is identity; `printfn 5 : int`, NOT the
                // printf rule (which would reject a bare int format).
                let tast = analyse "let printfn x = x\nlet r = printfn 5"
                Expect.equal (lastDeclType tast) tyInt "r : int via shadowing identity"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- Cold residuals: re-errored at the gate (no FSharp.Core fallback) ----
            // These specifiers have no faithful native lowering: the runtime-width zero-pad
            // forms (`%0*d`, `%0*.Nf`) have no handler taking a runtime width, and `%0*A` is
            // the `0`-flag-on-`%*A` parsing quirk (renders flat AND discards the width).
            // With the FSharp.Core cold printf being removed there is no fallback, so the
            // gate turns each into an error naming the offending specifier rather than
            // routing it silently.

            test "`%0*d` (runtime-width zero-pad) is a diagnosed residual" {
                rejectsResidual "%0*d" "let r = printfn \"%0*d\" 5 42"
            }

            test "`%0*.2f` (runtime-width zero-pad float) is a diagnosed residual" {
                rejectsResidual "%0*.2f" "let r = printfn \"%0*.2f\" 8 3.5"
            }

            test "`%0*A` (0-flag star-%A quirk) is a diagnosed residual" {
                rejectsResidual "%0*A" "let r = printfn \"%0*A\" 20 42"
            }

            // A residual mixed with lowerable holes still rejects the whole call — the
            // gate names the FIRST offending specifier.
            test "a residual among lowerable holes still re-errors" {
                rejectsResidual "%0*d" "let r = printfn \"ok %d then %0*d\" 1 5 42"
            }

            // ---- `%+08.2f` / `% 08.2f`: forced-sign zero-pad float lowers natively ----
            // The sign is forced onto a half-to-even `"F<prec>"` body then zero-padded
            // after it (`FieldFormat.ForcedSign` carrying a `zeroPad` width), so it lowers
            // rather than routing cold. Byte parity (incl. midpoint rounding) is exercised
            // by the Codegen PrintfHappyPath run-tests.

            test "`%+08.2f` lowers to a ForcedSign fixed-float carrying its zero-pad width" {
                let tast = analyse "let r = printfn \"%+08.2f\" 1234.5"
                Expect.isEmpty tast.Diagnostics "no diagnostics — %+08.2f lowers natively"

                match Lexing.parseFormatSpecifier "%+08.2f" with
                | ValueSome ph ->
                    match PrintfHoleForm.tryClassify ph with
                    | ValueSome(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(false,
                                                                                                    PrintfHoleForm.Prec.Const 2,
                                                                                                    'f',
                                                                                                    Some 8),
                                                              PrintfHoleForm.Alignment.None)) -> ()
                    | other -> failtestf "expected ForcedSign(+, .2, 'f', zeroPad 8), got: %A" other
                | ValueNone -> failtest "expected a placeholder"
            }

            test "`% 08.2f` lowers to a space-sign ForcedSign fixed-float" {
                let tast = analyse "let r = printfn \"% 08.2f\" 1234.5"
                Expect.isEmpty tast.Diagnostics "no diagnostics — % 08.2f lowers natively"

                match Lexing.parseFormatSpecifier "% 08.2f" with
                | ValueSome ph ->
                    match PrintfHoleForm.tryClassify ph with
                    | ValueSome(PrintfHoleForm.HoleForm.Field(PrintfHoleForm.FieldFormat.ForcedSign(true,
                                                                                                    PrintfHoleForm.Prec.Const 2,
                                                                                                    'f',
                                                                                                    Some 8),
                                                              PrintfHoleForm.Alignment.None)) -> ()
                    | other -> failtestf "expected ForcedSign(space, .2, 'f', zeroPad 8), got: %A" other
                | ValueNone -> failtest "expected a placeholder"
            }

            // ---- `%-*A` / `%+*A`: the `-`/`+` flags are no-ops on `%A` ----
            // A flagged star-`%A` renders byte-identically to a bare `%*A`, so it lowers
            // to the same `PrintWidth.Star` structural hole (no diagnostic), unlike the
            // declined `%0*A`.

            test "`%-*A` lowers as a PercentA(Star) hole (flag is a no-op)" {
                let tast = analyse "let r = printfn \"%-*A\" 20 42"
                Expect.isEmpty tast.Diagnostics "no diagnostics — the `-` flag is inert on %A"

                match Lexing.parseFormatSpecifier "%-*A" with
                | ValueSome ph ->
                    match PrintfHoleForm.tryClassify ph with
                    | ValueSome(PrintfHoleForm.HoleForm.PercentA(PrintfHoleForm.PrintWidth.Star, _)) -> ()
                    | other -> failtestf "expected PercentA(Star), got: %A" other
                | ValueNone -> failtest "expected a placeholder"
            }

            test "`%+*A` lowers as a PercentA(Star) hole (flag is a no-op)" {
                let tast = analyse "let r = printfn \"%+*A\" 20 42"
                Expect.isEmpty tast.Diagnostics "no diagnostics — the `+` flag is inert on %A"

                match Lexing.parseFormatSpecifier "%+*A" with
                | ValueSome ph ->
                    match PrintfHoleForm.tryClassify ph with
                    | ValueSome(PrintfHoleForm.HoleForm.PercentA(PrintfHoleForm.PrintWidth.Star, _)) -> ()
                    | other -> failtestf "expected PercentA(Star), got: %A" other
                | ValueNone -> failtest "expected a placeholder"
            }

            // A fully-applied literal call with lowerable specifiers now freezes
            // to a `TExpr.Format` — no `New PrintfFormat`
            // / `App printfn`. The non-lowerable cases above (`%x`, `%a`, partial
            // application, `fprintf`, shadowing) still type the same and keep the
            // FSharp.Core path; this section pins the lowered shape.

            test "printfn \"%d\" 42 freezes to a Format node (stdout + newline, one int hole)" {
                let tast = analyse "let r = printfn \"%d\" 42"

                Expect.equal (TastShape.prettyDecl tast.Decls.[0]) "let v0 = format:stdoutln[{42}]" "TAST shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "the %d hole carries its static type (int), no format / alignment" {
                let tast = analyse "let r = printfn \"%d\" 42"

                match lastDeclValue tast with
                | TExpr.Format(FormatSink.ToStdOut true, segs, ty, _) ->
                    Expect.equal ty tyUnit "printfn result is unit"

                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _, _)) ] ->
                        Expect.equal hole.Ty tyInt "the %d hole types as int"

                        // step (d): the hole carries its classified `Source`; `%d` (no
                        // flags) is a plain verbatim field with no alignment (no .NET
                        // format string, no padding).
                        match hole.Source with
                        | HoleSpecSource.Classified hf ->
                            Expect.equal
                                hf
                                (PrintfHoleForm.HoleForm.Field(
                                    PrintfHoleForm.FieldFormat.Verbatim,
                                    PrintfHoleForm.Alignment.None
                                ))
                                "%d → verbatim field, no alignment"
                        | other -> failtestf "expected a Classified source for %%d, got: %A" other
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "unexpected lowered shape: %A" other
            }

            test "sprintf \"%s %d\" \"n\" 42 freezes to a string-sink Format with both holes" {
                let tast = analyse "let r = sprintf \"%s %d\" \"n\" 42"

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let v0 = format:string[{\"n\"}; \" \"; {42}]"
                    "TAST shape"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- E1: format literal bound to a name / ascribed ----
            // A `PrintfFormat`-annotated `let` (or `(… : Fmt)` ascription) types the
            // string literal AS the format (not `string`) — `tryTypeFormatLiteral` —
            // and records it for const-propagation. A later `sprintf fmt …` recovers
            // the literal at the gate and lowers to the SAME native `TExpr.Format` a
            // syntactic literal would, rather than a cold `New PrintfFormat` + `App`
            // (which has no runtime in the self-host contract).

            test "E1: an annotated format binding types the literal as the PrintfFormat (no mismatch)" {
                let tast =
                    analyse
                        "let fmt : Vesper.Format<int -> string, unit, string, string> = \"%d!\"\nlet s = sprintf fmt 42"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "E1: `sprintf fmt 42` const-props to a native string-sink Format (not a cold App)" {
                let tast =
                    analyse
                        "let fmt : Vesper.Format<int -> string, unit, string, string> = \"%d!\"\nlet s = sprintf fmt 42"

                match lastDeclValue tast with
                | TExpr.Format(FormatSink.ToString, segs, _, _) ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _, _)); FormatSeg.Lit "!" ] ->
                        Expect.equal hole.Ty tyInt "the %d hole types as int"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "expected a native string-sink Format, got: %A" other
            }

            test "E1: a `<_>` wildcard printer is inferred from the specifiers" {
                let tast =
                    analyse "let fmt : Vesper.PrintfFormat<_, unit, string, string> = \"x=%d\"\nlet s = sprintf fmt 7"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match lastDeclValue tast with
                | TExpr.Format(FormatSink.ToString, _, _, _) -> ()
                | other -> failtestf "expected a native string-sink Format, got: %A" other
            }

            test "E1: an ascribed format literal (`(… : Fmt)`) const-props to a native Format" {
                let tast =
                    analyse
                        "let fmt = (\"%d\" : Vesper.PrintfFormat<int -> string, unit, string, string>)\nlet s = sprintf fmt 99"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match lastDeclValue tast with
                | TExpr.Format(FormatSink.ToString, _, _, _) -> ()
                | other -> failtestf "expected a native string-sink Format, got: %A" other
            }

            test "E1: a mismatched format annotation diagnoses an error, does not throw" {
                // `Format<int -> string>` declares one hole but the literal has two —
                // real F# rejects this. Malformed source must surface an ERROR
                // diagnostic; elaboration degrades to diagnostics-only rather than
                // crashing on a Freeze invariant.
                let tast =
                    analyse
                        "open Vesper\nlet fmt : Format<int -> string, unit, string, string> = \"%d %s\"\nlet s = sprintf fmt 1 \"a\""

                Expect.isTrue
                    (tast.Diagnostics |> List.exists (fun d -> d.Severity = Severity.Error))
                    "arity-mismatched format annotation → an error diagnostic (not a throw)"
            }

            test "E1: an unannotated `let fmt = \"%d\"` stays a plain string (not const-propagated)" {
                // Real F# rejects `sprintf fmt 42` here (fmt : string). We must NOT
                // recover the literal: the binding is a plain string, so no
                // `PrintfFormatLiterals` entry, and the printf gate never fires on it.
                let tast = analyse "let fmt = \"%d\"\nlet s = fmt"

                match lastDeclValue tast with
                | TExpr.Format _ -> failtest "a plain-string binding must not lower as a format"
                | _ -> ()
            }
        ]
