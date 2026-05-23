module XParsec.FSharp.SemanticAnalysis.Tests.PrintfTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

/// Type of the last `let` declaration's binding.
let private lastDeclType (tast: TastFile) : SemType =
    match List.tryLast tast.Decls with
    | Some(TDecl.Let(_, _, _, ty)) -> ty
    | other -> failwithf "expected a trailing TDecl.Let, got %A" other

let private lastDeclValue (tast: TastFile) : TExpr =
    match List.tryLast tast.Decls with
    | Some(TDecl.Let(_, v, _, _)) -> v
    | other -> failwithf "expected a trailing TDecl.Let, got %A" other

/// FormatType of a single specifier, via the canonical lexer parser.
let private specType (s: string) : FormatType =
    match Lexing.parseFormatSpecifier s with
    | ValueSome ph -> ph.Type
    | ValueNone -> failwithf "not a well-formed placeholder: %s" s

let private tyUnit = MockBuiltins.tyUnit
let private tyInt = MockBuiltins.tyInt
let private tyString = MockBuiltins.tyString

[<Tests>]
let tests =
    testList
        "Printf"
        [
            // ---- Canonical specifier parser (shared with the lexer) ----

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
                    Expect.equal ph.Width (ValueSome(bigint 5)) "width 5"
                    Expect.equal ph.Precision (ValueSome(bigint 2)) "precision 2"
                | ValueNone -> failtest "expected a placeholder"
            }

            // ---- FormatType -> SemType typing map ----

            test "argType: every integer base types as int" {
                let fresh () = TyConst "FRESH"

                for t in
                    [
                        FormatType.DecimalInt
                        FormatType.UnsignedDecimalInt
                        FormatType.UnsignedHex
                        FormatType.UnsignedOctal
                        FormatType.UnsignedBinary
                    ] do
                    Expect.equal (PrintfSpec.argType fresh t) (ValueSome tyInt) (sprintf "%A : int" t)
            }

            test "argType: %A and %O both consume a fresh (polymorphic) arg" {
                let fresh () = TyConst "FRESH"
                Expect.equal (PrintfSpec.argType fresh FormatType.Structured) (ValueSome(TyConst "FRESH")) "%A poly"
                Expect.equal (PrintfSpec.argType fresh FormatType.Object) (ValueSome(TyConst "FRESH")) "%O poly"
            }

            test "argType: %a / %t are not typed in v1" {
                let fresh () = TyConst "FRESH"
                Expect.equal (PrintfSpec.argType fresh FormatType.FormatFunction) ValueNone "%a deferred"
                Expect.equal (PrintfSpec.argType fresh FormatType.Text) ValueNone "%t deferred"
            }

            // ---- Typing ----

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
                Expect.equal (lastDeclType tast) (TyFun(TyConst "System.IO.TextWriter", tyUnit)) "TextWriter -> unit"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "v1 does not type %a — it falls through (and surfaces a diagnostic)" {
                // %a needs a callback printer; not modelled in v1, so the
                // special-case defers and the literal can't match PrintfFormat.
                let tast = analyse "let r = printfn \"%a\" 42"
                Expect.isNonEmpty tast.Diagnostics "unsupported %a is flagged, not silently mistyped"
            }

            test "a local binding shadowing `printfn` is an ordinary function" {
                // The local `printfn` is identity; `printfn 5 : int`, NOT the
                // printf rule (which would reject a bare int format).
                let tast = analyse "let printfn x = x\nlet r = printfn 5"
                Expect.equal (lastDeclType tast) tyInt "r : int via shadowing identity"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // ---- Freeze shape ----
            //
            // A fully-applied literal call with lowerable specifiers now freezes
            // to a `TExpr.Format` (vesper-printf-plan P1) — no `New PrintfFormat`
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
                | TExpr.Format(FormatSink.ToStdOut true, segs, ty) ->
                    Expect.equal ty tyUnit "printfn result is unit"

                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Const(TConstValue.Int 42, _)) ] ->
                        Expect.equal hole.Ty tyInt "the %d hole types as int"
                        Expect.equal hole.Format None "no .NET format string for %d"
                        Expect.equal hole.Alignment None "no alignment"
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
        ]
