module XParsec.FSharp.Codegen.Js.Tests.PrimitiveExprTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Primitive Expressions"
        [
            test "int32 `+` emits the masked `| 0` template, operands parenthesised" {
                Expect.equal
                    (emitJs "printfn \"%d\" (2 + 2)")
                    "console.log((((2) + (2)) | 0));\n"
                    "the emitted ESM source"
            }

            test "a top-level `let` becomes a `const`; its use resolves to the same name" {
                Expect.equal
                    (emitJs "let x = 2 + 3\nprintfn \"%d\" x")
                    "const x = (((2) + (3)) | 0);\nconsole.log(x);\n"
                    "const binding + reference"
            }

            test "`if … then … else` emits a parenthesised ternary" {
                Expect.equal
                    (emitJs "printfn \"%d\" (if true then 1 else 2)")
                    "console.log((true ? 1 : 2));\n"
                    "conditional expression"
            }

            test "int32 arithmetic executes (2 + 2 = 4)" {
                match runJs "prim-add" "printfn \"%d\" (2 + 2)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "4" "prints 4"
            }

            test "int32 multiply wraps (Math.imul: 100000 * 100000 = 1410065408)" {
                match runJs "prim-imul" "printfn \"%d\" (100000 * 100000)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1410065408" "int32 wrapping via Math.imul"
            }

            test "float arithmetic executes (1.5 + 2.0 = 3.5, `%f` defaults to 6 places)" {
                match runJs "prim-float" "printfn \"%f\" (1.5 + 2.0)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    // `%f` lowers to `toFixed(6)`, matching F#'s
                    // default fixed-point precision (CLR `printfn "%f" 3.5` = "3.500000").
                    Expect.equal out "3.500000" "float add formatted at `%f`'s default 6 places"
            }

            test "int equality executes (2 = 2 → true)" {
                match runJs "prim-eq" "printfn \"%b\" (2 = 2)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "strict === over int32"
            }

            test "conditional executes (if true then 1 else 2 → 1)" {
                match runJs "prim-if" "printfn \"%d\" (if true then 1 else 2)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1" "ternary picks the consequent"
            }

            test "a `let` binding + reference executes (x = 2 + 3 → 5)" {
                match runJs "prim-let" "let x = 2 + 3\nprintfn \"%d\" x" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "const binding round-trips"
            }

            test "int64 add emits the BigInt-wrap template over BigInt literals" {
                // Observing the runtime value needs int64 print/conversion infra not yet wired;
                // check the emitted template instead.
                Expect.equal
                    (emitJs "let x = 2L + 3L")
                    "const x = (BigInt.asIntN(64, (2n) + (3n)));\n"
                    "int64 BigInt-wrapped add"
            }

            test "a type with no JS representation (decimal) is rejected as a semantic diagnostic" {
                // `decimal` has no `.js.fs` companion, so its `platform` face is `None` and
                // `PlatformTypes` flags it as a per-decl error (not a failwith in the emitter).
                let msg =
                    try
                        emitJs "let x = 1.0m" |> ignore
                        None
                    with e ->
                        Some e.Message

                match msg with
                | None -> failtest "expected a platform-unsupported diagnostic for `decimal`"
                | Some m ->
                    Expect.stringContains m "no representation on the target platform" "names the platform verdict"
                    Expect.stringContains m "decimal" "names the offending type"
            }

            test "`let x = m` snapshots a mutable read; a later `m <- _` is not seen through x" {
                // F# `let x = m` copies m at the bind point, so after `m <- 2` the use of
                // `x` still yields 1. The pure-`let` substitution must NOT inline `x := m`
                // (that would re-read m after the mutation and return 2).
                let src =
                    String.concat
                        "\n"
                        [
                            "let f () ="
                            "    let mutable m = 1"
                            "    let x = m"
                            "    m <- 2"
                            "    x"
                            "printfn \"%d\" (f ())"
                        ]

                // `x` stays a bound arrow parameter (snapshot of m) rather than being
                // inlined to a re-read of m — the inner `((x) => …)(m)` captures m's
                // bind-time value before `m = 2`.
                Expect.equal
                    (emitJs src)
                    "const f = () => ((m) => ((x) => ((m = 2), x))(m))(1);\nconsole.log(f());\n"
                    "x is a captured snapshot, not an inlined re-read of m"

                match runJs "let-snapshot-mutable" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1" "x holds the bind-time snapshot, not the post-mutation value"
            }

            test "a reassigned module-level `let mutable` emits `let` (not `const`) and runs" {
                let src =
                    String.concat "\n" [ "let mutable m = 1"; "let x = m"; "m <- 2"; "printfn \"%d\" x" ]

                Expect.equal
                    (emitJs src)
                    "let m = 1;\nconst x = m;\n(m = 2);\nconsole.log(x);\n"
                    "script-mode reassignable top-level binding"

                match runJs "toplevel-mutable" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1" "x snapshots m=1; the `let m` reassignment does not TypeError"
            }

            test "a reassigned module-level `let mutable` emits `export let` in library mode" {
                let src = String.concat "\n" [ "let mutable m = 1"; "m <- 2" ]

                Expect.equal
                    (emitJsLibrary src)
                    "export let m = 1;\n(m = 2);\n"
                    "library-mode reassignable top-level binding"
            }

            test "a module-level `let mutable` written from a lambda is observed (no ref-cell promotion)" {
                // A closure writing a MODULE-level mutable. On the CLR the cell is a
                // static field (RefCellPromotion deliberately never promotes a top-level
                // binder); on JS the `let count` binding is captured by reference, so the
                // lambda's writes are observed with zero boxing. The mutable must stay a
                // bare reassignable binding — a `.contents` ref cell here would read
                // `undefined` (bare decl + promoted reads), the pre-fix miscompile.
                let src =
                    String.concat
                        "\n"
                        [
                            "let mutable count = 0"
                            "let bump () = count <- count + 1"
                            "bump ()"
                            "bump ()"
                            "bump ()"
                            "printfn \"%d\" count"
                        ]

                Expect.isFalse
                    ((emitJs src).Contains "contents")
                    "a module-level mutable is not promoted to a `Vesper.Ref` cell — it stays a bare `let`"

                match runJs "toplevel-mutable-closure" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "3" "the lambda's three writes to the module-level mutable are observed"
            }

            test "a generic intrinsic (array) is NOT flagged unrepresentable on JS" {
                // Array shares `platform = None` with `decimal` but has arity >= 1, so
                // `PlatformTypes` skips the platform-repr check. Any other failure is fine.
                let msg =
                    try
                        emitJs "let x = [| 1; 2; 3 |]" |> ignore
                        None
                    with e ->
                        Some e.Message

                match msg with
                | None -> ()
                | Some m ->
                    Expect.isFalse
                        (m.Contains "no representation on the target platform")
                        (sprintf "array must not trip the platform-unrepresentable verdict, got: %s" m)
            }
        ]
