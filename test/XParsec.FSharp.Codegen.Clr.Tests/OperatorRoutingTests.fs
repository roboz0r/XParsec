module XParsec.FSharp.Codegen.Clr.Tests.OperatorRoutingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The C-Eq1 last mile (docs/core-operators-handoff.md): an operator use site
// (`a = b`, `x + y`, `a < b`) freezes to an `External(op_*)` call head; `Emit`
// rewrites the saturated application to the operator's inline-IL body so it emits
// through the single `TExpr.ILIntrinsic` path — codegen owns no per-operator
// recipe. These tests pin the rewrite at the TAST level (`Emit.lower`) and end to
// end (compile + run real CIL). The op→opcode dispatch lives in the operator
// bodies (`BuiltinOps`), the stopgap for the not-yet-frozen operator `.fs`.

[<Tests>]
let tests =
    testList
        "OperatorRouting"
        [
            test
                "`let f a b = a = b` lowers the `=` use site to a `ceq` TExpr.ILIntrinsic (no External op_Equality survives)" {
                let tast = analyse "let f a b = a = b"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match Emit.lower tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple _,
                              TExpr.Lambda(_,
                                           TExpr.Lambda(_,
                                                        TExpr.ILIntrinsic("ceq",
                                                                          [ TExpr.Var _; TExpr.Var _ ],
                                                                          TyConst "bool"),
                                                        _),
                                           _),
                              false,
                              _) ] -> ()
                | other -> failtestf "expected `=` to lower to a ceq ILIntrinsic, got %A" other
            }

            test "`let f a b = a = b` (the handoff target): `ceq` is true for 2=2, false for 2<>3" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f a b = a = b"
                            "printfn \"%d\" (if f 2 2 then 1 else 0)"
                            "printfn \"%d\" (if f 2 3 then 1 else 0)"
                        ]

                let _, artifact = compileSource "OpRoutingEq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "f 2 2 = true, f 2 3 = false"
            }

            test "primitive equality pins no FSharp.Core dependency (eq §4: no runtime library)" {
                // `=` on ints lowers to bare `ceq` — no metadata, no comparer call —
                // so the emitted PE is FSharp.Core-free (the happy-path `printfn` is
                // too, via Vesper.Formatter).
                let _, artifact =
                    compileSource "OpRoutingEqNoDep" "printfn \"%d\" (if 2 = 2 then 1 else 0)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive `=` pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "2 = 2"
            }

            test "`<>` lowers to `not ceq` and runs: false for 2<>2, true for 2<>3" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f a b = a <> b"
                            "printfn \"%d\" (if f 2 2 then 1 else 0)"
                            "printfn \"%d\" (if f 2 3 then 1 else 0)"
                        ]

                let _, artifact = compileSource "OpRoutingNeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "0\n1" "f 2 2 = false, f 2 3 = true"
            }

            test "the ordering ops route to clt/cgt (and their negations) and run" {
                // `<` → clt, `>` → cgt, `<=` → not cgt, `>=` → not clt.
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (if 2 < 3 then 1 else 0)" // 1
                            "printfn \"%d\" (if 3 < 2 then 1 else 0)" // 0
                            "printfn \"%d\" (if 2 > 3 then 1 else 0)" // 0
                            "printfn \"%d\" (if 3 > 2 then 1 else 0)" // 1
                            "printfn \"%d\" (if 2 <= 2 then 1 else 0)" // 1
                            "printfn \"%d\" (if 3 <= 2 then 1 else 0)" // 0
                            "printfn \"%d\" (if 2 >= 2 then 1 else 0)" // 1
                            "printfn \"%d\" (if 2 >= 3 then 1 else 0)" // 0
                        ]

                let _, artifact = compileSource "OpRoutingOrdering" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0\n0\n1\n1\n0\n1\n0" "ordering ops compute correctly"
            }

            test "a nested mix of arithmetic + equality lowers and runs (one IL path for the whole surface)" {
                // `(1 + 2) * 3 = 9` exercises add, mul, ceq nested through the same
                // `TExpr.ILIntrinsic` machinery.
                let _, artifact =
                    compileSource "OpRoutingMixed" "printfn \"%d\" (if (1 + 2) * 3 = 9 then 1 else 0)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "(1 + 2) * 3 = 9 is true"
            }
        ]
