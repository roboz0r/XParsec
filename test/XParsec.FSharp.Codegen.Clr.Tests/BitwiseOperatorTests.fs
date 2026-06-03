module XParsec.FSharp.Codegen.Clr.Tests.BitwiseOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The bitwise operator family (`&&& ||| ^^^ <<< >>> ~~~`) sourced from the
// `Vesper.Core/ops-platform.fs` contract bodies. Binding heads needed the
// source-text fallback in `Desugar.opPatCompiledName` (the parenthesised ops lex
// to generic tokens); use sites resolve to their distinct `Token` enums and were
// wired into `Desugar.infixOpName` + `Unification.tryPrimitiveTraitCandidate`
// (the `bitwiseBinaryOps` / `shiftOps` sets). `>>>` is a static-opt (signed `shr`
// base + unsigned `shr.un` clauses); the rest are single-IL bodies. See
// docs/operators-plan.md (the arithmetic/bitwise/unary task).

[<Tests>]
let tests =
    testList
        "BitwiseOperators"
        [
            test "bitwise binding heads freeze from Vesper.Core and are collected as cross-package inlines" {
                let inlines = SymbolProviders.contractInlineBodies defaultManifests

                for name in
                    [
                        "op_BitwiseAnd"
                        "op_BitwiseOr"
                        "op_ExclusiveOr"
                        "op_LeftShift"
                        "op_RightShift"
                        "op_LogicalNot"
                    ] do
                    Expect.isTrue (Map.containsKey name inlines) (sprintf "%s body sourced from ops-platform.fs" name)
            }

            test "bitwise and/or/xor and complement compute correctly through the contract" {
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (13 &&& 11)" // 9
                            "printfn \"%d\" (13 ||| 11)" // 15
                            "printfn \"%d\" (13 ^^^ 11)" // 6
                            "printfn \"%d\" (~~~0)" // -1
                        ]

                let _, artifact = compileSource "BitwiseLogic" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "9\n15\n6\n-1" "and/or/xor/complement compute correctly"
            }

            test "shifts compute correctly through the contract (left + arithmetic right)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (1 <<< 4)" // 16
                            "printfn \"%d\" (208 >>> 2)" // 52
                            "printfn \"%d\" (13 <<< 2)" // 52
                        ]

                let _, artifact = compileSource "BitwiseShift" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "16\n52\n52" "left/right shifts compute correctly"
            }

            test "bitwise ops pin no FSharp.Core dependency (no runtime library)" {
                let _, artifact = compileSource "BitwiseNoDep" "printfn \"%d\" (13 &&& 11 ||| 4)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive bitwise pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                // &&& binds tighter than |||: (13 &&& 11) ||| 4 = 9 ||| 4 = 13
                Expect.equal (output.Trim()) "13" "(13 &&& 11) ||| 4 = 13"
            }
        ]
