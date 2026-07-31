module XParsec.FSharp.Codegen.Clr.Tests.BitwiseOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The bitwise operator family (`&&& ||| ^^^ <<< >>> ~~~`) sourced from the
// `Vesper.Core/ops-platform.fs` contract bodies. Binding heads needed the
// source-text fallback in `Desugar.opPatCompiledName` (the parenthesised ops lex
// to generic tokens); use sites resolve to their distinct `Token` enums and were
// wired into `Desugar.infixOpName` + `Unification.tryPrimitiveTraitCandidate`
// (the `bitwiseBinaryOps` / `shiftOps` sets). `>>>` is a static-opt (signed `shr`
// base + unsigned `shr.un` clauses); the rest are single-IL bodies.

[<Tests>]
let tests =
    testList
        "BitwiseOperators"
        [
            test "bitwise binding heads freeze from Vesper.Core and are collected as cross-package inlines" {
                let inlines = ClrSymbolProviders.contractInlineBodies defaultManifests

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

            // A *deferred* operand pins the *contract-surface* resolution path, not
            // the ground SRTP-synthesis fast path the cases above exercise: in a
            // generalisable local function the shift/bitwise operands are still
            // unsolved typars when the infix is typed, so `inferInfix` falls past
            // `tryPrimitiveTraitCandidate` to `OpenScope.tryResolve` on the compiled
            // op name. That lookup missed entirely until the contract extractor
            // mapped the *parenthesised* binding heads `(<<<)` / `(&&&)` (which lex
            // to generic operator tokens, not the distinct enum) to their compiled
            // names — and the shift's `int32` param dealiased to `int` so the `1`
            // literal unifies. This is the exact `Set<'T>.ComputeHashCode` shape.
            test "bitwise ops in a generalisable local function resolve through the contract surface" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let combineHash x y = (x <<< 1) + (y &&& 255) + 631"
                            "printfn \"%d\" (combineHash 3 11)"
                        ]

                let _, artifact = compileSource "BitwiseDeferred" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                // (3 <<< 1) + (11 &&& 255) + 631 = 6 + 11 + 631 = 648
                Expect.equal (output.Replace("\r", "").Trim()) "648" "deferred-operand bitwise resolves and computes"
            }

            // PENDING — records a known hole, not a passing contract. Unlike the arithmetic
            // family, `&&& ||| ^^^ ~~~` carry single-IL bodies with no static-opt clause
            // list, so nothing downstream narrows the operand set: whatever the trait
            // synthesis admits reaches the splice. It admits every numeric name, so
            // `1.0 &&& 2.0` today compiles with NO diagnostic and emits CIL `and` over two
            // float64s — verified to throw `InvalidProgramException` at run time. Enable
            // once the bitwise family's operand set is declared on the primitives.
            ptest "non-integral operands are rejected by the bitwise family" {
                failsWith "does not support the operator" "let x = 1.0 &&& 2.0\nignore x"
            }
        ]
