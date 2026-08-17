module XParsec.FSharp.Codegen.Clr.Tests.BitwiseOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Each of `&&& ||| ^^^ <<< >>> ~~~` is a bare trait call in
// `Vesper.Core/ops-platform.clr.fs`; which widths support it and what IL each lowers to
// are stated on the primitives themselves (`prim-types-*.fsi` / `.fs`).

[<Tests>]
let tests =
    testList
        "BitwiseOperators"
        [
            test "bitwise operator bindings freeze from Vesper.Core and are collected as cross-package inlines" {
                let inlines = ClrSymbolProviders.contractInlineBodies defaultPackages

                for name in
                    [
                        "op_BitwiseAnd"
                        "op_BitwiseOr"
                        "op_ExclusiveOr"
                        "op_LeftShift"
                        "op_RightShift"
                        "op_LogicalNot"
                    ] do
                    Expect.isTrue
                        (Map.containsKey name inlines)
                        (sprintf "%s body sourced from ops-platform.clr.fs" name)
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

                expectNoFSharpCore artifact "primitive bitwise"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                // &&& binds tighter than |||: (13 &&& 11) ||| 4 = 9 ||| 4 = 13
                Expect.equal (output.Trim()) "13" "(13 &&& 11) ||| 4 = 13"
            }

            // `combineHash` generalises, so `x` and `y` are still unsolved typars when
            // the infixes are typed: resolution goes through the contract surface by
            // compiled op name (`op_LeftShift`), not through a pinned operand type.
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

            // The operand set is the set of types declaring the member; `float` and
            // `decimal` declare none, so these are ordinary "no such member" rejections
            // rather than CIL `and` over two float64s.
            test "non-integral operands are rejected by the bitwise family" {
                failsWith "does not support the operator" "let x = 1.0 &&& 2.0\nignore x"
                failsWith "does not support the operator" "let x = 1.5M ||| 2.5M\nignore x"
                failsWith "does not support the operator" "let x = ~~~1.0\nignore x"
                failsWith "does not support the operator" "let x = 1.0 <<< 2\nignore x"
            }
        ]
