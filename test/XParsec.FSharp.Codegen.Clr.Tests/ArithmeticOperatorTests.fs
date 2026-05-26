module XParsec.FSharp.Codegen.Clr.Tests.ArithmeticOperatorTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The arithmetic / unary-negation operator family (`+ - * / %`, `~-`) is now
// sourced from the `Vesper.Core/ops-platform.fs` contract bodies — the second
// operator family (after equality) to leave the `Emit.BuiltinOps` stopgap. Each
// body is an F# static-optimization whose *base* `(# "add" x y : ^T #)` covers
// every wide signed/float width and whose `when ^T : …` clauses carry the cases
// that need DIFFERENT IL (narrow-int `conv.*` truncation, unsigned `*.un`). Those
// clauses return DIFFERENT types (`byte` / `int16` / …) than the declared `^T`,
// which only type-checks because `inferLibraryOnlyStaticOptimization` no longer
// cross-unifies clause bodies (the per-clause static-opt return typing fix —
// docs/operators-plan.md, the arithmetic/bitwise/unary task).
//
// The byte-wraparound tests are the load-bearing proof the contract body — not
// the `BuiltinOps` fallback — is what emits at a ground use site: `BuiltinOps`
// emits a bare `add` (the int32-on-stack sum, no truncation), so `200uy + 100uy`
// would be `300`; the contract's `when ^T : byte` clause wraps it to `44` via
// `conv.u1`.

[<Tests>]
let tests =
    testList
        "ArithmeticOperators"
        [
            test "arithmetic + unary-neg bindings freeze from Vesper.Core and are collected as cross-package inlines" {
                let _, inlines = SymbolProviders.buildContract defaultManifests

                for name in [ "op_Addition"; "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ] do
                    Expect.isTrue (Map.containsKey name inlines) (sprintf "%s body sourced from ops-platform.fs" name)

                Expect.isTrue
                    (Map.containsKey "op_UnaryNegation" inlines)
                    "op_UnaryNegation body sourced from ops-platform.fs"

                // The binary ops are curried two-param static-opt inlines.
                let isBinaryStaticOpt =
                    function
                    | TDecl.Let(_, TExpr.Lambda(_, TExpr.Lambda(_, TExpr.StaticOptimization _, _), _), true, _) -> true
                    | _ -> false

                for name in [ "op_Addition"; "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ] do
                    Expect.isTrue (isBinaryStaticOpt inlines.[name]) (sprintf "%s is a static-opt inline" name)

                // Unary negation has no narrow/sign variants — a single `neg` IL body.
                match inlines.["op_UnaryNegation"] with
                | TDecl.Let(_, TExpr.Lambda(_, TExpr.ILIntrinsic("neg", _, _), _), true, _) -> ()
                | other -> failtestf "op_UnaryNegation should be a single `neg` inline, got %A" other
            }

            test "integer arithmetic compiles + runs through the contract bodies" {
                // `*` / `/` / `%` bind tighter than `+` / `-`.
                //   (10 + 5) * 2  = 30
                //   30 - 3        = 27
                //   27 ... plus a div/mod: 100 / 7 = 14, 100 % 7 = 2
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" ((10 + 5) * 2 - 3)" // 27
                            "printfn \"%d\" (100 / 7)" // 14
                            "printfn \"%d\" (100 % 7)" // 2
                        ]

                let _, artifact = compileSource "ArithInt" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "27\n14\n2" "int arithmetic computes correctly"
            }

            test "unary negation routes through the contract `neg` body" {
                let src =
                    String.concat "\n" [ "let negate (x: int) = -x"; "printfn \"%d\" (negate 5 + 8)" ] // -5 + 8 = 3

                let _, artifact = compileSource "ArithNeg" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "(-5) + 8 = 3"
            }

            test "byte `+` wraps via the contract `conv.u1` clause (200uy + 100uy = 44uy, not 300)" {
                // The proof the contract body — not the BuiltinOps stopgap — emits:
                // BuiltinOps would leave the int32 sum 300 on the stack (no conv), so
                // `(200uy + 100uy) = 44uy` would be false. The contract's `when ^T :
                // byte` clause truncates to 44, so it is true.
                let src = "printfn \"%d\" (if 200uy + 100uy = 44uy then 1 else 0)"

                let _, artifact = compileSource "ArithByteWrap" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "byte addition wraps mod 256 (conv.u1 truncation)"
            }

            test "byte `*` wraps via the contract `conv.u1` clause (20uy * 20uy = 144uy, not 400)" {
                // 20 * 20 = 400; 400 mod 256 = 144.
                let src = "printfn \"%d\" (if 20uy * 20uy = 144uy then 1 else 0)"

                let _, artifact = compileSource "ArithByteMul" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "byte multiplication wraps mod 256"
            }

            test "primitive arithmetic pins no FSharp.Core dependency (no runtime library)" {
                let _, artifact = compileSource "ArithNoDep" "printfn \"%d\" (2 + 2 * 3)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive arithmetic pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "8" "2 + 2 * 3 = 8"
            }
        ]
