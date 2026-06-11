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
// This file is the Layer-1 exemplar for docs/codegen-test-strategy-plan.md: the
// dense `(expr, result)` corpus below is the broad, cheap regression net (it
// would catch a change to operator routing / Pratt RHS silently breaking
// `100 % 7`), and the thick tests beneath it are the Layer-2/3 anchors that
// prove *which* emission path fired:
//   - the freeze test proves the bodies are collected as cross-package inlines;
//   - the byte-wraparound tests are the load-bearing proof the contract body —
//     not the `BuiltinOps` fallback — emits at a ground use site: `BuiltinOps`
//     emits a bare `add` (the int32-on-stack sum, no truncation), so
//     `200uy + 100uy` would be `300`; the contract's `when ^T : byte` clause
//     wraps it to `44` via `conv.u1`;
//   - the no-dependency test proves primitive arithmetic pins no FSharp.Core.

/// Layer 1 — behavioral corpus (wide, cheap). The row sources *are* the coverage
/// map: each `(expr, expected)` is one `runs` assertion named by its source.
let private corpus: Test list =
    [
        for src, expected in
            [
                // the four binary ops, each on its own so a routing break is punctual
                """printfn "%d" (2 + 3)""", "5"
                """printfn "%d" (10 - 4)""", "6"
                """printfn "%d" (6 * 7)""", "42"
                """printfn "%d" (100 / 7)""", "14"
                """printfn "%d" (100 % 7)""", "2"
                // negative results
                """printfn "%d" (3 - 10)""", "-7"
                // left-associativity (7-3-2 = 2, not 6; 100/5/2 = 10, not 40)
                """printfn "%d" (7 - 3 - 2)""", "2"
                """printfn "%d" (100 / 5 / 2)""", "10"
                // precedence: * / % bind tighter than + -
                """printfn "%d" (2 + 3 * 4)""", "14"
                """printfn "%d" ((10 + 5) * 2 - 3)""", "27"
                """printfn "%d" (100 - 2 * 3 + 1)""", "95"
            ] -> test src { runs expected src }
    ]

[<Tests>]
let tests =
    testList
        "Arithmetic"
        (corpus
         @ [
             // unary negation through a binding routes the variable through the
             // contract `neg` body (a literal `-5` is a negative constant, not the
             // operator — so exercise it via a function parameter).
             test "unary negation routes through the contract `neg` body" {
                 runs "3" "let negate (x: int) = -x\nprintfn \"%d\" (negate 5 + 8)" // -5 + 8 = 3
             }

             // ---- Layer 2/3: milestone + structural anchors (keep) -----------
             test "arithmetic + unary-neg bindings freeze from Vesper.Core and are collected as cross-package inlines" {
                 let inlines = SymbolProviders.contractInlineBodies defaultManifests

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
                     Expect.isTrue (isBinaryStaticOpt inlines.[name].Decl) (sprintf "%s is a static-opt inline" name)

                 // Unary negation has no narrow/sign variants — a single `neg` IL body.
                 match inlines.["op_UnaryNegation"].Decl with
                 | TDecl.Let(_, TExpr.Lambda(_, TExpr.ILIntrinsic("neg", _, _, _), _), true, _) -> ()
                 | other -> failtestf "op_UnaryNegation should be a single `neg` inline, got %A" other
             }

             test "byte `+` wraps via the contract `conv.u1` clause (200uy + 100uy = 44uy, not 300)" {
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
         ])
