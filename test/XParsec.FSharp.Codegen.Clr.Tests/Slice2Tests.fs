module XParsec.FSharp.Codegen.Clr.Tests.Slice2Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Slice2"
        [
            test "`printfn \"%d\" (1 + 2)` prints 3 (spine peeling + Invoke, no locals)" {
                let _, artifact = compileSource "Slice2Arith" "printfn \"%d\" (1 + 2)"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "1 + 2 printed via the %d printer"
            }

            test "the full sample analyses clean and splits into a Let decl + Expression" {
                let tast = analyse "let x = 1 + 2\nprintfn \"%d\" x"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(TPat.NamedSimple(kx, _),
                                     TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _),
                                                         TExpr.Const(TConstValue.Int 1, _),
                                                         _),
                                               TExpr.Const(TConstValue.Int 2, _),
                                               _),
                                     false,
                                     _)
                           TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(hole, TExpr.Var(kxUse, _)) ] ->
                        Expect.equal kxUse kx "the hole's `Var` references the let-bound NodeKey"
                        Expect.equal hole.Ty (TyConst "int") "the %d hole types as int"
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "unexpected slice-2 TAST: %A" other
            }

            test "the full sample compiles, runs, prints 3, exits 0" {
                let _, artifact = compileSource "Slice2Sample" "let x = 1 + 2\nprintfn \"%d\" x"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "x = 1 + 2 reloaded from its local and printed"
            }
        ]
