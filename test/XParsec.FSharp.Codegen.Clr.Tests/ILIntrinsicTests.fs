module XParsec.FSharp.Codegen.Clr.Tests.ILIntrinsicTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Value-level inline IL `(# "op" args : ty #)` — the general IL-interpretation
// machinery the operator surface (`=`/`<`/`+`/…) lowers onto: an operator `.fs`
// body supplies the per-primitive opcode and codegen interprets it via
// `TExpr.ILIntrinsic`, owning no per-operator dispatch.

[<Tests>]
let tests =
    testList
        "ILIntrinsic"
        [
            test "an inline-IL `(# \"ceq\" #)` body analyses to a TExpr.ILIntrinsic over the lambda params" {
                let tast =
                    analyse "let inline eqi (x: int) (y: int) : bool = (# \"ceq\" x y : bool #)"

                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(TPat.NamedSimple _,
                                     TExpr.Lambda(_,
                                                  TExpr.Lambda(_,
                                                               TExpr.ILIntrinsic("ceq", _, args, TyConst(key, _), _),
                                                               _,
                                                               _),
                                                  _,
                                                  _),
                                     true,
                                     _) ] when SymbolKeyOps.simpleName key = DisplayName "bool" ->
                    match args with
                    | EqList [ TExpr.Var _; TExpr.Var _ ] -> ()
                    | other -> failtestf "expected two Var operands, got %A" other
                | _ -> failtestf "unexpected IL-intrinsic TAST: %A" tast.Decls
            }

            test "`(# \"ceq\" x y : bool #)` emits CIL `ceq`: equal ints branch true, unequal branch false" {
                // No metadata, no runtime library — the inline body expands at the
                // call site and the bare opcode drives the `if`. Proves the
                // node types (bool), freezes, inline-expands, and emits end to end.
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline eqi (x: int) (y: int) : bool = (# \"ceq\" x y : bool #)"
                            "printfn \"%d\" (if eqi 2 2 then 1 else 0)"
                            "printfn \"%d\" (if eqi 2 3 then 1 else 0)"
                        ]

                let _, artifact = compileSource "ILIntrinsicCeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "ceq is true for 2=2 and false for 2<>3"
            }

            test "`(# \"add\" #)` emits CIL `add` — same machinery carries arithmetic opcodes" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline addi (x: int) (y: int) : int = (# \"add\" x y : int #)"
                            "printfn \"%d\" (addi 40 2)"
                        ]

                let _, artifact = compileSource "ILIntrinsicAdd" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "add 40 2 = 42"
            }
        ]
