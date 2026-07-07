module XParsec.FSharp.Codegen.Clr.Tests.StaticOptimizationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Static-optimization clause resolution at `let inline` expansion. An inline body
// of the FSharp.Core operator shape
//
//     let inline f (x: ^T) … =
//         <default>
//         when ^T : int   = <clause>
//         when ^T : ^T    = <catch-all>
//
// freezes to a `TExpr.StaticOptimization`; when the inline is expanded at a call
// site, the clause whose `^T` matches the monomorphised operand type is selected
// (the catch-all `when ^T : ^T` otherwise), and only that branch is emitted —
// codegen never sees the static-opt. These tests pin the freeze (TAST shape) and
// the per-type resolution end to end (compile + run real CIL).

[<Tests>]
let tests =
    testList
        "StaticOptimization"
        [
            test "an inline body with `when ^T : …` clauses freezes to a TExpr.StaticOptimization" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline kindOf (x: ^T) : int ="
                            "    -1"
                            "    when ^T : int   = 1"
                            "    when ^T : float = 2"
                            "    when ^T : ^T    = 0"
                        ]

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(TPat.NamedSimple _,
                                     TExpr.Lambda(_,
                                                  TExpr.StaticOptimization(clauses,
                                                                           TExpr.Const(TConstValue.Int -1, _, _),
                                                                           TyConst(key, _),
                                                                           _),
                                                  _,
                                                  _),
                                     true,
                                     _) ] when SymbolKeyOps.simpleName key = "int" ->
                    Expect.equal clauses.Length 3 "three when-clauses, in source order"

                    // First clause is `when ^T : int = 1` — one constraint, body `1`.
                    if clauses.Length > 0 && clauses.[0].Constraints.Length = 1 then
                        match clauses.[0].Body with
                        | TExpr.Const(TConstValue.Int 1, _, _) -> ()
                        | other -> failtestf "unexpected first clause body: %A" other
                    else
                        failtestf "unexpected first clause: %A" clauses.[0]
                | _ -> failtestf "expected a static-opt inline binding, got %A" tast.Decls
            }

            test "the catch-all `when ^T : ^T` is a self-referential TyconEquals constraint" {
                // Both sides of `^T : ^T` are the same typar, so after substitution
                // they are one concrete type — the clause matches unconditionally.
                let src =
                    String.concat "\n" [ "let inline kindOf (x: ^T) : int ="; "    -1"; "    when ^T : ^T = 0" ]

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(_,
                                     TExpr.Lambda(_, TExpr.StaticOptimization(EqList [ clause ], _, _, _), _, _),
                                     true,
                                     _) ] ->
                    match clause.Constraints with
                    | EqList [ TStaticOptConstraint.TyconEquals(TyVar a, TyVar b) ] ->
                        Expect.isTrue
                            (System.Object.ReferenceEquals(UnionFind.find a, UnionFind.find b))
                            "both sides reference the same typar root"
                    | other -> failtestf "expected one self-referential TyconEquals, got %A" other
                | _ -> failtestf "expected a single-clause static-opt, got %A" tast.Decls
            }

            test "clause selection by call-site type: int→1, float→2, catch-all→0" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline kindOf (x: ^T) : int ="
                            "    -1"
                            "    when ^T : int   = 1"
                            "    when ^T : float = 2"
                            "    when ^T : ^T    = 0"
                            "printfn \"%d\" (kindOf 5)" // int clause   → 1
                            "printfn \"%d\" (kindOf 5.0)" // float clause → 2
                            "printfn \"%d\" (kindOf true)" // catch-all    → 0
                        ]

                let _, artifact = compileSource "StaticOptKindOf" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n0" "each call resolves to its type's clause"
            }

            test "the selected clause's inline-IL body emits: int `=` rides `ceq`, bool falls to the catch-all" {
                // Equality shape (return bool under every clause). The int clause is a
                // real `(# \"ceq\" x y : bool #)` over the operands; the catch-all is a
                // `false` sentinel. `eq3 true true` returning false (not true) proves
                // the catch-all — not the ceq clause — was selected for bool.
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline eq3 (x: ^T) (y: ^T) : bool ="
                            "    false"
                            "    when ^T : int = (# \"ceq\" x y : bool #)"
                            "    when ^T : ^T  = false"
                            "printfn \"%d\" (if eq3 2 2 then 1 else 0)" // ceq → 1
                            "printfn \"%d\" (if eq3 2 3 then 1 else 0)" // ceq → 0
                            "printfn \"%d\" (if eq3 true true then 1 else 0)" // catch-all false → 0
                        ]

                let _, artifact = compileSource "StaticOptEq3" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0\n0" "ceq for ints; catch-all (false) for bools"
            }

            test "the same inline resolves independently per call site (no clause bleed across types)" {
                // `kindOf 5` and `kindOf 5.0` in one expression: each expansion must
                // pick its own clause — a shared/aliased resolution would collapse them.
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline kindOf (x: ^T) : int ="
                            "    -1"
                            "    when ^T : int   = 1"
                            "    when ^T : float = 2"
                            "    when ^T : ^T    = 0"
                            "printfn \"%d\" (kindOf 5 + kindOf 5.0)" // 1 + 2 = 3
                        ]

                let _, artifact = compileSource "StaticOptIndependent" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "kindOf 5 (=1) + kindOf 5.0 (=2)"
            }
        ]
