module XParsec.FSharp.Codegen.Clr.Tests.OperatorRoutingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The C-Eq1 last mile (docs/operators-plan.md): an operator use site
// (`a = b`, `x + y`, `a < b`) freezes to an `External(op_*)` call head; `Emit`
// rewrites the saturated application to the operator's inline-IL body so it emits
// through the single `TExpr.ILIntrinsic` path — codegen owns no per-operator
// recipe. These tests pin the rewrite at the TAST level (`Emit.lower`, which has
// no contract bodies, so it exercises the `BuiltinOps` fallback) and end to end
// (compile + run real CIL).
//
// The equality family (`=`/`<>`) is now *also* sourced from the frozen
// `Vesper.Core/ops-platform.fs` contract body when compiled through the contract
// stack (`compileSource`): the operator-named binding `let inline (=) …` freezes
// (the gap operators-plan.md "Phase 3" called out), is collected by
// `SymbolProviders.inlineBodies`, and is spliced + static-opt-resolved at each use
// site — the same cross-package-inline path `hash` uses. The collection test below
// pins that the body is sourced from the contract; the run tests pin behaviour.
// The static-opt *base* is now the structural `EqualityComparer<^T>.Default.Equals`
// fall-clause (type-args-bug.md Layers 1+3): a distinct-but-equal aggregate
// compares structurally, not by reference — see the "DU `=` is structural" test.
// (An unpinned generic operand still falls back to `BuiltinOps`'s `ceq` via the
// codegen `isGround` guard — `let f a b = a = b`, below.)

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

            test
                "`=`/`<>` freeze from the Vesper.Core contract and are collected as cross-package inlines (the Phase-3 gap is closed)" {
                // The headline deliverable: the operator-named bindings `let inline (=)`
                // / `let inline (<>)` in `ops-platform.fs` now freeze and are sourced by
                // the codegen inline-body loader — so `=`/`<>` emit from the contract
                // `.fs`, not just the `BuiltinOps` stopgap. (Previously these bindings
                // never made it through the front end; `operators-plan.md`.)
                let _, inlines = SymbolProviders.buildContract defaultManifests

                Expect.isTrue (Map.containsKey "op_Equality" inlines) "op_Equality body sourced from ops-platform.fs"

                Expect.isTrue
                    (Map.containsKey "op_Inequality" inlines)
                    "op_Inequality body sourced from ops-platform.fs"

                // Each body is an `inline` curried lambda over a static optimization
                // (the `(# \"ceq\" … #)` per-primitive clauses + the fall-clause base).
                let isStaticOptInline =
                    function
                    | TDecl.Let(_, TExpr.Lambda(_, TExpr.Lambda(_, TExpr.StaticOptimization _, _), _), true, _) -> true
                    | _ -> false

                Expect.isTrue (isStaticOptInline inlines.["op_Equality"]) "op_Equality is a static-opt inline"
                Expect.isTrue (isStaticOptInline inlines.["op_Inequality"]) "op_Inequality is a static-opt inline"
            }

            test
                "DU `=` is structural: a distinct-but-equal pair returns true via the comparer (where `ceq` gives false)" {
                // The headline of the restored structural fall-clause
                // (type-args-bug.md). `x` and `y` are two *distinct* heap instances
                // with equal payloads; the static-opt base routes `^T = Tag` to
                // `EqualityComparer<Tag>.Default.Equals(x, y)` — structural — so
                // `x = y` is true. A reference `ceq` (the old stopgap base) would give
                // false, so this asserts the comparer path, not just "doesn't crash".
                let src =
                    String.concat
                        "\n"
                        [
                            "type Tag = Tag of int"
                            "let x = Tag 1"
                            "let y = Tag 1"
                            "printfn \"%d\" (if x = y then 1 else 0)" // distinct instances, equal payload → comparer → 1
                            "printfn \"%d\" (if x <> y then 1 else 0)" // → 0
                        ]

                let _, artifact = compileSource "OpEqDUStructural" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n0"
                    "distinct-but-equal DU pair compares structurally (true), not by reference"
            }
        ]
