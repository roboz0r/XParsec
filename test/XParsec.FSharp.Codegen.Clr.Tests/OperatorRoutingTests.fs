module XParsec.FSharp.Codegen.Clr.Tests.OperatorRoutingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// An operator use site (`a = b`, `x + y`, `a < b`) freezes to an `External(op_*)`
// call head; `Emit` rewrites the saturated application to the operator's inline-IL
// body so it emits through the single `TExprG.ILIntrinsic` path — codegen owns no
// per-operator recipe. These tests pin the rewrite at the TAST level (`Emit.lower`,
// which exercises the `BuiltinOps` fallback) and end to end (compile + run real CIL).
//
// The equality family (`=`/`<>`) is sourced from the frozen
// `Vesper.Core/ops-platform.fs` contract body when compiled through the contract
// stack: the operator-named binding `let inline (=) …` is collected by
// `ClrSymbolProviders.inlineBodies` and spliced + static-opt-resolved at each use site
// by the pre-freeze `Passes.InlineExpansion` pass. The static-opt *base* is
// `EqualityComparer<^T>.Default.Equals` — a distinct-but-equal aggregate compares
// structurally, not by reference (see the "DU `=` is structural" test). An unpinned
// generic operand falls back to `BuiltinOps`'s `ceq` via `expandBuiltinOps`.

[<Tests>]
let tests =
    testList
        "OperatorRouting"
        [
            test
                "`let f a b = a = b` lowers the `=` use site to a `ceq` TExprG.ILIntrinsic (no External op_Equality survives)" {
                let tast = analyse "let f a b = a = b"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match Emit.lower (Freeze.run tast).Decls with
                | [ TDeclG.Let(TPatG.NamedSimple _,
                               TExprG.Lambda(_,
                                             TExprG.Lambda(_,
                                                           TExprG.ILIntrinsic("ceq",
                                                                              _,
                                                                              EqList [ TExprG.Var _; TExprG.Var _ ],
                                                                              FTConst("bool", _),
                                                                              _),
                                                           _,
                                                           _),
                                             _,
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
                // `TExprG.ILIntrinsic` machinery.
                let _, artifact =
                    compileSource "OpRoutingMixed" "printfn \"%d\" (if (1 + 2) * 3 = 9 then 1 else 0)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "(1 + 2) * 3 = 9 is true"
            }

            test "`=`/`<>` freeze from the Vesper.Core contract and are collected as cross-package inlines" {
                // The operator-named bindings `let inline (=)` / `let inline (<>)` in
                // `ops-platform.fs` freeze and are sourced by the codegen inline-body
                // loader — so `=`/`<>` emit from the contract `.fs`, not just the
                // `BuiltinOps` stopgap.
                let inlines = ClrSymbolProviders.contractInlineBodies defaultManifests

                Expect.isTrue (Map.containsKey "op_Equality" inlines) "op_Equality body sourced from ops-platform.fs"

                Expect.isTrue
                    (Map.containsKey "op_Inequality" inlines)
                    "op_Inequality body sourced from ops-platform.fs"

                // Each body is an `inline` curried lambda over a static optimization
                // (the `(# \"ceq\" … #)` per-primitive clauses + the fall-clause base).
                let isStaticOptInline =
                    function
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.Lambda(_, TExprG.StaticOptimization _, _, _), _, _), true, _) ->
                        true
                    | _ -> false

                Expect.isTrue (isStaticOptInline inlines.["op_Equality"].Decl) "op_Equality is a static-opt inline"
                Expect.isTrue (isStaticOptInline inlines.["op_Inequality"].Decl) "op_Inequality is a static-opt inline"
            }

            test
                "DU `=` is structural: a distinct-but-equal pair returns true via the comparer (where `ceq` gives false)" {
                // `x` and `y` are two *distinct* heap instances with equal payloads;
                // the static-opt base routes `^T = Tag` to
                // `EqualityComparer<Tag>.Default.Equals(x, y)` — structural — so
                // `x = y` is true. A reference `ceq` would give false, so this asserts
                // the comparer path, not just "doesn't crash".
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

            // `ignore` on a NON-unit value: FSharp.Core `ignore : 'T -> unit` has no
            // emit recipe, so `expr |> ignore` once crashed codegen with "no call
            // recipe for external 'ignore'". A saturated application now lowers to the
            // `let _ = expr` shape (eval + pop + reify unit). The side effect (the
            // `printfn`) must still run, proving the arg is evaluated, not elided.
            test "`expr |> ignore` on a non-unit value evaluates the arg and discards it" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (x: int) : int ="
                            "    printfn \"side %d\" x"
                            "    x + 1"
                            "f 41 |> ignore"
                            "ignore (f 7)" // the un-piped form, same head + spine
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "IgnoreNonUnit" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "side 41\nside 7\ndone"
                    "both ignore forms run the side effect and discard the int result"
            }
        ]
