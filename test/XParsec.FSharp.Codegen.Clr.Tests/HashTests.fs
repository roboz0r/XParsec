module XParsec.FSharp.Codegen.Clr.Tests.HashTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `hash` — the third equality-family member (docs/operators-plan.md,
// C-Eq1) — now sourced from `src/Vesper.Core/ops-platform.fs`, not the
// `Emit.isHash` codegen stopgap (milestone M, docs/symbol-resolution-handoff.md).
//
// `let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj` is
// loaded as a cross-package inline body (`SymbolProviders.inlineBodies`) and
// spliced at each `hash` use site by `Emit.lowerWith`. So `hash 5` lowers to the
// two `ExternalMember` nodes (`EqualityComparer<int>.Default` static property +
// `GetHashCode` instance method) that P4 emits — the same `EqualityComparer<T>`
// family the DU triple hashes its fields through, so `hash` and `=` agree by
// construction (equal values hash equal). BCL-only (no FSharp.Core). These tests
// run on `compileSourceContract` (the `Vesper.Core` manifest at the head of the
// resolution stack); `hash` resolves from the contract's `[<AutoOpen>] Operators`.

[<Tests>]
let tests =
    testList
        "Hash"
        [
            test
                "`hash 5` lowers to the EqualityComparer<int>.Default.GetHashCode ExternalMember nodes (no surviving External)" {
                // The contract path resolves `hash` to its `ops-platform.fs` inline
                // body, which `Emit.lowerWith` splices in: `hash 5` becomes
                // `let _ = 5 in EqualityComparer<int>.Default.GetHashCode _` —
                // `'T` pinned to `int`, the `External("hash")` head gone.
                let provider = SymbolProviders.build [ vesperCoreManifest ]
                let inlines = SymbolProviders.inlineBodies provider [ vesperCoreManifest ]
                Expect.isTrue (Map.containsKey "hash" inlines) "hash inline body loaded from ops-platform.fs"

                let lexed, file = parseFile "let v = hash 5"
                let tast = Pipeline.analyse provider "let v = hash 5" lexed file
                Expect.isEmpty (tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)) "no errors"

                match Emit.lowerWith inlines tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple _,
                              TExpr.Let(_,
                                        TExpr.Const(TConstValue.Int 5, _),
                                        TExpr.App(TExpr.ExternalMember(ValueSome(TExpr.ExternalMember(ValueNone,
                                                                                                      _,
                                                                                                      "Default",
                                                                                                      true,
                                                                                                      _)),
                                                                       _,
                                                                       "GetHashCode",
                                                                       false,
                                                                       _),
                                                  TExpr.Var _,
                                                  _),
                                        _),
                              false,
                              _) ] -> ()
                | other ->
                    failtestf "expected `hash 5` to lower to EqualityComparer<int>.Default.GetHashCode, got %A" other
            }

            test "`hash n` for an int is the identity (Int32.GetHashCode returns the value)" {
                // EqualityComparer<int>.Default.GetHashCode(n) = n.GetHashCode() = n,
                // so the printed hash equals the input — a deterministic check that
                // the comparer body runs end to end.
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (hash 5)" // 5
                            "printfn \"%d\" (hash 0)" // 0
                            "printfn \"%d\" (hash 42)" // 42
                        ]

                let _, artifact = compileSourceContract "HashInt" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "5\n0\n42" "hash of an int is the int"
            }

            test "`hash` of a bool: true → 1, false → 0 (Boolean.GetHashCode), so the elem type threads beyond int" {
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (hash true)" // 1
                            "printfn \"%d\" (hash false)" // 0
                        ]

                let _, artifact = compileSourceContract "HashBool" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "hash true = 1, hash false = 0"
            }

            test "equal values hash equal; distinct values differ (the `hash`/`=` agreement, via char)" {
                // The char hash value is runtime-internal, so assert the *property*
                // (consistency with `=`) rather than the magic number: hashing the
                // same char twice agrees, two different chars don't.
                let src =
                    String.concat
                        "\n"
                        [
                            "printfn \"%d\" (if (hash 'A') = (hash 'A') then 1 else 0)" // 1
                            "printfn \"%d\" (if (hash 'A') = (hash 'B') then 1 else 0)" // 0
                        ]

                let _, artifact = compileSourceContract "HashCharConsistency" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "equal chars hash equal, distinct chars differ"
            }

            test "`hash` pins no FSharp.Core dependency (eq §4: it rides the BCL comparer, not a runtime library)" {
                let _, artifact = compileSourceContract "HashNoDep" "printfn \"%d\" (hash 5)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive `hash` pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "5" "hash 5 = 5"
            }
        ]
