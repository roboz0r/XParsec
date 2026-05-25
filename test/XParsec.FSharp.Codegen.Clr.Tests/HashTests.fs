module XParsec.FSharp.Codegen.Clr.Tests.HashTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `hash` — the third equality-family member (docs/core-operators-handoff.md,
// C-Eq1). It has no single IL opcode, so unlike `=`/`<`/`+` (which lower to a
// `TExpr.ILIntrinsic`) a `hash x` use site stays an `External("hash")` call and
// the backend emits `EqualityComparer<'T>.Default.GetHashCode(x)` — the same
// `EqualityComparer<T>` family the DU triple hashes its fields through, so `hash`
// and `=` agree by construction (equal values hash equal). BCL-only.

[<Tests>]
let tests =
    testList
        "Hash"
        [
            test "`hash 5` resolves and survives lowering as an `External(\"hash\")` call (no opcode rewrite)" {
                let tast = analyse "let v = hash 5"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match Emit.lower tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple _, TExpr.App(TExpr.External("hash", _), _, _), false, _) ] -> ()
                | other -> failtestf "expected `hash 5` to stay an External(\"hash\") application, got %A" other
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

                let _, artifact = compileSource "HashInt" src
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

                let _, artifact = compileSource "HashBool" src
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

                let _, artifact = compileSource "HashCharConsistency" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "equal chars hash equal, distinct chars differ"
            }

            test "`hash` pins no FSharp.Core dependency (eq §4: it rides the BCL comparer, not a runtime library)" {
                let _, artifact = compileSource "HashNoDep" "printfn \"%d\" (hash 5)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive `hash` pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "5" "hash 5 = 5"
            }
        ]
