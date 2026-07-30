module XParsec.FSharp.Codegen.Clr.Tests.HashTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `hash` — the third equality-family member (C-Eq1) — now sourced from
// `src/Vesper.Core/ops-platform.fs`, not the
// `Emit.isHash` codegen stopgap.
//
// `let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj` is
// loaded as a cross-package inline body (`ClrSymbolProviders.inlineBodies`) and
// spliced at each `hash` use site by the pre-freeze `Passes.InlineExpansion` pass,
// which reaches the body off the resolved symbol that carries `hash`'s key. So
// `hash 5` freezes to the two `ExternalMember` nodes
// (`EqualityComparer<int>.Default` static property +
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
                "`hash 5` freezes to the EqualityComparer<int>.Default.GetHashCode ExternalMember nodes (no surviving External)" {
                // The contract path resolves `hash` to its `ops-platform.fs` inline
                // body, which the pre-freeze `Passes.InlineExpansion` pass splices in:
                // `hash 5` becomes
                // `let _ = 5 in EqualityComparer<int>.Default.GetHashCode _` —
                // `'T` pinned to `int`, the `External("hash")` head gone — already in
                // the frozen `tast.Decls`, before codegen runs.
                let provider = ClrSymbolProviders.buildContract [ vesperCoreManifest ]
                let inlines = ClrSymbolProviders.contractInlineBodies [ vesperCoreManifest ]
                Expect.isTrue (Map.containsKey "hash" inlines) "hash inline body loaded from ops-platform.fs"

                let lexed, file = parseFile "let v = hash 5"

                let tast =
                    Pipeline.analyseSem provider (Hashing.originSourceOfText "let v = hash 5" lexed) file

                Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) "no errors"

                // The decl carries the EDGE and the operand; the resolved body is the entry
                // it names, abstracted over that operand. Both halves are asserted, because
                // the operand riding the edge (rather than being fused into the body) is
                // what makes the entry shareable across call sites at this grounding.
                match EqArray.toList tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple _,
                              TExpr.InlineCall(spec,
                                               EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32, 5L), _, _) ],
                                               _,
                                               _),
                              false,
                              _) ] ->
                    match specializationValue tast spec with
                    | TExpr.Lambda(_,
                                   TExpr.App(TExpr.ExternalMember(ValueSome(TExpr.ExternalMember(ValueNone,
                                                                                                 _,
                                                                                                 "Default",
                                                                                                 MemberStorage.Property,
                                                                                                 _,
                                                                                                 _)),
                                                                  _,
                                                                  "GetHashCode",
                                                                  MemberStorage.Method,
                                                                  _,
                                                                  _),
                                             TExpr.Var _,
                                             _,
                                             _),
                                   _,
                                   _) -> ()
                    | other ->
                        failtestf
                            "expected `hash`'s entry to be EqualityComparer<int>.Default.GetHashCode over its parameter, got %A"
                            other
                | other -> failtestf "expected `hash 5` to lower to an edge into the `hash` entry, got %A" other
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
