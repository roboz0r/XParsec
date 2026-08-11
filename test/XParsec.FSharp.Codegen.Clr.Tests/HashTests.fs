module XParsec.FSharp.Codegen.Clr.Tests.HashTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj` is a
// cross-package inline body spliced at each use site, so `hash 5` freezes to two
// `ExternalMember` nodes. It resolves only under `compileSourceContract`.

[<Tests>]
let tests =
    testList
        "Hash"
        [
            test
                "`hash 5` freezes to the EqualityComparer<int>.Default.GetHashCode ExternalMember nodes (no surviving External)" {
                // Inline expansion runs before freeze, so `hash 5` is already
                // `EqualityComparer<int>.Default.GetHashCode 5` in `tast.Decls`, with
                // `'T` pinned to `int` and the `External("hash")` node gone.
                let provider = ClrSymbolProviders.buildContract [ vesperCoreManifest ]
                let inlines = ClrSymbolProviders.contractInlineBodies [ vesperCoreManifest ]
                Expect.isTrue (Map.containsKey "hash" inlines) "hash inline body loaded from ops-platform.clr.fs"

                let lexed, file = parseFile "let v = hash 5"

                let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file

                Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) "no errors"

                // `InlineCall` keeps the operand on the call node and the entry abstracts
                // over it, so one entry serves every call site at this grounding.
                match EqArray.toList tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple _,
                              TExpr.InlineCall(
                                  spec = spec
                                  args = EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32, 5L), _, _) ]),
                              false,
                              _) ] ->
                    match specializationValue tast spec with
                    | TExpr.Lambda(_,
                                   TExpr.App(TExpr.ExternalMember(ValueSome(TExpr.ExternalMember(ValueNone,
                                                                                                 _,
                                                                                                 "Default",
                                                                                                 MemberStorage.Property,
                                                                                                 _,
                                                                                                 _,
                                                                                                 _)),
                                                                  _,
                                                                  "GetHashCode",
                                                                  MemberStorage.Method,
                                                                  _,
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
                // The char hash value is runtime-internal, so assert consistency with
                // `=` rather than a magic number.
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

            test "`hash` pins no FSharp.Core dependency (it rides the BCL comparer)" {
                let _, artifact = compileSourceContract "HashNoDep" "printfn \"%d\" (hash 5)"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "primitive `hash` pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "5" "hash 5 = 5"
            }
        ]
