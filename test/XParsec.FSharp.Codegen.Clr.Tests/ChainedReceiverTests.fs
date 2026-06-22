module XParsec.FSharp.Codegen.Clr.Tests.ChainedReceiverTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Isolation tests for the chained method-call-receiver freeze gap (fixed
// `eed60c7`): a chained method-call receiver
// `f.Invoke(a).Invoke(b)` (a method call on the RESULT of a method call) as the
// body/return of an interface-impl member mis-types the member's return as the
// INNER call's result; the OUTER application is dropped at freeze. The identical
// chain at a top-level `let` types correctly.

[<Tests>]
let chainedReceiverTests =
    testList
        "ChainedReceiver"
        [
            // The bug: inside an interface-impl member whose declared return is the
            // OUTER result `int`, `f.Invoke(a).Invoke(b)` must type as `int`. The
            // `f` field is a curried `Fun<int, Fun<int,int>>`; `f.Invoke(a)` yields
            // `Fun<int,int>`, and `.Invoke(b)` yields `int`.
            test "chained Invoke(a).Invoke(b) as interface-impl member body types as OUTER result" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type AddB(a: int) ="
                            "    interface Fun<int, int> with"
                            "        member this.Invoke(b: int) : int = a + b"
                            "type AddCurried() ="
                            "    interface Fun<int, Fun<int, int>> with"
                            "        member this.Invoke(a: int) : Fun<int, int> = AddB(a) :> Fun<int, int>"
                            // Mirror Vesper.Core `Flattened`: the receiver `f` is a
                            // ctor-captured field of curried type; the impl body is the
                            // chain `f.Invoke(a).Invoke(b)` with declared return `int`.
                            "type FlattenedT(f: Fun<int, Fun<int, int>>) ="
                            "    interface Fun2<int, int, int> with"
                            "        member _.Invoke(a: int, b: int) : int = f.Invoke(a).Invoke(b)"
                            "let r = (FlattenedT(AddCurried() :> Fun<int, Fun<int, int>>) :> Fun2<int, int, int>).Invoke(20, 22)"
                            "printfn \"%d\" r"
                        ]

                let tast, artifact = compileSource "ChainInIfaceMember" src
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "chain in iface member yields OUTER result 42"
            }

            // Control: the IDENTICAL chain at a top-level `let` types correctly. If
            // this passes while the member-body test fails, the divergence is
            // confirmed member-body-specific.
            test "chained Invoke(a).Invoke(b) at top-level let types as OUTER result (control)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type AddB(a: int) ="
                            "    interface Fun<int, int> with"
                            "        member this.Invoke(b: int) : int = a + b"
                            "type AddCurried() ="
                            "    interface Fun<int, Fun<int, int>> with"
                            "        member this.Invoke(a: int) : Fun<int, int> = AddB(a) :> Fun<int, int>"
                            "let chain (f: Fun<int, Fun<int, int>>) (a: int) (b: int) : int = f.Invoke(a).Invoke(b)"
                            "printfn \"%d\" (chain (AddCurried() :> Fun<int, Fun<int, int>>) 20 22)"
                        ]

                let tast, artifact = compileSource "ChainAtTopLevel" src
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "chain at top-level yields OUTER result 42"
            }
        ]
