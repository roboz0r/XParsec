module XParsec.FSharp.Codegen.Clr.Tests.ChainedMethodCallTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A method call on the RESULT of a method call (`f.Invoke(a).Invoke(b)`), used as an
// interface-impl member body, must type as the OUTER call's result. Mis-typing it as the
// inner result drops the outer application at freeze.

[<Tests>]
let chainedMethodCallTests =
    testList
        "ChainedMethodCall"
        [
            // `f : Fun<int, Fun<int,int>>`, so `f.Invoke(a)` yields `Fun<int,int>` and
            // `.Invoke(b)` yields the `int` the member declares.
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
                            // Mirrors Vesper.Core's `Flattened`: the object argument `f`
                            // is a ctor-captured field of curried type.
                            "type FlattenedT(f: Fun<int, Fun<int, int>>) ="
                            "    interface Fun<int, int, int> with"
                            "        member _.Invoke(a: int, b: int) : int = f.Invoke(a).Invoke(b)"
                            "let r = (FlattenedT(AddCurried() :> Fun<int, Fun<int, int>>) :> Fun<int, int, int>).Invoke(20, 22)"
                            "printfn \"%d\" r"
                        ]

                let tast, artifact = compileSource "ChainInIfaceMember" src
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "chain in iface member yields OUTER result 42"
            }

            // The identical chain at a top-level `let`. Passing here while the member
            // test fails localises the divergence to the member body.
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
