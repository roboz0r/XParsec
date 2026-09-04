module XParsec.FSharp.Codegen.Clr.Tests.StatementPositionTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// A `unit` value is a zero-field struct materialised by `ldloca; initobj; ldloc` into a local
// slot. Value position pays that cost; statement position (a `Sequential` item, a loop body, a
// `void` method's whole body) does not.

[<Tests>]
let statementPositionTests =
    testList
        "StatementPosition"
        [
            test "a `void` body of unit-valued calls declares no local" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Box() ="
                            "    member this.Touch () = ()"
                            "let touchTwice (box: Box) ="
                            "    box.Touch ()"
                            "    box.Touch ()"
                            "let box = Box()"
                            "touchTwice box"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "StatementPositionVoidBody" src
                let bytes = Codegen.toBytes artifact

                // `member this.Touch () = ()` is a `void` method whose body is the unit
                // constant, so its whole body is `ret`.
                Expect.equal (peMethodIl bytes "Box" "Touch") [| 0x2Auy |] "Touch is a bare `ret`"
                Expect.equal (peMethodLocalCount bytes "Box" "Touch") 0 "Touch declares no locals"

                Expect.equal
                    (peMethodLocalCount bytes "Program" "touchTwice")
                    0
                    "a sequence of unit-valued calls declares no locals"

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "done" "the program still runs"
            }

            test "a unit value a consumer takes is still materialised" {
                // The call's result fills a `ValueTuple` slot, so it is reified.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Box() ="
                            "    member this.Touch () = ()"
                            "let box = Box()"
                            "let pair = (box.Touch (), 42)"
                            "let u, n = pair"
                            "printfn \"%d\" n"
                        ]

                runs "42" src
            }
        ]
