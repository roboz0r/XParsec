module XParsec.FSharp.SemanticAnalysis.Tests.AssemblyFilePathTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

[<Tests>]
let tests =
    testList
        "AssemblyFilePath"
        [
            // Text handed over with no file has no path, so the text itself is its identity.
            // Two fragments that collided here would share an anchor domain, and each one's
            // token indices would resolve against the other's text.
            testList
                "ofText"
                [
                    test "the same text yields the same identity" {
                        Expect.equal
                            (AssemblyFilePath.ofText "let x = 1")
                            (AssemblyFilePath.ofText "let x = 1")
                            "identity is a function of the text"
                    }

                    test "different texts yield different identities" {
                        Expect.notEqual
                            (AssemblyFilePath.ofText "let x = 1")
                            (AssemblyFilePath.ofText "let x = 2")
                            "two fragments are two anchor domains"
                    }
                ]
        ]
