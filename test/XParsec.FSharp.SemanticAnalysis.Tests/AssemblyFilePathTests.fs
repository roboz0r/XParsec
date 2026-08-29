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

            // Both cases travel as two strings; the file half alone distinguishes them, and
            // every real id spells it non-blank.
            testList
                "ofStored"
                [
                    test "a blank file half reads back as Nowhere" {
                        Expect.equal (AssemblyFilePath.ofStored "" "") AssemblyFilePath.Nowhere "no file"
                    }

                    test "an assembly's file reads back whole" {
                        let path =
                            AssemblyFilePath.InFile(
                                ValueSome(AssemblyName "App"),
                                AssemblyFileId.ofRelative "math/z.fs"
                            )

                        Expect.equal
                            (AssemblyFilePath.ofStored
                                (AssemblyName.toStored path.Assembly)
                                (AssemblyFileId.toStored path.Relative))
                            path
                            "the identity came back off the wire"
                    }

                    test "a file no assembly claims keeps its name" {
                        Expect.equal
                            (AssemblyFilePath.ofStored "" "math/z.fs")
                            (AssemblyFilePath.InFile(ValueNone, AssemblyFileId.ofRelative "math/z.fs"))
                            "a blank assembly half is an unclaimed file, not no file"
                    }
                ]

            // `LexedFiles` is keyed by path and enumerated in key order, so a retained file's
            // position in that enumeration turns on this.
            test "Nowhere orders before every file" {
                Expect.isLessThan
                    AssemblyFilePath.Nowhere
                    (AssemblyFilePath.ofText "")
                    "a pool that is nobody's file sorts first"
            }
        ]
