module XParsec.FSharp.SemanticAnalysis.Tests.HashingTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

[<Tests>]
let tests =
    testList
        "Hashing"
        [
            testList
                "determinism"
                [
                    test "hashString is pure" {
                        Expect.equal (Hashing.hashString "abc") (Hashing.hashString "abc") "same input, same hash"
                    }

                    test "hashString distinguishes inputs" {
                        Expect.notEqual (Hashing.hashString "abc") (Hashing.hashString "abd") "one byte differs"
                    }
                ]

            // A source handed over as text has no path, so its content hash IS its identity.
            // Two fragments that collided here would share an anchor domain, and each one's
            // token indices would resolve against the other's text.
            testList
                "textOriginPath"
                [
                    test "the same text yields the same identity" {
                        Expect.equal
                            (Hashing.textOriginPath "let x = 1")
                            (Hashing.textOriginPath "let x = 1")
                            "identity is a function of the text"
                    }

                    test "different texts yield different identities" {
                        Expect.notEqual
                            (Hashing.textOriginPath "let x = 1")
                            (Hashing.textOriginPath "let x = 2")
                            "two fragments are two anchor domains"
                    }
                ]
        ]
