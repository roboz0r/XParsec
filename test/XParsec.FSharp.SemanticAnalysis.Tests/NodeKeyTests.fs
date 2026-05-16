module XParsec.FSharp.SemanticAnalysis.Tests.NodeKeyTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

[<Tests>]
let tests =
    testList
        "NodeKey"
        [
            test "ofSource round-trips offset and kind" {
                let key = NodeKey.ofSource 42 NodeKind.ExprApp
                Expect.equal key.Offset 42 "offset"
                Expect.equal key.Kind NodeKind.ExprApp "kind"
                Expect.isFalse key.IsSynthetic "real key"
            }

            test "ofSynthetic sets the syn bit" {
                let key = NodeKey.ofSynthetic 17 NodeKind.SynthCEMethodCall
                Expect.equal key.Offset 17 "spawning offset"
                Expect.equal key.Kind NodeKind.SynthCEMethodCall "synth kind"
                Expect.isTrue key.IsSynthetic "synthetic key"
            }

            test "real and synthetic keys at same offset/kind are distinct" {
                let real = NodeKey.ofSource 100 NodeKind.ExprApp
                let synth = NodeKey.ofSynthetic 100 NodeKind.ExprApp
                Expect.notEqual real.Raw synth.Raw "wire values differ in syn bit"
            }

            test "different kinds at same offset are distinct" {
                let a = NodeKey.ofSource 50 NodeKind.ExprApp
                let b = NodeKey.ofSource 50 NodeKind.PatIdent
                Expect.notEqual a.Raw b.Raw "kind disambiguates"
            }
        ]
