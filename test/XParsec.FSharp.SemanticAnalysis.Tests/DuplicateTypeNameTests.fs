module XParsec.FSharp.SemanticAnalysis.Tests.DuplicateTypeNameTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

let private isDuplicate (tast: TastFile) =
    tast.Diagnostics
    |> Seq.exists (fun d -> d.Message.StartsWith "Duplicate type definition")

// Type kinds are registered in a fixed order (record → union → enum → abbrev → class),
// and each registrar rejects a name already taken by a kind registered BEFORE it. That
// makes the obligation asymmetric: the LATER kind owns the collision. These pin every
// ordered pair, so a kind added later cannot quietly skip a check — the same drift that
// left `enum` out of the abbrev and class registrars.
[<Tests>]
let tests =
    testList
        "DuplicateTypeName"
        [
            test "record vs record" {
                let src = "type T = { a: int }\ntype T = { b: int }"
                Expect.isTrue (isDuplicate (analyse src)) "duplicate record name"
            }

            test "union after record" {
                let src = "type T = { a: int }\ntype T = A | B"
                Expect.isTrue (isDuplicate (analyse src)) "union collides with record"
            }

            test "enum after record" {
                let src = "type T = { a: int }\ntype T = | A = 1"
                Expect.isTrue (isDuplicate (analyse src)) "enum collides with record"
            }

            test "enum after union" {
                let src = "type T = A | B\ntype T = | A = 1"
                Expect.isTrue (isDuplicate (analyse src)) "enum collides with union"
            }

            test "abbrev after record" {
                let src = "type T = { a: int }\ntype T = int"
                Expect.isTrue (isDuplicate (analyse src)) "abbrev collides with record"
            }

            test "abbrev after enum" {
                let src = "type T = | A = 1\ntype T = int"
                Expect.isTrue (isDuplicate (analyse src)) "abbrev collides with enum"
            }

            test "class after record" {
                let src = "type T = { a: int }\ntype T() =\n    member this.X = 1"
                Expect.isTrue (isDuplicate (analyse src)) "class collides with record"
            }

            test "class after union" {
                let src = "type T = A | B\ntype T() =\n    member this.X = 1"
                Expect.isTrue (isDuplicate (analyse src)) "class collides with union"
            }

            test "class after enum" {
                let src = "type T = | A = 1\ntype T() =\n    member this.X = 1"
                Expect.isTrue (isDuplicate (analyse src)) "class collides with enum"
            }

            test "class after abbrev" {
                let src = "type T = int\ntype T() =\n    member this.X = 1"
                Expect.isTrue (isDuplicate (analyse src)) "class collides with abbrev"
            }
        ]
