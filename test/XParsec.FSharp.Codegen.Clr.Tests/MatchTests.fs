module XParsec.FSharp.Codegen.Clr.Tests.MatchTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

let private du =
    String.concat "\n" [ "type Shape ="; "    | Dot"; "    | Pair of int * int" ]

let private rec' = "type R = { n: int }"

[<Tests>]
let tests =
    testList
        "Match"
        [
            for src, expected in
                [
                    "printfn \"%d\" (match 1 with | 0 -> 10 | 1 -> 20 | _ -> 30)", "20"
                    "printfn \"%d\" (match 7 with | 0 -> 10 | 1 -> 20 | _ -> 30)", "30"
                    "printfn \"%d\" (match 5 with | 0 -> 100 | n -> n + 1)", "6"
                    "printfn \"%d\" (match 5 with | n when n > 3 -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match 2 with | n when n > 3 -> 1 | _ -> 0)", "0"
                    "printfn \"%d\" (match true with | true -> 1 | false -> 0)", "1"
                    "printfn \"%d\" (match false with | true -> 1 | false -> 0)", "0"
                    "printfn \"%d\" (match 'b' with | 'a' -> 1 | _ -> 0)", "0"
                    // One row per alternative: the middle and last are the ones a
                    // "keep the left arm" lowering silently drops.
                    "printfn \"%d\" (match '-' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match '+' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match ' ' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match 'x' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "0"
                    "printfn \"%d\" (match 2 with | 1 | 2 -> 10 | _ -> 0)", "10"
                    "printfn \"%d\" (match 9 with | 1 | 2 -> 10 | _ -> 0)", "0"
                    // A DU match inline in argument position.
                    du + "\nprintfn \"%d\" (match Pair(3, 4) with | Dot -> 0 | Pair(a, b) -> a + b)", "7"
                    du + "\nprintfn \"%d\" (match Dot with | Dot -> 0 | Pair(a, b) -> a + b)", "0"
                    // `:? T as x` on a value type: isinst, then unbox.any binds the
                    // unboxed int; a miss falls through to the wildcard.
                    "let o = (42 :> obj)\nlet r = match o with | :? int as n -> n | _ -> 0\nprintfn \"%d\" r", "42"
                    "let o = (42 :> obj)\nlet r = match o with | :? bool as b -> 1 | _ -> 0\nprintfn \"%d\" r", "0"
                    // Reference-type target: isinst binds the cast-down object, then
                    // the arm reads a field off the bound variable.
                    rec'
                    + "\nlet o = ({ n = 7 } :> obj)\nlet r = match o with | :? R as x -> x.n | _ -> -1\nprintfn \"%d\" r",
                    "7"
                    rec'
                    + "\nlet o = (42 :> obj)\nlet r = match o with | :? R as x -> x.n | _ -> -1\nprintfn \"%d\" r",
                    "-1"
                ] -> test src { runs expected src }
        ]
