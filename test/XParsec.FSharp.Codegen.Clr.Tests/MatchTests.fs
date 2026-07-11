module XParsec.FSharp.Codegen.Clr.Tests.MatchTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: `match`. Rung2Tests holds the deep anchors (TAST
// shape, the emitted-union deconstruction); this is the broad net over the
// pattern forms the backend lowers — literal arms, the wildcard default, a
// named binder, `when` guards, and bool/char scrutinees. A DU + nested-DU row
// (multi-line, needs a type decl) closes the loop on constructor patterns.

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
                    // literal arms: hit, and fall through to the wildcard
                    "printfn \"%d\" (match 1 with | 0 -> 10 | 1 -> 20 | _ -> 30)", "20"
                    "printfn \"%d\" (match 7 with | 0 -> 10 | 1 -> 20 | _ -> 30)", "30"
                    // a named binder used in the body
                    "printfn \"%d\" (match 5 with | 0 -> 100 | n -> n + 1)", "6"
                    // `when` guards: guard passes / guard fails (falls to default)
                    "printfn \"%d\" (match 5 with | n when n > 3 -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match 2 with | n when n > 3 -> 1 | _ -> 0)", "0"
                    // bool scrutinee
                    "printfn \"%d\" (match true with | true -> 1 | false -> 0)", "1"
                    "printfn \"%d\" (match false with | true -> 1 | false -> 0)", "0"
                    // char scrutinee
                    "printfn \"%d\" (match 'b' with | 'a' -> 1 | _ -> 0)", "0"
                    // char OR-pattern: every alternative must match, not just the
                    // leftmost. The middle (`'+'`) and last (`' '`) alternatives are
                    // the ones a "keep the left arm" lowering silently drops.
                    "printfn \"%d\" (match '-' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match '+' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match ' ' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "1"
                    "printfn \"%d\" (match 'x' with | '-' | '+' | ' ' -> 1 | _ -> 0)", "0"
                    // int OR-pattern alongside a wildcard default
                    "printfn \"%d\" (match 2 with | 1 | 2 -> 10 | _ -> 0)", "10"
                    "printfn \"%d\" (match 9 with | 1 | 2 -> 10 | _ -> 0)", "0"
                    // DU constructor patterns: nullary + a payload-binding case.
                    // Let-bound (not inline as the printfn arg): an inline DU-match
                    // in argument position currently trips a Elaborate translateApp
                    // bug, whereas the let-bound form is the proven Rung2 shape.
                    du
                    + "\nlet r = match Pair(3, 4) with | Dot -> 0 | Pair(a, b) -> a + b\nprintfn \"%d\" r",
                    "7"
                    du
                    + "\nlet r = match Dot with | Dot -> 0 | Pair(a, b) -> a + b\nprintfn \"%d\" r",
                    "0"
                    // `:? T as x` type-test patterns (G1). Value-type target hits
                    // (isinst + unbox.any binds the unboxed int) and misses (falls
                    // through to the wildcard).
                    "let o = (42 :> obj)\nlet r = match o with | :? int as n -> n | _ -> 0\nprintfn \"%d\" r", "42"
                    "let o = (42 :> obj)\nlet r = match o with | :? bool as b -> 1 | _ -> 0\nprintfn \"%d\" r", "0"
                    // Reference-type target (the `set.fs` shape): isinst + bind the
                    // cast-down receiver, then read a field off the binder.
                    rec'
                    + "\nlet o = ({ n = 7 } :> obj)\nlet r = match o with | :? R as x -> x.n | _ -> -1\nprintfn \"%d\" r",
                    "7"
                    rec'
                    + "\nlet o = (42 :> obj)\nlet r = match o with | :? R as x -> x.n | _ -> -1\nprintfn \"%d\" r",
                    "-1"
                ] -> test src { runs expected src }
        ]
