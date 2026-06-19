module XParsec.FSharp.Codegen.Js.Tests.Step5bTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Step5b"
        [
            test "a cons-list literal imports the case classes and nests Cons ending in Empty" {
                Expect.equal
                    (emitJs "let xs = [1; 2; 3]")
                    ("import { List_Cons as $Vesper_List_List_Cons, List_Empty as $Vesper_List_List_Empty } from \"./Vesper.List.mjs\";\n"
                     + "const xs = new $Vesper_List_List_Cons(1, new $Vesper_List_List_Cons(2, new $Vesper_List_List_Cons(3, new $Vesper_List_List_Empty())));\n")
                    "external cons-list union → import Cons/Empty from the home module (no local re-emit); literal → nested Cons ending in Empty"
            }

            test "a `[1;2;3]` literal sums via []/:: match recursion (→ 6)" {
                match
                    runJs
                        "step5b-sum"
                        ("let rec sum xs =\n"
                         + "    match xs with\n"
                         + "    | [] -> 0\n"
                         + "    | h :: t -> h + sum t\n"
                         + "printfn \"%d\" (sum [1; 2; 3])")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "6" "nested Cons literal folded through []/:: match"
            }

            test "`::` constructs a cons cell; head reads back through a match" {
                // Monomorphic on purpose: generic element-typed consumer of an external cons-list is out of scope.
                match
                    runJs
                        "step5b-cons"
                        ("let xs = 10 :: 20 :: []\n"
                         + "let v =\n"
                         + "    match xs with\n"
                         + "    | [] -> -1\n"
                         + "    | h :: _ -> h\n"
                         + "printfn \"%d\" v")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "10" "`::` builds the cell, the cons arm binds its head"
            }

            test "an empty list takes the `[]` arm (length [] → 0)" {
                match
                    runJs
                        "step5b-empty"
                        ("let rec length xs =\n"
                         + "    match xs with\n"
                         + "    | [] -> 0\n"
                         + "    | _ :: t -> 1 + length t\n"
                         + "let e : int list = []\n"
                         + "printfn \"%d\" (length e)\n"
                         + "printfn \"%d\" (length [7; 8])")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0\n2" "`[]` matches Empty (tag 0); a 2-cell list counts 2"
            }

            test "a nested `h1 :: h2 :: _` pattern binds the first two elements" {
                match
                    runJs
                        "step5b-nested-pat"
                        ("let firstTwo xs =\n"
                         + "    match xs with\n"
                         + "    | a :: b :: _ -> a + b\n"
                         + "    | _ -> 0\n"
                         + "printfn \"%d\" (firstTwo [3; 4; 5])\n"
                         + "printfn \"%d\" (firstTwo [9])")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7\n0" "two-deep cons pattern binds a+b; a 1-element list falls through"
            }

            test "List.length imports the runtime function AND the case classes it constructs" {
                Expect.equal
                    (emitJs "let n = List.length [1; 2; 3]")
                    ("import { List_Cons as $Vesper_List_List_Cons, List_Empty as $Vesper_List_List_Empty, length as $Vesper_Collections_ListModule_length } from \"./Vesper.List.mjs\";\n"
                     + "const n = $Vesper_Collections_ListModule_length(new $Vesper_List_List_Cons(1, new $Vesper_List_List_Cons(2, new $Vesper_List_List_Cons(3, new $Vesper_List_List_Empty()))));\n")
                    "one import merges the function and the case classes; the `new` sites reference the imported class aliases (no local re-emit)"
            }

            test "List.length counts a literal under Node (→ 3)" {
                match runJs "step5b-length" "printfn \"%d\" (List.length [1; 2; 3])" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "3" "imported List.length walks the cons cells"
            }

            test "List.length of the empty list is 0 under Node" {
                match runJs "step5b-length-empty" "let e : int list = []\nprintfn \"%d\" (List.length e)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0" "Empty has tag 0; the runtime loop counts none"
            }

            test "List.map transforms values into a consumer-matchable list (sum → 60)" {
                match
                    runJs
                        "step5b-map-sum"
                        ("let ys = List.map (fun x -> x * 10) [1; 2; 3]\n"
                         + "let rec sum xs =\n"
                         + "    match xs with\n"
                         + "    | [] -> 0\n"
                         + "    | h :: t -> h + sum t\n"
                         + "printfn \"%d\" (sum ys)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "60" "map applies f to each element; result is a real cons-list"
            }

            test "List.length of a List.map result composes under Node (→ 3)" {
                match
                    runJs "step5b-map-length" "printfn \"%d\" (List.length (List.map (fun x -> x + 1) [4; 5; 6]))"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "3" "map preserves length; the two imports compose"
            }
        ]
