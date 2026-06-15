module XParsec.FSharp.Codegen.Js.Tests.Step5bTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 5b (Phase 1) — the cons-list *literals* + `[]`/`::` *patterns*.
// On the JS target there is no FSharp.Core, so `DefaultListIsVesper` is always on
// (the front end runs through `Pipeline.analyseSemForSelfHost`): an unpinned
// `[]`/`::` freezes to the Vesper cons-list `TyUnion(Vesper.Collections.List`1)`.
// That union is *external* (the program references but does not declare it), so it
// resolves through the symbol provider exactly like `Option` (Step 5) — `Empty`
// tag 0, `Cons` tag 1 with `Head`/`Tail` fields — and is emitted as honest nominal
// JS classes. A `[1;2;3]` literal freezes as nested `UnionCons("Cons", …)` ending
// in `UnionCons("Empty", …)`, so construction + `[]`/`::` match ride the unchanged
// Step-4 union path with NO list-awareness in the JS backend and NO runtime import
// (the List *module* functions — `List.map`/`length` — are Phase 2). Golden-text
// plus execution under Node.

[<Tests>]
let tests =
    testList
        "Codegen.Js Step5b"
        [
            // ---- golden text: the cons-list emits as honest nominal classes ----

            test "the cons-list emits as base + Empty/Cons subclasses (tag 0 / tag 1)" {
                Expect.equal
                    (emitJs "let xs = [1; 2; 3]")
                    ("class List {\n"
                     + "  constructor(tag) {\n"
                     + "    this.tag = tag;\n"
                     + "  }\n"
                     + "  cases() {\n"
                     + "    return [\"Empty\", \"Cons\"];\n"
                     + "  }\n"
                     + "}\n"
                     + "class List_Empty extends List {\n"
                     + "  constructor() {\n"
                     + "    super(0);\n"
                     + "  }\n"
                     + "}\n"
                     + "class List_Cons extends List {\n"
                     + "  constructor(Head, Tail) {\n"
                     + "    super(1);\n"
                     + "    this.Head = Head;\n"
                     + "    this.Tail = Tail;\n"
                     + "  }\n"
                     + "}\n"
                     + "const xs = new List_Cons(1, new List_Cons(2, new List_Cons(3, new List_Empty())));\n")
                    "external cons-list union → base + Empty/Cons subclasses, literal → nested Cons ending in Empty"
            }

            // ---- execution under Node ----

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
                // Monomorphic (`int list`) on purpose: a *generic* element-typed
                // consumer function over an external cons-list match
                // (`'a list -> 'a`) is a known front-end gap
                // (project_cons_patterns_and_list_cutover), out of Phase-1 scope.
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

            // ---- Phase 2: List module functions (the first real runtime import) ----

            test "List.length imports from the Vesper.List runtime module and calls it" {
                Expect.equal
                    (emitJs "let n = List.length [1; 2; 3]")
                    ("import { length as $Vesper_Collections_ListModule_length } from \"./Vesper.List.mjs\";\n"
                     + "class List {\n"
                     + "  constructor(tag) {\n"
                     + "    this.tag = tag;\n"
                     + "  }\n"
                     + "  cases() {\n"
                     + "    return [\"Empty\", \"Cons\"];\n"
                     + "  }\n"
                     + "}\n"
                     + "class List_Empty extends List {\n"
                     + "  constructor() {\n"
                     + "    super(0);\n"
                     + "  }\n"
                     + "}\n"
                     + "class List_Cons extends List {\n"
                     + "  constructor(Head, Tail) {\n"
                     + "    super(1);\n"
                     + "    this.Head = Head;\n"
                     + "    this.Tail = Tail;\n"
                     + "  }\n"
                     + "}\n"
                     + "const n = $Vesper_Collections_ListModule_length(new List_Cons(1, new List_Cons(2, new List_Cons(3, new List_Empty()))));\n")
                    "import leads the program; the call references the import alias"
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
                // map runs in the runtime module (curried `map(f)(xs)`), building
                // cons cells of the same `{tag,Head,Tail}` shape; the monomorphic
                // consumer `sum` then folds the result through a `[]`/`::` match.
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
