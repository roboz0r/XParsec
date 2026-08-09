module XParsec.FSharp.Codegen.Js.Tests.FunctionEmissionTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// Every identifier bound by the parameter list of the top-level arrow `const <name> =
/// (…) => …`, destructuring leaves included. The whole vector binds in ONE arrow, so two
/// equal names here are a duplicate parameter, which is a `SyntaxError` in module code.
let private arrowParamBindings (js: string) (name: string) : string list =
    let opening = "const " + name + " = ("

    match js.IndexOf opening with
    | -1 -> failtestf "no top-level arrow `%s` in:\n%s" name js
    | i ->
        let start = i + opening.Length

        match js.IndexOf(") =>", start) with
        | -1 -> failtestf "`%s` is not an arrow in:\n%s" name js
        | stop ->
            js.Substring(start, stop - start).Split([| ','; '['; ']'; ' ' |])
            |> Array.filter (fun s -> s <> "")
            |> List.ofArray

[<Tests>]
let tests =
    testList
        "Codegen.Js Function Emission"
        [
            // A `Fun` IS its own callable in JS, so an unapplied `f.Invoke` can pass the object
            // argument straight through only at the curried arity. Above it the escaped member
            // takes one TUPLED parameter while the flat `Fun` is an N-positional arrow.

            test "an unapplied curried `Fun.Invoke` is the object argument itself" {
                Expect.equal
                    (emitJs "let use1 (f: Fun<int, int>) =\n    let g = f.Invoke\n    g 5\n")
                    "const use1 = (f) => ((g) => g(5))(f);\n"
                    "one parameter either way — no wrapper"
            }

            test "an unapplied flat `Fun.Invoke` eta-wraps its tupled parameter open" {
                Expect.equal
                    (emitJs "let use2 (f: Fun<int, int, int>) =\n    let g = f.Invoke\n    g (1, 2)\n")
                    "const use2 = (f) => ((g) => g([1, 2]))((_a3) => f(_a3[0], _a3[1]));\n"
                    "the one tupled param opens to the arrow's two positions"
            }

            test "a module function emits one flat multi-arg arrow" {
                Expect.equal
                    (emitJs "let add x y = x + y")
                    "const add = (x, y) => (((x) + (y)) | 0);\n"
                    "curried source groups flatten to one multi-arg arrow (Fable-style)"
            }

            test "a saturated call collapses the arguments to one flat call" {
                Expect.equal
                    (emitJs "let add x y = x + y\nprintfn \"%d\" (add 2 3)")
                    "const add = (x, y) => (((x) + (y)) | 0);\nconsole.log(add(2, 3));\n"
                    "f a b → f(a, b)"
            }

            test "a partial application wraps the flat function in a curried adapter" {
                Expect.equal
                    (emitJs "let add x y = x + y\nlet add5 = add 5")
                    "const add = (x, y) => (((x) + (y)) | 0);\nconst add5 = ((_c14) => (_c15) => add(_c14, _c15))(5);\n"
                    "under-applied module function adapts to the source-shaped currying"
            }

            // A parameter no source spells (`_`, `()`) still needs a JS name, and an INLINED
            // body carries the call site's one token on every node it owns, so a token cannot
            // tell two of them apart.

            test "two wildcard parameters of one spliced flat arrow are distinct bindings" {
                let src =
                    "let inline mk ((a: int, _: int), (b: int, _: int)) = a + b\n"
                    + "let f = mk\n"
                    + "printfn \"%d\" (f ((1, 2), (3, 4)))\n"

                let names = arrowParamBindings (emitJs src) "f"
                Expect.equal names.Length 4 "two destructured pairs, four bindings"
                Expect.equal (List.distinct names) names "no parameter name is bound twice"

                match runJs "fn-spliced-wildcards" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "4" "1 + 3"
            }

            test "two unit parameters of one spliced flat arrow are distinct bindings" {
                let src = "let inline mk ((), ()) = 42\nlet g = mk\nprintfn \"%d\" (g ((), ()))\n"

                let names = arrowParamBindings (emitJs src) "g"
                Expect.equal names.Length 2 "one binding per unit parameter"
                Expect.equal (List.distinct names) names "no parameter name is bound twice"

                match runJs "fn-spliced-units" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "the unit parameters are accepted and dropped"
            }

            test "a self-tail-recursive function becomes a `while (true)` trampoline" {
                Expect.equal
                    (emitJs "let rec loop n = if n = 0 then 42 else loop (n - 1)")
                    ("const loop = (n) => {\n"
                     + "  while (true) {\n"
                     + "    if (((n) === (0))) {\n"
                     + "      return 42;\n"
                     + "    } else {\n"
                     + "      const _tc0 = (((n) - (1)) | 0);\n"
                     + "      n = _tc0;\n"
                     + "      continue;\n"
                     + "    }\n"
                     + "  }\n"
                     + "};\n")
                    "tail self-call → temp + param write-back + continue"
            }

            test "a saturated curried call executes (add 2 3 = 5)" {
                match runJs "fn-add" "let add x y = x + y\nprintfn \"%d\" (add 2 3)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "curried saturated call"
            }

            test "partial application executes (add 5 then 3 = 8)" {
                match runJs "fn-partial" "let add x y = x + y\nlet add5 = add 5\nprintfn \"%d\" (add5 3)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "8" "partially-applied function holds the first argument"
            }

            test "non-tail recursion executes (fact 5 = 120)" {
                match
                    runJs "fn-fact" "let rec fact n = if n = 0 then 1 else n * fact (n - 1)\nprintfn \"%d\" (fact 5)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "120" "ordinary (non-tail) recursion via Math.imul multiply"
            }

            test "self-tail recursion runs in constant stack (loop 1000000 = 42)" {
                // Without the trampoline this overflows the JS call stack (RangeError).
                match
                    runJs "fn-loop" "let rec loop n = if n = 0 then 42 else loop (n - 1)\nprintfn \"%d\" (loop 1000000)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "deep tail recursion does not grow the stack"
            }

            test "multi-parameter tail recursion mutates params in lockstep (sum 10 0 = 55)" {
                // new `acc` reads the *old* `n`, so both must be captured before either is written back.
                match
                    runJs
                        "fn-sum"
                        "let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)\nprintfn \"%d\" (sum 10 0)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "55" "1+2+…+10, temps capture old param values"
            }
        ]
