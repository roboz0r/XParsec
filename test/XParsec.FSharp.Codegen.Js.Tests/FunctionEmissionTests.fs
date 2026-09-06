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

            test "a tail self-call in a match arm trampolines" {
                Expect.equal
                    (emitJs "let rec loop n = match n with 0 -> 42 | _ -> loop (n - 1)")
                    ("const loop = (n) => {\n"
                     + "  while (true) {\n"
                     + "    const _m12 = n;\n"
                     + "    if ((_m12 === 0)) {\n"
                     + "      return 42;\n"
                     + "    }\n"
                     + "    {\n"
                     + "      const _tc0 = (((n) - (1)) | 0);\n"
                     + "      n = _tc0;\n"
                     + "      continue;\n"
                     + "    }\n"
                     + "    throw new Error(\"The match cases were incomplete\");\n"
                     + "  }\n"
                     + "};\n")
                    "the scrutinee binds as a const in the loop body; the arm writes back and continues"
            }

            test "tail recursion through match arms runs in constant stack (loop 1000000 = 42)" {
                match
                    runJs
                        "fn-loop-match"
                        ("let rec loop n = match n with 0 -> 42 | _ -> loop (n - 1)\n"
                         + "printfn \"%d\" (loop 1000000)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "deep tail recursion through a match does not grow the stack"
            }

            // `a` is classified `NonRecursive`, so it binds as an IIFE parameter and its value's
            // reference to `b` is out of scope (`ReferenceError: b is not defined`).
            ptest
                "GAP: a local `let rec … and …` member that references only its sibling binds before the sibling is declared" {
                match
                    runJs
                        "fn-local-mutual-rec"
                        ("let run () =\n"
                         + "    let rec a x = if x = 0 then 0 else b (x - 1)\n"
                         + "    and b x = a x\n"
                         + "    a 3\n"
                         + "printfn \"%d\" (run ())")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0" "each member is in scope of every value in the group"
            }

            test "a tail self-call under a tail-position let trampolines" {
                let js = emitJs "let rec f n = let m = n - 1 in if m < 0 then 0 else f m"
                Expect.stringContains js "while (true)" "the TailRecursive binding loops"
                Expect.stringContains js "continue;" "the marked call writes back and continues"
            }

            test "a TailRecursive let in a trampolined function's tail position gets its own loop" {
                let js =
                    emitJs (
                        "let rec outer n =\n"
                        + "    let rec inner m = if m = 0 then 0 else inner (m - 1)\n"
                        + "    if n = 0 then inner 3 else outer (n - 1)"
                    )

                let loops = (js.Split("while (true)")).Length - 1
                Expect.equal loops 2 (sprintf "outer and inner each trampoline:\n%s" js)
            }

            test "a tupled parameter group trampolines onto its flattened parameters" {
                // One source application carries two flat params, so the write-back must open
                // the tuple rather than assume one argument per parameter.
                Expect.equal
                    (emitJs "let rec loop (n, acc) = if n = 0 then acc else loop (n - 1, acc + n)")
                    ("const loop = (n, acc) => {\n"
                     + "  while (true) {\n"
                     + "    if (((n) === (0))) {\n"
                     + "      return acc;\n"
                     + "    } else {\n"
                     + "      const _tc0 = (((n) - (1)) | 0);\n"
                     + "      const _tc1 = (((acc) + (n)) | 0);\n"
                     + "      n = _tc0;\n"
                     + "      acc = _tc1;\n"
                     + "      continue;\n"
                     + "    }\n"
                     + "  }\n"
                     + "};\n")
                    "tuple group opened onto both flat params, then `continue`"
            }

            test "tail recursion through a tupled group runs in constant stack (sum (60000, 0))" {
                // 60000 frames overflow the JS call stack untrampolined; the sum stays in `int`.
                match
                    runJs
                        "fn-loop-tupled"
                        ("let rec sum (n, acc) = if n = 0 then acc else sum (n - 1, acc + n)\n"
                         + "printfn \"%d\" (sum (60000, 0))")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1800030000" "deep tail recursion over a tuple group does not grow the stack"
            }

            test "a mutable local passed to an inline function returning a closure is captured by value" {
                // The parameter is bound before the closure is created, so the later write to
                // `m` is invisible to it. Substituting `m` into the closure would read it at
                // call time.
                match
                    runJs
                        "fn-inline-closure-snapshot"
                        ("let inline delay (x: int) = fun () -> x\n"
                         + "let test () =\n    let mutable m = 1\n    let g = delay m\n    m <- 2\n    g ()\n"
                         + "printfn \"%d\" (test ())")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1" "the closure holds the value bound before the write"
            }
        ]
