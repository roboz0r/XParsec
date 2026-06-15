module XParsec.FSharp.Codegen.Js.Tests.Step2Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 2 — functions, currying, partial application, and
// self-recursion. F# functions emit as nested *unary* arrows (`(x) => (y) => …`)
// and applications as unary calls (`f a b` → `f(a)(b)`), so currying and partial
// application fall out without call-site arity analysis. A function that makes a
// saturated self-call in tail position is trampolined to a `while (true)` loop
// with param-shadow mutation, so self-recursion runs in constant stack.
// Golden-text plus execution under Node (the latter skips when `node` is absent).
//
// Tests stay on `= <>` + arithmetic (`+ - * /`): the ordering operators live in
// `Vesper.Comparison`, which has no JS inline bodies yet (TestHelpers `jsManifests`).

[<Tests>]
let tests =
    testList
        "Codegen.Js Step2"
        [
            // ---- golden text ----

            test "a curried function emits nested unary arrows" {
                Expect.equal
                    (emitJs "let add x y = x + y")
                    "const add = (x) => (y) => (((x) + (y)) | 0);\n"
                    "two-parameter curried arrow"
            }

            test "a saturated call emits one unary call per argument" {
                Expect.equal
                    (emitJs "let add x y = x + y\nprintfn \"%d\" (add 2 3)")
                    "const add = (x) => (y) => (((x) + (y)) | 0);\nconsole.log(add(2)(3));\n"
                    "f a b → f(a)(b)"
            }

            test "partial application is just a shorter call chain" {
                Expect.equal
                    (emitJs "let add x y = x + y\nlet add5 = add 5")
                    "const add = (x) => (y) => (((x) + (y)) | 0);\nconst add5 = add(5);\n"
                    "add 5 → add(5), a function value"
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

            // ---- execution under Node ----

            test "a saturated curried call executes (add 2 3 = 5)" {
                match runJs "step2-add" "let add x y = x + y\nprintfn \"%d\" (add 2 3)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "curried saturated call"
            }

            test "partial application executes (add 5 then 3 = 8)" {
                match runJs "step2-partial" "let add x y = x + y\nlet add5 = add 5\nprintfn \"%d\" (add5 3)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "8" "partially-applied function holds the first argument"
            }

            test "non-tail recursion executes (fact 5 = 120)" {
                match
                    runJs "step2-fact" "let rec fact n = if n = 0 then 1 else n * fact (n - 1)\nprintfn \"%d\" (fact 5)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "120" "ordinary (non-tail) recursion via Math.imul multiply"
            }

            test "self-tail recursion runs in constant stack (loop 1000000 = 42)" {
                // A million-deep tail recursion: without the trampoline this
                // overflows the JS call stack (RangeError); with it, it returns 42.
                match
                    runJs
                        "step2-loop"
                        "let rec loop n = if n = 0 then 42 else loop (n - 1)\nprintfn \"%d\" (loop 1000000)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42" "deep tail recursion does not grow the stack"
            }

            test "multi-parameter tail recursion mutates params in lockstep (sum 10 0 = 55)" {
                // `sum (n-1) (acc+n)`: the new `acc` reads the *old* `n`, so the
                // arguments must be evaluated into temporaries before either param
                // is written back.
                match
                    runJs
                        "step2-sum"
                        "let rec sum n acc = if n = 0 then acc else sum (n - 1) (acc + n)\nprintfn \"%d\" (sum 10 0)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "55" "1+2+…+10, temps capture old param values"
            }
        ]
