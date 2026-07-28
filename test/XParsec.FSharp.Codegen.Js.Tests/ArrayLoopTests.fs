module XParsec.FSharp.Codegen.Js.Tests.ArrayLoopTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS-backend array / iteration gaps:
// general `'T[]` (`newarr`/`ldelem`/`stelem`/`ldlen`), `while` loops, and the
// mutable locals a loop drives (`let mutable` + `Assignment`). Each construct is
// proven by emission (the lowering shape) plus Node execution (the behaviour).

// A function whose body sums `1 .. n` with a `while` loop over two mutable locals —
// the canonical loop+mutation shape.
let private sumToSrc =
    String.concat
        "\n"
        [
            "let sumTo n ="
            "    let mutable acc = 0"
            "    let mutable i = 1"
            "    while i <= n do"
            "        acc <- acc + i"
            "        i <- i + 1"
            "    acc"
            "printfn \"%d\" (sumTo 5)"
        ]

// Allocate an `int[]`, fill it by index, then sum it with a `while` loop driven by
// `arr.Length`. Array creation uses the raw `newarr` intrinsic (the same mnemonic
// `Vesper.Array`'s `zeroCreate` carries) so the slice is self-contained — it needs
// no `Vesper.Array` runtime module, only the now-present `ops-platform.js.fs` array ops.
let private arraySrc =
    String.concat
        "\n"
        [
            "let build () ="
            "    let a : int[] = (# \"newarr !0\" type (int) 3 : int[] #)"
            "    a.[0] <- 10"
            "    a.[1] <- 20"
            "    a.[2] <- 30"
            "    let mutable sum = 0"
            "    let mutable i = 0"
            "    while i < a.Length do"
            "        sum <- sum + a.[i]"
            "        i <- i + 1"
            "    sum"
            "printfn \"%d\" (build ())"
        ]

// `for i = 1 to n do …` — the counted-loop sibling of `while`. Sums `1 .. n` by
// accumulating into a mutable local; the limit `n` is evaluated once (hoisted)
// and the loop variable `i` counts up inclusively.
let private forToSrc =
    String.concat
        "\n"
        [
            "let sumTo n ="
            "    let mutable acc = 0"
            "    for i = 1 to n do"
            "        acc <- acc + i"
            "    acc"
            "printfn \"%d\" (sumTo 5)"
        ]

[<Tests>]
let tests =
    testList
        "Codegen.Js ArrayLoop"
        [
            // ---- for i = a to b (counted loop) -------------------------------

            test "a `for i = a to b` loop emits a hoisted limit `const` and a `for` statement" {
                let js = emitJs forToSrc
                Expect.stringContains js "for (let i = " "emits a counted for-loop binding i"
                Expect.stringContains js "i <= " "iterates up to the limit inclusively"
                // The limit `n` is hoisted into a `const` so it is read once, not re-evaluated.
                Expect.stringContains js "_lim" "the end-expr is hoisted into a limit binding"
            }

            // Two counted loops of ONE inlined body land side by side in the SAME JS
            // block, and every node of a spliced body carries the call site's one token —
            // so a limit named after that token would declare the same `const` twice,
            // which is a `SyntaxError`, not a shadow.
            test "two hoisted limits from one spliced body are distinct `const`s" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline twice (n: int) ="
                            "    for i = 1 to n do printfn \"%d\" i"
                            "    for j = 1 to n do printfn \"%d\" j"
                            "twice 2"
                        ]

                let limits =
                    emitJs src
                    |> fun js -> js.Split '\n'
                    |> Array.filter (fun l -> l.StartsWith "const _lim")
                    |> List.ofArray

                Expect.equal limits.Length 2 "one hoisted limit per spliced loop"
                Expect.equal (List.distinct limits) limits "the two limits are separate bindings"

                match runJs "arrayloop-spliced-limits" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1\n2\n1\n2" "both spliced loops run"
            }

            test "the `for i = 1 to 5` sum executes (1+2+3+4+5 = 15)" {
                match runJs "arrayloop-forto-sum" forToSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "15" "prints 15"
            }

            // ---- while + mutable locals --------------------------------------

            test "a `while` loop over mutable locals emits a `let` binding and a `while` statement" {
                let js = emitJs sumToSrc
                // The mutable accumulator is a reassignable `let`, never a `const`.
                Expect.stringContains js "while (" "emits a while loop"
                Expect.isTrue (js.Contains "(acc = ") "the accumulator is reassigned"
            }

            test "the `while`/mutable sum executes (1+2+3+4+5 = 15)" {
                match runJs "arrayloop-while-sum" sumToSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "15" "prints 15"
            }

            // ---- general arrays ----------------------------------------------

            test "`Array.zeroCreate` (newarr) emits a dense `Array(n).fill(null)`" {
                let js = emitJs arraySrc
                Expect.stringContains js "Array(" "allocates via Array(n)"
                Expect.stringContains js ".fill(null)" "fills dense so reads aren't sparse holes"
            }

            test "`arr.[i] <- v` / `arr.[i]` / `arr.Length` emit index assign / read / `.length`" {
                let js = emitJs arraySrc
                Expect.stringContains js ".length" "arr.Length → arr.length"
                // A computed-member access `…[0]` (set) / `…[i]` (read).
                Expect.stringContains js "[0]" "indexed write uses a computed member"
            }

            test "the array build+index+length+loop executes (10+20+30 = 60)" {
                match runJs "arrayloop-array-sum" arraySrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "60" "prints 60"
            }

            // ---- string indexing ---------------------------------------------
            //
            // `s.[i]` on a `string` desugars to the `StringIntrinsics.GetString` inline
            // intrinsic (the string analogue of `GetArray`), whose JS body emits the
            // native `s[i]`. The front end routes here because JS `string` has no BCL
            // `get_Chars`; on CLR `get_Chars` resolves first, so CLR is untouched.

            test "string indexing `s.[i]` emits a native computed-member read" {
                let js =
                    emitJs "let charAt (s: string) (i: int) = s.[i]\nprintfn \"%c\" (charAt \"hello\" 1)"
                // `GetString`'s `(# "$0[$1]" #)` body lowers to `(s)[(i)]`.
                Expect.stringContains js "[(" "string index lowers to a bracket access"
            }

            test "string indexing `s.[i]` executes (\"hello\".[1] = 'e')" {
                let src =
                    "let charAt (s: string) (i: int) = s.[i]\nprintfn \"%c\" (charAt \"hello\" 1)"

                match runJs "arrayloop-string-index" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "e" "indexes the string to its char"
            }

            // ---- `let _ = effect` (Wildcard binder) --------------------------
            //
            // `let _ = expr in body` discards `expr` (kept for its side effects) — the
            // structural-printer's render-into-buffer idiom (`let _ = renderDoc …`), and
            // the natural spelling for an effectful unit expression. JS has no
            // let-expression, so a Wildcard binder in expression position lowers to a comma
            // sequence `(<effect>, <body>)`; a *pure* discarded value drops away entirely.

            test "`let _ = effect in body` emits a comma sequence, not an IIFE" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f () ="
                            "    let a : int[] = (# \"newarr !0\" type (int) 1 : int[] #)"
                            "    let _ = (a.[0] <- 7)"
                            "    a.[0]"
                            "printfn \"%d\" (f ())"
                        ]

                let js = emitJs src
                // The discarded `stelem` is impure, so it survives as the first comma operand
                // of `((a[0] = 7), a[0])` — not hoisted into a named `const`/IIFE binder.
                Expect.stringContains js "(a[0] = 7), a[0]" "the effect is the head of a comma sequence"
            }

            test "`let _ = pure in body` drops the discarded pure value" {
                // `1 + 1` is pure to `isPureValue`, so the wildcard binder collapses to body.
                let js = emitJs "let f () =\n    let _ = 1 + 1\n    42\nprintfn \"%d\" (f ())"
                Expect.isFalse (js.Contains "1 + 1") "a pure discarded value is elided"
            }

            test "`let _ = effect in body` executes (effect runs, body returned)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let f () ="
                            "    let a : int[] = (# \"newarr !0\" type (int) 1 : int[] #)"
                            "    let _ = (a.[0] <- 7)"
                            "    a.[0]"
                            "printfn \"%d\" (f ())"
                        ]

                match runJs "arrayloop-wildcard-let" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "the discarded assignment ran; the body read it back"
            }
        ]
