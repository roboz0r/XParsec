module XParsec.FSharp.Codegen.Js.Tests.ArrayLoopTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS lowering of general `'T[]` (`newarr`/`ldelem`/`stelem`/`ldlen`), `while` and counted
// `for` loops, and the mutable locals a loop drives. Each construct is pinned by its
// emitted shape, then run under Node.

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

// Allocation is the raw `newarr` intrinsic rather than `Array.zeroCreate`, which carries
// the same mnemonic, so this fixture needs only the platform array ops and no
// `Vesper.Array` runtime module.
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
                Expect.stringContains js "_lim" "the end-expr is hoisted into a limit binding"
            }

            // Two counted loops from ONE inlined body land in the same JS block, and every
            // node of a spliced body carries the call site's single token, so a limit named
            // after that token would redeclare one `const`: a `SyntaxError`, not a shadow.
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

            test "a `while` loop over mutable locals emits a `while` statement that reassigns them" {
                let js = emitJs sumToSrc
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

            test "a raw `newarr` emits a dense `Array(n).fill(null)`" {
                let js = emitJs arraySrc
                Expect.stringContains js "Array(" "allocates via Array(n)"
                Expect.stringContains js ".fill(null)" "fills dense so reads aren't sparse holes"
            }

            test "`arr.[i] <- v` / `arr.[i]` / `arr.Length` emit index assign / read / `.length`" {
                let js = emitJs arraySrc
                Expect.stringContains js ".length" "arr.Length → arr.length"
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

            // `s.[i]` on a `string` routes to an inline intrinsic whose JS body is the
            // native `s[i]`. JS `string` has no `get_Chars`; on CLR `get_Chars` resolves
            // first, so CLR is untouched.

            test "string indexing `s.[i]` emits a native computed-member read" {
                let js =
                    emitJs "let charAt (s: string) (i: int) = s.[i]\nprintfn \"%c\" (charAt \"hello\" 1)"
                // The intrinsic body `(# "$0[$1]" #)` lowers to `(s)[(i)]`.
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

            // ---- `let _ = effect` (wildcard bound variable) --------------------------

            // JS has no let-expression, so `let _ = expr in body` in expression position
            // lowers to a comma sequence `(<effect>, <body>)`. A discarded value that is
            // PURE drops away entirely.

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
                Expect.stringContains js "(a[0] = 7), a[0]" "the effect is the head of a comma sequence"
            }

            test "`let _ = pure in body` drops the discarded pure value" {
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
