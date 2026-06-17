module XParsec.FSharp.Codegen.Js.Tests.ArrayLoopTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// printf-shared-core-plan Phase 2 — the JS-backend array / iteration gaps:
// general `'T[]` (`newarr`/`ldelem`/`stelem`/`ldlen`), `while` loops, and the
// mutable locals a loop drives (`let mutable` + `Assignment`). Each construct is
// proven by emission (the lowering shape) plus Node execution (the behaviour).

// A function whose body sums `1 .. n` with a `while` loop over two mutable locals —
// the canonical loop+mutation shape this phase enables.
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

[<Tests>]
let tests =
    testList
        "Codegen.Js ArrayLoop"
        [
            // ---- while + mutable locals --------------------------------------

            test "a `while` loop over mutable locals emits a `let` binding and a `while` statement" {
                let js = emitJs sumToSrc
                // The mutable accumulator is a reassignable `let`, never a `const`.
                Expect.stringContains js "while (" "emits a while loop"
                Expect.isTrue (js.Contains "(acc = ") "the accumulator is reassigned"
            }

            test "the `while`/mutable sum executes (1+2+3+4+5 = 15)" {
                match runJs "phase2-while-sum" sumToSrc with
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
                match runJs "phase2-array-sum" arraySrc with
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

                match runJs "phase2-string-index" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "e" "indexes the string to its char"
            }
        ]
