module XParsec.FSharp.Codegen.Js.Tests.ManualEnumerationTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The MANUAL enumeration protocol on JS: `let e = src.GetEnumerator()` then
// `while e.MoveNext() do … e.Current`, the pull loop `for … in` is sugar for. Each row's
// emitted shape is pinned, then the loop is run under Node.

// `Counter(n)` yields 0 … n-1 through its own `Enum` cursor. Its `GetEnumerator` impl emits
// as a `*[Symbol.iterator]()` generator, so the manual protocol below runs the consumer
// adapter over that generator, checking the implementer and consumer halves against each other.
let private counterPrelude =
    String.concat
        "\n"
        [
            "type Enum ="
            "    val mutable Cur : int"
            "    val Stop : int"
            "    new(cur: int, stop: int) = { Cur = cur; Stop = stop }"
            "    interface System.Collections.Generic.IEnumerator<int> with"
            "        member this.MoveNext() : bool ="
            "            this.Cur <- this.Cur + 1"
            "            this.Cur < this.Stop"
            "        member this.Current : int = this.Cur"
            "type Counter(stop: int) ="
            "    interface System.Collections.Generic.IEnumerable<int> with"
            "        member _.GetEnumerator() : System.Collections.Generic.IEnumerator<int> ="
            "            (new Enum(-1, stop) :> System.Collections.Generic.IEnumerator<int>)"
        ]

/// `counterPrelude` + a `sum` over the manual protocol, whose enumerator bound variable is `boundVar`
/// (`let` / `use`) and which runs `afterLoop` once the loop ends. `sum (Counter 4)` = 0+1+2+3 = 6.
let private manualSumSrc (boundVar: string) (afterLoop: string list) =
    String.concat
        "\n"
        [
            counterPrelude
            "let sum (s: seq<int>) ="
            "    let mutable acc = 0"
            sprintf "    %s e = s.GetEnumerator()" boundVar
            "    while e.MoveNext() do"
            "        acc <- acc + e.Current"
            yield! afterLoop
            "    acc"
            "printfn \"%d\" (sum (Counter(4) :> seq<int>))"
        ]

let private letSrc = manualSumSrc "let" []
let private useSrc = manualSumSrc "use" []
let private disposeSrc = manualSumSrc "let" [ "    e.Dispose()" ]

[<Tests>]
let tests =
    testList
        "ManualEnumeration"
        [
            test "`src.GetEnumerator()` lowers to the `Vesper.Core.mjs` iterator adapter" {
                let js = emitJs letSrc

                Expect.stringContains
                    js
                    "enumeratorOf"
                    "GetEnumerator → the `enumeratorOf` runtime adapter, not an object-argument method"

                Expect.stringContains
                    js
                    "from \"./Vesper.Core/Vesper.Core.mjs\""
                    "the adapter is imported from the Vesper.Core runtime module"

                // A type-prefixed `seq__GetEnumerator` import names an export
                // `Vesper.Core.mjs` does not have: an ESM link error under Node, not a
                // compile diagnostic.
                Expect.isFalse (js.Contains "seq__GetEnumerator") "no mangled `seq__GetEnumerator` import"
            }

            test "the cursor's `MoveNext`/`Current` lower to the attached slots the impl emits" {
                let js = emitJs letSrc
                Expect.stringContains js "e.MoveNext()" "MoveNext → an attached zero-arg call"
                // An interface property compiles to a zero-arg method, so the READ is a call.
                Expect.stringContains js "e.Current()" "Current → an attached zero-arg call, not a data read"
                Expect.isFalse (js.Contains "enumerator__MoveNext") "no mangled `enumerator__MoveNext` import"
                Expect.isFalse (js.Contains "enumerator__get_Current") "no mangled `enumerator__get_Current` import"
            }

            test "the manual protocol pulls a `seq<int>` end-to-end under Node" {
                match runJs "manual-enum-let" letSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "6" "`while e.MoveNext() do acc <- acc + e.Current` sums 0..3"
            }

            test "`use e = src.GetEnumerator()` disposes the cursor through `[Symbol.dispose]()`" {
                let js = emitJs useSrc
                Expect.stringContains js "[Symbol.dispose]()" "`use` disposal → the native disposal slot"
                Expect.stringContains js "finally" "disposal runs in a `finally`"
            }

            test "`use e = src.GetEnumerator()` still pulls the sequence under Node" {
                match runJs "manual-enum-use" useSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "6" "the disposed cursor yields the same elements"
            }

            test "an explicit `e.Dispose()` lowers to the same `[Symbol.dispose]()` slot" {
                let js = emitJs disposeSrc
                Expect.stringContains js "[Symbol.dispose]()" "`Dispose` (the disposal capability) → the native slot"
                Expect.isFalse (js.Contains "disposable__Dispose") "no mangled `disposable__Dispose` import"

                match runJs "manual-enum-dispose" disposeSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "6" "an explicitly disposed cursor yields the same elements"
            }
        ]
