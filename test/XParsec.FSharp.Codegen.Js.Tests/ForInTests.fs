module XParsec.FSharp.Codegen.Js.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS lowering of F# `for x in source do …`. Only the `Interface` enumerator (a source
// typed `IEnumerable<'T>`/`seq<'T>`) reaches JS codegen — duck-typed `Pattern` sources
// and bare arrays/lists are rejected by the BCL-free JS front-end probe. The `Interface`
// arm carries no member keys, so it lowers to a JS `for…of` that drives the source's own
// `Symbol.iterator` at runtime. The first two tests are emission-shape assertions; the
// slice-3 test below makes a Vesper class genuinely `Symbol.iterator`-able and runs the
// loop end-to-end under Node.

// A function iterating a `seq<int>` parameter — the proven ForIn-producing source.
let private forInSeqSrc =
    String.concat "\n" [
        "let f (s: seq<int>) ="
        "    for x in s do"
        "        printfn \"%d\" x" ]

// A wildcard binder still drives the loop (effect-only body).
let private forInWildcardSrc =
    String.concat "\n" [
        "let f (s: seq<int>) ="
        "    let mutable n = 0"
        "    for _ in s do"
        "        n <- n + 1"
        "    n" ]

// Slice 3 (Track I): a project-local CLASS implementing `seq<int>` (`IEnumerable<int>`)
// over its own `IEnumerator<int>` enumerator. The class's `GetEnumerator` impl is routed
// to a native `*[Symbol.iterator]()` GENERATOR (`emitIteratorMethod`) that drives the F#
// enumerator protocol (`MoveNext()` / `Current`) into JS's — `yield` auto-produces the
// `{ value, done }` iterator results — so `for x in (c :> seq<int>)` actually iterates
// under Node. The enumerator's mutable `Cur` is a `val mutable` field, constructed in
// field order (the JS backend emits a positional field ctor, so `Enum(-1, stop)` aligns
// `Cur = -1`, `Stop = stop`). The upcast forces the Interface enumerator (the only path
// JS supports — a duck-typed `Pattern` source is rejected).
let private seqClassSrc =
    String.concat "\n" [
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
        "let c = Counter(3)"
        "for x in (c :> seq<int>) do"
        "    printfn \"%d\" x" ]

[<Tests>]
let tests =
    testList
        "ForIn"
        [ test "`for x in (s: seq<_>)` lowers the Interface enumerator to a JS `for…of`" {
              let js = emitJs forInSeqSrc
              Expect.stringContains js "for (const x of s)" "Interface ForIn → `for…of` over the source"
          }

          test "a wildcard `for _ in …` binder gets a fresh slot but still iterates" {
              let js = emitJs forInWildcardSrc
              Expect.stringContains js "for (const _forin" "wildcard binder → a fresh `_forin<tok>` of-binding"
              Expect.stringContains js " of s)" "still iterates the source"
          }

          test "a class implementing `seq<int>` emits a `*[Symbol.iterator]()` generator adapter" {
              let js = emitJs seqClassSrc
              // The `GetEnumerator` impl is routed to a native generator keyed by `Symbol.iterator`.
              Expect.stringContains js "*[Symbol.iterator]()" "enumerable capability → computed-key generator"
              // The adapter drives the enumerator protocol: `while (e.MoveNext()) yield e.Current()`.
              Expect.stringContains js ".MoveNext())" "generator drives the enumerator's MoveNext"
              Expect.stringContains js "yield " "yields each element (auto `{ value, done }`)"
              // No plain named `GetEnumerator(` attached method remains.
              Expect.isFalse (js.Contains "GetEnumerator(") "the GetEnumerator slot is consumed by the iterator adapter"
          }

          test "a class implementing `seq<int>` iterates end-to-end under Node" {
              match runJs "forin-seq-class" seqClassSrc with
              | None -> skiptest "node not found on PATH"
              | Some(code, out) ->
                  Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                  Expect.equal out "0\n1\n2" "`for x in (c :> seq<int>)` walks the class's enumerator in order"
          } ]
