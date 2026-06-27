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

// Same slice, but the enumerable capability rides a UNION instead of a class. A JS union
// value is a CASE-subclass instance (`UCounter_Stop3 extends UCounter`), so the union's
// `IEnumerable<int>` impl must land on the BASE class `UCounter` to be inherited by every
// case instance — and `for x in (Stop3 :> seq<int>)` then drives `[Symbol.iterator]`
// resolved on the concrete case instance. This exercises that a union routes its interface
// impls through the SAME `partitionClassMembers` path the class uses, attaching
// `*[Symbol.iterator]()` to the union base class. (Referencing the receiver `this` or a
// case PAYLOAD inside a union interface-impl body hits a separate front-end binder-scoping
// gap — slice 1 did not scope the self/pattern binders for a union's impl bodies — so the
// `GetEnumerator` body is receiver-free and the case is nullary.)
let private seqUnionSrc =
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
        "type UCounter ="
        "    | Stop3"
        "    interface System.Collections.Generic.IEnumerable<int> with"
        "        member _.GetEnumerator() : System.Collections.Generic.IEnumerator<int> ="
        "            (new Enum(-1, 3) :> System.Collections.Generic.IEnumerator<int>)"
        "let u = Stop3"
        "for x in (u :> seq<int>) do"
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
          }

          test "a UNION implementing `seq<int>` emits `*[Symbol.iterator]()` on the BASE class" {
              let js = emitJs seqUnionSrc
              // The union's `GetEnumerator` impl is routed to the base-class iterator adapter.
              Expect.stringContains js "*[Symbol.iterator]()" "union enumerable capability → base-class generator"
              // It must land on the BASE class (`class UCounter {`), BEFORE the case subclass
              // (`class UCounter_Stop extends UCounter`), so every case instance inherits it.
              let iterIdx = js.IndexOf "*[Symbol.iterator]()"
              let subclassIdx = js.IndexOf "extends UCounter"
              Expect.isGreaterThan subclassIdx 0 "the case subclass `extends UCounter` is emitted"
              Expect.isLessThan iterIdx subclassIdx "the iterator sits on the base class, ahead of the case subclass"
              // No plain named `GetEnumerator(` attached method remains.
              Expect.isFalse (js.Contains "GetEnumerator(") "the GetEnumerator slot is consumed by the iterator adapter"
          }

          test "a UNION implementing `seq<int>` iterates end-to-end under Node" {
              match runJs "forin-seq-union" seqUnionSrc with
              | None -> skiptest "node not found on PATH"
              | Some(code, out) ->
                  Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                  Expect.equal out "0\n1\n2" "`for x in (u :> seq<int>)` walks the union's enumerator in order"
          }

          // §14.6 capstone (W1+W3): a BARE cons-list `[1;2;3]` — NO `:> seq` upcast —
          // iterates over the REAL `Vesper.List` JS runtime. `runJs` materialises the
          // committed `Vesper.List.mjs` (regenerated from `list.js.fs`, now carrying the
          // base-class `*[Symbol.iterator]()` adapter over its `ListEnumerator` cursor)
          // beside the program, so the emitted `for…of` over the list drives the list's
          // own iterator under Node.
          test "a BARE cons-list `for x in [1;2;3]` iterates the real Vesper.List on JS (Node)" {
              let src =
                  String.concat "\n" [ "for x in [1; 2; 3] do"; "    printfn \"%d\" x" ]

              match runJs "forin-bare-list" src with
              | None -> skiptest "node not found on PATH"
              | Some(code, out) ->
                  Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                  Expect.equal out "1\n2\n3" "`for x in [1;2;3]` walks the list's native iterator in order"
          } ]
