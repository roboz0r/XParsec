module XParsec.FSharp.Codegen.Js.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS lowering of F# `for x in source do …`. An `IEnumerable<'T>`/`seq<'T>` source lowers
// to `for (const x of src)`, driving the source's own `Symbol.iterator`; a duck-typed
// `GetEnumerator()` source is unsupported and the emitter fails on it.

let private forInSeqSrc =
    String.concat "\n" [ "let f (s: seq<int>) ="; "    for x in s do"; "        printfn \"%d\" x" ]

let private forInWildcardSrc =
    String.concat
        "\n"
        [
            "let f (s: seq<int>) ="
            "    let mutable n = 0"
            "    for _ in s do"
            "        n <- n + 1"
            "    n"
        ]

// A project-local CLASS implementing `seq<int>` over its own `IEnumerator<int>`. The JS
// backend emits a positional field ctor, so `Enum(-1, stop)` sets `Cur = -1`, `Stop = stop`
// in field order; the `:> seq<int>` upcast is what selects the interface enumerator.
let private seqClassSrc =
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
            "let c = Counter(3)"
            "for x in (c :> seq<int>) do"
            "    printfn \"%d\" x"
        ]

// The same capability on a UNION. A JS union value is a CASE-subclass instance
// (`UCounter_Stop3 extends UCounter`), so the `IEnumerable<int>` impl must land on the base
// class `UCounter` for every case instance to inherit `*[Symbol.iterator]()`.
let private seqUnionSrc =
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
            "type UCounter ="
            "    | Stop3"
            "    interface System.Collections.Generic.IEnumerable<int> with"
            "        member _.GetEnumerator() : System.Collections.Generic.IEnumerator<int> ="
            "            (new Enum(-1, 3) :> System.Collections.Generic.IEnumerator<int>)"
            "let u = Stop3"
            "for x in (u :> seq<int>) do"
            "    printfn \"%d\" x"
        ]

// A RECORD source with NO `:> seq<int>` upcast, so the source stays a record type and the
// front end must admit it through its nominal record arm; the class and union tests above
// upcast and never reach that arm. The impl body reads `this` (`this.Stop`).
let private seqRecordSrc =
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
            "type RCounter ="
            "    { Stop: int }"
            "    interface System.Collections.Generic.IEnumerable<int> with"
            "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<int> ="
            "            (new Enum(-1, this.Stop) :> System.Collections.Generic.IEnumerator<int>)"
            "let r = { Stop = 3 }"
            "for x in r do"
            "    printfn \"%d\" x"
        ]

[<Tests>]
let tests =
    testList
        "ForIn"
        [
            test "`for x in (s: seq<_>)` lowers the Interface enumerator to a JS `for…of`" {
                let js = emitJs forInSeqSrc
                Expect.stringContains js "for (const x of s)" "Interface ForIn → `for…of` over the source"
            }

            test "a wildcard `for _ in …` bound variable gets a fresh slot but still iterates" {
                let js = emitJs forInWildcardSrc

                Expect.stringContains
                    js
                    "for (const _forin"
                    "wildcard bound variable → a fresh `_forin<tok>` of-binding"

                Expect.stringContains js " of s)" "still iterates the source"
            }

            test "a class implementing `seq<int>` emits a `*[Symbol.iterator]()` generator adapter" {
                let js = emitJs seqClassSrc
                Expect.stringContains js "*[Symbol.iterator]()" "enumerable capability → computed-key generator"
                // The generator body is `while (e.MoveNext()) yield e.Current()`.
                Expect.stringContains js ".MoveNext())" "generator drives the enumerator's MoveNext"
                Expect.stringContains js "yield " "yields each element (auto `{ value, done }`)"

                Expect.isFalse
                    (js.Contains "GetEnumerator(")
                    "the GetEnumerator slot is consumed by the iterator adapter"
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
                Expect.stringContains js "*[Symbol.iterator]()" "union enumerable capability → base-class generator"
                let iterIdx = js.IndexOf "*[Symbol.iterator]()"
                let subclassIdx = js.IndexOf "extends UCounter"
                Expect.isGreaterThan subclassIdx 0 "the case subclass `extends UCounter` is emitted"
                Expect.isLessThan iterIdx subclassIdx "the iterator sits on the base class, ahead of the case subclass"

                Expect.isFalse
                    (js.Contains "GetEnumerator(")
                    "the GetEnumerator slot is consumed by the iterator adapter"
            }

            test "a UNION implementing `seq<int>` iterates end-to-end under Node" {
                match runJs "forin-seq-union" seqUnionSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0\n1\n2" "`for x in (u :> seq<int>)` walks the union's enumerator in order"
            }

            test "a RECORD implementing `seq<int>` emits a `*[Symbol.iterator]()` generator" {
                let js = emitJs seqRecordSrc
                Expect.stringContains js "*[Symbol.iterator]()" "record enumerable capability → computed-key generator"
                Expect.stringContains js ".MoveNext())" "generator drives the enumerator's MoveNext"

                Expect.isFalse
                    (js.Contains "GetEnumerator(")
                    "the GetEnumerator slot is consumed by the iterator adapter"
            }

            test "an un-upcast RECORD source `for x in r` iterates end-to-end under Node" {
                match runJs "forin-seq-record" seqRecordSrc with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0\n1\n2" "`for x in r` walks the record's enumerator in order"
            }

            // A BARE cons-list, no `:> seq` upcast. `runJs` materialises the committed
            // `Vesper.List.mjs` beside the program, so the emitted `for…of` drives the
            // list's own base-class `*[Symbol.iterator]()` over its `ListEnumerator` cursor.
            test "a BARE cons-list `for x in [1;2;3]` iterates the real Vesper.List on JS (Node)" {
                let src = String.concat "\n" [ "for x in [1; 2; 3] do"; "    printfn \"%d\" x" ]

                match runJs "forin-bare-list" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1\n2\n3" "`for x in [1;2;3]` walks the list's native iterator in order"
            }
        ]
