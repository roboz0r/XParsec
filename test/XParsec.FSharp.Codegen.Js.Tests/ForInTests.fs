module XParsec.FSharp.Codegen.Js.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS lowering of F# `for x in source do …`. Only the `Interface` enumerator (a source
// typed `IEnumerable<'T>`/`seq<'T>`) reaches JS codegen — duck-typed `Pattern` sources
// and bare arrays/lists are rejected by the BCL-free JS front-end probe. The `Interface`
// arm carries no member keys, so it lowers to a JS `for…of` that drives the source's own
// `Symbol.iterator` at runtime. These are emission-shape assertions (the lowering's
// "presence"); end-to-end Node execution waits on an iterable JS runtime value (a later
// slice that makes Vesper collections `Symbol.iterator`-able).

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
          } ]
